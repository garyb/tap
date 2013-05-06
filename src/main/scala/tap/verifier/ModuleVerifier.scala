package tap.verifier

import tap.ast._
import tap.ir.TapNode.ResolveState
import tap.ir._
import tap.types.Type._
import tap.types._
import tap.types.classes.ClassEnvironments.Inst
import tap.types.classes.{TypeclassDef, Qual, IsIn}
import tap.types.inference.Substitutions
import tap.types.kinds.Kind._
import tap.types.kinds._
import tap.util.{trace, Graph}
import tap.util.PrettyPrint._
import tap.verifier.defs.{DefinitionsLookup, ModuleDefinitions}
import tap.verifier.errors._
import tap.{InstId, ModuleId, Id}
import language.reflectiveCalls

class ModuleVerifier(val scopes: Map[String, DefinitionsLookup]) {

    type ModuleName = String
    type TypeConstructors = Map[ModuleId, TCon]
    type TypeVars = Map[String, TVar]

    def apply(modules: Seq[ASTModule], verifiedDefs: ModuleDefinitions): ModuleDefinitions = {

        val dtASTs = modules.flatMap { m => m.members.collect { case dtd: ASTDataTypeDefinition => ModuleId(m.name, dtd.name) -> dtd } }.toMap
        val tcATSs = modules.flatMap { m => m.members.collect { case tcd: ASTTypeClassDefinition => ModuleId(m.name, tcd.name) -> tcd } }.toMap
        val instASTs = modules.flatMap { m => m.members.collect { case tci: ASTTypeClassInstance => m.name -> tci } }
        val mdASTs = modules.flatMap { m => m.members.collect { case md: ASTDef => m.name -> md } }

        var defs = verifiedDefs
        defs = addDataTypeDefs(dtASTs, defs)
        defs = addTypeclassDefs(tcATSs, defs)
        defs = addTypeclassInstances(instASTs, defs)
        defs = addMemberDefs(mdASTs, defs)
        defs = addTypeclassMemberDefs(tcATSs, defs)
        defs = addMemberImplementations(modules, mdASTs, defs)
        defs
    }

    def addDataTypeDefs(dtASTs: Map[ModuleId, ASTDataTypeDefinition], defs: ModuleDefinitions): ModuleDefinitions = {

        // Find the type constructor dependencies
        val tconDeps = dtASTs map { case (id @ ModuleId(mId, _), ASTDataTypeDefinition(_, _, dcons)) =>
            id -> (dcons flatMap { dcon =>
                val tcons = ASTUtil.findAllTypeConstructors(scopes(mId).tcons, dcon.args)
                // Exclude dependencies that have already been resolved
                tcons filterNot { d => defs.tcons contains d }
            })
        }

        // Type constructors can have circular dependencies, so find and sort the strongly connected components
        val tconOrd = Graph.components(tconDeps)

        // Extract the type constructors and data constructors for each module
        val (tcons, dcons) = tconOrd.foldLeft((defs.tcons, defs.dcons)) { case ((tcons0, dcons0), currentDtdNames) =>

            // The ASTs for the current group of data type definitions
            val currentDtds = currentDtdNames.zip (currentDtdNames map { msn => dtASTs(msn) }).toMap

            // Infer kinds for each of the parameters in the type constructors, based on how the parameters are
            // used in each of the data constructors.
            val km = KInfer.solve(currentDtds.flatMap { case (id, dtd) =>
                dtd.constructors.flatMap { dcon =>
                    KInfer.constrain(scopes(id.mId).tcons, tcons0, id, currentDtdNames, dcon.args)
                }
            }.toList, NullFilePosition)

            // Create type constructor related stuff - the TCon definitions, the types that the data constructors
            // for each type constructor will produce when applied, and type environments containing just the
            // parameters for each type constructor
            val (tcons1, dts, dtenvs) = currentDtds.foldLeft((tcons0, Map.empty[ModuleId, Type], Map.empty[ModuleId, TypeVars])) {
                case ((tcs, dtdts, penvs), (id, dtd)) =>

                    // Find the kinds of the params used for the current type constructor
                    val pkinds = dtd.params.map { p => p -> KInfer(km, Kvar(id, p)) }.toMap

                    // Create the kind for the current type constructor
                    val k = dtd.params.foldRight(Star: Kind) { (p, k) => Kfun(pkinds(p), k) }

                    // The type constructor definition
                    val tcon = TCon(id, k)

                    // Create a type environment containing just the parameters in the type constructor
                    val penv = pkinds map { case (i, k) => i -> TVar(i, k) }

                    // The type that the data constructors for this type constructor will produce
                    val t = dtd.params.foldLeft(tcon: Type) { (t, p) => TAp(t, penv(p)) }

                    (tcs + (id -> tcon), dtdts + (id -> t), penvs + (id -> penv))
            }

            // Create function types for the data constructors.
            val dcons1 = currentDtds.foldLeft(dcons0) { case (dcs, (id, dtd)) =>
                dtd.constructors.foldLeft(dcs) { case (dcs, dcon) =>
                    val at = dcon.args match {
                        case Seq() => dts(id)
                        case as => as.map { a => ASTUtil.getType(scopes(id.mId).tcons, tcons1, dtenvs(id), a) }.foldRight(dts(id): Type) { (x, y) => x fn y }
                    }
                    dcs + (ModuleId(id.mId, dcon.name) -> Type.quantify(Substitutions.tv(at), at))
                }
            }

            (tcons1, dcons1)
        }

        defs.copy(tcons = tcons, dcons = dcons)
    }

    def addTypeclassDefs(tcASTs: Map[ModuleId, ASTTypeClassDefinition], defs: ModuleDefinitions): ModuleDefinitions = {

        // TODO: check all class tvs are reachable in the member arguments
        // tv reach test will also need refining when fundeps are in place, as then reach can be based on other
        // parameters as well as the simple presence check

        tcASTs.values foreach { case ast @ ASTTypeClassDefinition(_, _, ps, _) =>
            if (ps.isEmpty) throw new VerifierMiscError("Typeclass has no type variables", ast)
        }

        // Build the list of dependencies each typeclass has (dependencies being superclasses - typeclasses referenced
        // in the context of the current typeclass definition)
        val tcDeps = tcASTs.map { case (id, tcd @ ASTTypeClassDefinition(_, context, _, _)) =>
            id -> context.map { case ASTTypeClassReference(name, _) =>
                scopes(id.mId).tcs.get(name) match {
                    case None => throw UnknownTypeclassError(name, tcd)
                    case Some(tcRef) => tcRef
                }
            }
        }

        // Sort typeclasses to ensure they resolve in the correct order. For example, Ord depends on Eq, so the result
        // of this sort will place Eq ahead of Ord in the list.
        val tcOrd = try {
            Graph.tsort(tcDeps).filter { msn => tcASTs.contains(msn) }
        } catch {
            case e: IllegalArgumentException => throw TypeclassRecursiveHeirarchyError(tcDeps.keys)
        }

        // Resolve the typeclasses
        val tcs = tcOrd.foldLeft(defs.tcs) { case (tcs, id) =>

            val currLookup = scopes(id.mId)

            // When ordering the typeclasses we discarded the AST nodes in favour of module-qualified identifiers,
            // so get the AST for the current definition
            val ast @ ASTTypeClassDefinition(_, context, tyvars, members) = tcASTs(id)

            // Add kind inference constraints from superclasses
            val kcs0 = context.foldLeft(List.empty[(Kind, Kind)]) { case (kcs, ast @ ASTTypeClassReference(supername, params)) =>

                // Check the params passed to superclasses are included in the typeclass parameters
                params find { p => !(tyvars contains p) } match {
                    case Some(p) => throw UnknownTypeVariableError(p, ast)
                    case None =>
                }

                val superId = currLookup.tcs(supername)
                if (superId == id) throw TypeclassRecursiveHeirarchyError(List(currLookup.tcs(supername), id))

                // Check the params passed to the superclass matches the arity of the superclass
                val superParams = tcs(superId).vs
                if (superParams.length != params.length) throw TypeclassArityError(supername, superParams.length, params.length, ast)

                // Add constraints for each of the parameters, using the kinds from the superclass parameters
                ((params map { p => Kvar(id, p) }) zip (superParams map kind)) ++ kcs
            }

            // Extract types from members to use to generate kind constraints for the typeclass parameters
            val memberTypes = members.collect {
                case ast @ ASTTypeClassMemberDefinition(name, _, ttype) =>
                    val tvs = ASTUtil.findTypeVars(ttype)
                    if (!(tyvars forall { v => tvs contains v})) throw TypeclassIllegalMemberDefinition(id, name, ast)
                    ttype
            }

            // Accumulate kind constraints from member types
            val kcs1 = KInfer.constrain(currLookup.tcons, defs.tcons, id, Seq.empty, memberTypes)

            // Attempt to solve kind constraints
            val km = KInfer.solve(kcs0 ++ kcs1, ast)

            // Extract kind for each parameter based on the solve mapping
            val pts = tyvars.map { p => TVar(p, KInfer(km, Kvar(id, p))) }

            // Create the context predicates using the type vars
            val tvs = (pts map { pt => pt.id -> pt }).toMap
            val contextParams = getPredicates(currLookup.tcs, tcs, context, tvs)

            // Extract names of members defined within the typeclass
            val memberNames = members.foldLeft(Set.empty[String]) {
                case (result, ast @ ASTTypeClassMemberDefinition(memn, _, _)) =>
                    if (result contains memn) throw TypeclassDuplicateMemberDefinitionError(id, memn, ast)
                    else result + memn
                case (result, _) => result
            }

            // Extract names of members implemented within the typeclass
            val defaultMemberNames = members.foldLeft(Set.empty[String]) {
                case (result, ast @ ASTTypeClassMemberImplementation(memn, _)) =>
                    if (!(memberNames contains memn)) throw TypeclassImplementsUnknownMemberError(id, memn, ast)
                    if (result contains memn) throw TypeclassDuplicateMemberImplementationError(id, memn, ast)
                    else result + memn
                case (result, _) => result
            }

            tcs + (id -> TypeclassDef(id, contextParams, pts, memberNames, defaultMemberNames).setFilePosFrom(ast))
        }

        defs.copy(tcs = tcs)
    }

    def addTypeclassInstances(instASTs: Seq[(ModuleName, ASTTypeClassInstance)], defs: ModuleDefinitions): ModuleDefinitions = {

        val tcis = instASTs.foldLeft(defs.tcis) { case (result, (mId, ast @ ASTTypeClassInstance(name, context, params, members))) =>

            // TODO: warn about orphaned instances. also decide what an orphan instance is in the multi parameter typeclass system.

            val tconLookup = scopes(mId).tcons
            val tcIds = scopes(mId).tcs
            val tcId = tcIds(name)
            val tc = defs.tcs(tcId)

            // TODO: get all type variables used in concrete applied types
            // TODO: check kinds of type variables used all work out
            val usedParams = params.collect {
                case p @ ASTTypeApply(tc, _) if ASTUtil.isConcrete(tc) => ASTUtil.findTypeVars(p, Set.empty)
            }.flatten

            val ps = (params zip tc.vs) map {
                case (param: ASTTypeVar, _) => throw TypeclassIllegalParameterError("Typeclass parameters cannot be type variables " + (usedParams contains param.name), param)
                case (param, tcp) =>
                    val t = lookupInstanceType(tconLookup, defs.tcons, param)
                    if (kind(tcp) != kind(t)) throw TypeclassIllegalParameterError("Type parameter kind is wrong: '" + kind(t) + "' should be '" + kind(tcp) + "' " + prettyPrint(t), param)
                    t
            }

            val tvs = (ps flatMap { p => Substitutions.tv(p) } map { tv => tv.id -> tv }).toMap

            val preds = getPredicates(tcIds, defs.tcs, context, tvs)

            val membersImplemented = members.foldLeft(Set.empty[String]) {
                case (result, ast @ ASTTypeClassMemberImplementation(memn, _)) =>
                    if (!(tc.members contains memn)) throw InstanceUnknownMemberError(tc, ps, memn, ast)
                    if (result contains memn) throw InstanceDuplicateMemberError(tc, ps, memn, ast)
                    else result + memn
            }

            val membersUnimplemented = tc.members -- membersImplemented -- tc.defaultMembers
            if (membersUnimplemented.nonEmpty) throw InstanceIncompleteError(tc, ps, membersUnimplemented, ast)

            val existingInsts = result.getOrElse(tcId, List.empty)
            val inst = Inst(mId, preds, IsIn(tcId, ps)).setFilePosFrom(ast)
            result + (tcId -> (inst :: existingInsts))
        }

        defs.copy(tcis = tcis)
    }

    def addMemberDefs(mdASTs: Seq[(ModuleName, ASTDef)], defs: ModuleDefinitions): ModuleDefinitions = {

        val mts = mdASTs.foldLeft(defs.mts) { case (result, (mId, ast @ ASTDef(id, context, ttype))) =>
            val qId = ModuleId(mId, id)
            if (result contains qId) throw throw ModuleDuplicateDefinition(mId, "member", id, ast)
            val qt = getMemberType(qId, ttype, context, defs)
            result + (qId -> Qual.quantify(Substitutions.tv(qt), qt))
        }

        defs.copy(mts = mts)
    }

    def addTypeclassMemberDefs(tcASTs: Map[ModuleId, ASTTypeClassDefinition], defs: ModuleDefinitions): ModuleDefinitions = {

        val mts = tcASTs.foldLeft(defs.mts) { case (result, (ModuleId(mId, tcName), ASTTypeClassDefinition(_, _, _, members))) =>

            val msntc = scopes(mId).tcs(tcName)
            val tc = defs.tcs(msntc)
            val pred = IsIn(msntc, tc.vs)

            members.foldLeft(result) {
                case (result, ast @ ASTTypeClassMemberDefinition(id, context, ttype)) =>
                    val qId = ModuleId(mId, id)
                    if (result contains qId) throw ModuleDuplicateDefinition(mId, "member", id, ast)
                    val qt0 = getMemberType(qId, ttype, context, defs)
                    val qt1 = Qual(pred :: qt0.ps, qt0.h)
                    result + (qId -> Qual.quantify(Substitutions.tv(qt1), qt1))
                case (result, _) => result
            }
        }

        defs.copy(mts = mts)
    }

    def addMemberImplementations(modules: Seq[ASTModule], mdASTs: Seq[(ModuleName, ASTDef)], defs: ModuleDefinitions): ModuleDefinitions = {

        val mis = defs.mis ++ modules.flatMap { m =>
            val mId = m.name
            val ms = scopes(mId)
            val rs = ResolveState(ms.dcons, ms.members, Set.empty, ms.tcons, defs.tcons)

            // TODO: warn or error on incomplete matches

            val mis: Map[Id, TapExpr] = (m.members.collect { case ASTLet(name, expr) =>
                val id = ModuleId(mId, name)
                id -> TapNode.fromAST(expr, rs, id, Natives.types)
            } ++
            m.members.collect { case tc: ASTTypeClassDefinition =>
                tc.members.collect { case ASTTypeClassMemberImplementation(name, tcm) =>
                    val id = ModuleId(mId, name)
                    id -> TapNode.fromAST(tcm, rs, id, Natives.types)
                }
            }.flatten ++
            m.members.collect { case tci: ASTTypeClassInstance =>
                tci.members.map { case ASTTypeClassMemberImplementation(name, tcim) =>
                    val id = InstId(m.name, ms.tcs(tci.tcName), tci.params map { p => ASTUtil.getTConName(ms.tcons, p) }, name)
                    id -> TapNode.fromAST(tcim, rs, id, Natives.types)
                }
            }.flatten).toMap

            val mDeps = mis mapValues { mi => TapNodeUtil.findImmediateDependencies(mi).toList } filter { case (id, deps) => deps.nonEmpty }
            val extDeps = mDeps.values.flatten.toSet.filter { id => !(mDeps contains id) }

            val xss = Graph.components(mDeps ++ extDeps.map { _ -> Seq.empty })
            xss find { xs => xs.length > 1 } match {
                case Some(xs) => throw ModuleMemberInitCycleError(xs)
                case None => mis
            }
        }

        val mdIds = mdASTs.map { case (mId, md) => ModuleId(mId, md.name) -> md }.toMap
        mdIds find { case (k, v) => !(mis contains k) } match {
            case Some((ModuleId(mId, id), ast)) => throw ModuleMissingImplementationError(mId, id, ast)
            case _ =>
        }

        defs.copy(mis = mis)
    }

    def getMemberType(qId: ModuleId, ttype: ASTType, context: List[ASTTypeClassReference], defs: ModuleDefinitions): Qual[Type] = {
        val ms = scopes(qId.mId)
        val ki = KInfer.constrain(ms.tcons, defs.tcons, qId, Seq(qId), Seq(ttype))
        val km = KInfer.solve(ki, ttype)
        val tvNames = ASTUtil.findTypeVars(ttype, Set.empty)
        val tvs = (tvNames map { p => p -> TVar(p, KInfer(km, Kvar(qId, p))) }).toMap
        val ps1 = getPredicates(ms.tcs, defs.tcs, context, tvs)
        Qual(ps1, ASTUtil.getType(ms.tcons, defs.tcons, tvs, ttype))
    }

    def getPredicates(lookup: Map[String, ModuleId], tcs: Map[ModuleId, TypeclassDef], context: List[ASTTypeClassReference], tvs: Map[String, TVar]): List[IsIn] =
        context map { case ast @ ASTTypeClassReference(tcName, params) =>
            val msntc = lookup(tcName)
            val ks = tcs(msntc).vs map kind
            val ps = (params zip ks) map { case (p, k) =>
                tvs.get(p) match {
                    case None => throw UnknownTypeVariableError(p, ast)
                    case Some(tv) =>
                        if (kind(tv) != k) throw KindMismatchError(p, kind(tv), k, ast)
                        else TVar(p, k)
                }
            }
            IsIn(msntc, ps)
        }

    def lookupInstanceType(lookup: Map[String, ModuleId], tcons: TypeConstructors, ttype: ASTType): Type = ttype match {

        case t: ASTTypeCon => ASTUtil.getType(lookup, tcons, Map.empty, t)

        case t @ ASTTypeApply(thing: ASTTypeCon, params) =>

            // TODO: this can probably be simplified with a more intelligent usage of ASTUtil.findTypeVars and ASTUtil.getType

            val tcon = ASTUtil.getType(lookup, tcons, Map.empty, thing).asInstanceOf[TCon]

            tcon.k match {
                case Star if params.nonEmpty => throw TypeConstructorNoArgsError(tcon, t)
                case Star => throw new Error("Illegal AST: AST Type apply with no parameters")
                case k: Kfun =>
                    val (tvars, _) = params.foldLeft((Map.empty[String, TVar], k: Kind)) { case ((result, kind), param) =>
                        param match {
                            case ast @ ASTTypeVar(name) =>
                                kind match {
                                    case Star => throw TypeConstructorTooManyArgsError(tcon, ast)
                                    case Kfun(x, y) => (result + (name -> TVar(name, x)), y)
                                    case _: Kvar => throw new Error("param kind is Kvar")
                                }
                            case _ => throw new Error("Illegal AST: type constructor is being applied with a non-typevar argument in parameters of typeclass.")
                        }
                    }
                    ASTUtil.getType(lookup, tcons, tvars, t)
                case _: Kvar => throw new Error("tcon kind is Kvar")
            }

        case _ => throw new Error("Illegal AST: typeclass instance parameter is not a type constructor or applied type.")
    }
}