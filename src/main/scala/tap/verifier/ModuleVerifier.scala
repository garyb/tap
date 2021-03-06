package tap.verifier

import tap.ast._
import tap.ir.TapNode.ResolveState
import tap.ir._
import tap.types.Type._
import tap.types._
import tap.types.classes.ClassEnvironments.Inst
import tap.types.classes.{TypeclassDef, Qual, IsIn}
import tap.types.inference.{TIEnv, Substitutions}
import tap.types.kinds.Kind._
import tap.types.kinds._
import tap.util.{trace, Graph}
import tap.util.PrettyPrint._
import tap.util.ContextOps._
import tap.verifier.defs.{DefinitionsLookup, ModuleDefinitions}
import tap.verifier.errors._
import tap.{InstId, ModuleId, Id}
import language.reflectiveCalls

class ModuleVerifier(val scopes: Map[String, DefinitionsLookup]) {

    type ModuleName = String
    type TypeConstructors = Map[ModuleId, TCon]
    type TypeVars = Map[String, TVar]

    def apply(modules: Seq[ASTModule], verifiedDefs: ModuleDefinitions, env0: TIEnv): (TIEnv, ModuleDefinitions) = {

        val dtASTs = modules.flatMap { m => m.members.collect { case dtd: ASTDataType => m.name -> dtd } }
        val tcATSs = modules.flatMap { m => m.members.collect { case tcd: ASTClass => m.name -> tcd } }
        val instASTs = modules.flatMap { m => m.members.collect { case tci: ASTClassInst => m.name -> tci } }
        val mdASTs = modules.flatMap { m => m.members.collect { case md: ASTDef => m.name -> md } }

        val (env1, defs1) = addDataTypeDefs(env0, dtASTs, verifiedDefs)
        val        defs2  = addTypeclassDefs(tcATSs, defs1)
        val (env2, defs3) = addTypeclassInstances(env1, instASTs, defs2)
        val (env3, defs4) = addMemberDefs(env2, mdASTs, defs3)
        val (env4, defs5) = addTypeclassMemberDefs(env3, tcATSs, defs4)
        val        defs6  = addMemberImplementations(modules, mdASTs, defs5)
        (env4, defs6)
    }

    def addDataTypeDefs(env: TIEnv, dtASTs: Seq[(ModuleName, ASTDataType)], defs: ModuleDefinitions): (TIEnv, ModuleDefinitions) = {

        // Check the data types do not conflict with imported definitions in the modules they belong to
        dtASTs foreach { case (mId, dt) =>
            val moduleScope = scopes(mId)
            moduleScope.tcons.get(dt.name) match {
                case Some(id) if id.mId != mId => throw NamespaceError("type constructor", dt.name, dt)
                case _ =>
            }
            dt.constructors foreach { dcon =>
                moduleScope.dcons.get(dcon.name) match {
                    case Some(id) if id.mId != mId => throw NamespaceError("data constructor", dt.name, dt)
                    case _ =>
                }
            }
        }

        val dtASTLookup = dtASTs.foldLeft(Map.empty: Map[ModuleId, ASTDataType]) { case (result, (mId, dt)) =>
            val id = ModuleId(mId, dt.name)
            if (result contains id) throw ModuleDuplicateDefinition(mId, "type constructor", dt.name, dt)
            result + (id -> dt)
        }

        // Find the type constructor dependencies
        val tconDeps = dtASTLookup map { case (id @ ModuleId(mId, _), ASTDataType(_, _, dcons)) =>
            id -> (dcons flatMap { dcon =>
                val tcons = ASTUtil.findAllTypeConstructors(scopes(mId).tcons, dcon.args)
                // Exclude dependencies that have already been resolved
                tcons filterNot { d => defs.tcons contains d }
            })
        }

        // Type constructors can have circular dependencies, so find and sort the strongly connected components
        val tconOrd = Graph.components(tconDeps)

        // Extract the type constructors and data constructors for each module
        val (env1, tcons, dcons) = tconOrd.foldLeft((env, defs.tcons, defs.dcons)) { case ((env0, tcons0, dcons0), currentDtdNames) =>

            // The ASTs for the current group of data type definitions
            val currentDtds = currentDtdNames.zip (currentDtdNames map { msn => dtASTLookup(msn) }).toMap

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
                case ((tcons, dtdts, penvs), (id, dtd)) =>

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

                    (tcons + (id -> tcon), dtdts + (id -> t), penvs + (id -> penv))
            }

            // Create function types for the data constructors.
            val (env1, dcons1) = currentDtds.foldLeft((env0, dcons0)) { case ((env0, dcs), (id, dtd)) =>
                dtd.constructors.foldLeft((env0, dcs)) { case ((env0, dcons), dcon) =>
                    val (env1, at) = dcon.args match {
                        case Seq() => (env0, dts(id))
                        case as =>
                            val (env1, as1) = env0.map(as) { (env0, a) =>
                                val (env1, _, a1) = ASTUtil.getType(env0, scopes(id.mId).tcons, tcons1, dtenvs(id), a)
                                (env1, a1)
                            }
                            (env1, as1.foldRight(dts(id): Type) { (x, y) => x fn y })
                    }
                    val did = ModuleId(id.mId, dcon.name)
                    if (dcons contains did) throw ModuleDuplicateDefinition(id.mId, "data constructor", dcon.name, dtd)
                    val (env2, _, qt) = Type.quantify(env1, Type.tv(at), at)
                    (env2, dcons + (did -> qt))
                }
            }

            (env1, tcons1, dcons1)
        }

        (env1, defs.copy(tcons = tcons, dcons = dcons))
    }

    def addTypeclassDefs(tcASTs: Seq[(ModuleName, ASTClass)], defs: ModuleDefinitions): ModuleDefinitions = {

        // Check typeclass definitions do not conflict with imported definitions in the modules they belong to
        tcASTs foreach { case (mId, m) =>
            scopes(mId).tcs.get(m.name) match {
                case Some(id) if id.mId != mId => throw NamespaceError("typeclass", m.name, m)
                case _ =>
            }
        }

        // Construct a map that allows the classes to be looked up by module qualified name, also, ensure there are no
        // duplicate definitions
        val tcASTsLookup = tcASTs.foldLeft(Map.empty: Map[ModuleId, ASTClass]) { case (result, (mId, tc)) =>
            val id = ModuleId(mId, tc.name)
            if (result contains id) throw ModuleDuplicateDefinition(mId, "typeclass", tc.name, tc)
            result + (id -> tc)
        }

        tcASTs foreach { case (mId, ast @ ASTClass(_, _, ps, _)) =>
            if (ps.isEmpty) throw new VerifierMiscError("Typeclass has no type variables", ast)
        }

        // Build the list of dependencies each typeclass has (dependencies being superclasses - typeclasses referenced
        // in the context of the current typeclass definition)
        val tcDeps = tcASTsLookup.map { case (id, tcd @ ASTClass(_, context, _, _)) =>
            id -> context.map { case ASTClassRef(name, _) =>
                scopes(id.mId).tcs.get(name) match {
                    case None => throw UnknownTypeclassError(name, tcd)
                    case Some(tcRef) => tcRef
                }
            }
        }

        // Sort typeclasses to ensure they resolve in the correct order. For example, Ord depends on Eq, so the result
        // of this sort will place Eq ahead of Ord in the list.
        val tcOrd = try {
            Graph.tsort(tcDeps).filter { msn => tcASTsLookup.contains(msn) }
        } catch {
            case e: IllegalArgumentException => throw TypeclassRecursiveHeirarchyError(tcDeps.keys)
        }

        // Resolve the typeclasses
        val tcs = tcOrd.foldLeft(defs.tcs) { case (tcs, id) =>

            val currLookup = scopes(id.mId)

            // When ordering the typeclasses we discarded the AST nodes in favour of module-qualified identifiers,
            // so get the AST for the current definition
            val ast @ ASTClass(_, context, tyvars, members) = tcASTsLookup(id)

            // Add kind inference constraints from superclasses
            val kcs0 = context.foldLeft(List.empty[(Kind, Kind)]) { case (kcs, ast @ ASTClassRef(supername, params)) =>

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
                case ast @ ASTClassMemberDef(name, ASTQType(_, ttype)) =>
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
                case (result, ast @ ASTClassMemberDef(memn, _)) =>
                    if (result contains memn) throw TypeclassDuplicateMemberDefinitionError(id, memn, ast)
                    else result + memn
                case (result, _) => result
            }

            // Extract names of members implemented within the typeclass
            val defaultMemberNames = members.foldLeft(Set.empty[String]) {
                case (result, ast @ ASTClassMemberImpl(memn, _)) =>
                    if (!(memberNames contains memn)) throw TypeclassImplementsUnknownMemberError(id, memn, ast)
                    if (result contains memn) throw TypeclassDuplicateMemberImplementationError(id, memn, ast)
                    else result + memn
                case (result, _) => result
            }

            tcs + (id -> TypeclassDef(id, contextParams, pts, memberNames, defaultMemberNames).setFilePosFrom(ast))
        }

        defs.copy(tcs = tcs)
    }

    def addTypeclassInstances(env: TIEnv, instASTs: Seq[(ModuleName, ASTClassInst)], defs: ModuleDefinitions): (TIEnv, ModuleDefinitions) = {

        val (env1, tcis) = instASTs.foldLeft((env, defs.tcis)) { case ((env0, result), (mId, ast @ ASTClassInst(name, context, params, members))) =>

            val tconLookup = scopes(mId).tcons
            val tcIds = scopes(mId).tcs
            val tcId = tcIds.getOrElse(name, throw new UnknownTypeclassError(name, ast))
            val tc = defs.tcs.getOrElse(tcId, throw new UnknownTypeclassError(name, ast))

            // TODO: get all type variables used in concrete applied types
            // TODO: check kinds of type variables used all work out
            val usedParams = params.collect {
                case p @ ASTTypeApply(tc, _) if ASTUtil.isConcrete(tc) => ASTUtil.findTypeVars(p, Set.empty)
            }.flatten

            if (params.length != tc.vs.length) throw TypeclassArityError(name, tc.vs.length, params.length, ast)

            val (env1, ps) = env0.map(params zip tc.vs) {
                case (env0, (param: ASTTypeVar, _)) => throw TypeclassIllegalParameterError("Typeclass parameters cannot be type variables " + (usedParams contains param.name), param)
                case (env0, (param, tcp)) =>
                    val (env1, t) = lookupInstanceParamType(env0, tconLookup, defs.tcons, param)
                    if (kind(tcp) != kind(t)) throw TypeclassIllegalParameterError("Type parameter kind is wrong: '" + kind(t) + "' should be '" + kind(tcp) + "' " + prettyPrint(t), param)
                    (env1, t)
            }

            val tvs = (ps flatMap { p => Type.tv(p) } map { tv => tv.id -> tv }).toMap

            val preds = getPredicates(tcIds, defs.tcs, context, tvs)

            val membersImplemented = members.foldLeft(Set.empty[String]) {
                case (result, ast @ ASTClassMemberImpl(memn, _)) =>
                    if (!(tc.members contains memn)) throw InstanceUnknownMemberError(tc, ps, memn, ast)
                    if (result contains memn) throw InstanceDuplicateMemberError(tc, ps, memn, ast)
                    else result + memn
            }

            val membersUnimplemented = tc.members -- membersImplemented -- tc.defaultMembers
            if (membersUnimplemented.nonEmpty) throw InstanceIncompleteError(tc, ps, membersUnimplemented, ast)

            val existingInsts = result.getOrElse(tcId, List.empty)
            val inst = Inst(mId, preds, IsIn(tcId, ps)).setFilePosFrom(ast)
            (env1, result + (tcId -> (inst :: existingInsts)))
        }

        (env1, defs.copy(tcis = tcis))
    }

    def addMemberDefs(env: TIEnv, mdASTs: Seq[(ModuleName, ASTDef)], defs: ModuleDefinitions): (TIEnv, ModuleDefinitions) = {

        // Check the member definitions do not conflict with imported definitions in the modules they belong to
        mdASTs foreach { case (mId, m) =>
            scopes(mId).members.get(m.name) match {
                case Some(id) if id.mId != mId => throw NamespaceError("member", m.name, m)
                case _ =>
            }
        }

        val (env1, mts) = mdASTs.foldLeft((env, defs.mts)) { case ((env0, result), (mId, ast @ ASTDef(id, qtype))) =>
            val qId = ModuleId(mId, id)
            if (result contains qId) throw throw ModuleDuplicateDefinition(mId, "member", id, ast)
            val (env1, mt) = getMemberType(env0, qId, qtype, defs)
            (env1, result + (qId -> mt))
        }

        (env1, defs.copy(mts = mts))
    }

    def addTypeclassMemberDefs(env: TIEnv, tcASTs: Seq[(ModuleName, ASTClass)], defs: ModuleDefinitions): (TIEnv, ModuleDefinitions) = {

        val (env1, mts) = tcASTs.foldLeft((env, defs.mts)) { case ((env0, result), (mId, ASTClass(tcName, _, _, members))) =>

            val currScope = scopes(mId)
            val msntc = currScope.tcs(tcName)
            val tc = defs.tcs(msntc)
            val pred = IsIn(msntc, tc.vs)

            members.foldLeft((env0, result)) {
                case ((env0, result), ast @ ASTClassMemberDef(id, qtype)) =>
                    currScope.members.get(id) match {
                        case Some(msn) if msn.mId != mId => throw NamespaceError("member", id, ast)
                        case _ =>
                    }
                    val qId = ModuleId(mId, id)
                    if (result contains qId) throw ModuleDuplicateDefinition(mId, "member", id, ast)
                    val (env1, mt) = getMemberType(env0, qId, qtype, defs, Some(pred))
                    (env1, result + (qId -> mt))
                case (result, _) => result
            }
        }

        (env1, defs.copy(mts = mts))
    }

    def addMemberImplementations(modules: Seq[ASTModule], mdASTs: Seq[(ModuleName, ASTDef)], defs: ModuleDefinitions): ModuleDefinitions = {

        val mis = defs.mis ++ modules.flatMap { m =>
            val mId = m.name
            val ms = scopes(mId)
            val rs = ResolveState(ms.dcons, ms.members, Set.empty, ms.tcons, defs.tcons)

            val mis: Seq[(Id, TapExpr)] = m.members.collect { case ASTLet(name, expr) =>
                val id = ModuleId(mId, name)
                id -> TapNode.fromAST(expr, rs)
            } ++
            m.members.collect { case tc: ASTClass =>
                tc.members.collect { case ASTClassMemberImpl(name, tcm) =>
                    val id = ModuleId(mId, name)
                    id -> TapNode.fromAST(tcm, rs)
                }
            }.flatten ++
            m.members.collect { case tci: ASTClassInst =>
                tci.members.map { case ASTClassMemberImpl(name, tcim) =>
                    val id = InstId(m.name, ms.tcs(tci.tcName), tci.params map { p => ASTUtil.getTConName(ms.tcons, p) }, name)
                    id -> TapNode.fromAST(tcim, rs)
                }
            }.flatten

            val miMap = mis.foldLeft(Map.empty: Map[Id, TapExpr]) {
                case (result, kv @ (id, expr)) =>
                    if (result contains id) throw ModuleDuplicateDefinition(mId, "member", id.id, expr)
                    result + kv
            }

            val mDeps = miMap mapValues { mi => TapNodeUtil.findImmediateDependencies(mi).toList } filter { case (id, deps) => deps.nonEmpty }

            mDeps find { case (id, deps) => deps contains id } match {
                case Some((id, _)) => throw ModuleMemberInitRecursiveError(id)
                case None =>
            }

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

    def getPredicates(lookup: Map[String, ModuleId], tcs: Map[ModuleId, TypeclassDef], context: List[ASTClassRef], tvs: Map[String, TVar]): List[IsIn] =
        context map { case ast @ ASTClassRef(tcName, params) =>
            val msntc = lookup(tcName)
            val ks = tcs(msntc).vs map kind
            if (ks.length != params.length) throw TypeclassArityError(tcName, ks.length, params.length, ast)
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

    def getMemberType(env: TIEnv, qId: ModuleId, qtype: ASTQType, defs: ModuleDefinitions, classPred: Option[IsIn] = None): (TIEnv, Qual[Type]) = {
        val ASTQType(context, ttype) = qtype
        val ms = scopes(qId.mId)
        val ki = KInfer.constrain(ms.tcons, defs.tcons, qId, Seq(qId), Seq(ttype))
        val km = KInfer.solve(ki, ttype)
        val tvNames = ASTUtil.findTypeVars(ttype, Set.empty)
        val tvs = (tvNames map { p => p -> TVar(p, KInfer(km, Kvar(qId, p))) }).toMap
        val ps0 = getPredicates(ms.tcs, defs.tcs, context, tvs)
        val ps1 = classPred match {
            case Some(p) => p :: ps0
            case None => ps0
        }
        val (env0, s0, t0) = ASTUtil.getType(env, ms.tcons, defs.tcons, tvs, ttype)
        val (env1, s1, t1) = Type.quantify(env0, tvs.values.toList, t0)
        val s2 = s0 ++ s1
        (env1, Qual(ps1 map { p => Substitutions.applySubst(s2, p) }, t1))
    }

    def lookupInstanceParamType(env: TIEnv, lookup: Map[String, ModuleId], tcons: TypeConstructors, ttype: ASTType): (TIEnv, Type) = ttype match {

        case t: ASTTypeCon =>
            val (env1, _, t1) = ASTUtil.getType(env, lookup, tcons, Map.empty, t)
            (env1, t1)

        case t @ ASTTypeApply(thing: ASTTypeCon, params) =>

            // TODO: this can probably be simplified with a more intelligent usage of ASTUtil.findTypeVars and ASTUtil.getType

            val tcon = ASTUtil.getType(env, lookup, tcons, Map.empty, thing)._3.asInstanceOf[TCon]

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
                    val (env1, _, t1) = ASTUtil.getType(env, lookup, tcons, tvars, t)
                    (env1, t1)
                case _: Kvar => throw new Error("tcon kind is Kvar")
            }

        case _ => throw new Error("Illegal AST: typeclass instance parameter is not a type constructor or applied type.")
    }
}