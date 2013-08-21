package tap.verifier

import annotation.tailrec
import tap.ir._
import tap.ir.TapNodeUtil._
import tap.types.Type._
import tap.types.Natives._
import tap.types._
import tap.types.kinds._
import tap.types.kinds.Kind._
import tap.types.classes.ClassEnvironments.Inst
import tap.types.classes.{TypeclassDef, Qual, IsIn}
import tap.types.inference.Substitutions.{applySubst, Subst}
import tap.types.inference.{TIEnv, Substitutions}
import tap.util._
import tap.util.ContextOps._
import tap.util.PrettyPrint._
import tap.verifier.defs.{ModuleDefinitions, SimpleDefinitions}
import tap.{LocalId, InstId, ModuleId, Id}
import language.implicitConversions
import language.reflectiveCalls

/**
 * Notes:
 *      - subst needs apply to ets at the end.
 *      - predicate args are inserted in front of the arg that decides their type. this is so the arg can be inserted
 *        before return types in the case of `read`, etc.
 *
 * @param defs The definitions to rewrite.
 */
class TypeclassInlining(defs: ModuleDefinitions, initEnv: TIEnv)
{
    val tcs = defs.tcs

    type TCons = Map[ModuleId, TCon]
    type DCons = Map[ModuleId, (Id, Type)]
    type Impls = Map[ModuleId, TapExpr]
    type ExprTypes = Map[TapNode, Qual[Type]]
    type TCInstMembers = Map[InstId, ModuleId]

    case class Context(env: TIEnv, tcTcons: TCons, tcDcons: DCons, tcInstNames: Map[Inst, ModuleId], tcInstMembers: TCInstMembers, tcDefaultMembers: Map[ModuleId, ModuleId]) {

        /**
         * Takes an expression that is chain of applications and a type for the function at the bottom of the chain, and
         * assigns the appropriate types to each of the apply nodes.
         */
        def setApplyTypes(e: TapExpr, ft: Type): (Context, TapExpr) = {
            val ts = getFunctionTypes(ft)
            @tailrec def setTypes(ctx: Context, e: TapExpr, depth: Int): Context = e match {
                case a @ ApplyExpr(fn, arg) =>
                    // TODO: set arg types as well
                    val ctx1 = ctx.setExprType(a, makeFunctionType(ts.drop(depth)))._1
                    if (depth > 0) setTypes(ctx1, fn, depth - 1) else ctx1
                case v =>
                    ctx.setExprType(v, ft)._1
            }
            (setTypes(this, e, getFunctionArity(ft)), e)
        }

        /**
         * Takes an expression that is chain of function defs and a type for the function at the top of the chain, and
         * assigns the appropriate types to each of the function and arg nodes.
         */
        def setFunctionTypes(e: TapExpr, ft: Type): (Context, TapExpr) = {
            @tailrec def setTypes(ctx: Context, e: TapExpr, ts: List[Type]): Context = e match {
                case a @ FunctionExpr(arg, body) =>
                    val ctx1 = ctx.setExprType(a, ft)._1
                                  .setExprType(arg, ts.head)._1
                    setTypes(ctx1, body, ts.tail)
                case v => ctx.setExprType(v, makeFunctionType(ts))._1
            }
            (setTypes(this, e, getFunctionTypes(ft)), e)
        }

        /**
         * Finds a typeclass instance based on a typeclass ID and a list of type arguments.
         */
        def getInst(tcId: Id, pArgs: List[Id]): Inst = {
            tcInstNames.keys find { case Inst(_, _, IsIn(tcId0, params)) =>
                tcId == tcId0 && pArgs.size == params.size && ((params map getTConID) == pArgs)
            } match {
                case Some(tci) => tci
                case None => throw new Error("No instance found for " + prettyPrint(tcId) + " " + (pArgs mkString ", "))
            }
        }

        /**
         * Finds the remapped name of a typeclass instance based on a typeclass ID and a list of type arguments.
         */
        def getInstName(tcId: Id, pArgs: List[Id]): ModuleId = tcInstNames(getInst(tcId, pArgs))

        private def withEnv[A](next: (TIEnv, A)) = (copy(env = next._1), next._2)
        def freshInst(s: Qual[Type]): (Context, Qual[Type]) = withEnv(env.freshInst(s))
        def freshInst(s: Type): (Context, Type) = withEnv(env.freshInst(s))
        def freshInstPartial(ts0: List[Type], s: Type): (Context, Type) = withEnv(env.freshInstPartial(ts0, s))
        def setExprType[E <: TapNode](e: E, t: Type): (Context, E) = (copy(env = env.setNodeType(e, t)), e)
    }

    /**
     * Map over nested lists of depth 2, useful for mapping over the argMods list generated by findPredicateArgs.
     */
    implicit def map2enabler[A](xss: List[List[A]]) = new {
        def map2[B](fn: A => B): List[List[B]] = xss map { xs => xs map fn }
    }

    def apply(): (TIEnv, SimpleDefinitions) = {

        val memberIds = defs.mts.keySet

        trace("\nTypeclass data constructors:\n--------------------------------------------------------------------------------")
        val (env1, tcTcons, tcDcons) = createTypeclassDatatypes(initEnv, defs.tcons.keySet, defs.mts)
        for ((id, (dconId, dcon)) <- tcDcons) trace(prettyPrint(dconId), "::", prettyPrint(dcon))

        trace("\nTypeclass instance naming:\n--------------------------------------------------------------------------------")
        val tcInstNames = createTypeclassInstanceNames(defs.tcis.values.flatten.toList, memberIds, Map.empty)
        val memberIds1 = memberIds ++ tcInstNames.values.toSet
        for ((tci, nid) <- tcInstNames) trace("instance " + prettyPrint(tci.tc.id) + " (" + (tci.tc.ts map prettyPrint mkString ", ") + ")", "=>", prettyPrint(nid))

        trace("\nTypeclass default member renaming:\n--------------------------------------------------------------------------------")
        val tcDefaultImplNames = renameTypeclassDefaultMembers(tcs.values.toList, memberIds1, Map.empty)
        val memberIds2 = memberIds1 ++ tcDefaultImplNames.values.toSet
        for ((oid, nid) <- tcDefaultImplNames) trace(prettyPrint(oid), "=>", prettyPrint(nid))

        trace("\nTypeclass instance member renaming:\n--------------------------------------------------------------------------------")
        val tcInstImplNames = renameTypeclassInstanceMembers(defs.mis.keys.toList, tcInstNames, memberIds2, Map.empty)
        for ((oid, nid) <- tcInstImplNames) trace(prettyPrint(oid), "=>", prettyPrint(nid))

        trace("\nTypeclass stub implementations:\n--------------------------------------------------------------------------------")
        val (ctx1, tcStubImpls) = createTypeclassStubImplementations(tcs.values.toList, defs.mts, Context(env1, tcTcons, tcDcons, tcInstNames, tcInstImplNames, tcDefaultImplNames), Map.empty)
        for ((id, fn) <- tcStubImpls) {
            val t = ctx1.env.ets(fn)
            trace()
            trace(id, "::", prettyPrint(t))
            trace(" " * id.toString.length, "  ", prettyPrint(fn))
        }

        trace("\nTypeclass instances:\n--------------------------------------------------------------------------------")
        val (ctx2, tcInsts) = createTypeclassInstances(defs.tcis.values.flatten.toList, ctx1)
        for ((id, fn) <- tcInsts) {
            trace()
            trace(id, "::", prettyPrint(ctx2.env.ets(fn)))
            trace(" " * id.toString.length, "  ", prettyPrint(fn))
        }

        trace("\nMember implementations:\n--------------------------------------------------------------------------------")
        val (ctx3, impls) = rewriteMemberImplementations(defs.mis, tcDefaultImplNames, tcInstImplNames, ctx2)
        for ((id, fn) <- impls.toList.sortBy { case (id, e) => prettyPrint(e) }) {
            trace()
            trace(id, "::", prettyPrint(applySubst(ctx3.env.s, ctx3.env.ets(fn))))
            trace(" " * id.toString.length, "  ", prettyPrint(fn))
        }

        // TODO: there's some confusion around TCons and DCons here, DCons should be the Foralls to match the representation of the defs that came in
        /*SimpleDefinitions(
            defs.tcons ++ (tcTcons mapValues { sc => getTCon(sc.qt.h) }),
            (defs.dcons mapValues { sc => freshInst(sc).h }) ++ (tcDcons mapValues { case (id, t) => t }),
            combineMis(List(tcStubImpls, tcDefaultImplNames.values.flatten.toMap, impls, tcInsts)),
            ctx3.ets map { case (node, qt) =>
                if (qt.ps.nonEmpty) throw new Error("ets still contains qualified type: " + prettyPrint(qt) + " for " + prettyPrint(node))
                node -> qt.h
            })*/
        (ctx3.env, SimpleDefinitions(
            Map.empty,
            Map.empty,
            Map.empty,
            Map.empty))
    }

    /**
     * Combines member implementation maps ensuring no collisions occur.
     */
    @tailrec final def combineMis(xs: List[Impls], result: Impls = Map.empty): Impls = xs match {
        case Seq() => result
        case x :: xs => x.keys find { xk => result contains xk } match {
            case None => combineMis(xs, x ++ result)
            case Some(k) => throw new Error("There seems to be a duplicate definition for " + k)
        }
    }

    /**
     * Creates type constructors and data constructors for typeclasses.
     */
    def createTypeclassDatatypes(env: TIEnv, reservedIds: Set[ModuleId], mts: Map[Id, Qual[Type]]): (TIEnv, TCons, DCons) = {

        @tailrec def loop(env: TIEnv, xs: List[TypeclassDef], tcons: TCons, dcons: DCons): (TIEnv, TCons, DCons) = xs match {
            case List() => (env, tcons, dcons)
            case TypeclassDef(msn @ ModuleId(mId, name), ps, vs, memberIds, _) :: xs =>

                // Generate a name for the type constructor, ensuring we don't overwrite anything that already exists
                val moduleTcons = (reservedIds ++ tcons.keys) collect { case ModuleId(mId2, name) if mId == mId2 => name }
                val tconName = ModuleId(mId, IDUtil.makeId("TC" + name, moduleTcons))

                // Generate argument types for the superclasses
                val scs = ps map { p => predToType(p, tcons) }

                // Generate argument types for the members
                val (env1, tcmts) = env.map(memberIds.toList) { (env, m) =>
                    val sc = mts(ModuleId(mId, m))
                    if (sc.ps(0).id != msn) throw new Error("Typeclass member incorrect predicate found - " + m + " :: " + prettyPrint(sc))
                    val (env1, qt) = env.freshInstPartial(vs, sc)
                    val tvs = Qual.tv(qt) filterNot { tv => vs contains tv }
                    (env1, if (tvs.nonEmpty) Qual.quantify(tvs, qt)._2.h else qt.h)
                }

                // Build the type constructor for the typeclass
                val tcon = TCon(tconName, (vs map kind).foldRight(Star: Kind)(Kfun.apply))

                // Create an instantiated type from the type constructor
                val t = vs.foldLeft(tcon: Type) { (rt, v) => TAp(rt, v) }

                // Create the data constructor type - a function type that accepts superclass and member argument types
                // and returns the instantiated type constructor.
                val dcon = (scs ++ tcmts).foldRight(t) { _ fn _ }

                val sc = Type.quantify(tv(dcon), dcon)
                trace(prettyPrint(msn))
                trace("-", prettyPrint(tcon))
                trace("-", prettyPrint(Qual.quantify(tv(t), Qual(Nil, t))._2))
                trace("-", prettyPrint(dcon))
                trace("-", prettyPrint(sc._2))

                loop(env1, xs, tcons + (msn -> tcon), dcons + (msn -> (tconName -> sc._2)))
        }

        // Loop through the typeclasses in dependency order
        val tcDeps = tcs mapValues { tc => tc.ps map { p => p.id } }
        val tcOrd = Graph.tsort(tcDeps) map { k => k -> tcs(k) }
        loop(env, tcOrd map { _._2 }, Map.empty, Map.empty)
    }

    /**
     * Creates member names for typeclass instance constructor functions.
     */
    @tailrec final def createTypeclassInstanceNames(xs: List[Inst], reservedIds: Set[Id], result: Map[Inst, ModuleId]): Map[Inst, ModuleId] = xs match {
        case List() => result
        case (inst @ Inst(mId, ps, tc)) :: xs =>
            val moduleReservedIds = reservedIds collect { case ModuleId(mId1, mName) if mId == mId1 => mName }
            val pIds = tc.ts map { p => getTConID(p).id } mkString ""
            val newId = ModuleId(mId, IDUtil.makeId("tc" + tc.id.id + pIds, moduleReservedIds))
            createTypeclassInstanceNames(xs, reservedIds + newId, result + (inst -> newId))
    }

    /**
     * Creates a mapping specifying a new name for the default implementations for typeclass members. Without this
     * mapping there would be a namespace collision between the stub methods and default methods.
     */
    @tailrec final def renameTypeclassDefaultMembers(xs: List[TypeclassDef], reservedIds: Set[Id], result: Map[ModuleId, ModuleId]): Map[ModuleId, ModuleId] = xs match {
        case List() => result
        case TypeclassDef(ModuleId(mId, tcName), _, _, _, defaultMembers) :: xs =>
            val moduleMemberIds = reservedIds collect { case ModuleId(mId1, mName) if mId == mId1 => mName }
            val (reservedIds1, result1) = defaultMembers.toList.foldLeft((reservedIds, result)) {
                case ((reservedIds, result), m) =>
                    val msn = ModuleId(mId, m)
                    val msn1 = ModuleId(mId, IDUtil.makeId(m + "_default_", moduleMemberIds))
                    (reservedIds + msn1, result + (msn -> msn1))
            }
            renameTypeclassDefaultMembers(xs, reservedIds1, result1)
    }

    /**
     * Creates a mapping for typeclass instance member implementations, providing non-instance-scoped identifiers.
     */
    @tailrec final def renameTypeclassInstanceMembers(xs: List[Id], tcInstNames: Map[Inst, ModuleId], reservedIds: Set[Id], result: TCInstMembers): TCInstMembers = xs match {
        case List() => result
        case (x @ InstId(mId, tc, params, name)) :: xs =>
            val moduleMemberIds = reservedIds collect { case ModuleId(mId1, mName) if mId == mId1 => mName }
            val pNames = params map { p => p.id } mkString ""
            val newId = ModuleId(mId, IDUtil.makeId(name + pNames, moduleMemberIds))
            renameTypeclassInstanceMembers(xs, tcInstNames, reservedIds + newId, result + (x -> newId))
        case _ :: xs =>
            renameTypeclassInstanceMembers(xs, tcInstNames, reservedIds, result)
    }

    /**
     * Creates the "stub" implementations for typeclass members. These functions accept all the arguments of the
     * original function, but in addition capture an argument that specifies which typeclass to use. The body of the
     * generated function destructures the typeclass, and then applies the capture arguments to the appropriate
     * function in the typeclass.
     *
     * For example, a function with type:
     *   ((Plus x) => x -> x -> y)
     * will be implemented something like this:
     *   (lambda (tc a b) (match tc (case (TCPlus fn) (fn a b))))
     * with a rewritten type of:
     *   (TCPlus x -> x -> x -> y)
     *
     * The typeclass argument is inserted at the point in the arguments where the predicate becomes deciable, for the
     * type:
     *   ((Plus x) => z -> x -> y)
     * the stub implementation would be something like this:
     *   (lambda (a tc b) (match tc (case (TCPlus fn) (fn a b))))
     * and typed as:
     *   (z -> TCPlus x -> x -> y)
     */
    @tailrec final def createTypeclassStubImplementations(xs: List[TypeclassDef], mts: Map[Id, Qual[Type]], ctx: Context, result: Impls): (Context, Impls) = xs match {
        case List() => (ctx, result)
        case TypeclassDef(msn @ ModuleId(mId, name), context, _, members, _) :: xs =>
            val dconId = ctx.tcDcons(msn)._1
            val (ctx1, members1) = ctx.map(members.toList) { (ctx, m) =>

                val sc = mts(ModuleId(mId, m))
                val (ctx1, qt) = ctx.freshInst(sc)
                val argMods = findPredicateArgs(ctx1.env.s, qt)
                val tcIndex = argMods indexWhere { ps => ps.nonEmpty }

                // TODO: need to call setExprType for all generated exprs, not just the outmost one
                def makeTypeclassMember(t: Type, i: Int, args: List[String]): TapExpr = t match {

                    case TAp(TAp(tArrow, a), b) =>
                        val argId = IDUtil.makeAlphabeticId(i, args.toSet)
                        val e = makeTypeclassMember(b, i + 1, argId :: args)
                        FunctionExpr(Argument(argId), e)

                    case _ =>
                        val tcId = args(args.size - tcIndex - 1)
                        val fnId = IDUtil.makeAlphabeticId(i, args.toSet)
                        val unapplyArgs = context.toList.map { _ => WildcardValueExpr } ++ members map { m1 => if (m == m1) BindNode(fnId, None) else WildcardValueExpr }
                        val applyArgs = args collect { case id if id != tcId => ValueReadExpr(LocalId(id)) }
                        val fnApply = makeApply(ValueReadExpr(LocalId(fnId)), applyArgs)
                        val cs = List(MatchCase(UnapplyNode(dconId, unapplyArgs), None, fnApply))
                        MatchExpr(ValueReadExpr(LocalId(tcId)), cs)
                }

                val t = interleavePredicateArgTypes(ctx1.env.s, qt.h, argMods, ctx1.tcTcons)
                val fn = makeTypeclassMember(t, 0, Nil)
                val id = ModuleId(mId, m)
                val (ctx2, _) = ctx1.setExprType(fn, t)
                (ctx2, id -> fn)
            }
            createTypeclassStubImplementations(xs, mts, ctx1, result ++ members1)
    }

    /**
     * Creates the members that return typeclass instances.
     */
    def createTypeclassInstances(xs: List[Inst], ctx: Context): (Context, Impls) = {

        @tailrec def loop(xs: List[Inst], ctx: Context, result: Impls): (Context, Impls) = xs match {
            case List() => (ctx, result)
            case (inst @ Inst(mId, ps, p @ IsIn(tcId, params))) :: xs =>

                val predIds = (ps map { p => p -> predToArgName(p, Set.empty) }).toMap
                val paramNames = params map { t => getTConID(t) }
                val id = ctx.getInstName(tcId, paramNames)
                val (dconId, dconType) = ctx.tcDcons(tcId)

                val tc = tcs(tcId)
                val s = (tc.vs zip params).toMap

                val (ctx1, superArgs) = ctx.map(tc.ps) { (ctx, p) =>
                    predToVal(Substitutions.applySubst(s, p), ctx, predIds)
                }

                val (ctx2, dt) = ctx1.freshInst(dconType)
                val typeArgs = getFunctionTypeArgs(dt).drop(tc.ps.length)
                val memberDefs = tc.members.toList zip typeArgs
                val (ctx3, memberArgs) = ctx2.map(memberDefs.toList) { case (ctx, (memberId, t)) =>
                    ctx.tcInstMembers.get(InstId(mId, tcId, paramNames, memberId)) match {
                        case Some(m) =>
                            // Instance members need the instance context passing to them
                            // TODO: set types
                            val e = makeApply(ValueReadExpr(m), predIds.values.toList map { id => ValueReadExpr(LocalId(id)) })
                            ctx.setExprType(e, t)
                        case None =>
                            // Default members don't need to know about the instance context, but we do need to provide
                            // a reference to the current typeclass implementation. As we're currently constructing
                            // that, we can't make a reference to it yet, so eta-abstraction is used to capture the
                            // rest of the arguments, and the reference is added to the inside so it is only evaluated
                            // when the member is called with all of its arguments.
                            // TODO: set types
                            val numArgs = getFunctionArity(t)
                            val argIds = List.range(0, numArgs) map { i => IDUtil.makeAlphabeticId(i, Set.empty) }
                            val e0 = ValueReadExpr(ctx.tcDefaultMembers(ModuleId(tcId.mId, memberId)))
                            val self = makeApply(ValueReadExpr(id), predIds.values.toList map { id => ValueReadExpr(LocalId(id)) })
                            val e1 = makeApply(e0, self :: (argIds map { id => ValueReadExpr(LocalId(id))}))
                            (ctx, makeFunc(argIds map { id => Argument(id) }, e1))
                    }
                }

                // TODO: set types
                val f = makeApply(ValueReadExpr(dconId), superArgs ++ memberArgs)
                val e = makeFunc(predIds.values.toList map { id => Argument(id) }, f)
                val (ctx4, _) = ctx3.setExprType(e, predToType(p, ctx.tcTcons))

                loop(xs, ctx4, result + (id -> e))
        }

        loop(xs, ctx, Map.empty)
    }

    /**
     * Rewrites the members of the module using all the previously gathered information about typeclasses, instances,
     * and defaults.
     */
    def rewriteMemberImplementations(xs: Map[Id, TapExpr], defaults: Map[ModuleId, ModuleId], instanceMembers: TCInstMembers, ctx: Context): (Context, Impls) = {
        ctx.map(xs) { case (ctx, (qId, mi)) =>
            val (newId, (ctx1, impl)) = qId match {
                case qId: InstId => instanceMembers(qId) -> rewriteInstanceMember(mi, ctx)
                case qId: ModuleId if defaults contains qId => defaults(qId) -> rewriteInstanceMember(mi, ctx)
                case qId: ModuleId =>  qId -> rewriteMember(mi, ctx)
                case qId => throw new Error("Invalid id in rewriteMemberImplementations: " + prettyPrint(qId))
            }
            (ctx1, newId -> impl)
        }
    }

    def rewriteInstanceMember(expr: TapExpr, ctx: Context): (Context, TapExpr) = {
        val t0 = ctx.env.ets(expr)
        expr match {

            case f @ FunctionExpr(arg, body) if t0.ps.nonEmpty =>

                // When inserting predicate arguments to instance members we move all them up front rather than
                // interleaving the arguments using findPredicateArgs as we do with normal members -- the predicates
                // we're adding here are provided when the typeclass instance is created, as they're part of the
                // typeclass super or instance context
                val t1 = insertPredicateArgTypes(t0.ps, t0.h, ctx.tcTcons)

                // Create names for the predicate arguments and rewrite the function definition to capture them
                val reservedIds = findLocalIds(f, Set.empty)
                val predIds = (t0.ps map { p => p -> predToArgName(p, reservedIds) }).toMap
                val (ctx1, f1) = ctx.setFunctionTypes(makeFunc(predIds.values map { id => Argument(id) }, f), t1)

                // Rewrite the original function with the predicate arguments in scope
                rewriteExpr(f1, ctx1, predIds)

            case expr => rewriteMember(expr, ctx)
        }
    }

    def rewriteMember(expr: TapExpr, ctx: Context): (Context, TapExpr) = {
        val t0 = ctx.env.ets(expr)
        expr match {

            // A value read at the definition level means we're just giving an alternate name to an existing
            // definition, so no need to rewrite - that will be handled at the value the definition is pointing to.
            case expr: ValueReadExpr if t0.ps.nonEmpty =>
                ctx.setExprType(expr, interleavePredicateArgTypes(ctx.env.s, t0.h, findPredicateArgs(ctx.env.s, t0), ctx.tcTcons))

            case expr => rewriteExpr(expr, ctx, Map.empty)
        }
    }

    /**
     * Rewrites expressions to include additional arguments where needed to provide or accept typeclass instance
     * implementations.
     */
    def rewriteExpr(expr: TapExpr, ctx: Context, predIds: Map[IsIn, String]): (Context, TapExpr) = {
        val t0 = ctx.env.ets(expr)
        expr match {

            // Rewriting an apply where the value being applied is predicated is a special case: the arguments that
            // specify typeclass arguments need to be interleaved with the original arguments that are being passed
            // to the function
            case a @ ApplyExpr(f, e) if isPredicatedFunc(a, ctx) =>
                val fn = getAppliedFunc(a)
                val ft = ctx.env.ets(fn)
                val argMods = findPredicateArgs(ctx.env.s, ft)
                val ft1 = interleavePredicateArgTypes(ctx.env.s, ft.h, argMods, ctx.tcTcons)
                interleavePredicateArgVals(fn, getApplyArgs(a), argMods, ctx, predIds, ft1)

            // A value-read with predicates is treated similarly to applying a predicated function - the predicates
            // here are required to determine the return type of the expression. Occurs in cases when values like
            // mempty (from Monoid) or mzero (from MonadPlus) are referenced.
            case _: ValueReadExpr if t0.ps.nonEmpty =>
                val argMods = findPredicateArgs(ctx.env.s, t0)
                if (argMods.tail.flatten.nonEmpty) throw new Error("ValueReadExpr that has argMods that need interleaving?")
                val t1 = interleavePredicateArgTypes(ctx.env.s, t0.h, argMods, ctx.tcTcons)
                interleavePredicateArgVals(expr, Nil, List(argMods.head), ctx, predIds, t1)

            // For all other predicated expressions eta-abstraction is used to capture the typeclass implementation
            // arguments. This is a convenient way of handling things that are already functions as well as any other
            // value type, or cases where a function type hides the fact that some expressions occur in the middle of
            // the chain of function definitions.
            case expr if t0.ps.nonEmpty =>
                val numArgs = getFunctionArity(t0.h)
                val reservedIds = findLocalIds(expr, predIds.values.toSet)
                val argIds = List.range(0, numArgs) map { i => IDUtil.makeAlphabeticId(i, reservedIds) }
                val reservedIds1 = reservedIds ++ argIds
                val predIds1 = predIds ++ (t0.ps map { p => p -> predToArgName(p, reservedIds1) })
                val argMods = findPredicateArgs(ctx.env.s, t0)
                argMods.drop(numArgs + 1) foreach { ps => if (ps.nonEmpty) throw new Error("Unexpected argMods") }
                val (ctx1, e1) = rewriteExpr(expr, ctx.setExprType(expr, t0.h)._1, predIds1)
                val (ctx2, e2) = ctx1.setApplyTypes(makeApply(e1, argIds map { id => ValueReadExpr(LocalId(id)) }), applySubst(ctx1.env.s, t0.h))
                val newArgs = applyMods(argMods map2 { p => Argument(predIds1(p)) }, argIds map { id => Argument(id) })
                ctx2.setFunctionTypes(makeFunc(newArgs, e2), interleavePredicateArgTypes(ctx2.env.s, t0.h, argMods, ctx2.tcTcons))

            case a @ ApplyExpr(f, e) =>
                val (ctx1, f1) = rewriteExpr(f, ctx, predIds)
                val (ctx2, e1) = rewriteExpr(e, ctx1, predIds)
                ctx2.setExprType(ApplyExpr(f1, e1), t0.h)

            case BlockExpr(es) =>
                val (ctx1, es1) = ctx.map(es) { (ctx, e) => rewriteExpr(e, ctx, predIds) }
                ctx1.setExprType(BlockExpr(es1), t0.h)

            case MatchExpr(e, cs) =>
                val (ctx1, e1) = rewriteExpr(e, ctx, predIds)
                val (ctx2, cs1) = ctx1.map(cs) { (ctx, c) =>
                    c match {
                        case MatchCase(v, None, e) =>
                            val (ctx1, e1) = rewriteExpr(e, ctx, predIds)
                            (ctx1, MatchCase(v, None, e1))
                        case MatchCase(v, Some(g), e) =>
                            val (ctx1, g1) = rewriteExpr(g, ctx, predIds)
                            val (ctx2, e1) = rewriteExpr(e, ctx1, predIds)
                            (ctx2, MatchCase(v, Some(g1), e1))
                    }
                }
                ctx2.setExprType(MatchExpr(e1, cs1), t0.h)

            case LetExpr(id, e, f) =>
                val (ctx1, e1) = rewriteExpr(e, ctx, predIds)
                val (ctx2, f1) = rewriteExpr(f, ctx1, predIds)
                ctx2.setExprType(LetExpr(id, e1, f1), t0.h)

            case FunctionExpr(a, e) =>
                val (ctx1, e1) = rewriteExpr(e, ctx, predIds)
                ctx1.setExprType(FunctionExpr(a, e1), t0.h)

            case RaiseErrorExpr(e) =>
                val (ctx1, e1) = rewriteExpr(e, ctx, predIds)
                ctx1.setExprType(RaiseErrorExpr(e1), t0.h)

            case CastExpr(e, t) =>
                val (ctx1, e1) = rewriteExpr(e, ctx, predIds)
                ctx1.setExprType(CastExpr(e1, t), t0.h)

            case _: ExprLeaf => (ctx, expr)
        }
    }

    /**
     * Checks whether a function at the bottom of a chain of applications has predicates.
     */
    def isPredicatedFunc(ap: ApplyExpr, ctx: Context): Boolean =
        ctx.env.ets(getAppliedFunc(ap)).ps.nonEmpty

    /**
     * Finds the type for a predicate, using the specified (typeclass -> type constructor) mapping.
     */
    def predToType(p: IsIn, tcTcons: TCons): Type = p.ts.foldLeft(tcTcons(p.id): Type)(TAp)

    /**
     * Generates a name for a function argument that accepts a typeclass data type, based on the predicate for that
     * typeclass.
     */
    def predToArgName(p: IsIn, reservedIds: Set[String]): String = {
        val ids = p.ts flatMap { t => tv(t) } map { tv => tv.id }
        if (ids.isEmpty) throw new Error("predToArgName called on satisfied predicate: " + prettyPrint(p))
        IDUtil.makeId(p.id.id + "_" + (ids mkString ""), reservedIds)
    }

    /**
     * Generates an expression or value for a predicate.
     */
    def predToVal(pred: IsIn, ctx: Context, predIds: Map[IsIn, String]): (Context, TapExpr) = {

        /**
         * Checks the available predicate mapping to find a suitable mapping. A direct lookup in predIds doesn't work
         * in the case where a member of a superclass is being used.
         */
        def getMappedPred(pred: IsIn, predIds: Map[IsIn, String]): Option[(IsIn, String)] = {
            def tcSatisfies(tcId: ModuleId, predId: Id) = {
                def findRec(tc: TypeclassDef): Boolean = (tc.ps find { p => p.id == predId || findRec(tcs(p.id)) }) != None
                tcId == predId || findRec(tcs(tcId))
            }
            predIds find { case (p @ IsIn(tcId, ts), _) =>
                (pred.ts forall { p => ts contains p }) && tcSatisfies(tcId, pred.id)
            }
        }

        /**
         * Finds the chain of superclasses that are passed through to get from a typeclass to a particular superclass
         * predicate.
         */
        def getTypeclassChain(start: IsIn, end: IsIn): List[IsIn] = {
            def findRec(xs: List[IsIn], result: List[IsIn]): Option[List[IsIn]] = xs match {
                case Seq() => None
                case x :: xs if x.id == end.id => Some(x :: result)
                case x :: xs =>
                    val tc = tcs(x.id)
                    val s = (tc.vs zip start.ts).toMap
                    findRec(tc.ps map { p => applySubst(s, p) }, x :: result) match {
                        case None => findRec(xs, result)
                        case r => r
                    }
            }
            findRec(List(start), List.empty) match {
                case Some(tcs) => tcs
                case None => throw new Error("Could not build superclass chain")
            }
        }

        val t = predToType(pred, ctx.tcTcons)

        /**
         * Makes a chain of matches destructuring typeclass data constructors, so a member of a superclass can be
         * reached. For example, if a member uses < and == on the same type, only the Ord typeclass will be present on
         * the definition, but the Eq typeclass can be reached by extracting it from the TCOrd data constructor.
         */
        def liftIntoTypeclass(curr: IsIn, target: IsIn, tv: TapExpr, ctx: Context): (Context, TapExpr) = {

            def makeUnapply(xs: List[IsIn], parent: IsIn, inner: PatternNode, ctx: Context): (Context, PatternNode) = xs match {
                case Seq() => (ctx, inner)
                case x :: xs =>
                    val tc = tcs(x.id)
                    val aargs = tc.ps map { p => if (p.id == parent.id) inner else WildcardValueExpr }
                    val args = aargs ++ (List.range(0, tc.members.size) map { _ => WildcardValueExpr })
                    val (dconId, dcon) = ctx.tcDcons(tc.name)
                    // TODO: this is wrong, comes out as: (TestBits.TCBase Prelude.Number)
                    //                     but should be: (TestBits.TCBase µ9)
                    val (ctx1, ft) = ctx.freshInstPartial(x.ts, dcon)
                    val argTypes = getFunctionTypeArgs(ft)
                    trace("-", prettyPrint(dconId))
                    trace(argTypes map prettyPrint mkString "\n")
                    // TODO: grab DCon for type, freshInst it and use it to set type of all wildcard args
                    val (ctx2, ua) = ctx.setExprType(UnapplyNode(dconId, args), predToType(x, ctx1.tcTcons))
                    makeUnapply(xs, x, ua, ctx2)
            }

            if (curr.id == target.id) (ctx, tv)
            else {
                trace("--->", prettyPrint(curr), ">>>", prettyPrint(target))
                trace.indent()

                val xs = getTypeclassChain(curr, target)
                val tx = predToType(xs.head, ctx.tcTcons)
                trace("xx", prettyPrint(tx))
                val (ctx1, b) = ctx.setExprType(BindNode("s", None), tx)
                val (ctx2, cn) = makeUnapply(xs.tail, xs.head, b, ctx1)
                val (ctx3, v) = ctx2.setExprType(ValueReadExpr(LocalId("s")), tx)
                trace("!!", prettyPrint(tx))
                trace("  ", prettyPrint(cn))
                trace.dedent()

                // TODO: set types (properly)
                val m = MatchExpr(tv, List(MatchCase(cn, None, v)))
                ctx3.setExprType(m, tx)
            }
        }

        // TODO: can probably take advantage of IsIn.bySuper instead of the insanity of getTypeclassChain

        getMappedPred(pred, predIds) match {

            case Some((p, id)) =>
                val (ctx1, v0) = ctx.setExprType(ValueReadExpr(LocalId(id)), t)
                liftIntoTypeclass(p, pred, v0, ctx1)

            case None =>
                val tci = ctx.getInst(pred.id, pred.ts map getTConID)
                val s1 = IsIn.matchPred(tci.tc, pred).get
                val ps1 = tci.ps map { p => Substitutions.applySubst(s1, p) }
                val (ctx1, predArgs) = ctx.map(ps1) { (ctx, p) => predToVal(p, ctx, predIds) }
                ctx1.setApplyTypes(makeApply(ValueReadExpr(ctx.tcInstNames(tci)), predArgs), t)
        }
    }

    /**
     * Creates a chain of applications, interleaving predicate arguments with a list of the arguments that were
     * originally applied.
     */
    def interleavePredicateArgVals(fn: TapExpr, args: List[TapExpr], argMods: List[List[IsIn]], ctx: Context, predIds: Map[IsIn, String], t: Type): (Context, TapExpr) = {
        val (ctx1, args1) = ctx.map(args) { (ctx, arg) => rewriteExpr(arg, ctx, predIds) }
        val (ctx2, argMods1) = ctx1.map(argMods) { (ctx, ps) =>
            ctx.map(ps) { (ctx, p) => predToVal(p, ctx, predIds) }
        }
        ctx2.setApplyTypes(makeApply(fn, applyMods(argMods1, args1)), t)
    }

    /**
     * Takes a qualified type and examines the arguments to determine at what position predicates become deciable. The
     * result is a list of lists, the outer list corresponds to argument indicies, and the inner lists are the
     * predicates that are decided by that argument.
     */
    def findPredicateArgs(s: Subst, qt: Qual[Type]): List[List[IsIn]] = qt match {
        case Qual(Seq(), t) => Nil
        case Qual(ps, t) =>

            val psSat = ps filter { p => IsIn.tv(p).isEmpty }
            if (psSat.nonEmpty) {
                // This is a problem because it makes it impossible to decide which argument satisfies the predicate
                // e.g. if a function comes in as (SomeTC String) => String -> String -> String there is nothing to say
                // where the argument for SomeTC should be passed. Instead the type should be something like:
                // (SomeTC a) => String -> a -> a, that way we know a is decided by the second argument, and we can
                // generate a new type like: String -> (TCSomeTC a) -> a -> a
                throw new Error("Type already has some satisfied predicates: " + prettyPrint(qt))
            }

            def transform(t: Type, ps: List[IsIn], tvs: Set[TVar]): List[List[IsIn]] = t match {
                case TAp(TAp(f, a), b) if f == tArrow =>
                    val tvs1 = tvs ++ tv(a)
                    val psSat = ps filter { p => IsIn.tv(p) forall { tv => tvs1 contains tv } }
                    psSat :: transform(b, ps filter { p => !(psSat contains p) }, tvs1)
                case a =>
                    val tva = tv(a)
                    List(ps filter { p => IsIn.tv(p) forall { tv => tva contains tv } })
            }

            transform(t, ps, Set.empty) map2 { p => applySubst(s, p) }
    }

    /**
     * Splices argMods into a list of arguments at the appropriate positions.
     */
    def applyMods[T](argMods: List[List[T]], args: List[T]): List[T] = {
        if (args.length != argMods.length - 1) throw new Error("argMods arity mismatch: " + args.length + " args, " + argMods.length + " argMods")
        (argMods zip args).foldRight(argMods.last) { case ((args, arg), result) => args ++ (arg :: result) }
    }

    /**
     * Creates a function type, inserting predicates as arguments before another type.
     */
    def insertPredicateArgTypes(ps: List[IsIn], t: Type, tcTcons: TCons): Type =
        ps.foldRight(t) { (p, t) => predToType(p, tcTcons) fn t }

    /**
     * Transforms a function type to insert arguments at the specified slots. The indices of items in the argMods array
     * corresponds with the position of the argument at which the new arguments will be inserted at.
     */
    def interleavePredicateArgTypes(s: Subst, t: Type, argMods: List[List[IsIn]], tcTcons: TCons): Type = {
        val ts = getFunctionTypes(t)
        val argMods1 = argMods map2 { p => predToType(p, tcTcons) }
        applySubst(s, makeFunctionType(applyMods(argMods1, ts.init), ts.last))
    }
}
