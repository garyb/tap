package tap.verifier.defs

import tap.ModuleId
import tap.verifier.errors.ImportConflictError

case class DefinitionsLookup(tcons: Map[String, ModuleId],
                               dcons: Map[String, ModuleId],
                               tcs: Map[String, ModuleId],
                               members: Map[String, ModuleId]) {

    def addTCon(local: String, qn: ModuleId) = DefinitionsLookup(tcons + (local -> qn), dcons, tcs, members)
    def addDCon(local: String, qn: ModuleId) = DefinitionsLookup(tcons, dcons + (local -> qn), tcs, members)
    def addClass(local: String, qn: ModuleId) = DefinitionsLookup(tcons, dcons, tcs + (local -> qn), members)
    def addMember(local: String, qn: ModuleId) = DefinitionsLookup(tcons, dcons, tcs, members + (local -> qn))
}

object DefinitionsLookup {

    val empty = DefinitionsLookup(Map.empty, Map.empty, Map.empty, Map.empty)

    /**
     * Merges the imported definitions for two modules, to be included in module `mId`.
     */
    def merge(mId: String, x: DefinitionsLookup, y: DefinitionsLookup): DefinitionsLookup = {
        DefinitionsLookup(
            mergeMap(mId, "type constructor", x.tcons, y.tcons),
            mergeMap(mId, "data constructor", x.dcons, y.dcons),
            mergeMap(mId, "type class", x.tcs, y.tcs),
            mergeMap(mId, "member", x.members, y.members)
        )
        // TODO: ensure data constructors and members do not conflict. shouldn't be possible at the moment as members must start lowercase and data constructors must start uppercase.
    }

    def mergeMap(mId: String, defType: String, xs: Map[String, ModuleId], ys: Map[String, ModuleId]) = {
        ys.foldLeft(xs) { case (result, kv @ (localId, moduleId)) =>
            result.get(localId) match {
                case Some(moduleId1) =>
                    if (moduleId1 != moduleId) throw ImportConflictError(mId, defType, localId, moduleId, moduleId1)
                    result
                case None => result + kv
            }
        }
    }
}