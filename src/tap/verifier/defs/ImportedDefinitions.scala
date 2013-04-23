package tap.verifier.defs

import tap.ModuleId

case class ImportedDefinitions(tcons: Map[String, ModuleId],
                               dcons: Map[String, ModuleId],
                               tcs: Map[String, ModuleId],
                               members: Map[String, ModuleId]) {

    def addClass(local: String, qn: ModuleId) = ImportedDefinitions(tcons, dcons, tcs + (local -> qn), members)
    def addTCon(local: String, qn: ModuleId) = ImportedDefinitions(tcons + (local -> qn), dcons, tcs, members)
    def addDCon(local: String, qn: ModuleId) = ImportedDefinitions(tcons, dcons + (local -> qn), tcs, members)
    def addMember(local: String, qn: ModuleId) = ImportedDefinitions(tcons, dcons, tcs, members + (local -> qn))
}

object ImportedDefinitions {
    val empty = ImportedDefinitions(Map("->" -> ModuleId("Prelude", "->")), Map.empty, Map.empty, Map.empty)
}