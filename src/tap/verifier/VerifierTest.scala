package tap.verifier

import tap.types.Tyvar
import tap.types.kinds.Star
import tap.util.PrettyPrint._
import tools.nsc.io.Directory
import tap.util.trace
import tap.{InstId, ModuleId}

object VerifierTest {

    def main (args: Array[String]) {

        // No named arguments, just pass "./tap/lab/" or whatever
        if (args.length == 0) throw new Error("Tap classpath argument is missing")

        // Find all .stap files recursively
        val files = args flatMap { cp => Directory(cp).deepFiles.filter(f => f.extension == "stap") }

        // Run the verifier
        val (moduleGroups, defs, ets) = VerifierFrontend(files)


        moduleGroups foreach { moduleNames =>
            moduleNames foreach { mname =>

                trace("\n---------------------- Module [" + mname + "] ----------------------")

                val mtcons = defs.tcons.filterKeys { msn => msn.mId == mname  }
                if (mtcons.nonEmpty) trace("\nType constructors:\n" + (mtcons.map { case (ModuleId(_, tci), tc) => tci + " :: " + prettyPrint(tc.c.k) }.toList.sortBy({ x => x }) mkString "\n"))

                val mdcons = defs.dcons.filterKeys { msn => msn.mId == mname  }
                if (mdcons.nonEmpty) trace("\nData constructors:\n" + (mdcons.map { case (ModuleId(_, dci), dc) => dci + " :: " + prettyPrint(dc) }.toList.sortBy({ x => x }) mkString "\n"))

                val mtcs = defs.tcs.filterKeys { msn => msn.mId == mname  }
                if (mtcs.nonEmpty) trace("\nType classes:\n" + (mtcs.map { case (ModuleId(_, tci), tc) => tci + " :: " + printTypeclassKind(tc.vs) }.toList.sortBy({ x => x }) mkString "\n"))

                val mis = defs.mis.filterKeys {
                    case ModuleId(mId, _) => mId == mname
                    case InstId(mId, _, _, _) => mId == mname
                    case _ => false
                }
                if (mis.nonEmpty) trace("\nMember implementations:\n" + (mis.map { case (qId, mi) =>
                    qId.id + " :: " + prettyPrint(ets(mi)) + "\n" +
                        (" " * (qId.id.length + 4)) + prettyPrint(mi)

                }).toList.sortBy({ x => x }).mkString("\n\n"))
            }
        }
    }

    def printTypeclassKind(ps: List[Tyvar]): String =
        ps.map { p => p.k }.map {
            case Star => "*"
            case k => "(" + prettyPrint(k) + ")"
        }.mkString(" -> ")
}