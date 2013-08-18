package tap.verifier

import tap.types.TVar
import tap.types.kinds.Star
import tap.util.PrettyPrint._
import tap.util.trace
import tap.{InstId, ModuleId}
import java.io.File

object VerifierTest {

    def main (args: Array[String]) {

        // No named arguments, just pass "./tap/lab/" or whatever
        if (args.length == 0) throw new Error("Tap classpath argument is missing")

        // Find all .stap files recursively
        val files = args flatMap { cp =>
            val dir = new File(cp)
            if (!dir.exists) throw new Error("Classpath directory " + dir.getAbsolutePath + " does not exist")
            if (!dir.isDirectory) throw new Error("Classpath entry " + dir.getAbsolutePath + " is not a directory")
            findSourceFiles(dir)
        }

        // Run the verifier
        val (moduleGroups, defs, ets) = FilesVerifier(files)

        moduleGroups foreach { moduleNames =>
            moduleNames foreach { mname =>

                trace("\n---------------------- Module [" + mname + "] ----------------------")

                val mtcons = defs.tcons.filterKeys { msn => msn.mId == mname  }
                if (mtcons.nonEmpty) trace("\nType constructors:\n" + (mtcons.map { case (ModuleId(_, tci), tc) => tci + " :: " + prettyPrint(tc.k) }.toList.sortBy({ x => x }) mkString "\n"))

                val mdcons = defs.dcons.filterKeys { msn => msn.mId == mname  }
                if (mdcons.nonEmpty) trace("\nData constructors:\n" + (mdcons.map { case (ModuleId(_, dci), dc) => dci + " :: " + prettyPrint(dc) }.toList.sortBy({ x => x }) mkString "\n"))

                val mtcs = defs.tcs.filterKeys { msn => msn.mId == mname  }
                if (mtcs.nonEmpty) trace("\nType classes:\n" + (mtcs.map { case (ModuleId(_, tci), tc) => tci + " :: " + printTypeclassKind(tc.vs) }.toList.sortBy({ x => x }) mkString "\n"))

                val mts = defs.mts.filterKeys {
                    case id @ ModuleId(mId, _) => mId == mname && !(defs.dcons contains id)
                    case InstId(mId, _, _, _) => mId == mname
                    case _ => false
                }
                if (mts.nonEmpty) trace("\nMember types:\n" + mts.map { case (qId, mt) =>
                    qId.id + " :: " + prettyPrint(mt)
                }.toList.sortBy({ x => x }).mkString("\n\n"))

                val mis = defs.mis.filterKeys {
                    case ModuleId(mId, _) => mId == mname
                    case InstId(mId, _, _, _) => mId == mname
                    case _ => false
                }
                if (mis.nonEmpty) trace("\nMember implementations:\n" + mis.map { case (qId, mi) =>
                    qId.id + " :: " + prettyPrint(ets(mi)) + "\n" +
                        (" " * (qId.id.length + 4)) + prettyPrint(mi)

                }.toList.sortBy({ x => x }).mkString("\n\n"))
            }
        }
    }

    def findSourceFiles(dir: File): Array[File] = {
        val allFiles = dir.listFiles()
        val srcFiles = allFiles filter { f =>
            val name = f.getName
            val i = name.lastIndexOf('.')
            f.isFile && i != -1 && name.substring(i) == ".stap"
        }
        val dirs = allFiles filter { _.isDirectory }
        srcFiles ++ (dirs flatMap findSourceFiles)
    }

    def printTypeclassKind(ps: List[TVar]): String =
        ps.map { p => p.k }.map {
            case Star => "*"
            case k => "(" + prettyPrint(k) + ")"
        }.mkString(" -> ")
}