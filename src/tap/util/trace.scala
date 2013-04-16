package tap.util

object trace extends IndentedTextBuffer
{
	def apply(text: Any*) = {
		var str = ""
		for (item <- text) item match {
			case null => str = str + "null "
			case _ => str = str + item.toString + " "
		}
		Console.print(("    " * tabs) + str.replace("\r", "").replace("\n", "\n" + ("    " * tabs)) + "\n")
	}
}