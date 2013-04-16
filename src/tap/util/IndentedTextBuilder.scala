package tap.util

class IndentedTextBuilder extends IndentedTextBuffer {

	protected val buff = new StringBuilder
	protected var isInitLine = true

	def apply(text: Any*) = {
		var isFirstItem = true
		var str = ""
		for (item <- text) {
			if (isFirstItem) isFirstItem = false
			else str += " "
			item match {
				case null => str += "null"
				case _ => str += item.toString
			}
		}
		if (isInitLine) isInitLine = false
		else buff append "\n"
		buff append "\t" * tabs
		buff append str.replace("\r", "").replace("\n", "\n" + ("\t" * tabs))
	}

	override def toString = buff.toString
}