package tap.util

trait IndentedTextBuffer {
    protected var tabs: Int = 0
    def indent() {tabs = tabs + 1}
    def dedent() {if (tabs > 0) tabs = tabs - 1}
    def apply(text: Any*)
}