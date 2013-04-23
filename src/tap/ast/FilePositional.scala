package tap.ast

import scala.util.parsing.input.Positional

/**
 * An extension of the Positional trait to include a filename along side source position information.
 */
trait FilePositional extends Positional {

    /**
     * The file this object came from.
     */
    var file: String = null

    /**
     * Sets the source file this object came from.
     */
    def setFile(newFile: String): this.type = {
        file = newFile
        this
    }

    /**
     * Sets the source file and position within the file this object came from.
     */
    def setFilePosFrom(fp: FilePositional): this.type = {
        file = fp.file
        pos = fp.pos
        this
    }
}

object NullFilePosition extends FilePositional