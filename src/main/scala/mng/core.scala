package mng

import scala.io.Source
import java.io.{File, FileOutputStream, FileWriter}
import asm.{CodeSection, EncoderBase}




object Compiler{

  private def convertListToLowerCase( tokens: Array[String]): Unit = {
    for(idx <- tokens.indices){
      tokens(idx) = tokens(idx).toLowerCase()
    }
  }

  def compile(srcFilePath: String, desFilePath: String): Unit = {

    /**
     * check integrity
     * */

    /**
     * initialize variable
     * */

    val fileSource          = Source.fromFile(srcFilePath)
    val desFileOutputStream = new FileOutputStream(desFilePath)
    val cs = CodeSection(0)

    /**
     * read data
     * */

    for (line <- fileSource.getLines()){
      /** split the token*/
      val tokens = line.split("\\s+")
      /** encode the token*/
      if (tokens.nonEmpty && tokens(0).nonEmpty && (tokens(0)(0) != ';') ){ ///// we skip empty line
        val newInstr  = EncoderBase.encode(tokens)
        cs.addIntr(newInstr)
      }

    }
    fileSource.close()

    /**
     * write data
     */

    cs.write(desFileOutputStream);
    desFileOutputStream.close()

  }

}