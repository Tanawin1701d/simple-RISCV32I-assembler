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

  private def checkIsInstr(firstToken: String): Boolean = {
    firstToken.nonEmpty && (firstToken(0) != ';') && (firstToken(0) != '.')
  }

  private def checkIsComment(firstToken: String): Boolean = {
    firstToken.isEmpty || (firstToken(0) == ';')
  }

  private def checkIsSection(firstToken: String): Boolean = {
    firstToken.nonEmpty && (firstToken(0) == '.')
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
    val cs = CodeSection(0, true)
    val ed = EncoderBase()

    /**
     * read data
     * */

    val rawData = fileSource.getLines().toList
    var curNextAddr: Int = 0

    /** read all first to get section and its address*/
    for (line <- rawData){
      ////println("section1 start")
      val tokens = line.trim.split("\\s+")
      convertListToLowerCase(tokens)
      if (checkIsSection(tokens(0))){ //// add address to section
        ed.addSection(tokens(0), curNextAddr)
        println(s"code section add ----> ${tokens(0)}")
      }else if (checkIsInstr(tokens(0))){ ///// increment address
        curNextAddr += 4
      }
    }

    /** encode instruction*/

    curNextAddr = 0
    for (line <- rawData) {
      val tokens = line.trim.split("\\s+")
      convertListToLowerCase(tokens)
      ///println("section2 start")
      ////println(line)
      if (checkIsInstr(tokens(0))) { //// add address to section
        val instr = ed.encode(tokens, curNextAddr)
        curNextAddr += 4
        cs.addIntr(instr)
      }
    }

    println(s"instruction count ${curNextAddr/4}")

    fileSource.close()

    /**
     * write data
     */

    cs.write(desFileOutputStream);
    desFileOutputStream.close()

  }

}