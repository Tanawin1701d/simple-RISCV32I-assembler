package asm
import java.io._
import scala.collection.mutable.ArrayBuffer

class CodeSection(StartAddr: Int){

  val instrs: ArrayBuffer[IntrBase] = ArrayBuffer()

  def addIntr(instr: IntrBase) = {
    instrs += instr
  }

  def write(fileOutPutSteam: FileOutputStream): Unit = {
    ////// create dta
    val dataoutputStream = DataOutputStream(fileOutPutSteam)
    
    for (instr <- instrs) {
      dataoutputStream.writeInt(instr.dump)
    }
    
    dataoutputStream.close()
    
  }



}