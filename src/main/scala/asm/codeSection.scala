package asm
import java.io._
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import java.nio.ByteOrder

class CodeSection(StartAddr: Int, swapEndian: Boolean){

  val instrs: ArrayBuffer[IntrBase] = ArrayBuffer()

  def addIntr(instr: IntrBase): Unit = {
    instrs += instr
  }

  def swapEndian(instrRaw: Int): Int = {
    ByteBuffer.allocate(4)
      .order(ByteOrder.BIG_ENDIAN)
      .putInt(instrRaw)
      .order(ByteOrder.LITTLE_ENDIAN)
      .getInt(0)
  }

  def write(fileOutPutSteam: FileOutputStream): Unit = {
    ////// create dta
    val dataoutputStream = DataOutputStream(fileOutPutSteam)
    
    for (instr <- instrs) {
      if(swapEndian){
        dataoutputStream.writeInt(swapEndian(instr.dump))
      }else {
        dataoutputStream.writeInt(instr.dump)
      }
    }
    
    dataoutputStream.close()
    
  }



}