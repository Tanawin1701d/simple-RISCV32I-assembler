package asm

import scala.collection.mutable
import scala.collection.mutable.Map

object Mnemonic{
  /////// map to mnemonic to funct3 and funct7
  val R_ALU_mnemonic: mutable.Map[String, (Int, Int)] = mutable.Map("add" -> (0,0), "sub"  ->(0,32), "sll" -> (1,0),
                            "slt" -> (2,0), "sltu" ->(3, 0), "xor" -> (4,0),
                            "srl" -> (5,0), "sra"  ->(5,32), "or"  -> (6,0),
                            "and" -> (7,0))

  val R_ALU_mnemonic_opcode: Byte = Integer.parseInt("0110011", 2).toByte

  /////// map to mnemonic to funct3 and opcode
  val I_ALU_mnemonic_opcode = Integer.parseInt("0010011", 2).toByte
  val I_JUMP_mnemonic_opcode = Integer.parseInt("1100111", 2).toByte
  val I_LD_mnemonic_opcode   = Integer.parseInt("0000011", 2).toByte

  val I_POOL_mnemonic: mutable.Map[String, (Int, Byte)] = mutable.Map(
                             "addi" -> (0,I_ALU_mnemonic_opcode), "slti" -> (2,I_ALU_mnemonic_opcode), "sltiu" -> (3,I_ALU_mnemonic_opcode),
                             "xori" -> (4,I_ALU_mnemonic_opcode), "ori"  -> (6,I_ALU_mnemonic_opcode), "andi"  -> (7,I_ALU_mnemonic_opcode),
                             "slli" -> (1,I_ALU_mnemonic_opcode), "srli" -> (5,I_ALU_mnemonic_opcode), "srai"  -> (5,I_ALU_mnemonic_opcode),
                             "jalr" -> (0,I_JUMP_mnemonic_opcode),    ////// jump with base register
                             "lb"   -> (0,I_LD_mnemonic_opcode),  "lh"   -> (1,I_LD_mnemonic_opcode), "lw" -> (2,I_LD_mnemonic_opcode),
                             "lbu"  -> (4,I_LD_mnemonic_opcode),  "lhu"  -> (5,I_LD_mnemonic_opcode)
  )   ///// shift have shinked

  ///// map to opcode
  val U_LDPC_mnemonic: mutable.Map[String, Byte] = mutable.Map(
                            "lui"   -> Integer.parseInt("0110111", 2).toByte,
                            "auipc" -> Integer.parseInt("0010111", 2).toByte)


  ///// map to opcode
  val J_JUMP_mnemonic: mutable.Map[String, Byte] = mutable.Map("jal" -> Integer.parseInt("1101111", 2).toByte)

  //// map to funct3
  val B_BRANCH_mnemonic: mutable.Map[String, Int] = mutable.Map(
                              "beq" -> 0, "bne"  -> 1, "blt"  -> 4,
                              "bge" -> 5, "bltu" -> 6, "bgeu" -> 7)

  val B_BRANCH_mnemonic_opcode: Byte = Integer.parseInt("1100011", 2).toByte


  //// map to funct 3

  val S_STORE_mnemonic: mutable.Map[String, Int] = mutable.Map(
                              "sb" -> 0, "sh"  -> 1, "sw"  -> 2)

  val S_STORE_mnemonic_opcode: Byte = Integer.parseInt("0100011", 2).toByte

}

object EncoderBase{


  def cvtRegNameToIdx(regName: String): Int = {

    assert(regName.length >= 2, "register index is error")
    assert(regName(0) == 'r', "rigister idx must start with r")

    try {
      val intVal = regName.substring(1).toInt;
      return intVal
    }catch{
      case e: NumberFormatException =>
        throw new IllegalArgumentException(s"register index can't be converted $regName")
    }


  }

  def cvtImmToInt(immStr: String): Int = {
    assert(immStr.length >= 1, "imm value is error")

    try{
      val int = immStr.toInt;
      return int
    }catch{
      case e: NumberFormatException =>
        throw new IllegalArgumentException(s"imm can't be converted $immStr")
    }
  }


  def encode(tokens: Array[String]): IntrBase = {

    assert(tokens.nonEmpty, "instruction should not empty")
    val mnemonic = tokens(0)

    if (Mnemonic.R_ALU_mnemonic.contains(mnemonic)){
      assert(tokens.length == 4, "invalid r alu args")
      val rd = cvtRegNameToIdx(tokens(1))
      val r1 = cvtRegNameToIdx(tokens(2))
      val r2 = cvtRegNameToIdx(tokens(3))
      return R_INSTR(Mnemonic.R_ALU_mnemonic_opcode, rd,
                     r1, r2,
                     Mnemonic.R_ALU_mnemonic(mnemonic)(0), ////// funct3
                     Mnemonic.R_ALU_mnemonic(mnemonic)(1)  ////// funct7
                    )
    }

    if (Mnemonic.I_POOL_mnemonic.contains(mnemonic)){
      assert(tokens.length == 4, "invalidd imm arge")
      val rd  = cvtRegNameToIdx(tokens(1))
      val r1  = cvtRegNameToIdx(tokens(2))
      var imm = cvtImmToInt    (tokens(3))
      /////"slli" -> (1,I_ALU_mnemonic_opcode), "srli" -> (5,I_ALU_mnemonic_opcode), "srai"

      if (mnemonic == "slli"){
        assert(imm < G_PARAM.XLEN && imm >= 0)
      }else if (mnemonic == "srli"){
        assert(imm <  G_PARAM.XLEN && imm >= 0)
      }else if (mnemonic == "srai"){
        assert(imm <  G_PARAM.XLEN && imm >= 0)
        imm = imm | (1 << 10)
      }

      return I_INSTR(Mnemonic.I_POOL_mnemonic(mnemonic)(1), rd,
                     r1,imm,
                     Mnemonic.I_POOL_mnemonic(mnemonic)(0)
                     )
    }

    if (Mnemonic.U_LDPC_mnemonic.contains(mnemonic)){
      assert(tokens.length == 3, "invalid u instr")
      val rd = cvtRegNameToIdx(tokens(1))
      val imm = cvtImmToInt   (tokens(2))

      return U_INSTR(Mnemonic.U_LDPC_mnemonic(mnemonic), rd, imm)
    }


    if (Mnemonic.J_JUMP_mnemonic.contains(mnemonic)){
      assert(tokens.length == 3, "invalid j instr (jal)")
      val rd  = cvtRegNameToIdx(tokens(1))
      val imm = cvtImmToInt    (tokens(2))
      return J_INSTR(Mnemonic.J_JUMP_mnemonic(mnemonic), rd, imm)

    }

    if (Mnemonic.B_BRANCH_mnemonic.contains(mnemonic)){
      assert(tokens.length == 4, "invalid b instr")
      val imm = cvtImmToInt    (tokens(0))
      val r1  = cvtRegNameToIdx(tokens(1))
      val r2  = cvtRegNameToIdx(tokens(2))
      return B_INSTR(Mnemonic.B_BRANCH_mnemonic_opcode,
                     r1, r2, imm,
                     Mnemonic.B_BRANCH_mnemonic(mnemonic)
      )
    }

    if (Mnemonic.S_STORE_mnemonic.contains(mnemonic)){
      assert(tokens.length == 4, "invalid s isntr")
      val rbase = cvtRegNameToIdx(tokens(0))
      val imm   = cvtImmToInt    (tokens(1))
      val rsrc  = cvtRegNameToIdx(tokens(2))
      
      return S_INSTR(Mnemonic.S_STORE_mnemonic_opcode,
        rbase, rsrc, imm,
        Mnemonic.S_STORE_mnemonic(mnemonic)
      )
    }


    throw new IllegalAccessException("there is no instruction to decode")

  }

}
