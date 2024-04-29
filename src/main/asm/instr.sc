type SIDX =  (Int, Int)

object INSTR_SLICE_META{
  val OP: SIDX = (0,7)
  val RD: SIDX = (7,12)
  val F3: SIDX = (12,15)
  val R1: SIDX = (15,20)
  val R2: SIDX = (20,25)
  val F7: SIDX = (25, 32)

  val IMM_I_0_11 : SIDX = (20, 31)

  val IMM_S_0_4  : SIDX = ( 7, 12)
  val IMM_S_5_11 : SIDX = (25, 32)

  val IMM_B_1_4  : SIDX = (8, 12)
  val IMM_B_11   : SIDX = (7, 8)
  val IMM_B_5_10 : SIDX = (25, 31)
  val IMM_B_12   : SIDX = (31, 32)

  val IMM_U_12_31 : SIDX = (12, 32)

  val IMM_J_20    : SIDX = (31, 32)
  val IMM_J_1_10  : SIDX = (21, 31)
  val IMM_J_11    : SIDX = (20, 21)
  val IMM_J_12_19 : SIDX = (12, 20)

  def getSize(slidx: SIDX): Int = {
    return slidx._2 - slidx._1;
  }
}

object VAL_CHECK{

  val MAX_AMT_IDX = 31
  val MAX_AMT_F3  = 8
  val MAX_AMT_F7  = 128

  def checkRegIdx(regIdx: Int): Unit = {
    assert(regIdx >= 0, "register index must be ge 0")
    assert(regIdx < MAX_AMT_IDX, s"register index must be Lt $MAX_AMT_IDX")
  }

  def checkF3(f3: Int): Unit = {
    assert(f3 >= 0, "f3 index must be ge 0")
    assert(f3 < MAX_AMT_F3, s"f3 index must be lt $MAX_AMT_F3")
  }

  def checkF8(f8: Int): Unit = {
    assert(f8 >= 0, "f8 index must be ge 0")
    assert(f8 < MAX_AMT_F7, s"f3 index must be lt $MAX_AMT_F7")
  }

  def checkImm(sidx: SIDX, imm: Int): Unit = {

    val minRange = (1 << sidx._1) - 1
    val maxRange = minRange + (1 << (sidx._2 - sidx._1 + 1 - 1))  /////// get bit size and reduce 1 bit for sign

    if (imm >= 0) {
      assert(imm < maxRange, "imm exceed range")
      assert(imm >= minRange, "imm exceed range")
    }else{
      assert(imm <= (-minRange), "imm neg exceed range"  )
      assert(imm >= (-maxRange), "imm neg exceed range" )
    }
  }

}

trait IntrBase{

  var result: Int = 0

  def dump: Int = {
    return result
  }

}


class Slice(var sidx : SIDX,
            var value: Int,

           ){

  assert(sidx._1 < sidx._2, "idx fault")
  ///// build necessary data before build result
  val size = sidx._2 - sidx._1;
  val neededMask = (1 << size) - 1;
  ///// build result
  var result: Int = 0;
      result = value & neededMask;
      result = result << sidx._1;

  def getSliceVal: Int = {return result;}

}




class R_INSTR(var opcode: Int, var rd_idx: Int,
              var r1_idx: Int, var r2_idx: Int,
              var funct3: Int, var funct7: Int)
  extends IntrBase {

  /** integrity check*/
  VAL_CHECK.checkRegIdx(rd_idx)
  VAL_CHECK.checkRegIdx(r1_idx)
  VAL_CHECK.checkRegIdx(r2_idx)
  VAL_CHECK.checkF3(funct3)
  VAL_CHECK.checkF8(funct7)
  /** build value*/
  result |= opcode;
  result |= Slice(INSTR_SLICE_META.RD, rd_idx).getSliceVal;
  result |= Slice(INSTR_SLICE_META.R1, r1_idx).getSliceVal;
  result |= Slice(INSTR_SLICE_META.R2, r2_idx).getSliceVal;

  result |= Slice(INSTR_SLICE_META.F3, funct3).getSliceVal;
  result |= Slice(INSTR_SLICE_META.F7, funct7).getSliceVal;

}

class I_INSTR(var opcode: Int, var rd_idx: Int,
              var r1_idx: Int, var imm   : Int,
              var funct3: Int)
  extends IntrBase {
  /** integrity check*/
  VAL_CHECK.checkRegIdx(rd_idx)
  VAL_CHECK.checkRegIdx(r1_idx)
  VAL_CHECK.checkImm((0, 11), imm)
  VAL_CHECK.checkF3(funct3)
  /** build value */
  result |= opcode;
  result |= Slice(INSTR_SLICE_META.RD,         rd_idx).getSliceVal;
  result |= Slice(INSTR_SLICE_META.R1,         r1_idx).getSliceVal;
  result |= Slice(INSTR_SLICE_META.F3, funct3).getSliceVal;
  /** imm*/
  result |= Slice(INSTR_SLICE_META.IMM_I_0_11, imm   ).getSliceVal;


}

class S_INSTR(var opcode: Int,
              var r1_idx: Int, var r2_idx: Int,var imm: Int,
              var funct3: Int)
  extends IntrBase {
  /** integrity check*/
  VAL_CHECK.checkRegIdx(r1_idx)
  VAL_CHECK.checkRegIdx(r2_idx)
  VAL_CHECK.checkImm((0, 11), imm)
  VAL_CHECK.checkF3(funct3)
  /** build value */
  result |= opcode;
  result |= Slice(INSTR_SLICE_META.R1, r1_idx).getSliceVal;
  result |= Slice(INSTR_SLICE_META.R2, r2_idx).getSliceVal;
  result |= Slice(INSTR_SLICE_META.F3, funct3).getSliceVal;
  /*** imm encoder*/
  result |= Slice(INSTR_SLICE_META.IMM_S_0_4, imm   ).getSliceVal;
  imm = imm >> INSTR_SLICE_META.getSize(INSTR_SLICE_META.IMM_S_0_4);

}


class B_INSTR(var opcode: Int,
              var r1_idx: Int, var r2_idx: Int,var imm: Int,
              var funct3: Int)
  extends IntrBase {
  /** integrity check */
  VAL_CHECK.checkRegIdx(r1_idx)
  VAL_CHECK.checkRegIdx(r2_idx)
  VAL_CHECK.checkImm((1, 12), imm)
  VAL_CHECK.checkF3(funct3)

  /** build value */
  result |= opcode;
  result |= Slice(INSTR_SLICE_META.R1, r1_idx).getSliceVal;
  result |= Slice(INSTR_SLICE_META.R2, r2_idx).getSliceVal;
  result |= Slice(INSTR_SLICE_META.F3, funct3).getSliceVal;
  /*** imm encoder*/
  imm = imm >> 1;
  result |= Slice(INSTR_SLICE_META.IMM_B_1_4, imm   ).getSliceVal;
  imm = imm >> INSTR_SLICE_META.getSize(INSTR_SLICE_META.IMM_B_1_4);

  result |= Slice(INSTR_SLICE_META.IMM_B_5_10, imm).getSliceVal;
  imm = imm >> INSTR_SLICE_META.getSize(INSTR_SLICE_META.IMM_B_5_10);

  result |= Slice(INSTR_SLICE_META.IMM_B_11, imm).getSliceVal;
  imm = imm >> INSTR_SLICE_META.getSize(INSTR_SLICE_META.IMM_B_11);

  result |= Slice(INSTR_SLICE_META.IMM_B_12, imm).getSliceVal;

  override def dump: Int = {
    result;
  }

}

class U_INSTR(var opcode: Int, var rd_idx: Int,
              var imm: Int)
  extends IntrBase {
  /** integrity check */
  VAL_CHECK.checkRegIdx(rd_idx)
  VAL_CHECK.checkImm((12, 31), imm)

  /** build value */
  result |= opcode;
  result |= Slice(INSTR_SLICE_META.RD, rd_idx).getSliceVal;

  /*** imm encoder*/
  imm = imm >> 12;
  result |= Slice(INSTR_SLICE_META.IMM_U_12_31, imm   ).getSliceVal;


}

class J_INSTR(var opcode: Int, var rd_idx: Int,
              var imm: Int)
  extends IntrBase {
  /** integrity check */
  VAL_CHECK.checkRegIdx(rd_idx)
  VAL_CHECK.checkImm((1, 20), imm)

  /** build value */
  result |= opcode;
  result |= Slice(INSTR_SLICE_META.RD, rd_idx).getSliceVal;

  /*** imm encoder*/
  imm = imm >> 1;

  result |= Slice(INSTR_SLICE_META.IMM_J_1_10, imm   ).getSliceVal;
  imm = imm >> INSTR_SLICE_META.getSize(INSTR_SLICE_META.IMM_J_1_10);

  result |= Slice(INSTR_SLICE_META.IMM_J_11, imm).getSliceVal;
  imm = imm >> INSTR_SLICE_META.getSize(INSTR_SLICE_META.IMM_J_11);

  result |= Slice(INSTR_SLICE_META.IMM_J_12_19, imm).getSliceVal;
  imm = imm >> INSTR_SLICE_META.getSize(INSTR_SLICE_META.IMM_J_12_19);

  result |= Slice(INSTR_SLICE_META.IMM_J_20, imm).getSliceVal;
  imm = imm >> INSTR_SLICE_META.getSize(INSTR_SLICE_META.IMM_J_20);


}