package org.altk.lab

import chisel3._
import chisel3.util.Enum

package object cpu {
  def Word = UInt(p.xlen.W)
  def Address = Word
  def Byte = UInt(8.W)
  def RawInstruction = Word
  def RawInstructionWithPc = new Bundle {
    val pc = Address
    val instruction = RawInstruction
  }

  def Funct7 = UInt(7.W)
  def Funct3 = UInt(3.W)
  def Opcode = UInt(7.W)
  def Register = UInt(5.W)

  object InstructionType {
    val arithmetic :: jump :: branch :: indirection :: csr :: unimp :: Nil = Enum(6)
    val load = indirection
    val store = indirection
    def T = chiselTypeOf(arithmetic)
  }
  // TODO
  def Instruction = new Bundle {
    val pc = Address
    val itype = InstructionType.T
    val opcode = Opcode
    val rd = Register
    val funct3 = Funct3
    val rs1 = Register
    val rs2 = Register
    val funct7 = Funct7
    val immediate = SInt(32.W)
    override def toString () =
      s"pc: ${pc.litValue.toString(16)}, itype: ${itype.litValue}, opcode: ${opcode.litValue.toString(2)}, rd: ${rd.litValue}, funct3: ${funct3.litValue.toString(2)}, rs1: ${rs1.litValue}, rs2: ${rs2.litValue}, funct7: ${funct7.litValue.toString(2)}, imm: ${immediate.litValue.toString(16)}"
  }
}
