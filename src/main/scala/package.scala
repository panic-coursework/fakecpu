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
    val arithmetic :: bitwise :: jump :: branch :: indirection :: Nil = Enum(5)
    def T = chiselTypeOf(arithmetic)
  }
  // TODO
  def Instruction = new Bundle {
    val itype = InstructionType.T
    val opcode = Opcode
    val rd = Register
    val funct3 = Funct3
    val rs1 = Register
    val rs2 = Register
    val funct7 = Funct7
    // immediate numbers
    val immIS = UInt(12.W)
    val immSB = UInt(13.W)
    val immU = UInt(32.W)
    val immUJ = UInt(21.W)
  }
}
