package org.altk.lab.cpu

import chisel3._
import chisel3.util._

import org.altk.lab.cpu.`package`.InstructionType._

class InstructionDecoder extends CModule {
  val io = IO(new Bundle {
    val input = Input(Valid(RawInstructionWithPc))
    val output = Output(Valid(Instruction))
  })

  val ins = io.input.bits.instruction
  val out = OutputReg(io.output.bits)
  val outValid = OutputReg(io.output.valid)

  when (ready) {
    outValid := io.input.valid
    out.pc := io.input.bits.pc

    val opcode = ins(6, 0)
    out.opcode := opcode
    out.rd := ins(11, 7)
    out.funct7 := ins(31, 25)
    out.funct3 := ins(14, 12)
    out.rs1 := ins(19, 15)
    out.rs2 := ins(24, 20)
    out.funct7 := ins(31, 25)

    val immI = ins(31, 20).asSInt pad p.xlen
    val immS = (ins(31, 25) ## ins(11, 7)).asSInt pad p.xlen
    val immB = (ins(31) ## ins(7) ## ins(30, 25) ## ins(11, 8)).asSInt pad p.xlen
    val immU = (ins(31, 12) ## 0.U(12.W)).asSInt pad p.xlen
    val immJ = (ins(31) ## ins(19, 12) ## ins(20) ## ins(30, 21) ## 0.U(1.W)).asSInt pad p.xlen

    val OP_IMM   = "b0010011".U // I
    val LUI      = "b0110111".U // U
    val AUIPC    = "b0010111".U // U
    val OP       = "b0110011".U // R
    val JAL      = "b1101111".U // J
    val JALR     = "b1100111".U // I
    val BRANCH   = "b1100011".U // B
    val LOAD     = "b0000011".U // I
    val STORE    = "b0100011".U // S
    val MISC_MEM = "b0001111".U
    val SYSTEM   = "b1110011".U // I

    val pc = io.input.bits.pc.asSInt
    when (opcode === OP_IMM) {
      out.immediate := immI
      out.itype := arithmetic
    }.elsewhen (opcode === LUI) {
      out.immediate := immU
      out.itype := arithmetic
    }.elsewhen (opcode === AUIPC) {
      out.immediate := immU + pc
      out.itype := arithmetic
    }.elsewhen (opcode === OP) {
      out.immediate := DontCare
      out.itype := arithmetic
    }.elsewhen (opcode === JAL) {
      out.immediate := immJ + pc
      out.itype := jump
    }.elsewhen (opcode === JALR) {
      out.immediate := immI
      out.itype := branch
    }.elsewhen (opcode === BRANCH) {
      out.immediate := immB + pc
      out.itype := branch
    }.elsewhen (opcode === LOAD) {
      out.immediate := immI
      out.itype := load
    }.elsewhen (opcode === STORE) {
      out.immediate := immS
      out.itype := store
    }.elsewhen (opcode === MISC_MEM) {
      out.immediate := DontCare
      out.itype := unimp
    }.elsewhen (opcode === SYSTEM) {
      out.immediate := immI
      out.itype := unimp
    }.otherwise {
      out.immediate := DontCare
      out.itype := unimp
    }
  }.otherwise {
    io <> DontCare
  }
}
