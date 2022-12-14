package org.altk.lab.cpu

import chisel3._
import chisel3.util._

object AluOp {
  def T = UInt(5.W)

  val add  = "b00000".U
  val sll  = "b00001".U
  val slt  = "b00010".U
  val sltu = "b00011".U
  val xor  = "b00100".U
  val srl  = "b00101".U
  val or   = "b00110".U
  val and  = "b00111".U
  val sub  = "b01000".U
  val sra  = "b01101".U
  val beq  = "b10000".U
  val bne  = "b10001".U
  val blt  = "b10100".U
  val bge  = "b10101".U
  val bltu = "b10110".U
  val bgeu = "b10111".U
}

import AluOp._

class AluRequest extends CBundle {
  val op = AluOp.T
  val lhs = Word
  val rhs = Word
  val dest = RobId
}

class ArithmeticLogicUnit extends CModule {
  val io = IO(new CBundle {
    val req = Input(CValid(new AluRequest))
    val resp = Output(CdbSender)
  })

  val resp = OutputReg(io.resp)

  def pad (b: Bool) = 0.U((p.xlen - 1).W) ## b

  when (ready) {
    resp.valid := io.req.valid
    resp.bits.id := io.req.bits.dest
    val lhs: UInt = io.req.bits.lhs
    val rhs: UInt = io.req.bits.rhs
    resp.bits.value := MuxLookup(io.req.bits.op, 0xdeadbeefL.U, Seq(
      add  -> (lhs + rhs),
      sll  -> (lhs << rhs(4, 0)),
      slt  -> pad(lhs.asSInt < lhs.asSInt),
      sltu -> pad(lhs < rhs),
      xor  -> (lhs ^ rhs),
      srl  -> (lhs >> rhs(4, 0)),
      or   -> (lhs | rhs),
      and  -> (lhs & rhs),
      sub  -> (lhs - rhs),
      sra  -> (lhs.asSInt >> rhs(4, 0)).asUInt,
      beq  -> pad(lhs === rhs),
      bne  -> pad(lhs =/= rhs),
      blt  -> pad(lhs.asSInt < rhs.asSInt),
      bge  -> pad(lhs.asSInt >= rhs.asSInt),
      bltu -> pad(lhs < rhs),
      bgeu -> pad(lhs >= rhs),
    ))

    when (io.req.valid) {
      dprintf("alu", p"req: ${io.req.bits}")
    }
  }.otherwise {
    io <> DontCare
  }
}
