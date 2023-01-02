package org.altk.lab.cpu

import chisel3._
import chisel3.util._

class BpFeedback extends CBundle {
  val pc = Address
  val history = BpHistory
  val predictedTake = Bool()
  val actualTake = Bool()
}

class BpQuery extends CBundle {
  val pc = Input(Address)
  val take = Output(Bool())
  val history = Output(BpHistory)
}

class BranchPredictor extends CModule {
  val io = IO(new CBundle {
    val query = new BpQuery
    val feedback = Input(CValid(new BpFeedback))
  })

  val history = RegInit(0.U(p.bp.history.width.W))
  val ram = Mem(p.bp.history.lines * p.bp.pc.lines, UInt(p.bp.v.bits.W))

  def hash (pc: UInt) = pc(p.bp.pc.skip + p.bp.pc.width - 1, p.bp.pc.skip)

  io.query.history := history
  io.query.take := ram(history ## hash(io.query.pc)) >= p.bp.v.threshold.U

  if (p.iverilog) {
    // branch predicting just doesn't work on iverilog.
    io.query.take := true.B
  }

  if (p.bp.v.init) {
    when (reset.asBool) {
      for (i <- 0 until p.bp.history.lines * p.bp.pc.lines) {
        ram(i) := p.bp.v.threshold.U
      }
    }
  }

  when (ready && io.feedback.valid) {
    val data = io.feedback.bits
    val key = data.history ## hash(data.pc)
    val state = ram(key)
    dprintf("bp", p"updating prediction table for $key take ${data.actualTake}")
    when (data.actualTake) {
      when (state < p.bp.v.max.U) {
        state := state + 1.U
      }
    }.otherwise {
      when (state > 0.U) {
        state := state - 1.U
      }
    }

    history := (history << 1) | data.actualTake
  }
}
