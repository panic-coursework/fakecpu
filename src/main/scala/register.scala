package org.altk.lab.cpu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class RegUpdate extends CBundle {
  val id = RegId
  val rob = RobId
  val value = Word
}

class RegQuery extends CBundle {
  val req = Input(RegId)
  val resp = Output(new Register)
}

class Register extends CBundle {
  val valid = Bool()
  val src = RobId
  val value = Word
}

class RegBorrow extends CBundle {
  val id = RegId
  val src = RobId
}

class RegisterFile extends CModule {
  val io = IO(new CBundle {
    val query = Vec(p.reg.query.lines, new RegQuery)
    val borrow = Input(CValid(new RegBorrow))
    val update = Input(CValid(new RegUpdate))
  })

  val regs = Mem(p.reg.count, new Register)

  val incoming = Wire(new Register)
  incoming.valid := true.B
  incoming.src := 0.U
  incoming.value := io.update.bits.value
  for (i <- 0 until p.reg.query.lines) {
    val q = io.query(i)
    val reg = regs(q.req)
    q.resp := MuxCase(reg, Seq(
      (q.req === 0.U) -> (new Register).Lit(_.valid -> true.B, _.src -> 0.U, _.value -> 0.U),
      (io.update.valid && io.update.bits.id === q.req && reg.src === io.update.bits.rob) -> incoming,
    ))
  }

  when (reset.asBool) {
    for (i <- 0 until p.reg.count) {
      val reg = regs(i)
      reg.valid := true.B
      reg.src := 0.U
      reg.value := 0.U
    }
  }

  when (ready) {
    when (io.update.valid) {
      val data = io.update.bits
      when (data.id =/= 0.U) {
        if (p.isFirrtlBuggy) {
          val reg = WireDefault(regs(data.id))
          reg.value := data.value
          reg.valid := data.rob === reg.src
          regs.write(data.id, reg)
        } else {
          val reg = regs(data.id)
          reg.value := data.value
          reg.valid := data.rob === reg.src
        }
      }
    }

    when (io.borrow.valid) {
      val data = io.borrow.bits
      when (data.id =/= 0.U) {
        if (p.isFirrtlBuggy) {
          val reg = WireDefault(regs(data.id))
          reg.valid := false.B
          reg.src := data.src
          regs.write(data.id, reg)
        } else {
          val reg = regs(data.id)
          reg.valid := false.B
          reg.src := data.src
        }
      }
    }
  }.otherwise {
    io <> DontCare
  }
}
