package org.altk.lab.cpu

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

object RobOp {
  val reg :: store :: branch :: jalr :: unimp :: Nil = Enum(5)
  def T = chiselTypeOf(reg)
}

class RobLine extends CBundle {
  val valid = Bool()

  val pc = Address

  val op = RobOp.T
  val value = CValid(Word)

  // for reg, branch, jalr
  val dest = CValid(RegId)
  // for store, jalr
  val size = LoadStoreSize.T
  val addr = CValid(Address)
  // for branch
  val fallback = Address
  val predictedTake = Bool()
  val history = BpHistory
  // for jalr
  val predictedAddr = Address
}

class RobQuery extends CBundle {
  val req = Input(RobId)
  val resp = Output(CValid(Word))
}

class BranchInfo extends CBundle {
  // for branch
  val fallback = Address
  val predictedTake = Bool()
  val history = BpHistory
  // for jalr
  val predictedAddr = Bool()
}

class RobEnqueue extends CBundle {
  val pc = Address
  val op = RobOp.T
  val value = CValid(Word)
  val dest = CValid(RegId)
  val size = LoadStoreSize.T
  val branch = new BranchInfo
}

class ReorderBuffer extends CModule {
  val io = IO(new CBundle {
    val enq = Flipped(Decoupled(new RobEnqueue))
    val cdb = Input(CdbReceiver)
    val clear = Output(CValid(Address))
    val enqPtr = Output(RobId)
    val deqPtr = Output(RobId)

    val setReg = Output(CValid(new RegUpdate))
    val bpFeedback = Output(CValid(new BpFeedback))
    val lsqEnq = Decoupled(new LsQueuePayload)

    val lbQuery = if (p.loadOoO) Some(new LoadBufferQuery) else None
    val lbUnblock = if (p.loadOoO) Some(Output(CValid(RobId))) else None
    val sbWrite = Input(CValid(new StoreBufferWrite))
    val decodeQuery = Vec(p.robQueryLines, new RobQuery)

    val pc = Output(Address)
  })

  val buffer = CModule(new CQueue(p.robWidth, new RobLine) {
    val update = IO(Input(CdbReceiver))
    val lbQuery = if (p.loadOoO) Some(IO(new LoadBufferQuery)) else None
    val sbWrite = IO(Input(CValid(new StoreBufferWrite)))
    val decodeQuery = IO(Vec(p.robQueryLines, new RobQuery))
    val enqPtr = IO(Output(chiselTypeOf(enq_ptr.value)))
    val deqPtr = IO(Output(chiselTypeOf(deq_ptr.value)))
    enqPtr := enq_ptr.value
    deqPtr := deq_ptr.value

    // register file may become corrupt on rob pointers match (and there are no good alternative
    // fixes that I could think of), so leave out some space
    io.enq.ready := enq_ptr.value + 1.U =/= deq_ptr.value

    lbQuery.map(_.block := MuxCase(CValid.bind(false.B, 0.U(p.robWidth.W)), (0 until p.robLines).map { i: Int =>
      val notInQueue = empty || (!full && Mux(
        enq_ptr.value > deq_ptr.value,
        i.U >= enq_ptr.value || i.U < deq_ptr.value,
        i.U >= enq_ptr.value && i.U < deq_ptr.value,
      ))
      val afterQuery = (i.U - deq_ptr.value) >= (lbQuery.get.id - deq_ptr.value)
      val line = ram(i)
      val notStore = line.op =/= RobOp.store
      val notOverlapping = line.addr.valid && !overlap(lbQuery.get.addr, lbQuery.get.size, line.addr.bits, line.size)
      !(notInQueue || afterQuery || notStore || notOverlapping) -> CValid.bind(true.B, i.U(p.robWidth.W))
    }))

    when (ready && sbWrite.fire) {
      val line = ram(sbWrite.bits.id)
      line.addr.valid  := true.B
      line.addr.bits   := sbWrite.bits.addr
      line.value.valid := true.B
      line.value.bits  := sbWrite.bits.value
    }

    for (i <- 0 until p.robQueryLines) {
      val query = decodeQuery(i)
      query.resp := Mux(
        query.req === enq_ptr.value && io.enq.fire,
        io.enq.bits.value,
        ram(query.req).value,
      )
    }

    when (ready) {
      for (i <- 0 until p.cdb.lines) {
        val req = update(i)
        val line = ram(req.bits.id)
        when (req.valid) {
          when (line.op === RobOp.jalr) {
            line.addr.valid := true.B
            line.addr.bits := req.bits.value
          }.otherwise {
            line.value.valid := true.B
            line.value.bits := req.bits.value
          }
        }
      }
      when (io.deq.fire) {
        ram(deq_ptr.value).valid := false.B
      }
    }
  })

  io.lbQuery.map(_ <> buffer.lbQuery.get)
  io.decodeQuery <> buffer.decodeQuery
  io.enqPtr <> buffer.enqPtr
  io.deqPtr <> buffer.deqPtr
  io.sbWrite <> buffer.sbWrite
  io.cdb <> buffer.update
  io.lbUnblock.map(_.valid := false.B)
  io.lbUnblock.map(_.bits  := DontCare)

  val clear = OutputReg(io.clear)
  val setReg = OutputReg(io.setReg)
  val bpFeedback = OutputReg(io.bpFeedback)
  val lsqEnq = OutputReg(io.lsqEnq.bits)
  val lsqEnqValid = OutputReg(io.lsqEnq.valid)
  val pc = OutputRegInit(io.pc, 0xdeadbeefL.U(p.xlen.W))

  when (reset.asBool) {
    Seq(clear, setReg, bpFeedback).foreach(_.valid := false.B)
    lsqEnqValid := false.B
  }

  when (ready) {
    buffer.io.flush := clear.valid
    setReg.valid := false.B
    setReg.bits := DontCare
    bpFeedback.valid := false.B
    bpFeedback.bits := DontCare
    when (io.lsqEnq.fire) {
      lsqEnqValid := false.B
      lsqEnq := DontCare
    }
    clear.valid := false.B

    when (!clear.valid) {
      val enq = Wire(new RobLine)
      val enqIn = io.enq.bits
      buffer.io.enq.valid := io.enq.valid
      buffer.io.enq.bits  := enq
      io.enq.ready := buffer.io.enq.ready

      enq.pc            := enqIn.pc
      enq.addr.valid    := false.B
      enq.addr.bits     := DontCare
      enq.value         := enqIn.value
      enq.dest          := enqIn.dest
      enq.fallback      := enqIn.branch.fallback
      enq.history       := enqIn.branch.history
      enq.op            := enqIn.op
      enq.predictedAddr := enqIn.branch.predictedAddr
      enq.predictedTake := enqIn.branch.predictedTake
      enq.size          := enqIn.size
      enq.valid         := true.B

      when (buffer.io.enq.fire) {
        dprintf("rob", p"enq ptr ${io.enqPtr}, content $enq")
      }

      commit()
    }.otherwise {
      io <> DontCare
      buffer.io.enq.noenq()
      buffer.io.deq.ready := false.B
    }
  }.otherwise {
    io <> DontCare
    buffer.io <> DontCare
  }

  def commit (): Unit = {
    val line = buffer.io.deq.bits
    buffer.io.deq.ready := line.value.valid &&
      ((line.op =/= RobOp.store && line.op =/= RobOp.jalr) || line.addr.valid) &&
      (!lsqEnqValid || io.lsqEnq.ready)
    when (buffer.io.deq.fire) {
      pc := line.pc
      dprintf("commit", p"id: ${buffer.deqPtr}, line: $line")
      when (line.op === RobOp.branch || line.op === RobOp.jalr) {
        // JAL / JALR
        when (line.dest.valid) {
          setReg.valid      := true.B
          setReg.bits.id    := line.dest.bits
          setReg.bits.value := line.value.bits
          setReg.bits.rob   := buffer.deqPtr
        }
        val altAddr = Mux(
          line.op === RobOp.branch,
          line.fallback,
          alignPc(line.addr.bits),
        )
        val mispredicted = Mux(
          line.op === RobOp.branch,
          line.predictedTake =/= line.value.bits(0),
          line.predictedAddr =/= altAddr,
        )
        when (line.op === RobOp.branch) {
          bpFeedback.valid              := true.B
          bpFeedback.bits.pc            := line.pc
          bpFeedback.bits.predictedTake := line.predictedTake
          bpFeedback.bits.actualTake    := line.value.bits
          bpFeedback.bits.history       := line.history
        }
        when (mispredicted) {
          dprintf("rob", "branch mispredicted, clearing pipeline in favor of %x", altAddr);
          clear.valid := true.B
          clear.bits := altAddr
        }
      }
      when (line.op === RobOp.reg) {
        setReg.valid      := true.B
        setReg.bits.id    := line.dest.bits
        setReg.bits.value := line.value.bits
        setReg.bits.rob   := buffer.deqPtr
      }
      when (line.op === RobOp.store) {
        lsqEnqValid := true.B
        val lsq = lsqEnq
        lsq.op      := LsQueueOp.store
        lsq.addr    := line.addr.bits
        lsq.size    := line.size
        lsq.rob     := DontCare
        lsq.cleared := DontCare
        lsq.value   := line.value.bits

        io.lbUnblock.map(_.valid := true.B)
        io.lbUnblock.map(_.bits  := buffer.deqPtr)
      }
      when (line.op === RobOp.unimp) {
        // unreachable
      }
    }
  }
}
