package org.altk.lab.cpu

import chisel3._
import chisel3.util._

import org.altk.lab.cpu.InstructionType._

object InstructionType {
  val arithmetic :: load :: store :: csr :: unimp :: Nil = Enum(5)
  val branch = arithmetic
  val jump = arithmetic
  def T = chiselTypeOf(arithmetic)
}

class RsUnitEnqueue extends CBundle {
  val itype = InstructionType.T
  val op = AluOp.T
  val size = LoadStoreSize.T
  // for LOAD, value1 = addr
  // for STORE, value1 = addr, value2 = value
  val value1 = new Register
  val value2 = new Register
  val dest = RobId
  val offset = Address
}

class LoadBufferQuery extends CBundle {
  val id = Input(RobId)
  val addr = Input(Address)
  val size = Input(LoadStoreSize.T)
  val block = Output(CValid(RobId))
}

trait HasRegisters extends Data {
  def valueRecords: List[Register]
  def enq (bits: RsUnitEnqueue): Unit
}

class ReservationStationAlu extends CBundle with HasRegisters {
  val op = AluOp.T
  val lhs = new Register
  val rhs = new Register
  val dest = RobId
  def valueRecords: List[Register] = List(lhs, rhs)
  def enq (bits: RsUnitEnqueue): Unit = {
    op := bits.op
    lhs := bits.value1
    rhs := bits.value2
    dest := bits.dest
  }
}

class LoadBuffer extends CBundle with HasRegisters {
  val addr = new Register
  val size = LoadStoreSize.T
  val dest = RobId
  val offset = Address
  val blocked = CValid(RobId)
  def valueRecords: List[Register] = List(addr)
  def enq (bits: RsUnitEnqueue): Unit = {
    size := bits.size
    addr := bits.value1
    dest := bits.dest
    offset := bits.offset
    blocked.valid := false.B
  }
}

class StoreBuffer extends CBundle with HasRegisters {
  val addr = new Register
  val value = new Register
  val offset = Address
  val dest = RobId
  def valueRecords: List[Register] = List(addr, value)
  def enq (bits: RsUnitEnqueue): Unit = {
    addr := bits.value1
    value := bits.value2
    offset := bits.offset
    dest := bits.dest
  }
}

class StoreBufferWrite extends CBundle {
  val id = RobId
  val addr = Word
  val value = Word
}

class RsQueue[T <: HasRegisters](w: Int, gen: T) extends CModule {
  val io = IO(new CBundle {
    val cdb = Input(CdbReceiver)
    val enq = Flipped(Decoupled(gen))
    val deq = Decoupled(gen)
    val robDeq = Input(RobId)
    val lbBlock = if (gen.isInstanceOf[LoadBuffer]) Some(Input(CValid(RobId))) else None
    val lbUnblock = if (gen.isInstanceOf[LoadBuffer]) Some(Input(Vec(2, CValid(RobId)))) else None
  })

  val entries = 1 << w

  val free = CModule(new CQueue(w, UInt(w.W), true, Some(VecInit((0 until entries).map(_.U(w.W))))))
  free.io.flush := false.B

  val ram = Reg(Vec(entries, CValid(gen)))
  when (reset.asBool)(ram.foreach(_.valid := false.B))
  val ramAccessors = (0 until entries).map(ram(_))

  io.enq.ready := free.io.deq.valid
  free.io.deq.ready := io.enq.valid

  when (ready) {
    for (i <- 0 until p.cdb.lines) {
      val cdb = io.cdb(i)
      when (cdb.valid) {
        ramAccessors.zipWithIndex.foreach { case (line, lineId) =>
          when (line.valid) {
            line.bits.valueRecords.foreach { rec =>
              when (!rec.valid && rec.src === cdb.bits.id) {
                dprintf("rs", p"cdb match ${cdb.bits} with rs line $lineId")
                rec.valid := true.B
                rec.value := cdb.bits.value
              }
            }
          }
        }
      }
    }

    val readyId = MuxCase(
      CValid.bind(false.B, 0.U(w.W)),
      ramAccessors.zipWithIndex.map { line =>
        (
          line._1.valid &
          line._1.bits.valueRecords.map { _.valid }.reduceLeft(_ & _) &
          (if (gen.isInstanceOf[LoadBuffer]) {
            val lb = line._1.bits.asInstanceOf[LoadBuffer]
            // virtual loads must be in-order
            val virtualValid = lb.addr.value < p.iommu.base.U || lb.dest === io.robDeq
            // prevention of dead loops
            val notBlocked = !lb.blocked.valid
            virtualValid && notBlocked
          } else true.B)
        ) -> CValid.bind(true.B, line._2.U(w.W))
      },
    )
    val deqLine = ram(readyId.bits)
    io.deq.bits := deqLine.bits
    io.deq.valid := readyId.valid
    free.io.enq.valid := io.deq.fire
    free.io.enq.bits := readyId.bits
    when (io.deq.fire) {
      dprintf("rs", p"deq from ${readyId.bits}, content ${deqLine.bits}")
      deqLine.valid := false.B
    }
    io.lbBlock.map { block =>
      val unblocks = (0 to 1).map(io.lbUnblock.get(_))
      when (io.deq.valid && unblocks.map(u => !u.valid || u.bits =/= block.bits).reduce(_ && _)) {
        dprintf("lb", p"block $block to ${deqLine.bits}")
        deqLine.bits.asInstanceOf[LoadBuffer].blocked := block
      }
    }
    io.lbUnblock.map { unblock =>
      for (i <- 0 to 1) {
        val line = unblock(i)
        when (line.valid) {
          ramAccessors.foreach { accessor =>
            val block = accessor.bits.asInstanceOf[LoadBuffer].blocked
            when (block.bits === line.bits) {
              dprintf("lb", p"unblock $block in ${accessor.bits}")
              block.valid := false.B
            }
          }
        }
      }
    }

    when (io.enq.fire) {
      val ix = free.io.deq.bits
      dprintf("rs", p"enq to $ix, content ${io.enq.bits}")
      val accessor = ram(ix)
      accessor.bits := io.enq.bits
      accessor.valid := true.B
    }
  }.otherwise {
    io <> DontCare
    free.io <> DontCare
  }
}

class RsUnit extends CModule {
  val io = IO(new CBundle {
    val cdb = Input(CdbReceiver)
    val clear = Input(Bool())
    val enq = Flipped(Decoupled(new RsUnitEnqueue))

    val alu = Output(CValid(new AluRequest))
    val lb = Decoupled(new LsQueuePayload)
    val sb = Output(CValid(new StoreBufferWrite))

    val lbQuery = Flipped(new LoadBufferQuery)
    val robDeq = Input(RobId)
    val lbUnblock = Input(CValid(RobId))

    val full = Output(UInt(3.W))
  })

  val alu = CModule(new RsQueue(p.rsWidth, new ReservationStationAlu))
  val lb = CModule(new RsQueue(p.loadBufferWidth, new LoadBuffer))
  val sb = CModule(new RsQueue(p.storeBufferWidth, new StoreBuffer))
  lb.io.lbUnblock.map(_(0) := io.lbUnblock)
  Seq(alu, lb, sb).foreach { q =>
    q.reset        := reset.asBool || io.clear
    q.io.enq.valid := false.B
    q.io.enq.bits  := DontCare
    q.io.cdb       := io.cdb
    q.io.robDeq    := io.robDeq
  }
  io.full := Seq(alu, lb, sb).map(!_.io.enq.ready).reduceLeft[UInt](_ ## _)

  val types = Seq(
    arithmetic -> alu,
    load -> lb,
    store -> sb,
  )

  io.enq.ready := MuxLookup(io.enq.bits.itype, true.B, types.map { p =>
    p._1 -> p._2.io.enq.ready
  })

  val aluOut = OutputReg(io.alu)
  val lbOut = OutputReg(io.lb.bits)
  val lbOutValid = OutputReg(io.lb.valid)
  val sbOut = OutputReg(io.sb)
  when (reset.asBool) {
    Seq(aluOut, sbOut).foreach(_.valid := false.B)
    lbOutValid := false.B
  }

  when (ready) {
    when (io.enq.fire && !io.clear) {
      types.foreach { case (itype, queue) =>
        when (io.enq.bits.itype === itype) {
          val enq = queue.io.enq
          enq.valid := true.B
          val data = WireDefault(io.enq.bits)
          for (i <- 0 until p.cdb.lines) {
            val line = io.cdb(i)
            when (line.valid) {
              when (line.bits.id === data.value1.src && !io.enq.bits.value1.valid) {
                data.value1.valid := true.B
                data.value1.value := line.bits.value
              }
              when (line.bits.id === data.value2.src && !io.enq.bits.value2.valid) {
                data.value2.valid := true.B
                data.value2.value := line.bits.value
              }
            }
          }
          enq.bits.enq(data)
        }
      }
    }

    when (io.clear) {
      io.alu.valid := false.B
      io.alu.bits := DontCare
      io.sb.valid := false.B
      io.sb.bits := DontCare
      io.lb.valid := false.B
      io.lb.bits := DontCare
      io.lbQuery <> DontCare
      aluOut.valid := false.B
      sbOut.valid := false.B
      lbOutValid := false.B
      types.foreach(_._2.io.deq.ready := false.B)
      lb.io.lbBlock.get := lb.io.deq.bits.blocked
      lb.io.lbUnblock.get(1) := CValid.bind(false.B, 0.U(p.robWidth.W))
    }.otherwise {
      alu.io.deq.ready := true.B
      aluOut.valid := alu.io.deq.valid
      val aluDeq = alu.io.deq.bits
      aluOut.bits.op   := aluDeq.op
      aluOut.bits.lhs  := aluDeq.lhs.value
      aluOut.bits.rhs  := aluDeq.rhs.value
      aluOut.bits.dest := aluDeq.dest

      sb.io.deq.ready := true.B
      sbOut.valid := sb.io.deq.valid
      val sbDeq = sb.io.deq.bits
      sbOut.bits.id    := sbDeq.dest
      sbOut.bits.addr  := sbDeq.addr.value + sbDeq.offset
      sbOut.bits.value := sbDeq.value.value
      lb.io.lbUnblock.get(1) := CValid.bind(sb.io.deq.fire, sbDeq.dest)

      val lbDeq = lb.io.deq.bits
      val lbAddr = lbDeq.addr.value + lbDeq.offset
      io.lbQuery.id   := lbDeq.dest
      io.lbQuery.addr := lbAddr
      io.lbQuery.size := lbDeq.size
      when (io.lb.ready) {
        lbOut.op      := LsQueueOp.load
        lbOut.addr    := lbAddr
        lbOut.size    := lbDeq.size
        lbOut.rob     := lbDeq.dest
        lbOut.cleared := false.B
        lbOut.value   := DontCare
        lb.io.deq.ready := !io.lbQuery.block.bits
        lbOutValid := lb.io.deq.valid && !io.lbQuery.block.bits
        lb.io.lbBlock.get := io.lbQuery.block
      }.otherwise {
        lb.io.deq.ready := false.B
        lb.io.lbBlock.get := lb.io.deq.bits.blocked
      }
    }
  }.otherwise {
    io <> DontCare
    alu.io <> DontCare
    sb.io <> DontCare
    lb.io <> DontCare
  }
}
