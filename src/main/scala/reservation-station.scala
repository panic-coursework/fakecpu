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
  val addr = Input(Address)
  val size = Input(LoadStoreSize.T)
  val block = Output(Bool())
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
  def valueRecords: List[Register] = List(addr)
  def enq (bits: RsUnitEnqueue): Unit = {
    size := bits.size
    addr := bits.value1
    dest := bits.dest
    offset := bits.offset
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
  })

  val entries = 1 << w

  val free = CModule(new CQueue(w, UInt(w.W), true) {
    when (reset.asBool) {
      for (i <- 0 until entries) {
        ram(i) := i.U
      }
    }
  })
  free.io.flush := false.B

  val ram = Mem(entries, CValid(gen))
  val ramAccessors = (0 until entries).map(ram(_))

  io.enq.ready := free.io.deq.valid
  free.io.deq.ready := io.enq.valid

  when (reset.asBool) {
    ramAccessors.foreach { _.valid := false.B }
  }

  when (ready) {
    for (i <- 0 until p.cdb.lines) {
      val cdb = io.cdb(i)
      when (cdb.valid) {
        ramAccessors.zipWithIndex.foreach { case (line, lineId) =>
          when (line.valid) {
            line.bits.valueRecords.foreach { rec =>
              when (!rec.valid && rec.src === cdb.bits.id) {
                dprintf(p"[rs] cdb match ${cdb.bits} with rs line $lineId\n")
                rec.valid := true.B
                rec.value := cdb.bits.value
              }
            }
          }
        }
      }
    }

    def ready (valid: Bool, bits: UInt): CValid[UInt] = {
      val v = Wire(CValid(UInt(w.W)))
      v.valid := valid
      v.bits := bits
      v
    }

    val readyId = MuxCase(
      ready(false.B, 0.U),
      ramAccessors.zipWithIndex.map { line =>
        (line._1.valid & line._1.bits.valueRecords.map { _.valid }.reduceLeft(_ & _)) -> ready(true.B, line._2.U)
      },
    )
    val deqLine = ram(readyId.bits)
    io.deq.bits := deqLine.bits
    io.deq.valid := readyId.valid
    free.io.enq.valid := io.deq.fire
    free.io.enq.bits := readyId.bits
    when (io.deq.fire) {
      dprintf(p"[rs] deq from ${readyId.bits}, content ${deqLine.bits}\n")
      deqLine.valid := false.B
    }

    when (io.enq.fire) {
      val ix = free.io.deq.bits
      dprintf(p"[rs] enq to $ix, content ${io.enq.bits}\n")
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
  })

  val alu = CModule(new RsQueue(p.rsWidth, new ReservationStationAlu))
  val lb = CModule(new RsQueue(p.loadBufferWidth, new LoadBuffer))
  val sb = CModule(new RsQueue(p.storeBufferWidth, new StoreBuffer))
  Seq(alu, lb, sb).foreach { q =>
    q.reset := reset.asBool || io.clear
    q.io.enq.valid := false.B
    q.io.enq.bits := DontCare
    q.io.cdb <> io.cdb
  }

  val types = Seq(
    arithmetic -> alu,
    load -> lb,
    store -> sb,
  )

  io.enq.ready := MuxLookup(io.enq.bits.itype, true.B, types.map { p =>
    p._1 -> p._2.io.enq.ready
  })

  when (ready) {
    when (io.enq.fire) {
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
      types.foreach(_._2.io.deq.ready := false.B)
    }.otherwise {
      alu.io.deq.ready := true.B
      io.alu.valid := alu.io.deq.valid
      val aluOut = io.alu.bits
      val aluDeq = alu.io.deq.bits
      aluOut.op   := aluDeq.op
      aluOut.lhs  := aluDeq.lhs.value
      aluOut.rhs  := aluDeq.rhs.value
      aluOut.dest := aluDeq.dest

      sb.io.deq.ready := true.B
      io.sb.valid := sb.io.deq.valid
      val sbOut = io.sb.bits
      val sbDeq = sb.io.deq.bits
      sbOut.id    := sbDeq.dest
      sbOut.addr  := sbDeq.addr.value + sbDeq.offset
      sbOut.value := sbDeq.value.value

      val lbOut = io.lb.bits
      val lbDeq = lb.io.deq.bits
      val lbAddr = lbDeq.addr.value + lbDeq.offset
      lbOut.op      := LsQueueOp.load
      lbOut.addr    := lbAddr
      lbOut.size    := lbDeq.size
      lbOut.rob     := lbDeq.dest
      lbOut.cleared := false.B
      lbOut.value   := DontCare
      io.lbQuery.addr := lbAddr
      io.lbQuery.size := lbDeq.size
      lb.io.deq.ready := io.lb.ready && !io.lbQuery.block
      io.lb.valid := lb.io.deq.valid && !io.lbQuery.block
    }
  }.otherwise {
    io <> DontCare
    alu.io <> DontCare
    sb.io <> DontCare
    lb.io <> DontCare
  }
}
