package org.altk.lab.cpu

import chisel3._
import chisel3.util._

object LsQueueOp {
  val load :: store :: Nil = Enum(2)
  def T = chiselTypeOf(load)
}

object LoadStoreSize {
  def T = UInt(3.W)
  val byte   = "b000".U
  val hword  = "b001".U
  val word   = "b010".U
  val byteu  = "b100".U
  val hwordu = "b101".U
}

class LsQueuePayload extends CBundle {
  val op = LsQueueOp.T
  val addr = Address
  val size = LoadStoreSize.T

  // load only
  val rob = RobId
  val cleared = Bool()
  // store only
  val value = Word
}

class LoadStoreQueue extends CModule {
  val io = IO(new CBundle {
    val enq = Vec(p.lsq.enq.lines, Flipped(Decoupled(new LsQueuePayload)))
    val clear = Input(Bool())
    val full = Output(Bool())
    val broadcast = Output(CdbSender)
    val ram = new RamInterface
  })

  val queue = CModule(new CQueue(p.lsq.width, new LsQueuePayload) {
    val clear = IO(Input(Bool()))

    when (ready && clear) {
      for (i <- 0 until entries) {
        ram(i).cleared := true.B
      }
    }
  })
  queue.clear := io.clear
  io.full := !queue.io.enq.ready

  val offset = RegInit(0.U(3.W))
  val lastRamFire = CRegNextInit(io.ram.req.fire, false.B)
  val loadValue = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
  val broadcast = OutputReg(io.broadcast)
  when (reset.asBool)(broadcast.valid := false.B)

  when (ready) {
    queue.io.enq.noenq()
    (0 until p.lsq.enq.lines).foldLeft(queue.io.enq.ready) { (cond, i) =>
      io.enq(i).ready := cond
      when (io.enq(i).fire) {
        queue.io.enq.enq(io.enq(i).bits)
      }
      cond && !io.enq(i).fire
    }

    broadcast.valid := false.B
    io.ram.req.valid := false.B
    io.ram.req.bits := DontCare
    queue.io.deq.ready := false.B
    queue.io.flush := false.B

    def loadComplete () = {
      offset := 0.U
      (0 until p.wordSize).foreach(loadValue(_) := 0.U)
      queue.io.deq.ready := true.B
    }

    when (queue.io.deq.valid) {
      val line = queue.io.deq.bits
      val max = lsSize(line.size)
      when (line.op === LsQueueOp.load) {
        when (line.cleared) {
          loadComplete()
        }.elsewhen (offset < max) {
          io.ram.req.valid            := true.B
          io.ram.req.bits.writeEnable := false.B
          io.ram.req.bits.address     := line.addr + offset
          when (io.ram.req.fire) {
            offset := offset + 1.U
          }
          when (lastRamFire) {
            loadValue(CRegNext(offset)) := io.ram.resp
          }
        }.otherwise {
          loadComplete()

          val value = cat(line.size)
          dprintf("lsq", p"loaded $value from addr ${line.addr} size $max")
          broadcast.valid      := true.B
          broadcast.bits.id    := line.rob
          broadcast.bits.value := value
        }
      }.otherwise {
        when (offset < max) {
          val bytes = (0 until p.wordSize).map { i => line.value(i * 8 + 7, i * 8) }
          io.ram.req.valid            := true.B
          io.ram.req.bits.writeEnable := true.B
          io.ram.req.bits.address     := line.addr + offset
          io.ram.req.bits.dataIn      := MuxLookup(offset, 0.U, bytes.zipWithIndex.map { case (b, i) => i.U -> b })
          when (io.ram.req.fire) {
            offset := offset + 1.U
          }
        }.otherwise {
          offset := 0.U
          queue.io.deq.ready := true.B
          dprintf("lsq", p"stored ${line.value} to addr ${line.addr} size $max")
        }
      }
    }
  }.otherwise {
    io <> DontCare
    queue.io <> DontCare
  }

  def cat (size: UInt): UInt = {
    val value = WireDefault(loadValue)
    when (lastRamFire) {
      val byte = io.ram.resp
      value(lsSize(size) - 1.U) := byte
    }
    val word = WireDefault((0 until p.wordSize).map(value(_)).reverse.reduceLeft(_ ## _))
    when (size === LoadStoreSize.byte) {
      word := (value(0).asSInt pad p.xlen).asUInt
    }.elsewhen (size === LoadStoreSize.hword) {
      word := ((value(1) ## value(0)).asSInt pad p.xlen).asUInt
    }
    word
  }
}
