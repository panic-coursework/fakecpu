package org.altk.lab.cpu

import scala.language.experimental.macros

import chisel3._
import chisel3.internal.sourceinfo.{InstTransform, SourceInfo, UnlocatableSourceInfo}
import chisel3.util._
import chisel3.internal.firrtl.Width

object CModule {
  def apply[T <: CModule](bc: => T): T = macro InstTransform.apply[T]
  def do_apply[T <: CModule](bc: => T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T = {
    val module = Module.do_apply(bc)(sourceInfo, compileOptions)
    module.ready <> CurrentModule().get.asInstanceOf[CModule].ready
    module
  }
}

class CModule extends Module {
  implicit val ready = IO(Input(Bool()))
}

class CBundle extends Bundle

object CRegNext {
  def apply[T <: Data](next: T)(implicit ready: Bool): T = {
    val reg = Reg(chiselTypeOf(next))
    when (ready) {
      reg := next
    }
    reg
  }
}

object CRegNextInit {
  def apply[T <: Data](next: T, init: T)(implicit ready: Bool): T = {
    val reg = RegInit(init)
    when (ready) {
      reg := next
    }
    reg
  }
}

object OutputReg {
  def apply[T <: Data](out: T): T = {
    val reg = Reg(chiselTypeOf(out))
    out := reg
    reg
  }
}

object OutputRegInit {
  def apply[T <: Data](out: T, init: T): T = {
    val reg = RegInit(init)
    out := reg
    reg
  }
}

class CValid[T <: Data](gen: T) extends CBundle {
  val valid = Bool()
  val bits = gen
  def fire = valid
}

object CValid {
  def apply[T <: Data](gen: T) = new CValid(gen)
  def bind[T <: Data](valid: Bool, bits: T): CValid[T] = {
    val w = Wire(CValid(chiselTypeOf(bits)))
    w.valid := valid
    w.bits := bits
    w
  }
}

class CQueueIO[T <: Data, U <: Data](val w: Width, private val gen: T) extends CBundle {
  val enq = Flipped(Decoupled(gen))
  val deq = Decoupled(gen)
  val count = Output(UInt(w))
  val flush = Input(Bool())
}

class CQueue[T <: Data](val w: Int, val gen: T, val initFull: Boolean = false, initRam: => Option[Vec[T]] = None) extends CModule {
  val io = IO(new CQueueIO(w.W, gen))
  val entries = 1 << w

  val ram = initRam.map(RegInit(_)).getOrElse(Reg(Vec(entries, gen)))
  val enq_ptr = Counter(entries)
  val deq_ptr = Counter(entries)
  val maybe_full = RegInit(initFull.B)
  val ptr_match = enq_ptr.value === deq_ptr.value
  val empty = ptr_match && !maybe_full
  val full = ptr_match && maybe_full

  when (ready) {
    when (io.enq.fire) {
      ram(enq_ptr.value) := io.enq.bits
      enq_ptr.inc()
    }
    when (io.deq.fire) {
      deq_ptr.inc()
    }
    when (io.enq.fire =/= io.deq.fire) {
      maybe_full := io.enq.fire
    }
    when (io.flush) {
      enq_ptr.reset()
      deq_ptr.reset()
      maybe_full := initFull.B
    }

    io.deq.valid := !empty
    io.deq.bits := ram(deq_ptr.value)
    io.enq.ready := !full

    val ptr_diff = enq_ptr.value - deq_ptr.value
    io.count := Mux(full, entries.U, 0.U) | ptr_diff
  }.otherwise {
    io <> DontCare
  }
}

trait MemLike[T <: Data] {
  def apply(i: Int): T
  def apply(i: UInt): T
}

class MemLikeMem[T <: Data](val ram: Mem[T]) extends MemLike[T] {
  def apply(i: Int) = ram(i)
  def apply(i: UInt) = ram(i)
}

class VecLikeMem[T <: Data](val vec: Vec[T]) extends MemLike[T] {
  def apply(i: Int) = vec(i)
  def apply(i: UInt) = vec(i)
}
