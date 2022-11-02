package org.altk.lab.cpu

import chisel3._
import chisel3.util.Valid

class InstructionFetchUnit extends CModule {
  val io = IO(new Bundle {
    val icache = new CacheInterface
    val forcePc = Input(Valid(Address))
    val next = Output(Valid(RawInstructionWithPc))
  })
  val pc = RegInit(Address, p.initialPc)
  val nextPc = Wire(Valid(Address))
  val lastPc = Reg(Address)
  lastPc := pc

  io.icache.dataWrite <> DontCare
  io.icache.writeEnable := false.B

  io.icache.address := pc
  io.icache.valid := !io.forcePc.valid
  io.next.valid := io.icache.ready && !io.forcePc.valid
  io.next.bits.pc := lastPc
  io.next.bits.instruction := io.icache.dataRead

  nextPc.valid := ready && (io.forcePc.valid || io.icache.ready)
  nextPc.bits := Mux(io.forcePc.valid, io.forcePc.bits, pc + 4.U)
  when (nextPc.valid) {
    // TODO: JAL early jump
    pc := nextPc.bits
  }
}
