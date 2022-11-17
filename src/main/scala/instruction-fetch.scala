package org.altk.lab.cpu

import chisel3._
import chisel3.util.Valid

class InstructionFetchUnit extends CModule {
  val io = IO(new Bundle {
    val icache = new ReadOnlyCacheInterface
    val forcePc = Input(Valid(Address))
    val next = Output(Valid(RawInstructionWithPc))
  })
  val pc = RegInit(Address, p.initialPc)
  val nextPc = Wire(Valid(Address))
  val lastPc = CRegNext(pc)
  val lastReady = CRegNext(io.icache.ready)

  when (ready) {
    io.icache.address := pc
    io.icache.valid := !io.forcePc.valid
    io.next.valid := lastReady && !io.forcePc.valid
    io.next.bits.pc := lastPc
    io.next.bits.instruction := io.icache.dataRead

    nextPc.valid := ready && (io.forcePc.valid || io.icache.ready)
    nextPc.bits := Mux(io.forcePc.valid, io.forcePc.bits, pc + 4.U)
    when (nextPc.valid) {
      // TODO: JAL early jump
      pc := nextPc.bits
    }
  }.otherwise {
    io <> DontCare
  }
}
