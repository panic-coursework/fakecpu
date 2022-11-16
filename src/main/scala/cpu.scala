package org.altk.lab.cpu

import chisel3._
import chisel3.util.Valid

class Cpu extends CModule {
  val io = IO(new Bundle {
    val ram = Flipped(new RamIo())
    val ioBufferFull = Input(Bool())
    val dbg = Output(Valid(RawInstructionWithPc))
  })
  val ramCtrl = CModule(new RamController)
  ramCtrl.io.ram <> io.ram
  val icache = CModule(new Cache()(new CacheParameters(1, 8, 8, true)))
  ramCtrl.io.instruction <> icache.io.ram
  ramCtrl.io.data <> DontCare
  val ifetch = CModule(new InstructionFetchUnit)
  icache.io.interface <> ifetch.io.icache
  ifetch.io.forcePc.valid := false.B
  ifetch.io.forcePc.bits := DontCare
  ifetch.io.next <> io.dbg
  when (ready) {
    // TODO
  }
}
