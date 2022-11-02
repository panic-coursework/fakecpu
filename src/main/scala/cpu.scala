package org.altk.lab.cpu

import chisel3._

class Cpu extends CModule {
  val io = IO(new Bundle {
    val ram = new RamIo()
    val ioBufferFull = Input(Bool())
  })
  val ramCtrl = CModule(new RamController)
  when (ready) {
    // TODO
  }
}
