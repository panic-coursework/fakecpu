package org.altk.lab.cpu

import chisel3.stage.ChiselStage

object Main extends App {
  val cp = new CacheParameters(2, 2, 4)
  (new ChiselStage).emitVerilog(new Cache()(cp))
}
