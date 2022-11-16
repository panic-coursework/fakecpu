package org.altk.lab.cpu

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.RawTester.test
import chisel3.stage.ChiselStage

class TestModule extends CModule {
  val io = IO(Output(Valid(RawInstructionWithPc)))
  val cpu = CModule(new Cpu)
  val ram = CModule(new Ram(128 * 1024 * 1024, Some("data.data")))
  cpu.io.ram <> ram.io
  cpu.io.ioBufferFull := false.B
  io <> cpu.io.dbg
}

object Main extends App {
  test (new TestModule, Seq(VerilatorBackendAnnotation)) { c =>
    c.ready.poke(true.B)
    for (i <- 0 until 512) {
      println(s"Cycle $i: Valid? ${c.io.valid.peek().litValue}, PC: ${c.io.bits.peek().pc.litValue.toString(16)}, Inst: ${c.io.bits.peek().instruction.litValue.toString(16)}")
      c.clock.step(1)
    }
  }
  // (new ChiselStage).emitVerilog(new Cpu)
}
