package org.altk.lab.cpu

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import chiseltest.RawTester.test
import chisel3.stage.ChiselStage

class TestModule extends CModule {
  val io = IO(new CBundle {
    val halt = Output(Bool())
    val pc = Output(Address)
  })
  val cpu = CModule(new Cpu)
  val ram = CModule(new Ram(p.testModule.ram.size, p.testModule.ram.dump))
  val iommu = CModule(new Iommu(p.testModule.infile))
  io.halt := iommu.io.halt
  cpu.io.ram <> iommu.io.cpu
  ram.io <> iommu.io.ram
  cpu.io.ioBufferFull := false.B
  cpu.io.pcSel := false.B
  io.pc := cpu.io.pc
}

class Poc extends Module {
  val io = IO(new Bundle {
    val sel = Input(UInt(1.W))
    val en = Input(Bool())
    val value = Output(Valid(UInt(1.W)))
  })
  // Valid: { valid, bits }
  val mem = Mem(2, Valid(UInt(1.W)))
  val readwriter1 = mem(0.U)
  val readwriter2 = mem(readwriter1.bits)
  io.value.valid := readwriter2.valid
  io.value.bits := readwriter2.bits
  when (io.en) {
    readwriter1.valid := true.B
    readwriter2.valid := false.B
  }
}

object Main extends App {
  (new ChiselStage).emitVerilog(new Poc, Array("-X", "low"))
  p.mode match {
    case "codegen" =>
      (new ChiselStage).emitVerilog(p.codegen.module, p.codegen.options)

    case "test" =>
      test (new TestModule, Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.clock.setTimeout(0)
        c.reset.poke(true.B)
        c.clock.step(2)
        c.reset.poke(false.B)
        var cycle = 0
        while (!c.io.halt.peek().litToBoolean) {
          c.clock.step(1)
          c.ready.poke(true.B)
          cycle += 1
          if (cycle > 100000) {
            throw new Exception("CPU not finishing on time")
          }
        }
      }

    case _ =>
      throw new Exception(f"Unknown run mode ${p.mode}")
  }
}
