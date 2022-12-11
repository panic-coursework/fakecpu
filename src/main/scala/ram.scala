package org.altk.lab.cpu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline

class RamIo extends CBundle {
  val dataIn = Input(Byte)
  val dataOut = Output(Byte)
  val address = Input(Address)
  val writeEnable = Input(Bool())
}

class RamRequest extends CBundle {
  val dataIn = Byte
  val address = Address
  val writeEnable = Bool()
  def read (addr: Data) = {
    dataIn := DontCare
    writeEnable := false.B
    address := addr
  }
  def write (addr: Data, bits: Data) = {
    dataIn := bits
    writeEnable := true.B
    address := addr
  }
}
class RamInterface extends CBundle {
  val req = Decoupled(new RamRequest())
  val resp = Input(Byte)
}

class RamController extends CModule {
  val io = IO(new CBundle {
    val ram = Flipped(new RamIo)
    val instruction = Flipped(new RamInterface)
    val data = Flipped(new RamInterface)
    val ioBufferFull = Input(Bool())
  })
  val arbiter = Module(new Arbiter(new RamRequest(), 2))
  for ((iface, i) <- Seq(io.data, io.instruction).zipWithIndex) {
    arbiter.io.in(i) <> iface.req
    iface.resp := io.ram.dataOut
  }
  arbiter.io.out.ready := true.B
  val req = arbiter.io.out.bits
  val valid = arbiter.io.out.valid
  io.ram.dataIn := Mux(valid, req.dataIn, 0.U)
  io.ram.address := Mux(valid, req.address, 0.U)
  io.ram.writeEnable := Mux(valid, req.writeEnable, false.B)

  when (ready) {
    when (io.ioBufferFull) {
      // TODO
    }
  }
}

class Ram (size: Int, filename: Option[String]) extends CModule {
  val io = IO(new RamIo)
  val mem = SyncReadMem(size, Byte)
  io.dataOut := mem.read(io.address)

  when (ready) {
    when (io.writeEnable) {
      mem.write(io.address, io.dataIn)
    }
  }.otherwise {
    io <> DontCare
  }

  filename.map { loadMemoryFromFileInline(mem, _) }
}

class Iommu extends CModule {
  val io = IO(new CBundle {
    val cpu = new RamIo
    val ram = Flipped(new RamIo)
    val halt = Output(Bool())
  })

  val clk = RegInit(0.U(64.W))

  io.ram.address := io.cpu.address
  io.halt := false.B
  val lastAddress = CRegNext(io.cpu.address)
  val vdata = MuxLookup(lastAddress(2, 0), 0.U, Seq(
    0.U -> 0.U, // TODO: read a byte from cpuput
    4.U -> clk(7, 0),
    5.U -> clk(15, 8),
    6.U -> clk(23, 16),
    7.U -> clk(31, 24),
  ))

  io.ram.dataIn := io.cpu.dataIn
  val vread = lastAddress >= p.iommu.base.U
  io.cpu.dataOut := Mux(vread, vdata, io.ram.dataOut)
  val vwrite = io.cpu.address >= p.iommu.base.U
  io.ram.writeEnable := Mux(vwrite, false.B, io.cpu.writeEnable)

  when (ready) {
    clk := clk + 1.U
    when (vwrite && io.cpu.writeEnable) {
      when (io.cpu.address === 0x30000.U) {
        printf("[iommu] %x (%c)\n", io.cpu.dataIn, io.cpu.dataIn)
      }.elsewhen (io.cpu.address === 0x30004.U) {
        io.halt := true.B
      }.otherwise {
        printf(p"\nUnknown write at ${io.cpu.address}\n")
      }
    }
  }
}
