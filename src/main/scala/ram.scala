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
  val io = IO(new Bundle {
    val ram = Flipped(new RamIo)
    val instruction = Flipped(new RamInterface)
    val data = Flipped(new RamInterface)
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
}

class Ram (size: Int, filename: Option[String]) extends CModule {
  val io = IO(new RamIo)
  val mem = SyncReadMem(size, Byte)
  when (ready) {
    when (io.writeEnable) {
      mem.write(io.address, io.dataIn)
      io.dataOut := DontCare
    }.otherwise {
      io.dataOut := mem.read(io.address)
    }
  }.otherwise {
    io <> DontCare
  }

  filename.map { loadMemoryFromFileInline(mem, _) }
}
