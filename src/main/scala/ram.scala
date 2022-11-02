package org.altk.lab.cpu

import chisel3._
import chisel3.util._

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
  for ((iface, i) <- Seq(io.instruction, io.data).zipWithIndex) {
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
