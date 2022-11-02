package org.altk.lab.cpu

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import chisel3.util.Valid

class CacheInterface extends CBundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val address = Output(Address)
  val dataRead = Input(Word)
  val dataWrite = Output(Word)
  val writeEnable = Output(Bool())
}

case class CacheParameters(
  val ways: Int,
  val indexBits: Int,
  val blockBits: Int,
) {
  def tagBits = p.xlen - indexBits - blockBits
  def nBytesInLine = 1 << blockBits
  def nSets = 1 << indexBits
}

class CacheLine(implicit cp: CacheParameters) extends CBundle {
  val valid = Bool()
  val tag = UInt(cp.tagBits.W)
  val data = Vec(cp.nBytesInLine, Byte)
}

class Cache(implicit cp: CacheParameters) extends CModule {
  val io = IO(new Bundle {
    val interface = Flipped(new CacheInterface)
    val ram = new RamInterface
  })
  val sets = RegInit(VecInit(Seq.fill(cp.nSets) { (new CacheLine).Lit(
    _.valid -> false.B,
    _.tag -> 0.U,
    _.data -> Vec.Lit(Seq.fill(cp.nBytesInLine) { 0.U(8.W) }: _*),
  ) }))
  io.interface <> DontCare
  io.ram <> DontCare
}
