package org.altk.lab.cpu

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import chisel3.util._

class ReadOnlyCacheInterface extends CBundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val address = Output(Address)
  val dataRead = Input(Word)
}

class ReadWriteCacheInterface extends ReadOnlyCacheInterface {
  val dataWrite = Output(Word)
  val writeEnable = Output(Bool())
}

case class CacheParameters(
  val ways: Int,
  val indexBits: Int,
  val blockBits: Int,
  val readOnly: Boolean,
) {
  def tagBits = p.xlen - indexBits - blockBits
  def nBytesInLine = 1 << blockBits
  def nLines = 1 << indexBits
}

class CacheLine(implicit cp: CacheParameters) extends CBundle {
  val valid = Bool()
  val tag = UInt(cp.tagBits.W)
  val data = Vec(cp.nBytesInLine, Byte)
}

class CacheAddress(implicit cp: CacheParameters) extends CBundle {
  val tag = UInt(cp.tagBits.W)
  val index = UInt(cp.indexBits.W)
  val offset = UInt(cp.blockBits.W)
}

class Cache(implicit cp: CacheParameters) extends CModule {
  val io = IO(new Bundle {
    val interface = Flipped(if (cp.readOnly) {
      new ReadOnlyCacheInterface
    } else {
      new ReadWriteCacheInterface
    })
    val ram = new RamInterface
  })
  if (cp.ways != 1) {
    throw new Exception("Non-direct-mapped caches are currently not supported")
  }
  if (!cp.readOnly) {
    throw new Exception("TODO")
  }

  val sets = Mem(cp.nLines, new CacheLine)

  val fetchProgress = RegInit(0.U(cp.blockBits.W))
  val lastProgress = CRegNext(fetchProgress)

  val addr = io.interface.address.asTypeOf(new CacheAddress)
  val lastIndex = CRegNext(addr.index)
  val lastOffset = (0 until p.wordSize).map { (x: Int) => CRegNext(addr.offset + x.U) }.reverse
  val lastLineRead = sets.read(lastIndex)
  io.interface.dataRead := Cat(lastOffset.map { lastLineRead.data(_) })

  val currentLineRead = sets.read(addr.index)
  io.interface.ready := currentLineRead.valid && currentLineRead.tag === addr.tag

  val lastRamReady = CRegNext(io.ram.req.valid && io.ram.req.ready)

  when (ready) {
    when (io.interface.valid && !io.interface.ready) {
      // cache miss
      val readAddr = Wire(new CacheAddress)
      readAddr.tag := addr.tag
      readAddr.index := addr.index
      readAddr.offset := fetchProgress
      val req = Wire(new RamRequest)
      req.read(readAddr.asUInt)
      io.ram.req.enq(req)

      when (io.ram.req.ready) {
        when (fetchProgress === (cp.nBytesInLine - 1).U) {
          // TODO: we could save a cycle here
          val currentLine = sets(addr.index)
          currentLine.valid := true.B
          currentLine.tag := addr.tag
          fetchProgress := 0.U
        }.otherwise {
          fetchProgress := fetchProgress + 1.U
        }
      }
    }.otherwise {
      io.ram.req.noenq()
    }

    when (lastRamReady) {
      printf(p"lastRamReady: $lastRamReady, prog: $lastProgress, resp: ${io.ram.resp}\n")
      sets(lastIndex).data(lastProgress) := io.ram.resp
    }
  }.otherwise {
    io <> DontCare
  }
}
