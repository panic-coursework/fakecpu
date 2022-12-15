package org.altk.lab.cpu

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import chisel3.util._

class ReadOnlyCacheInterface extends CBundle {
  val address = Decoupled(Address)
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
}

class CacheAddress(implicit cp: CacheParameters) extends CBundle {
  val tag = UInt(cp.tagBits.W)
  val index = UInt(cp.indexBits.W)
  val offset = UInt(cp.blockBits.W)
}

class Cache(implicit cp: CacheParameters) extends CModule {
  val io = IO(new CBundle {
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
  if (cp.nBytesInLine < p.wordSize) {
    throw new Exception("Block size too small")
  }

  val sets = Mem(cp.nLines, new CacheLine)
  val data = Mem(cp.nLines * cp.nBytesInLine, Byte)

  if (p.initCache) {
    when (reset.asBool) {
      for (i <- 0 until cp.nLines) {
        sets(i).valid := false.B
      }
    }
  }

  val idle :: fetching :: Nil = Enum(2)
  val state = RegInit(idle)

  val fetchProgress = RegInit(0.U(cp.blockBits.W))
  val lastProgress = CRegNext(fetchProgress)

  val addr = io.interface.address.bits.asTypeOf(new CacheAddress)
  val lastAddr = RegInit(Address, 0.U(p.xlen.W))
  when (ready && io.interface.address.valid) {
    lastAddr := io.interface.address.bits
  }
  val offset = (0 until p.wordSize).map { (x: Int) => addr.index ## (addr.offset + x.U) }.reverse
  io.interface.dataRead := Cat(offset.map { data.read(_) })

  val currentLine = sets(addr.index)
  val cacheHit = currentLine.valid && currentLine.tag === addr.tag
  io.interface.address.ready := cacheHit

  val clearProgress = state === idle || io.interface.address.bits =/= lastAddr

  when (ready) {
    when (io.interface.address.valid && !cacheHit) {
      // cache miss
      when (clearProgress) {
        currentLine.valid := false.B
        currentLine.tag := addr.tag
        state := fetching
        fetchProgress := 0.U
      }

      val readAddr = Wire(new CacheAddress)
      readAddr.tag := addr.tag
      readAddr.index := addr.index
      val readOffset = Mux(clearProgress, 0.U, fetchProgress)
      lastProgress := readOffset
      readAddr.offset := readOffset
      val req = Wire(new RamRequest)
      req.read(readAddr.asUInt)
      io.ram.req.enq(req)

      when (io.ram.req.ready) {
        when (lastProgress === (cp.nBytesInLine - 1).U && !clearProgress) {
          currentLine.valid := true.B
        }.otherwise {
          fetchProgress := readOffset + 1.U
        }
      }
    }.otherwise {
      io.ram.req.noenq()
    }

    when (CRegNextInit(io.ram.req.fire, false.B)) {
      val lastIndex = CRegNext(addr.index)
      dprintf("cache", p"index: $lastIndex, progress: $lastProgress, resp: ${io.ram.resp}")
      data.write(lastIndex ## lastProgress, io.ram.resp)
    }
  }.otherwise {
    io <> DontCare
  }
}
