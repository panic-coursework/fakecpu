package org.altk.lab.cpu

import chisel3._

object p {
  val isFirrtlBuggy = true
  val mode = "codegen"
  object codegen {
    def module = new Cpu
    val options = Array("--emission-options=disableMemRandomization,disableRegisterRandomization")
  }
  object debug {
    val enable = false
    val units = Map(
      "alu"    -> true,
      "cache"  -> true,
      "cdb"    -> true,
      "clock"  -> true,
      "commit" -> true,
      "ifetch" -> true,
      "iommu"  -> true,
      "lb"     -> false,
      "lsq"    -> true,
      "rob"    -> true,
      "rs"     -> true,
    )
  }

  val wordSize = 4
  val xlen = 32
  val initialPc = 0.U(xlen.W)

  object reg {
    val count = 32
    val width = 5.W
    object query {
      val lines = 3
      val idecode0 = 0
      val idecode1 = 1
      val ifetch = 2
    }
  }

  val icache = new CacheParameters(1, 5, 5, true)

  val robWidth = 5
  val robLines = 1 << robWidth
  val rsWidth = 4
  val rsLines = 1 << rsWidth
  val loadBufferWidth = 4
  val loadBufferLines = 1 << loadBufferWidth
  val storeBufferWidth = 4
  val storeBufferLines = 1 << storeBufferWidth

  object cdb {
    val lines = 2
    val alu = 0
    val lsq = 1
  }
  val robQueryLines = 2

  object lsq {
    val width = 4
    val lines = 1 << width
    object enq {
      val lines = 2
      val rob = 0
      val rs = 1
    }
  }

  // branch predictor
  object bp {
    object history {
      val width = 4
      val lines = 1 << width
    }
    object pc {
      val width = 6
      val skip = 2
      val lines = 1 << width
    }
    object v {
      val bits = 2
      val threshold = 2
      val max = (1 << bits) - 1
    }
  }

  object iommu {
    val base = 0x30000
  }
}
