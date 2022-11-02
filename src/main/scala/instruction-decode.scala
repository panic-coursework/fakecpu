package org.altk.lab.cpu

import chisel3._

class InstructionDecoder extends CModule {
  val io = IO(new Bundle {
    val instruction = RawInstruction
  })
}
