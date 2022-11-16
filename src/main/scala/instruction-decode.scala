package org.altk.lab.cpu

import chisel3._
import chisel3.util.Valid

class InstructionDecoder extends CModule {
  val io = IO(new Bundle {
    val instruction = Input(Valid(RawInstructionWithPc))
  })
}
