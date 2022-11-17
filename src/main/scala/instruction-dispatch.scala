package org.altk.lab.cpu

import chisel3._
import chisel3.util._

class InstructionDispatcher extends CModule {
  val io = IO(new Bundle {
    val instruction = Input(Valid(Instruction))
  })
}
