package chisel3

import chisel3.internal.Builder

object CurrentModule {
  def apply() = Builder.currentModule
}
