package org.altk.lab.cpu

import scala.language.experimental.macros

import chisel3._
import chisel3.internal.sourceinfo.{InstTransform, SourceInfo, UnlocatableSourceInfo}

object CModule {
  def apply[T <: CModule](bc: => T): T = macro InstTransform.apply[T]
  def do_apply[T <: CModule](bc: => T)(implicit sourceInfo: SourceInfo, compileOptions: CompileOptions): T = {
    val module = Module.do_apply(bc)(sourceInfo, compileOptions)
    module.ready <> CurrentModule().get.asInstanceOf[CModule].ready
    module
  }
}

class CModule extends Module {
  val ready = IO(Input(Bool()))
}

class CBundle extends Bundle
