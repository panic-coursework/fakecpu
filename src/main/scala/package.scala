package org.altk.lab

import chisel3._
import chisel3.util._

package object cpu {
  def Word = UInt(p.xlen.W)
  def Address = Word
  def Byte = UInt(8.W)
  def RawInstruction = Word

  def Funct7 = UInt(7.W)
  def Funct3 = UInt(3.W)
  def Opcode = UInt(7.W)
  def Register = UInt(5.W)

  def RobId = UInt(p.robWidth.W)
  def RegId = UInt(p.reg.width)

  def CdbReceiver = Vec(p.cdb.lines, CValid(new CdbMessage))
  def CdbSender = CValid(new CdbMessage)

  def BpHistory = UInt(p.bp.history.width.W)

  def lsSize (width: UInt) = MuxLookup(width(1, 0), 0.U, Seq(
    LoadStoreSize.byte -> 1.U,
    LoadStoreSize.hword -> 2.U,
    LoadStoreSize.word -> 4.U,
  ))

  def overlap (addr1: UInt, width1: UInt, addr2: UInt, width2: UInt): Bool = {
    val sameAddr = addr1 === addr2
    val overlap = Mux(
      addr1 < addr2,
      addr1 + lsSize(width1) <= addr2,
      addr2 + lsSize(width2) <= addr1,
    )
    sameAddr || overlap
  }

  def dprintf (fmt: String, data: Bits*) = {
    if (p.debug) {
      printf(fmt, data: _*)
    }
  }
  def dprintf (pable: Printable) = {
    if (p.debug) {
      printf(pable)
    }
  }
}
