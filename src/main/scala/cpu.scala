package org.altk.lab.cpu

import chisel3._
import chisel3.util._

class CdbMessage extends CBundle {
  val id = RobId
  val value = Word
}

class Cpu extends CModule {
  val io = IO(new CBundle {
    val ram = Flipped(new RamIo)
    val ioBufferFull = Input(Bool())
    val debugReg = Output(Word)
    val pcSel = Input(Bool())
    val pc = Output(Address)
    val full = Output(UInt(5.W))
  })
  val cdb = Wire(CdbReceiver)
  val clear = Wire(CValid(Address))
  val clearFlag = clear.valid

  // not used
  io.debugReg := 0.U

  val ramCtrl = CModule(new RamController)
  ramCtrl.io.ram          <> io.ram
  ramCtrl.io.ioBufferFull := io.ioBufferFull

  val regfile = CModule(new RegisterFile)
  regfile.io.clear := clearFlag

  val icache = CModule(new Cache()(p.icache))
  ramCtrl.io.instruction <> icache.io.ram

  val bp = CModule(new BranchPredictor)

  val ifetch = CModule(new InstructionFetchUnit)
  ifetch.io.clear    := clear
  ifetch.io.icache   <> icache.io.interface
  ifetch.io.regQuery <> regfile.io.query(p.reg.query.ifetch)
  ifetch.io.bpQuery  <> bp.io.query

  val alu = CModule(new ArithmeticLogicUnit)
  cdb(p.cdb.alu) := alu.io.resp

  val lsq = CModule(new LoadStoreQueue)
  lsq.io.clear   := clearFlag
  lsq.io.ram     <> ramCtrl.io.data
  cdb(p.cdb.lsq) := lsq.io.broadcast
  val lsqFull = lsq.io.full

  val rob = CModule(new ReorderBuffer)
  clear := rob.io.clear
  rob.io.cdb        := cdb
  rob.io.setReg     <> regfile.io.update
  rob.io.bpFeedback <> bp.io.feedback
  rob.io.lsqEnq     <> lsq.io.enq(p.lsq.enq.rob)
  val robFull = !rob.io.enq.ready

  val rs = CModule(new RsUnit)
  rs.io.cdb       := cdb
  rs.io.clear     := clearFlag
  rs.io.alu       <> alu.io.req
  rs.io.sb        <> rob.io.sbWrite
  rs.io.lb        <> lsq.io.enq(p.lsq.enq.rs)
  rs.io.robDeq    := rob.io.deqPtr
  rs.io.lbQuery.map(_ <> rob.io.lbQuery.get)
  rs.io.lbUnblock.map(_ := rob.io.lbUnblock.get)
  val rsFull = rs.io.full

  val idecode = CModule(new InstructionDecoder)
  idecode.io.input       <> ifetch.io.next
  idecode.io.rs          <> rs.io.enq
  idecode.io.rob         <> rob.io.enq
  idecode.io.cdb         := cdb
  idecode.io.robEnqPtr   <> rob.io.enqPtr
  idecode.io.robQuery    <> rob.io.decodeQuery
  idecode.io.regQuery(0) <> regfile.io.query(p.reg.query.idecode0)
  idecode.io.regQuery(1) <> regfile.io.query(p.reg.query.idecode1)
  idecode.io.regBorrow   <> regfile.io.borrow

  io.pc := Mux(io.pcSel, ifetch.io.pc, rob.io.pc)
  io.full := lsqFull ## robFull ## rsFull

  when (ready) {
    for (i <- 0 until p.cdb.lines) {
      val line = cdb(i)
      when (line.valid) {
        dprintf("cdb", p"broadcast on line $i: ${line.bits}")
      }
    }
  }
}
