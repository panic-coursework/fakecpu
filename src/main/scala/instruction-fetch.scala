package org.altk.lab.cpu

import chisel3._
import chisel3.util._

class InstructionFetchResult extends CBundle {
  val pc = Address
  val instruction = Word
  val branch = new BranchInfo
}

class InstructionFetchUnit extends CModule {
  val io = IO(new Bundle {
    val icache = new ReadOnlyCacheInterface
    val clear = Input(CValid(Address))
    val next = Decoupled(new InstructionFetchResult)

    val regQuery = Flipped(new RegQuery)
    val bpQuery = Flipped(new BpQuery)

    val pc = if (p.debug.enable) Some(Output(Address)) else None
  })
  val pc = RegInit(Address, p.initialPc)
  io.pc.map(_ := pc)

  when (ready) {
    io.icache.address.bits := pc
    io.icache.address.valid := !io.clear.valid
    io.next.valid := io.icache.address.fire
    io.next.bits.pc := pc
    val ins = io.icache.dataRead
    io.next.bits.instruction := ins

    val opcode = ins(6, 0)
    val rs1 = ins(19, 15)
    val JALR = "b1100111".U
    val JAL = "b1101111".U
    val BRANCH = "b1100011".U
    val immI = ins(31, 20).asSInt pad p.xlen
    val immB = (ins(31) ## ins(7) ## ins(30, 25) ## ins(11, 8) ## 0.U(1.W)).asSInt pad p.xlen
    val immJ = (ins(31) ## ins(19, 12) ## ins(20) ## ins(30, 21) ## 0.U(1.W)).asSInt pad p.xlen

    io.regQuery.req := rs1
    io.bpQuery.pc := pc
    val predictedAddr = alignPc(io.regQuery.resp.value + immI.asUInt)
    val take = io.bpQuery.take
    val takePc = alignPc(immB.asUInt + pc)
    val nextPc = pc + 4.U
    val next = MuxCase(nextPc, Seq(
      (opcode === BRANCH && take) -> takePc,
      (opcode === JAL) -> alignPc(pc + immJ.asUInt),
      (opcode === JALR) -> predictedAddr,
    ))
    val fallback = Mux(take, nextPc, takePc)

    val branch = io.next.bits.branch
    branch.predictedAddr := predictedAddr
    branch.predictedTake := take
    branch.history       := io.bpQuery.history
    branch.fallback      := fallback

    when ((io.next.ready && io.icache.address.fire) || io.clear.valid) {
      dprintf("ifetch", "pc: %x", pc);
      dprintf("ifetch", p"instruction: $ins, next pc: $next, clear? ${io.clear.valid} (${io.clear.bits})")
      when (opcode === BRANCH) {
        dprintf("ifetch", p"take? $take, immB $immB, takePc $takePc")
      }
      pc := Mux(io.clear.valid, io.clear.bits, next)
    }
  }.otherwise {
    io <> DontCare
  }
}
