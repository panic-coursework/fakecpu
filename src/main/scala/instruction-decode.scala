package org.altk.lab.cpu

import chisel3._
import chisel3.util._

import org.altk.lab.cpu.InstructionType._

class InstructionDecoder extends CModule {
  val io = IO(new CBundle {
    val input = Flipped(Decoupled(new InstructionFetchResult))
    val rs = Decoupled(new RsUnitEnqueue)
    val rob = Decoupled(new RobEnqueue)

    val cdb = Input(CdbReceiver)
    val robEnqPtr = Input(UInt(p.robWidth.W))
    val regQuery = Vec(2, Flipped(new RegQuery))
    val robQuery = Vec(2, Flipped(new RobQuery))
    val regBorrow = Output(CValid(new RegBorrow))
  })

  val ins = io.input.bits.instruction
  val rs = OutputReg(io.rs.bits)
  val rsValid = OutputReg(io.rs.valid)
  val rob = OutputReg(io.rob.bits)
  val robValid = OutputReg(io.rob.valid)
  val robId = Mux(io.rob.fire, io.robEnqPtr + 1.U, io.robEnqPtr)

  when (ready) {
    io.input.ready := io.rs.ready && io.rob.ready

    io.regBorrow.valid := false.B
    io.regBorrow.bits := DontCare

    // TODO: cond?
    val fire = io.input.fire
    rsValid := fire
    robValid := fire
    when (fire) {
      val opcode = ins(6, 0)
      val rd = ins(11, 7)
      val funct7 = ins(31, 25)
      val funct3 = ins(14, 12)
      val rs1 = ins(19, 15)
      val rs2 = ins(24, 20)

      val immI = ins(31, 20).asSInt pad p.xlen
      val immS = (ins(31, 25) ## ins(11, 7)).asSInt pad p.xlen
      val immB = (ins(31) ## ins(7) ## ins(30, 25) ## ins(11, 8) ## 0.U(1.W)).asSInt pad p.xlen
      val immU = (ins(31, 12) ## 0.U(12.W)).asSInt pad p.xlen
      val immJ = (ins(31) ## ins(19, 12) ## ins(20) ## ins(30, 21) ## 0.U(1.W)).asSInt pad p.xlen

      val OP_IMM   = "b0010011".U // I
      val LUI      = "b0110111".U // U
      val AUIPC    = "b0010111".U // U
      val OP       = "b0110011".U // R
      val JAL      = "b1101111".U // J
      val JALR     = "b1100111".U // I
      val BRANCH   = "b1100011".U // B
      val LOAD     = "b0000011".U // I
      val STORE    = "b0100011".U // S
      val MISC_MEM = "b0001111".U
      val SYSTEM   = "b1110011".U // I

      val destValid = Wire(Bool())
      val robOp = Wire(RobOp.T)

      rs.op := Mux(
        opcode === OP_IMM && funct3 =/= "b001".U && funct3 =/= "b101".U,
        0.U(2.W),
        funct7(6, 5),
      ) ## funct3
      rs.size   := funct3
      rs.dest   := robId
      rs.offset := DontCare
      rob.op          := robOp
      rob.value.valid := false.B
      rob.dest.bits   := rd
      rob.dest.valid  := destValid
      rob.size        := funct3
      rob.branch      := io.input.bits.branch

      def query (src: UInt, reg: RegQuery, rob: RobQuery, dest: Register): Unit = {
        reg.req := src
        val robId = reg.resp.src
        dest.src := robId
        rob.req := robId
        dest.valid := reg.resp.valid || rob.resp.valid || (0 until p.cdb.lines).map { i =>
          val line = io.cdb(i)
          line.valid && line.bits.id === robId
        }.reduceLeft(_ || _)
        dest.value := MuxCase(0.U, Seq(
          reg.resp.valid -> reg.resp.value,
          rob.resp.valid -> rob.resp.bits,
        ) ++ (0 until p.cdb.lines).map { i =>
          val line = io.cdb(i)
          (line.valid && line.bits.id === robId) -> line.bits.value
        })
      }
      query(rs1, io.regQuery(0), io.robQuery(0), rs.value1)
      query(rs2, io.regQuery(1), io.robQuery(1), rs.value2)

      val pc = io.input.bits.pc.asSInt
      when (opcode === OP_IMM) {
        robOp := RobOp.reg
        destValid := true.B

        rs.itype := arithmetic
        rs.value2.valid := true.B
        rs.value2.value := immI.asUInt
      }.elsewhen (opcode === LUI || opcode === AUIPC || opcode === JAL) {
        rsValid := false.B

        robOp := RobOp.reg
        rob.value.valid := true.B
        rob.value.bits := MuxLookup(opcode, 0.S, Seq(
          LUI -> immU,
          AUIPC -> (immU + pc),
          JAL -> (pc + 4.S), // jump is handled pre-decode
        )).asUInt

        destValid := true.B
      }.elsewhen (opcode === OP) {
        robOp := RobOp.reg
        rs.itype := arithmetic
        destValid := true.B
      }.elsewhen (opcode === JALR) {
        robOp := RobOp.jalr
        rob.value.valid := true.B
        rob.value.bits := (pc + 4.S).asUInt

        rs.itype := arithmetic
        rs.op := AluOp.add
        rs.value2.valid := true.B
        rs.value2.value := immI.asUInt
        destValid := true.B
      }.elsewhen (opcode === BRANCH) {
        robOp := RobOp.branch
        rs.itype := branch
        destValid := false.B
      }.elsewhen (opcode === LOAD) {
        robOp := RobOp.reg
        rs.itype := load
        rs.offset := immI.asUInt
        destValid := true.B
      }.elsewhen (opcode === STORE) {
        robOp := RobOp.store
        rs.itype := store
        rs.offset := immS.asUInt
        destValid := false.B
      }.elsewhen (opcode === MISC_MEM) {
        robOp := RobOp.unimp
        rs.itype := unimp
        destValid := false.B
      }.elsewhen (opcode === SYSTEM) {
        robOp := RobOp.unimp
        rs.itype := unimp
        destValid := false.B
      }.otherwise {
        robOp := RobOp.unimp
        rs.itype := unimp
        destValid := false.B
      }

      when (destValid) {
        io.regBorrow.valid := true.B
        io.regBorrow.bits.src := robId
        io.regBorrow.bits.id := rd
      }
    }.otherwise {
      io.regQuery <> DontCare
      io.robQuery <> DontCare
    }
  }.otherwise {
    io <> DontCare
  }
}
