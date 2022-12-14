`include "TestModule.v"

module testbench;

reg clk;
reg rst;
reg rdy;
wire hlt;

TestModule top (
  .clock(clk),
  .reset(rst),
  .ready(rdy),
  .io_halt(hlt)
);

initial begin
  $dumpfile("test.vcd");
  $dumpvars;
  clk = 0;
  rst = 1;
  repeat(3) #1 clk = !clk;
  rst = 0;
  rdy = 1;
  forever #1 clk = !clk;
  // repeat(10000000) #1 clk = !clk;

  $finish;
end

always @(posedge clk) begin
  if (hlt) $finish;
end

endmodule
