// implements 128KB of on-board RAM

module ram
#(
  parameter ADDR_WIDTH = 17
)
(
  input  wire                   clk_in,   // system clock
  input wire reset,
  output reg ready,
  input  wire                   en_in,    // chip enable
  input  wire                   r_nw_in,  // read/write select (read: 1, write: 0)
  input  wire  [ADDR_WIDTH-1:0] a_in,     // memory address
  input  wire  [ 7:0]           d_in,     // data input
  output wire  [ 7:0]           d_out     // data output
);

wire       ram_bram_we;
wire [7:0] ram_bram_dout;
reg [ADDR_WIDTH-1:0] resetProgress;
wire [ADDR_WIDTH-1:0] nextProgress = resetProgress + 1;

wire [ADDR_WIDTH-1:0] addr = ready ? a_in : resetProgress;
wire [7:0] data = ready ? d_in : 8'd0;

single_port_ram_sync #(.ADDR_WIDTH(ADDR_WIDTH),
                       .DATA_WIDTH(8)) ram_bram(
  .clk(clk_in),
  .we(ram_bram_we | !ready),
  .addr_a(addr),
  .din_a(data),
  .dout_a(ram_bram_dout)
);

assign ram_bram_we = (en_in) ? ~r_nw_in      : 1'b0;
assign d_out       = (en_in) ? ram_bram_dout : 8'h00;

always @(posedge clk_in) begin
  if (reset) begin
    ready <= 0;
    resetProgress <= 0;
  end else if (!ready) begin
    resetProgress <= nextProgress;
    if (nextProgress == 0) begin
      ready <= 1;
    end
  end
end

endmodule
