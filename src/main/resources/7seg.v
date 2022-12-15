module seg (
  input wire clock,
  input wire reset,
  input wire [15:0] pc,
  output reg [6:0] seg,
  output reg dp,
  output reg [3:0] an
);
  reg [3:0] digit;
  reg [19:0] counter;
  wire [1:0] sw;
  assign sw = counter[19:18];

  always @(posedge clock or posedge reset) begin
    counter <= reset ? 0 : counter + 1;
  end

  always @(posedge clock) begin
    case (sw)
      2'b00: begin
        an <= 4'b0111;
        digit = pc[15:12];
      end
      2'b01: begin
        an <= 4'b1011;
        digit = pc[11:8];
      end
      2'b10: begin
        an <= 4'b1101;
        digit = pc[7:4];
      end
      2'b11: begin
        an <= 4'b1110;
        digit = pc[3:0];
      end
    endcase

    case (digit)
      4'b0000: seg <= 7'b1000000;
      4'b0001: seg <= 7'b1111001;
      4'b0010: seg <= 7'b0100100;
      4'b0011: seg <= 7'b0110000;
      4'b0100: seg <= 7'b0011001;
      4'b0101: seg <= 7'b0010010;
      4'b0110: seg <= 7'b0000010;
      4'b0111: seg <= 7'b1111000;
      4'b1000: seg <= 7'b0000000;
      4'b1001: seg <= 7'b0010000;
      4'b1010: seg <= 7'b0001000;
      4'b1011: seg <= 7'b0000011;
      4'b1100: seg <= 7'b1000110;
      4'b1101: seg <= 7'b0100001;
      4'b1110: seg <= 7'b0000110;
      4'b1111: seg <= 7'b0001110;
    endcase

    dp <= 1'b1;
  end
endmodule
