module clashup_cpu (
    input wire clk,
    input wire rst,
    output reg [7:0] out
);
    reg [7:0] pc = 8'h00;
    reg [7:0] regs[0:7];
    reg [7:0] ram[0:255];
    reg [7:0] instr;

    always @(posedge clk or posedge rst) begin
        if (rst) begin
            pc <= 8'h00;
        end else begin
            instr <= ram[pc];
            case (instr)
                8'h01: begin // SET reg, imm
                    pc <= pc + 1;
                    reg [2:0] r = ram[pc][2:0];
                    pc <= pc + 1;
                    regs[r] <= ram[pc];
                    pc <= pc + 1;
                end
                8'h02: begin // ADD r1, r2
                    pc <= pc + 1;
                    reg [2:0] r1 = ram[pc][2:0];
                    pc <= pc + 1;
                    reg [2:0] r2 = ram[pc][2:0];
                    regs[r1] <= regs[r1] + regs[r2];
                    pc <= pc + 1;
                end
                8'h03: begin // OUT r
                    pc <= pc + 1;
                    reg [2:0] r = ram[pc][2:0];
                    out <= regs[r];
                    pc <= pc + 1;
                end
                default: begin
                    pc <= pc + 1;
                end
            endcase
        end
    end
endmodule
