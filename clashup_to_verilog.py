def ir_to_verilog(ir_file="clashup-hdl.ir", out_file="clashup_core.v"):
    regs = set()
    with open(ir_file) as f:
        lines = f.readlines()

    with open(out_file, 'w') as v:
        v.write("module clashup_core(input clk, output reg [7:0] out);\n")
        for i in range(8):
            v.write(f"  reg [7:0] r{i};\n")
        v.write("  always @(posedge clk) begin\n")

        for line in lines:
            parts = line.strip().split()
            if parts[0] == "SET":
                v.write(f"    {parts[1]} <= 8'd{parts[2]};\n")
            elif parts[0] == "OUT":
                v.write(f"    out <= {parts[1]};\n")
            elif parts[0] == "IF_EQ":
                v.write(f"    if ({parts[1]} == 8'd{parts[2]}) begin\n")
                v.write(f"      // TODO: branch logic\n")
                v.write("    end\n")
        v.write("  end\nendmodule\n")
    print("✅ Verilog written to clashup_core.v")
