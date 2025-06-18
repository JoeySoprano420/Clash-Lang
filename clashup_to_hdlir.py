def parse_clsh_to_ir(clsh_file):
    ir = []
    with open(clsh_file) as f:
        for line in f:
            if line.startswith("let"):
                parts = line.strip().split()
                ir.append(f"SET {parts[1]} {parts[3]}")
            elif "print(" in line:
                var = line.split("print(")[1].split(")")[0]
                ir.append(f"OUT {var}")
            elif "if_eq" in line:
                var, val = line.split()[1:3]
                ir.append(f"IF_EQ {var} {val}")
    with open("clashup-hdl.ir", "w") as out:
        out.write("\n".join(ir))
    print("✅ IR written to clashup-hdl.ir")
