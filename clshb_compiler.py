def compile_clshb(filename="input.clsh"):
    out = []
    with open(filename) as f:
        for line in f:
            if line.startswith("let"):
                parts = line.strip().split()
                reg = int(parts[1][1])
                val = int(parts[3])
                out += [0x01, reg, val]
            elif "add" in line:
                parts = line.strip().split()
                r1 = int(parts[1][1])
                r2 = int(parts[2][1])
                out += [0x02, r1, r2]
            elif "print" in line:
                r = int(line.split("(")[1].split(")")[0][1])
                out += [0x03, r]
    with open("program.clshb", "wb") as f:
        f.write(bytearray(out))
    print("✅ Bytecode written to program.clshb")
