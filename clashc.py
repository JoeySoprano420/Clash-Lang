import sys

def parse_clsh(filename):
    asm_lines = []

    with open(filename, 'r') as f:
        lines = f.readlines()

    for line in lines:
        line = line.strip()
        if line.startswith("print "):
            msg = line[6:].strip().strip('"')
            asm_lines.append(f'    mov rax, 1')
            asm_lines.append(f'    mov rdi, 1')
            asm_lines.append(f'    mov rsi, msg_{len(asm_lines)}')
            asm_lines.append(f'    mov rdx, {len(msg)}')
            asm_lines.append(f'    syscall\n')
            asm_lines.append(f'msg_{len(asm_lines)}: db "{msg}", 10')
        elif line == "exit":
            asm_lines.append("    mov rax, 60")
            asm_lines.append("    xor rdi, rdi")
            asm_lines.append("    syscall\n")
        elif line.endswith(":"):
            label = line[:-1]
            asm_lines.append(f"{label}:")
    return asm_lines

def write_output(asm_lines, output_file):
    with open(output_file, 'w') as f:
        f.write("section .text\n")
        f.write("global _start\n")
        f.write("_start:\n")
        for line in asm_lines:
            f.write(line + "\n")

def main():
    if len(sys.argv) < 2:
        print("Usage: python clashc.py input.clsh [output.asm]")
        return

    input_file = sys.argv[1]
    output_file = sys.argv[2] if len(sys.argv) > 2 else "output.asm"

    asm_lines = parse_clsh(input_file)
    write_output(asm_lines, output_file)
    print(f"✔ ASM written to {output_file}")

if __name__ == "__main__":
    main()
