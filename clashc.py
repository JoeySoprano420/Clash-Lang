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

import sys
import re

class ClashCompiler:
    def __init__(self):
        self.output = []
        self.variables = {}
        self.data_section = []
        self.bss_section = []
        self.labels = []
        self.funcs = {}
        self.func_stack = []
        self.next_id = 0

    def label(self):
        self.next_id += 1
        return f".L{self.next_id}"

    def add_var(self, name):
        if name not in self.variables:
            self.variables[name] = name
            self.bss_section.append(f"{name}: resq 1")

    def emit(self, line):
        self.output.append("    " + line)

    def parse_line(self, line):
        if line.endswith(":"):
            self.output.append(line)
        elif line.startswith("print "):
            val = line[6:].strip()
            if val.isdigit():
                self.emit(f"mov rdi, {val}")
            else:
                self.emit(f"mov rdi, [{val}]")
            self.emit("call print_int")
        elif "let" in line:
            m = re.match(r'let (\w+) = (.+)', line)
            if m:
                var, expr = m.group(1), m.group(2)
                self.add_var(var)
                expr = expr.replace("+", " + ").replace("-", " - ")
                tokens = expr.split()
                if len(tokens) == 3:
                    a, op, b = tokens
                    if a.isdigit(): self.emit(f"mov rax, {a}")
                    else: self.emit(f"mov rax, [{a}]")
                    if b.isdigit(): self.emit(f"{'add' if op == '+' else 'sub'} rax, {b}")
                    else: self.emit(f"{'add' if op == '+' else 'sub'} rax, [{b}]")
                    self.emit(f"mov [{var}], rax")
                else:
                    self.emit(f"mov rax, {expr}" if expr.isdigit() else f"mov rax, [{expr}]")
                    self.emit(f"mov [{var}], rax")
        elif line.startswith("if_eq"):
            m = re.match(r'if_eq (\w+) (\d+) then (.+)', line)
            if m:
                var, val, then = m.group(1), m.group(2), m.group(3)
                label = self.label()
                self.emit(f"mov rax, [{var}]")
                self.emit(f"cmp rax, {val}")
                self.emit(f"jne {label}")
                self.parse_line(then)
                self.output.append(label + ":")
        elif line == "exit":
            self.emit("mov rax, 60")
            self.emit("xor rdi, rdi")
            self.emit("syscall")
        elif line.startswith("goto "):
            self.emit(f"jmp {line[5:].strip()}")
        elif line.startswith("func"):
            m = re.match(r'func (\w+)\((.+)\) {', line)
            if m:
                fname, args = m.group(1), m.group(2)
                self.output.append(f"{fname}:")
                self.emit("push rbp")
                self.emit("mov rbp, rsp")
                self.func_stack.append(fname)
        elif line.strip() == "}":
            self.emit("pop rbp")
            self.emit("ret")
            self.func_stack.pop()
        elif line.startswith("return "):
            val = line[7:].strip()
            self.emit(f"mov rax, {val}" if val.isdigit() else f"mov rax, [{val}]")
            self.emit("ret")
        elif re.match(r'\w+\(\d+\)', line):
            m = re.match(r'(\w+)\((\d+)\)', line)
            fname, arg = m.group(1), m.group(2)
            self.emit(f"mov rdi, {arg}")
            self.emit(f"call {fname}")
            self.emit(f"call print_int")

    def write_output(self, filename):
        with open(filename, 'w') as f:
            f.write("section .text\n")
            f.write("global _start\n\n")
            f.write("_start:\n")
            for line in self.output:
                f.write(line + "\n")
            f.write("\n; runtime\n")
            f.write("print_int:\n")
            f.write("    mov rsi, rsp\n")
            f.write("    sub rsp, 32\n")
            f.write("    mov rcx, 10\n")
            f.write("    mov rbx, rsp\n")
            f.write("    mov rdx, 0\n")
            f.write("._print_loop:\n")
            f.write("    xor rdx, rdx\n")
            f.write("    div rcx\n")
            f.write("    add rdx, '0'\n")
            f.write("    dec rbx\n")
            f.write("    mov [rbx], dl\n")
            f.write("    test rax, rax\n")
            f.write("    jnz ._print_loop\n")
            f.write("    mov rax, 1\n")
            f.write("    mov rdi, 1\n")
            f.write("    mov rsi, rbx\n")
            f.write("    mov rdx, rsp\n")
            f.write("    sub rdx, rbx\n")
            f.write("    syscall\n")
            f.write("    mov rsp, rsi\n")
            f.write("    ret\n\n")

            if self.bss_section:
                f.write("section .bss\n")
                for line in self.bss_section:
                    f.write(line + "\n")

    def compile(self, source):
        with open(source, 'r') as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith("//"):
                    self.parse_line(line)

if __name__ == "__main__":
    src = sys.argv[1] if len(sys.argv) > 1 else "input.clsh"
    out = sys.argv[2] if len(sys.argv) > 2 else "output.asm"
    c = ClashCompiler()
    c.compile(src)
    c.write_output(out)
    print(f"✅ Compiled {src} → {out}")
