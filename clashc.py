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

from clashc.lexer import tokenize
from clashc.parser import parse
from clashc.codegen import CodeGenerator
from clashc.optimizer import optimize

import sys

if __name__ == "__main__":
    src = sys.argv[1] if len(sys.argv) > 1 else "input.clsh"
    tokens = tokenize(open(src).read())
    ast = parse(tokens)
    optimize(ast)
    gen = CodeGenerator()
    asm = gen.generate(ast)
    with open("output.asm", "w") as f:
        f.write(asm)
    print("✅ Compilation complete: output.asm")

import os
import subprocess

def main():
    print("🔥 Welcome to Clashup Launcher")
    print("1. Compile input.clsh")
    print("2. Run binary")
    print("3. Edit source")
    print("4. Exit")

    choice = input("Select: ")
    if choice == "1":
        os.system("python3 clashc.py input.clsh")
        os.system("nasm -f elf64 output.asm -o output.o")
        os.system("ld output.o -o output")
        print("✅ Compiled")
    elif choice == "2":
        os.system("./output")
    elif choice == "3":
        os.system("nano input.clsh")
    else:
        exit()

if __name__ == "__main__":
    while True:
        main()

def optimize(ast):
    for node in ast:
        if node.type == 'assign' and node.right.type == 'const' and node.right.value == 0:
            node.right = {'type': 'const', 'value': 0}  # constant folding example

def tokenize(src):
    import re
    tokens = []
    lines = src.split('\n')
    for line in lines:
        line = line.strip()
        if line and not line.startswith("//"):
            tokens.append(line)
    return tokens

from .ast import *

def parse(tokens):
    ast = []
    for line in tokens:
        if line.startswith("print "):
            ast.append(PrintNode(line[6:].strip()))
        elif "let" in line:
            parts = line[4:].split("=")
            var = parts[0].strip()
            val = parts[1].strip()
            ast.append(AssignNode(var, val))
        elif line.startswith("func"):
            fn = line[5:].split("(")[0].strip()
            ast.append(FuncNode(fn))
        elif line.startswith("while "):
            cond = line[6:].strip()
            ast.append(WhileNode(cond))
        else:
            ast.append(RawNode(line))
    return ast

class Node: pass

class PrintNode(Node):
    def __init__(self, value): self.value = value

class AssignNode(Node):
    def __init__(self, var, value): self.var = var; self.value = value

class FuncNode(Node):
    def __init__(self, name): self.name = name

class WhileNode(Node):
    def __init__(self, condition): self.condition = condition

class RawNode(Node):
    def __init__(self, text): self.text = text

def generate_header():
    return [
        "section .text",
        "global _start",
        "_start:"
    ]

def generate_footer():
    return [
        "mov rax, 60", "xor rdi, rdi", "syscall"
    ]

def CodeGenerator():
    class Generator:
        def generate(self, ast):
            lines = generate_header()
            for node in ast:
                if isinstance(node, PrintNode):
                    lines += [
                        f"mov rdi, {node.value}",
                        "call print_int"
                    ]
                elif isinstance(node, AssignNode):
                    lines += [
                        f"mov rax, {node.value}",
                        f"mov [{node.var}], rax"
                    ]
                elif isinstance(node, RawNode):
                    lines.append(node.text)
            lines += generate_footer()
            return "\n".join(lines)
    return Generator()

import logging
from typing import List, Callable, Type
from dataclasses import dataclass

from dataclasses import dataclass, field
from typing import Any, List, Optional, Union

@dataclass
class ASTNode:
    """Base class for all Abstract Syntax Tree nodes."""
    node_type: str
    children: List['ASTNode'] = field(default_factory=list)
    value: Optional[Union[str, int, float, bool]] = None
    line: Optional[int] = None
    column: Optional[int] = None

    def add_child(self, child: 'ASTNode') -> None:
        self.children.append(child)

    def __str__(self) -> str:
        val = f", value={self.value}" if self.value is not None else ""
        pos = f", at line {self.line}, col {self.column}" if self.line is not None else ""
        return f"{self.node_type}({len(self.children)} children{val}{pos})"


# Specific AST node type for assignments.
@dataclass
class AssignNode(ASTNode):
    variable: str
    value: str

    def __repr__(self) -> str:
        return f"AssignNode(variable={self.variable!r}, value={self.value!r})"

def optimize_assign_node(node: AssignNode) -> None:
    """
    Optimize an AssignNode by applying constant folding.

    Currently, if the assignment's value is the literal "0", a constant fold is performed,
    though this example is a placeholder and can be extended for other cases.
    
    Parameters:
        node (AssignNode): The assignment node to optimize.
    """
    if node.value == "0":
        original_value = node.value
        node.value = "0"  # This example folds the constant 0.
        logging.debug(f"[OPTIMIZE] Constant folded: {original_value} to {node.value} for variable {node.variable}")

def optimize(ast: List[ASTNode]) -> None:
    """
    Optimize a list of AST nodes in place.

    The optimizer dispatches nodes to type-specific optimization routines via a dispatch map.
    This design allows adding new optimizations easily while keeping the core processing loop simple,
    which is essential for scaling up to large and complex ASTs.
    
    Parameters:
        ast (List[ASTNode]): The abstract syntax tree (AST) represented as a list of nodes.
    """
    # Map specific AST node types to their optimizer functions.
    optimizers: dict[Type[ASTNode], Callable[[ASTNode], None]] = {
        AssignNode: optimize_assign_node,
    }

    for node in ast:
        optimizer = optimizers.get(type(node))
        if optimizer:
            try:
                optimizer(node)
            except Exception as error:
                logging.error(f"Error optimizing node {node}: {error}")

if __name__ == "__main__":
    # Configure logging for development. In production, logging
    # may be configured differently or through a configuration file.
    logging.basicConfig(level=logging.DEBUG)
    
    # Example usage with a sample AST.
    ast_example: List[ASTNode] = [
        AssignNode("x", "0"),
        AssignNode("y", "1"),
    ]
    
    logging.info("Before optimization:")
    for node in ast_example:
        logging.info(node)
    
    optimize(ast_example)
    
    logging.info("After optimization:")
    for node in ast_example:
        logging.info(node)

import os
while True:
    print("\n🎮 Clashup GUI Launcher")
    print("1. Edit Source")
    print("2. Compile")
    print("3. Run")
    print("4. Exit")
    c = input("Choose> ")
    if c == "1": os.system("nano input.clsh")
    elif c == "2": os.system("python3 clashc.py input.clsh")
    elif c == "3": os.system("./output")
    elif c == "4": break

from clashc.lexer import tokenize
from clashc.parser import parse
from clashc.codegen import CodeGenerator
from clashc.optimizer import optimize

import sys

if __name__ == "__main__":
    src = sys.argv[1] if len(sys.argv) > 1 else "input.clsh"
    tokens = tokenize(open(src).read())
    ast = parse(tokens)
    optimize(ast)
    gen = CodeGenerator()
    asm = gen.generate(ast)
    with open("output.asm", "w") as f:
        f.write(asm)
    print("✅ output.asm ready")

class ClashVM:
    def __init__(self):
        self.registers = {'ACC': 0}
        self.memory = [0] * 1024
        self.pc = 0
        self.instructions = []

    def load(self, bytecode):
        self.instructions = bytecode

    def run(self):
        while self.pc < len(self.instructions):
            inst = self.instructions[self.pc]
            if inst[0] == "LOAD":
                self.registers['ACC'] = inst[1]
            elif inst[0] == "ADD":
                self.registers['ACC'] += inst[1]
            elif inst[0] == "PRINT":
                print(self.registers['ACC'])
            elif inst[0] == "JMP":
                self.pc = inst[1]
                continue
            elif inst[0] == "HLT":
                break
            self.pc += 1

def emit_wasm(ast):
    lines = ["(module"]
    lines.append("  (import \"env\" \"print\" (func $print (param i32)))")
    lines.append("  (func $main")
    for node in ast:
        if isinstance(node, PrintNode):
            lines.append(f"    i32.const {node.value}")
            lines.append(f"    call $print")
    lines.append("  )")
    lines.append("  (start $main)")
    lines.append(")")
    return "\n".join(lines)

def interpret(ast):
    env = {}
    for node in ast:
        if isinstance(node, AssignNode):
            env[node.var] = int(node.value)
        elif isinstance(node, PrintNode):
            val = env.get(node.value, node.value)
            print(val)

import pygame

def game_loop():
    pygame.init()
    screen = pygame.display.set_mode((640, 480))
    pygame.display.set_caption("Clashup Game Engine")
    clock = pygame.time.Clock()

    running = True
    while running:
        screen.fill((30, 30, 30))
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        pygame.draw.circle(screen, (255, 0, 0), (320, 240), 50)
        pygame.display.flip()
        clock.tick(60)

    pygame.quit()

import os, shutil

CLASHUP_LIB = "clashc/stdlib/"

def install(pkg):
    url = f"https://example.com/{pkg}.clsh"
    content = f"// mock {pkg} module"
    with open(os.path.join(CLASHUP_LIB, f"{pkg}.clsh"), "w") as f:
        f.write(content)
    print(f"✅ Installed {pkg}")

def list_packages():
    for f in os.listdir(CLASHUP_LIB):
        if f.endswith(".clsh"):
            print(f"• {f}")

from clashc.lexer import tokenize
from clashc.parser import parse
from interpreter import interpret

src = open("gamescript.clsh").read()
tokens = tokenize(src)
ast = parse(tokens)
interpret(ast)

elif c == "5":
    os.system("python3 clashgame.py")
elif c == "6":
    import pkgman; pkgman.install(input("Install package> "))

import tkinter as tk
from tkinter import filedialog, messagebox
import subprocess, os

class ClashupStudio:
    def __init__(self, root):
        self.root = root
        self.root.title("Clashup Studio IDE")
        self.text = tk.Text(root, bg="black", fg="lime", insertbackground="white")
        self.text.pack(fill=tk.BOTH, expand=True)

        menubar = tk.Menu(root)
        filemenu = tk.Menu(menubar, tearoff=0)
        filemenu.add_command(label="New", command=self.new_file)
        filemenu.add_command(label="Open", command=self.open_file)
        filemenu.add_command(label="Save", command=self.save_file)
        filemenu.add_command(label="Compile", command=self.compile)
        filemenu.add_command(label="Run", command=self.run)
        filemenu.add_separator()
        filemenu.add_command(label="Exit", command=root.quit)
        menubar.add_cascade(label="File", menu=filemenu)

        self.root.config(menu=menubar)
        self.file_path = "input.clsh"

    def new_file(self):
        self.text.delete("1.0", tk.END)

    def open_file(self):
        self.file_path = filedialog.askopenfilename(defaultextension=".clsh")
        with open(self.file_path, "r") as f:
            self.text.delete("1.0", tk.END)
            self.text.insert(tk.END, f.read())

    def save_file(self):
        with open(self.file_path, "w") as f:
            f.write(self.text.get("1.0", tk.END))

    def compile(self):
        self.save_file()
        subprocess.call(["python3", "clashc.py", self.file_path])
        messagebox.showinfo("Compile", "✅ Compilation complete.")

    def run(self):
        subprocess.call(["./output"])

if __name__ == "__main__":
    root = tk.Tk()
    ClashupStudio(root)
    root.mainloop()

class ClashVMStack:
    def __init__(self):
        self.stack = []
        self.pc = 0
        self.instructions = []

    def load(self, bytecode):
        self.instructions = bytecode

    def run(self):
        while self.pc < len(self.instructions):
            op = self.instructions[self.pc]
            if op[0] == "PUSH":
                self.stack.append(op[1])
            elif op[0] == "ADD":
                b, a = self.stack.pop(), self.stack.pop()
                self.stack.append(a + b)
            elif op[0] == "PRINT":
                print(self.stack.pop())
            elif op[0] == "HLT":
                break
            self.pc += 1

import subprocess

def compile_wat_to_wasm(wat_file, wasm_file):
    result = subprocess.run(["wat2wasm", wat_file, "-o", wasm_file])
    if result.returncode == 0:
        print(f"✅ Compiled {wat_file} to {wasm_file}")
    else:
        print(f"❌ Compilation failed.")

def autosave(self):
    with open("autosave.clsh", "w") as f:
        f.write(self.text.get("1.0", tk.END))

self.debug = tk.Text(root, height=5, bg="#111", fg="orange", insertbackground="white")
self.debug.pack(fill=tk.X)

def compile(self):
    self.save_file()
    result = subprocess.run(["python3", "clashc.py", self.file_path], capture_output=True, text=True)
    self.debug.delete("1.0", tk.END)
    self.debug.insert(tk.END, result.stdout + result.stderr)

from clashc.parser import parse
from clashc.lexer import tokenize
import tkinter as tk
from tkinter import ttk

def visualize_ast(code):
    tokens = tokenize(code)
    ast = parse(tokens)

    root = tk.Tk()
    root.title("Clashup AST Viewer")
    tree = ttk.Treeview(root)
    tree.pack(fill=tk.BOTH, expand=True)

    for i, node in enumerate(ast):
        node_text = type(node).__name__
        item = tree.insert('', 'end', text=f"{i}: {node_text}")
        for attr in vars(node):
            tree.insert(item, 'end', text=f"{attr} = {getattr(node, attr)}")

    root.mainloop()

if __name__ == "__main__":
    src = open("input.clsh").read()
    visualize_ast(src)

import tkinter as tk

def clashx_window():
    root = tk.Tk()
    root.title("ClashX GUI Toolkit")

    def clicked(): print("👆 Button clicked!")

    btn = tk.Button(root, text="Click Me", command=clicked)
    btn.pack(pady=20)

    slider = tk.Scale(root, from_=0, to=100, orient="horizontal")
    slider.pack()

    entry = tk.Entry(root)
    entry.pack()

    root.mainloop()

import pygame

def read_gamepad():
    pygame.init()
    pygame.joystick.init()
    joystick = pygame.joystick.Joystick(0)
    joystick.init()

    while True:
        pygame.event.pump()
        x = joystick.get_axis(0)
        y = joystick.get_axis(1)
        a = joystick.get_button(0)
        b = joystick.get_button(1)
        print(f"X:{x:.2f}, Y:{y:.2f}, A:{a}, B:{b}")

import shutil, os, platform

def package_project():
    target = "Clashup_Build"
    os.makedirs(target, exist_ok=True)
    shutil.copy("input.clsh", f"{target}/input.clsh")
    shutil.copy("output.asm", f"{target}/output.asm")
    if os.path.exists("output"):
        shutil.copy("output", f"{target}/clashup_exec")
    elif os.path.exists("output.exe"):
        shutil.copy("output.exe", f"{target}/clashup_exec.exe")
    print(f"📦 Packaged to: {target}/")

from clashc.lexer import tokenize
from clashc.parser import parse
from clashc.optimizer import optimize
from clashc.codegen import CodeGenerator

def clash_assistant(source_code):
    print("🧠 Clashup Trainer Activated...")
    try:
        tokens = tokenize(source_code)
        print(f"🔍 {len(tokens)} tokens identified.")
        ast = parse(tokens)
        print(f"🌳 AST built with {len(ast)} nodes.")
        optimize(ast)
        print(f"⚙️ Optimization complete.")
        gen = CodeGenerator()
        asm = gen.generate(ast)
        print(f"💾 Assembly output prepared.")
        print("🎯 You can now run or debug.")
    except Exception as e:
        print("❌ ERROR:")
        print(str(e))
        if "token" in str(e):
            print("📘 Hint: Check your syntax (missing quotes, colons, or parens).")
        if "assign" in str(e):
            print("📘 Hint: Use `let x = 5`, not just `x = 5`.")

if __name__ == "__main__":
    code = open("input.clsh").read()
    clash_assistant(code)

import shutil, platform, os

def build_installer():
    target = "ClashupInstaller"
    os.makedirs(target, exist_ok=True)
    shutil.copy("input.clsh", f"{target}/input.clsh")
    shutil.copy("output.asm", f"{target}/output.asm")
    shutil.copy("README.md", f"{target}/README.md")
    shutil.copy("ClashupManual.pdf", f"{target}/ClashupManual.pdf")

    if platform.system() == "Windows":
        shutil.copy("output.exe", f"{target}/clashup_exec.exe")
        with open(f"{target}/install.bat", "w") as f:
            f.write("@echo off\n")
            f.write("echo Installing Clashup...\n")
            f.write("copy clashup_exec.exe %SYSTEMROOT%\\System32\\clashup.exe\n")
            f.write("echo Installed.\n")
    else:
        shutil.copy("output", f"{target}/clashup_exec")
        with open(f"{target}/install.sh", "w") as f:
            f.write("#!/bin/bash\n")
            f.write("echo Installing Clashup...\n")
            f.write("sudo cp clashup_exec /usr/local/bin/clashup\n")
            f.write("echo Installed.\n")

    print("✅ Installer Build Complete.")

if __name__ == "__main__":
    build_installer()

#!/usr/bin/env python3

import sys, os

def tokenize(line):
    tokens = line.strip().split()
    return tokens

def parse(tokens):
    if not tokens:
        return ""
    cmd = tokens[0]
    if cmd == "print":
        msg = ' '.join(tokens[1:]).strip('"')
        label = f"message_{hash(msg) % 10000}"
        data = f"{label}: db \"{msg}\", 10, 0"
        code = [
            f"mov rsi, {label}",
            "call print_string"
        ]
        return code, data
    elif cmd == "exit":
        return ["call exit_program"], None
    elif cmd == "input":
        var = tokens[1]
        code = [f"; input placeholder for {var} (not implemented yet)"]
        return code, None
    return [f"; unrecognized: {' '.join(tokens)}"], None

def generate_asm(source_lines):
    code_section = []
    data_section = []
    seen_data = set()
    for line in source_lines:
        tokens = tokenize(line)
        code, data = parse(tokens)
        if code:
            code_section.extend(code)
        if data and data not in seen_data:
            data_section.append(data)
            seen_data.add(data)
    return code_section, data_section

def write_asm(code, data, output_file):
    with open(output_file, "w") as f:
        f.write("section .data\n")
        for d in data:
            f.write("    " + d + "\n")
        f.write("\nsection .text\n")
        f.write("global _start\n")
        f.write("_start:\n")
        for line in code:
            f.write("    " + line + "\n")

def main():
    if len(sys.argv) < 2:
        print("Usage: clashc.py <input.clsh>")
        return
    input_file = sys.argv[1]
    output_file = input_file.replace(".clsh", ".asm")

    with open(input_file, "r") as f:
        source_lines = f.readlines()

    code, data = generate_asm(source_lines)
    write_asm(code, data, output_file)
    print(f"✅ Generated {output_file}")

if __name__ == "__main__":
    main()


---

## 🖥️ 5. Playground GUI Code (Minimal TUI Version)

```python
# clashup_gui.py
import tkinter as tk
import subprocess

def compile_clsh():
    code = editor.get("1.0", tk.END)
    with open("input.clsh", "w") as f:
        f.write(code)
    subprocess.run(["python3", "clashc.py", "input.clsh"])
    subprocess.run(["nasm", "-f", "elf64", "input.asm", "-o", "input.o"])
    subprocess.run(["ld", "input.o", "System.o", "-o", "input"])
    output_label.config(text="✅ Compiled to ./input")

root = tk.Tk()
root.title("Clashup Playground")

editor = tk.Text(root, height=20, width=80)
editor.insert("1.0", "start:\n    print \"Hello!\"\n    exit")
editor.pack()

compile_btn = tk.Button(root, text="Compile", command=compile_clsh)
compile_btn.pack()

output_label = tk.Label(root, text="")
output_label.pack()

root.mainloop()

#!/usr/bin/env python3
import sys, os

label_count = 0
variables = {}
data_labels = {}
data_section = []
code_section = []

def next_label():
    global label_count
    label_count += 1
    return f"_label_{label_count}"

def tokenize(line):
    return line.strip().split()

def parse(tokens):
    if not tokens:
        return

    global code_section, data_section

    cmd = tokens[0]

    if cmd == "let":
        var = tokens[1]
        val = tokens[3]
        variables[var] = val
        code_section.append(f"mov rax, {val}")
        code_section.append(f"mov [{var}], rax")
        data_section.append(f"{var}: dq 0")

    elif cmd == "print":
        msg = ' '.join(tokens[1:]).strip('"')
        if msg not in data_labels:
            label = f"msg_{len(data_labels)}"
            data_labels[msg] = label
            data_section.append(f"{label}: db \"{msg}\", 10, 0")
        label = data_labels[msg]
        code_section.append(f"mov rsi, {label}")
        code_section.append("call print_string")

    elif cmd == "exit":
        code_section.append("call exit_program")

    elif cmd == "loop":
        loop_label = next_label()
        code_section.append(f"{loop_label}:")
        parse(tokenize(' '.join(tokens[1:])))
        code_section.append(f"jmp {loop_label}")

    elif cmd == "if_eq":
        var = tokens[1]
        val = tokens[2]
        then_idx = tokens.index("then")
        then_cmd = tokens[then_idx + 1:]
        end_label = next_label()
        code_section.append(f"mov rax, [{var}]")
        code_section.append(f"cmp rax, {val}")
        code_section.append(f"jne {end_label}")
        parse(then_cmd)
        code_section.append(f"{end_label}:")

def generate_asm(source_lines):
    for line in source_lines:
        tokens = tokenize(line)
        parse(tokens)

def write_asm(output_file):
    with open(output_file, "w") as f:
        f.write("section .data\n")
        for d in set(data_section):
            f.write("    " + d + "\n")

        f.write("\nsection .bss\n")
        for var in variables:
            f.write(f"    {var}: resq 1\n")

        f.write("\nsection .text\n")
        f.write("global _start\n")
        f.write("_start:\n")
        for line in code_section:
            f.write("    " + line + "\n")

def main():
    if len(sys.argv) < 2:
        print("Usage: clashc.py <file.clsh>")
        return

    input_file = sys.argv[1]
    with open(input_file, "r") as f:
        source_lines = f.readlines()

    generate_asm(source_lines)
    write_asm(input_file.replace(".clsh", ".asm"))
    print("✅ Compilation complete.")

if __name__ == "__main__":
    main()

debug_mode = True
instr_count = 0

def debug(line):
    global instr_count
    if debug_mode:
        print(f"[{instr_count:04}] >> {line}")
        instr_count += 1

def emit(line):
    debug(line)
    code_section.append(line)

def optimize():
    for i, line in enumerate(code_section):
        if "mov rax," in line and "add rax," in code_section[i+1]:
            val1 = int(line.split(",")[1])
            val2 = int(code_section[i+1].split(",")[1])
            code_section[i] = f"mov rax, {val1 + val2}"
            code_section[i+1] = "; folded"

def remove_dead_code():
    code_section[:] = [line for line in code_section if not line.startswith("; dead")]

functions = {}

def parse_func(tokens, lines, index):
    """
    Parses a Clashup-style function definition starting at `index` in `lines`.
    Stores the function in the global `functions` dictionary.
    
    tokens: list of tokens from the function declaration line
    lines: full list of source lines
    index: current line index where 'func' is declared
    """
    if len(tokens) < 3:
        raise SyntaxError("Invalid function declaration. Usage: func name(args) {")

    name = tokens[1]
    arg_str = tokens[2].strip("()")
    args = [arg.strip() for arg in arg_str.split(",") if arg.strip()]
    
    body = []
    i = index + 1

    while i < len(lines):
        line = lines[i].strip()
        if line == "}":
            break
        body.append(lines[i])
        i += 1
    else:
        raise SyntaxError(f"Function '{name}' missing closing '}}'")

    functions[name] = {
        "args": args,
        "body": body
    }

    return i  # Return the index of the closing brace line

functions = {}
function_asm_blocks = {}
function_args_regs = ['rdi', 'rsi', 'rdx', 'rcx', 'r8', 'r9']

def parse_func(tokens, lines, index):
    name = tokens[1]
    arg_str = tokens[2].strip("()")
    args = [a.strip() for a in arg_str.split(",") if a.strip()]
    
    body = []
    i = index + 1
    while i < len(lines):
        if lines[i].strip() == "}":
            break
        body.append(lines[i])
        i += 1
    else:
        raise SyntaxError(f"Function '{name}' missing closing brace")

    functions[name] = {"args": args, "body": body}
    return i

def generate_func_asm():
    for fname, fdata in functions.items():
        args = fdata['args']
        body = fdata['body']
        asm = [f"{fname}:", "    push rbp", "    mov rbp, rsp"]
        
        # Assign arguments to local vars
        for idx, arg in enumerate(args):
            if idx < len(function_args_regs):
                reg = function_args_regs[idx]
                asm.append(f"    mov [{arg}], {reg}")
                data_section.append(f"{arg}: dq 0")

        # Process body
        for line in body:
            tokens = tokenize(line)
            if not tokens:
                continue
            if tokens[0] == "let":
                var = tokens[1]
                val = tokens[3]
                if val in args:
                    asm.append(f"    mov rax, [{val}]")
                else:
                    asm.append(f"    mov rax, {val}")
                asm.append(f"    mov [{var}], rax")
                data_section.append(f"{var}: dq 0")
            elif tokens[0] == "return":
                ret = tokens[1]
                if ret in args:
                    asm.append(f"    mov rax, [{ret}]")
                else:
                    asm.append(f"    mov rax, {ret}")
        
        asm.append("    pop rbp")
        asm.append("    ret")
        function_asm_blocks[fname] = asm

def handle_function_call(name, args):
    for i, arg in enumerate(args):
        reg = function_args_regs[i]
        if arg.isdigit():
            code_section.append(f"mov {reg}, {arg}")
        else:
            code_section.append(f"mov rax, [{arg}]")
            code_section.append(f"mov {reg}, rax")
    code_section.append(f"call {name}")
    code_section.append("call print_int")

