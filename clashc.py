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

