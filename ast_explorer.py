from graphviz import Digraph
import json

def generate_ast(code_lines):
    ast = {"type": "Program", "body": []}
    for line in code_lines:
        if line.startswith("let"):
            ast["body"].append({"type": "Declaration", "value": line.strip()})
        elif "print(" in line:
            ast["body"].append({"type": "Output", "value": line.strip()})
        elif "func" in line:
            ast["body"].append({"type": "Function", "value": line.strip()})
    return ast

def visualize_ast(ast_dict, filename="ast_tree"):
    dot = Digraph()
    dot.node("root", "Program")
    for i, node in enumerate(ast_dict["body"]):
        label = f"{node['type']}\n{node['value']}"
        dot.node(f"n{i}", label)
        dot.edge("root", f"n{i}")
    dot.render(filename, format="png", cleanup=True)
    print(f"🌲 AST tree rendered to {filename}.png")

if __name__ == "__main__":
    with open("input.clsh") as f:
        code = f.readlines()
    ast = generate_ast(code)
    visualize_ast(ast)
