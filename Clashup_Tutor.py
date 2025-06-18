def explain_line(line):
    line = line.strip()
    if line.startswith("let"):
        return "🧠 Declares a variable and assigns a value."
    elif line.startswith("print("):
        return "🧠 Prints the value or string to the output."
    elif line.startswith("input("):
        return "🧠 Waits for user input and stores it."
    elif line.startswith("if_eq"):
        return "🧠 Conditional check: if variable equals value, execute block."
    elif line.startswith("while"):
        return "🧠 While loop: repeats while condition is true."
    elif line.startswith("for"):
        return "🧠 For loop: iterate through a numeric range."
    elif line.startswith("break"):
        return "🧠 Break: exits current loop."
    elif line.startswith("continue"):
        return "🧠 Continue: jumps to next loop iteration."
    elif "breakpoint()" in line:
        return "🧠 Debug breakpoint — halts program for inspection."
    elif "symbol_dump()" in line:
        return "🧠 Prints current variable state for debugging."
    elif line.startswith("func"):
        return "🧠 Function definition."
    return "🧠 Statement or operation."

def tutor(filename="input.clsh"):
    print("📘 Clashup AI Tutor Activated")
    with open(filename, 'r') as f:
        for lineno, line in enumerate(f.readlines(), 1):
            print(f"{lineno:>3}: {line.strip()}")
            print(f"     {explain_line(line)}")

if __name__ == "__main__":
    tutor()
