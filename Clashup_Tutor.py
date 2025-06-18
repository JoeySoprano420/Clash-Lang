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

import random

quiz_bank = {
    "What does `let x = 5` do?": {
        "a": "Defines a function",
        "b": "Prints a variable",
        "c": "Declares and assigns a variable",
        "d": "Starts a loop",
        "answer": "c"
    },
    "What does `if_eq x 7` mean?": {
        "a": "Compare and jump if true",
        "b": "Assign 7 to x",
        "c": "Exit the program",
        "d": "Loop while x = 7",
        "answer": "a"
    },
    "What does `breakpoint()` do?": {
        "a": "Saves the file",
        "b": "Halts execution for inspection",
        "c": "Declares a variable",
        "d": "Starts a loop",
        "answer": "b"
    }
}

def take_quiz():
    print("🎓 Clashup Quiz Begins!")
    score = 0
    for question, opts in quiz_bank.items():
        print("\n" + question)
        for key in "abcd":
            print(f"  {key}) {opts[key]}")
        ans = input("Your answer: ").lower()
        if ans == opts["answer"]:
            print("✅ Correct!")
            score += 1
        else:
            print(f"❌ Incorrect. Correct: {opts['answer']}")
    print(f"\n🎯 Final Score: {score}/{len(quiz_bank)}")

if __name__ == "__main__":
    print("Choose an option:\n1) Explain .clsh\n2) Take Quiz")
    choice = input("> ")
    if choice == "1":
        from clashc import tutor
        tutor()
    elif choice == "2":
        take_quiz()

