from flask import Flask, render_template_string, request
import subprocess

app = Flask(__name__)

HTML = """
<!DOCTYPE html>
<html>
<head>
    <title>Clashup Online Playground</title>
</head>
<body style="font-family: monospace;">
    <h2>🧱 Clashup Playground</h2>
    <form method="POST">
        <textarea name="code" rows="20" cols="80">{{code}}</textarea><br>
        <button type="submit">Compile</button>
    </form>
    <h3>Output:</h3>
    <pre>{{output}}</pre>
</body>
</html>
"""

@app.route("/", methods=["GET", "POST"])
def index():
    code = ""
    output = ""
    if request.method == "POST":
        code = request.form["code"]
        with open("input.clsh", "w") as f:
            f.write(code)
        proc = subprocess.run(["python3", "clashc.py", "input.clsh"], capture_output=True, text=True)
        output = proc.stdout + "\n" + proc.stderr
    return render_template_string(HTML, code=code, output=output)

if __name__ == "__main__":
    app.run(port=8080, debug=True)

import re

def is_safe(code):
    # Simple sandboxing: reject system commands
    banned = ['import os', 'subprocess', 'open(', 'eval', 'exec']
    return not any(bad in code for bad in banned)

@app.route("/", methods=["GET", "POST"])
def index():
    code = ""
    output = ""
    if request.method == "POST":
        code = request.form["code"]
        if not is_safe(code):
            output = "🚫 Unsafe code detected."
        else:
            with open("input.clsh", "w") as f:
                f.write(code)
            proc = subprocess.run(["python3", "clashc.py", "input.clsh"], capture_output=True, text=True)
            output = proc.stdout + "\n" + proc.stderr
    return render_template_string(HTML, code=code, output=output)

from PyQt5.QtWidgets import QApplication, QTabWidget, QWidget, QVBoxLayout, QTextEdit, QPushButton
import sys, subprocess

class ClashupTab(QWidget):
    def __init__(self, label, file):
        super().__init__()
        self.file = file
        self.editor = QTextEdit()
        self.output = QTextEdit()
        self.output.setReadOnly(True)

        self.compile_btn = QPushButton("Compile")
        self.run_btn = QPushButton("Run")
        self.compile_btn.clicked.connect(self.compile_code)
        self.run_btn.clicked.connect(self.run_code)

        layout = QVBoxLayout()
        layout.addWidget(self.editor)
        layout.addWidget(self.compile_btn)
        layout.addWidget(self.run_btn)
        layout.addWidget(self.output)
        self.setLayout(layout)

    def compile_code(self):
        with open(self.file, 'w') as f:
            f.write(self.editor.toPlainText())
        proc = subprocess.run(["python3", "clashc.py", self.file], capture_output=True, text=True)
        self.output.setText(proc.stdout + proc.stderr)

    def run_code(self):
        proc = subprocess.run(["./clashup"], capture_output=True, text=True)
        self.output.setText(proc.stdout + proc.stderr)

class ClashupApp(QTabWidget):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Clashup IDE")
        self.setGeometry(100, 100, 800, 600)
        self.addTab(ClashupTab("main", "input.clsh"), "Main")
        self.addTab(ClashupTab("math", "math.clshlib"), "Math Library")
        self.addTab(ClashupTab("string", "string.clshlib"), "String Library")

app = QApplication(sys.argv)
win = ClashupApp()
win.show()
sys.exit(app.exec_())

