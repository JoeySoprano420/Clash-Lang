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

