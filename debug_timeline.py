from PyQt5.QtWidgets import QWidget, QVBoxLayout, QTextEdit, QPushButton, QListWidget, QLabel
import time

class TimelineDebugger(QWidget):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("🔍 Clashup Debugger Timeline")
        self.editor = QTextEdit()
        self.timeline = QListWidget()
        self.label = QLabel("🧠 Execution Output")
        self.step_button = QPushButton("▶️ Step")

        layout = QVBoxLayout()
        layout.addWidget(self.editor)
        layout.addWidget(self.timeline)
        layout.addWidget(self.step_button)
        layout.addWidget(self.label)
        self.setLayout(layout)

        self.step_button.clicked.connect(self.step_through)
        self.program = []
        self.current = 0

    def load_code(self, filename="input.clsh"):
        with open(filename, 'r') as f:
            self.program = [line.strip() for line in f if line.strip()]
        self.timeline.clear()
        for i, line in enumerate(self.program):
            self.timeline.addItem(f"{i+1}: {line}")
        self.editor.setPlainText("\n".join(self.program))
        self.current = 0

    def step_through(self):
        if self.current < len(self.program):
            line = self.program[self.current]
            self.label.setText(f"🧠 Executing: {line}")
            self.timeline.setCurrentRow(self.current)
            self.timeline.item(self.current).setBackgroundColor("#fffb91")
            self.current += 1
            time.sleep(0.2)
        else:
            self.label.setText("✅ Execution complete.")

self.symbols = {}
self.symbol_label = QLabel("🔍 Symbol Watch: {}")
layout.addWidget(self.symbol_label)

