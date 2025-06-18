from PyQt5.QtWidgets import QWidget, QTextEdit, QPushButton, QVBoxLayout
import openai

class AIAssistant(QWidget):
    def __init__(self):
        super().__init__()
        self.prompt_input = QTextEdit()
        self.code_output = QTextEdit()
        self.code_output.setReadOnly(True)
        self.button = QPushButton("Generate Code")
        self.button.clicked.connect(self.run_prompt)

        layout = QVBoxLayout()
        layout.addWidget(self.prompt_input)
        layout.addWidget(self.button)
        layout.addWidget(self.code_output)
        self.setLayout(layout)

    def run_prompt(self):
        prompt = self.prompt_input.toPlainText()
        response = openai.ChatCompletion.create(
            model="gpt-4",
            messages=[{"role": "user", "content": f"Write this in Clashup code:\n{prompt}"}]
        )
        result = response.choices[0].message.content.strip()
        self.code_output.setText(result)
