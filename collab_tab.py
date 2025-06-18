from PyQt5.QtWidgets import QWidget, QTextEdit, QVBoxLayout, QLabel
import threading
import websocket
import json

class CollabTab(QWidget):
    def __init__(self):
        super().__init__()
        self.editor = QTextEdit()
        self.status = QLabel("🟢 Connected")
        layout = QVBoxLayout()
        layout.addWidget(self.status)
        layout.addWidget(self.editor)
        self.setLayout(layout)
        self.editor.textChanged.connect(self.send_code)
        threading.Thread(target=self.connect_ws, daemon=True).start()

    def connect_ws(self):
        self.ws = websocket.WebSocketApp("ws://localhost:8765",
                                         on_message=self.on_message)
        self.ws.run_forever()

    def send_code(self):
        if hasattr(self, "ws") and self.ws.sock and self.ws.sock.connected:
            msg = {"text": self.editor.toPlainText()}
            self.ws.send(json.dumps(msg))

    def on_message(self, ws, message):
        data = json.loads(message)
        self.editor.blockSignals(True)
        self.editor.setPlainText(data["text"])
        self.editor.blockSignals(False)
