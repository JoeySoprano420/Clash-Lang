from PyQt5.QtWidgets import *
import websocket, threading, sys

class Arena(QWidget):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Clashup Code Arena")
        self.layout = QVBoxLayout(self)
        self.text = QTextEdit()
        self.chat = QTextEdit()
        self.layout.addWidget(self.text)
        self.layout.addWidget(self.chat)
        self.ws = websocket.WebSocketApp("ws://localhost:9898", on_message=self.on_msg)
        threading.Thread(target=self.ws.run_forever, daemon=True).start()

    def on_msg(self, ws, msg):
        self.chat.append(f"NPC: {self.npc_respond(msg)}")

    def npc_respond(self, msg):
        if "for" in msg or "loop" in msg:
            return "Nice use of loops! Try recursion next."
        elif "print" in msg:
            return "Output is a coder's voice."
        return "Keep coding!"

if __name__ == "__main__":
    app = QApplication(sys.argv)
    arena = Arena()
    arena.show()
    sys.exit(app.exec_())
