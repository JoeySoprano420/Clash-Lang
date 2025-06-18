from PyQt5.QtWidgets import QApplication, QTreeWidget, QTreeWidgetItem, QVBoxLayout, QWidget, QTextEdit
import json, sys

class Codex(QWidget):
    def __init__(self, codex_file="codex.json"):
        super().__init__()
        self.setWindowTitle("VACU World-Builder Codex")
        self.layout = QVBoxLayout(self)
        self.tree = QTreeWidget()
        self.detail = QTextEdit()
        self.layout.addWidget(self.tree)
        self.layout.addWidget(self.detail)

        self.tree.setHeaderLabels(["Codex"])
        with open(codex_file) as f:
            data = json.load(f)
        self.add_items(self.tree, data)
        self.tree.itemClicked.connect(self.show_detail)

    def add_items(self, parent, data):
        for k, v in data.items():
            item = QTreeWidgetItem([k])
            parent.addTopLevelItem(item) if parent == self.tree else parent.addChild(item)
            if isinstance(v, dict):
                self.add_items(item, v)
            else:
                item.setData(0, 1, v)

    def show_detail(self, item, _):
        self.detail.setText(str(item.data(0, 1)))

if __name__ == "__main__":
    app = QApplication(sys.argv)
    codex = Codex()
    codex.show()
    sys.exit(app.exec_())
