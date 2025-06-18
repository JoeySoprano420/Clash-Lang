line = self.program[self.current]
self.timeline.setCurrentRow(self.current)

# Detect assignments
if line.startswith("let"):
    parts = line.split()
    if len(parts) >= 4:
        var = parts[1]
        val = parts[3]
        self.symbols[var] = val
elif "=" in line:
    parts = line.split("=")
    var = parts[0].strip()
    val = parts[1].strip()
    self.symbols[var] = val

# Update label
self.symbol_label.setText(f"🔍 Symbol Watch: {self.symbols}")
