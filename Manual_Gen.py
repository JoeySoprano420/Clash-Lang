from reportlab.lib.pagesizes import letter
from reportlab.pdfgen import canvas

def generate_manual(filename="Clashup_Manual.pdf"):
    c = canvas.Canvas(filename, pagesize=letter)
    width, height = letter
    y = height - 40

    title = "📘 Clashup Language Manual"
    c.setFont("Helvetica-Bold", 20)
    c.drawString(72, y, title)
    y -= 40

    sections = [
        ("Basics", [
            "let x = 5 — declare and assign",
            "input(name) — get input from user",
            "print(name) — output a value",
        ]),
        ("Control Flow", [
            "if_eq x 7 then { ... } — conditional block",
            "for i in range(0,10) { ... } — loop range",
            "while x < 5 { ... } — loop while true",
            "break, continue — flow control",
        ]),
        ("Functions", [
            "func f(x: int) { return x + 1 }",
        ]),
        ("Debug Tools", [
            "breakpoint() — halt for inspection",
            "symbol_dump() — log variables",
        ]),
        ("Imports", [
            "import \"math.clshlib\" — reuse code"
        ])
    ]

    c.setFont("Helvetica", 12)
    for section, entries in sections:
        y -= 20
        c.setFont("Helvetica-Bold", 14)
        c.drawString(72, y, f"🔹 {section}")
        c.setFont("Helvetica", 12)
        for entry in entries:
            y -= 16
            c.drawString(92, y, f"- {entry}")
        y -= 10
        if y < 100:
            c.showPage()
            y = height - 40

    c.save()
    print(f"✅ PDF manual created: {filename}")

if __name__ == "__main__":
    generate_manual()
