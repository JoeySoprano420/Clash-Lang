from reportlab.lib.pagesizes import letter
from reportlab.pdfgen import canvas
from PIL import Image, ImageDraw, ImageFont

def generate_manual():
    c = canvas.Canvas("Clashup_Manual.pdf", pagesize=letter)
    y = 750
    c.setFont("Helvetica-Bold", 18)
    c.drawString(72, y, "📕 Clashup Supreme Manual")
    c.setFont("Courier", 12)
    y -= 40
    chapters = [
        "1. Language Syntax", "2. Timeline Debugger", "3. Game Runtime",
        "4. Codex Engine", "5. MMO Arena", "6. Bootloader + Kernel"
    ]
    for ch in chapters:
        c.drawString(100, y, f"• {ch}")
        y -= 30
    c.save()

def generate_boxed_art():
    img = Image.new("RGB", (600, 300), color=(20, 20, 40))
    draw = ImageDraw.Draw(img)
    font = ImageFont.load_default()
    draw.text((30, 120), "CLASHUP SUPREME", font=font, fill=(255, 255, 0))
    img.save("Clashup_BoxArt.png")

generate_manual()
generate_boxed_art()
