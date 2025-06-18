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
