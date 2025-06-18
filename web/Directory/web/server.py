from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse, PlainTextResponse
from pydantic import BaseModel
from clashc.lexer import tokenize
from clashc.parser import parse
from clashc.interpreter import interpret

app = FastAPI()

class Code(BaseModel):
    code: str

@app.get("/", response_class=HTMLResponse)
def index():
    return open("web/playground.html").read()

@app.post("/run", response_class=PlainTextResponse)
def run_code(payload: Code):
    tokens = tokenize(payload.code)
    ast = parse(tokens)
    from io import StringIO
    import sys
    old_stdout = sys.stdout
    sys.stdout = mystdout = StringIO()
    interpret(ast)
    sys.stdout = old_stdout
    return mystdout.getvalue()
