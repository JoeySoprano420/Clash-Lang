/Clash/
│
├── clash.asm           # Main language compiler/interpreter
├── clash_std.asm       # Standard macros and I/O routines
├── clash_lib.asm       # Optional: math, string, logic helpers
├── examples/
│   ├── hello.clsh
│   └── loop.clsh
├── build_clash.bat     # Windows build script (NASM + GoLink)
├── README.md

Clashup/
├── clashc.py              # Clashup compiler
├── runtime.asm            # Shared low-level syscall helpers
├── input.clsh             # Clashup source
├── build_linux.sh         # Linux compile/run
├── build_windows.bat      # Windows compile/run
└── output.asm             # Auto-generated

Clashup/
├── clashc/                          ← Compiler folder
│   ├── __init__.py
│   ├── lexer.py
│   ├── parser.py
│   ├── ast.py
│   ├── codegen.py
│   ├── optimizer.py
│   └── stdlib/                      ← Mini Clashup stdlib
│       ├── math.clsh
│       └── io.clsh
│
├── gui_launcher.py                 ← TUI Launcher
├── clashc.py                       ← CLI Entrypoint
├── output.asm
├── input.clsh
├── build_linux.sh
├── build_windows.bat
├── README.md
└── .vscode/
    ├── syntax.tmLanguage.json
    ├── settings.json
    └── launch.json
