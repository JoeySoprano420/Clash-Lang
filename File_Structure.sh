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

