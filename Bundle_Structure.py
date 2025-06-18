Clashup-Release/
├── src/                         # Full Source Code
│   ├── clashc/                  # Compiler Toolchain
│   ├── modules/                 # Standard Library
│   ├── studio/                  # IDE Files
│   ├── gui/                     # ClashX Widgets
│   ├── vm/                      # Stack VM + Bytecode
│   └── emitters/                # WASM, .asm
│
├── examples/                    # Demo Scripts
│   ├── hello.clsh
│   ├── gamepad.clsh
│   └── netfetch.clsh
│
├── bin/                         # Compiled Artifacts
│   ├── clashup_exec             # Linux/macOS
│   ├── clashup_exec.exe         # Windows
│   └── output.asm
│
├── tools/                       # Extra Tools
│   ├── package.py               # One-Click Bundler
│   ├── build_linux.sh
│   └── build_windows.bat
│
├── clashup.clshproj             # Project Metadata
├── autosave.clsh
├── README.md
├── LICENSE.md
└── .vscode/
    ├── syntax.tmLanguage.json
    ├── settings.json
    └── launch.json



