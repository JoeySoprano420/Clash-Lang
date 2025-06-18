#!/bin/bash
python3 clashup_to_hdlir.py input.clsh
python3 clashup_to_verilog.py
yosys -p "synth_ice40 -top clashup_core -blif clashup.blif" clashup_core.v
nextpnr-ice40 --hx8k --package ct256 --json clashup.json --pcf constraints.pcf --asc clashup.asc
icepack clashup.asc clashup.bin
openFPGALoader clashup.bin
