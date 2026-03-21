# Ghidra Script: COL Editor 2 - COL Format Analyzer
# Run from: Ghidra Script Manager (Window > Script Manager > Python)
# Author: X-Seti / Claude - March 2026
#
# USAGE:
# 1. Ghidra: Window > Script Manager
# 2. Script Directories > add this file's folder
# 3. Double-click ghidra_col_analyzer.py to run
# 4. Results in ~/ghidra_col_report.txt — paste to Claude

import os
import re

output_lines = []

def log(s):
    print(s)
    output_lines.append(str(s))

def run():
    log("=" * 60)
    log("COL Editor 2 - Ghidra Analysis Script")
    log("=" * 60)

    program = currentProgram
    memory  = program.getMemory()
    listing = program.getListing()
    refs    = program.getReferenceManager()
    funcs   = program.getFunctionManager()
    dtmgr   = program.getDataTypeManager()

    # ── 1. Find COL magic strings ────────────────────────────────
    log("\n[1] Searching for COL magic bytes...")
    magics = {
        b"COLL": "COL1_magic",
        b"COL2": "COL2_magic",
        b"COL3": "COL3_magic",
        b"COL4": "COL4_magic",
    }

    magic_addrs = {}
    for magic_bytes, label in magics.items():
        found = findBytes(None, magic_bytes, 50)
        while found is not None:
            log(f"  {label} at {found}")
            magic_addrs.setdefault(label, []).append(found)
            try:
                createLabel(found, label, True)
            except Exception:
                pass
            try:
                found = findBytes(found.add(1), magic_bytes, 50)
            except Exception:
                break

    # ── 2. Find functions referencing magic strings ──────────────
    log("\n[2] Functions referencing COL magic strings...")
    col_functions = {}
    for label, addrs in magic_addrs.items():
        for addr in addrs:
            try:
                for ref in refs.getReferencesTo(addr):
                    fn = funcs.getFunctionContaining(ref.getFromAddress())
                    if fn:
                        key = fn.getName()
                        if key not in col_functions:
                            col_functions[key] = fn
                            log(f"  {key} @ {fn.getEntryPoint()} (refs {label})")
            except Exception as e:
                log(f"  ref error: {e}")

    # ── 3. Decompile and dump pseudocode ─────────────────────────
    log("\n[3] Decompiling COL functions (first 15)...")
    try:
        from ghidra.app.decompiler import DecompInterface
        decomp = DecompInterface()
        decomp.openProgram(program)
        monitor = ghidra.util.task.TaskMonitor.DUMMY

        for fn_name, fn in list(col_functions.items())[:15]:
            log(f"\n{'='*40}")
            log(f"FUNCTION: {fn_name}  @ {fn.getEntryPoint()}")
            log(f"{'='*40}")
            try:
                result = decomp.decompileFunction(fn, 60, monitor)
                if result and result.decompileCompleted():
                    code = result.getDecompiledFunction().getC()
                    for line in code.split('\n')[:80]:
                        log(line)
                else:
                    log("  [decompile failed]")
            except Exception as e:
                log(f"  [error: {e}]")

        decomp.closeProgram()
    except Exception as e:
        log(f"Decompiler unavailable: {e}")

    # ── 4. Find fread/ReadFile calls ─────────────────────────────
    log("\n[4] File I/O calls...")
    for io_name in ["fread","fwrite","ReadFile","WriteFile","_read","_write","fopen","CreateFileA"]:
        try:
            syms = getSymbols(io_name, None)
            for sym in syms:
                log(f"  {io_name} @ {sym.getAddress()}")
                for ref in refs.getReferencesTo(sym.getAddress()):
                    fn = funcs.getFunctionContaining(ref.getFromAddress())
                    if fn:
                        log(f"    <- {fn.getName()} @ {ref.getFromAddress()}")
        except Exception:
            pass

    # ── 5. Search for struct size constants ──────────────────────
    log("\n[5] Looking for struct size constants (typical COL sizes)...")
    # COL1 sphere=20, box=28, face=8, vertex=6
    # COL2 sphere=20, box=28, face=8, vertex=12
    interesting = [4,6,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64]
    # We just note this for manual cross-reference
    log("  Key struct sizes to watch for in decompiler output:")
    log("  COL1: bounding_sphere=20, sphere=20, box=28, face=8, vertex=6")
    log("  COL2: bounding_sphere=20, sphere=20, box=28, face=8, vertex=12")
    log("  COL3: same as COL2 + face_groups")
    log("  Header: name=24bytes, model_id=2bytes, size=4bytes")

    # ── 6. Write report ──────────────────────────────────────────
    report_path = os.path.expanduser("~/ghidra_col_report.txt")
    try:
        with open(report_path, 'w') as f:
            f.write('\n'.join(output_lines))
        log(f"\n[DONE] Report saved to: {report_path}")
    except Exception as e:
        log(f"\n[ERROR] Could not save report: {e}")
        log("Copy the Script Manager output manually instead.")

    log("\nPaste the report contents to Claude for analysis.")

run()
