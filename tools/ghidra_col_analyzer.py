# @title COL Editor 2 Analyzer
# @author X-Seti / Claude
# @category Analysis
# @keybinding
# @menupath Tools.COL Editor 2 Analyzer
# @toolbar

# Ghidra Script: COL Editor 2 - COL Format Analyzer
# Run from: Script Manager > double-click
# Results saved to: ~/ghidra_col_report.txt
# Paste that file to Claude for struct analysis

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
    listing = program.getListing()
    refs    = program.getReferenceManager()
    funcs   = program.getFunctionManager()

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
            log("  {} at {}".format(label, found))
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
                            log("  {} @ {} (refs {})".format(key, fn.getEntryPoint(), label))
            except Exception as e:
                log("  ref error: {}".format(e))

    # ── 3. Decompile COL functions ───────────────────────────────
    log("\n[3] Decompiling COL functions...")
    try:
        from ghidra.app.decompiler import DecompInterface
        decomp = DecompInterface()
        decomp.openProgram(program)
        monitor = ghidra.util.task.TaskMonitor.DUMMY

        for fn_name, fn in list(col_functions.items())[:15]:
            log("\n" + "="*50)
            log("FUNCTION: {}  @ {}".format(fn_name, fn.getEntryPoint()))
            log("="*50)
            try:
                result = decomp.decompileFunction(fn, 60, monitor)
                if result and result.decompileCompleted():
                    code = result.getDecompiledFunction().getC()
                    for line in code.split('\n')[:80]:
                        log(line)
                else:
                    log("  [decompile failed]")
            except Exception as e:
                log("  [error: {}]".format(e))

        decomp.closeProgram()
    except Exception as e:
        log("Decompiler error: {}".format(e))

    # ── 4. Find fread/ReadFile calls ─────────────────────────────
    log("\n[4] File I/O calls...")
    for io_name in ["fread","fwrite","ReadFile","WriteFile","fopen","CreateFileA"]:
        try:
            syms = getSymbols(io_name, None)
            for sym in syms:
                log("  {} @ {}".format(io_name, sym.getAddress()))
                for ref in refs.getReferencesTo(sym.getAddress()):
                    fn = funcs.getFunctionContaining(ref.getFromAddress())
                    if fn:
                        log("    <- {} @ {}".format(fn.getName(), ref.getFromAddress()))
        except Exception:
            pass

    # ── 5. Known struct sizes for reference ──────────────────────
    log("\n[5] Known COL struct sizes for cross-reference:")
    log("  COL1: header=32, sphere=20, box=28, face=8, vertex=6")
    log("  COL2: header=32, sphere=20, box=28, face=8, vertex=12")
    log("  COL3: same as COL2 + face_groups, shadow_mesh flags")
    log("  Name field: 24 bytes (null padded)")
    log("  Model ID: 2 bytes (uint16)")

    # ── 6. Write report ──────────────────────────────────────────
    report_path = os.path.expanduser("~/ghidra_col_report.txt")
    try:
        with open(report_path, 'w') as f:
            f.write('\n'.join(output_lines))
        log("\n[DONE] Report saved to: {}".format(report_path))
    except Exception as e:
        log("\n[ERROR] Could not save: {}".format(e))
        log("Copy Script Manager output manually instead.")

    log("Paste report to Claude for struct analysis.")

run()
