# @title COL Editor 2 Analyzer
# @author X-Seti / Claude
# @category Analysis
# @keybinding
# @menupath Tools.COL Editor 2 Analyzer
# @toolbar

import os

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
                # Pass single address directly — not as list
                ref_iter = refs.getReferencesTo(addr)
                for ref in ref_iter:
                    fn = funcs.getFunctionContaining(ref.getFromAddress())
                    if fn:
                        key = fn.getName()
                        if key not in col_functions:
                            col_functions[key] = fn
                            log("  {} @ {} (refs {})".format(
                                key, fn.getEntryPoint(), label))
            except Exception as e:
                log("  ref error: {}".format(e))

    # ── 3. Decompile COL functions ───────────────────────────────
    log("\n[3] Decompiling COL functions...")
    try:
        from ghidra.app.decompiler import DecompInterface
        from ghidra.util.task import ConsoleTaskMonitor

        decomp = DecompInterface()
        decomp.openProgram(program)
        monitor = ConsoleTaskMonitor()

        count = 0
        for fn_name, fn in col_functions.items():
            if count >= 20:
                break
            count += 1
            log("\n" + "="*50)
            log("FUNCTION: {}  @ {}".format(fn_name, fn.getEntryPoint()))
            log("="*50)
            try:
                result = decomp.decompileFunction(fn, 60, monitor)
                if result and result.decompileCompleted():
                    code = result.getDecompiledFunction().getC()
                    for line in code.split('\n')[:100]:
                        log(line)
                else:
                    log("  [decompile failed or timed out]")
            except Exception as e:
                log("  [error: {}]".format(e))

        decomp.closeProgram()
    except Exception as e:
        log("Decompiler setup error: {}".format(e))

    # ── 4. Find fread/ReadFile calls ─────────────────────────────
    log("\n[4] File I/O calls...")
    for io_name in ["fread", "fwrite", "ReadFile", "WriteFile",
                    "fopen", "CreateFileA", "CreateFileW",
                    "_fread", "_fwrite"]:
        try:
            syms = getSymbols(io_name, None)
            for sym in syms:
                log("  {} @ {}".format(io_name, sym.getAddress()))
                ref_iter = refs.getReferencesTo(sym.getAddress())
                for ref in ref_iter:
                    fn = funcs.getFunctionContaining(ref.getFromAddress())
                    if fn:
                        log("    <- {} @ {}".format(
                            fn.getName(), ref.getFromAddress()))
        except Exception as e:
            log("  {} error: {}".format(io_name, e))

    # ── 5. Search address 005087ec neighbourhood ─────────────────
    log("\n[5] Analysing neighbourhood of COL1 magic at 005087ec...")
    try:
        from ghidra.program.model.address import Address
        addr = toAddr(0x005087ec)
        # Show 64 bytes before and after
        mem = program.getMemory()
        log("  Bytes at 005087ec:")
        row = ""
        for i in range(-16, 64):
            try:
                b = mem.getByte(addr.add(i)) & 0xFF
                row += "{:02X} ".format(b)
                if (i+17) % 16 == 0:
                    log("  " + row)
                    row = ""
            except Exception:
                pass
        if row:
            log("  " + row)

        # Find function containing this address
        fn = funcs.getFunctionContaining(addr)
        if fn:
            log("  Contained in function: {} @ {}".format(
                fn.getName(), fn.getEntryPoint()))
            col_functions[fn.getName()] = fn
        else:
            log("  Not inside a function — likely a data section")
            # Show references TO this address
            log("  References to 005087ec:")
            for ref in refs.getReferencesTo(addr):
                fn2 = funcs.getFunctionContaining(ref.getFromAddress())
                fname = fn2.getName() if fn2 else "?"
                log("    from {} in {}".format(ref.getFromAddress(), fname))
                if fn2 and fn2.getName() not in col_functions:
                    col_functions[fn2.getName()] = fn2
    except Exception as e:
        log("  neighbourhood error: {}".format(e))

    # ── 6. Decompile any newly found functions ───────────────────
    if col_functions:
        log("\n[6] Decompiling all found functions...")
        try:
            from ghidra.app.decompiler import DecompInterface
            from ghidra.util.task import ConsoleTaskMonitor
            decomp = DecompInterface()
            decomp.openProgram(program)
            monitor = ConsoleTaskMonitor()
            for fn_name, fn in list(col_functions.items())[:20]:
                log("\n" + "="*50)
                log("FUNCTION: {}  @ {}".format(fn_name, fn.getEntryPoint()))
                log("="*50)
                try:
                    result = decomp.decompileFunction(fn, 60, monitor)
                    if result and result.decompileCompleted():
                        code = result.getDecompiledFunction().getC()
                        for line in code.split('\n')[:120]:
                            log(line)
                    else:
                        log("  [decompile failed]")
                except Exception as e:
                    log("  [error: {}]".format(e))
            decomp.closeProgram()
        except Exception as e:
            log("Decompiler error: {}".format(e))

    # ── 7. Known struct sizes ────────────────────────────────────
    log("\n[7] Known COL struct sizes for reference:")
    log("  COL1: header=32, sphere=20, box=28, face=8, vertex=6")
    log("  COL2: header=32, sphere=20, box=28, face=8, vertex=12")
    log("  COL3: same as COL2 + face_groups + shadow mesh flags")
    log("  Name: 24 bytes null-padded, Model ID: 2 bytes uint16")

    # ── Write report ─────────────────────────────────────────────
    report_path = os.path.expanduser("~/ghidra_col_report.txt")
    try:
        with open(report_path, 'w') as f:
            f.write('\n'.join(output_lines))
        log("\n[DONE] Report saved to: {}".format(report_path))
    except Exception as e:
        log("\n[ERROR] Could not save: {}".format(e))

    log("Paste ~/ghidra_col_report.txt to Claude.")

run()
