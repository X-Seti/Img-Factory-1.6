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

def get_refs_to(refs, addr):
    """Safe wrapper — always passes a single Address object."""
    try:
        # addr might be an Address or Address[] — unwrap if needed
        if hasattr(addr, '__iter__') and not hasattr(addr, 'getOffset'):
            results = []
            for a in addr:
                results.extend(list(refs.getReferencesTo(a)))
            return results
        return list(refs.getReferencesTo(addr))
    except Exception as e:
        log("  getReferencesTo error: {}".format(e))
        return []

def run():
    log("=" * 60)
    log("COL Editor 2 - Ghidra Analysis Script")
    log("=" * 60)

    program = currentProgram
    listing = program.getListing()
    refs    = program.getReferenceManager()
    funcs   = program.getFunctionManager()
    mem     = program.getMemory()

    # ── 1. Find COL magic bytes ──────────────────────────────────
    log("\n[1] Searching for COL magic bytes...")
    magics = {
        b"COLL": "COL1_magic",
        b"COL2": "COL2_magic",
        b"COL3": "COL3_magic",
        b"COL4": "COL4_magic",
    }

    # Store as flat list of (addr_object, label)
    all_magic_hits = []
    for magic_bytes, label in magics.items():
        cursor = None
        while True:
            found = findBytes(cursor, magic_bytes, 10)
            if found is None:
                break
            # findBytes returns a single Address in PyGhidra
            # but sometimes wraps it — get the first element safely
            if hasattr(found, '__len__') and not hasattr(found, 'getOffset'):
                if len(found) == 0:
                    break
                addr = found[0]
            else:
                addr = found
            log("  {} at {}".format(label, addr))
            all_magic_hits.append((addr, label))
            try:
                createLabel(addr, label, True)
            except Exception:
                pass
            try:
                cursor = addr.add(1)
            except Exception:
                break

    # ── 2. Find functions referencing magic addresses ────────────
    log("\n[2] Functions referencing COL magic strings...")
    col_functions = {}
    for addr, label in all_magic_hits:
        try:
            for ref in refs.getReferencesTo(addr):
                fn = funcs.getFunctionContaining(ref.getFromAddress())
                if fn:
                    key = fn.getName()
                    if key not in col_functions:
                        col_functions[key] = fn
                        log("  {} @ {} (refs {})".format(
                            key, fn.getEntryPoint(), label))
        except Exception as e:
            log("  ref error for {}: {}".format(addr, e))

    # ── 3. Find fread/file I/O functions ─────────────────────────
    log("\n[3] File I/O functions...")
    io_fns = {}
    for io_name in ["fread","fwrite","ReadFile","WriteFile",
                    "fopen","fclose","CreateFileA","CreateFileW",
                    "_fread","_fwrite","_fopen"]:
        try:
            syms = getSymbols(io_name, None)
            for sym in syms:
                sym_addr = sym.getAddress()
                log("  {} @ {}".format(io_name, sym_addr))
                for ref in refs.getReferencesTo(sym_addr):
                    fn = funcs.getFunctionContaining(ref.getFromAddress())
                    if fn:
                        key = fn.getName()
                        log("    <- {} @ {}".format(key, ref.getFromAddress()))
                        if key not in io_fns:
                            io_fns[key] = fn
        except Exception as e:
            log("  {} : {}".format(io_name, e))

    # Merge io functions into col_functions
    for k, v in io_fns.items():
        if k not in col_functions:
            col_functions[k] = v

    # ── 4. Dump raw bytes around known magic ─────────────────────
    log("\n[4] Raw bytes at known COL data address 005087ec...")
    try:
        base = toAddr(0x005087ec)
        # Show 128 bytes starting 16 before
        start = base.subtract(16)
        log("  Offset  | 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F | ASCII")
        log("  " + "-"*70)
        for row in range(9):
            row_addr = start.add(row * 16)
            hex_part  = ""
            ascii_part = ""
            for col in range(16):
                try:
                    b = mem.getByte(row_addr.add(col)) & 0xFF
                    hex_part   += "{:02X} ".format(b)
                    ascii_part += chr(b) if 32 <= b < 127 else '.'
                except Exception:
                    hex_part   += "?? "
                    ascii_part += '?'
            log("  {:08X} | {} | {}".format(
                row_addr.getOffset(), hex_part.strip(), ascii_part))

        # Find references to this area
        log("\n  References to 005087ec:")
        for ref in refs.getReferencesTo(base):
            fn = funcs.getFunctionContaining(ref.getFromAddress())
            fname = fn.getName() if fn else "?"
            log("    from {} in {}".format(ref.getFromAddress(), fname))
            if fn and fn.getName() not in col_functions:
                col_functions[fn.getName()] = fn

    except Exception as e:
        log("  bytes error: {}".format(e))

    # ── 5. Decompile all found functions ─────────────────────────
    log("\n[5] Decompiling {} found functions...".format(len(col_functions)))
    if col_functions:
        try:
            from ghidra.app.decompiler import DecompInterface
            from ghidra.util.task import ConsoleTaskMonitor
            decomp = DecompInterface()
            decomp.openProgram(program)
            monitor = ConsoleTaskMonitor()

            for fn_name, fn in list(col_functions.items())[:25]:
                log("\n" + "="*55)
                log("FUNCTION: {}  @ {}".format(fn_name, fn.getEntryPoint()))
                log("="*55)
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
    else:
        log("  No functions found — try manual search:")
        log("  In Ghidra Search > Memory, search for: 43 4F 4C 4C")
        log("  Then right-click result > References > Find References")

    # ── 6. Manual search hints ───────────────────────────────────
    log("\n[6] Manual analysis hints:")
    log("  The bytes at 005087ec show:")
    log("  43 4F 4C 4C = 'COLL' magic")
    log("  Next 4 bytes = file size field")
    log("  Next 24 bytes = model name (null padded)")
    log("  Next 2 bytes = model ID (uint16)")
    log("  Try: Search > Memory for 43 4F 4C 4C to find all COL data")
    log("  Then use References panel to find read/write functions")

    # ── Write report ─────────────────────────────────────────────
    report_path = os.path.expanduser("~/ghidra_col_report.txt")
    try:
        with open(report_path, 'w') as f:
            f.write('\n'.join(output_lines))
        log("\n[DONE] Report saved to: {}".format(report_path))
    except Exception as e:
        log("\n[ERROR] {}".format(e))

run()
