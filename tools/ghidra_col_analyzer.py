## ###
#
# @title COL Editor 2 Analyzer
# @author X-Seti
# @category Analysis.COL
# @keybinding 
# @menupath Tools.COL Analyzer
# @toolbar 
#
###

# COL Editor 2 - Structure Analyzer
# Place in ~/ghidra_scripts/ and run from Script Manager
# Results written to ~/ghidra_col_report.txt

from ghidra.app.script import GhidraScript
from ghidra.app.decompiler import DecompInterface
from ghidra.util.task import TaskMonitor
import os

output = []

def log(msg):
    print(str(msg))
    output.append(str(msg))

log("COL Editor 2 Analyzer starting...")

program  = currentProgram
listing  = program.getListing()
refmgr   = program.getReferenceManager()
funcmgr  = program.getFunctionManager()

# 1. Find magic bytes
log("\n--- Magic String Search ---")
col_magics = ["COLL", "COL2", "COL3", "COL4"]
found_funcs = {}

for magic in col_magics:
    addr = findBytes(None, magic.encode(), 20)
    while addr:
        log("  Found {} at {}".format(magic, addr))
        try:
            createLabel(addr, magic + "_magic", True)
        except:
            pass
        for ref in refmgr.getReferencesTo(addr):
            fn = funcmgr.getFunctionContaining(ref.getFromAddress())
            if fn and fn.getName() not in found_funcs:
                found_funcs[fn.getName()] = fn
                log("    -> referenced in: {} @ {}".format(fn.getName(), fn.getEntryPoint()))
        try:
            addr = findBytes(addr.add(1), magic.encode(), 20)
        except:
            break

# 2. Decompile
log("\n--- Decompiling {} functions ---".format(len(found_funcs)))
decomp = DecompInterface()
decomp.openProgram(program)

for name, fn in list(found_funcs.items())[:10]:
    log("\n=== {} @ {} ===".format(name, fn.getEntryPoint()))
    try:
        res = decomp.decompileFunction(fn, 30, TaskMonitor.DUMMY)
        if res.decompileCompleted():
            for line in res.getDecompiledFunction().getC().split('\n')[:60]:
                log(line)
    except Exception as e:
        log("  error: {}".format(e))

decomp.closeProgram()

# 3. fread references
log("\n--- fread / ReadFile callers ---")
for sym_name in ["fread", "ReadFile", "fopen", "CreateFileA"]:
    syms = list(getSymbols(sym_name, None))
    for sym in syms:
        log("  {} @ {}".format(sym_name, sym.getAddress()))
        for ref in refmgr.getReferencesTo(sym.getAddress()):
            fn = funcmgr.getFunctionContaining(ref.getFromAddress())
            if fn:
                log("    <- {}".format(fn.getName()))

# Save report
path = os.path.expanduser("~/ghidra_col_report.txt")
with open(path, "w") as f:
    f.write("\n".join(output))
log("\nSaved to " + path)
