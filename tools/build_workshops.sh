#!/usr/bin/env bash
#this belongs in tools/build_workshops.sh - Version: 1
# X-Seti - October 13 2026 - IMG Factory 1.6 - Build standalone workshops
#
# Usage: tools/build_workshops.sh [map,model,radar,water]
# Output: dist/<Name>Workshop/ (run the exe inside; icons/ copied beside it)
set -e

cd "$(dirname "$0")/.."
python3 -m PyInstaller --version >/dev/null 2>&1 || {
    echo "PyInstaller missing: pip install pyinstaller"; exit 1; }

export WORKSHOPS="${1:-map,model,radar,water}"
python3 -m PyInstaller --noconfirm workshops.spec

declare -A NAMES=([map]=MapWorkshop [model]=ModelWorkshop [radar]=RadarWorkshop [water]=WaterWorkshop)
for key in ${WORKSHOPS//,/ }; do
    out="dist/${NAMES[$key]}"
    cp -r icons "$out/"                         # ribbon community icons
    echo "Built $out"
done
