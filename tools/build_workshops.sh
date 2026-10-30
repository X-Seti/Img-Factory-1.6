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

export WORKSHOPS="${1:-asset,breakable,capture,col,dat,disc,dp5,handling,hex,map,model, modelv,radar,path,ped,radar,terrain,retro,timecyc,txd,vehicle,water}"
python3 -m PyInstaller --noconfirm workshops.spec

declare -A NAMES=(
        [asset]=AssetWorkshop
        [breakable]=BreakableEditor
        [capture]=CaptureWorkshop
        [col]=ColEditor
        [dat]=DatBrowser
        [disc]=DiscWorkshop
        [dp5]=Dp5Workshop
        [handling]=HandlingEditor
        [hex]=HexEditor
        [map]=MapWorkshop
        [model]=ModelEditor
        [modelv]=ModelViewer
        [path]=PathWorkshop
        [ped]=PedWorkshop
        [radar]=RadarWorkshop
        [terrain]=TerrainWorkshop
        [retro]=RetroGTAWorkshop
        [timecyc]=TimeCycEditor
        [txd]=TxdEditor
        [vehicle]=VehicleWorkshop
        [water]=WaterEditor
        )
for key in ${WORKSHOPS//,/ }; do
    out="dist/${NAMES[$key]}"
    cp -r icons "$out/"                         # ribbon community icons
    echo "Built $out"
done
