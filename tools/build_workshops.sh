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

export WORKSHOPS="${1:-apps, asset,breakable,capture,col,dat,disc,dp5,handling,hex,map,model, modelv,radar,path,ped,radar,terrain,retro,timecyc,txd,vehicle,water}"
python3 -m PyInstaller --noconfirm workshops.spec

declare -A NAMES=(
        [apps]=utils
        [asset]=Asset_Workshop
        [breakable]=Breakable_Editor
        [capture]=Capture_Workshop
        [col]=Col_Editor
        [dat]=Dat_Browser
        [disc]=Disc_Workshop
        [dp5]=DP5_Workshop
        [handling]=Handling_Editor
        [hex]=Hex_Editor
        [map]=Map_Editor
        [model]=Model_Editor
        [modelv]=Model_Viewer
        [path]=Path_Workshop
        [ped]=Ped_Workshop
        [radar]=Radar_Editor
        [terrain]=Terrain_Workshop
        [retro]=Retro_GTA_Workshop
        [timecyc]=TimeCyc_Editor
        [txd]=Txd_Editor
        [vehicle]=Vehicle_Workshop
        [water]=Water_Editor
        )
for key in ${WORKSHOPS//,/ }; do
    out="./dist/${NAMES[$key]}"
    cp -r icons "$out/" # ribbon community icons
    echo "./Build/$out"
done
