#!/usr/bin/env bash
set -e

echo "[1] System deps..."
sudo apt update
sudo apt install -y \
python3-full python3-venv git build-essential cmake \
qt6-base-dev qt6-tools-dev qt6-tools-dev-tools \
libxcb-cursor0 libxcb-xinerama0 libxkbcommon-x11-0 \
libxcb-render0 libxcb-shape0 libxcb-xfixes0 \
libxcb-randr0 libxcb-glx0 libgl1 libegl1 libx11-xcb1

echo "[2] Python venv..."
python3 -m venv .venv
source .venv/bin/activate

echo "[3] Python deps..."
pip install --upgrade pip
pip install PyQt6 pyinstaller

if [ -f requirements.txt ]; then
    pip install -r requirements.txt
fi

echo "[4] Done. Activate with:"
echo "source .venv/bin/activate"
