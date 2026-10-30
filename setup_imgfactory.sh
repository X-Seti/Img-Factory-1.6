#!/usr/bin/env bash
# setup_imgfactory.sh — IMG Factory 1.6 universal Linux / WSL2 setup - Version: 2
# Supports: Ubuntu/Debian/Mint, Arch/Manjaro, Fedora, openSUSE
# Run once after cloning: chmod +x setup_imgfactory.sh && ./setup_imgfactory.sh
# Then launch with: python3 launch.py

set -e

echo ""
echo "=== IMG Factory 1.6 — Setup ==="
echo ""

#    WSL detection                                                               
IS_WSL=0
if grep -qi microsoft /proc/version 2>/dev/null; then
    IS_WSL=1
    echo "[i] WSL2 environment detected (Windows Subsystem for Linux)"
    echo "[i] GUI uses WSLg (Windows 11) or requires VcXsrv (Windows 10)"
    echo ""
fi

#    Distro detection                                                            
if [ -f /etc/os-release ]; then
    . /etc/os-release
else
    echo "[!] Cannot detect distro — /etc/os-release not found"
    exit 1
fi

DISTRO=$ID
echo "[+] Distro: $DISTRO"

#    System packages                                                             
install_debian() {
    sudo apt update
    sudo apt install -y \
        python3 python3-pip python3-venv git build-essential \
        qt6-base-dev qt6-tools-dev qt6-tools-dev-tools \
        libxcb-cursor0 libxcb-xinerama0 libxkbcommon-x11-0 \
        libxcb-render0 libxcb-shape0 libxcb-xfixes0 \
        libxcb-randr0 libxcb-glx0 libgl1 libegl1 libx11-xcb1 \
        portaudio19-dev liblzo2-dev libcairo2 ffmpeg \
        gstreamer1.0-pipewire gstreamer1.0-plugins-good gstreamer1.0-plugins-bad \
        gstreamer1.0-plugins-ugly gstreamer1.0-tools
}

install_arch() {
    sudo pacman -Sy --noconfirm \
        python python-pip git base-devel \
        qt6-base qt6-tools \
        libxcb xcb-util-cursor xcb-util xcb-util-image \
        xcb-util-keysyms xcb-util-renderutil xcb-util-wm \
        portaudio lzo cairo ffmpeg \
        gst-plugin-pipewire gst-plugins-good gst-plugins-bad gst-plugins-ugly gstreamer jxrlib
}

install_fedora() {
    sudo dnf install -y \
        python3 python3-pip git @development-tools \
        qt6-qtbase qt6-qttools \
        xcb-util-cursor xcb-util-wm \
        libxkbcommon-x11 mesa-libGL mesa-libEGL \
        portaudio-devel lzo-devel cairo ffmpeg-free \
        pipewire-gstreamer gstreamer1-plugins-good gstreamer1-plugins-bad-free gstreamer1
}

install_opensuse() {
    sudo zypper install -y \
        python3 python3-pip git gcc make \
        libQt6Core6 libQt6Gui6 libQt6Widgets6 libqt6-qttools \
        libxcb-cursor0 libxkbcommon-x11-0 Mesa-libGL1 Mesa-libEGL1 \
        portaudio-devel lzo-devel libcairo2 ffmpeg \
        gstreamer-plugin-pipewire gstreamer-plugins-good gstreamer-plugins-bad gstreamer-utils
}

case "$DISTRO" in
    armbian|ubuntu|debian|linuxmint|pop)  install_debian ;;
    arch|garuda|garujaro|manjaro|endeavouros)     install_arch ;;
    fedora)                        install_fedora ;;
    opensuse*|suse)                install_opensuse ;;
    *)
        echo "[!] Unsupported distro: $DISTRO"
        echo "    Install Qt6, Python3 and XCB libraries manually, then re-run."
        exit 1 ;;
esac

#    Python virtual environment                                                  
echo ""
echo "[+] Creating Python virtual environment (.venv)..."
python3 -m venv .venv
source .venv/bin/activate

echo "[+] Installing Python dependencies..."
pip install --upgrade pip
echo "[+] Installing from requirements.txt..."
pip install -r requirements.txt

#    WSL display setup                                                           
if [ "$IS_WSL" = "1" ]; then
    echo ""
    echo "[i] Configuring WSL display..."
    [ -z "$DISPLAY" ] && export DISPLAY=:0
    export QT_QPA_PLATFORM=xcb
    export LIBGL_ALWAYS_INDIRECT=1
    echo "    DISPLAY=$DISPLAY  QT_QPA_PLATFORM=xcb"
    echo ""
    echo "    Windows 11: WSLg handles display automatically."
    echo "    Windows 10: install VcXsrv, launch with 'Disable access control',"
    echo "    then set DISPLAY=:0.0 in your terminal before running."
fi

#    Done                                                                        
echo ""
echo "=== Setup complete ==="
echo ""
echo "To launch IMG Factory:"
echo "    source .venv/bin/activate"
echo "    python3 launch.py 1"
echo ""
echo "Or pick any tool from the menu:"
echo "    python3 launch.py"
echo ""
echo "Or just run IMG Factory now? (Ctrl+C to skip)"
sleep 2
python3 launch.py 1
