#!/usr/bin/env bash
# this belongs in apps/components/DP5_Workshop/tools/kde_info_properties/install.sh - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - build + install the Dolphin Properties extractor
# Needs: cmake, extra-cmake-modules, qt6-base, kfilemetadata (Garuda/Arch: sudo pacman -S cmake extra-cmake-modules kfilemetadata)
# Run kde_info_thumbnailer/install.sh first (it adds the application/x-amiga-icon MIME type).
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
build="$here/build"
cmake -S "$here" -B "$build" -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr
cmake --build "$build" -j"$(nproc)"
sudo cmake --install "$build"
echo "Installed. Restart Dolphin; see Properties - Details on any .info file."
