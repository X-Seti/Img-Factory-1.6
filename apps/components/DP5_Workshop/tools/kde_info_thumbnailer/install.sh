#!/usr/bin/env bash
# this belongs in apps/components/DP5_Workshop/tools/kde_info_thumbnailer/install.sh - Version: 1
# X-Seti - October 27 2026 - IMG Factory 1.6 - per-user install of the Amiga .info thumbnailer
# Needs KDE Frameworks 6.6+ (KIO thumbnailer files), python3, python-pillow, python-numpy.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
data="${XDG_DATA_HOME:-$HOME/.local/share}"
dest="$data/dp5-amiga-info"
mkdir -p "$dest" "$data/thumbnailers" "$data/mime/packages" "$data/kio/servicemenus"
cp "$here/amiga_info_thumb.py" "$here/../../depends/amiga_icon.py" "$dest/"
chmod +x "$dest/amiga_info_thumb.py"
cp "$here/amiga-icon.xml" "$data/mime/packages/"
update-mime-database "$data/mime"
sed "s|@DIR@|$dest|g" "$here/amiga-info.thumbnailer.in" | grep -v '^#' > "$data/thumbnailers/amiga-info.thumbnailer"
sed "s|@DIR@|$dest|g" "$here/amiga-info-details.desktop.in" | grep -v '^#' > "$data/kio/servicemenus/amiga-info-details.desktop"
chmod +x "$data/kio/servicemenus/amiga-info-details.desktop"
echo "Installed. In Dolphin enable previews (Configure - Interface - Previews), then restart Dolphin."
