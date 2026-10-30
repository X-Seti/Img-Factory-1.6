#!/bin/sh
# Installs Img Factory 1.6 Full for this user: ~/.local/opt/Img_Factory_Full, menu entry, ~/.local/bin/Img_Factory_Full
set -e
HERE="$(cd "$(dirname "$0")" && pwd)"
DEST="$HOME/.local/opt/Img_Factory_Full"
ICONS="$HOME/.local/share/icons/hicolor/256x256/apps"
APPS="$HOME/.local/share/applications"
mkdir -p "$DEST" "$HOME/.local/bin" "$ICONS" "$APPS"
[ "$HERE" = "$DEST" ] || cp -a "$HERE/." "$DEST/"
ln -sf "$DEST/Img_Factory_Full" "$HOME/.local/bin/Img_Factory_Full"
[ -f "$DEST/Img_Factory_Full.png" ] && cp "$DEST/Img_Factory_Full.png" "$ICONS/Img_Factory_Full.png"
cat > "$APPS/Img_Factory_Full.desktop" <<EOF
[Desktop Entry]
Type=Application
Name=Img Factory 1.6 Full
Exec="$DEST/Img_Factory_Full.exe" %f
Icon=Img_Factory_Full.png
Terminal=false
Categories=Graphics;Development;
EOF
command -v update-desktop-database >/dev/null && update-desktop-database "$APPS"
echo "Img Factory 1.6 Full installed in $DEST - start it from the menu or run: Img_Factory_Full"
