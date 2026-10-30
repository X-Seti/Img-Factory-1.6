#!/bin/sh
# Removes Img Factory 1.6 Full installed by install.sh
rm -f "$HOME/.local/bin/Img_Factory_Full" "$HOME/.local/share/applications/Img_Factory_Full.desktop" \
      "$HOME/.local/share/icons/hicolor/256x256/apps/Img_Factory_Full.png"
rm -rf "$HOME/.local/opt/Img_Factory_Full"
echo "Img Factory 1.6 Full removed"
