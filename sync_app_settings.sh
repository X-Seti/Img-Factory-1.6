#!/bin/bash
# Syncs apps/utils/app_settings_system.py to the standalone App-Settings-System repo
# Run from the Img-Factory-1.6 root after any changes to app_settings_system.py

set -e

IMGFACTORY_FILE="apps/utils/app_settings_system.py"
APPSETTINGS_REPO="/home/x2/Documents/GitHub/App-Settings-System"
APPSETTINGS_FILE="$APPSETTINGS_REPO/app_settings_system.py"

if [ ! -f "$IMGFACTORY_FILE" ]; then
    echo "ERROR: $IMGFACTORY_FILE not found"
    exit 1
fi

if [ ! -d "$APPSETTINGS_REPO" ]; then
    echo "ERROR: App-Settings-System repo not found at $APPSETTINGS_REPO"
    exit 1
fi

echo "Syncing app_settings_system.py → App-Settings-System repo..."
cp "$IMGFACTORY_FILE" "$APPSETTINGS_FILE"

# Also sync themes
cp -r apps/themes/* "$APPSETTINGS_REPO/themes/" 2>/dev/null || true

cd "$APPSETTINGS_REPO"
git add -A
git commit -m "Sync from IMG Factory 1.6: $(date '+%Y-%m-%d')" || echo "Nothing to commit"
git push
echo "Done."
