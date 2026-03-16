#!/bin/bash
# sync_app_settings.sh
# Keeps apps/utils/app_settings_system.py and the standalone
# App-Settings-System repo identical.
#
# Run from Img-Factory-1.6 root after any edit to app_settings_system.py:
#   ./sync_app_settings.sh "optional commit message"
#
# Keith's paths (adjust ASS_REPO if your clone is elsewhere):
IMGFACTORY_ROOT="$(cd "$(dirname "$0")" && pwd)"
IMGF_FILE="$IMGFACTORY_ROOT/apps/utils/app_settings_system.py"

# Try common locations for the standalone repo
for candidate in \
    "$HOME/Documents/GitHub/App-Settings-System" \
    "$HOME/G/App-Settings-System" \
    "$HOME/App-Settings-System" \
    "$(dirname "$IMGFACTORY_ROOT")/App-Settings-System"; do
    if [ -d "$candidate/.git" ]; then
        ASS_REPO="$candidate"
        break
    fi
done

if [ -z "$ASS_REPO" ]; then
    echo "ERROR: Could not find App-Settings-System repo."
    echo "Set ASS_REPO manually in this script."
    exit 1
fi

ASS_FILE="$ASS_REPO/app_settings_system.py"
MSG="${1:-Sync app_settings_system.py from IMG Factory $(date '+%Y-%m-%d')}"

echo "IMG Factory : $IMGF_FILE"
echo "Standalone  : $ASS_FILE"
echo ""

# Copy IMG Factory → standalone (IMG Factory is master)
cp "$IMGF_FILE" "$ASS_FILE"

# Sync themes both ways (IMG Factory themes are master)
if [ -d "$IMGFACTORY_ROOT/apps/themes" ] && [ -d "$ASS_REPO/themes" ]; then
    cp -r "$IMGFACTORY_ROOT/apps/themes/." "$ASS_REPO/themes/"
    echo "Themes synced."
fi

# Commit standalone repo
cd "$ASS_REPO"
git add -A
if git commit -m "$MSG"; then
    git push && echo "App-Settings-System pushed."
else
    echo "App-Settings-System: nothing to commit."
fi

# Commit IMG Factory repo
cd "$IMGFACTORY_ROOT"
git add apps/utils/app_settings_system.py apps/themes/ 2>/dev/null || true
if git commit -m "$MSG"; then
    git push && echo "IMG Factory pushed."
else
    echo "IMG Factory: nothing to commit."
fi

echo ""
echo "Both repos in sync."
