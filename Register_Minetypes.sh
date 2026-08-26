```bash
#!/usr/bin/env bash
#
# Img Factory - MIME Association Manager
# register_mime.sh
#
# Registers Img Factory component applications as handlers for file types.
#
# Examples:
#   ./register_mime.sh install txd Txd_Editor
#   ./register_mime.sh install col Col_Editor
#   ./register_mime.sh install dff Model_Editor
#
#   ./register_mime.sh remove txd
#   ./register_mime.sh list
#
# The component directory is expected to contain the main application:
#
#   apps/components/Txd_Editor/txd_workshop.py
#   apps/components/Col_Editor/col_workshop.py
#   apps/components/Model_Editor/model_workshop.py
#
# "depends" directories are ignored.
#

set -u

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMPONENT_ROOT="$PROJECT_ROOT/apps/components"

LOCAL_SHARE="$HOME/.local/share"
APPLICATION_DIR="$LOCAL_SHARE/applications"
MIME_DIR="$LOCAL_SHARE/mime"
MIME_PACKAGE_DIR="$MIME_DIR/packages"

mkdir -p "$APPLICATION_DIR"
mkdir -p "$MIME_PACKAGE_DIR"

# ---------------------------------------------------------------------------
# Colours
# ---------------------------------------------------------------------------

if [[ -t 1 ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    BLUE='\033[0;34m'
    CYAN='\033[0;36m'
    RESET='\033[0m'
else
    RED=''
    GREEN=''
    YELLOW=''
    BLUE=''
    CYAN=''
    RESET=''
fi

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

info()
{
    echo -e "${BLUE}[INFO]${RESET} $*"
}

success()
{
    echo -e "${GREEN}[OK]${RESET} $*"
}

warning()
{
    echo -e "${YELLOW}[WARNING]${RESET} $*"
}

error()
{
    echo -e "${RED}[ERROR]${RESET} $*" >&2
}

usage()
{
    cat <<EOF

Img Factory MIME Association Manager

Usage:

  $0 install <extension> <component>

      Register an Img Factory component as the default application
      for a file extension.

  $0 remove <extension>

      Remove an Img Factory MIME association.

  $0 list

      List currently registered Img Factory associations.

  $0 help

      Show this help.

Examples:

  $0 install txd Txd_Editor
  $0 install col Col_Editor
  $0 install dff Model_Editor
  $0 install ipl Ipl_Editor

  $0 remove txd

  $0 list

Component examples:

  Txd_Editor
  Col_Editor
  Model_Editor
  Ipl_Editor
  Radar_Editor
  Handling_Editor
  Map_Editor
  Hex_Editor

The script expects the main Python application to be directly inside
the component directory and ignores "depends" directories.

EOF
}

# ---------------------------------------------------------------------------
# Validate extension
# ---------------------------------------------------------------------------

normalise_extension()
{
    local ext="$1"

    ext="${ext#.}"
    ext="${ext,,}"

    echo "$ext"
}

# ---------------------------------------------------------------------------
# Find component
# ---------------------------------------------------------------------------

find_component()
{
    local component="$1"
    local component_dir="$COMPONENT_ROOT/$component"

    if [[ ! -d "$component_dir" ]]; then
        error "Component does not exist:"
        error "  $component_dir"
        return 1
    fi

    echo "$component_dir"
}

# ---------------------------------------------------------------------------
# Find main Python application
# ---------------------------------------------------------------------------
#
# Expected structure:
#
#   Component/
#       component_name.py
#       depends/
#           ...
#
# We deliberately ignore:
#
#   depends/
#   __pycache__/
#   files beginning with "__"
#
# If there is only one suitable Python file, use it.
#
# If there are multiple, try to identify the obvious "workshop/editor"
# application before asking the user.
#

find_main_python()
{
    local component_dir="$1"

    local candidates=()

    while IFS= read -r -d '' file; do
        candidates+=("$file")
    done < <(
        find "$component_dir" \
            -maxdepth 1 \
            -type f \
            -name "*.py" \
            ! -name "__init__.py" \
            -print0
    )

    # No Python files
    if [[ ${#candidates[@]} -eq 0 ]]; then
        error "No Python application found in:"
        error "  $component_dir"
        return 1
    fi

    # Exactly one Python application
    if [[ ${#candidates[@]} -eq 1 ]]; then
        echo "${candidates[0]}"
        return 0
    fi

    # Prefer files containing common application names.
    local preferred=""

    for file in "${candidates[@]}"; do
        local base
        base="$(basename "$file" .py)"

        case "$base" in
            *_workshop|*_editor|*_browser|*_creator|*_viewer)
                preferred="$file"
                break
                ;;
        esac
    done

    if [[ -n "$preferred" ]]; then
        echo "$preferred"
        return 0
    fi

    error "Multiple Python files found in:"
    error "  $component_dir"
    echo

    local i=1

    for file in "${candidates[@]}"; do
        echo "  $i) $(basename "$file")"
        ((i++))
    done

    echo
    read -rp "Select the main application [1-${#candidates[@]}]: " choice

    if ! [[ "$choice" =~ ^[0-9]+$ ]]; then
        error "Invalid selection."
        return 1
    fi

    if (( choice < 1 || choice > ${#candidates[@]} )); then
        error "Invalid selection."
        return 1
    fi

    echo "${candidates[$((choice-1))]}"
}

# ---------------------------------------------------------------------------
# Create safe desktop/application ID
# ---------------------------------------------------------------------------

make_app_id()
{
    local ext="$1"

    echo "imgfactory-$ext"
}

# ---------------------------------------------------------------------------
# Create MIME type name
# ---------------------------------------------------------------------------

make_mime_type()
{
    local ext="$1"

    echo "application/x-imgfactory-$ext"
}

# ---------------------------------------------------------------------------
# Generate human-readable application name
# ---------------------------------------------------------------------------

make_app_name()
{
    local component="$1"
    local ext="$2"

    echo "Img Factory - ${component} (${ext^^})"
}

# ---------------------------------------------------------------------------
# MIME XML
# ---------------------------------------------------------------------------

create_mime_xml()
{
    local ext="$1"
    local component="$2"

    local mime_type
    mime_type="$(make_mime_type "$ext")"

    local mime_file="$MIME_PACKAGE_DIR/application-x-imgfactory-$ext.xml"

    cat > "$mime_file" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
    <mime-type type="$mime_type">
        <comment>Img Factory ${ext^^} file</comment>
        <glob pattern="*.$ext"/>
    </mime-type>
</mime-info>
EOF

    echo "$mime_file"
}

# ---------------------------------------------------------------------------
# Desktop file
# ---------------------------------------------------------------------------

create_desktop_file()
{
    local ext="$1"
    local component="$2"
    local python_file="$3"

    local app_id
    app_id="$(make_app_id "$ext")"

    local mime_type
    mime_type="$(make_mime_type "$ext")"

    local app_name
    app_name="$(make_app_name "$component" "$ext")"

    local desktop_file="$APPLICATION_DIR/$app_id.desktop"

    # Absolute paths are intentional.
    #
    # We do NOT use:
    #
    #   ~/GitHub/...
    #
    # because Exec= entries in desktop files have special parsing rules
    # and KDE can reject malformed executable paths.
    #
    # We explicitly invoke Python 3.
    #
    local python_bin

    python_bin="$(command -v python3 || true)"

    if [[ -z "$python_bin" ]]; then
        error "python3 could not be found."
        return 1
    fi

    cat > "$desktop_file" <<EOF
[Desktop Entry]
Type=Application
Name=$app_name
GenericName=Img Factory ${ext^^} Editor
Comment=Open ${ext^^} files with Img Factory
Exec=$python_bin $python_file %f
Icon=application-x-executable
Terminal=false
Categories=Graphics;Development;Utility;
MimeType=$mime_type;
StartupNotify=true
NoDisplay=false
EOF

    chmod 644 "$desktop_file"

    echo "$desktop_file"
}

# ---------------------------------------------------------------------------
# Set default MIME application
# ---------------------------------------------------------------------------

set_default_application()
{
    local ext="$1"

    local mime_type
    mime_type="$(make_mime_type "$ext")"

    local app_id
    app_id="$(make_app_id "$ext").desktop"

    xdg-mime default "$app_id" "$mime_type"

    # Also explicitly write the association into mimeapps.list.
    #
    # KDE uses this file for user-level associations.
    #
    mkdir -p "$HOME/.config"

    local mimeapps="$HOME/.config/mimeapps.list"

    if [[ ! -f "$mimeapps" ]]; then
        cat > "$mimeapps" <<EOF
[Default Applications]
EOF
    fi

    # Remove an existing association for this MIME type.
    sed -i "\|^$mime_type=|d" "$mimeapps"

    # Ensure [Default Applications] exists.
    if ! grep -q '^\[Default Applications\]' "$mimeapps"; then
        {
            echo
            echo "[Default Applications]"
        } >> "$mimeapps"
    fi

    # Insert the association immediately after the section header.
    sed -i "/^\[Default Applications\]$/a $mime_type=$app_id" "$mimeapps"
}

# ---------------------------------------------------------------------------
# Refresh MIME and desktop databases
# ---------------------------------------------------------------------------

refresh_databases()
{
    info "Updating MIME database..."

    if command -v update-mime-database >/dev/null 2>&1; then
        update-mime-database "$MIME_DIR" >/dev/null 2>&1 || \
            warning "update-mime-database reported an error."
    else
        warning "update-mime-database is not installed."
    fi

    info "Updating desktop database..."

    if command -v update-desktop-database >/dev/null 2>&1; then
        update-desktop-database "$APPLICATION_DIR" >/dev/null 2>&1 || \
            warning "update-desktop-database reported an error."
    else
        warning "update-desktop-database is not installed."
    fi

    # KDE normally notices the changes automatically.
    # kbuildsycoca6 is used by Plasma 6.
    #
    # Do not fail if it is unavailable.
    if command -v kbuildsycoca6 >/dev/null 2>&1; then
        kbuildsycoca6 >/dev/null 2>&1 || true
    elif command -v kbuildsycoca5 >/dev/null 2>&1; then
        kbuildsycoca5 >/dev/null 2>&1 || true
    fi
}

# ---------------------------------------------------------------------------
# Install association
# ---------------------------------------------------------------------------

install_association()
{
    local ext="$1"
    local component="$2"

    ext="$(normalise_extension "$ext")"

    if [[ -z "$ext" ]]; then
        error "File extension cannot be empty."
        return 1
    fi

    if [[ ! "$ext" =~ ^[a-zA-Z0-9][a-zA-Z0-9_-]*$ ]]; then
        error "Invalid file extension: $ext"
        error "Use something like: txd, dff, col, ipl"
        return 1
    fi

    info "Img Factory MIME registration"
    echo

    info "Extension: .$ext"
    info "Component: $component"

    local component_dir

    component_dir="$(find_component "$component")" || return 1

    info "Component directory:"
    echo "  $component_dir"

    local python_file

    python_file="$(find_main_python "$component_dir")" || return 1

    info "Application:"
    echo "  $python_file"

    # Resolve the path completely.
    python_file="$(realpath "$python_file")"

    echo
    info "Creating MIME definition..."

    local mime_file
    mime_file="$(create_mime_xml "$ext" "$component")"

    success "$mime_file"

    info "Creating KDE application launcher..."

    local desktop_file
    desktop_file="$(create_desktop_file "$ext" "$component" "$python_file")" || \
        return 1

    success "$desktop_file"

    info "Setting default application..."

    set_default_application "$ext"

    success "Default application set."

    refresh_databases

    echo
    success "Registered .$ext"
    echo
    echo "  File type : $ext"
    echo "  MIME type : $(make_mime_type "$ext")"
    echo "  Component : $component"
    echo "  Program   : $python_file"
    echo "  Desktop   : $desktop_file"
    echo
    echo "KDE should now offer:"
    echo
    echo "  Img Factory - ${component} (${ext^^})"
    echo
}

# ---------------------------------------------------------------------------
# Remove association
# ---------------------------------------------------------------------------

remove_association()
{
    local ext="$1"

    ext="$(normalise_extension "$ext")"

    local mime_type
    mime_type="$(make_mime_type "$ext")"

    local app_id
    app_id="$(make_app_id "$ext").desktop"

    local mime_file="$MIME_PACKAGE_DIR/application-x-imgfactory-$ext.xml"
    local desktop_file="$APPLICATION_DIR/$app_id"

    info "Removing Img Factory .$ext association..."

    # Remove MIME definition
    if [[ -f "$mime_file" ]]; then
        rm -f "$mime_file"
        success "Removed MIME definition."
    else
        warning "MIME definition not found."
    fi

    # Remove desktop launcher
    if [[ -f "$desktop_file" ]]; then
        rm -f "$desktop_file"
        success "Removed desktop launcher."
    else
        warning "Desktop launcher not found."
    fi

    # Remove default association from mimeapps.list
    local mimeapps="$HOME/.config/mimeapps.list"

    if [[ -f "$mimeapps" ]]; then
        sed -i "\|^$mime_type=|d" "$mimeapps"
    fi

    refresh_databases

    echo
    success "Removed .$ext association."
}

# ---------------------------------------------------------------------------
# List associations
# ---------------------------------------------------------------------------

list_associations()
{
    echo
    echo "Img Factory File Associations"
    echo "============================="
    echo

    local found=0

    shopt -s nullglob

    for desktop_file in "$APPLICATION_DIR"/imgfactory-*.desktop; do

        found=1

        local filename
        filename="$(basename "$desktop_file")"

        local ext="${filename#imgfactory-}"
        ext="${ext%.desktop}"

        local name
        name="$(grep '^Name=' "$desktop_file" | head -n1 | cut -d= -f2-)"

        local exec_line
        exec_line="$(grep '^Exec=' "$desktop_file" | head -n1 | cut -d= -f2-)"

        echo "  .$ext"
        echo "      Application : $name"
        echo "      Exec        : $exec_line"
        echo
    done

    shopt -u nullglob

    if (( found == 0 )); then
        echo "  No Img Factory MIME associations found."
        echo
    fi
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

case "${1:-}" in

    install)
        if [[ $# -ne 3 ]]; then
            error "Install requires:"
            error "  $0 install <extension> <component>"
            echo
            usage
            exit 1
        fi

        install_association "$2" "$3"
        ;;

    remove)
        if [[ $# -ne 2 ]]; then
            error "Remove requires:"
            error "  $0 remove <extension>"
            exit 1
        fi

        remove_association "$2"
        ;;

    list)
        list_associations
        ;;

    help|-h|--help)
        usage
        ;;

    "")
        usage
        ;;

    *)
        error "Unknown command: $1"
        echo
        usage
        exit 1
        ;;

esac
```
