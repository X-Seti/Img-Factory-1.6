#!/bin/bash
#this belongs in root /unix_launcher.sh - version 2
# IMG Factory 1.5 - Modern Unix Launcher
# X-Seti - July03 2025

set -euo pipefail  # Strict error handling

# Color definitions
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

print_header() {
    echo -e "${CYAN}================================================================${NC}"
    echo -e "${CYAN}  IMG Factory 1.5 - Unix Launcher${NC}"
    echo -e "${CYAN}  Advanced IMG Archive Management Tool${NC}"
    echo -e "${CYAN}================================================================${NC}"
    echo ""
}

check_command() {
    command -v "$1" >/dev/null 2>&1
}

find_python() {
    local python_cmd=""
    
    # Check for python3 first, then python
    if check_command python3; then
        python_cmd="python3"
    elif check_command python; then
        python_cmd="python"
    else
        echo -e "${RED}ERROR: Python is not installed or not in PATH${NC}"
        echo "Please install Python 3.8 or newer"
        echo ""
        echo "Ubuntu/Debian: sudo apt install python3 python3-pip"
        echo "CentOS/RHEL:   sudo yum install python3 python3-pip"
        echo "macOS:         brew install python3"
        echo "Arch Linux:    sudo pacman -S python python-pip"
        return 1
    fi
    
    echo "$python_cmd"
}

check_python_version() {
    local python_cmd="$1"
    
    echo -e "${BLUE}Checking Python version...${NC}"
    
    # Get version
    local version=$($python_cmd --version 2>&1 | awk '{print $2}')
    echo "   Found: Python $version"
    
    # Check if version is 3.8 or higher
    local major=$(echo "$version" | cut -d. -f1)
    local minor=$(echo "$version" | cut -d. -f2)
    
    if [[ $major -ge 3 && $minor -ge 8 ]]; then
        echo -e "   ${GREEN}✓ Python version OK${NC}"
        return 0
    else
        echo -e "   ${RED}✗ Python 3.8+ required, found $version${NC}"
        echo "Please upgrade Python to 3.8 or newer"
        return 1
    fi
}

check_project_structure() {
    echo -e "${BLUE}Checking project structure...${NC}"
    
    local required_files=("imgfactory.py")
    local optional_files=("launch_imgfactory.py" "methods.img_core_classes.py" "gui/gui_layout.py")
    local directories=("components" "gui")
    
    local missing_required=()
    
    # Check required files
    for file in "${required_files[@]}"; do
        if [[ -f "$file" ]]; then
            echo -e "   ${GREEN}✓ $file${NC}"
        else
            echo -e "   ${RED}✗ $file (required)${NC}"
            missing_required+=("$file")
        fi
    done
    
    # Check optional files
    for file in "${optional_files[@]}"; do
        if [[ -f "$file" ]]; then
            echo -e "   ${GREEN}✓ $file${NC}"
        else
            echo -e "   ${YELLOW}⚠ $file (optional)${NC}"
        fi
    done
    
    # Check directories
    for dir in "${directories[@]}"; do
        if [[ -d "$dir" ]]; then
            local count=$(find "$dir" -name "*.py" 2>/dev/null | wc -l)
            echo -e "   ${GREEN}✓ $dir/ ($count Python files)${NC}"
        else
            echo -e "   ${YELLOW}⚠ $dir/ (missing)${NC}"
        fi
    done
    
    if [[ ${#missing_required[@]} -eq 0 ]]; then
        echo -e "   ${GREEN}✓ Project structure OK${NC}"
        return 0
    else
        echo -e "   ${RED}✗ Missing required files: ${missing_required[*]}${NC}"
        return 1
    fi
}

check_pyqt6() {
    local python_cmd="$1"
    
    echo -e "${BLUE}Checking PyQt6 installation...${NC}"
    
    if $python_cmd -c "import PyQt6; print('PyQt6 version:', PyQt6.QtCore.PYQT_VERSION_STR)" 2>/dev/null; then
        echo -e "   ${GREEN}✓ PyQt6 is available${NC}"
        return 0
    else
        echo -e "   ${RED}✗ PyQt6 is not installed${NC}"
        return 1
    fi
}

install_pyqt6() {
    local python_cmd="$1"
    
    echo -e "${YELLOW}Attempting to install PyQt6...${NC}"
    
    # Try pip3 first, then pip
    local pip_cmd=""
    if check_command pip3; then
        pip_cmd="pip3"
    elif check_command pip; then
        pip_cmd="pip"
    else
        echo -e "${RED}ERROR: pip is not available${NC}"
        echo "Please install pip manually"
        return 1
    fi
    
    echo "   Using: $pip_cmd install PyQt6"
    
    if $pip_cmd install PyQt6; then
        echo -e "   ${GREEN}✓ PyQt6 installed successfully${NC}"
        return 0
    else
        echo -e "   ${RED}✗ Failed to install PyQt6${NC}"
        echo ""
        echo "Manual installation options:"
        echo "  Ubuntu/Debian: sudo apt install python3-pyqt6"
        echo "  CentOS/RHEL:   sudo yum install python3-pyqt6"
        echo "  macOS:         pip3 install PyQt6"
        echo "  Arch Linux:    sudo pacman -S python-pyqt6"
        return 1
    fi
}

launch_application() {
    local python_cmd="$1"
    
    echo -e "${BLUE}Starting IMG Factory 1.5...${NC}"
    echo "================================================================"
    echo ""
    
    # Try modern launcher first
    if [[ -f "launch_imgfactory.py" ]]; then
        echo -e "${BLUE}Using modern launcher...${NC}"
        $python_cmd launch_imgfactory.py
        return $?
    else
        echo -e "${BLUE}Using direct launch...${NC}"
        $python_cmd imgfactory.py
        return $?
    fi
}

show_help() {
    echo ""
    echo -e "${PURPLE}Troubleshooting Help:${NC}"
    echo "─────────────────────────────────────────"
    echo "If IMG Factory won't start:"
    echo "1. Ensure Python 3.8+ is installed"
    echo "2. Install PyQt6: pip3 install PyQt6"
    echo "3. Check all files are in the correct locations"
    echo "4. Try: pip3 install --upgrade PyQt6"
    echo "5. Run with verbose output: python3 -v imgfactory.py"
    echo ""
    echo "Distribution-specific PyQt6 installation:"
    echo "• Ubuntu/Debian: sudo apt install python3-pyqt6"
    echo "• CentOS/RHEL:   sudo yum install python3-pyqt6"
    echo "• Arch Linux:    sudo pacman -S python-pyqt6"
    echo "• macOS:         brew install pyqt6"
}

main() {
    print_header
    
    # Step 1: Find Python
    local python_cmd
    if ! python_cmd=$(find_python); then
        show_help
        return 1
    fi
    
    # Step 2: Check Python version
    if ! check_python_version "$python_cmd"; then
        show_help
        return 1
    fi
    
    echo ""
    
    # Step 3: Check project structure
    if ! check_project_structure; then
        show_help
        return 1
    fi
    
    echo ""
    
    # Step 4: Check PyQt6
    if ! check_pyqt6 "$python_cmd"; then
        if ! install_pyqt6 "$python_cmd"; then
            show_help
            return 1
        fi
    fi
    
    echo ""
    echo "================================================================"
    
    # Step 5: Launch application
    local exit_code
    if launch_application "$python_cmd"; then
        exit_code=$?
        echo ""
        echo "================================================================"
        echo -e "${GREEN}✓ IMG Factory closed normally${NC}"
    else
        exit_code=$?
        echo ""
        echo "================================================================"
        
        if [[ $exit_code -eq 130 ]]; then
            echo -e "${YELLOW}⚠ Cancelled by user${NC}"
        else
            echo -e "${RED}✗ Application exited with error code: $exit_code${NC}"
            show_help
        fi
    fi
    
    return $exit_code
}

# Make script executable
chmod +x "$0" 2>/dev/null || true

# Handle signals gracefully
trap 'echo -e "\n${YELLOW}⚠ Interrupted by user${NC}"; exit 130' INT TERM

# Run main function
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
    exit $?
fi
