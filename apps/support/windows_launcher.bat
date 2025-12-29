@echo off
REM #this belongs in root /windows_launcher.bat - version 2
REM IMG Factory 1.5 - Modern Windows Launcher
REM X-Seti - July03 2025

setlocal enabledelayedexpansion

title IMG Factory 1.5 Launcher

echo.
echo ================================================================
echo   IMG Factory 1.5 - Windows Launcher
echo   Advanced IMG Archive Management Tool
echo ================================================================
echo.

REM Color definitions (if available)
if exist "%windir%\system32\choice.exe" (
    set "GREEN=[92m"
    set "RED=[91m"
    set "YELLOW=[93m"
    set "BLUE=[94m"
    set "NC=[0m"
) else (
    set "GREEN="
    set "RED="
    set "YELLOW="
    set "BLUE="
    set "NC="
)

REM Check Python availability
echo %BLUE%Checking Python installation...%NC%
python --version >nul 2>&1
if errorlevel 1 (
    echo %RED%ERROR: Python is not installed or not in PATH%NC%
    echo Please install Python 3.8 or newer from https://python.org
    echo Make sure to check "Add Python to PATH" during installation
    echo.
    pause
    exit /b 1
)

REM Get and display Python version
for /f "tokens=2" %%v in ('python --version 2^>^&1') do set PYTHON_VERSION=%%v
echo %GREEN%Found Python %PYTHON_VERSION%%NC%

REM Check Python version (basic check for 3.x)
echo %PYTHON_VERSION% | findstr /r "^3\.[8-9]\." >nul
if errorlevel 1 (
    echo %PYTHON_VERSION% | findstr /r "^3\.1[0-9]\." >nul
    if errorlevel 1 (
        echo %YELLOW%Warning: Python 3.8+ recommended, you have %PYTHON_VERSION%%NC%
        echo.
    )
)

REM Check if we're in the correct directory
echo.
echo %BLUE%Checking project files...%NC%
if not exist "imgfactory.py" (
    echo %RED%ERROR: imgfactory.py not found%NC%
    echo Make sure you're running this from the IMG Factory directory
    echo Current directory: %CD%
    echo.
    pause
    exit /b 1
)

if exist "launch_imgfactory.py" (
    echo %GREEN%Found launch_imgfactory.py%NC%
) else (
    echo %YELLOW%Warning: launch_imgfactory.py not found%NC%
)

REM Check for component directories
if exist "components\" (
    echo %GREEN%Found components directory%NC%
) else (
    echo %YELLOW%Warning: components directory not found%NC%
)

if exist "gui\" (
    echo %GREEN%Found gui directory%NC%
) else (
    echo %YELLOW%Warning: gui directory not found%NC%
)

REM Check PyQt6 installation
echo.
echo %BLUE%Checking PyQt6 installation...%NC%
python -c "import PyQt6; print('PyQt6 version:', PyQt6.QtCore.PYQT_VERSION_STR)" 2>nul
if errorlevel 1 (
    echo %RED%ERROR: PyQt6 is not installed%NC%
    echo.
    echo %YELLOW%Attempting to install PyQt6...%NC%
    python -m pip install PyQt6
    if errorlevel 1 (
        echo %RED%Failed to install PyQt6 automatically%NC%
        echo Please install manually: pip install PyQt6
        echo.
        pause
        exit /b 1
    ) else (
        echo %GREEN%PyQt6 installed successfully%NC%
    )
) else (
    echo %GREEN%PyQt6 is available%NC%
)

echo.
echo ================================================================
echo %GREEN%Starting IMG Factory 1.5...%NC%
echo ================================================================
echo.

REM Try to launch using the modern launcher first
if exist "launch_imgfactory.py" (
    echo %BLUE%Using modern launcher...%NC%
    python launch_imgfactory.py
    set LAUNCH_EXIT=!errorlevel!
) else (
    echo %BLUE%Using direct launch...%NC%
    python imgfactory.py
    set LAUNCH_EXIT=!errorlevel!
)

echo.
echo ================================================================

REM Handle exit codes
if !LAUNCH_EXIT! equ 0 (
    echo %GREEN%IMG Factory closed normally%NC%
) else if !LAUNCH_EXIT! equ 130 (
    echo %YELLOW%Cancelled by user%NC%
) else (
    echo %RED%Application exited with error code: !LAUNCH_EXIT!%NC%
    echo.
    echo %YELLOW%Troubleshooting steps:%NC%
    echo 1. Make sure all files are in the correct location
    echo 2. Try: pip install --upgrade PyQt6
    echo 3. Check for error messages above
    echo 4. Run: python -c "import imgfactory" to test imports
    echo 5. Try running: python -v imgfactory.py for verbose output
)

echo.
echo Press any key to exit...
pause >nul
exit /b !LAUNCH_EXIT!
