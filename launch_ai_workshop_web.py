#!/usr/bin/env python3
"""
X-Seti - March 2026 - AI Workshop 1.0 - Web Launcher
#this belongs in root /launch_ai_workshop_web.py - Version: 2

Starts the FastAPI web server for AI Workshop.
- Auto-installs missing dependencies (including uvicorn[standard] for WebSocket support)
- Reads config from ~/.config/imgfactory/ai_workshop_web.json
- Opens browser automatically at startup
- Port, host, Ollama URL all configurable
"""
import sys
import os
import json
import subprocess
import importlib
from pathlib import Path

root_dir = Path(__file__).parent.resolve()
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

VERSION = "1.0"

# ── Requirements ─────────────────────────────────────────────────────────────
# NOTE: uvicorn must be installed as uvicorn[standard] to get WebSocket support
# (websockets library). Without it you get:
#   WARNING: No supported WebSocket library detected.

REQUIREMENTS = [
    ("fastapi",    "fastapi>=0.110.0"),
    ("uvicorn",    "uvicorn[standard]>=0.27.0"),   # [standard] includes websockets
    ("httpx",      "httpx>=0.26.0"),
    ("multipart",  "python-multipart>=0.0.9"),
]

OPTIONAL_REQUIREMENTS = [
    ("paramiko",   "paramiko>=3.4.0",  "SSH file access"),
]


def _pip_install(pkg_spec: str) -> bool:
    result = subprocess.run(
        [sys.executable, "-m", "pip", "install", pkg_spec, "--break-system-packages"],
        capture_output=True, text=True
    )
    if result.returncode != 0:
        print(f"  FAILED: {result.stderr.strip()}")
    return result.returncode == 0


def check_and_install() -> bool:
    missing = []
    for import_name, pkg_spec in REQUIREMENTS:
        try:
            importlib.import_module(import_name)
        except ImportError:
            missing.append((import_name, pkg_spec))

    # Special case: uvicorn may be installed but WITHOUT websockets support
    # Reinstall with [standard] if websockets is missing
    try:
        import uvicorn
        try:
            import websockets
        except ImportError:
            try:
                import wsproto
            except ImportError:
                print("  uvicorn installed but missing WebSocket library — upgrading...")
                if not _pip_install("uvicorn[standard]>=0.27.0"):
                    print("  WARNING: WebSocket support may be unavailable")

    except ImportError:
        pass  # Caught in missing list above

    if not missing:
        return True

    print(f"\nAI Workshop Web — installing {len(missing)} missing package(s):")
    for _, pkg in missing:
        print(f"  {pkg}")
    print()

    for _, pkg_spec in missing:
        print(f"  Installing {pkg_spec}...")
        if not _pip_install(pkg_spec):
            print(f"\nERROR: Failed to install {pkg_spec}")
            print(f"Try manually:  pip install \"{pkg_spec}\" --break-system-packages")
            return False

    # Check optional
    print("\nChecking optional packages...")
    for import_name, pkg_spec, desc in OPTIONAL_REQUIREMENTS:
        try:
            importlib.import_module(import_name)
            print(f"  ✓ {import_name} ({desc})")
        except ImportError:
            print(f"  - {import_name} not installed ({desc} unavailable)")
            print(f"    Install with:  pip install {pkg_spec} --break-system-packages")

    print("\nAll required dependencies installed.\n")
    return True


# ── Config ────────────────────────────────────────────────────────────────────

CONFIG_PATH = os.path.expanduser("~/.config/imgfactory/ai_workshop_web.json")

DEFAULT_CONFIG = {
    "host":         "127.0.0.1",
    "port":         8080,
    "ollama_url":   "http://localhost:11434",
    "sessions_dir": os.path.expanduser("~/.config/imgfactory/ai_sessions"),
    "open_browser": True,
    "log_level":    "warning",
    "ssh": {
        "host":      "127.0.0.1",
        "port":      22,
        "username":  "",
        "password":  "",
        "key_path":  "",
        "root_path": "/home"
    }
}


def load_config() -> dict:
    try:
        if os.path.exists(CONFIG_PATH):
            with open(CONFIG_PATH) as f:
                cfg = json.load(f)
            # Merge with defaults so new keys always exist
            merged = dict(DEFAULT_CONFIG)
            merged.update(cfg)
            return merged
    except Exception as e:
        print(f"  Config load error: {e}")

    # First run — write defaults
    os.makedirs(os.path.dirname(CONFIG_PATH), exist_ok=True)
    with open(CONFIG_PATH, "w") as f:
        json.dump(DEFAULT_CONFIG, f, indent=2)
    print(f"  Created default config: {CONFIG_PATH}")
    return dict(DEFAULT_CONFIG)


def save_config(cfg: dict):
    os.makedirs(os.path.dirname(CONFIG_PATH), exist_ok=True)
    with open(CONFIG_PATH, "w") as f:
        json.dump(cfg, f, indent=2)


# ── Startup banner ────────────────────────────────────────────────────────────

def print_banner(url: str, cfg: dict):
    host       = cfg.get("host", "127.0.0.1")
    display    = "127.0.0.1" if host == "0.0.0.0" else host
    port       = cfg.get("port", 8080)
    sessions   = cfg.get("sessions_dir", "~/.config/imgfactory/ai_sessions")
    ollama     = cfg.get("ollama_url", "http://localhost:11434")
    access     = "localhost only" if display == "127.0.0.1" else "all network interfaces"

    print()
    print("╔══════════════════════════════════════════════════════╗")
    print(f"║   AI Workshop {VERSION} — Web Interface                   ║")
    print("╠══════════════════════════════════════════════════════╣")
    print(f"║   URL      : http://{display}:{port:<28}║")
    print(f"║   Access   : {access:<41}║")
    print(f"║   Ollama   : {ollama:<41}║")
    print(f"║   Sessions : {sessions[:41]:<41}║")
    print("╠══════════════════════════════════════════════════════╣")
    print("║   Press Ctrl+C to stop                               ║")
    print("╚══════════════════════════════════════════════════════╝")
    print()


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    print(f"\nAI Workshop {VERSION} — Web Launcher")
    print("─" * 40)

    if not check_and_install():
        sys.exit(1)

    cfg  = load_config()
    host = cfg.get("host", "127.0.0.1")
    port = cfg.get("port", 8080)
    url  = f"http://{'127.0.0.1' if host == '0.0.0.0' else host}:{port}"

    print_banner(url, cfg)

    # Open browser
    if cfg.get("open_browser", True):
        def _open():
            import time, webbrowser
            time.sleep(1.5)
            webbrowser.open(url)
            print(f"  ► Browser opened: {url}\n")
        import threading
        threading.Thread(target=_open, daemon=True).start()

    # Start uvicorn with WebSocket support
    import uvicorn
    uvicorn.run(
        "apps.components.Ai_Workshop.web.server:app",
        host=host,
        port=port,
        reload=False,
        log_level=cfg.get("log_level", "warning"),
        # ws="websockets" ensures WebSocket library is used explicitly
        ws="websockets",
    )


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nAI Workshop Web stopped.")
    except Exception as e:
        print(f"\nERROR: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
