#!/usr/bin/env python3
"""
X-Seti - March 2026 - AI Workshop 1.0 - Web Launcher
#this belongs in root /launch_ai_workshop_web.py - Version: 1

Starts the FastAPI web server for AI Workshop.
- Auto-installs missing dependencies
- Opens browser automatically
- Port configurable via ~/.config/imgfactory/ai_workshop_web.json
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

# ── Requirements ────────────────────────────────────────────────────────────

REQUIREMENTS = [
    ("fastapi",    "fastapi"),
    ("uvicorn",    "uvicorn"),
    ("httpx",      "httpx"),
    ("multipart",  "python-multipart"),  # for file uploads
]


def check_and_install():
    missing = []
    for import_name, pkg_name in REQUIREMENTS:
        try:
            importlib.import_module(import_name)
        except ImportError:
            missing.append((import_name, pkg_name))

    if not missing:
        return True

    print(f"\nAI Workshop Web — installing {len(missing)} missing package(s):")
    for _, pkg in missing:
        print(f"  pip install {pkg}")

    print()
    for _, pkg in missing:
        result = subprocess.run(
            [sys.executable, "-m", "pip", "install", pkg, "--break-system-packages"],
            capture_output=False
        )
        if result.returncode != 0:
            print(f"\nERROR: Failed to install {pkg}")
            print("Try manually:  pip install " + pkg + " --break-system-packages")
            return False

    print("\nAll dependencies installed.\n")
    return True


# ── Config ──────────────────────────────────────────────────────────────────

CONFIG_PATH = os.path.expanduser("~/.config/imgfactory/ai_workshop_web.json")

DEFAULT_CONFIG = {
    "host":         "127.0.0.1",
    "port":         8080,
    "ollama_url":   "http://localhost:11434",
    "sessions_dir": os.path.expanduser("~/.config/imgfactory/ai_sessions"),
    "ssh": {
        "host": "127.0.0.1", "port": 22,
        "username": "", "password": "", "key_path": "", "root_path": "/home"
    }
}


def load_config():
    try:
        if os.path.exists(CONFIG_PATH):
            with open(CONFIG_PATH) as f:
                cfg = json.load(f)
            merged = dict(DEFAULT_CONFIG)
            merged.update(cfg)
            return merged
    except Exception as e:
        print(f"Config load error: {e}")
    # Write defaults on first run
    os.makedirs(os.path.dirname(CONFIG_PATH), exist_ok=True)
    with open(CONFIG_PATH, "w") as f:
        json.dump(DEFAULT_CONFIG, f, indent=2)
    print(f"Created default config at: {CONFIG_PATH}")
    return dict(DEFAULT_CONFIG)


# ── Main ─────────────────────────────────────────────────────────────────────

def main():
    print("=" * 55)
    print("  AI Workshop 1.0 — Web Interface")
    print("=" * 55)

    if not check_and_install():
        sys.exit(1)

    cfg  = load_config()
    host = cfg.get("host", "127.0.0.1")
    port = cfg.get("port", 8080)
    url  = f"http://{host}:{port}"

    print(f"\n  URL:  {url}")
    print(f"  Sessions: {cfg.get('sessions_dir', '~/.config/imgfactory/ai_sessions')}")
    print(f"  Ollama:   {cfg.get('ollama_url', 'http://localhost:11434')}")
    print(f"\n  Press Ctrl+C to stop\n")

    # Open browser after short delay (give server time to start)
    def _open_browser():
        import time, webbrowser
        time.sleep(1.2)
        webbrowser.open(url)
        print(f"  Browser opened: {url}")

    import threading
    threading.Thread(target=_open_browser, daemon=True).start()

    # Start uvicorn
    import uvicorn
    uvicorn.run(
        "apps.components.Ai_Workshop.web.server:app",
        host=host,
        port=port,
        reload=False,
        log_level="warning",
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
