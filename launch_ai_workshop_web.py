#!/usr/bin/env python3
"""
X-Seti - March 2026 - AI Workshop Web Server Launcher
#this belongs in root /launch_ai_workshop_web.py - Version: 1
"""
import sys
import os
import subprocess
import threading
import time
import webbrowser
from pathlib import Path

root_dir = Path(__file__).parent.resolve()
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

REQUIREMENTS = ["fastapi", "uvicorn[standard]", "httpx", "python-multipart"]
IMPORT_NAMES = {"fastapi": "fastapi", "uvicorn[standard]": "uvicorn",
                "httpx": "httpx", "python-multipart": "multipart"}


def ensure_deps():
    missing = []
    for pkg in REQUIREMENTS:
        imp = IMPORT_NAMES.get(pkg, pkg.split("[")[0].replace("-", "_"))
        try:
            __import__(imp)
        except ImportError:
            missing.append(pkg)
    if not missing:
        return True
    print(f"Installing: {', '.join(missing)}")
    r = subprocess.run([sys.executable, "-m", "pip", "install",
                        "--break-system-packages"] + missing)
    if r.returncode != 0:
        print("pip install failed. Try manually:")
        print(f"  pip install {' '.join(missing)} --break-system-packages")
        return False
    print("Done.\n")
    return True


def load_config():
    cfg = {"port": 8080, "host": "127.0.0.1",
           "ollama_url": "http://localhost:11434",
           "sessions_dir": os.path.expanduser("~/.config/imgfactory/ai_sessions")}
    try:
        import json
        p = os.path.expanduser("~/.config/imgfactory/ai_workshop_web.json")
        if os.path.exists(p):
            with open(p) as f:
                cfg.update(json.load(f))
    except Exception:
        pass
    return cfg


def main():
    print("=" * 50)
    print("  AI Workshop — Web Server")
    print("=" * 50)

    if not ensure_deps():
        sys.exit(1)

    cfg  = load_config()
    port = int(os.environ.get("AIWS_PORT", cfg["port"]))
    host = os.environ.get("AIWS_HOST", cfg["host"])

    from apps.components.Ai_Workshop.web.server import app, configure
    configure(ollama_url=cfg["ollama_url"], sessions_dir=cfg["sessions_dir"],
              port=port, host=host)

    display_host = "localhost" if host in ("0.0.0.0", "127.0.0.1") else host
    url = f"http://{display_host}:{port}"

    print(f"\n  URL      : {url}")
    print(f"  Sessions : {cfg['sessions_dir']}")
    print(f"  Ollama   : {cfg['ollama_url']}")
    print(f"\n  Press Ctrl+C to stop\n")

    def _open():
        time.sleep(1.2)
        print(f"  Opening browser…")
        webbrowser.open(url)

    threading.Thread(target=_open, daemon=True).start()

    import uvicorn
    uvicorn.run(app, host=host, port=port, log_level="warning")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\nServer stopped.")
    except Exception as e:
        print(f"ERROR: {e}")
        import traceback; traceback.print_exc()
        sys.exit(1)
