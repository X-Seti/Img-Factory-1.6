#!/usr/bin/env python3
# apps/components/Ai_Workshop/web/server.py - Version: 1
# AI Workshop Web Server - FastAPI + WebSockets + Ollama proxy
# X-Seti March 2026

import os
import sys
import json
import uuid
import base64
import mimetypes
from pathlib import Path

current_dir  = Path(__file__).parent
project_root = current_dir.parent.parent.parent.parent
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from fastapi import FastAPI, WebSocket, WebSocketDisconnect, UploadFile, File, HTTPException
from fastapi.staticfiles import StaticFiles
from fastapi.responses import HTMLResponse, JSONResponse, FileResponse
from fastapi.middleware.cors import CORSMiddleware
import httpx
import uvicorn

app = FastAPI(title="AI Workshop", version="1.0")
app.add_middleware(CORSMiddleware, allow_origins=["*"],
                   allow_methods=["*"], allow_headers=["*"])

static_dir    = current_dir / "static"
templates_dir = current_dir / "templates"
app.mount("/static", StaticFiles(directory=str(static_dir)), name="static")

_config = {
    "ollama_url":   "http://localhost:11434",
    "sessions_dir": os.path.expanduser("~/.config/imgfactory/ai_sessions"),
    "port":         8080,
    "host":         "0.0.0.0",
}

def configure(**kwargs):
    _config.update({k: v for k, v in kwargs.items() if v is not None})

_ssh_connections: dict = {}


# ---------------------------------------------------------------------------
# HTML entry point
# ---------------------------------------------------------------------------

@app.get("/", response_class=HTMLResponse)
async def index():
    return HTMLResponse((templates_dir / "index.html").read_text())


# ---------------------------------------------------------------------------
# Theme API
# ---------------------------------------------------------------------------

@app.get("/api/theme")
async def get_theme(name: str = None):
    fallback = {
        "bg_primary": "#1a1a2e", "bg_secondary": "#16213e",
        "bg_tertiary": "#0f3460", "panel_bg": "#1e1e2e",
        "text_primary": "#e0e0e0", "text_secondary": "#a0a0b0",
        "text_accent": "#4fc3f7", "accent_primary": "#7c3aed",
        "accent_secondary": "#a855f7", "border": "#2a2a4a",
        "button_normal": "#2a2a4a", "button_hover": "#3a3a6a",
        "selection_background": "#7c3aed", "selection_text": "#ffffff",
        "success": "#4caf50", "warning": "#ff9800", "error": "#f44336",
    }
    try:
        from apps.utils.app_settings_system import AppSettings
        s = AppSettings()
        colors  = s.get_theme_colors(name)
        themes  = list(s.get_available_themes().keys()) if hasattr(s, 'get_available_themes') else []
        current = s.current_settings.get("theme", "IMG_Factory")
        return JSONResponse({"colors": colors, "themes": themes, "current": current})
    except Exception as e:
        return JSONResponse({"colors": fallback, "themes": [], "current": "default", "error": str(e)})


@app.post("/api/theme")
async def set_theme(data: dict):
    try:
        from apps.utils.app_settings_system import AppSettings
        s = AppSettings()
        if t := data.get("theme"):
            s.current_settings["theme"] = t
            s.save_settings()
        return JSONResponse({"ok": True})
    except Exception as e:
        return JSONResponse({"ok": False, "error": str(e)})


# ---------------------------------------------------------------------------
# Ollama
# ---------------------------------------------------------------------------

@app.get("/api/models")
async def list_models():
    try:
        async with httpx.AsyncClient(timeout=5) as c:
            r = await c.get(f"{_config['ollama_url']}/api/tags")
            r.raise_for_status()
            return JSONResponse({"models": [m["name"] for m in r.json().get("models", [])],
                                 "status": "connected"})
    except Exception as e:
        return JSONResponse({"models": [], "status": "offline", "error": str(e)})


@app.get("/api/ollama/status")
async def ollama_status():
    try:
        async with httpx.AsyncClient(timeout=3) as c:
            await c.get(f"{_config['ollama_url']}/api/tags")
        return JSONResponse({"running": True})
    except Exception:
        return JSONResponse({"running": False})


# ---------------------------------------------------------------------------
# Sessions
# ---------------------------------------------------------------------------

def _sm():
    from apps.components.Ai_Workshop.depends.session_manager import SessionManager
    return SessionManager(_config["sessions_dir"])


@app.get("/api/sessions")
async def list_sessions():
    try:
        sessions = _sm().load_all()
        lite = [{"id": s.get("id"), "name": s.get("name"),
                 "pinned": s.get("pinned", False),
                 "updated_at": s.get("updated_at", ""),
                 "msg_count": len(s.get("messages", []))} for s in sessions]
        return JSONResponse({"sessions": lite})
    except Exception as e:
        return JSONResponse({"sessions": [], "error": str(e)})


@app.get("/api/sessions/{sid}")
async def get_session(sid: str):
    path = os.path.join(_config["sessions_dir"], f"{sid}.json")
    if not os.path.exists(path):
        raise HTTPException(404, "Session not found")
    with open(path) as f:
        return JSONResponse(json.load(f))


@app.post("/api/sessions")
async def create_session(data: dict = {}):
    try:
        sm = _sm()
        s  = sm.new_session(data.get("name", ""))
        sm.save_session(s)
        return JSONResponse(s)
    except Exception as e:
        raise HTTPException(500, str(e))


@app.put("/api/sessions/{sid}")
async def update_session(sid: str, data: dict):
    path = os.path.join(_config["sessions_dir"], f"{sid}.json")
    if not os.path.exists(path):
        raise HTTPException(404, "Not found")
    with open(path) as f:
        s = json.load(f)
    for key in ("name", "pinned", "messages"):
        if key in data:
            s[key] = data[key]
    _sm().save_session(s)
    return JSONResponse({"ok": True})


@app.delete("/api/sessions/{sid}")
async def delete_session(sid: str):
    _sm().delete_session(sid)
    return JSONResponse({"ok": True})


@app.get("/api/sessions/{sid}/export")
async def export_session(sid: str, fmt: str = "md"):
    path = os.path.join(_config["sessions_dir"], f"{sid}.json")
    if not os.path.exists(path):
        raise HTTPException(404, "Not found")
    with open(path) as f:
        s = json.load(f)
    sm  = _sm()
    tmp = f"/tmp/aiws_export_{sid}.{fmt}"
    (sm.export_txt if fmt == "txt" else sm.export_md)(s, tmp)
    return FileResponse(tmp, filename=f"{s.get('name','session')}.{fmt}")


# ---------------------------------------------------------------------------
# File upload
# ---------------------------------------------------------------------------

@app.post("/api/upload")
async def upload_file(file: UploadFile = File(...)):
    try:
        from apps.components.Ai_Workshop.depends.file_attachments import classify_file
        content = await file.read()
        ftype   = classify_file(file.filename or "")
        mime    = file.content_type or mimetypes.guess_type(file.filename or "")[0] or "application/octet-stream"
        if ftype == "image":
            return JSONResponse({"name": file.filename, "type": "image", "mime": mime,
                                  "content": base64.b64encode(content).decode(), "size": len(content)})
        return JSONResponse({"name": file.filename, "type": ftype, "mime": mime,
                              "content": content.decode("utf-8", errors="replace"), "size": len(content)})
    except Exception as e:
        raise HTTPException(500, str(e))


# ---------------------------------------------------------------------------
# SSH browser
# ---------------------------------------------------------------------------

@app.post("/api/ssh/connect")
async def ssh_connect(data: dict):
    try:
        from apps.components.Ai_Workshop.depends.ssh_file_access import SSHFileAccess
        ssh = SSHFileAccess()
        ok, msg = ssh.connect(data.get("host","127.0.0.1"), data.get("port",22),
                               data.get("username",""), data.get("password",""),
                               data.get("key_path",""))
        if ok:
            cid = str(uuid.uuid4())[:8]
            _ssh_connections[cid] = ssh
            return JSONResponse({"ok": True, "conn_id": cid, "message": msg})
        return JSONResponse({"ok": False, "message": msg})
    except Exception as e:
        return JSONResponse({"ok": False, "message": str(e)})


@app.get("/api/ssh/{cid}/ls")
async def ssh_ls(cid: str, path: str = "/home"):
    ssh = _ssh_connections.get(cid)
    if not ssh:
        raise HTTPException(404, "SSH connection not found")
    ok, entries = ssh.list_dir(path)
    return JSONResponse({"ok": ok, "entries": entries, "path": path})


@app.get("/api/ssh/{cid}/read")
async def ssh_read(cid: str, path: str):
    ssh = _ssh_connections.get(cid)
    if not ssh:
        raise HTTPException(404, "SSH connection not found")
    from apps.components.Ai_Workshop.depends.file_attachments import classify_file
    ftype = classify_file(path)
    if ftype == "image":
        ok, data = ssh.read_binary(path)
        if ok:
            return JSONResponse({"ok": True, "type": "image",
                                  "mime": mimetypes.guess_type(path)[0] or "image/png",
                                  "content": base64.b64encode(data).decode(),
                                  "name": Path(path).name})
    else:
        ok, text = ssh.read_text(path)
        if ok:
            return JSONResponse({"ok": True, "type": ftype, "content": text,
                                  "name": Path(path).name})
    return JSONResponse({"ok": False, "content": ""})


@app.delete("/api/ssh/{cid}")
async def ssh_disconnect(cid: str):
    if ssh := _ssh_connections.pop(cid, None):
        try: ssh.disconnect()
        except Exception: pass
    return JSONResponse({"ok": True})


# ---------------------------------------------------------------------------
# WebSocket streaming chat
# ---------------------------------------------------------------------------

@app.websocket("/ws/chat")
async def chat_ws(websocket: WebSocket):
    await websocket.accept()
    try:
        while True:
            payload     = json.loads(await websocket.receive_text())
            model       = payload.get("model", "")
            messages    = payload.get("messages", [])
            temperature = float(payload.get("temperature", 0.7))
            max_tokens  = int(payload.get("max_tokens", 2048))
            system_p    = payload.get("system_prompt", "")
            attachments = payload.get("attachments", [])

            from apps.components.Ai_Workshop.depends.file_attachments import build_message_content

            api_msgs = []
            if system_p.strip():
                api_msgs.append({"role": "system", "content": system_p})
            for m in messages[:-1]:
                api_msgs.append({"role": m["role"], "content": m["content"]})

            last_text   = messages[-1]["content"] if messages else ""
            api_content = build_message_content(last_text, attachments)
            api_msgs.append({"role": "user",
                              "content": api_content if isinstance(api_content, str) else last_text})

            full = ""
            try:
                async with httpx.AsyncClient(timeout=120) as client:
                    async with client.stream("POST",
                            f"{_config['ollama_url']}/api/chat",
                            json={"model": model, "messages": api_msgs, "stream": True,
                                  "options": {"temperature": temperature,
                                              "num_predict": max_tokens}}) as resp:
                        resp.raise_for_status()
                        async for line in resp.aiter_lines():
                            if not line:
                                continue
                            chunk = json.loads(line)
                            token = chunk.get("message", {}).get("content", "")
                            if token:
                                full += token
                                await websocket.send_json({"type": "token", "token": token})
                            if chunk.get("done"):
                                break
                await websocket.send_json({"type": "done", "response": full})
            except httpx.ConnectError:
                await websocket.send_json({"type": "error",
                                            "message": "Cannot connect to Ollama. Run: ollama serve"})
            except Exception as e:
                await websocket.send_json({"type": "error", "message": str(e)})

    except WebSocketDisconnect:
        pass


def run(host: str = "0.0.0.0", port: int = 8080):
    uvicorn.run(app, host=host, port=port, log_level="warning")
