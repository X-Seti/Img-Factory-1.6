#!/usr/bin/env python3
# apps/components/Ai_Workshop/depends/file_attachments.py - Version: 1
# AI Workshop - File attachment handling (images, code, text, SSH remote files)

import os
import base64
import mimetypes
from pathlib import Path


# File types we treat as text/code (inject as markdown code block)
TEXT_EXTENSIONS = {
    ".py", ".txt", ".cfg", ".dat", ".ini", ".json", ".xml",
    ".md", ".rst", ".yaml", ".yml", ".sh", ".bat", ".c",
    ".cpp", ".h", ".js", ".ts", ".html", ".css", ".sql",
    ".ipl", ".ide", ".gxt", ".fxt",
}

IMAGE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".gif", ".webp", ".bmp"}

# IMG Factory project file extensions
PROJECT_EXTENSIONS = {".img", ".dir", ".col", ".txd", ".dff", ".ifp"}


def classify_file(path: str) -> str:
    """Returns 'image', 'text', 'project', or 'binary'."""
    ext = Path(path).suffix.lower()
    if ext in IMAGE_EXTENSIONS:
        return "image"
    if ext in TEXT_EXTENSIONS:
        return "text"
    if ext in PROJECT_EXTENSIONS:
        return "project"
    return "binary"


def read_local_file(path: str, max_text_bytes: int = 256 * 1024) -> dict:
    """
    Read a local file and return an attachment dict:
    {
        'path': str,
        'name': str,
        'type': 'image'|'text'|'project'|'binary',
        'content': str,       # text content or base64 for images
        'mime': str,
        'size': int,
        'error': str | None,
    }
    """
    name = os.path.basename(path)
    ftype = classify_file(path)
    mime = mimetypes.guess_type(path)[0] or "application/octet-stream"

    try:
        size = os.path.getsize(path)
    except Exception:
        size = 0

    attachment = {
        "path":    path,
        "name":    name,
        "type":    ftype,
        "mime":    mime,
        "size":    size,
        "content": "",
        "error":   None,
        "source":  "local",
    }

    try:
        if ftype == "image":
            with open(path, "rb") as f:
                raw = f.read()
            attachment["content"] = base64.b64encode(raw).decode("ascii")
        elif ftype in ("text", "project"):
            with open(path, "r", encoding="utf-8", errors="replace") as f:
                attachment["content"] = f.read(max_text_bytes)
        else:
            attachment["error"] = f"Binary file — cannot inject content ({size} bytes)"
    except Exception as e:
        attachment["error"] = str(e)

    return attachment


def read_ssh_file(ssh_access, remote_path: str) -> dict:
    """
    Read a file via SSHFileAccess and return the same attachment dict shape.
    """
    name  = os.path.basename(remote_path)
    ftype = classify_file(remote_path)
    mime  = mimetypes.guess_type(remote_path)[0] or "application/octet-stream"

    attachment = {
        "path":    remote_path,
        "name":    name,
        "type":    ftype,
        "mime":    mime,
        "size":    0,
        "content": "",
        "error":   None,
        "source":  "ssh",
    }

    if ftype == "image":
        ok, data = ssh_access.read_binary(remote_path)
        if ok:
            attachment["content"] = base64.b64encode(data).decode("ascii")
            attachment["size"]    = len(data)
        else:
            attachment["error"] = data  # error string
    elif ftype in ("text", "project"):
        ok, text = ssh_access.read_text(remote_path)
        if ok:
            attachment["content"] = text
            attachment["size"]    = len(text)
        else:
            attachment["error"] = text
    else:
        attachment["error"] = "Binary file — cannot inject content"

    return attachment


def build_message_content(text: str, attachments: list[dict]) -> list:
    """
    Build the Ollama API `content` field from user text + attachments.
    Returns a list of content blocks (multimodal format).
    """
    parts = []

    # Text attachments: prepend as code blocks before the user message
    text_blocks = ""
    for att in attachments:
        if att.get("error"):
            text_blocks += f"\n[Attachment error: {att['name']}: {att['error']}]\n"
            continue
        if att["type"] == "image":
            continue  # handled separately below
        ext  = Path(att["name"]).suffix.lstrip(".")
        lang = _ext_to_lang(ext)
        text_blocks += f"\n### Attached file: `{att['name']}`\n```{lang}\n{att['content']}\n```\n"

    full_text = (text_blocks + "\n" + text).strip() if text_blocks else text

    if full_text:
        parts.append({"type": "text", "text": full_text})

    # Image attachments
    for att in attachments:
        if att["type"] == "image" and att.get("content") and not att.get("error"):
            parts.append({
                "type":       "image_url",
                "image_url":  {
                    "url": f"data:{att['mime']};base64,{att['content']}"
                }
            })

    # Fallback: plain string if no images (most models)
    if not any(p["type"] == "image_url" for p in parts):
        return full_text  # plain string, compatible with all models

    return parts  # multimodal list, for llava etc.


def _ext_to_lang(ext: str) -> str:
    mapping = {
        "py": "python", "js": "javascript", "ts": "typescript",
        "sh": "bash",   "c":  "c",          "cpp": "cpp",
        "h":  "c",      "json": "json",      "xml": "xml",
        "yaml": "yaml", "yml": "yaml",       "html": "html",
        "css": "css",   "sql": "sql",        "md": "markdown",
        "cfg": "ini",   "ini": "ini",        "txt": "text",
    }
    return mapping.get(ext.lower(), "")


# ---------------------------------------------------------------------------
# Qt chip widget for showing attached files
# ---------------------------------------------------------------------------

def create_attachment_chip(attachment: dict, on_remove=None, parent=None):
    """
    Returns a small QFrame 'chip' showing the attachment name with an X button.
    on_remove(attachment) is called when X is clicked.
    """
    from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel, QPushButton
    from PyQt6.QtCore import Qt

    TYPE_ICONS = {
        "image":   "🖼",
        "text":    "📄",
        "project": "🗂",
        "binary":  "📦",
    }

    chip = QFrame()
    chip.setFrameStyle(QFrame.Shape.StyledPanel)
    chip.setStyleSheet("""
        QFrame {
            background: #2a4a2a;
            border: 1px solid palette(mid);
            border-radius: 4px;
            padding: 2px 4px;
        }
    """)

    layout = QHBoxLayout(chip)
    layout.setContentsMargins(4, 2, 4, 2)
    layout.setSpacing(4)

    icon  = TYPE_ICONS.get(attachment.get("type", "binary"), "📎")
    name  = attachment.get("name", "file")
    label = QLabel(f"{icon} {name}")
    label.setStyleSheet("font-size: 10px; color: palette(placeholderText); background: transparent; border: none;")
    layout.addWidget(label)

    if on_remove:
        x_btn = QPushButton("✕")
        x_btn.setFixedSize(16, 16)
        x_btn.setStyleSheet("""
            QPushButton {
                background: transparent;
                border: none;
                color: #888;
                font-size: 10px;
                padding: 0;
            }
            QPushButton:hover { color: palette(buttonText); }
        """)
        x_btn.clicked.connect(lambda: on_remove(attachment))
        layout.addWidget(x_btn)

    if attachment.get("error"):
        chip.setStyleSheet(chip.styleSheet().replace("#2a4a2a", "#4a2a2a").replace("#3d7a3d", "#7a3d3d"))
        label.setStyleSheet(label.styleSheet().replace("#c0e0c0", "#e0c0c0"))

    return chip
