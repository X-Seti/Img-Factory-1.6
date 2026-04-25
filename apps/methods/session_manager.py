#!/usr/bin/env python3
# apps/components/Ai_Workshop/depends/session_manager.py - Version: 1
# AI Workshop - Session persistence, pin, search, export

import os
import json
import tarfile
from datetime import datetime
from pathlib import Path


DEFAULT_SESSIONS_DIR = os.path.expanduser("~/.config/imgfactory/ai_sessions")


class SessionManager:
    """Handles saving, loading, searching and exporting chat sessions."""

    def __init__(self, sessions_dir: str = None):
        self.sessions_dir = sessions_dir or DEFAULT_SESSIONS_DIR
        os.makedirs(self.sessions_dir, exist_ok=True)

    # -----------------------------------------------------------------------
    # CRUD
    # -----------------------------------------------------------------------

    def save_session(self, session: dict) -> bool:
        """Save a single session to disk. Returns True on success."""
        try:
            sid = session.get("id")
            if not sid:
                return False
            path = os.path.join(self.sessions_dir, f"{sid}.json")
            session["updated_at"] = datetime.now().isoformat()
            with open(path, "w", encoding="utf-8") as f:
                json.dump(session, f, indent=2, ensure_ascii=False)
            return True
        except Exception as e:
            print(f"[SessionManager] save error: {e}")
            return False

    def load_all(self) -> list[dict]:
        """Load all sessions from disk, sorted pinned-first then by updated_at."""
        sessions = []
        try:
            for fname in os.listdir(self.sessions_dir):
                if not fname.endswith(".json"):
                    continue
                path = os.path.join(self.sessions_dir, fname)
                try:
                    with open(path, "r", encoding="utf-8") as f:
                        s = json.load(f)
                    sessions.append(s)
                except Exception as e:
                    print(f"[SessionManager] load error {fname}: {e}")
        except Exception as e:
            print(f"[SessionManager] listdir error: {e}")

        sessions.sort(key=lambda s: (
            not s.get("pinned", False),
            s.get("updated_at", "")
        ), reverse=False)
        # pinned first, then most recent last — reverse for display
        sessions.sort(key=lambda s: (
            not s.get("pinned", False),
            s.get("updated_at", "9999")
        ))
        return sessions

    def delete_session(self, session_id: str) -> bool:
        """Delete session file from disk."""
        try:
            path = os.path.join(self.sessions_dir, f"{session_id}.json")
            if os.path.exists(path):
                os.remove(path)
            return True
        except Exception as e:
            print(f"[SessionManager] delete error: {e}")
            return False

    def new_session(self, name: str = "") -> dict:
        """Create a new session dict with a unique ID."""
        import uuid
        sid = str(uuid.uuid4())[:8]
        now = datetime.now().isoformat()
        return {
            "id":         sid,
            "name":       name or f"Session {datetime.now().strftime('%b %d %H:%M')}",
            "messages":   [],
            "pinned":     False,
            "created_at": now,
            "updated_at": now,
            "attachments": [],
        }

    # -----------------------------------------------------------------------
    # Search
    # -----------------------------------------------------------------------

    def search(self, query: str, sessions: list[dict]) -> list[dict]:
        """Return sessions whose name or message content contains query."""
        if not query.strip():
            return sessions
        q = query.lower()
        results = []
        for s in sessions:
            if q in s.get("name", "").lower():
                results.append(s)
                continue
            for msg in s.get("messages", []):
                if q in msg.get("content", "").lower():
                    results.append(s)
                    break
        return results

    # -----------------------------------------------------------------------
    # Export
    # -----------------------------------------------------------------------

    def export_txt(self, session: dict, path: str) -> bool:
        try:
            with open(path, "w", encoding="utf-8") as f:
                f.write(f"Session: {session.get('name', 'Unnamed')}\n")
                f.write(f"Created: {session.get('created_at', '')}\n")
                f.write("=" * 60 + "\n\n")
                for msg in session.get("messages", []):
                    role = msg.get("role", "?").upper()
                    content = msg.get("content", "")
                    f.write(f"[{role}]\n{content}\n\n")
            return True
        except Exception as e:
            print(f"[SessionManager] export_txt error: {e}")
            return False

    def export_md(self, session: dict, path: str) -> bool:
        try:
            with open(path, "w", encoding="utf-8") as f:
                f.write(f"# {session.get('name', 'Unnamed')}\n\n")
                f.write(f"*Created: {session.get('created_at', '')}*\n\n---\n\n")
                for msg in session.get("messages", []):
                    role = msg.get("role", "?")
                    content = msg.get("content", "")
                    if role == "user":
                        f.write(f"**You:**\n\n{content}\n\n")
                    elif role == "assistant":
                        f.write(f"**AI:**\n\n{content}\n\n")
                    else:
                        f.write(f"**{role.title()}:**\n\n{content}\n\n")
                    f.write("---\n\n")
            return True
        except Exception as e:
            print(f"[SessionManager] export_md error: {e}")
            return False

    # -----------------------------------------------------------------------
    # Backup
    # -----------------------------------------------------------------------

    def backup_directory(self, source_dir: str, backup_dir: str = None) -> tuple[bool, str]:
        """
        Create a single tar.gz backup of source_dir.
        Returns (success, backup_path_or_error).
        Only keeps the most recent backup — older one is replaced.
        """
        try:
            if not os.path.isdir(source_dir):
                return False, f"Not a directory: {source_dir}"

            if backup_dir is None:
                backup_dir = os.path.dirname(source_dir)
            os.makedirs(backup_dir, exist_ok=True)

            dirname = os.path.basename(source_dir.rstrip("/"))
            backup_path = os.path.join(backup_dir, f"{dirname}_ai_backup.tar.gz")

            # Remove old backup first (only keep one)
            if os.path.exists(backup_path):
                os.remove(backup_path)

            with tarfile.open(backup_path, "w:gz") as tar:
                tar.add(source_dir, arcname=dirname)

            size_mb = os.path.getsize(backup_path) / (1024 * 1024)
            return True, f"{backup_path} ({size_mb:.1f} MB)"

        except Exception as e:
            return False, str(e)
