#!/usr/bin/env python3
# apps/components/Ai_Workshop/ai_workshop.py - Version: 1
# X-Seti - March 2026 - AI Workshop: Ollama chat interface
# Based on COL Workshop template (col_workshop.py)

"""
components/Ai_Workshop/ai_workshop.py
AI Workshop - Local LLM chat interface via Ollama REST API
Three-panel layout: Sessions | Chat | Settings
"""

import os
import json
import requests
import threading
from datetime import datetime
from pathlib import Path

os.environ['QT_QPA_PLATFORM'] = 'xcb'
os.environ['QSG_RHI_BACKEND'] = 'opengl'

import sys

current_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(os.path.dirname(os.path.dirname(current_dir)))
if str(project_root) not in sys.path:
    sys.path.insert(0, str(project_root))

from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QListWidget, QListWidgetItem, QLabel, QPushButton, QFrame,
    QTextEdit, QLineEdit, QMessageBox, QGroupBox, QComboBox,
    QSpinBox, QTabWidget, QScrollArea, QCheckBox, QDialog,
    QFormLayout, QFontComboBox, QSlider, QDoubleSpinBox,
    QSizePolicy, QAbstractItemView, QMenu
)
from PyQt6.QtCore import Qt, pyqtSignal, QSize, QPoint, QThread, pyqtSlot
from PyQt6.QtGui import QFont, QIcon, QColor, QPainter, QPen, QBrush, QPainterPath, QKeySequence, QShortcut

App_name = "AI Workshop"
DEBUG_STANDALONE = False
OLLAMA_BASE_URL = "http://localhost:11434"

# --- Try importing shared infrastructure ---
try:
    from apps.methods.imgfactory_svg_icons import SVGIconFactory
    ICONS_AVAILABLE = True
except ImportError:
    ICONS_AVAILABLE = False
    class SVGIconFactory:
        def __getattr__(self, name):
            return lambda *a, **k: QIcon()
        @staticmethod
        def clear_cache(): pass

try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    AppSettings = None


# ---------------------------------------------------------------------------
# Worker thread: streams Ollama response without blocking UI
# ---------------------------------------------------------------------------

class OllamaWorker(QThread):
    """Runs Ollama API call in background thread."""

    token_received  = pyqtSignal(str)   # incremental token
    response_done   = pyqtSignal(str)   # full response on completion
    error_occurred  = pyqtSignal(str)   # error message

    def __init__(self, model: str, messages: list, base_url: str = OLLAMA_BASE_URL,
                 temperature: float = 0.7, max_tokens: int = 2048):
        super().__init__()
        self.model       = model
        self.messages    = messages
        self.base_url    = base_url
        self.temperature = temperature
        self.max_tokens  = max_tokens
        self._full_response = ""

    def run(self):
        try:
            url  = f"{self.base_url}/api/chat"
            body = {
                "model":    self.model,
                "messages": self.messages,
                "stream":   True,
                "options":  {
                    "temperature": self.temperature,
                    "num_predict": self.max_tokens,
                }
            }

            with requests.post(url, json=body, stream=True, timeout=120) as resp:
                resp.raise_for_status()
                for line in resp.iter_lines():
                    if not line:
                        continue
                    chunk = json.loads(line)
                    token = chunk.get("message", {}).get("content", "")
                    if token:
                        self._full_response += token
                        self.token_received.emit(token)
                    if chunk.get("done"):
                        break

            self.response_done.emit(self._full_response)

        except requests.exceptions.ConnectionError:
            self.error_occurred.emit(
                "Cannot connect to Ollama.\n"
                "Make sure it is running:  ollama serve"
            )
        except Exception as e:
            self.error_occurred.emit(str(e))


# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------

def _fetch_models(base_url: str = OLLAMA_BASE_URL) -> list:
    """Return list of locally installed model names."""
    try:
        r = requests.get(f"{base_url}/api/tags", timeout=5)
        r.raise_for_status()
        return [m["name"] for m in r.json().get("models", [])]
    except Exception:
        return []


def _ollama_running(base_url: str = OLLAMA_BASE_URL) -> bool:
    try:
        requests.get(f"{base_url}/api/tags", timeout=3)
        return True
    except Exception:
        return False


# ---------------------------------------------------------------------------
# Main window class
# ---------------------------------------------------------------------------

class AIWorkshop(QWidget):
    """AI Workshop – Ollama chat UI built from COL Workshop skeleton."""

    workshop_closed = pyqtSignal()
    window_closed   = pyqtSignal()

    # -----------------------------------------------------------------------
    # Init
    # -----------------------------------------------------------------------

    def __init__(self, parent=None, main_window=None):
        super().__init__(parent)

        self.main_window        = main_window
        self.standalone_mode    = (main_window is None)
        self.is_docked          = not self.standalone_mode

        # Chat state
        self.sessions: list[dict] = []   # [{name, messages, created}]
        self.current_session_index = -1
        self.worker: OllamaWorker | None = None

        # Settings
        self.selected_model   = ""
        self.temperature      = 0.7
        self.max_tokens       = 2048
        self.system_prompt    = (
            "You are a helpful coding assistant specialising in Python, "
            "PyQt6, and GTA modding tools. Be concise and practical."
        )

        # Fonts (mirrors COL Workshop)
        self.title_font   = QFont("Arial", 14)
        self.panel_font   = QFont("Arial", 10)
        self.button_font  = QFont("Arial", 10)
        self.chat_font    = QFont("Courier New", 10)
        self.button_display_mode = 'both'

        # Window chrome
        self.use_system_titlebar  = False
        self.window_always_on_top = False
        self.dragging             = False
        self.drag_position        = None
        self.resizing             = False
        self.resize_corner        = None
        self.corner_size          = 20
        self.hover_corner         = None

        # AppSettings
        if main_window and hasattr(main_window, 'app_settings'):
            self.app_settings = main_window.app_settings
        elif APPSETTINGS_AVAILABLE:
            try:
                self.app_settings = AppSettings()
            except Exception:
                self.app_settings = None
        else:
            self.app_settings = None

        if self.app_settings and hasattr(self.app_settings, 'theme_changed'):
            self.app_settings.theme_changed.connect(self._refresh_icons)

        # Icon factory
        self.icon_factory = SVGIconFactory()

        self.setWindowTitle(App_name)
        if ICONS_AVAILABLE:
            self.setWindowIcon(SVGIconFactory.ai_icon(24))
        self.resize(1400, 800)
        self.setMinimumSize(800, 500)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self._new_session()   # start with one blank session
        self.setup_ui()
        self._setup_hotkeys()
        self._apply_theme()
        self._refresh_model_list()

    # -----------------------------------------------------------------------
    # UI construction
    # -----------------------------------------------------------------------

    def setup_ui(self):
        main_layout = QVBoxLayout(self)
        main_layout.setContentsMargins(5, 5, 5, 5)
        main_layout.setSpacing(5)

        toolbar = self._create_toolbar()
        self._workshop_toolbar = toolbar
        if not self.standalone_mode:
            toolbar.setVisible(False)
        main_layout.addWidget(toolbar)

        splitter = QSplitter(Qt.Orientation.Horizontal)

        left   = self._create_left_panel()
        centre = self._create_centre_panel()
        right  = self._create_right_panel()

        splitter.addWidget(left)
        splitter.addWidget(centre)
        splitter.addWidget(right)
        splitter.setStretchFactor(0, 1)   # sessions
        splitter.setStretchFactor(1, 4)   # chat
        splitter.setStretchFactor(2, 1)   # settings

        main_layout.addWidget(splitter)

    # --- Toolbar -----------------------------------------------------------

    def _create_toolbar(self):
        self.titlebar = QFrame()
        self.titlebar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")
        self.titlebar.installEventFilter(self)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setMaximumHeight(50)

        layout = QHBoxLayout(self.toolbar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        # Settings
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self.icon_factory.settings_icon())
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("AI Workshop Settings")
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.title_label)

        layout.addStretch()

        # New session
        self.new_session_btn = QPushButton()
        self.new_session_btn.setFont(self.button_font)
        self.new_session_btn.setIcon(self.icon_factory.add_icon())
        self.new_session_btn.setText("New Session")
        self.new_session_btn.setIconSize(QSize(20, 20))
        self.new_session_btn.clicked.connect(self._new_session_action)
        self.new_session_btn.setToolTip("Start a new chat session")
        layout.addWidget(self.new_session_btn)

        # Clear chat
        self.clear_btn = QPushButton()
        self.clear_btn.setFont(self.button_font)
        self.clear_btn.setIcon(self.icon_factory.delete_icon())
        self.clear_btn.setText("Clear")
        self.clear_btn.setIconSize(QSize(20, 20))
        self.clear_btn.clicked.connect(self._clear_current_session)
        self.clear_btn.setToolTip("Clear current session messages")
        layout.addWidget(self.clear_btn)

        # Theme
        self.properties_btn = QPushButton()
        self.properties_btn.setFont(self.button_font)
        self.properties_btn.setIcon(self.icon_factory.properties_icon() if hasattr(self.icon_factory, 'properties_icon') else QIcon())
        self.properties_btn.setToolTip("Theme")
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        layout.addWidget(self.properties_btn)

        # Dock / Tearoff
        self.dock_btn = QPushButton("D")
        self.dock_btn.setMinimumWidth(40)
        self.dock_btn.setMaximumWidth(40)
        self.dock_btn.setMinimumHeight(30)
        self.dock_btn.setToolTip("Dock")
        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        layout.addWidget(self.dock_btn)

        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T")
            self.tearoff_btn.setMinimumWidth(40)
            self.tearoff_btn.setMaximumWidth(40)
            self.tearoff_btn.setMinimumHeight(30)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("Tear off window")
            layout.addWidget(self.tearoff_btn)

        # Window controls
        for attr, icon_method, slot, tip in [
            ('minimize_btn', 'minimize_icon', self.showMinimized, "Minimize"),
            ('maximize_btn', 'maximize_icon', self._toggle_maximize,  "Maximize"),
            ('close_btn',    'close_icon',    self.close,             "Close"),
        ]:
            btn = QPushButton()
            btn.setIcon(getattr(self.icon_factory, icon_method)())
            btn.setIconSize(QSize(20, 20))
            btn.setMinimumWidth(40); btn.setMaximumWidth(40); btn.setMinimumHeight(30)
            btn.clicked.connect(slot)
            btn.setToolTip(tip)
            setattr(self, attr, btn)
            layout.addWidget(btn)

        return self.toolbar

    # --- Left panel: session list ------------------------------------------

    def _create_left_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(160)
        panel.setMaximumWidth(260)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)

        header = QLabel("Sessions")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(header)

        self.session_list = QListWidget()
        self.session_list.setAlternatingRowColors(True)
        self.session_list.currentRowChanged.connect(self._on_session_selected)
        self.session_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.session_list.customContextMenuRequested.connect(self._session_context_menu)
        layout.addWidget(self.session_list)

        new_btn = QPushButton("+ New Session")
        new_btn.setFont(self.button_font)
        new_btn.clicked.connect(self._new_session_action)
        layout.addWidget(new_btn)

        self._refresh_session_list()
        return panel

    # --- Centre panel: chat ------------------------------------------------

    def _create_centre_panel(self):
        panel = QGroupBox("Chat")
        layout = QVBoxLayout(panel)
        layout.setSpacing(4)

        # Status bar (Ollama connection)
        self.status_label = QLabel("Checking Ollama...")
        self.status_label.setFont(QFont("Arial", 9))
        self.status_label.setStyleSheet("color: #aaa;")
        layout.addWidget(self.status_label)

        # Chat display
        self.chat_display = QTextEdit()
        self.chat_display.setReadOnly(True)
        self.chat_display.setFont(self.chat_font)
        self.chat_display.setPlaceholderText("Messages will appear here…")
        layout.addWidget(self.chat_display, stretch=1)

        # Typing indicator
        self.typing_label = QLabel("")
        self.typing_label.setFont(QFont("Arial", 9))
        self.typing_label.setStyleSheet("color: #888; font-style: italic;")
        layout.addWidget(self.typing_label)

        # Input area
        input_frame = QFrame()
        input_layout = QHBoxLayout(input_frame)
        input_layout.setContentsMargins(0, 0, 0, 0)
        input_layout.setSpacing(4)

        self.input_box = QTextEdit()
        self.input_box.setFont(self.chat_font)
        self.input_box.setMaximumHeight(80)
        self.input_box.setPlaceholderText("Type your message… (Ctrl+Enter to send)")
        self.input_box.installEventFilter(self)
        input_layout.addWidget(self.input_box, stretch=1)

        send_col = QVBoxLayout()
        self.send_btn = QPushButton("Send")
        self.send_btn.setFont(self.button_font)
        self.send_btn.setFixedWidth(70)
        self.send_btn.setMinimumHeight(36)
        self.send_btn.clicked.connect(self._send_message)
        self.send_btn.setToolTip("Send message (Ctrl+Enter)")
        send_col.addWidget(self.send_btn)

        self.stop_btn = QPushButton("Stop")
        self.stop_btn.setFont(self.button_font)
        self.stop_btn.setFixedWidth(70)
        self.stop_btn.setEnabled(False)
        self.stop_btn.clicked.connect(self._stop_generation)
        self.stop_btn.setToolTip("Stop generation")
        send_col.addWidget(self.stop_btn)

        send_col.addStretch()
        input_layout.addLayout(send_col)
        layout.addWidget(input_frame)

        return panel

    # --- Right panel: settings ---------------------------------------------

    def _create_right_panel(self):
        panel = QFrame()
        panel.setFrameStyle(QFrame.Shape.StyledPanel)
        panel.setMinimumWidth(180)
        panel.setMaximumWidth(280)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(8, 8, 8, 8)
        layout.setSpacing(10)

        header = QLabel("Settings")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        layout.addWidget(header)

        # Model selector
        model_group = QGroupBox("Model")
        model_layout = QVBoxLayout(model_group)

        self.model_combo = QComboBox()
        self.model_combo.setFont(self.panel_font)
        self.model_combo.setToolTip("Select installed Ollama model")
        self.model_combo.currentTextChanged.connect(self._on_model_changed)
        model_layout.addWidget(self.model_combo)

        refresh_btn = QPushButton("Refresh Models")
        refresh_btn.setFont(self.button_font)
        refresh_btn.clicked.connect(self._refresh_model_list)
        model_layout.addWidget(refresh_btn)

        layout.addWidget(model_group)

        # Generation parameters
        gen_group = QGroupBox("Generation")
        gen_layout = QFormLayout(gen_group)

        self.temp_spin = QDoubleSpinBox()
        self.temp_spin.setRange(0.0, 2.0)
        self.temp_spin.setSingleStep(0.1)
        self.temp_spin.setValue(self.temperature)
        self.temp_spin.setToolTip("Temperature: 0=deterministic, 1=creative")
        self.temp_spin.valueChanged.connect(lambda v: setattr(self, 'temperature', v))
        gen_layout.addRow("Temperature:", self.temp_spin)

        self.tokens_spin = QSpinBox()
        self.tokens_spin.setRange(128, 16384)
        self.tokens_spin.setSingleStep(256)
        self.tokens_spin.setValue(self.max_tokens)
        self.tokens_spin.setToolTip("Maximum tokens to generate")
        self.tokens_spin.valueChanged.connect(lambda v: setattr(self, 'max_tokens', v))
        gen_layout.addRow("Max tokens:", self.tokens_spin)

        layout.addWidget(gen_group)

        # System prompt
        sys_group = QGroupBox("System Prompt")
        sys_layout = QVBoxLayout(sys_group)

        self.system_prompt_edit = QTextEdit()
        self.system_prompt_edit.setFont(QFont("Courier New", 9))
        self.system_prompt_edit.setMaximumHeight(120)
        self.system_prompt_edit.setPlainText(self.system_prompt)
        self.system_prompt_edit.textChanged.connect(
            lambda: setattr(self, 'system_prompt', self.system_prompt_edit.toPlainText())
        )
        sys_layout.addWidget(self.system_prompt_edit)

        layout.addWidget(sys_group)

        # Ollama URL
        url_group = QGroupBox("Ollama")
        url_layout = QFormLayout(url_group)

        self.url_input = QLineEdit(OLLAMA_BASE_URL)
        self.url_input.setFont(self.panel_font)
        self.url_input.setToolTip("Ollama base URL")
        url_layout.addRow("URL:", self.url_input)

        layout.addWidget(url_group)

        layout.addStretch()

        # Ollama status indicator
        self.ollama_status = QLabel("● Checking…")
        self.ollama_status.setFont(QFont("Arial", 9))
        layout.addWidget(self.ollama_status)

        self._update_ollama_status()

        return panel

    # -----------------------------------------------------------------------
    # Session management
    # -----------------------------------------------------------------------

    def _new_session(self, name: str = "") -> dict:
        idx = len(self.sessions) + 1
        if not name:
            name = f"Session {idx}"
        session = {"name": name, "messages": [], "created": datetime.now().isoformat()}
        self.sessions.append(session)
        self.current_session_index = len(self.sessions) - 1
        return session

    def _new_session_action(self):
        self._new_session()
        self._refresh_session_list()
        self.session_list.setCurrentRow(self.current_session_index)
        self._load_session(self.current_session_index)

    def _refresh_session_list(self):
        self.session_list.blockSignals(True)
        self.session_list.clear()
        for s in self.sessions:
            self.session_list.addItem(s["name"])
        self.session_list.blockSignals(False)
        if self.current_session_index >= 0:
            self.session_list.setCurrentRow(self.current_session_index)

    def _on_session_selected(self, row: int):
        if row < 0 or row >= len(self.sessions):
            return
        self.current_session_index = row
        self._load_session(row)

    def _load_session(self, index: int):
        session = self.sessions[index]
        self.chat_display.clear()
        for msg in session["messages"]:
            self._append_bubble(msg["role"], msg["content"])

    def _session_context_menu(self, pos):
        item = self.session_list.itemAt(pos)
        if not item:
            return
        row = self.session_list.row(item)
        menu = QMenu(self)
        rename = menu.addAction("Rename")
        delete = menu.addAction("Delete")
        action = menu.exec(self.session_list.mapToGlobal(pos))
        if action == rename:
            self._rename_session(row)
        elif action == delete:
            self._delete_session(row)

    def _rename_session(self, row: int):
        from PyQt6.QtWidgets import QInputDialog
        name, ok = QInputDialog.getText(self, "Rename Session",
                                        "Session name:", text=self.sessions[row]["name"])
        if ok and name.strip():
            self.sessions[row]["name"] = name.strip()
            self._refresh_session_list()

    def _delete_session(self, row: int):
        if len(self.sessions) <= 1:
            QMessageBox.information(self, "Delete", "Cannot delete the only session.")
            return
        self.sessions.pop(row)
        self.current_session_index = max(0, row - 1)
        self._refresh_session_list()
        self._load_session(self.current_session_index)

    def _clear_current_session(self):
        if self.current_session_index < 0:
            return
        reply = QMessageBox.question(self, "Clear Session",
                                     "Clear all messages in this session?",
                                     QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply == QMessageBox.StandardButton.Yes:
            self.sessions[self.current_session_index]["messages"] = []
            self.chat_display.clear()

    # -----------------------------------------------------------------------
    # Chat logic
    # -----------------------------------------------------------------------

    def _send_message(self):
        text = self.input_box.toPlainText().strip()
        if not text:
            return
        if self.worker and self.worker.isRunning():
            return

        if self.current_session_index < 0:
            self._new_session_action()

        session = self.sessions[self.current_session_index]

        # Append user message
        session["messages"].append({"role": "user", "content": text})
        self._append_bubble("user", text)
        self.input_box.clear()

        # Build messages list for API (prepend system prompt)
        api_messages = []
        if self.system_prompt.strip():
            api_messages.append({"role": "system", "content": self.system_prompt})
        api_messages.extend(session["messages"])

        model = self.model_combo.currentText()
        if not model:
            QMessageBox.warning(self, "No Model", "Select a model in the Settings panel.")
            return

        base_url = self.url_input.text().rstrip("/")

        self.send_btn.setEnabled(False)
        self.stop_btn.setEnabled(True)
        self.typing_label.setText(f"{model} is thinking…")

        # Placeholder for assistant response
        self._current_response = ""
        self._append_bubble("assistant", "")   # empty bubble, will fill in

        self.worker = OllamaWorker(model, api_messages, base_url,
                                   self.temperature, self.max_tokens)
        self.worker.token_received.connect(self._on_token)
        self.worker.response_done.connect(self._on_response_done)
        self.worker.error_occurred.connect(self._on_error)
        self.worker.start()

    def _on_token(self, token: str):
        self._current_response += token
        # Update the last assistant bubble in-place
        cursor = self.chat_display.textCursor()
        html = self.chat_display.toHtml()
        # Simple approach: replace last placeholder with growing response
        self._refresh_last_assistant_bubble(self._current_response)

    def _on_response_done(self, full_response: str):
        session = self.sessions[self.current_session_index]
        session["messages"].append({"role": "assistant", "content": full_response})
        self._refresh_last_assistant_bubble(full_response)
        self.send_btn.setEnabled(True)
        self.stop_btn.setEnabled(False)
        self.typing_label.setText("")
        self.worker = None

    def _on_error(self, error_msg: str):
        self.typing_label.setText("")
        self.send_btn.setEnabled(True)
        self.stop_btn.setEnabled(False)
        self._append_bubble("error", error_msg)
        self.worker = None

    def _stop_generation(self):
        if self.worker and self.worker.isRunning():
            self.worker.terminate()
            self.worker = None
            self.typing_label.setText("Stopped.")
            self.send_btn.setEnabled(True)
            self.stop_btn.setEnabled(False)

    # -----------------------------------------------------------------------
    # Chat display helpers
    # -----------------------------------------------------------------------

    def _append_bubble(self, role: str, content: str):
        """Append a styled message bubble to the chat display."""
        colors = {
            "user":      ("#1a3a5c", "#d0e8ff", "You"),
            "assistant": ("#1a3d1a", "#d0ffd0", "AI"),
            "error":     ("#5c1a1a", "#ffd0d0", "Error"),
        }
        bg, fg, label = colors.get(role, ("#333", "#eee", role.title()))

        escaped = content.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\n", "<br>")

        html = (
            f'<div style="margin:6px 2px; padding:8px 12px; '
            f'background:{bg}; border-radius:6px; color:{fg};">'
            f'<b style="font-size:10px; opacity:0.7;">{label}</b><br>'
            f'<span style="font-family:Courier New; font-size:10px;">{escaped}</span>'
            f'</div>'
        )
        self.chat_display.append(html)
        self.chat_display.verticalScrollBar().setValue(
            self.chat_display.verticalScrollBar().maximum()
        )

    def _refresh_last_assistant_bubble(self, content: str):
        """Rebuild the entire chat HTML to update the last assistant message."""
        session = self.sessions[self.current_session_index]
        self.chat_display.clear()
        for msg in session["messages"][:-1]:   # all but last (already committed)
            self._append_bubble(msg["role"], msg["content"])
        # Now the streaming one:
        self._append_bubble("assistant", content)

    # -----------------------------------------------------------------------
    # Model management
    # -----------------------------------------------------------------------

    def _refresh_model_list(self):
        base_url = self.url_input.text().rstrip("/") if hasattr(self, 'url_input') else OLLAMA_BASE_URL
        models = _fetch_models(base_url)
        if hasattr(self, 'model_combo'):
            self.model_combo.blockSignals(True)
            self.model_combo.clear()
            if models:
                self.model_combo.addItems(models)
                if self.selected_model in models:
                    self.model_combo.setCurrentText(self.selected_model)
                else:
                    self.selected_model = models[0]
                    self.model_combo.setCurrentIndex(0)
            else:
                self.model_combo.addItem("(no models found)")
            self.model_combo.blockSignals(False)
        self._update_ollama_status()

    def _on_model_changed(self, model: str):
        self.selected_model = model

    def _update_ollama_status(self):
        base_url = self.url_input.text().rstrip("/") if hasattr(self, 'url_input') else OLLAMA_BASE_URL
        running = _ollama_running(base_url)
        if hasattr(self, 'ollama_status'):
            if running:
                self.ollama_status.setText("● Ollama running")
                self.ollama_status.setStyleSheet("color: #4caf50;")
            else:
                self.ollama_status.setText("● Ollama offline")
                self.ollama_status.setStyleSheet("color: #f44336;")
        if hasattr(self, 'status_label'):
            self.status_label.setText(
                f"Ollama: {'connected' if running else 'not running  — start with: ollama serve'}"
            )

    # -----------------------------------------------------------------------
    # eventFilter: intercept Ctrl+Enter in input box
    # -----------------------------------------------------------------------

    def eventFilter(self, obj, event):
        from PyQt6.QtCore import QEvent
        # Guard: input_box may not exist yet during early toolbar init
        if hasattr(self, 'input_box') and obj is self.input_box:
            if event.type() == QEvent.Type.KeyPress:
                from PyQt6.QtGui import QKeyEvent
                ke: QKeyEvent = event
                if (ke.key() in (Qt.Key.Key_Return, Qt.Key.Key_Enter) and
                        ke.modifiers() & Qt.KeyboardModifier.ControlModifier):
                    self._send_message()
                    return True
        return super().eventFilter(obj, event)

    # -----------------------------------------------------------------------
    # Settings dialog
    # -----------------------------------------------------------------------

    def _show_workshop_settings(self):
        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(500)
        dialog.setMinimumHeight(400)

        layout = QVBoxLayout(dialog)
        tabs = QTabWidget()

        # --- Fonts tab ---
        fonts_tab = QWidget()
        fl = QVBoxLayout(fonts_tab)

        for label_text, attr in [
            ("Chat Font",   "chat_font"),
            ("Panel Font",  "panel_font"),
            ("Button Font", "button_font"),
        ]:
            g = QGroupBox(label_text)
            gl = QHBoxLayout(g)
            combo = QFontComboBox()
            combo.setCurrentFont(getattr(self, attr))
            spin = QSpinBox(); spin.setRange(7, 20); spin.setValue(getattr(self, attr).pointSize())
            gl.addWidget(combo); gl.addWidget(spin)
            fl.addWidget(g)
            # store refs so apply can read them
            setattr(dialog, f"_{attr}_combo", combo)
            setattr(dialog, f"_{attr}_spin",  spin)

        fl.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # --- General tab ---
        gen_tab = QWidget()
        gtl = QFormLayout(gen_tab)

        url_edit = QLineEdit(self.url_input.text())
        gtl.addRow("Ollama URL:", url_edit)

        tabs.addTab(gen_tab, "General")
        layout.addWidget(tabs)

        # Buttons
        btns = QHBoxLayout()
        btns.addStretch()

        apply_btn = QPushButton("Apply")
        def _apply():
            for attr in ("chat_font", "panel_font", "button_font"):
                combo = getattr(dialog, f"_{attr}_combo")
                spin  = getattr(dialog, f"_{attr}_spin")
                setattr(self, attr, QFont(combo.currentFont().family(), spin.value()))
            self.url_input.setText(url_edit.text())
            self._update_ollama_status()
        apply_btn.clicked.connect(_apply)
        btns.addWidget(apply_btn)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.close)
        btns.addWidget(close_btn)
        layout.addLayout(btns)

        dialog.exec()

    # -----------------------------------------------------------------------
    # Theme / AppSettings
    # -----------------------------------------------------------------------

    def _apply_theme(self):
        try:
            app_settings = self.app_settings
            if not app_settings and self.main_window:
                app_settings = getattr(self.main_window, 'app_settings', None)
            if app_settings:
                self.setStyleSheet(app_settings.get_stylesheet())
            else:
                self.setStyleSheet("""
                    QWidget { background-color: #2b2b2b; color: #e0e0e0; }
                    QTextEdit, QListWidget { background-color: #1e1e1e; border: 1px solid #3a3a3a; }
                    QGroupBox { border: 1px solid #3a3a3a; margin-top: 6px; }
                """)
        except Exception as e:
            print(f"[AI Workshop] Theme error: {e}")

    def _refresh_icons(self):
        SVGIconFactory.clear_cache()

    def _launch_theme_settings(self):
        try:
            if not APPSETTINGS_AVAILABLE:
                return
            dialog = SettingsDialog(self.app_settings, self)
            dialog.themeChanged.connect(lambda _: self._apply_theme())
            if dialog.exec():
                self._apply_theme()
        except Exception as e:
            QMessageBox.warning(self, "Theme Error", str(e))

    # -----------------------------------------------------------------------
    # Window management (mirrors COL Workshop)
    # -----------------------------------------------------------------------

    def _get_icon_color(self):
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            return colors.get('text_primary', '#ffffff')
        return '#ffffff'

    def toggle_dock_mode(self):
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _dock_to_main(self):
        self.is_docked = True
        self.show(); self.raise_()

    def _undock_from_main(self):
        self.setWindowFlags(Qt.WindowType.Window)
        self.is_docked = False
        self.resize(1200, 800)
        self.show(); self.raise_()

    def _toggle_tearoff(self):
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

    def _toggle_maximize(self):
        if self.isMaximized():
            self.showNormal()
        else:
            self.showMaximized()

    def closeEvent(self, event):
        self.window_closed.emit()
        event.accept()

    # -----------------------------------------------------------------------
    # Corner resize + dragging (identical pattern to COL Workshop)
    # -----------------------------------------------------------------------

    def _get_resize_corner(self, pos):
        size = self.corner_size; w = self.width(); h = self.height()
        if pos.x() < size and pos.y() < size:           return "top-left"
        if pos.x() > w - size and pos.y() < size:       return "top-right"
        if pos.x() < size and pos.y() > h - size:       return "bottom-left"
        if pos.x() > w - size and pos.y() > h - size:   return "bottom-right"
        return None

    def _update_cursor(self, direction):
        cursors = {
            "top":          Qt.CursorShape.SizeVerCursor,
            "bottom":       Qt.CursorShape.SizeVerCursor,
            "left":         Qt.CursorShape.SizeHorCursor,
            "right":        Qt.CursorShape.SizeHorCursor,
            "top-left":     Qt.CursorShape.SizeFDiagCursor,
            "bottom-right": Qt.CursorShape.SizeFDiagCursor,
            "top-right":    Qt.CursorShape.SizeBDiagCursor,
            "bottom-left":  Qt.CursorShape.SizeBDiagCursor,
        }
        self.setCursor(cursors.get(direction, Qt.CursorShape.ArrowCursor))

    def _is_on_draggable_area(self, pos):
        if not hasattr(self, 'titlebar'):
            return False
        if not self.titlebar.rect().contains(pos):
            return False
        for w in self.titlebar.findChildren(QPushButton):
            if w.isVisible() and w.geometry().contains(pos):
                return False
        return True

    def mousePressEvent(self, event):
        if event.button() != Qt.MouseButton.LeftButton:
            return super().mousePressEvent(event)
        pos = event.pos()
        self.resize_corner = self._get_resize_corner(pos)
        if self.resize_corner:
            self.resizing = True
            self.drag_position = event.globalPosition().toPoint()
            self.initial_geometry = self.geometry()
            event.accept(); return
        if hasattr(self, 'titlebar') and self.titlebar.geometry().contains(pos):
            tb_pos = self.titlebar.mapFromParent(pos)
            if self._is_on_draggable_area(tb_pos):
                handle = self.windowHandle()
                if handle:
                    handle.startSystemMove()
                event.accept(); return
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event):
        if event.buttons() == Qt.MouseButton.LeftButton:
            if self.resizing and self.resize_corner:
                self._handle_corner_resize(event.globalPosition().toPoint())
                event.accept(); return
        else:
            corner = self._get_resize_corner(event.pos())
            if corner != self.hover_corner:
                self.hover_corner = corner
                self.update()
            self._update_cursor(corner)
        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event):
        if event.button() == Qt.MouseButton.LeftButton:
            self.dragging = self.resizing = False
            self.resize_corner = None
            self.setCursor(Qt.CursorShape.ArrowCursor)
            event.accept()

    def _handle_corner_resize(self, global_pos):
        if not self.resize_corner or not self.drag_position:
            return
        delta = global_pos - self.drag_position
        geometry = self.initial_geometry
        min_w, min_h = 800, 500
        if self.resize_corner == "bottom-right":
            nw = geometry.width() + delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.resize(nw, nh)
        elif self.resize_corner == "bottom-left":
            nx = geometry.x() + delta.x()
            nw = geometry.width() - delta.x()
            nh = geometry.height() + delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, geometry.y(), nw, nh)
        elif self.resize_corner == "top-right":
            ny = geometry.y() + delta.y()
            nw = geometry.width() + delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(geometry.x(), ny, nw, nh)
        elif self.resize_corner == "top-left":
            nx = geometry.x() + delta.x()
            ny = geometry.y() + delta.y()
            nw = geometry.width() - delta.x()
            nh = geometry.height() - delta.y()
            if nw >= min_w and nh >= min_h:
                self.setGeometry(nx, ny, nw, nh)

    def paintEvent(self, event):
        super().paintEvent(event)
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if self.app_settings:
            colors = self.app_settings.get_theme_colors()
            accent = QColor(colors.get('accent_primary', '#1976d2'))
        else:
            accent = QColor(100, 150, 255)
        accent.setAlpha(180)
        hover = QColor(accent); hover.setAlpha(255)
        w, h, size = self.width(), self.height(), self.corner_size
        corners = {
            'top-left':     [(0,0),(size,0),(0,size)],
            'top-right':    [(w,0),(w-size,0),(w,size)],
            'bottom-left':  [(0,h),(size,h),(0,h-size)],
            'bottom-right': [(w,h),(w-size,h),(w,h-size)],
        }
        for name, pts in corners.items():
            path = QPainterPath()
            path.moveTo(*pts[0]); path.lineTo(*pts[1]); path.lineTo(*pts[2])
            path.closeSubpath()
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(hover if self.hover_corner == name else accent))
            painter.drawPath(path)
        painter.end()

    def resizeEvent(self, event):
        super().resizeEvent(event)

    # -----------------------------------------------------------------------
    # Hotkeys
    # -----------------------------------------------------------------------

    def _setup_hotkeys(self):
        QShortcut(QKeySequence("Ctrl+N"), self).activated.connect(self._new_session_action)
        QShortcut(QKeySequence("Ctrl+W"), self).activated.connect(self.close)
        QShortcut(QKeySequence.StandardKey.Preferences, self).activated.connect(self._show_workshop_settings)


# ---------------------------------------------------------------------------
# Public factory functions (mirrors col_workshop.py pattern)
# ---------------------------------------------------------------------------

def open_ai_workshop(main_window=None) -> AIWorkshop:
    """Open AI Workshop standalone or embedded."""
    try:
        workshop = AIWorkshop(None, main_window)
        workshop.setWindowFlags(Qt.WindowType.Window)
        workshop.setWindowTitle(App_name)
        workshop.resize(1300, 800)
        workshop.show()
        return workshop
    except Exception as e:
        if main_window:
            QMessageBox.critical(main_window, "AI Workshop Error", str(e))
        return None


# ---------------------------------------------------------------------------
# Standalone entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    import traceback

    print(App_name + " starting…")
    try:
        app = QApplication(sys.argv)
        w = AIWorkshop()
        w.setWindowTitle(App_name + " – Standalone")
        w.resize(1300, 800)
        w.show()
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)
