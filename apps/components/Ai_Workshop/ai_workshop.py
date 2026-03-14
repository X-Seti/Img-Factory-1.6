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
        self._stop_flag  = False
        self.setTerminationEnabled(True)

    def stop(self):
        """Request clean stop."""
        self._stop_flag = True

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
                    if self._stop_flag:
                        break
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
            if not self._stop_flag:
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
        self._current_response   = ""
        self._pending_attachments: list[dict] = []  # files queued for next send

        # Settings
        self.selected_model   = ""
        self.temperature      = 0.7
        self.max_tokens       = 2048
        self.system_prompt    = (
            "You are a helpful coding assistant specialising in Python, "
            "PyQt6, and GTA modding tools. Be concise and practical."
        )
        self.sessions_dir     = os.path.expanduser("~/.config/imgfactory/ai_sessions")

        # SSH settings
        self.ssh_config = {
            "host":      "127.0.0.1",
            "port":      22,
            "username":  "",
            "password":  "",
            "key_path":  "",
            "root_path": "/home",
        }
        self._ssh = None   # SSHFileAccess instance, lazy-init

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

        # Frameless in standalone (custom titlebar), widget when docked
        if self.standalone_mode:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        else:
            self.setWindowFlags(Qt.WindowType.Widget)

        if parent:
            p = parent.pos()
            self.move(p.x() + 50, p.y() + 80)

        self.setup_ui()
        self._load_sessions_from_disk()   # load persisted sessions AFTER widgets exist
        self._refresh_session_list()
        self._load_session(self.current_session_index)
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
        toolbar.setVisible(self.standalone_mode)   # shown standalone, hidden docked
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

        # Theme-aware icon colour
        icon_color = self._get_icon_color()

        # Settings button (standalone only — docked uses right-panel button)
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))
        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("AI Workshop Settings")
        self.settings_btn.setVisible(self.standalone_mode)
        layout.addWidget(self.settings_btn)

        layout.addStretch()

        # AI icon + title in centre
        title_row = QHBoxLayout()
        title_row.setSpacing(6)
        ai_icon_lbl = QLabel()
        if ICONS_AVAILABLE:
            pix = SVGIconFactory.ai_icon(20, icon_color).pixmap(20, 20)
            ai_icon_lbl.setPixmap(pix)
        title_row.addWidget(ai_icon_lbl)
        self.title_label = QLabel(App_name)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_row.addWidget(self.title_label)
        layout.addLayout(title_row)

        layout.addStretch()

        # New session
        self.new_session_btn = QPushButton()
        self.new_session_btn.setFont(self.button_font)
        self.new_session_btn.setIcon(SVGIconFactory.add_icon(20, icon_color))
        self.new_session_btn.setText("New Session")
        self.new_session_btn.setIconSize(QSize(20, 20))
        self.new_session_btn.clicked.connect(self._new_session_action)
        self.new_session_btn.setToolTip("Start a new chat session (Ctrl+N)")
        layout.addWidget(self.new_session_btn)

        # Clear chat
        self.clear_btn = QPushButton()
        self.clear_btn.setFont(self.button_font)
        self.clear_btn.setIcon(SVGIconFactory.delete_icon(20, icon_color))
        self.clear_btn.setText("Clear")
        self.clear_btn.setIconSize(QSize(20, 20))
        self.clear_btn.clicked.connect(self._clear_current_session)
        self.clear_btn.setToolTip("Clear current session messages")
        layout.addWidget(self.clear_btn)

        # Theme / Properties
        self.properties_btn = QPushButton()
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(20, icon_color))
        self.properties_btn.setIconSize(QSize(20, 20))
        self.properties_btn.setFixedSize(35, 35)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        layout.addWidget(self.properties_btn)

        # Dock button — hidden in standalone (nothing to dock to)
        self.dock_btn = QPushButton("D")
        self.dock_btn.setMinimumWidth(40)
        self.dock_btn.setMaximumWidth(40)
        self.dock_btn.setMinimumHeight(30)
        self.dock_btn.setToolTip("Dock into IMG Factory")
        self.dock_btn.clicked.connect(self.toggle_dock_mode)
        self.dock_btn.setVisible(not self.standalone_mode)
        layout.addWidget(self.dock_btn)

        # Tear-off button — only when docked
        if not self.standalone_mode:
            self.tearoff_btn = QPushButton("T")
            self.tearoff_btn.setMinimumWidth(40)
            self.tearoff_btn.setMaximumWidth(40)
            self.tearoff_btn.setMinimumHeight(30)
            self.tearoff_btn.clicked.connect(self._toggle_tearoff)
            self.tearoff_btn.setToolTip("Tear off to standalone window")
            layout.addWidget(self.tearoff_btn)

        # Window controls (standalone only)
        if self.standalone_mode:
            for attr, icon_method, slot, tip in [
                ('minimize_btn', 'minimize_icon', self.showMinimized,   "Minimize"),
                ('maximize_btn', 'maximize_icon', self._toggle_maximize, "Maximize"),
                ('close_btn',    'close_icon',    self.close,            "Close"),
            ]:
                btn = QPushButton()
                btn.setIcon(getattr(SVGIconFactory, icon_method)(20, icon_color))
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
        panel.setMinimumWidth(180)
        panel.setMaximumWidth(280)

        layout = QVBoxLayout(panel)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(4)

        # Header row
        hdr = QHBoxLayout()
        header = QLabel("Sessions")
        header.setFont(QFont("Arial", 10, QFont.Weight.Bold))
        hdr.addWidget(header)
        hdr.addStretch()

        new_btn = QPushButton("+")
        new_btn.setFixedSize(24, 24)
        new_btn.setToolTip("New session")
        new_btn.clicked.connect(self._new_session_action)
        hdr.addWidget(new_btn)
        layout.addLayout(hdr)

        # Search bar
        self.session_search = QLineEdit()
        self.session_search.setPlaceholderText("Search sessions…")
        self.session_search.setFont(self.panel_font)
        self.session_search.textChanged.connect(self._on_session_search)
        layout.addWidget(self.session_search)

        # Session list
        self.session_list = QListWidget()
        self.session_list.setAlternatingRowColors(True)
        self.session_list.currentRowChanged.connect(self._on_session_selected)
        self.session_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.session_list.customContextMenuRequested.connect(self._session_context_menu)
        self.session_list.setToolTip("Right-click for options")
        layout.addWidget(self.session_list)

        # Bottom buttons
        btn_row = QHBoxLayout()
        export_btn = QPushButton("Export")
        export_btn.setFont(self.button_font)
        export_btn.setToolTip("Export current session")
        export_btn.clicked.connect(self._export_current_session)
        btn_row.addWidget(export_btn)

        del_btn = QPushButton("Delete")
        del_btn.setFont(self.button_font)
        del_btn.setToolTip("Delete current session")
        del_btn.clicked.connect(lambda: self._delete_session(self.current_session_index))
        btn_row.addWidget(del_btn)
        layout.addLayout(btn_row)

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
        self.status_label.setObjectName("status_label")
        layout.addWidget(self.status_label)

        # Chat display
        self.chat_display = QTextEdit()
        self.chat_display.setReadOnly(True)
        self.chat_display.setFont(self.chat_font)
        self.chat_display.document().setDefaultFont(self.chat_font)
        self.chat_display.setPlaceholderText("Messages will appear here…")
        layout.addWidget(self.chat_display, stretch=1)

        # Typing indicator
        self.typing_label = QLabel("")
        self.typing_label.setFont(QFont("Arial", 9))
        self.typing_label.setObjectName("typing_label")
        layout.addWidget(self.typing_label)

        # Attachment chips area (hidden when empty)
        self.attachments_frame = QFrame()
        self.attachments_layout = QHBoxLayout(self.attachments_frame)
        self.attachments_layout.setContentsMargins(0, 2, 0, 2)
        self.attachments_layout.setSpacing(4)
        self.attachments_layout.addStretch()
        self.attachments_frame.setVisible(False)
        layout.addWidget(self.attachments_frame)

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

        btn_col = QVBoxLayout()
        self.send_btn = QPushButton("Send")
        self.send_btn.setFont(self.button_font)
        self.send_btn.setFixedWidth(70)
        self.send_btn.setMinimumHeight(30)
        self.send_btn.clicked.connect(self._send_message)
        self.send_btn.setToolTip("Send message (Ctrl+Enter)")
        btn_col.addWidget(self.send_btn)

        self.stop_btn = QPushButton("Stop")
        self.stop_btn.setFont(self.button_font)
        self.stop_btn.setFixedWidth(70)
        self.stop_btn.setEnabled(False)
        self.stop_btn.clicked.connect(self._stop_generation)
        self.stop_btn.setToolTip("Stop generation")
        btn_col.addWidget(self.stop_btn)

        # Attach local file
        attach_btn = QPushButton("📎")
        attach_btn.setFixedWidth(70)
        attach_btn.setFont(self.button_font)
        attach_btn.setToolTip("Attach local file")
        attach_btn.clicked.connect(self._attach_local_file)
        btn_col.addWidget(attach_btn)

        # Attach via SSH
        ssh_btn = QPushButton("🔗")
        ssh_btn.setFixedWidth(70)
        ssh_btn.setFont(self.button_font)
        ssh_btn.setToolTip("Attach file via SSH")
        ssh_btn.clicked.connect(self._attach_ssh_file)
        btn_col.addWidget(ssh_btn)

        btn_col.addStretch()
        input_layout.addLayout(btn_col)
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

        # Chat Display options
        display_group = QGroupBox("Chat Display")
        display_layout = QFormLayout(display_group)

        # Text size spinner
        self.chat_size_spin = QSpinBox()
        self.chat_size_spin.setRange(8, 24)
        self.chat_size_spin.setValue(self.chat_font.pointSize())
        self.chat_size_spin.setSuffix(" pt")
        self.chat_size_spin.setToolTip("Chat message text size")
        self.chat_size_spin.valueChanged.connect(self._on_chat_size_changed)
        display_layout.addRow("Text size:", self.chat_size_spin)

        # Font family
        self.chat_font_combo = QFontComboBox()
        self.chat_font_combo.setCurrentFont(self.chat_font)
        self.chat_font_combo.setToolTip("Chat message font")
        self.chat_font_combo.currentFontChanged.connect(self._on_chat_font_changed)
        display_layout.addRow("Font:", self.chat_font_combo)

        layout.addWidget(display_group)

        # Ollama URL
        url_group = QGroupBox("Ollama")
        url_layout = QFormLayout(url_group)

        self.url_input = QLineEdit(OLLAMA_BASE_URL)  # http://localhost:11434
        self.url_input.setFont(self.panel_font)
        self.url_input.setToolTip("Ollama base URL")
        url_layout.addRow("URL:", self.url_input)

        layout.addWidget(url_group)

        layout.addStretch()

        # Ollama status indicator
        self.ollama_status = QLabel("● Checking…")
        self.ollama_status.setFont(QFont("Arial", 9))
        layout.addWidget(self.ollama_status)

        # Settings button — only shown when docked (toolbar is hidden in docked mode)
        self.docked_settings_btn = QPushButton()
        self.docked_settings_btn.setFont(self.button_font)
        self.docked_settings_btn.setIcon(self.icon_factory.settings_icon())
        self.docked_settings_btn.setText("Settings / Options")
        self.docked_settings_btn.setIconSize(QSize(18, 18))
        self.docked_settings_btn.clicked.connect(self._show_workshop_settings)
        self.docked_settings_btn.setToolTip("AI Workshop Settings")
        self.docked_settings_btn.setVisible(not self.standalone_mode)
        layout.addWidget(self.docked_settings_btn)

        return panel

    # -----------------------------------------------------------------------
    # Session management
    # -----------------------------------------------------------------------

    # -----------------------------------------------------------------------
    # Session management — backed by SessionManager
    # -----------------------------------------------------------------------

    def _get_session_manager(self):
        """Lazy-init SessionManager using current sessions_dir setting."""
        from apps.components.Ai_Workshop.depends.session_manager import SessionManager
        return SessionManager(self.sessions_dir)

    def _new_session(self, name: str = "") -> dict:
        sm = self._get_session_manager()
        session = sm.new_session(name)
        self.sessions.append(session)
        self.current_session_index = len(self.sessions) - 1
        sm.save_session(session)
        return session

    def _new_session_action(self):
        self._new_session()
        self._refresh_session_list()
        if hasattr(self, 'session_list'):
            self.session_list.setCurrentRow(self.current_session_index)
        self._load_session(self.current_session_index)

    def _load_sessions_from_disk(self):
        """Load all persisted sessions from disk on startup."""
        try:
            sm = self._get_session_manager()
            loaded = sm.load_all()
            if loaded:
                self.sessions = loaded
                self.current_session_index = 0
            else:
                self._new_session()
        except Exception as e:
            print(f"[AI Workshop] load sessions error: {e}")
            self._new_session()

    def _save_current_session(self):
        """Persist current session to disk."""
        if self.current_session_index < 0 or self.current_session_index >= len(self.sessions):
            return
        try:
            sm = self._get_session_manager()
            sm.save_session(self.sessions[self.current_session_index])
        except Exception as e:
            print(f"[AI Workshop] save session error: {e}")

    def _refresh_session_list(self):
        if not hasattr(self, 'session_list'):
            return
        # Apply search filter if active
        query = self.session_search.text() if hasattr(self, 'session_search') else ""
        if query.strip():
            try:
                sm = self._get_session_manager()
                visible = sm.search(query, self.sessions)
            except Exception:
                visible = self.sessions
        else:
            visible = self.sessions

        self.session_list.blockSignals(True)
        self.session_list.clear()
        for s in visible:
            pin  = "⭐ " if s.get("pinned") else ""
            name = s.get("name", "Unnamed")
            msgs = len(s.get("messages", []))
            item = QListWidgetItem(f"{pin}{name}  ({msgs})")
            item.setData(Qt.ItemDataRole.UserRole, s.get("id"))
            self.session_list.addItem(item)
        self.session_list.blockSignals(False)

        # Re-select current
        if self.current_session_index >= 0:
            # Find the item matching current session id
            cur_id = self.sessions[self.current_session_index].get("id") if self.sessions else None
            for i in range(self.session_list.count()):
                if self.session_list.item(i).data(Qt.ItemDataRole.UserRole) == cur_id:
                    self.session_list.setCurrentRow(i)
                    break

    def _on_session_search(self, text: str):
        self._refresh_session_list()

    def _on_session_selected(self, row: int):
        if not hasattr(self, 'session_list') or row < 0:
            return
        item = self.session_list.item(row)
        if not item:
            return
        sid = item.data(Qt.ItemDataRole.UserRole)
        for i, s in enumerate(self.sessions):
            if s.get("id") == sid:
                self.current_session_index = i
                self._load_session(i)
                return

    def _load_session(self, index: int):
        if not hasattr(self, 'chat_display'):
            return
        if index < 0 or index >= len(self.sessions):
            return
        session = self.sessions[index]
        self.chat_display.clear()
        for msg in session.get("messages", []):
            self._append_bubble(msg["role"], msg["content"])

    def _session_context_menu(self, pos):
        item = self.session_list.itemAt(pos)
        if not item:
            return
        sid = item.data(Qt.ItemDataRole.UserRole)
        row = next((i for i, s in enumerate(self.sessions) if s.get("id") == sid), -1)
        if row < 0:
            return

        menu = QMenu(self)
        rename_a  = menu.addAction("Rename")
        pin_a     = menu.addAction("Unpin ⭐" if self.sessions[row].get("pinned") else "Pin ⭐")
        menu.addSeparator()
        export_txt_a = menu.addAction("Export as .txt")
        export_md_a  = menu.addAction("Export as .md")
        menu.addSeparator()
        delete_a  = menu.addAction("Delete")

        action = menu.exec(self.session_list.mapToGlobal(pos))
        if action == rename_a:
            self._rename_session(row)
        elif action == pin_a:
            self._toggle_pin_session(row)
        elif action == export_txt_a:
            self._export_session(row, "txt")
        elif action == export_md_a:
            self._export_session(row, "md")
        elif action == delete_a:
            self._delete_session(row)

    def _rename_session(self, row: int):
        from PyQt6.QtWidgets import QInputDialog
        name, ok = QInputDialog.getText(self, "Rename Session",
                                        "Session name:",
                                        text=self.sessions[row].get("name", ""))
        if ok and name.strip():
            self.sessions[row]["name"] = name.strip()
            self._save_current_session()
            self._refresh_session_list()

    def _toggle_pin_session(self, row: int):
        self.sessions[row]["pinned"] = not self.sessions[row].get("pinned", False)
        # Re-sort: pinned first
        self.sessions.sort(key=lambda s: not s.get("pinned", False))
        self.current_session_index = next(
            (i for i, s in enumerate(self.sessions)
             if s.get("id") == self.sessions[row].get("id")), 0)
        self._save_current_session()
        self._refresh_session_list()

    def _delete_session(self, row: int):
        if row < 0 or row >= len(self.sessions):
            return
        if len(self.sessions) <= 1:
            QMessageBox.information(self, "Delete", "Cannot delete the only session.")
            return
        reply = QMessageBox.question(
            self, "Delete Session",
            f"Delete session '{self.sessions[row].get('name', '')}'?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        try:
            sm = self._get_session_manager()
            sm.delete_session(self.sessions[row].get("id", ""))
        except Exception:
            pass
        self.sessions.pop(row)
        self.current_session_index = max(0, min(row, len(self.sessions) - 1))
        self._refresh_session_list()
        self._load_session(self.current_session_index)

    def _clear_current_session(self):
        if self.current_session_index < 0 or not hasattr(self, 'chat_display'):
            return
        reply = QMessageBox.question(self, "Clear Session",
                                     "Clear all messages in this session?",
                                     QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply == QMessageBox.StandardButton.Yes:
            self.sessions[self.current_session_index]["messages"] = []
            self._save_current_session()
            self.chat_display.clear()
            self._refresh_session_list()

    def _export_current_session(self):
        if self.current_session_index < 0:
            return
        self._export_session(self.current_session_index, "md")

    def _export_session(self, row: int, fmt: str):
        from PyQt6.QtWidgets import QFileDialog
        session = self.sessions[row]
        name = session.get("name", "session").replace(" ", "_")
        default = os.path.join(os.path.expanduser("~"), f"{name}.{fmt}")
        path, _ = QFileDialog.getSaveFileName(
            self, f"Export Session as .{fmt}", default,
            f"{'Text' if fmt == 'txt' else 'Markdown'} (*.{fmt});;All Files (*)")
        if not path:
            return
        try:
            sm = self._get_session_manager()
            ok = sm.export_txt(session, path) if fmt == "txt" else sm.export_md(session, path)
            if ok:
                QMessageBox.information(self, "Export", f"Saved to:\n{path}")
            else:
                QMessageBox.warning(self, "Export", "Export failed.")
        except Exception as e:
            QMessageBox.warning(self, "Export Error", str(e))

    # -----------------------------------------------------------------------
    # File attachments
    # -----------------------------------------------------------------------

    def _attach_local_file(self):
        from PyQt6.QtWidgets import QFileDialog
        from apps.components.Ai_Workshop.depends.file_attachments import (
            read_local_file, create_attachment_chip)
        paths, _ = QFileDialog.getOpenFileNames(
            self, "Attach File(s)", os.path.expanduser("~"),
            "All Files (*);;Python (*.py);;Images (*.png *.jpg *.jpeg);;Text (*.txt *.cfg *.dat)")
        for path in paths:
            att = read_local_file(path)
            self._add_attachment(att)

    def _attach_ssh_file(self):
        """Open SSH file browser to pick a remote file."""
        if not self._ensure_ssh_connected():
            return
        self._show_ssh_browser()

    def _ensure_ssh_connected(self) -> bool:
        """Connect SSH if not already. Returns True if connected."""
        if self._ssh and getattr(self._ssh, 'connected', False):
            return True
        cfg = self.ssh_config
        if not cfg.get("username"):
            QMessageBox.warning(self, "SSH Not Configured",
                                "Configure SSH credentials in Settings → SSH tab first.")
            return False
        try:
            from apps.components.Ai_Workshop.depends.ssh_file_access import SSHFileAccess
            ssh = SSHFileAccess()
            ok, msg = ssh.connect(cfg["host"], cfg["port"],
                                  cfg["username"], cfg["password"], cfg["key_path"])
            if ok:
                self._ssh = ssh
                if hasattr(self, 'ollama_status'):
                    self.ollama_status.setText(f"SSH: {cfg['username']}@{cfg['host']}")
                return True
            else:
                QMessageBox.warning(self, "SSH Connection Failed", msg)
                return False
        except Exception as e:
            QMessageBox.warning(self, "SSH Error", str(e))
            return False

    def _show_ssh_browser(self):
        """Simple SSH directory browser dialog."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout,
                                      QListWidget, QListWidgetItem, QPushButton, QLabel)
        from apps.components.Ai_Workshop.depends.file_attachments import read_ssh_file

        dlg = QDialog(self)
        dlg.setWindowTitle("SSH File Browser")
        dlg.resize(500, 400)
        layout = QVBoxLayout(dlg)

        path_label = QLabel(self.ssh_config.get("root_path", "/home"))
        path_label.setFont(self.panel_font)
        layout.addWidget(path_label)

        file_list = QListWidget()
        layout.addWidget(file_list)

        current_path = [self.ssh_config.get("root_path", "/home")]

        def populate(path):
            file_list.clear()
            path_label.setText(path)
            ok, entries = self._ssh.list_dir(path)
            if not ok:
                return
            # Add parent dir entry
            if path != "/":
                up = QListWidgetItem("📁 ..")
                up.setData(Qt.ItemDataRole.UserRole, {"path": str(Path(path).parent), "is_dir": True})
                file_list.addItem(up)
            for e in entries:
                icon = "📁 " if e["is_dir"] else "📄 "
                size = f"  ({e['size'] // 1024} KB)" if not e["is_dir"] and e["size"] > 0 else ""
                item = QListWidgetItem(f"{icon}{e['name']}{size}")
                item.setData(Qt.ItemDataRole.UserRole, e)
                file_list.addItem(item)

        def on_double_click(item):
            entry = item.data(Qt.ItemDataRole.UserRole)
            if entry and entry.get("is_dir"):
                current_path[0] = entry["path"]
                populate(current_path[0])

        file_list.itemDoubleClicked.connect(on_double_click)
        populate(current_path[0])

        btn_row = QHBoxLayout()
        attach_btn = QPushButton("Attach Selected")
        cancel_btn = QPushButton("Cancel")
        btn_row.addWidget(attach_btn)
        btn_row.addWidget(cancel_btn)
        layout.addLayout(btn_row)

        def do_attach():
            item = file_list.currentItem()
            if not item:
                return
            entry = item.data(Qt.ItemDataRole.UserRole)
            if not entry or entry.get("is_dir"):
                return
            att = read_ssh_file(self._ssh, entry["path"])
            self._add_attachment(att)
            dlg.accept()

        attach_btn.clicked.connect(do_attach)
        cancel_btn.clicked.connect(dlg.reject)
        dlg.exec()

    def _add_attachment(self, attachment: dict):
        """Add attachment to pending list and show chip."""
        from apps.components.Ai_Workshop.depends.file_attachments import create_attachment_chip
        self._pending_attachments.append(attachment)
        chip = create_attachment_chip(attachment,
                                       on_remove=self._remove_attachment,
                                       parent=self)
        # Insert before the stretch
        self.attachments_layout.insertWidget(
            self.attachments_layout.count() - 1, chip)
        self.attachments_frame.setVisible(True)

        if attachment.get("error"):
            QMessageBox.warning(self, "Attachment Warning",
                                f"{attachment['name']}: {attachment['error']}")

    def _remove_attachment(self, attachment: dict):
        """Remove attachment chip and from pending list."""
        if attachment in self._pending_attachments:
            self._pending_attachments.remove(attachment)
        # Rebuild chip area
        self._rebuild_attachment_chips()

    def _rebuild_attachment_chips(self):
        """Redraw all attachment chips from pending list."""
        from apps.components.Ai_Workshop.depends.file_attachments import create_attachment_chip
        # Clear layout except stretch
        while self.attachments_layout.count() > 1:
            item = self.attachments_layout.takeAt(0)
            if item.widget():
                item.widget().deleteLater()
        for att in self._pending_attachments:
            chip = create_attachment_chip(att, on_remove=self._remove_attachment, parent=self)
            self.attachments_layout.insertWidget(
                self.attachments_layout.count() - 1, chip)
        self.attachments_frame.setVisible(bool(self._pending_attachments))

    # -----------------------------------------------------------------------
    # Backup helper
    # -----------------------------------------------------------------------

    def _offer_backup(self, path: str) -> bool:
        """
        Ask user to backup before writing. Returns True to proceed, False to cancel.
        Remembers 'don't ask again this session' if user chose Skip.
        """
        if getattr(self, '_backup_skipped_this_session', False):
            return True

        reply = QMessageBox.question(
            self, "Backup Before Writing",
            f"Create a tar.gz backup of:\n{path}\n\n"
            "This keeps only ONE backup (overwrites previous).\n\n"
            "Backup now?",
            QMessageBox.StandardButton.Yes |
            QMessageBox.StandardButton.No |
            QMessageBox.StandardButton.Cancel,
            QMessageBox.StandardButton.Yes)

        if reply == QMessageBox.StandardButton.Cancel:
            return False
        if reply == QMessageBox.StandardButton.No:
            self._backup_skipped_this_session = True
            return True

        # Do backup
        try:
            from apps.components.Ai_Workshop.depends.session_manager import SessionManager
            sm = SessionManager()
            ok, result = sm.backup_directory(path)
            if ok:
                QMessageBox.information(self, "Backup Created", f"Backup saved:\n{result}")
            else:
                ret = QMessageBox.warning(
                    self, "Backup Failed",
                    f"Backup failed: {result}\n\nProceed anyway?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
                return ret == QMessageBox.StandardButton.Yes
        except Exception as e:
            QMessageBox.warning(self, "Backup Error", str(e))
            return False
        return True

    # -----------------------------------------------------------------------
    # Chat logic
    # -----------------------------------------------------------------------

    def _send_message(self):
        text = self.input_box.toPlainText().strip()
        attachments = list(self._pending_attachments)

        if not text and not attachments:
            return
        if self.worker and self.worker.isRunning():
            return

        if self.current_session_index < 0:
            self._new_session_action()

        session = self.sessions[self.current_session_index]

        # Build display text (with attachment names listed)
        display_text = text
        if attachments:
            names = ", ".join(a["name"] for a in attachments)
            display_text = f"[📎 {names}]\n{text}".strip()

        # Build API content (text + file contents injected)
        try:
            from apps.components.Ai_Workshop.depends.file_attachments import build_message_content
            api_content = build_message_content(text, attachments)
        except Exception:
            api_content = display_text

        session["messages"].append({"role": "user", "content": display_text})
        self._append_bubble("user", display_text)
        self.input_box.clear()

        # Clear attachments
        self._pending_attachments.clear()
        self._rebuild_attachment_chips()

        # Save session after user message
        self._save_current_session()
        self._refresh_session_list()

        # Build API messages
        api_messages = []
        if self.system_prompt.strip():
            api_messages.append({"role": "system", "content": self.system_prompt})
        # History (use display text for history context)
        for msg in session["messages"][:-1]:
            api_messages.append({"role": msg["role"], "content": msg["content"]})
        # Latest message with full attachment content
        api_messages.append({"role": "user", "content": api_content})

        model = self.model_combo.currentText()
        if not model:
            QMessageBox.warning(self, "No Model", "Select a model in the Settings panel.")
            return

        base_url = self.url_input.text().rstrip("/")
        self.send_btn.setEnabled(False)
        self.stop_btn.setEnabled(True)
        self.typing_label.setText(f"{model} is thinking…")

        self._current_response = ""
        self._append_bubble("assistant", "")

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

    def _cleanup_worker(self):
        """Safely stop and wait for worker thread to finish."""
        if self.worker is not None:
            if self.worker.isRunning():
                self.worker.stop()          # set stop flag
                self.worker.quit()          # ask event loop to exit
                if not self.worker.wait(3000):  # wait up to 3s
                    self.worker.terminate() # hard kill if still stuck
                    self.worker.wait(1000)
            self.worker.deleteLater()
            self.worker = None

    def _on_response_done(self, full_response: str):
        session = self.sessions[self.current_session_index]
        session["messages"].append({"role": "assistant", "content": full_response})
        self._refresh_last_assistant_bubble(full_response)
        self.send_btn.setEnabled(True)
        self.stop_btn.setEnabled(False)
        self.typing_label.setText("")
        self._save_current_session()
        self._refresh_session_list()
        if self.worker:
            self.worker.deleteLater()
            self.worker = None

    def _on_error(self, error_msg: str):
        self.typing_label.setText("")
        self.send_btn.setEnabled(True)
        self.stop_btn.setEnabled(False)
        self._append_bubble("error", error_msg)
        if self.worker:
            self.worker.deleteLater()
            self.worker = None

    def _stop_generation(self):
        if self.worker and self.worker.isRunning():
            self._cleanup_worker()
            self.typing_label.setText("Stopped.")
            self.send_btn.setEnabled(True)
            self.stop_btn.setEnabled(False)

    # -----------------------------------------------------------------------
    # Chat display helpers
    # -----------------------------------------------------------------------

    def _bubble_colors(self, role: str) -> tuple:
        """Return (bg, fg, label) for a bubble role, using current theme colours."""
        colors = self.app_settings.get_theme_colors() if self.app_settings else {}

        bg_base   = colors.get('bg_secondary',  '#252525')
        accent    = colors.get('accent_primary', '#1976d2')
        text      = colors.get('text_primary',   '#e0e0e0')
        success   = colors.get('success',        '#4caf50')
        error_col = colors.get('error',          '#f44336')
        label_col = colors.get('text_secondary', '#aaaaaa')

        def _tint(hex_col: str, factor: float) -> str:
            """Mix hex_col into bg_base at factor strength."""
            try:
                c = QColor(hex_col)
                b = QColor(bg_base)
                r  = int(b.red()   * (1 - factor) + c.red()   * factor)
                g  = int(b.green() * (1 - factor) + c.green() * factor)
                bl = int(b.blue()  * (1 - factor) + c.blue()  * factor)
                return QColor(r, g, bl).name()
            except Exception:
                return bg_base

        # Always use text_primary as the message foreground — readable on any theme
        bubble_map = {
            "user":      (_tint(accent,    0.22), text, label_col, "You"),
            "assistant": (_tint(success,   0.18), text, label_col, "AI"),
            "error":     (_tint(error_col, 0.25), text, error_col, "Error"),
        }
        return bubble_map.get(role, (_tint(accent, 0.10), text, label_col, role.title()))

    def _append_bubble(self, role: str, content: str):
        """Append a styled message bubble to the chat display."""
        bg, fg, label_fg, label = self._bubble_colors(role)

        font_size   = self.chat_font.pointSize()
        font_family = self.chat_font.family()

        # Ensure QTextEdit document font matches chat_font
        # (QTextEdit.append ignores widget font for new HTML blocks without this)
        doc = self.chat_display.document()
        doc.setDefaultFont(self.chat_font)

        escaped = (content
                   .replace("&", "&amp;")
                   .replace("<", "&lt;")
                   .replace(">", "&gt;")
                   .replace("\n", "<br>"))

        html = (
            f'<div style="margin:5px 2px; padding:8px 12px; '
            f'background:{bg}; border-radius:6px; '
            f'border-left:3px solid {label_fg};">'
            f'<span style="font-size:{max(8, font_size - 2)}px; '
            f'color:{label_fg}; font-weight:bold; '
            f'font-family:Arial,sans-serif;">{label}</span><br>'
            f'<span style="font-family:{font_family}; font-size:{font_size}pt; '
            f'color:{fg};">{escaped}</span>'
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
        """Check Ollama connection and update status labels with theme colours."""
        base_url = self.url_input.text().rstrip("/") if hasattr(self, 'url_input') else OLLAMA_BASE_URL
        running  = _ollama_running(base_url)
        colors   = self.app_settings.get_theme_colors() if self.app_settings else {}
        success_col = colors.get('success',        '#4caf50')
        error_col   = colors.get('error',          '#f44336')
        secondary   = colors.get('text_secondary', '#aaaaaa')
        if hasattr(self, 'ollama_status'):
            if running:
                self.ollama_status.setText("● Ollama running")
                self.ollama_status.setStyleSheet(f"color: {success_col};")
            else:
                self.ollama_status.setText("● Ollama offline")
                self.ollama_status.setStyleSheet(f"color: {error_col};")
        if hasattr(self, 'status_label'):
            self.status_label.setStyleSheet(f"color: {secondary};")
            self.status_label.setText(
                f"Ollama: {'connected' if running else 'not running — start with: ollama serve'}"
            )

    def _on_chat_size_changed(self, size: int):
        """Live-update chat font size and redraw."""
        self.chat_font.setPointSize(size)
        if hasattr(self, 'chat_display'):
            self.chat_display.setFont(self.chat_font)
            self.chat_display.document().setDefaultFont(self.chat_font)
        if hasattr(self, 'input_box'):
            self.input_box.setFont(self.chat_font)
        self._redraw_chat()

    def _on_chat_font_changed(self, font):
        """Live-update chat font family and redraw."""
        self.chat_font.setFamily(font.family())
        if hasattr(self, 'chat_display'):
            self.chat_display.setFont(self.chat_font)
            self.chat_display.document().setDefaultFont(self.chat_font)
        if hasattr(self, 'input_box'):
            self.input_box.setFont(self.chat_font)
        self._redraw_chat()

    def _redraw_chat(self):
        """Redraw all bubbles in the current session with current font/theme."""
        if not hasattr(self, 'chat_display') or self.current_session_index < 0:
            return
        if self.current_session_index >= len(self.sessions):
            return
        self.chat_display.clear()
        for msg in self.sessions[self.current_session_index].get("messages", []):
            self._append_bubble(msg["role"], msg["content"])


        base_url = self.url_input.text().rstrip("/") if hasattr(self, 'url_input') else OLLAMA_BASE_URL
        running = _ollama_running(base_url)
        colors = self.app_settings.get_theme_colors() if self.app_settings else {}
        success_col = colors.get('success', '#4caf50')
        error_col   = colors.get('error',   '#f44336')
        secondary   = colors.get('text_secondary', '#aaaaaa')
        if hasattr(self, 'ollama_status'):
            if running:
                self.ollama_status.setText("● Ollama running")
                self.ollama_status.setStyleSheet(f"color: {success_col};")
            else:
                self.ollama_status.setText("● Ollama offline")
                self.ollama_status.setStyleSheet(f"color: {error_col};")
        if hasattr(self, 'status_label'):
            self.status_label.setStyleSheet(f"color: {secondary};")
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
        from PyQt6.QtWidgets import QFileDialog as QFD
        dialog = QDialog(self)
        dialog.setWindowTitle(App_name + " Settings")
        dialog.setMinimumWidth(560)
        dialog.setMinimumHeight(480)

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
            spin = QSpinBox(); spin.setRange(7, 20)
            spin.setValue(getattr(self, attr).pointSize())
            gl.addWidget(combo); gl.addWidget(spin)
            fl.addWidget(g)
            setattr(dialog, f"_{attr}_combo", combo)
            setattr(dialog, f"_{attr}_spin",  spin)
        fl.addStretch()
        tabs.addTab(fonts_tab, "Fonts")

        # --- General tab ---
        gen_tab = QWidget()
        gtl = QFormLayout(gen_tab)

        url_edit = QLineEdit(self.url_input.text() if hasattr(self, 'url_input') else OLLAMA_BASE_URL)
        gtl.addRow("Ollama URL:", url_edit)

        # Sessions directory
        sess_row = QHBoxLayout()
        sess_edit = QLineEdit(self.sessions_dir)
        sess_browse = QPushButton("…"); sess_browse.setFixedWidth(30)
        def _browse_sess():
            from PyQt6.QtWidgets import QFileDialog
            d = QFileDialog.getExistingDirectory(dialog, "Sessions Directory", self.sessions_dir)
            if d:
                sess_edit.setText(d)
        sess_browse.clicked.connect(_browse_sess)
        sess_row.addWidget(sess_edit); sess_row.addWidget(sess_browse)
        gtl.addRow("Sessions saved to:", sess_row)

        tabs.addTab(gen_tab, "General")

        # --- SSH tab ---
        ssh_tab = QWidget()
        ssh_layout = QVBoxLayout(ssh_tab)

        try:
            from apps.components.Ai_Workshop.depends.ssh_file_access import create_ssh_settings_widget
            ssh_group = create_ssh_settings_widget(dialog, self.ssh_config)
            ssh_layout.addWidget(ssh_group)
        except Exception as e:
            ssh_layout.addWidget(QLabel(f"SSH module error: {e}"))
            ssh_group = None

        # Test connection button
        test_btn = QPushButton("Test Connection")
        def _test_ssh():
            if not ssh_group:
                return
            vals = ssh_group.get_values()
            try:
                from apps.components.Ai_Workshop.depends.ssh_file_access import SSHFileAccess
                ssh = SSHFileAccess()
                ok, msg = ssh.connect(vals["host"], vals["port"],
                                       vals["username"], vals["password"], vals["key_path"])
                if ok:
                    ssh.disconnect()
                    QMessageBox.information(dialog, "SSH Test", f"✓ {msg}")
                else:
                    QMessageBox.warning(dialog, "SSH Test Failed", msg)
            except Exception as e:
                QMessageBox.warning(dialog, "SSH Error", str(e))
        test_btn.clicked.connect(_test_ssh)
        ssh_layout.addWidget(test_btn)

        paramiko_note = QLabel("Requires: pip install paramiko --break-system-packages")
        paramiko_note.setStyleSheet("color: #888; font-size: 10px; font-style: italic;")
        ssh_layout.addWidget(paramiko_note)
        ssh_layout.addStretch()
        tabs.addTab(ssh_tab, "SSH")

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
            if hasattr(self, 'url_input'):
                self.url_input.setText(url_edit.text())
            new_dir = sess_edit.text().strip()
            if new_dir and new_dir != self.sessions_dir:
                os.makedirs(new_dir, exist_ok=True)
                self.sessions_dir = new_dir
            if ssh_group:
                self.ssh_config = ssh_group.get_values()
                # Reconnect SSH with new settings
                if self._ssh:
                    try: self._ssh.disconnect()
                    except Exception: pass
                    self._ssh = None
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
                # Apply theme colours to status labels
                colors = app_settings.get_theme_colors()
                secondary = colors.get('text_secondary', '#aaaaaa')
                if hasattr(self, 'typing_label'):
                    self.typing_label.setStyleSheet(
                        f"color: {secondary}; font-style: italic;")
            else:
                self.setStyleSheet("""
                    QWidget { background-color: #2b2b2b; color: #e0e0e0; }
                    QTextEdit, QListWidget { background-color: #1e1e1e; border: 1px solid #3a3a3a; }
                    QGroupBox { border: 1px solid #3a3a3a; margin-top: 6px; }
                """)
            # Redraw chat bubbles with updated theme colours
            self._redraw_chat()
            self._update_ollama_status()
        except Exception as e:
            print(f"[AI Workshop] Theme error: {e}")

    def _refresh_icons(self):
        SVGIconFactory.clear_cache()
        # Re-apply theme-aware icon colour to toolbar buttons
        color = self._get_icon_color()
        icon_map = [
            ('settings_btn',    'settings_icon'),
            ('new_session_btn', 'add_icon'),
            ('clear_btn',       'delete_icon'),
            ('properties_btn',  'properties_icon'),
            ('docked_settings_btn', 'settings_icon'),
        ]
        for attr, method in icon_map:
            if hasattr(self, attr):
                btn = getattr(self, attr)
                btn.setIcon(getattr(SVGIconFactory, method)(20, color))
        # Window control buttons
        for attr, method in [('minimize_btn','minimize_icon'),
                              ('maximize_btn','maximize_icon'),
                              ('close_btn','close_icon')]:
            if hasattr(self, attr):
                getattr(self, attr).setIcon(getattr(SVGIconFactory, method)(20, color))

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
        self.standalone_mode = False
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(False)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(False)
        self.show(); self.raise_()

    def _undock_from_main(self):
        self.standalone_mode = True
        self.is_docked = False
        self.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        if hasattr(self, '_workshop_toolbar'):
            self._workshop_toolbar.setVisible(True)
        if hasattr(self, 'docked_settings_btn'):
            self.docked_settings_btn.setVisible(False)
        if hasattr(self, 'settings_btn'):
            self.settings_btn.setVisible(True)
        if hasattr(self, 'dock_btn'):
            self.dock_btn.setVisible(False)
        self.resize(1300, 800)
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
        self._cleanup_worker()
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
