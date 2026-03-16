#belongs in gui/gui_layout_custom.py - Version 6
# X-Seti - February04 2026 - Img Factory 1.6 - Custom UI Module

from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QTabWidget, QFrame,
    QGroupBox, QPushButton, QLabel, QCheckBox, QSizePolicy, QMenu
)
from PyQt6.QtCore import Qt, QObject, pyqtSignal, QSize, QTimer
from PyQt6.QtGui import QIcon
from .gui_layout import IMGFactoryGUILayout
from apps.methods.imgfactory_svg_icons import SVGIconFactory
from apps.methods.imgfactory_svg_icons import (
    get_add_icon, get_open_icon, get_refresh_icon, get_close_icon,
    get_save_icon, get_export_icon, get_import_icon, get_remove_icon,
    get_edit_icon, get_view_icon, get_search_icon, get_settings_icon,
    get_rebuild_icon, get_undobar_icon, get_undo_icon, get_redo_icon
)

from apps.gui.gui_menu_custom import CustomMenuManager, show_popup_menu_at_button

# Temporary 3D viewport placeholder
class COL3DViewport(QWidget):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setMinimumSize(400, 400)
        layout = QVBoxLayout(self)
        label = QLabel("3D Viewport - Placeholder")
        label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(label)
    def set_current_file(self, col_file): pass
    def set_view_options(self, **options): pass

VIEWPORT_AVAILABLE = False  # 3D viewport not yet implemented

# Add root directory to path
App_name = "Img Factory"
vers = "1.6"
DEBUG_STANDALONE = False

class TitleBarEventFilter(QObject):
    """Event filter for custom title bar to handle dragging"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.start_pos = None
    
    def eventFilter(self, obj, event):
        # Implement drag functionality if needed
        # For now, just return False to let events pass through normally
        return False

# Import AppSettings
try:
    from apps.utils.app_settings_system import AppSettings, SettingsDialog
    APPSETTINGS_AVAILABLE = True
except ImportError:
    APPSETTINGS_AVAILABLE = False
    print("Warning: AppSettings not available")


class ToolTaskbar(QWidget):  # vers 2
    """Tool taskbar — lives INSIDE the titlebar row between the title and undo.

    Each button:
      - Left-click  : raise / show the associated workshop window or tab
      - Right-click : context menu with Open / Close options
      - Active underline shown when the target widget is visible/raised

    register(key, label, icon, target, tooltip)
    unregister(key)
    update_target(key, target)
    set_active(key, bool)   — draw underline on the button
    """

    def __init__(self, main_window, parent=None):
        super().__init__(parent)
        self._main_window = main_window
        self._tools: dict = {}   # key → {label, icon, target, btn, active}
        self.setObjectName("tool_taskbar")
        self.setSizePolicy(
            QSizePolicy.Policy.Preferred,
            QSizePolicy.Policy.Fixed,
        )

        self._layout = QHBoxLayout(self)
        self._layout.setContentsMargins(2, 0, 2, 0)
        self._layout.setSpacing(2)

        # Start hidden — shown as soon as the first tool registers
        self.setVisible(False)

    # ── helpers ───────────────────────────────────────────────────────────────

    def _make_btn_style(self, active: bool, acc: str = "#1976d2",
                        txt: str = "#cccccc", bg: str = "transparent") -> str:
        underline = (
            f"border-bottom: 2px solid {acc}; border-radius:0px;"
        ) if active else "border-bottom: 2px solid transparent; border-radius:3px;"
        return (
            f"QPushButton#tbtn {{"
            f"  background:{bg}; color:{txt}; font-size:11px;"
            f"  padding:2px 6px; border:1px solid transparent; {underline}"
            f"}}"
            f"QPushButton#tbtn:hover {{"
            f"  background:{acc}33; border:1px solid {acc}; border-radius:3px;"
            f"  border-bottom: 2px solid {acc};"
            f"}}"
            f"QPushButton#tbtn:pressed {{ background:{acc}66; }}"
        )

    def _raise_target(self, key: str) -> None:
        """Raise / show the tool's widget or call its opener."""
        info = self._tools.get(key)
        if not info:
            return
        t = info["target"]
        if t is None:
            return

        # Dir tree button: toggle the content splitter
        if key == "dirtree":
            mw = self._main_window
            gl = getattr(mw, "gui_layout", None)
            splitter = getattr(gl, "content_splitter", None) if gl else None
            if splitter and splitter.count() >= 2:
                sizes = splitter.sizes()
                total = sum(sizes) or 10000
                # If tree is hidden or very small, show it in split mode
                if sizes[-1] < total * 0.1:
                    splitter.setSizes([total // 2, total // 2])
                    self._set_exclusive_active(key)
                else:
                    # Tree visible — hide it
                    splitter.setSizes([total, 0])
                    self.set_active(key, False)
                return

        if callable(t):
            t()
            self._set_exclusive_active(key)
            return
        if isinstance(t, QWidget):
            # If it's a docked tab, switch to it
            mw = self._main_window
            tw = getattr(mw, "main_tab_widget", None)
            if tw:
                for i in range(tw.count()):
                    w = tw.widget(i)
                    if w is t or (w and t in w.findChildren(QWidget)):
                        tw.setCurrentIndex(i)
                        self._set_exclusive_active(key)
                        return
            # Floating window
            if not t.isVisible():
                t.show()
            t.raise_()
            t.activateWindow()
            self._set_exclusive_active(key)

    def _context_menu(self, key: str, pos) -> None:
        """Right-click context menu for a taskbar button."""
        from PyQt6.QtWidgets import QMenu
        from PyQt6.QtGui import QAction
        info = self._tools.get(key)
        if not info:
            return

        menu = QMenu(self)
        menu.setTitle(info["label"])

        open_act = QAction(f"Open  {info['label']}", menu)
        open_act.triggered.connect(lambda: self._raise_target(key))
        menu.addAction(open_act)

        menu.addSeparator()

        close_act = QAction(f"Close  {info['label']}", menu)
        def _close():
            t = self._tools.get(key, {}).get("target")
            if key == "dirtree":
                # Hide the splitter panel rather than closing the widget
                mw = self._main_window
                gl = getattr(mw, "gui_layout", None)
                splitter = getattr(gl, "content_splitter", None) if gl else None
                if splitter and splitter.count() >= 2:
                    total = sum(splitter.sizes()) or 10000
                    splitter.setSizes([total, 0])
            elif isinstance(t, QWidget):
                mw = self._main_window
                tw = getattr(mw, "main_tab_widget", None)
                if tw:
                    for i in range(tw.count()):
                        if tw.widget(i) is t or t in tw.widget(i).findChildren(QWidget):
                            tw.removeTab(i)
                            break
                    else:
                        t.close()
                else:
                    t.close()
            self.unregister(key)
        close_act.triggered.connect(_close)
        menu.addAction(close_act)

        menu.exec(self.mapToGlobal(pos))

    # ── Public API ────────────────────────────────────────────────────────────

    def register(self, key: str, label: str, icon,
                 target, tooltip: str = "") -> None:
        """Add or update a tool button."""
        if key in self._tools:
            self.unregister(key)

        btn = QPushButton()
        btn.setIcon(icon)
        btn.setText(label)
        btn.setIconSize(QSize(16, 16))
        btn.setFixedHeight(28)
        btn.setMinimumWidth(52)
        btn.setMaximumWidth(110)
        btn.setFlat(False)
        btn.setToolTip(tooltip or label)
        btn.setObjectName("tbtn")
        btn.setStyleSheet(self._make_btn_style(False))

        btn.clicked.connect(lambda _=False, k=key: self._raise_target(k))

        btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        btn.customContextMenuRequested.connect(
            lambda pos, k=key: self._context_menu(k, pos))

        self._layout.addWidget(btn)

        self._tools[key] = {
            "label":  label,
            "icon":   icon,
            "target": target,
            "btn":    btn,
            "active": False,
        }
        self.setVisible(True)

    def unregister(self, key: str) -> None:
        """Remove a tool button."""
        if key not in self._tools:
            return
        btn = self._tools[key]["btn"]
        self._layout.removeWidget(btn)
        btn.deleteLater()
        del self._tools[key]
        if not self._tools:
            self.setVisible(False)

    def update_target(self, key: str, target) -> None:
        if key in self._tools:
            self._tools[key]["target"] = target

    def set_active(self, key: str, active: bool) -> None:
        """Draw / remove the active underline on a button."""
        if key not in self._tools:
            return
        self._tools[key]["active"] = active
        self._tools[key]["btn"].setStyleSheet(
            self._make_btn_style(active, acc=self._acc, txt=self._txt))

    def _set_exclusive_active(self, key: str) -> None:
        """Mark key as active, clear underline on all others."""
        for k in self._tools:
            self.set_active(k, k == key)

    def is_registered(self, key: str) -> bool:
        return key in self._tools

    def apply_theme(self, colors: dict) -> None:
        """Re-style all buttons from the live theme palette."""
        self._acc = colors.get("accent_primary", "#1976d2")
        self._txt = colors.get("text_primary",   "#cccccc")
        self._bg  = colors.get("bg_secondary",   "transparent")
        self.setStyleSheet(
            f"QWidget#tool_taskbar {{ background: transparent; }}"
        )
        for key, info in self._tools.items():
            info["btn"].setStyleSheet(
                self._make_btn_style(info["active"], self._acc, self._txt))

    # seed defaults so apply_theme isn't required before first use
    _acc = "#1976d2"
    _txt = "#cccccc"
    _bg  = "transparent"





class IMGFactoryGUILayoutCustom(IMGFactoryGUILayout):
    """Custom UI version of IMGFactoryGUILayout with modern theme and layout"""

    def __init__(self, main_window):
        super().__init__(main_window)

        # Load setting: use system titlebar by default
        self.use_system_titlebar = True
        if hasattr(self.main_window, 'app_settings'):
            self.use_system_titlebar = self.main_window.app_settings.current_settings.get('use_system_titlebar', False)

        # Apply initial window flags
        self._apply_window_flags()

        # ONLY set window-specific stuff in standalone mode

        self.window_always_on_top = False

        # Corner resize variables for standalone
        self.dragging = False
        self.drag_position = None
        self.resizing = False
        self.resize_corner = None
        self.corner_size = 20
        self.resize_margin = 10  # Edge margin for resize detection
        self.hover_corner = None
        self._initialize_features()
        self.enable_debug_check = False
        self.custom_menu_manager = CustomMenuManager(main_window)
        main_window.custom_menu_manager = self.custom_menu_manager


    @property
    def window_context(self): #vers 1
        """Get the appropriate window context for method calls

        Returns self.main_window if available (when embedded in IMG Factory),
        otherwise returns self (when running standalone)
        """
        return self.main_window if self.main_window is not None else self


    def apply_ui_mode(self, ui_mode: str, show_toolbar: bool, show_status_bar: bool, show_menu_bar: bool): #vers 4
        """Apply UI mode: 'system' or 'custom'"""
        # STORE MODE EARLY
        self.ui_mode = ui_mode

        # If toolbar exists, apply visibility immediately
        if hasattr(self, 'titlebar') and self.titlebar:
            self.titlebar.setVisible(True)  # Always visible

            # Control window buttons based on mode
            show_controls = (ui_mode == 'custom')
            if hasattr(self, 'minimize_btn'):
                self.minimize_btn.setVisible(show_controls)
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setVisible(show_controls)
            if hasattr(self, 'close_btn'):
                self.close_btn.setVisible(show_controls)

        # Menu bar visibility
        if hasattr(self.main_window, 'menuBar') and callable(self.main_window.menuBar):
            menu_bar = self.main_window.menuBar()
            if menu_bar:
                menu_bar.setVisible(show_menu_bar)

        # Status bar visibility
        if hasattr(self.main_window, 'statusBar') and callable(self.main_window.statusBar):
            status_bar = self.main_window.statusBar()
            if status_bar:
                status_bar.setVisible(show_status_bar)


    def create_main_ui_with_splitters(self, parent_layout): #vers 6
        """Create the main UI with toolbar for custom layout - TOOLBAR NOW IN _create_ui"""
        # NO LONGER CREATE TOOLBAR HERE - it's handled in imgfactory._create_ui

        # Create temp widget for main content
        temp_widget = QWidget()
        temp_layout = QVBoxLayout(temp_widget)
        temp_layout.setContentsMargins(0, 0, 0, 0)

        # Call parent method to create the main UI content
        super().create_main_ui_with_splitters(temp_layout)

        # Add the main content directly to parent layout
        parent_layout.addWidget(temp_widget)


        # NOW apply ui_mode visibility settings AFTER toolbar exists
        ui_mode = getattr(self, 'ui_mode', 'system')
        if ui_mode == 'custom':
            # Show window controls in custom mode
            if hasattr(self, 'minimize_btn'):
                self.minimize_btn.setVisible(True)
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setVisible(True)
            if hasattr(self, 'close_btn'):
                self.close_btn.setVisible(True)
        else:
            # Hide window controls in system mode
            if hasattr(self, 'minimize_btn'):
                self.minimize_btn.setVisible(False)
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setVisible(False)
            if hasattr(self, 'close_btn'):
                self.close_btn.setVisible(False)




    def _apply_window_flags(self): #vers 1
        """Apply window flags based on system/custom UI setting"""
        if not hasattr(self.main_window, 'setWindowFlags') or not hasattr(self.main_window, 'show'):
            return
        current_geometry = self.main_window.geometry()
        was_visible = self.main_window.isVisible()
        if self.use_system_titlebar:
            self.main_window.setWindowFlags(
                Qt.WindowType.Window |
                Qt.WindowType.WindowMinimizeButtonHint |
                Qt.WindowType.WindowMaximizeButtonHint |
                Qt.WindowType.WindowCloseButtonHint
            )
        else:
            self.main_window.setWindowFlags(Qt.WindowType.FramelessWindowHint)
        self.main_window.setGeometry(current_geometry)
        if was_visible:
            self.main_window.show()


    def toggle_maximize_restore(self):
        if self.main_window.isMaximized():
            self.main_window.showNormal()
        else:
            self.main_window.showMaximized()


    def _show_workshop_settings(self): #vers 4
        """Show IMG Factory Settings dialog - calls proper settings module"""
        try:
            from apps.methods.imgfactory_ui_settings import show_imgfactory_settings_dialog
            show_imgfactory_settings_dialog(self.main_window)
        except Exception as e:
            self.main_window.log_message(f"Error opening settings: {str(e)}")
            import traceback
            traceback.print_exc()


    def _create_toolbar(self):

        apply_btn.clicked.connect(apply_settings)
        button_layout.addWidget(apply_btn)

        # OK button (apply and close)
        ok_btn = QPushButton("OK")
        ok_btn.setStyleSheet("""
            QPushButton {
                background: #0078d4;
                color: white;
                padding: 8px 20px;
                font-weight: bold;
                font-size: 11px;
            }
            QPushButton:hover {
                background: #1984d8;
            }
        """)
        ok_btn.setDefault(True)

        def ok_clicked():
            apply_settings()
            dialog.accept()

        ok_btn.clicked.connect(ok_clicked)
        button_layout.addWidget(ok_btn)

        layout.addLayout(button_layout)

        # Show dialog
        dialog.exec()


    def _create_toolbar(self): #vers 7
        """Create toolbar - ALL BUTTONS CONNECTED"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        from PyQt6.QtGui import QFont

        if not hasattr(self, 'title_font'):
            self.title_font = QFont("Arial", 14)
        if not hasattr(self, 'button_font'):
            self.button_font = QFont("Arial", 10)
        if not hasattr(self, 'icon_factory'):
            self.icon_factory = SVGIconFactory()
        if not hasattr(globals(), 'App_name'):
            from apps.components.Img_Factory.imgfactory import App_name

        self.titlebar = QWidget()
        self.titlebar.setFixedHeight(45)
        self.titlebar.setObjectName("titlebar")

        # Install event filter for drag detection
        self.titlebar_event_filter = TitleBarEventFilter()
        self.titlebar.installEventFilter(self.titlebar_event_filter)
        self.titlebar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.titlebar.setMouseTracking(True)

        layout = QHBoxLayout(self.titlebar)
        layout.setContentsMargins(5, 5, 5, 5)
        layout.setSpacing(5)

        # Get icon color from theme text_primary
        icon_color = "#000000"
        if hasattr(self, 'main_window') and hasattr(self.main_window, 'app_settings'):
            current_theme = self.main_window.app_settings.current_settings.get("theme", "default")
            theme_colors = self.main_window.app_settings.get_theme_colors(current_theme)
            if theme_colors:
                icon_color = theme_colors.get('text_primary', '#000000')

        # M Menu button - BEFORE settings
        self.menu_btn = QPushButton()
        self.menu_btn.setFont(self.button_font)
        self.menu_btn.setIcon(self.icon_factory.menu_m_icon(20, icon_color))
        self.menu_btn.setText("Menu")
        self.menu_btn.setIconSize(QSize(20, 20))
        self.menu_btn.clicked.connect(self._show_popup_menu)
        self.menu_btn.setToolTip("Main Menu")
        layout.addWidget(self.menu_btn)

        # Settings button - PREVENT DUPLICATE CONNECTIONS
        self.settings_btn = QPushButton()
        self.settings_btn.setFont(self.button_font)
        self.settings_btn.setIcon(self.icon_factory.settings_icon(20, icon_color))
        self.settings_btn.setText("Settings")
        self.settings_btn.setIconSize(QSize(20, 20))

        try:
            self.settings_btn.clicked.disconnect()
        except (TypeError, RuntimeError):
            pass

        self.settings_btn.clicked.connect(self._show_workshop_settings)
        self.settings_btn.setToolTip("Img Factory Settings")
        layout.addWidget(self.settings_btn)

        # RW Reference button
        self.rw_ref_btn = QPushButton()
        self.rw_ref_btn.setFont(self.button_font)
        self.rw_ref_btn.setIcon(SVGIconFactory.info_icon(20, icon_color))
        self.rw_ref_btn.setText("RW Ref")
        self.rw_ref_btn.setIconSize(QSize(20, 20))
        self.rw_ref_btn.clicked.connect(self._show_rw_reference)
        self.rw_ref_btn.setToolTip("RenderWare Format Reference (versions, sections, texture formats, platforms)")
        layout.addWidget(self.rw_ref_btn)

        layout.addStretch()

        # App title in center
        try:
            from apps.app_info import App_build_num as _bnum
            _title_text = f"{App_name}  —  {_bnum}"
        except ImportError:
            _title_text = App_name
        self.title_label = QLabel(_title_text)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(self.title_label)

        layout.addStretch()

    # = TAB NAVIGATION BUTTONS

        #TODO buttons and connect signals needs to be fixed.
        #'create_new_img': lambda: create_new_img(self.main_window),
        #'open_img_file': lambda: open_file_dialog(self.main_window),
        #'reload_table': lambda: reload_current_file(self.main_window),


        # File Entries button - CONNECTED
        #self.file_open_btn = QPushButton()
        #self.file_open_btn.setFont(self.button_font)
        #self.file_open_btn.setIcon(self.icon_factory.open_icon())
        #self.file_open_btn.setText("Open")
        #self.file_open_btn.setIconSize(QSize(20, 20))
        #self.file_open_btn.setToolTip("Open file (Ctrl+O)")
        #self.file_open_btn.clicked.connect(lambda: self.open_img_file)
        #self.file_open_btn.clicked.connect(self.open_img_file)
        #layout.addWidget(self.file_open_btn)

        # Directory Tree button - CONNECTED
        #self.file_save_btn = QPushButton()
        #self.file_save_btn.setFont(self.button_font)
        #self.file_save_btn.setIcon(self.icon_factory.save_icon())
        #self.file_save_btn.setText("Save")
        #self.file_save_btn.setIconSize(QSize(20, 20))
        #self.file_save_btn.setToolTip("Save File (Ctrl+S)")
        #self.file_save_btn.clicked.connect(self.save_img_entry)
        #self.file_save_btn.clicked.connect(lambda: self.show_project_manager_dialog)

        #layout.addWidget(self.file_save_btn)

        # Search button - CONNECTED (renamed from "Search Results")
        #self.file_rebuild_btn = QPushButton()
        #self.file_rebuild_btn.setFont(self.button_font)
        #self.file_rebuild_btn.setIcon(self.icon_factory.rebuild_icon())
        #self.file_rebuild_btn.setText("Rebuild")
        #self.file_rebuild_btn.setIconSize(QSize(20, 20))
        #self.file_rebuild_btn.setToolTip("Rebuild (Ctrl+R)")
        #self.file_rebuild_btn.clicked.connect(lambda: self.main_window.search_manager.show_search_dialog())
        #self.file_rebuild_btn.clicked.connect(self.rebuild_img)
        #layout.addWidget(self.file_rebuild_btn)


        layout.addStretch()

        # ==================== ACTION BUTTONS ====================

        # ── Tool Taskbar — embedded inline between title and undo ──────────────
        # Populated by register_tool() whenever a workshop opens.
        # Hidden until first tool registers; sits flush in the titlebar row.
        # ToolTaskbar is defined earlier in this file — instantiate directly.
        self._inline_taskbar = ToolTaskbar(self.main_window, self.titlebar)
        self._inline_taskbar.setVisible(False)
        layout.addWidget(self._inline_taskbar)
        # Expose on main_window so register_tool finds it immediately
        self.main_window.tool_taskbar = self._inline_taskbar

        layout.addStretch()

        # Undo button - CONNECTED
        self.undo_btn = QPushButton()
        self.undo_btn.setFont(self.button_font)
        self.undo_btn.setIcon(self.icon_factory.undo_icon(20, icon_color))
        self.undo_btn.setText("")
        self.undo_btn.setIconSize(QSize(20, 20))
        self.undo_btn.setMinimumWidth(40)
        self.undo_btn.setMaximumWidth(40)
        self.undo_btn.setMinimumHeight(30)
        self.undo_btn.setToolTip("Undo last change")
        self.undo_btn.setEnabled(False)

        # Connect to undo manager if available
        if hasattr(self.main_window, 'undo_manager'):
            self.undo_btn.clicked.connect(self.main_window.undo_manager.undo)

        layout.addWidget(self.undo_btn)

        # Info button - CONNECTED
        self.info_btn = QPushButton()
        self.info_btn.setIcon(self.icon_factory.info_icon(20, icon_color))
        self.info_btn.setIconSize(QSize(20, 20))
        self.info_btn.setMinimumWidth(40)
        self.info_btn.setMaximumWidth(40)
        self.info_btn.setMinimumHeight(30)
        self.info_btn.setToolTip("Application Information")
        self.info_btn.clicked.connect(self._show_imgfactory_info)
        layout.addWidget(self.info_btn)

        # Properties/Theme button - CONNECTED
        self.properties_btn = QPushButton()
        self.properties_btn.setFont(self.button_font)
        self.properties_btn.setIcon(SVGIconFactory.properties_icon(24, icon_color))
        self.properties_btn.setIconSize(QSize(20, 20))
        self.properties_btn.setMinimumWidth(40)
        self.properties_btn.setMaximumWidth(40)
        self.properties_btn.setMinimumHeight(30)
        self.properties_btn.setToolTip("Theme Settings")
        self.properties_btn.clicked.connect(self._launch_theme_settings)
        self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        layout.addWidget(self.properties_btn)

        # AI Workshop button
        self.ai_btn = QPushButton()
        self.ai_btn.setFont(self.button_font)
        self.ai_btn.setIcon(SVGIconFactory.ai_icon(24, icon_color))
        self.ai_btn.setText("AI")
        self.ai_btn.setIconSize(QSize(24, 24))
        self.ai_btn.setMinimumWidth(64)
        self.ai_btn.setMaximumWidth(64)
        self.ai_btn.setMinimumHeight(30)
        self.ai_btn.setToolTip("AI Workshop – Local LLM chat via Ollama\nLeft-click: dock into tab\nRight-click: open standalone")
        self.ai_btn.clicked.connect(lambda: self.main_window.open_ai_workshop_docked())
        self.ai_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.ai_btn.customContextMenuRequested.connect(self._show_ai_context_menu)
        layout.addWidget(self.ai_btn)

        #self.debug_btn = QPushButton()
        #self.debug_btn.setFont(self.button_font)
        #self.debug_btn.setText("DEBUG")
        #self.debug_btn.setIconSize(QSize(20, 20))
        #self.debug_btn.setToolTip("Show tab debug info")
        #self.debug_btn.clicked.connect(self._debug_tabs)
        #layout.addWidget(self.debug_btn)

        # ==================== WINDOW CONTROLS (Custom UI only) ====================

        ui_mode = getattr(self, 'ui_mode', 'system')

        if ui_mode == 'custom':
            self.minimize_btn = QPushButton()
            self.minimize_btn.setIcon(self.icon_factory.minimize_icon(20, icon_color))
            self.minimize_btn.setIconSize(QSize(20, 20))
            self.minimize_btn.setMinimumWidth(40)
            self.minimize_btn.setMaximumWidth(40)
            self.minimize_btn.setMinimumHeight(30)
            self.minimize_btn.clicked.connect(self.main_window.showMinimized)
            self.minimize_btn.setToolTip("Minimize Window")
            layout.addWidget(self.minimize_btn)

            self.maximize_btn = QPushButton()
            self.maximize_btn.setIcon(self.icon_factory.maximize_icon(20, icon_color))
            self.maximize_btn.setIconSize(QSize(20, 20))
            self.maximize_btn.setMinimumWidth(40)
            self.maximize_btn.setMaximumWidth(40)
            self.maximize_btn.setMinimumHeight(30)
            self.maximize_btn.clicked.connect(self.toggle_maximize_restore)
            self.maximize_btn.setToolTip("Maximize/Restore Window")
            layout.addWidget(self.maximize_btn)

            self.close_btn = QPushButton()
            self.close_btn.setIcon(self.icon_factory.close_icon(20, icon_color))
            self.close_btn.setIconSize(QSize(20, 20))
            self.close_btn.setMinimumWidth(40)
            self.close_btn.setMaximumWidth(40)
            self.close_btn.setMinimumHeight(30)
            self.close_btn.clicked.connect(self.main_window.close)
            self.close_btn.setToolTip("Close Window")
            layout.addWidget(self.close_btn)

        return self.titlebar

    def _show_popup_menu(self): #vers 1
        """Show main popup menu"""
        try:
            if hasattr(self, 'menu_btn'):
                show_popup_menu_at_button(self.main_window, self.menu_btn)
            else:
                self.custom_menu_manager.show_popup_menu()
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Menu error: {str(e)}")


    def _create_left_three_section_panel(self): #vers 3
        """Create left panel with 3 sections: File Window, Status Window"""
        left_container = QWidget()
        left_layout = QVBoxLayout(left_container)
        left_layout.setContentsMargins(3, 3, 3, 3)
        left_layout.setSpacing(0)  # No spacing - splitter handles this

        # Create vertical splitter for the sections
        self.left_vertical_splitter = QSplitter(Qt.Orientation.Vertical)

        # 1. MIDDLE: File Window (table with sub-tabs)
        file_window = self._create_file_window()
        self.left_vertical_splitter.addWidget(file_window)

        # 2. BOTTOM: Status Window (log and status)
        #status_window = self.create_status_window()
        #self.left_vertical_splitter.addWidget(status_window)

        # Set section proportions: File(760px), Status(200px)
        self.left_vertical_splitter.setSizes([760, 200])

        # Prevent sections from collapsing completely
        self.left_vertical_splitter.setCollapsible(0, True)  # File window
        self.left_vertical_splitter.setCollapsible(1, True)  # Status window

        # Apply theme styling to vertical splitter
        self._apply_vertical_splitter_theme()

        left_layout.addWidget(self.left_vertical_splitter)
        return left_container


    def _switch_to_tab(self, index: int): #vers 5
        """Switch to the specified tab in the main tab widget"""
        try:
            # Check debug flag
            debug_enabled = False
            if hasattr(self.main_window, 'img_settings'):
                debug_enabled = self.main_window.img_settings.get("enable_debug", False)

            if debug_enabled:
                print(f"\n=== SWITCH TO TAB {index} ===")
                print(f"Has main_tab_widget: {hasattr(self.main_window, 'main_tab_widget')}")
                print(f"Has gui_layout: {hasattr(self.main_window, 'gui_layout')}")

            # Try main_tab_widget first (created in _create_ui)
            if hasattr(self.main_window, 'main_tab_widget') and self.main_window.main_tab_widget:
                tab_widget = self.main_window.main_tab_widget

                if debug_enabled:
                    print(f"Using main_tab_widget, count: {tab_widget.count()}")

                # Special handling for Directory Tree (index 1)
                if index == 1:
                    if not hasattr(self.main_window, 'directory_tree'):
                        from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                        integrate_directory_tree_browser(self.main_window)
                    
                    # Ensure the directory tree is properly populated with the game root
                    if hasattr(self.main_window, 'directory_tree') and hasattr(self.main_window, 'game_root'):
                        if hasattr(self.main_window.directory_tree, 'browse_directory'):
                            self.main_window.directory_tree.browse_directory(self.main_window.game_root)

                if index < tab_widget.count():
                    tab_widget.setCurrentIndex(index)
                    tab_name = tab_widget.tabText(index)

                    if debug_enabled:
                        print(f"✓ Switched to tab {index}: {tab_name}")

                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(f"Switched to: {tab_name}")
                else:
                    if debug_enabled:
                        print(f"✗ Tab {index} doesn't exist (only {tab_widget.count()} tabs)")
                        
                    # Try to handle the case where the tab doesn't exist but we want to show the content
                    if index == 0:  # File entries
                        self._switch_to_file_entries()
                    elif index == 1:  # Directory tree
                        self._switch_to_directory_tree()
                    elif index == 2:  # Search
                        self._switch_to_search()
                return

            # Try gui_layout.tab_widget as fallback
            if hasattr(self.main_window, 'gui_layout'):
                if hasattr(self.main_window.gui_layout, 'tab_widget') and self.main_window.gui_layout.tab_widget:
                    tab_widget = self.main_window.gui_layout.tab_widget

                    if debug_enabled:
                        print(f"Using gui_layout.tab_widget, count: {tab_widget.count()}")

                    if index < tab_widget.count():
                        tab_widget.setCurrentIndex(index)

                        if debug_enabled:
                            print(f"✓ Switched to tab {index}")
                    else:
                        # If the tab doesn't exist, try our button-based functions
                        if index == 0:  # File entries
                            self._switch_to_file_entries()
                        elif index == 1:  # Directory tree
                            self._switch_to_directory_tree()
                        elif index == 2:  # Search
                            self._switch_to_search()
                    return

            if debug_enabled:
                print("✗ No valid tab widget found!")

            # As a final fallback, try to use our button-based switching functions
            if index == 0:  # File entries
                self._switch_to_file_entries()
            elif index == 1:  # Directory tree
                self._switch_to_directory_tree()
            elif index == 2:  # Search
                self._switch_to_search()

        except Exception as e:
            if debug_enabled:
                print(f"✗ Error switching to tab {index}: {str(e)}")
                import traceback
                traceback.print_exc()


    def _switch_to_file_entries(self): #vers 2
        """Switch to current IMG file tab (last active tab > 0)"""
        try:
            if not hasattr(self.main_window, 'main_tab_widget') or not self.main_window.main_tab_widget:
                self.main_window.log_message("No tab widget available")
                return

            tab_widget = self.main_window.main_tab_widget
            current_index = tab_widget.currentIndex()

            # If already on an IMG tab (> 0), stay there
            if current_index > 0:
                tab_name = tab_widget.tabText(current_index)
                self.main_window.log_message(f"Already on: {tab_name}")
                return

            # Find last active IMG tab or first IMG tab
            if tab_widget.count() > 1:
                # Switch to Tab 1 (first IMG tab)
                tab_widget.setCurrentIndex(1)
                tab_name = tab_widget.tabText(1)
                self.main_window.log_message(f"→ Switched to: {tab_name}")
            else:
                self.main_window.log_message("No IMG files loaded")

        except Exception as e:
            self.main_window.log_message(f"Error switching to file entries: {str(e)}")
            import traceback
            traceback.print_exc()


    def _switch_to_directory_tree(self): #vers 12
        """Switch to Directory Tree - setup once, then block button"""
        try:
            # If already setup, block second press
            if hasattr(self.main_window, '_dirtree_setup_complete'):
                self.main_window.log_message("Directory Tree already loaded")
                return

            # Check game root
            if not hasattr(self.main_window, 'game_root') or not self.main_window.game_root:
                from PyQt6.QtWidgets import QMessageBox
                reply = QMessageBox.question(
                    self.main_window,
                    "No Project Configured",
                    "No project is currently configured.\n\nWould you like to open the Project Manager now?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                )
                if reply == QMessageBox.StandardButton.Yes:
                    from apps.components.Project_Manager.project_manager import show_project_manager_dialog
                    show_project_manager_dialog(self.main_window)
                return

            # ONE-TIME SETUP: Create directory tree
            if not hasattr(self.main_window, 'directory_tree'):
                from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                if not integrate_directory_tree_browser(self.main_window):
                    self.main_window.log_message("Failed to load Directory Tree")
                    return

            # Place in file window
            if hasattr(self.main_window.gui_layout, 'middle_vertical_splitter'):
                splitter = self.main_window.gui_layout.middle_vertical_splitter
                if splitter and splitter.count() > 0:
                    file_window = splitter.widget(0)
                    layout = file_window.layout()
                    if layout:
                        layout.addWidget(self.main_window.directory_tree)
                        self.main_window.log_message("✓ Directory tree setup complete")

            # Browse to game root
            if hasattr(self.main_window.directory_tree, 'browse_directory'):
                self.main_window.directory_tree.browse_directory(self.main_window.game_root)

            # Update Tab 0 label to "Dir Tree"
            if hasattr(self.main_window, 'main_tab_widget'):
                self.main_window.main_tab_widget.setTabText(0, "Dir Tree")

            # Mark setup as complete BEFORE showing
            self.main_window._dirtree_setup_complete = True
            self.main_window._dirtree_state = 0  # start hidden

            # Register dir tree in tool taskbar
            try:
                mw = self.main_window
                from apps.methods.imgfactory_svg_icons import SVGIconFactory
                _icon = SVGIconFactory.info_icon() if not hasattr(SVGIconFactory,'folder_icon') else SVGIconFactory.folder_icon()
                if hasattr(mw, 'register_tool'):
                    mw.register_tool("dirtree", "Browser", _icon,
                                     mw.directory_tree, "Directory Tree Browser")
            except Exception:
                pass

            # Show directory tree, hide table
            if hasattr(self.main_window.gui_layout, 'table'):
                self.main_window.gui_layout.table.hide()
            self.main_window.directory_tree.show()

            # Switch to Tab 0
            if hasattr(self.main_window, 'main_tab_widget'):
                self.main_window.main_tab_widget.setCurrentIndex(0)
                self.main_window.log_message("→ Directory Tree")

        except Exception as e:
            self.main_window.log_message(f"Error: {str(e)}")
            import traceback
            traceback.print_exc()


    def _ensure_tab_0_for_directory_tree(self): #vers 1
        """Ensure Tab 0 exists and contains directory tree widget"""
        try:
            if not hasattr(self.main_window, 'main_tab_widget') or not self.main_window.main_tab_widget:
                return False

            tab_widget = self.main_window.main_tab_widget

            # Check if Tab 0 exists
            if tab_widget.count() == 0:
                # Create Tab 0
                from PyQt6.QtWidgets import QWidget, QVBoxLayout
                tab_0 = QWidget()
                tab_0_layout = QVBoxLayout(tab_0)
                tab_0_layout.setContentsMargins(0, 0, 0, 0)

                # Add directory tree if it exists
                if hasattr(self.main_window, 'directory_tree'):
                    tab_0_layout.addWidget(self.main_window.directory_tree)

                tab_widget.insertTab(0, tab_0, "Dir Tree")
                self.main_window.log_message("Created Tab 0 for Directory Tree")
                return True
            else:
                # Tab 0 exists - ensure it has directory tree
                tab_0 = tab_widget.widget(0)
                if tab_0 and hasattr(self.main_window, 'directory_tree'):
                    # Check if directory tree is already in tab 0
                    if self.main_window.directory_tree.parent() != tab_0:
                        layout = tab_0.layout()
                        if not layout:
                            from PyQt6.QtWidgets import QVBoxLayout
                            layout = QVBoxLayout(tab_0)
                            layout.setContentsMargins(0, 0, 0, 0)
                        layout.addWidget(self.main_window.directory_tree)

                    # Update tab label
                    tab_widget.setTabText(0, "Dir Tree")
                    return True
            return False
        except Exception as e:
            self.main_window.log_message(f"Error ensuring Tab 0: {str(e)}")
            return False


    def _switch_to_search(self): #vers 1
        """Show search dialog and log results to status window and log tab"""
        try:
            if hasattr(self.main_window, 'search_manager') and self.main_window.search_manager:
                # Connect search results to log system
                if hasattr(self.main_window.search_manager, 'search_results_signal'):
                    # Connect search results to both status window and log
                    def handle_search_results(results):
                        self.main_window.log_message(f"Search completed: Found {len(results)} results")
                        if hasattr(self, 'log') and self.log:
                            self.log.append(f"[SEARCH] Found {len(results)} results")
                    
                    self.main_window.search_manager.search_results_signal.connect(handle_search_results)
                
                self.main_window.search_manager.show_search_dialog()
            else:
                # Try to create a search dialog directly
                from apps.core.gui_search import ASearchDialog
                search_dialog = ASearchDialog(self.main_window)
                search_dialog.exec()
                
                self.main_window.log_message("→ Showing Search Dialog")
            
            # If we have a main tab widget, try to switch to the search tab if it exists
            if hasattr(self.main_window, 'main_tab_widget') and self.main_window.main_tab_widget:
                tab_widget = self.main_window.main_tab_widget
                for i in range(tab_widget.count()):
                    if "Search" in tab_widget.tabText(i):
                        tab_widget.setCurrentIndex(i)
                        self.main_window.log_message("→ Switched to Search Tab")
                        return
        except Exception as e:
            self.main_window.log_message(f"Error: {str(e)}")


    def _log_button_press(self, button_name: str, action_func=None): #vers 1
        """Wrapper to log button presses before executing action"""
        def wrapper():
            try:
                self.main_window.log_message(f"[BUTTON] {button_name}")
                if action_func:
                    action_func()
            except Exception as e:
                self.main_window.log_message(f"[ERROR] {button_name}: {str(e)}")
        return wrapper


    def _toggle_log_visibility(self): #vers 3
        """Toggle extended activity log visibility including search results, imports, exports, and removals"""
        try:
            if not hasattr(self, 'log') or not self.log:
                self.main_window.log_message("Log widget not available")
                return

            is_visible = self.log.isVisible()
            self.log.setVisible(not is_visible)

            if not is_visible:
                self.main_window.log_message("=" * 60)
                self.main_window.log_message("EXTENDED ACTIVITY LOG - All operations tracked")
                self.main_window.log_message("Includes: Search Results | Imports | Exports | Removals | System Events")
                self.main_window.log_message("=" * 60)
                if hasattr(self, 'log_btn'):
                    self.log_btn.setText("Hide Log")
            else:
                if hasattr(self, 'log_btn'):
                    self.log_btn.setText("Log")

        except Exception as e:
            self.main_window.log_message(f"Error: {str(e)}")


    def handle_set_assists_project_folder(self): #vers 1
        """Handle Set Project folder (Assists) menu action - Creates Assists folder with Models, Maps, Collisions, Textures, Other"""
        try:
            from PyQt6.QtWidgets import QFileDialog, QMessageBox
            import os
            
            # Start from user's home directory
            start_dir = os.path.expanduser("~")
            
            # Select base directory where "Assists" folder will be created
            base_folder = QFileDialog.getExistingDirectory(
                self.main_window,
                "Select Base Directory for Assists Folder",
                start_dir,
                QFileDialog.Option.ShowDirsOnly
            )
            
            if base_folder:
                # Create the main "Assists" folder
                assists_folder = os.path.join(base_folder, "Assists")
                
                # Create the Assists folder and subfolders
                subfolders = ["Models", "Maps", "Collisions", "Textures", "Other"]
                
                os.makedirs(assists_folder, exist_ok=True)
                self.main_window.log_message(f"Created Assists folder: {assists_folder}")
                
                for subfolder in subfolders:
                    subfolder_path = os.path.join(assists_folder, subfolder)
                    os.makedirs(subfolder_path, exist_ok=True)
                    self.main_window.log_message(f"Created subfolder: {subfolder_path}")
                
                # Store the assists folder path for other functions
                if not hasattr(self.main_window, 'assists_folder'):
                    self.main_window.assists_folder = assists_folder
                
                # Show success message
                QMessageBox.information(
                    self.main_window,
                    "Assists Project Folder Created",
                    f"Assists project folder structure created successfully:\n{assists_folder}\n\nSubfolders created:\n• Models/\n• Maps/\n• Collisions/\n• Textures/\n• Other/"
                )
                
                self.main_window.log_message(f"Assists project folder ready: {assists_folder}")
                
            else:
                self.main_window.log_message("Assists project folder creation cancelled")
                
        except Exception as e:
            self.main_window.log_message(f"Error creating Assists project folder: {str(e)}")





    def _show_rw_scan_dialog(self): #vers 1
        """Show RW version scan dialog: lists all detected versions, offers force-rescan."""
        try:
            from PyQt6.QtWidgets import (
                QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QLabel,
                QTableWidget, QTableWidgetItem, QHeaderView, QProgressDialog,
                QAbstractItemView
            )
            from PyQt6.QtCore import Qt, QTimer
            from PyQt6.QtGui import QColor, QPalette

            mw = self.main_window
            img_file = getattr(mw, 'current_img', None)

            if not img_file or not img_file.entries:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.information(mw, "RW Scan", "No IMG file loaded.")
                return

            from apps.methods.rw_versions import get_rw_version_name, is_valid_rw_version

            # ── collect stats ────────────────────────────────────────
            def _collect(entries):
                """Return (rows, version_summary) from current entry list."""
                rows = []
                version_counts = {}
                for entry in entries:
                    ext = getattr(entry, 'extension', '').upper()
                    if ext not in ('DFF', 'TXD'):
                        continue
                    size = getattr(entry, 'size', 0)
                    rv   = getattr(entry, 'rw_version', 0)
                    rvn  = getattr(entry, 'rw_version_name', '')
                    if rv and is_valid_rw_version(rv):
                        label = rvn if rvn and rvn not in ('Unknown', '', 'N/A') \
                                    else get_rw_version_name(rv)
                        hex_v = f"0x{rv:08X}"
                    elif size == 0:
                        label, hex_v = "Empty", "—"
                    else:
                        label, hex_v = "Unknown", "—"
                    rows.append((entry.name, ext, hex_v, label))
                    version_counts[label] = version_counts.get(label, 0) + 1
                return rows, version_counts

            # ── build dialog ─────────────────────────────────────────
            dlg = QDialog(mw)
            dlg.setWindowTitle("RW Version Scan")
            dlg.setMinimumSize(700, 480)
            dlg.resize(750, 520)

            pal   = dlg.palette()
            c_bg  = pal.color(QPalette.ColorRole.Base).name()
            c_alt = pal.color(QPalette.ColorRole.AlternateBase).name()
            c_txt = pal.color(QPalette.ColorRole.Text).name()
            c_hdr = pal.color(QPalette.ColorRole.Button).name()
            c_sel = pal.color(QPalette.ColorRole.Highlight).name()

            dlg.setStyleSheet(f"""
                QDialog        {{ background:{c_bg}; color:{c_txt}; }}
                QTableWidget   {{ background:{c_bg}; color:{c_txt};
                                  gridline-color:{c_hdr};
                                  selection-background-color:{c_sel}; }}
                QHeaderView::section {{ background:{c_hdr}; color:{c_txt};
                                        padding:4px; border:none; }}
                QPushButton    {{ background:{c_hdr}; color:{c_txt};
                                  padding:4px 12px; border-radius:3px; }}
                QPushButton:hover {{ background:{c_sel}; }}
                QLabel         {{ color:{c_txt}; }}
            """)

            vlay = QVBoxLayout(dlg)
            vlay.setSpacing(6)

            # Title + summary row
            title_lbl = QLabel()
            title_lbl.setStyleSheet("font-weight:bold; font-size:12px;")
            vlay.addWidget(title_lbl)

            summary_lbl = QLabel()
            summary_lbl.setWordWrap(True)
            vlay.addWidget(summary_lbl)

            # Table: Name | Type | Version hex | Version name
            table = QTableWidget()
            table.setColumnCount(4)
            table.setHorizontalHeaderLabels(["Name", "Type", "RW Hex", "RW Version"])
            table.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
            table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
            table.horizontalHeader().setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
            table.horizontalHeader().setSectionResizeMode(3, QHeaderView.ResizeMode.ResizeToContents)
            table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
            table.setAlternatingRowColors(True)
            table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
            table.verticalHeader().setVisible(False)
            vlay.addWidget(table)

            def _populate(entries):
                rows, vcounts = _collect(entries)
                table.setRowCount(len(rows))
                unknown_count = 0
                for r, (name, ext, hex_v, label) in enumerate(rows):
                    table.setItem(r, 0, QTableWidgetItem(name))
                    table.setItem(r, 1, QTableWidgetItem(ext))
                    table.setItem(r, 2, QTableWidgetItem(hex_v))
                    table.setItem(r, 3, QTableWidgetItem(label))
                    if label == "Unknown":
                        unknown_count += 1
                        for c in range(4):
                            item = table.item(r, c)
                            if item:
                                item.setForeground(QColor(pal.color(QPalette.ColorRole.PlaceholderText)))

                dff_txd_total = sum(vcounts.values())
                title_lbl.setText(
                    f"RW Version Scan — {os.path.basename(img_file.file_path)}"
                    f"  ({dff_txd_total} DFF/TXD entries)"
                )
                parts = [f"{v}: {n}" for v, n in sorted(vcounts.items(), key=lambda x: -x[1])]
                summary_lbl.setText("  |  ".join(parts) if parts else "No DFF/TXD entries.")
                return unknown_count

            import os
            unknown_before = _populate(img_file.entries)

            # Bottom buttons
            hlay = QHBoxLayout()

            status_lbl = QLabel("")
            status_lbl.setStyleSheet("font-style:italic;")
            hlay.addWidget(status_lbl, 1)

            rescan_btn = QPushButton("⟳  Force Rescan All")
            rescan_btn.setToolTip("Re-read every DFF/TXD entry from disk and detect RW version")

            def _do_rescan():
                rescan_btn.setEnabled(False)
                status_lbl.setText("Scanning…")
                dlg.repaint()

                dff_txd_entries = [
                    e for e in img_file.entries
                    if getattr(e, 'extension', '').upper() in ('DFF', 'TXD')
                ]
                total = len(dff_txd_entries)
                found = 0

                for e in dff_txd_entries:
                    e.rw_version = 0
                    e.rw_version_name = ''
                    e._version_detected = False
                    try:
                        e.detect_file_type_and_version()
                        if getattr(e, 'rw_version', 0):
                            found += 1
                    except Exception:
                        pass

                unknown_after = _populate(img_file.entries)

                # Refresh the main IMG table too
                try:
                    from apps.methods.export_shared import get_active_table
                    active_table = get_active_table(mw)
                    if active_table and hasattr(mw, 'gui_layout'):
                        populator = getattr(mw.gui_layout, 'table_populator', None)
                        if populator is None:
                            from apps.methods.populate_img_table import IMGTablePopulator
                            populator = IMGTablePopulator(mw)
                        # Refresh only the RW Version column (col 5) for efficiency
                        from apps.methods.populate_img_table import IMGTablePopulator
                        pop = IMGTablePopulator(mw)
                        for row, entry in enumerate(img_file.entries):
                            if row >= active_table.rowCount():
                                break
                            ext = getattr(entry, 'extension', '').upper()
                            if ext in ('DFF', 'TXD'):
                                ver_text = pop.get_rw_version_light(entry)
                                from PyQt6.QtWidgets import QTableWidgetItem as QTI
                                item = QTI(ver_text)
                                active_table.setItem(row, 5, item)
                except Exception:
                    pass

                status_lbl.setText(
                    f"Done — {found}/{total} entries resolved, "
                    f"{unknown_after} still unknown."
                )
                rescan_btn.setEnabled(True)

            rescan_btn.clicked.connect(_do_rescan)
            hlay.addWidget(rescan_btn)

            close_btn = QPushButton("Close")
            close_btn.clicked.connect(dlg.accept)
            hlay.addWidget(close_btn)

            vlay.addLayout(hlay)

            if unknown_before > 0:
                status_lbl.setText(
                    f"{unknown_before} entries have no detected RW version — "
                    "click Rescan to re-read from disk."
                )

            dlg.exec()

        except Exception as e:
            try:
                self.main_window.log_message(f"RW Scan dialog error: {e}")
            except Exception:
                pass

    def _show_rw_reference(self): #vers 3
        """Show comprehensive GTA/RenderWare format reference — all researched binary layouts."""
        from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton,
                                     QTabWidget, QTextEdit, QLabel, QSplitter)
        from PyQt6.QtGui import QFont, QPalette
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtCore import Qt

        dialog = QDialog(self.main_window)
        dialog.setWindowTitle("GTA / RenderWare Format Reference — IMG Factory 1.6")
        dialog.setMinimumSize(980, 760)
        dialog.resize(1060, 800)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(6)
        layout.setContentsMargins(10, 10, 10, 10)

        hdr = QLabel("RenderWare & GTA Archive Format Reference")
        hdr.setFont(QFont("Arial", 13, QFont.Weight.Bold))
        hdr.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(hdr)

        sub = QLabel(
            "Researched binary layouts for every platform supported by IMG Factory 1.6 — "
            "including how each format was identified, what bytes we inspected, and "
            "what conclusions we drew.")
        sub.setWordWrap(True)
        sub.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(sub)

        tabs = QTabWidget()
        layout.addWidget(tabs, 1)

        def make_tab(html):
            t = QTextEdit()
            t.setReadOnly(True)
            t.setHtml(html)
            return t

        pal = QApplication.instance().palette()
        def _h(r): return pal.color(r).name()
        c_bg    = _h(QPalette.ColorRole.Base)
        c_bg2   = _h(QPalette.ColorRole.AlternateBase)
        c_text  = _h(QPalette.ColorRole.Text)
        c_text2 = _h(QPalette.ColorRole.PlaceholderText)
        c_acc   = _h(QPalette.ColorRole.Highlight)
        c_brd   = _h(QPalette.ColorRole.Mid)
        c_th    = _h(QPalette.ColorRole.Button)
        c_thtx  = _h(QPalette.ColorRole.ButtonText)
        _tc = {}
        if hasattr(self.main_window, 'app_settings'):
            _theme = self.main_window.app_settings.current_settings.get('theme', '')
            _tc = self.main_window.app_settings.get_theme_colors(_theme) or {}
        c_ok  = _tc.get('success','') or _h(QPalette.ColorRole.Link)
        c_err = _tc.get('error','')   or _h(QPalette.ColorRole.LinkVisited)
        c_wrn = _tc.get('warning','') or _h(QPalette.ColorRole.Highlight)

        css = (
            "<style>"
            f"body{{font-family:'Consolas','Courier New',monospace;font-size:11px;"
            f"background:{c_bg};color:{c_text};padding:4px;}}"
            f"h2{{color:{c_acc};border-bottom:1px solid {c_brd};padding-bottom:3px;"
            f"margin-top:10px;font-size:13px;}}"
            f"h3{{color:{c_acc};margin:8px 0 2px 0;font-size:12px;}}"
            f"h4{{color:{c_text};margin:5px 0 2px 0;font-size:11px;font-weight:bold;}}"
            f"table{{border-collapse:collapse;width:100%;margin-bottom:8px;}}"
            f"th{{background:{c_th};color:{c_thtx};padding:3px 8px;text-align:left;"
            f"border-bottom:1px solid {c_brd};font-size:11px;}}"
            f"td{{padding:2px 8px;border-bottom:1px solid {c_brd};color:{c_text};"
            f"background:{c_bg};vertical-align:top;}}"
            f"tr:nth-child(even) td{{background:{c_bg2};}}"
            f"code{{background:{c_bg2};padding:1px 4px;border-radius:3px;font-family:monospace;}}"
            f".note{{color:{c_text2};font-style:italic;}}"
            f".ok{{color:{c_ok};font-weight:bold;}}"
            f".err{{color:{c_err};}}"
            f".wrn{{color:{c_wrn};}}"
            f".hex{{font-family:monospace;color:{c_acc};}}"
            f".off{{color:{c_text2};font-size:10px;}}"
            "</style>"
        )

        # ══════════════════════════════════════════════════════════════════════
        # TAB 1: IMG Archive Formats
        # ══════════════════════════════════════════════════════════════════════
        img_html = css + """
<h2>IMG Archive Formats — All Platforms</h2>

<p class="note">All byte values are little-endian (LE) unless noted. Sector size is 2048 bytes for PC/Xbox/PS2 standard formats, 512 bytes for PS2 embedded-directory formats and iOS/PSP variants.</p>

<h3>VERSION 1 — DIR+IMG Pair (GTA3, VC PC; Bully PS2)</h3>
<p>Two files: <code>NAME.DIR</code> (directory) + <code>NAME.IMG</code> (data). The .DIR file is a flat array of 32-byte entries. Data begins at sector 0 of the .IMG file.</p>
<table>
<tr><th>Offset</th><th>Size</th><th>Field</th><th>Notes</th></tr>
<tr><td class="hex">+00</td><td>4</td><td>sector_offset</td><td>Sector number in .IMG where this entry begins</td></tr>
<tr><td class="hex">+04</td><td>4</td><td>sector_size</td><td>Entry size in 2048-byte sectors (round up)</td></tr>
<tr><td class="hex">+08</td><td>24</td><td>name</td><td>ASCII filename, null-padded. Max 23 chars + null terminator.</td></tr>
</table>
<p class="note">Entry count = DIR file size ÷ 32. No magic bytes, no header — first entry at byte 0. On Linux, companion lookup is case-insensitive (.DIR, .dir, .Dir all found).</p>
<p class="note"><b>Bully PS2 (WORLD.IMG, GTA3.IMG):</b> Uses identical V1 format. RW version <code class="hex">0x1C02000A</code> uniquely identifies Bully assets.</p>

<h3>VERSION 1.5 — Extended DIR+IMG (&gt;2 GB)</h3>
<p>Same 32-byte entry layout as V1 but entries may have no null terminator in the name field (all 24 bytes used for long filenames). Detected by: IMG size &gt; 2 GB, or name field has no <code>\x00</code> byte.</p>

<h3>VERSION 2 — Single-File VER2 (GTA SA PC, Android SA/LCS)</h3>
<p>Single <code>.IMG</code> file. Header followed immediately by directory, then data.</p>
<table>
<tr><th>Offset</th><th>Size</th><th>Field</th><th>Notes</th></tr>
<tr><td class="hex">+00</td><td>4</td><td>magic</td><td><code class="hex">56 45 52 32</code> = "VER2"</td></tr>
<tr><td class="hex">+04</td><td>4</td><td>entry_count</td><td>Number of directory entries</td></tr>
<tr><td class="hex">+08</td><td>32×N</td><td>entries[]</td><td>Same 32-byte layout as V1 (offset, size, name). Offsets relative to start of IMG.</td></tr>
<tr><td colspan="4" class="note">Data follows directory, sector-aligned. First entry typically starts at sector ceil((8+32N)/2048).</td></tr>
</table>

<h3>VERSION 3 — GTA IV (Single-File, optional AES-256 encryption)</h3>
<table>
<tr><th>Offset</th><th>Size</th><th>Field</th><th>Notes</th></tr>
<tr><td class="hex">+00</td><td>4</td><td>magic</td><td><code class="hex">52 2A 4E A9</code> = 0xA94E2A52 LE (unencrypted), or AES-256 ECB block (encrypted)</td></tr>
<tr><td class="hex">+04</td><td>4</td><td>entry_count</td><td>Number of entries</td></tr>
</table>

<h3>XBOX — DIR+IMG with LZO Compression (GTA3/VC Xbox)</h3>
<p>Same 32-byte DIR+IMG pair as V1, but each entry's data is LZO-compressed. Detected by reading the first entry's data and checking for the Xbox LZO magic.</p>
<table>
<tr><th>Field</th><th>Value</th><th>Notes</th></tr>
<tr><td>LZO magic</td><td class="hex">CE A1 A3 67</td><td>0x67A3A1CE LE — first 4 bytes of compressed entry data</td></tr>
<tr><td>Block header</td><td>12 bytes</td><td>always(4) + decompressed_size(4) + compressed_size(4)</td></tr>
<tr><td>RW header</td><td>always literal</td><td>RW chunk header (12 bytes) is always a literal run in the first LZO instruction, readable without decompressing the full block</td></tr>
</table>

<h3>PS2_V1 — 12-byte Embedded Directory (GTA3/VC PS2, iOS ports)</h3>
<p>No companion .DIR file. Entries are packed at the <b>very start</b> of the .IMG file, 12 bytes each, with no count header.</p>
<table>
<tr><th>Offset</th><th>Size</th><th>Field</th><th>Notes</th></tr>
<tr><td class="hex">+00</td><td>4</td><td>sector_offset</td><td>First entry's sector offset — this value × 512 = total directory size</td></tr>
<tr><td class="hex">+04</td><td>4</td><td>asset_id</td><td>High 16 bits = type (1=TXD, 2=DFF, 3=COL, 4=IPL…), low 16 bits = sequential index</td></tr>
<tr><td class="hex">+08</td><td>4</td><td>sector_size</td><td>Entry size in 512-byte sectors</td></tr>
</table>
<p class="note"><b>How we derived this:</b> GTA3.IMG from Liberty City Stories PS2 starts with <code>16 00 00 00 D4 9F 01 00 10 03 00 00</code>. The first field (0x16 = 22) is a plausible sector offset; the third field (0x310 = 784) is a plausible sector size. The directory ends at sector 22 × 512 = 11,264 bytes → 938 entries. Entry count = first_entry.sector_offset × 512 ÷ 12.</p>
<p class="note"><b>Asset ID decoding:</b> <code class="hex">0x00010001</code> → type=TXD(1), index=1 → displayed as <code>txd_0001</code>.</p>

<h3>PS2_VCS — Embedded Type-Code Directory (LCS/VCS PS2)</h3>
<p>Single .IMG file. Directory at the start, 32-byte entries with ASCII type codes. Sector size 512 bytes.</p>
<table>
<tr><th>Offset</th><th>Size</th><th>Field</th><th>Notes</th></tr>
<tr><td class="hex">+00</td><td>4</td><td>type_code</td><td>4-char ASCII e.g. <code>CDIN</code>, <code>MISS</code>, <code>SKEN</code></td></tr>
<tr><td class="hex">+04</td><td>4</td><td>padding</td><td>Always <code>00 00 00 00</code> — key detection field</td></tr>
<tr><td class="hex">+08</td><td>4</td><td>sector_offset</td><td>Offset in 512-byte sectors</td></tr>
<tr><td class="hex">+0C</td><td>4</td><td>sector_size</td><td>Size in 512-byte sectors</td></tr>
<tr><td class="hex">+10</td><td>16</td><td>padding</td><td>0x00 or 0x88 fill — 0x88 sentinel marks end of directory</td></tr>
</table>
<p class="note"><b>Detection:</b> bytes[0:4] all printable ASCII AND bytes[4:8] == <code>\x00\x00\x00\x00</code>. This distinguishes it from PS2_V1 (where bytes[4:8] is asset_id, usually non-zero) and from V1 (which has a .DIR companion).</p>

<h3>Android SA — VER2 + Mobile Texture DB</h3>
<p>VER2 single-file format. Identified by companion files <code>texdb.dat</code> / <code>texdb.toc</code> / <code>streaming.dat</code> in the same directory, or "android"/"mobile" in filename.</p>

<h3>Android LCS — VER2 + 0x1005FFFF TXD entries</h3>
<p>VER2 single-file format. Identified by "lcs"/"liberty" in filename, or by scanning first few entries for RW version <code class="hex">0x1005FFFF</code>.</p>

<h3>iOS *_pvr.img — 12-byte Entries, 512-byte Sectors</h3>
<p>Same 12-byte entry format as PS2_V1. Identified by <code>_pvr</code> suffix in filename. LCS iOS distinguished from GTA3/VC iOS by "lcs"/"liberty" in filename. The companion streaming segments (<code>indust.img</code>, <code>suburb.img</code>) have no directory — open <code>gta3.img</code> instead.</p>

<h3>Streaming Segment (LCS/VCS iOS/PSP)</h3>
<p>Raw data file with no directory. Lives alongside a <code>gta3.img</code> (VER2) in the same folder. Detected by: no magic, no .DIR companion, sibling <code>gta3.img</code> is VER2. Opening shows a helpful message to open the sibling instead.</p>

<h3>Platform Detection Order (detect_version)</h3>
<ol>
<li>Has .DIR companion → <b>V1 / V1.5 / Xbox</b> (probe for LZO magic first)</li>
<li>Starts with <code>VER2</code> → <b>V2</b> (sub-classify by filename/companion files for Android)</li>
<li>Starts with <code class="hex">0xA94E2A52</code> → <b>GTA IV unencrypted</b></li>
<li>First 16 bytes decrypt to valid GTA IV header → <b>GTA IV encrypted</b></li>
<li>No companion, ends in .img → probe: PS2_VCS, PS2_V1 (with *_pvr check), Bully, streaming segment, fallback V1/V1.5</li>
</ol>
<p class="note"><b>Linux case-sensitivity:</b> Companion lookup uses <code>_find_companion()</code> which tries .dir, .DIR, then a directory scan — so <code>GTA3.IMG</code> correctly finds <code>GTA3.DIR</code>.</p>
"""

        # ══════════════════════════════════════════════════════════════════════
        # TAB 2: RW Versions & Sections
        # ══════════════════════════════════════════════════════════════════════
        ver_html = css + """
<h2>RenderWare Versions</h2>

<h3>Version Encoding Formats</h3>
<p>RW version numbers appear in three distinct encoding styles depending on the game/platform.</p>

<h4>1. Plain integer (0x300–0x3FF) — GTA3 PS2, early PS2 titles</h4>
<table>
<tr><th>Hex</th><th>Decoded</th><th>Used by</th></tr>
<tr><td class="hex">0x00000300</td><td>3.0.0</td><td>GTA3 early builds</td></tr>
<tr><td class="hex">0x00000304</td><td>3.0.4</td><td>GTA3 early PC</td></tr>
<tr><td class="hex">0x00000310</td><td>3.1.0</td><td>GTA3/VC PS2 — <b>old-style plain integer</b></td></tr>
<tr><td class="hex">0x00000314</td><td>3.1.4</td><td>VC PS2 variant</td></tr>
<tr><td class="hex">0x00000320</td><td>3.2.0</td><td>GTA3/VC PS2 late</td></tr>
</table>
<p class="note"><b>How identified:</b> GTA3.IMG (LC PS2) started with <code>16 00 00 00 D4 9F 01 00 10 03 00 00</code>. Bytes [8:12] = <code>0x00000310</code> = 784 decimal. Cross-referenced against known GTA3 PS2 RW revision → 3.1.0 plain-int encoding confirmed.</p>

<h4>2. Packed format (low word = 0xFFFF) — GTA3 PC through GTA SA</h4>
<table>
<tr><th>Hex</th><th>Version</th><th>Platform/Game</th></tr>
<tr><td class="hex">0x0401FFFF</td><td>2.0.0.1</td><td>GTA3 early TXD</td></tr>
<tr><td class="hex">0x0800FFFF</td><td>3.0.0.0</td><td>GTA3 PS2</td></tr>
<tr><td class="hex">0x0C00FFFF</td><td>3.1.0.0</td><td>GTA3/VC PC</td></tr>
<tr><td class="hex">0x0C01FFFF</td><td>3.1.0.1</td><td>GTA VC PC</td></tr>
<tr><td class="hex">0x0C02FFFF</td><td>3.1.0.2</td><td>GTA3 PC / VC PS2</td></tr>
<tr><td class="hex">0x1000FFFF</td><td>3.2.0.0</td><td>GTA3 PC</td></tr>
<tr><td class="hex">0x1005FFFF</td><td>3.2.0.5</td><td>GTA VC PC / LCS Android (marker)</td></tr>
<tr><td class="hex">0x1400FFFF</td><td>3.4.0.0</td><td>GTA3/VC Xbox</td></tr>
<tr><td class="hex">0x1401FFFF</td><td>3.4.0.1</td><td>Manhunt / SOL</td></tr>
<tr><td class="hex">0x1800FFFF</td><td>3.5.0.0</td><td>SA Alpha / internal dev</td></tr>
<tr><td class="hex">0x1801FFFF</td><td>3.5.0.1</td><td>LCS / MDL</td></tr>
<tr><td class="hex">0x1802FFFF</td><td>3.5.0.2</td><td>VCS</td></tr>
<tr><td class="hex">0x1803FFFF</td><td>3.6.0.3</td><td>GTA SA PC</td></tr>
</table>
<p class="note">Packed format: high 16 bits = version nibbles, low 16 bits = always 0xFFFF. Decode: major = (v &gt;&gt; 14) &amp; 0x3C00, minor = ...(see RW SDK docs).</p>

<h4>3. Non-standard — SA Mobile, Bully</h4>
<table>
<tr><th>Hex</th><th>Game</th><th>Notes</th></tr>
<tr><td class="hex">0x1C020037</td><td>SA Mobile / PSP</td><td>Low word ≠ 0xFFFF, unique to mobile builds</td></tr>
<tr><td class="hex">0x1C02000A</td><td>Bully PS2/PC</td><td>Found in every Bully TXD/DFF. Low word = 0x000A. Confirmed from ASY_LobbyGlass.txd binary.</td></tr>
<tr><td class="hex">0x1C020085</td><td>Bully variant</td><td>Alternate Bully build</td></tr>
</table>

<h3>RW Chunk Header (12 bytes)</h3>
<p>Every RenderWare section starts with a 12-byte chunk header:</p>
<table>
<tr><th>Offset</th><th>Size</th><th>Field</th><th>Notes</th></tr>
<tr><td class="hex">+00</td><td>4</td><td>type_id</td><td>Section type (see table below)</td></tr>
<tr><td class="hex">+04</td><td>4</td><td>data_size</td><td>Byte count of data following this header (not including this header)</td></tr>
<tr><td class="hex">+08</td><td>4</td><td>version</td><td>RenderWare version encoding (see above)</td></tr>
</table>

<h3>Key RW Section Types</h3>
<table>
<tr><th>Type ID</th><th>Name</th><th>Description</th></tr>
<tr><td class="hex">0x0001</td><td>RpMaterial</td><td>Material definition</td></tr>
<tr><td class="hex">0x0002</td><td>RpTexture</td><td>Texture reference</td></tr>
<tr><td class="hex">0x0003</td><td>RpGeometry</td><td>Mesh geometry</td></tr>
<tr><td class="hex">0x0006</td><td>RpAtomicSection</td><td>Atomic/LOD data</td></tr>
<tr><td class="hex">0x0008</td><td>RpGeometryList</td><td>List of geometries</td></tr>
<tr><td class="hex">0x000F</td><td>RpMaterialList</td><td>Material list</td></tr>
<tr><td class="hex">0x0010</td><td>RpClump</td><td>3D model container (.dff)</td></tr>
<tr><td class="hex">0x0014</td><td>RwFrame</td><td>Frame/bone</td></tr>
<tr><td class="hex">0x0015</td><td>RpFrameList</td><td>Frame hierarchy</td></tr>
<tr><td class="hex">0x0016</td><td>RwTexDictionary</td><td>Texture dictionary (.txd) — <b>key marker for TXD files</b></td></tr>
<tr><td class="hex">0x001A</td><td>RwString</td><td>String data</td></tr>
<tr><td class="hex">0x001B</td><td>RwExtension</td><td>Extension data</td></tr>
<tr><td class="hex">0x0253F2FB</td><td>RpMorphPLG</td><td>Morph plugin</td></tr>
</table>
<p class="note"><b>How TXD files are identified in IMG:</b> First 4 bytes of a TXD entry = <code class="hex">0x00000016</code> (type 22 = RwTexDictionary). This is the primary fast-check before reading the full RW header. Similarly, DFF = <code class="hex">0x00000010</code> (RpClump).</p>

<h3>RW Version Scanning in IMG Factory</h3>
<p>For each DFF/TXD entry, the version is read from the first 16 bytes of entry data:</p>
<ol>
<li>Try version at <code>[8:12]</code> (standard: no prefix)</li>
<li>Try version at <code>[12:16]</code> (4-byte prefix before RW chunk)</li>
<li>Try version at <code>[16:20]</code> (8-byte prefix)</li>
<li>For Xbox LZO entries: skip 24-byte master+block header, then scan compressed payload</li>
</ol>
<p class="note">A version value is considered valid if it matches a known encoding pattern <b>or</b> has a named entry in the version table. This allows Bully (0x1C02000A) to pass validation even though it doesn't fit any standard encoding pattern.</p>
"""

        # ══════════════════════════════════════════════════════════════════════
        # TAB 3: TXD Format
        # ══════════════════════════════════════════════════════════════════════
        txd_html = css + """
<h2>TXD — Texture Dictionary Format</h2>

<h3>File Structure</h3>
<p>A .txd file is a RenderWare archive containing one or more native textures.</p>
<pre>
RwTexDictionary (0x0016) chunk header [12 bytes]
  ├── Struct (0x0001) — texture count [2 bytes] + device id [2 bytes]
  └── RwTexNative (0x0015) × N
        ├── Struct (0x0001) — platform id, filter, wrapping, name, mask
        ├── [Platform-specific texture data]
        └── RwExtension (0x001B)
</pre>

<h3>Platform IDs</h3>
<table>
<tr><th>ID</th><th>Platform</th><th>Texture Format</th></tr>
<tr><td>1</td><td>D3D8 (PC GTA3/VC)</td><td>DXT1/DXT3/uncompressed (D3DFMT_*)</td></tr>
<tr><td>2</td><td>D3D9 (PC GTA SA+)</td><td>DXT1/DXT3/DXT5/A8R8G8B8/R5G6B5</td></tr>
<tr><td>4</td><td>PS2</td><td>PS2-native, swizzled, GS Pixel Storage Modes</td></tr>
<tr><td>5</td><td>Xbox</td><td>Xbox-native, DXT compressed or unswizzled</td></tr>
<tr><td>6</td><td>GameCube</td><td>DXT1/CMPR</td></tr>
<tr><td>8</td><td>PVRTC (iOS)</td><td>PVRTC-2bpp, PVRTC-4bpp</td></tr>
<tr><td>9</td><td>ETC1 (Android)</td><td>ETC1 compressed</td></tr>
</table>

<h3>Texture Native Header (D3D9 example)</h3>
<table>
<tr><th>Offset</th><th>Size</th><th>Field</th></tr>
<tr><td class="hex">+00</td><td>4</td><td>platform_id (2 = D3D9)</td></tr>
<tr><td class="hex">+04</td><td>4</td><td>filter_flags</td></tr>
<tr><td class="hex">+08</td><td>32</td><td>texture_name (null-terminated)</td></tr>
<tr><td class="hex">+28</td><td>32</td><td>mask_name (null-terminated)</td></tr>
<tr><td class="hex">+48</td><td>4</td><td>raster_format (RASTER_DEFAULT, RASTER_1555, RASTER_565...)</td></tr>
<tr><td class="hex">+4C</td><td>4</td><td>d3d_format / has_alpha</td></tr>
<tr><td class="hex">+50</td><td>2</td><td>width</td></tr>
<tr><td class="hex">+52</td><td>2</td><td>height</td></tr>
<tr><td class="hex">+54</td><td>1</td><td>depth (bits per pixel)</td></tr>
<tr><td class="hex">+55</td><td>1</td><td>mip_count</td></tr>
<tr><td class="hex">+56</td><td>1</td><td>raster_type</td></tr>
<tr><td class="hex">+57</td><td>1</td><td>compression (0=none, 1=DXT1, 3=DXT3, 5=DXT5)</td></tr>
</table>

<h3>Mobile Formats (Android / iOS)</h3>
<table>
<tr><th>Format</th><th>Platform</th><th>Detection</th></tr>
<tr><td>PVRTC-2bpp / 4bpp</td><td>iOS</td><td>platform_id=8, files named *_pvr.img</td></tr>
<tr><td>ETC1</td><td>Android</td><td>platform_id=9, companion texdb.dat/toc present</td></tr>
<tr><td>DXT1/3/5</td><td>Android SA</td><td>Same as PC D3D9 format, VER2 container</td></tr>
</table>
<p class="note">Mobile TXDs from GTA SA Android live inside the standard VER2 .img file. A separate mobile texture database (<code>texdb.dat</code> / <code>texdb.toc</code>) maps texture IDs to streaming offsets — this is what IMG Factory reads to list mobile textures.</p>

<h3>DDS Support (iOS Game Trees)</h3>
<p>iOS game trees contain DDS files alongside *_pvr.img files. DDS uses the standard Microsoft DDS header with FourCC codes (DXT1/3/5) or uncompressed RGBA formats. These are pushed to the <code>game_trees</code> branch pending TXD Workshop integration.</p>

<h3>TXD Workshop</h3>
<p>Opens a TXD from either a standalone .txd file or from an entry inside an IMG archive. Displays the texture list, allows preview, import, and export. Platform is detected from the platform_id field in each RwTexNative. Version is read from the RwTexDictionary chunk header (bytes [8:12]).</p>
"""

        # ══════════════════════════════════════════════════════════════════════
        # TAB 4: COL Format
        # ══════════════════════════════════════════════════════════════════════
        col_html = css + """
<h2>COL — Collision Format</h2>

<h3>Magic Identifiers by Version</h3>
<table>
<tr><th>Magic</th><th>Version</th><th>Game</th></tr>
<tr><td class="hex">COLL</td><td>COL 1</td><td>GTA3, VC</td></tr>
<tr><td class="hex">COL2</td><td>COL 2</td><td>GTA SA</td></tr>
<tr><td class="hex">COL3</td><td>COL 3</td><td>GTA SA (extended)</td></tr>
<tr><td class="hex">COL4</td><td>COL 4</td><td>GTA IV</td></tr>
</table>

<h3>COL 1 Header (GTA3/VC)</h3>
<table>
<tr><th>Offset</th><th>Size</th><th>Field</th></tr>
<tr><td class="hex">+00</td><td>4</td><td>magic "COLL"</td></tr>
<tr><td class="hex">+04</td><td>4</td><td>data_size</td></tr>
<tr><td class="hex">+08</td><td>22</td><td>model_name (null-terminated)</td></tr>
<tr><td class="hex">+1E</td><td>2</td><td>model_id</td></tr>
<tr><td class="hex">+20</td><td>24</td><td>bounding_sphere (center xyz + radius, 4×float)</td></tr>
<tr><td class="hex">+38</td><td>Varies</td><td>spheres, boxes, mesh triangles, vertices</td></tr>
</table>

<h3>COL 2/3 Header Extensions (SA)</h3>
<p>SA adds surface properties, shadow meshes, and per-face material data. COL3 adds face groups for spatial optimization. The bounding box replaces/extends the bounding sphere.</p>

<h3>COL Workshop</h3>
<p>Opens COL files from standalone .col or from entries inside an IMG archive. Displays spheres, boxes, mesh collision, and allows editing of collision primitives. Registered in the Tool Taskbar when opened.</p>
"""

        # ══════════════════════════════════════════════════════════════════════
        # TAB 5: DAT / IDE / IPL Files
        # ══════════════════════════════════════════════════════════════════════
        dat_html = css + """
<h2>DAT / IDE / IPL — Game Data Files</h2>

<h3>IDE — Item Definition (object catalogue)</h3>
<p>Plain text sections defining every object in the game. Each section header is a keyword, entries follow one per line, section ends with <code>end</code>.</p>
<table>
<tr><th>Section</th><th>Content</th></tr>
<tr><td>objs</td><td>Static objects: ID, model, txd, draw_distance, flags</td></tr>
<tr><td>tobj</td><td>Timed objects (LOD variants by time of day)</td></tr>
<tr><td>weap</td><td>Weapon models</td></tr>
<tr><td>cars</td><td>Vehicle definitions: ID, model, txd, type, handling, flags...</td></tr>
<tr><td>peds</td><td>Pedestrian definitions</td></tr>
<tr><td>txdp</td><td>TXD parent references (SA: texture sharing between models)</td></tr>
<tr><td>2dfx</td><td>2D effects (lights, particles, escalators)</td></tr>
<tr><td>anim</td><td>Animation definitions</td></tr>
</table>

<h3>IPL — Item Placement (instance list)</h3>
<p>Defines where objects are placed in the world. Also plain text.</p>
<table>
<tr><th>Section</th><th>Content</th></tr>
<tr><td>inst</td><td>Object instances: ID, model, interior, pos xyz, rot xyzw, lot</td></tr>
<tr><td>cull</td><td>Occlusion zones</td></tr>
<tr><td>zone</td><td>Named zones</td></tr>
<tr><td>pick</td><td>Weapon pickups</td></tr>
<tr><td>path</td><td>AI paths (deprecated in SA — moved to nodes.dat)</td></tr>
</table>
<p class="note">Binary IPL: SA uses a binary variant for streaming IPL data inside GTA3.img. Identified by magic <code>bnry</code> at offset 0.</p>

<h3>DAT Files</h3>
<table>
<tr><th>File</th><th>Content</th></tr>
<tr><td>gta.dat / gta3.dat</td><td>Master load list — lists all IDE, IPL, IMG files to load</td></tr>
<tr><td>handling.cfg</td><td>Vehicle physics parameters</td></tr>
<tr><td>timecyc.dat</td><td>Time-of-day colour cycling for sky, sun, fog</td></tr>
<tr><td>object.dat</td><td>Object breakability and fire properties</td></tr>
<tr><td>pedstats.dat</td><td>Pedestrian behaviour statistics</td></tr>
<tr><td>weapon.dat</td><td>Weapon parameters and damage values</td></tr>
</table>

<h3>DAT Browser (IMG Factory)</h3>
<p>Reads gta.dat / gta3.dat to build a cross-reference of all IDE and IPL entries. IDE entries are linked to their source IMG file so the "COL" column in the file list can show which collision file covers each model. Registered in the Tool Taskbar when opened.</p>
"""

        # ══════════════════════════════════════════════════════════════════════
        # TAB 6: Platform Matrix & Detection
        # ══════════════════════════════════════════════════════════════════════
        matrix_html = css + """
<h2>Platform Support Matrix</h2>

<table>
<tr>
<th>Version Enum</th><th>Value</th><th>Platform</th><th>Container</th><th>Sector</th><th>Entry fmt</th><th>Status</th>
</tr>
<tr><td>VERSION_1</td><td>1</td><td>PC</td><td>DIR+IMG</td><td>2048</td><td>32-byte (off/sz/name)</td><td class="ok">✓</td></tr>
<tr><td>VERSION_1_5</td><td>15</td><td>PC (&gt;2GB)</td><td>DIR+IMG</td><td>2048</td><td>32-byte, long names</td><td class="ok">✓</td></tr>
<tr><td>VERSION_SOL</td><td>25</td><td>PC</td><td>DIR+IMG</td><td>2048</td><td>32-byte</td><td class="ok">✓</td></tr>
<tr><td>VERSION_2</td><td>2</td><td>PC</td><td>Single VER2</td><td>2048</td><td>32-byte in header</td><td class="ok">✓</td></tr>
<tr><td>VERSION_3</td><td>3</td><td>PC</td><td>Single 0xA94E2A52</td><td>2048</td><td>GTA IV extended</td><td class="ok">✓</td></tr>
<tr><td>VERSION_3_ENC</td><td>30</td><td>PC</td><td>AES-256 ECB</td><td>2048</td><td>Encrypted dir</td><td class="ok">✓</td></tr>
<tr><td>VERSION_XBOX</td><td>50</td><td>Xbox</td><td>DIR+IMG</td><td>2048</td><td>32-byte + LZO data</td><td class="ok">✓</td></tr>
<tr><td>VERSION_PS2_VCS</td><td>40</td><td>PS2</td><td>Single (embedded dir)</td><td>512</td><td>32-byte type-code</td><td class="ok">✓</td></tr>
<tr><td>VERSION_PS2_V1</td><td>42</td><td>PS2/iOS/Android</td><td>Single (embedded dir)</td><td>512</td><td>12-byte asset_id</td><td class="ok">✓</td></tr>
<tr><td>VERSION_PS2_LVZ</td><td>41</td><td>PS2</td><td>zlib DLRW stream</td><td>512</td><td>8-byte indexed</td><td class="ok">✓</td></tr>
<tr><td>VERSION_1_IOS</td><td>47</td><td>iOS</td><td>Single *_pvr.img</td><td>512</td><td>12-byte asset_id</td><td class="ok">✓</td></tr>
<tr><td>VERSION_LCS_IOS</td><td>53</td><td>iOS</td><td>Single *_pvr.img (LCS)</td><td>512</td><td>12-byte asset_id</td><td class="ok">✓</td></tr>
<tr><td>VERSION_LCS_ANDROID</td><td>52</td><td>Android</td><td>VER2 + 0x1005FFFF TXDs</td><td>2048</td><td>32-byte</td><td class="ok">✓</td></tr>
<tr><td>VERSION_SA_ANDROID</td><td>51</td><td>Android</td><td>VER2 + texdb.dat</td><td>2048</td><td>32-byte</td><td class="ok">✓</td></tr>
<tr><td>VERSION_STREAMING_SEG</td><td>60</td><td>iOS/PSP</td><td>Raw segment (no dir)</td><td>—</td><td>—</td><td class="wrn">Info only</td></tr>
<tr><td>VERSION_BULLY</td><td>44</td><td>PS2 (CUTS.IMG)</td><td>Single name-only dir</td><td>—</td><td>64-byte name entries</td><td class="wrn">Name-list only</td></tr>
<tr><td>VERSION_ANPK</td><td>43</td><td>PSP</td><td>Named DGAN clips</td><td>—</td><td>ANPK magic + blocks</td><td class="ok">✓</td></tr>
<tr><td>VERSION_HXD</td><td>45</td><td>PS2</td><td>Bone/animation</td><td>—</td><td>float header + path</td><td class="ok">✓</td></tr>
</table>
<p class="note">Bully WORLD.IMG / GTA3.IMG uses standard VERSION_1 (not VERSION_BULLY). VERSION_BULLY is reserved for CUTS.IMG-style name-only archives.</p>

<h2>Key Detection Heuristics (How We Identified Each Format)</h2>

<h3>PS2_V1 vs Bully vs PS2_VCS — disambiguation</h3>
<table>
<tr><th>Format</th><th>Bytes[0:4]</th><th>Bytes[4:8]</th><th>Bytes[8:12]</th><th>Key test</th></tr>
<tr><td>PS2_V1</td><td>Small int (sector offset, e.g. 22)</td><td>asset_id (type&lt;&lt;16|idx)</td><td>Sector size</td><td>asset_id high byte NOT printable ASCII; bytes[4:8] non-zero</td></tr>
<tr><td>Bully CUTS</td><td>Small int (entry count, e.g. 82)</td><td>First chars of first name (printable)</td><td>More name chars</td><td>Bytes[4] is printable ASCII letter; 4+ consecutive printable chars</td></tr>
<tr><td>PS2_VCS</td><td>4-char type code ("CDIN")</td><td>Always 0x00000000</td><td>Sector offset</td><td>Bytes[0:4] all printable AND bytes[4:8] == 0x00000000</td></tr>
</table>

<h3>False-Positive Avoidance</h3>
<ul>
<li><b>GTA3.IMG (LC) opened as PS2_V1:</b> Caused by Linux case-sensitivity — <code>GTA3.dir</code> check failed because file is <code>GTA3.DIR</code>. Fixed with <code>_find_companion()</code> case-insensitive search.</li>
<li><b>PS2_V1 first byte 0x16 rejected:</b> 0x16 = 22 is a valid small sector offset. Was wrongly in the "known RW types to reject" list alongside 0x10 (RpClump). Removed.</li>
<li><b>LC PS2 sec_off1 &lt; sec_off0:</b> Entry 0 at sector 22, entry 1 at sector 1. Our sequential-layout check (sec_off1 &gt;= sec_off0) correctly rejected this non-PS2_V1 file.</li>
</ul>

<h2>Rebuild Logic</h2>
<h3>V1 / V1.5 (DIR+IMG Pair)</h3>
<ol>
<li>Write all entry data sequentially to a temp .img file; record (sector_offset, sector_count) per entry</li>
<li>Write directory (32 bytes × N entries) to a temp .dir file using offsets from step 1</li>
<li>Atomically replace both .img and .dir files</li>
</ol>
<p class="note">Previous bug: single-file rebuild wrote dir+data into the .dir file, growing it from ~123 KB to ~116 MB.</p>

<h3>V2 / V3 (Single-File)</h3>
<ol>
<li>Write magic + entry count + directory placeholder (all zeros, 32×N bytes)</li>
<li>Align to sector boundary</li>
<li>Write each entry's data, padding to sector boundary, recording actual offsets</li>
<li>Seek back to directory placeholder and write real offsets + sizes</li>
</ol>
"""

        # ══════════════════════════════════════════════════════════════════════
        # TAB 7: Troubleshooting & Notes
        # ══════════════════════════════════════════════════════════════════════
        notes_html = css + """
<h2>Known Issues &amp; Troubleshooting</h2>

<h3>File Opens as Wrong Format</h3>
<ul>
<li><b>Symptom:</b> Garbage names, huge sizes, Korean/binary characters in Name column.</li>
<li><b>Cause:</b> Format detection chose the wrong parser. Usually means a companion .DIR file wasn't found (case-sensitivity on Linux) or a detector had a false positive.</li>
<li><b>Fix:</b> Ensure .DIR and .IMG are in the same folder. On Linux, both files must be accessible to the case-insensitive companion lookup.</li>
</ul>

<h3>RW Version Shows "Unknown"</h3>
<ul>
<li><b>Cause A:</b> Version value not in the known-valid set (e.g. Bully <code class="hex">0x1C02000A</code> before it was added).</li>
<li><b>Cause B:</b> Entry data not yet cached — version read requires opening the IMG file.</li>
<li><b>Fix:</b> Version is read lazily from the first 16 bytes of each entry on table population. If still Unknown, the RW chunk at that offset may not be a standard RW header.</li>
</ul>

<h3>Export Produces Wrong Content</h3>
<ul>
<li><b>Symptom:</b> Exported files contain directory data instead of asset data.</li>
<li><b>Cause:</b> Old code opened <code>img_archive.file_path</code> directly. For V1 files opened via .DIR, this reads directory bytes not asset data.</li>
<li><b>Fix:</b> Export uses <code>img_archive.read_entry_data(entry)</code> which correctly resolves .DIR → .IMG path.</li>
</ul>

<h3>Rebuild Makes File Much Larger</h3>
<ul>
<li><b>Symptom:</b> 104 MB .DIR grows to 312 MB after rebuild.</li>
<li><b>Cause:</b> Old rebuild wrote header + directory + all entry data into the .DIR file as a single blob.</li>
<li><b>Fix:</b> V1 rebuild writes .DIR and .IMG separately; V2+ uses seek-back to fill directory after writing data.</li>
</ul>

<h3>LCS iOS — "Open gta3.img Instead"</h3>
<p>Files like <code>indust.img</code>, <code>suburb.img</code>, <code>underg.img</code> in the iOS LCS Models folder are streaming segments — raw data with no internal directory. The directory for all segments lives in <code>gta3.img</code>. IMG Factory detects these as VERSION_STREAMING_SEG and shows a clear message.</p>

<h3>Ctrl+Up / Ctrl+Down — Entry Reordering</h3>
<p>Selected entries can be moved up or down in the entry list using Ctrl+Up / Ctrl+Down, or via the right-click context menu. This changes the order in <code>img_file.entries</code> — a rebuild is needed to write the new order to disk.</p>

<h2>IMG Factory Version Matrix</h2>
<table>
<tr><th>Component</th><th>Current Version</th><th>Notes</th></tr>
<tr><td>imgfactory.py</td><td>vers 63+</td><td>Main application window</td></tr>
<tr><td>img_core_classes.py</td><td>IMGFile vers 5</td><td>Format detection and opening</td></tr>
<tr><td>img_ps2_vcs.py</td><td>vers 8</td><td>PS2/PSP/Bully/HXD/ANPK parsers</td></tr>
<tr><td>populate_img_table.py</td><td>get_rw_version_light vers 6</td><td>RW version display</td></tr>
<tr><td>rw_versions.py</td><td>is_valid_rw_version vers 6</td><td>Version validation</td></tr>
<tr><td>rebuild.py</td><td>_perform_native_rebuild vers 9</td><td>V1 pair + V2 single-file rebuild</td></tr>
<tr><td>export.py / img_export_entry.py</td><td>export_entry vers 3</td><td>Uses read_entry_data for correct path</td></tr>
<tr><td>app_settings_system.py</td><td>SettingsDialog vers 15</td><td>Shared across IMG Factory tools</td></tr>
<tr><td>scan_img.py</td><td>vers 1 + RecentScansDialog vers 2</td><td>Recursive folder scanner + history</td></tr>
</table>
"""

        tabs.addTab(make_tab(img_html),    "IMG Formats")
        tabs.addTab(make_tab(ver_html),    "RW Versions")
        tabs.addTab(make_tab(txd_html),    "TXD / Textures")
        tabs.addTab(make_tab(col_html),    "COL Collision")
        tabs.addTab(make_tab(dat_html),    "DAT / IDE / IPL")
        tabs.addTab(make_tab(matrix_html), "Platform Matrix")
        tabs.addTab(make_tab(notes_html),  "Troubleshooting")

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.accept)
        close_btn.setFixedWidth(100)
        btn_row = QHBoxLayout()
        btn_row.addStretch()
        btn_row.addWidget(close_btn)
        layout.addLayout(btn_row)

        dialog.exec()

    def _show_imgfactory_info(self): #vers 1
        """Show IMG Factory application information dialog"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QTextEdit
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        try:
            from apps.components.Img_Factory.imgfactory import App_name, App_auth, App_build
        except:
            App_name = "IMG Factory"
            App_auth = "X-Seti"
            App_build = "1.6"

        dialog = QDialog(self.main_window)
        dialog.setWindowTitle("About IMG Factory")
        dialog.setMinimumWidth(500)
        dialog.setMinimumHeight(600)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)
        layout.setContentsMargins(20, 20, 20, 20)

        # Title
        title = QLabel(f"{App_name} {App_build}")
        title_font = QFont("Arial", 18, QFont.Weight.Bold)
        title.setFont(title_font)
        title.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(title)

        # Author
        author = QLabel(f"by {App_auth}")
        author_font = QFont("Arial", 12)
        author.setFont(author_font)
        author.setAlignment(Qt.AlignmentFlag.AlignCenter)
        author.setStyleSheet("color: palette(placeholder-text);")
        layout.addWidget(author)

        # Description
        description = QTextEdit()
        description.setReadOnly(True)
        description.setMaximumHeight(200)
        description.setHtml("""
            <h3>Professional IMG Archive Manager</h3>
            <p><b>IMG Factory</b> is a comprehensive tool for managing Grand Theft Auto game archives.</p>

            <p><b>Features:</b></p>
            <ul>
                <li>IMG, COL, TXD, DFF file support</li>
                <li>Import, export, and batch operations</li>
                <li>Built-in editors for collision and texture files</li>
                <li>IDE integration for proper file ordering</li>
                <li>Template system for quick project setup</li>
                <li>Undo/Redo system</li>
                <li>Custom UI modes</li>
            </ul>

            <p><b>Based on:</b> Original IMG Factory by MexUK (2007)</p>
            <p><b>Python Edition:</b> Complete rewrite in Python/PyQt6</p>
        """)
        layout.addWidget(description)

        # Version info
        version_info = QLabel(f"Version: {App_build} | Python Edition")
        version_info.setAlignment(Qt.AlignmentFlag.AlignCenter)
        version_info.setStyleSheet("color: palette(placeholder-text); font-size: 10px;")
        layout.addWidget(version_info)

        layout.addStretch()

        # Close button
        close_btn = QPushButton("Close")
        close_btn.setFixedWidth(100)
        close_btn.clicked.connect(dialog.accept)

        btn_layout = QHBoxLayout()
        btn_layout.addStretch()
        btn_layout.addWidget(close_btn)
        btn_layout.addStretch()

        layout.addLayout(btn_layout)

        dialog.exec()

    def show_search_dialog(self):  # vers 1
        """Show the search dialog"""
        try:
            # Create and show the search dialog
            search_dialog = ASearchDialog(self.main_window)
            search_dialog.exec()
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Search dialog error: {str(e)}")


    def find_next(self):  # vers 1
        """Find next occurrence of search term"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager.find_next()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Find Next: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Find Next error: {str(e)}")


    def find_previous(self):  # vers 1
        """Find previous occurrence of search term"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager.find_previous()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Find Previous: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Find Previous error: {str(e)}")


    def replace_dialog(self):  # vers 1
        """Show replace dialog"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                # Create and show the replace dialog
                from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit, QPushButton, QCheckBox
                dialog = QDialog(self.main_window)
                dialog.setWindowTitle("Replace Text")
                dialog.setModal(True)

                layout = QVBoxLayout()

                # Search text
                search_layout = QHBoxLayout()
                search_layout.addWidget(QLabel("Find:"))
                search_input = QLineEdit()
                search_layout.addWidget(search_input)
                layout.addLayout(search_layout)

                # Replace text
                replace_layout = QHBoxLayout()
                replace_layout.addWidget(QLabel("Replace with:"))
                replace_input = QLineEdit()
                replace_layout.addWidget(replace_input)
                layout.addLayout(replace_layout)

                # Options
                case_sensitive_check = QCheckBox("Case sensitive")
                layout.addWidget(case_sensitive_check)

                regex_check = QCheckBox("Regular expression")
                layout.addWidget(regex_check)

                # Buttons
                button_layout = QHBoxLayout()
                replace_btn = QPushButton("Replace")
                replace_all_btn = QPushButton("Replace All")
                cancel_btn = QPushButton("Cancel")

                def on_replace():
                    search_text = search_input.text().strip()
                    replace_text = replace_input.text()
                    if not search_text:
                        return

                    options = {
                        'case_sensitive': case_sensitive_check.isChecked(),
                        'regex': regex_check.isChecked(),
                        'search_mode': 'replace_one',
                        'replace_text': replace_text
                    }

                    # Perform search first to find matches
                    self.main_window.search_manager.perform_search(search_text, options)

                def on_replace_all():
                    search_text = search_input.text().strip()
                    replace_text = replace_input.text()
                    if not search_text:
                        return

                    options = {
                        'case_sensitive': case_sensitive_check.isChecked(),
                        'regex': regex_check.isChecked(),
                        'search_mode': 'replace_all',
                        'replace_text': replace_text
                    }

                    # Perform replace all
                    self.main_window.search_manager.perform_search(search_text, options)
                    dialog.accept()

                replace_btn.clicked.connect(on_replace)
                replace_all_btn.clicked.connect(on_replace_all)
                cancel_btn.clicked.connect(dialog.reject)

                button_layout.addWidget(replace_btn)
                button_layout.addWidget(replace_all_btn)
                button_layout.addStretch()
                button_layout.addWidget(cancel_btn)

                layout.addLayout(button_layout)
                dialog.setLayout(layout)
                dialog.exec()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Replace Dialog: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Replace Dialog error: {str(e)}")


    def advanced_search(self):  # vers 1
        """Show advanced search options"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager.show_search_dialog()
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Advanced Search: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Advanced Search error: {str(e)}")


    def clear_search(self):  # vers 1
        """Clear current search"""
        try:
            if hasattr(self.main_window, 'search_manager'):
                self.main_window.search_manager._clear_search()
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Search cleared")
            else:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message("Clear Search: Search manager not available")
        except Exception as e:
            if hasattr(self.main_window, 'log_message'):
                self.main_window.log_message(f"Clear Search error: {str(e)}")


    def move_entries_up(self):  # vers 3
        """Move selected entries up - updates file_object.entries and pushes undo."""
        try:
            from apps.methods.export_shared import get_active_table
            from apps.methods.tab_system import get_current_file_from_active_tab
            from apps.methods.img_entry_operations import move_entries_in_file
            self.table = get_active_table(self.main_window) or self.table
            if not self.table or not self.table.selectedItems():
                self.main_window.log_message("No entries selected to move")
                return
            rows = sorted(set(i.row() for i in self.table.selectedItems()))
            file_object, _ = get_current_file_from_active_tab(self.main_window)
            if not file_object:
                return
            if move_entries_in_file(self.main_window, file_object, rows, -1):
                from apps.core.undo_system import refresh_after_undo
                refresh_after_undo(self.main_window)
                # Re-select moved rows
                new_start = min(rows) - 1
                self.table.clearSelection()
                for i in range(len(rows)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start + i, col)
                        if item:
                            item.setSelected(True)
                self.main_window.log_message(f"{len(rows)} entries moved up")
        except Exception as e:
            self.main_window.log_message(f"Move up error: {str(e)}")

    def move_entries_down(self):  # vers 3
        """Move selected entries down - updates file_object.entries and pushes undo."""
        try:
            from apps.methods.export_shared import get_active_table
            from apps.methods.tab_system import get_current_file_from_active_tab
            from apps.methods.img_entry_operations import move_entries_in_file
            self.table = get_active_table(self.main_window) or self.table
            if not self.table or not self.table.selectedItems():
                self.main_window.log_message("No entries selected to move")
                return
            rows = sorted(set(i.row() for i in self.table.selectedItems()))
            file_object, _ = get_current_file_from_active_tab(self.main_window)
            if not file_object:
                return
            if move_entries_in_file(self.main_window, file_object, rows, 1):
                from apps.core.undo_system import refresh_after_undo
                refresh_after_undo(self.main_window)
                new_start = min(rows) + 1
                self.table.clearSelection()
                for i in range(len(rows)):
                    for col in range(self.table.columnCount()):
                        item = self.table.item(new_start + i, col)
                        if item:
                            item.setSelected(True)
                self.main_window.log_message(f"{len(rows)} entries moved down")
        except Exception as e:
            self.main_window.log_message(f"Move down error: {str(e)}")

    def _debug_tabs(self): #vers 1
        """Debug tab structure - TEMPORARY"""
        msg = "=== TAB DEBUG ===\n"

        # Check main_tab_widget
        if hasattr(self.main_window, 'main_tab_widget'):
            mtw = self.main_window.main_tab_widget
            msg += f"main_tab_widget: {mtw}\n"
            msg += f"  Count: {mtw.count()}\n"
            for i in range(mtw.count()):
                msg += f"  Tab {i}: {mtw.tabText(i)}\n"
        else:
            msg += "main_tab_widget: NOT FOUND\n"

        msg += "\n"

        # Check gui_layout.tab_widget
        if hasattr(self.main_window, 'gui_layout'):
            if hasattr(self.main_window.gui_layout, 'tab_widget'):
                gtw = self.main_window.gui_layout.tab_widget
                msg += f"gui_layout.tab_widget: {gtw}\n"
                msg += f"  Count: {gtw.count()}\n"
                for i in range(gtw.count()):
                    msg += f"  Tab {i}: {gtw.tabText(i)}\n"
            else:
                msg += "gui_layout.tab_widget: NOT FOUND\n"
        else:
            msg += "gui_layout: NOT FOUND\n"

        print(msg)
        if hasattr(self.main_window, 'log_message'):
            self.main_window.log_message(msg)


    def _launch_theme_settings(self): #vers 1
        """Launch theme settings dialog"""
        try:
            if hasattr(self.main_window, 'app_settings'):
                from apps.utils.app_settings_system import SettingsDialog
                dialog = SettingsDialog(self.main_window.app_settings, self.main_window)
                dialog.exec()
            else:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.information(
                    self.main_window,
                    "Theme Settings",
                    "Theme settings not available"
                )
        except Exception as e:
            print(f"Error launching theme settings: {str(e)}")


    def _show_settings_context_menu(self, pos): #vers 1
        """Show context menu for settings button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self.main_window)

        # Theme action
        theme_action = menu.addAction("Theme Settings")
        theme_action.triggered.connect(self._launch_theme_settings)

        # IMG Factory settings action
        imgfactory_action = menu.addAction("IMG Factory Settings")
        imgfactory_action.triggered.connect(self._show_workshop_settings)

        menu.exec(self.properties_btn.mapToGlobal(pos))


    def _show_ai_context_menu(self, pos): #vers 1
        """Right-click menu for AI Workshop button"""
        from PyQt6.QtWidgets import QMenu

        menu = QMenu(self.main_window)

        docked_action = menu.addAction("Open AI Workshop (Docked Tab)")
        docked_action.triggered.connect(lambda: self.main_window.open_ai_workshop_docked())

        standalone_action = menu.addAction("Open AI Workshop (Standalone Window)")
        standalone_action.triggered.connect(lambda: self.main_window.open_ai_workshop_standalone())

        menu.exec(self.ai_btn.mapToGlobal(pos))


        """Apply always on top window flag"""
        current_flags = self.windowFlags()

        if self.window_always_on_top:
            new_flags = current_flags | Qt.WindowType.WindowStaysOnTopHint
        else:
            new_flags = current_flags & ~Qt.WindowType.WindowStaysOnTopHint

        if new_flags != current_flags:
            # Save state
            current_geometry = self.geometry()
            was_visible = self.isVisible()

            self.setWindowFlags(new_flags)

            self.setGeometry(current_geometry)
            if was_visible:
                self.show()


    def _scan_available_locales(self): #vers 2
        """Scan locale folder and return list of available languages"""
        import os
        import configparser

        locales = []
        locale_path = os.path.join(os.path.dirname(__file__), 'locale')

        if not os.path.exists(locale_path):
            # Easter egg: Amiga Workbench 3.1 style error
            self._show_amiga_locale_error()
            # Return default English
            return [("English", "en", None)]

        try:
            for filename in os.listdir(locale_path):
                if filename.endswith('.lang'):
                    filepath = os.path.join(locale_path, filename)

                    try:
                        config = configparser.ConfigParser()
                        config.read(filepath, encoding='utf-8')

                        if 'Metadata' in config:
                            lang_name = config['Metadata'].get('LanguageName', 'Unknown')
                            lang_code = config['Metadata'].get('LanguageCode', 'unknown')
                            locales.append((lang_name, lang_code, filepath))

                    except Exception as e:
                        if self.main_window and hasattr(self.main_window, 'log_message'):
                            self.window_context.log_message(f"Failed to load locale {filename}: {e}")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"Locale scan error: {e}")

        locales.sort(key=lambda x: x[0])

        if not locales:
            locales = [("English", "en", None)]

        return locales


    def _show_amiga_locale_error(self): #vers 2
        """Show Amiga Workbench 3.1 style error dialog"""
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QLabel, QPushButton, QHBoxLayout
        from PyQt6.QtCore import Qt
        from PyQt6.QtGui import QFont

        dialog = QDialog(self)
        dialog.setWindowTitle("Workbench Request")
        dialog.setFixedSize(450, 150)

        # Amiga Workbench styling
        dialog.setStyleSheet("""
            QDialog {
                background-color: #aaaaaa;
                border: 2px solid #ffffff;
            }
            QLabel {
                color: #000000;
                background-color: #aaaaaa;
            }
            QPushButton {
                background-color: #8899aa;
                color: #000000;
                border: 2px outset #ffffff;
                padding: 5px 15px;
                min-width: 80px;
            }
            QPushButton:pressed {
                border: 2px inset #555555;
            }
        """)

        layout = QVBoxLayout(dialog)
        layout.setSpacing(15)
        layout.setContentsMargins(20, 20, 20, 20)

        # Amiga Topaz font style
        amiga_font = QFont("Courier", 10, QFont.Weight.Normal)

        # Error message
        message = QLabel("Workbench 3.1 installer\n\nPlease insert Local disk in any drive")
        message.setFont(amiga_font)
        message.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(message)

        layout.addStretch()

        # Button layout
        button_layout = QHBoxLayout()
        button_layout.addStretch()

        # Retry and Cancel buttons (Amiga style)
        retry_btn = QPushButton("Retry")
        retry_btn.setFont(amiga_font)
        retry_btn.clicked.connect(dialog.accept)
        button_layout.addWidget(retry_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.setFont(amiga_font)
        cancel_btn.clicked.connect(dialog.reject)
        button_layout.addWidget(cancel_btn)

        button_layout.addStretch()
        layout.addLayout(button_layout)

        dialog.exec()


# - Docking functions

    def _update_dock_button_visibility(self): #vers 2
        """Show/hide dock and tearoff buttons based on docked state"""
        if hasattr(self, 'dock_btn'):
            # Hide D button when docked, show when standalone
            self.dock_btn.setVisible(not self.is_docked)

        if hasattr(self, 'tearoff_btn'):
            # T button only visible when docked and not in standalone mode
            self.tearoff_btn.setVisible(self.is_docked and not self.standalone_mode)


    def toggle_dock_mode(self): #vers 2
        """Toggle between docked and standalone mode"""
        if self.is_docked:
            self._undock_from_main()
        else:
            self._dock_to_main()

        self._update_dock_button_visibility()


    def _dock_to_main(self): #vers 9
        """Dock handled by overlay system in imgfactory - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                self.show()
                self.raise_()
                return

            # For proper docking, we need to be called from imgfactory
            # This method should be handled by imgfactory's overlay system
            if self.main_window and hasattr(self.main_window, App_name + '_docked'):
                # If available, use the main window's docking system
                self.main_window.open_col_workshop_docked()
            else:
                # Fallback: just show the window
                self.show()
                self.raise_()

            # Update dock state
            self.is_docked = True
            self._update_dock_button_visibility()

            if hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"{App_name} docked to main window")


        except Exception as e:
            print(f"Error docking: {str(e)}")
            self.show()


    def _undock_from_main(self): #vers 4
        """Undock from overlay mode to standalone window - IMPROVED"""
        try:
            if hasattr(self, 'is_overlay') and self.is_overlay:
                # Switch from overlay to normal window
                self.setWindowFlags(Qt.WindowType.Window)
                self.is_overlay = False
                self.overlay_table = None

            # Set proper window flags for standalone mode
            self.setWindowFlags(Qt.WindowType.Window)

            # Ensure proper size when undocking
            if hasattr(self, 'original_size'):
                self.resize(self.original_size)
            else:
                self.resize(1000, 700)  # Reasonable default size

            self.is_docked = False
            self._update_dock_button_visibility()

            self.show()
            self.raise_()

            if hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"{App_name} undocked to standalone")

        except Exception as e:
            print(f"Error undocking: {str(e)}")
            # Fallback
            self.setWindowFlags(Qt.WindowType.Window)
            self.show()


    def _apply_button_mode(self, dialog): #vers 1
        """Apply button display mode"""
        mode_index = self.button_mode_combo.currentIndex()
        mode_map = {0: 'both', 1: 'icons', 2: 'text'}

        new_mode = mode_map[mode_index]

        if new_mode != self.button_display_mode:
            self.button_display_mode = new_mode
            self._update_all_buttons()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                mode_names = {0: 'Icons + Text', 1: 'Icons Only', 2: 'Text Only'}
                self.window_context.log_message(f"✨ Button style: {mode_names[mode_index]}")

        dialog.close()


# - Window functionality

    def _initialize_features(self): #vers 5
        """Initialize all features after UI setup - including enhanced logging"""
        try:
            self._apply_theme()
            self._update_status_indicators()
            
            # Connect various system events to the extended log
            self._connect_extended_logging()

            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message("All features initialized")
                self.window_context.log_message("Extended logging system active")
                
                # Check if directory tree autoload is enabled
                autoload_tree = True
                if hasattr(self.main_window, 'app_settings'):
                    from apps.methods.img_factory_settings import IMGFactorySettings
                    settings = IMGFactorySettings()
                    autoload_tree = settings.get('autoload_directory_tree', True)
                
                if autoload_tree:
                    self.window_context.log_message("Directory tree autoload enabled - switching in 100ms")
                    # Automatically switch to directory tree view at startup
                    QTimer.singleShot(100, self._switch_to_directory_tree)
                else:
                    self.window_context.log_message("Directory tree autoload disabled in settings")

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"Feature init error: {str(e)}")
            import traceback
            traceback.print_exc()

    
    def _connect_extended_logging(self): #vers 1
        """Connect various system events to the extended log system"""
        try:
            # Connect search results to log
            if hasattr(self.main_window, 'search_manager') and self.main_window.search_manager:
                if hasattr(self.main_window.search_manager, 'search_results_signal'):
                    def log_search_results(results):
                        self.main_window.log_message(f"[SEARCH RESULT] Found {len(results)} items")
                        
                    self.main_window.search_manager.search_results_signal.connect(log_search_results)
            
            # Connect import/export events if available
            if hasattr(self.main_window, 'import_manager'):
                def log_import(event_data):
                    self.main_window.log_message(f"[IMPORT] {event_data}")
                
                def log_export(event_data):
                    self.main_window.log_message(f"[EXPORT] {event_data}")
                
                # Connect import/export signals if they exist
                if hasattr(self.main_window.import_manager, 'import_signal'):
                    self.main_window.import_manager.import_signal.connect(log_import)
                if hasattr(self.main_window, 'export_signal'):
                    self.main_window.export_signal.connect(log_export)
            
            # Connect file removal events
            def log_removal(file_path):
                self.main_window.log_message(f"[REMOVAL] Removed: {file_path}")
            
            # This would connect to actual removal signals when they exist in the system
            self.main_window.log_removal_handler = log_removal

        except Exception as e:
            if self.main_window and hasattr(self.main_window, 'log_message'):
                self.window_context.log_message(f"Logging connection error: {str(e)}")


    def _is_on_draggable_area(self, pos): #vers 7
        """Check if position is on draggable titlebar area

        Args:
            pos: Position in titlebar coordinates (from eventFilter)

        Returns:
            True if position is on titlebar but not on any button
        """
        if not hasattr(self, 'titlebar'):
            print("[DRAG] No titlebar attribute")
            return False

        # Verify pos is within titlebar bounds
        if not self.titlebar.rect().contains(pos):
            print(f"[DRAG] Position {pos} outside titlebar rect {self.titlebar.rect()}")
            return False

        # Check if clicking on any button - if so, NOT draggable
        for widget in self.titlebar.findChildren(QPushButton):
            if widget.isVisible():
                # Get button geometry in titlebar coordinates
                button_rect = widget.geometry()
                if button_rect.contains(pos):
                    print(f"[DRAG] Clicked on button: {widget.toolTip()}")
                    return False

        # Not on any button = draggable
        print(f"[DRAG] On draggable area at {pos}")
        return True


# - From the fixed gui - move, drag

    def _update_all_buttons(self): #vers 4
        """Update all buttons to match display mode"""
        buttons_to_update = [
            # Toolbar buttons
            ('open_btn', 'Open'),
            ('save_btn', 'Save'),
            ('save_col_btn', 'Save TXD'),
        ]

        # Adjust transform panel width based on mode
        if hasattr(self, 'nav_icon_panel'):
            if self.button_display_mode == 'icons':
                self.nav_icon_panel.setMaximumWidth(50)
            else:
                self.nav_text_panel.setMaximumWidth(200)

        for btn_name, btn_text in buttons_to_update:
            if hasattr(self, btn_name):
                button = getattr(self, btn_name)
                self._apply_button_mode_to_button(button, btn_text)
        self._update_dock_button_visibility()


    def _toggle_maximize(self): #vers 1
        """Toggle window maximize state"""
        if self.main_window.isMaximized():
            self.main_window.showNormal()
        else:
            self.main_window.showMaximized()


    def refresh_icons(self, color: str): #vers 2
        """Refresh all toolbar SVG icons using the given color (text_primary from theme)"""
        try:
            from apps.methods.imgfactory_svg_icons import SVGIconFactory
            if hasattr(self, 'menu_btn'):
                self.menu_btn.setIcon(SVGIconFactory.menu_m_icon(20, color))
            if hasattr(self, 'settings_btn'):
                self.settings_btn.setIcon(SVGIconFactory.settings_icon(20, color))
            if hasattr(self, 'rw_ref_btn'):
                self.rw_ref_btn.setIcon(SVGIconFactory.info_icon(20, color))
            if hasattr(self, 'undo_btn'):
                self.undo_btn.setIcon(SVGIconFactory.undo_icon(20, color))
            if hasattr(self, 'info_btn'):
                self.info_btn.setIcon(SVGIconFactory.info_icon(20, color))
            if hasattr(self, 'properties_btn'):
                self.properties_btn.setIcon(SVGIconFactory.properties_icon(24, color))
            if hasattr(self, 'ai_btn'):
                self.ai_btn.setIcon(SVGIconFactory.ai_icon(24, color))
            if hasattr(self, 'minimize_btn'):
                self.minimize_btn.setIcon(SVGIconFactory.minimize_icon(20, color))
            if hasattr(self, 'maximize_btn'):
                self.maximize_btn.setIcon(SVGIconFactory.maximize_icon(20, color))
            if hasattr(self, 'close_btn'):
                self.close_btn.setIcon(SVGIconFactory.close_icon(20, color))
            # Refresh tool taskbar theme
            if hasattr(self, 'main_window') and hasattr(self.main_window, 'tool_taskbar'):
                colors = {}
                if hasattr(self.main_window, 'app_settings'):
                    colors = self.main_window.app_settings.get_theme_colors() or {}
                self.main_window.tool_taskbar.apply_theme(colors)
        except Exception as e:
            print(f"refresh_icons failed: {e}")

    def closeEvent(self, event): #vers 1
        """Handle close event"""
        self.window_closed.emit()
        event.accept()



# Export functions
__all__ = [
    'IMGFactoryGUILayoutCustom', '_create_toolbar', '_show_workshop_settings'
]


# ─────────────────────────────────────────────────────────────────────────────
# Tool Taskbar
# ─────────────────────────────────────────────────────────────────────────────
