#this belongs in apps/methods/titlebar_func.py - Version: 7
# X-Seti - October 20 2026 - IMG Factory 1.6 - Workshop Titlebar

"""
Workshop Titlebar - one titlebar builder for every workshop (COL / TXD layout).
Workshops add their own parts through the _titlebar_* hooks.
"""

from PyQt6.QtWidgets import QFrame, QHBoxLayout, QLabel, QMenu, QPushButton, QToolButton
from PyQt6.QtCore import Qt, QSize

from apps.methods.imgfactory_svg_icons import SVGIconFactory

##Methods list -
# _create_toolbar
# _menu_btn_popup
# _refresh_titlebar_icons
# _tb_button
# _tb_icon
# _tb_window_button
# _titlebar_actions
# _titlebar_icon_size
# _titlebar_info
# _titlebar_left_extras
# _titlebar_menu
# _titlebar_title
# _titlebar_title_icon

##class TitlebarMixin: -


class TitlebarMixin: #vers 1
    """Shared titlebar: Menu, Settings, extras, title, actions, info, theme,
    D / T, window controls. Frame is named 'titlebar' for drag and panel image."""

    TITLEBAR_DOCKED_VISIBLE = True                    # False: hidden when docked

    def _create_toolbar(self): #vers 5
        """Build the titlebar frame; returns it (also self.titlebar)."""
        ic = self._get_icon_color()
        isz = self._titlebar_icon_size()
        self._tb_icon_btns = []

        self.toolbar = QFrame()
        self.toolbar.setFrameStyle(QFrame.Shape.StyledPanel)
        self.toolbar.setObjectName("titlebar")
        self.toolbar.setMaximumHeight(50)
        self.toolbar.installEventFilter(self)          # drag detection
        self.toolbar.setAttribute(Qt.WidgetAttribute.WA_TransparentForMouseEvents, False)
        self.toolbar.setMouseTracking(True)
        self.titlebar = self.toolbar

        lo = QHBoxLayout(self.toolbar)
        lo.setContentsMargins(5, 5, 5, 5)
        lo.setSpacing(5)
        self.titlebar_layout = lo

        # Left: Menu, Settings, workshop extras
        title = self._titlebar_title()
        self.menu_btn = QToolButton()
        self.menu_btn.setFont(self.button_font)
        self.menu_btn.setText("Menu")
        self.menu_btn.setIcon(SVGIconFactory.menu_m_icon(isz, ic))
        self.menu_btn.setIconSize(QSize(isz, isz))
        self.menu_btn.setToolButtonStyle(Qt.ToolButtonStyle.ToolButtonTextBesideIcon)
        if self._menu_btn_popup():
            self.menu_btn.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
            self.menu_btn.setMenu(QMenu(self.menu_btn))
            self.menu_btn.menu().aboutToShow.connect(
                lambda: (self.menu_btn.menu().clear(), self._titlebar_menu(self.menu_btn.menu())))
        else:
            self.menu_btn.clicked.connect(lambda _c=False: self._on_menu_btn_clicked())
        self.menu_btn.setToolTip(f"All {title} commands")
        self._tb_icon_btns.append((self.menu_btn, 'menu_m_icon', isz))
        lo.addWidget(self.menu_btn)

        self.settings_btn = self._tb_button('settings_icon', "Settings", "Workshop Settings",
                                            self._show_workshop_settings, keep_text=True)
        lo.addWidget(self.settings_btn)
        self._titlebar_left_extras(lo)

        # Centre: title
        lo.addStretch()
        icon_fn = self._titlebar_title_icon()
        if icon_fn:                                      # optional icon left of title
            self.title_icon_lbl = QLabel()
            self.title_icon_lbl.setPixmap(self._tb_icon(icon_fn)(isz, ic).pixmap(isz, isz))
            lo.addWidget(self.title_icon_lbl)
        self.title_label = QLabel(title)
        self.title_label.setFont(self.title_font)
        self.title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        self.title_label.setObjectName("title_label")
        lo.addWidget(self.title_label)
        lo.addStretch()

        # Right: workshop actions, info, theme, dock, tear-off, window controls
        self._titlebar_actions(lo)
        lo.addSpacing(10)

        if (type(self)._titlebar_info is not TitlebarMixin._titlebar_info
                or hasattr(self, '_show_about')):        # only when there is info to show
            self.info_btn = self._tb_window_button('info_icon', "Information", self._titlebar_info)
            lo.addWidget(self.info_btn)

        self.properties_btn = self._tb_window_button('properties_icon', "Theme",
                                                     self._launch_theme_settings, 24)
        if hasattr(self, '_show_settings_context_menu'):
            self.properties_btn.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
            self.properties_btn.customContextMenuRequested.connect(self._show_settings_context_menu)
        lo.addWidget(self.properties_btn)

        if hasattr(self, 'toggle_dock_mode'):
            self.dock_btn = self._tb_window_button(None, "Dock", self.toggle_dock_mode, text="D")
            lo.addWidget(self.dock_btn)

        if not self.standalone_mode and hasattr(self, '_toggle_tearoff'):
            self.tearoff_btn = self._tb_window_button(None, f"{title} - Tearoff window",
                                                      self._toggle_tearoff, text="T")
            lo.addWidget(self.tearoff_btn)

        self.minimize_btn = self._tb_window_button('minimize_icon', "Minimize Window",
                                                   self.showMinimized)
        self.maximize_btn = self._tb_window_button('maximize_icon', "Maximize/Restore Window",
                                                   self._toggle_maximize)
        self.close_btn = self._tb_window_button('close_icon', "Close Window", self.close)
        for b in (self.minimize_btn, self.maximize_btn, self.close_btn):
            lo.addWidget(b)

        if not self.TITLEBAR_DOCKED_VISIBLE:
            self.toolbar.setVisible(self.standalone_mode)
        return self.toolbar

    def _menu_btn_popup(self): #vers 1
        """True: Menu drops down; False: clicked calls _on_menu_btn_clicked."""
        return True

    def _refresh_titlebar_icons(self): #vers 2
        """Recolour titlebar icons after a theme change."""
        ic = self._get_icon_color()
        for btn, icon_fn, size in getattr(self, '_tb_icon_btns', []):
            try:
                btn.setIcon(self._tb_icon(icon_fn)(size, ic))
            except RuntimeError:                           # button deleted
                pass

    def _tb_button(self, icon_fn, text, tip, slot, keep_text=False): #vers 5
        """Action button: icon (name or function) plus text; icon only when text empty or icons mode."""
        isz = self._titlebar_icon_size()
        b = QPushButton(text)
        b.setFont(self.button_font)
        b.setIcon(self._tb_icon(icon_fn)(isz, self._get_icon_color()))
        b.setIconSize(QSize(isz, isz))
        b.setToolTip(tip)
        b.clicked.connect(lambda _c=False, f=slot: f())   # no checked arg
        if not text:
            b.setFixedSize(35, 35)
        elif not keep_text and getattr(self, 'button_display_mode', 'both') == 'icons':
            b.setText("")
            b.setFixedSize(40, 40)
        self._tb_icon_btns.append((b, icon_fn, isz))
        return b

    def _tb_icon(self, icon_fn): #vers 1
        """Icon maker: SVGIconFactory name or a (size, color) function."""
        return getattr(SVGIconFactory, icon_fn) if isinstance(icon_fn, str) else icon_fn

    def _tb_window_button(self, icon_fn, tip, slot, size=20, text=""): #vers 2
        """Fixed-width right-side button (info, theme, D, T, window controls)."""
        b = QPushButton(text)
        if icon_fn:
            b.setIcon(self._tb_icon(icon_fn)(size, self._get_icon_color()))
            b.setIconSize(QSize(20, 20))
            self._tb_icon_btns.append((b, icon_fn, size))
        b.setFixedSize(40, 35)
        b.setToolTip(tip)
        b.clicked.connect(lambda _c=False, f=slot: f())   # no checked arg
        return b

    def _titlebar_actions(self, layout): #vers 1
        """Hook: workshop action buttons (Open, Save, Undo...)."""
        pass

    def _titlebar_icon_size(self): #vers 1
        """Icon size from Global App Settings."""
        from apps.utils.app_settings_system import get_titlebar_sizes
        _as = getattr(self, 'app_settings', None) or getattr(
              getattr(self, 'main_window', None), 'app_settings', None)
        return get_titlebar_sizes(_as)['icon_size']

    def _titlebar_info(self): #vers 1
        """Hook: info button action."""
        self._show_about()

    def _titlebar_left_extras(self, layout): #vers 1
        """Hook: widgets after Settings; default standalone file tabs."""
        if hasattr(self, '_build_doc_tabs_btn'):
            self._build_doc_tabs_btn(layout)

    def _titlebar_menu(self, menu): #vers 1
        """Hook: fill the Menu drop-down."""
        self._build_menus_into_qmenu(menu)

    def _titlebar_title_icon(self): #vers 1
        """Hook: icon name or (size, color) icon function left of the title, or None."""
        return None

    def _titlebar_title(self): #vers 1
        """Hook: centre title text."""
        return getattr(self, 'App_name', '') or self.windowTitle()
