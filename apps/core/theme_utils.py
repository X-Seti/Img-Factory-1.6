#this belongs in apps/core/theme_utils.py - Version: 2
# X-Seti - March 2026 - IMG Factory 1.6
# Shared theme utilities — makes QDialogs and QWidgets theme-aware.
"""
Usage:
    from apps.core.theme_utils import apply_dialog_theme
    class MyDialog(QDialog):
        def __init__(self, parent=None):
            super().__init__(parent)
            ...build UI...
            apply_dialog_theme(self)
"""
from __future__ import annotations


def get_theme_colors(widget) -> dict:
    """Walk up the widget tree to find app_settings.get_theme_colors().
    Falls back to light IMG Factory defaults if none found.
    """
    try:
        node = widget
        while node is not None:
            if hasattr(node, "app_settings") and node.app_settings:
                return node.app_settings.get_theme_colors()
            parent = node.parent() if callable(getattr(node, "parent", None)) else None
            if parent is node or parent is None:
                break
            node = parent
    except Exception:
        pass
    return _default_colors()


def _default_colors() -> dict:
    """Light IMG-Factory defaults — matches the app_settings_system fallback values."""
    return {
        "bg_primary":           "#ffffff",
        "bg_secondary":         "#f5f5f5",
        "bg_tertiary":          "#e9ecef",
        "panel_bg":             "#f0f0f0",
        "text_primary":         "#000000",
        "text_secondary":       "#666666",
        "text_accent":          "#0066cc",
        "accent_primary":       "#0078d4",
        "accent_secondary":     "#0A7Ad4",
        "border":               "#cccccc",
        "button_normal":        "#e0e0e0",
        "button_hover":         "#d0d0d0",
        "button_pressed":       "#b1b1b1",
        "selection_background": "#0188c4",
        "selection_text":       "#ffffff",
        "table_row_even":       "#fcfcfc",
        "table_row_odd":        "#f1f1f1",
        "alternate_row":        "#fefefe",
        "grid":                 "#e0e0e0",
        "panel_entries":        "#f0fdf4",
        "panel_filter":         "#fefce8",
        "toolbar_bg":           "#fafafa",
        "success":              "#4caf50",
        "warning":              "#ff9800",
        "error":                "#f44336",
    }


# Alias for backwards compat with scan_img.py private-name calls
_get_theme_colors = get_theme_colors


def build_dialog_stylesheet(colors: dict) -> str:
    """Return a QSS string that themes a QDialog using the real app theme colours.

    Uses panel_entries for list/tree backgrounds, panel_filter for toolbar area,
    bg_primary for the dialog window, table_row_even/odd for alternating rows,
    button_normal/hover/pressed for buttons, selection_background/text for
    selections — all taken directly from the loaded theme.
    """
    bg       = colors.get("bg_primary",           "#ffffff")
    bg2      = colors.get("bg_secondary",          "#f5f5f5")
    bg3      = colors.get("bg_tertiary",           "#e9ecef")
    panel    = colors.get("panel_bg",              "#f0f0f0")
    entries  = colors.get("panel_entries",         "#f5feec")  # list / tree background
    filt_bg  = colors.get("panel_filter",          "#ffe7e5")  # toolbar / filter row
    toolbar  = colors.get("toolbar_bg",            bg2)
    fg       = colors.get("text_primary",          "#000000")
    fg2      = colors.get("text_secondary",        "#666666")
    fg_acc   = colors.get("text_accent",           "#0066cc")
    acc      = colors.get("accent_primary",        "#0078d4")
    brd      = colors.get("border",                "#cccccc")
    # Handle style — 4 types: line, gradient, dots, invisible
    _hstyle  = colors.get("handle_style", "line")
    _hcol    = colors.get("handle_color",  brd)
    _hsize   = colors.get("handle_size",   "4")
    if _hstyle == "gradient":
        handle_css = f"background: qlineargradient(x1:0,y1:0,x2:1,y2:0, stop:0 transparent, stop:0.5 {_hcol}, stop:1 transparent); width: {_hsize}px;"
    elif _hstyle == "dots":
        handle_css = f"background: {_hcol}; width: {_hsize}px; border-top: 2px dotted {acc}; border-bottom: 2px dotted {acc};"
    elif _hstyle == "invisible":
        handle_css = f"background: transparent; width: {_hsize}px;"
    else:  # line (default)
        handle_css = f"background: {_hcol}; width: {_hsize}px;"
    btn_n    = colors.get("button_normal",         "#e0e0e0")
    btn_h    = colors.get("button_hover",          "#d0d0d0")
    btn_p    = colors.get("button_pressed",        "#b1b1b1")
    sel_bg   = colors.get("selection_background",  "#0188c4")
    sel_fg   = colors.get("selection_text",        "#ffffff")
    row_even = colors.get("table_row_even",        "#fcfcfc")
    row_odd  = colors.get("table_row_odd",         entries)
    grid     = colors.get("grid",                  "#e0e0e0")

    return f"""
        QDialog, QWidget {{
            background-color: {bg};
            color: {fg};
        }}

        /*    List / Tree / Table                                        */
        QTreeWidget, QTreeView,
        QListWidget, QListView,
        QTableWidget, QTableView {{
            background-color: {entries};
            color: {fg};
            border: 1px solid {brd};
            alternate-background-color: {row_odd};
            gridline-color: {grid};
        }}
        QTreeWidget::item, QListWidget::item, QTableWidget::item {{
            padding: 2px;
            color: {fg};
            background-color: transparent;
        }}
        QTreeWidget::item:alternate, QListWidget::item:alternate,
        QTableWidget::item:alternate {{
            background-color: {row_odd};
        }}
        QTreeWidget::item:selected, QListWidget::item:selected,
        QTableWidget::item:selected {{
            background-color: {sel_bg};
            color: {sel_fg};
        }}
        QTreeWidget::item:hover, QListWidget::item:hover,
        QTableWidget::item:hover {{
            background-color: {btn_h};
            color: {fg};
        }}

        /*    Header                                                     */
        QHeaderView::section {{
            background-color: {bg2};
            color: {fg};
            border: 1px solid {brd};
            padding: 3px 6px;
            font-weight: normal;
        }}
        QHeaderView::section:hover {{
            background-color: {btn_h};
        }}

        /*    Buttons                                                     */
        QPushButton {{
            background-color: {btn_n};
            color: {fg};
            border: 1px solid {brd};
            border-radius: 4px;
            padding: 4px 12px;
            min-height: 24px;
        }}
        QPushButton:hover {{
            background-color: {btn_h};
            color: {fg};
        }}
        QPushButton:pressed {{
            background-color: {btn_p};
        }}
        QPushButton:disabled {{
            color: {fg2};
        }}
        QPushButton:default {{
            border: 2px solid {acc};
        }}

        /*    Inputs                                                      */
        QLineEdit, QTextEdit, QPlainTextEdit {{
            background-color: {filt_bg};
            color: {fg};
            border: 1px solid {brd};
            border-radius: 3px;
            padding: 3px 6px;
            selection-background-color: {sel_bg};
            selection-color: {sel_fg};
        }}
        QLineEdit:focus, QTextEdit:focus, QPlainTextEdit:focus {{
            border-color: {acc};
        }}

        /*    ComboBox                                                    */
        QComboBox {{
            background-color: {filt_bg};
            color: {fg};
            border: 1px solid {brd};
            border-radius: 3px;
            padding: 3px 6px;
            min-height: 22px;
        }}
        QComboBox:focus {{ border-color: {acc}; }}
        QComboBox::drop-down {{
            border: none;
            background-color: {btn_n};
            width: 18px;
        }}
        QComboBox QAbstractItemView {{
            background-color: {entries};
            color: {fg};
            border: 1px solid {brd};
            selection-background-color: {sel_bg};
            selection-color: {sel_fg};
        }}

        /*    SpinBox                                                     */
        QSpinBox, QDoubleSpinBox {{
            background-color: {filt_bg};
            color: {fg};
            border: 1px solid {brd};
            border-radius: 3px;
            padding: 2px 4px;
        }}
        QSpinBox:focus, QDoubleSpinBox:focus {{ border-color: {acc}; }}

        /*    ProgressBar                                                 */
        QProgressBar {{
            background-color: {bg2};
            border: 1px solid {brd};
            border-radius: 2px;
            color: transparent;
        }}
        QProgressBar::chunk {{
            background-color: {acc};
            border-radius: 2px;
        }}

        /*    Labels                                                      */
        QLabel {{
            color: {fg};
            background: transparent;
        }}

        /*    GroupBox                                                    */
        QGroupBox {{
            color: {fg};
            border: 1px solid {brd};
            border-radius: 4px;
            margin-top: 8px;
            padding-top: 4px;
            background-color: {panel};
        }}
        QGroupBox::title {{
            color: {fg_acc};
            subcontrol-origin: margin;
            left: 8px;
            padding: 0 3px;
        }}

        /*    CheckBox / RadioButton                                       */
        QCheckBox, QRadioButton {{
            color: {fg};
            spacing: 6px;
            background: transparent;
        }}
        QCheckBox::indicator, QRadioButton::indicator {{
            width: 14px;
            height: 14px;
            border: 1px solid {brd};
            background: {bg2};
        }}
        QCheckBox::indicator {{ border-radius: 2px; }}
        QRadioButton::indicator {{ border-radius: 7px; }}
        QCheckBox::indicator:checked, QRadioButton::indicator:checked {{
            background: {acc};
            border-color: {acc};
        }}

        /*    Tabs                                                        */
        QTabWidget::pane {{
            border: 1px solid {brd};
            background-color: {bg};
        }}
        QTabBar::tab {{
            background-color: {bg2};
            color: {fg2};
            border: 1px solid {brd};
            padding: 4px 12px;
            border-bottom: none;
        }}
        QTabBar::tab:selected {{
            background-color: {bg};
            color: {fg};
        }}
        QTabBar::tab:hover {{
            background-color: {btn_h};
            color: {fg};
        }}

        /*    Splitter handles — style from theme handle_style key         */
        QSplitter::handle {{
            {handle_css}
        }}
        QSplitter::handle:hover {{
            background-color: {acc}; opacity: 0.8;
        }}

        /*    Scrollbars                                                  */
        QScrollBar:vertical {{
            background: {bg2};
            width: 10px;
            border: none;
        }}
        QScrollBar::handle:vertical {{
            background: {btn_n};
            border-radius: 4px;
            min-height: 20px;
        }}
        QScrollBar::handle:vertical:hover {{ background: {acc}; }}
        QScrollBar::add-line:vertical,
        QScrollBar::sub-line:vertical {{ height: 0; }}
        QScrollBar:horizontal {{
            background: {bg2};
            height: 10px;
            border: none;
        }}
        QScrollBar::handle:horizontal {{
            background: {btn_n};
            border-radius: 4px;
            min-width: 20px;
        }}
        QScrollBar::handle:horizontal:hover {{ background: {acc}; }}
        QScrollBar::add-line:horizontal,
        QScrollBar::sub-line:horizontal {{ width: 0; }}
    """


# Alias kept for scan_img.py backwards compat
_build_dialog_stylesheet = build_dialog_stylesheet


def apply_dialog_theme(dialog, parent=None) -> None:
    """Apply the app theme stylesheet to *dialog*.
    Call at the end of __init__, after building the UI.
    """
    source = parent or dialog
    colors = get_theme_colors(source)
    dialog.setStyleSheet(build_dialog_stylesheet(colors))
