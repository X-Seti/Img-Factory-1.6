#this belongs in apps/core/theme_utils.py - Version: 1
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


# ── Theme helper ──────────────────────────────────────────────────────────────

def get_theme_colors(parent) -> dict:
    """Return theme colour dict from parent window's app_settings, or dark defaults."""
    try:
        win = parent
        while win is not None:
            if hasattr(win, 'app_settings') and win.app_settings:
                return win.app_settings.get_theme_colors()
            win = win.parent() if hasattr(win, 'parent') and callable(win.parent) else None
    except Exception:
        pass
    return {
        'bg_primary':    '#1e1e1e',
        'bg_secondary':  '#2b2b2b',
        'bg_tertiary':   '#3a3a3a',
        'text_primary':  '#e0e0e0',
        'text_secondary':'#a0a0a0',
        'accent_primary':'#0078d4',
        'border':        '#3d3d3d',
        'success':       '#28a745',
        'warning':       '#ffc107',
        'error':         '#dc3545',
    }


def build_dialog_stylesheet(colors: dict) -> str:
    """Return a QSS string that themes a QDialog and its child widgets."""
    bg    = colors.get('bg_primary',    '#1e1e1e')
    bg2   = colors.get('bg_secondary',  '#2b2b2b')
    bg3   = colors.get('bg_tertiary',   '#3a3a3a')
    fg    = colors.get('text_primary',  '#e0e0e0')
    fg2   = colors.get('text_secondary','#a0a0a0')
    acc   = colors.get('accent_primary','#0078d4')
    brd   = colors.get('border',        '#3d3d3d')
    ok    = colors.get('success',       '#28a745')

    return f"""
        QDialog, QWidget {{
            background-color: {bg};
            color: {fg};
        }}
        QTreeWidget, QTreeView, QListWidget, QListView {{
            background-color: {bg2};
            color: {fg};
            border: 1px solid {brd};
            alternate-background-color: {bg3};
            gridline-color: {brd};
        }}
        QTreeWidget::item:selected, QListWidget::item:selected {{
            background-color: {acc};
            color: #ffffff;
        }}
        QTreeWidget::item:hover, QListWidget::item:hover {{
            background-color: {bg3};
        }}
        QHeaderView::section {{
            background-color: {bg3};
            color: {fg};
            border: 1px solid {brd};
            padding: 3px 6px;
        }}
        QPushButton {{
            background-color: {bg3};
            color: {fg};
            border: 1px solid {brd};
            border-radius: 4px;
            padding: 4px 12px;
            min-height: 24px;
        }}
        QPushButton:hover {{
            background-color: {acc};
            color: #ffffff;
            border-color: {acc};
        }}
        QPushButton:pressed {{
            background-color: {acc};
            color: #ffffff;
        }}
        QPushButton:disabled {{
            color: {fg2};
            border-color: {brd};
        }}
        QPushButton:default {{
            border: 2px solid {acc};
        }}
        QLineEdit, QComboBox {{
            background-color: {bg2};
            color: {fg};
            border: 1px solid {brd};
            border-radius: 3px;
            padding: 3px 6px;
            min-height: 22px;
        }}
        QLineEdit:focus, QComboBox:focus {{
            border-color: {acc};
        }}
        QComboBox::drop-down {{
            border: none;
            background-color: {bg3};
        }}
        QComboBox QAbstractItemView {{
            background-color: {bg2};
            color: {fg};
            border: 1px solid {brd};
            selection-background-color: {acc};
        }}
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
        QLabel {{
            color: {fg};
            background: transparent;
        }}
        QSplitter::handle {{
            background-color: {brd};
        }}
        QScrollBar:vertical {{
            background: {bg2};
            width: 10px;
            border: none;
        }}
        QScrollBar::handle:vertical {{
            background: {bg3};
            border-radius: 4px;
            min-height: 20px;
        }}
        QScrollBar::handle:vertical:hover {{
            background: {acc};
        }}
        QScrollBar::add-line:vertical, QScrollBar::sub-line:vertical {{
            height: 0px;
        }}
        QScrollBar:horizontal {{
            background: {bg2};
            height: 10px;
            border: none;
        }}
        QScrollBar::handle:horizontal {{
            background: {bg3};
            border-radius: 4px;
            min-width: 20px;
        }}
        QScrollBar::handle:horizontal:hover {{
            background: {acc};
        }}
        QScrollBar::add-line:horizontal, QScrollBar::sub-line:horizontal {{
            width: 0px;
        }}
        QGroupBox {{
            color: {fg};
            border: 1px solid {brd};
            border-radius: 4px;
            margin-top: 8px;
            padding-top: 4px;
        }}
        QGroupBox::title {{
            color: {fg};
            subcontrol-origin: margin;
            left: 8px;
            padding: 0 3px;
        }}
        QCheckBox {{
            color: {fg};
            spacing: 6px;
        }}
        QCheckBox::indicator {{
            width: 14px;
            height: 14px;
            border: 1px solid {brd};
            border-radius: 2px;
            background: {bg2};
        }}
        QCheckBox::indicator:checked {{
            background: {acc};
            border-color: {acc};
        }}
        QSplitter::handle:hover {{
            background-color: {acc};
        }}
    """



def apply_dialog_theme(dialog, parent=None) -> None:
    """Apply the app theme stylesheet to *dialog*.
    Call at the end of __init__, after building the UI.
    """
    source = parent or dialog
    colors = get_theme_colors(source)
    dialog.setStyleSheet(build_dialog_stylesheet(colors))
