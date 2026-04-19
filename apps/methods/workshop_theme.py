# apps/methods/workshop_theme.py — Version 1
# Shared theme application for all IMG Factory workshops.

def get_theme_colors(main_window=None):
    """Return theme color dict from app_settings, with sensible defaults."""
    defaults = {
        'bg_primary':        '#2b2b2b',
        'bg_secondary':      '#1e1e1e',
        'bg_tertiary':       '#3a3a3a',
        'panel_bg':          '#2b2b2b',
        'panel_entries':     '#252535',
        'text_primary':      '#e0e0e0',
        'text_secondary':    '#b0b0b0',
        'text_accent':       '#4db6ac',
        'accent_primary':    '#0d47a1',
        'accent_secondary':  '#1565c0',
        'border':            '#3a3a3a',
        'button_normal':     '#3a3a3a',
        'button_hover':      '#4a4a4a',
        'button_pressed':    '#2a2a2a',
        'button_text_color': '#e0e0e0',
        'table_row_odd':     '#252535',
        'table_row_even':    '#2a2a3a',
        'alternate_row':     '#2a2a3a',
        'selection_text':    '#ffffff',
        'error':             '#dc2626',
        'grid':              '#3a3a3a',
    }
    if main_window is None:
        return defaults
    try:
        app_settings = getattr(main_window, 'app_settings', None)
        if app_settings and hasattr(app_settings, 'get_theme_colors'):
            colors = app_settings.get_theme_colors() or {}
            merged = dict(defaults)
            merged.update({k: v for k, v in colors.items() if v})
            return merged
    except Exception:
        pass
    return defaults


def build_stylesheet(colors):
    """Build a comprehensive Qt stylesheet from theme color dict."""
    bg      = colors.get('bg_primary',        '#2b2b2b')
    bg2     = colors.get('bg_secondary',       '#1e1e1e')
    bg3     = colors.get('bg_tertiary',        '#3a3a3a')
    panel   = colors.get('panel_bg',           bg)
    entries = colors.get('panel_entries',      bg2)
    fg      = colors.get('text_primary',       '#e0e0e0')
    fg2     = colors.get('text_secondary',     '#b0b0b0')
    fg_acc  = colors.get('text_accent',        '#4db6ac')
    acc     = colors.get('accent_primary',     '#0d47a1')
    acc2    = colors.get('accent_secondary',   '#1565c0')
    border  = colors.get('border',             '#3a3a3a')
    btn     = colors.get('button_normal',      bg3)
    btn_h   = colors.get('button_hover',       '#4a4a4a')
    btn_p   = colors.get('button_pressed',     '#2a2a2a')
    btn_fg  = colors.get('button_text_color',  fg)
    row_odd = colors.get('table_row_odd',      bg2)
    row_even= colors.get('table_row_even',     colors.get('alternate_row', bg3))
    sel_fg  = colors.get('selection_text',     '#ffffff')
    grid    = colors.get('grid',               border)

    return f"""
    QWidget {{ background-color: {panel}; color: {fg}; selection-background-color: {acc}; selection-color: {sel_fg}; }}
    QFrame {{ background-color: {panel}; color: {fg}; }}
    QDialog, QMainWindow {{ background-color: {panel}; }}
    QLabel {{ background-color: transparent; color: {fg}; }}
    QPushButton {{ background-color: {btn}; color: {btn_fg}; border: 1px solid {border}; padding: 4px 8px; border-radius: 3px; }}
    QPushButton:hover {{ background-color: {btn_h}; }}
    QPushButton:pressed {{ background-color: {btn_p}; }}
    QPushButton:checked {{ background-color: {acc}; color: {sel_fg}; border-color: {acc2}; }}
    QPushButton:disabled {{ background-color: {bg2}; color: {fg2}; border-color: {border}; }}
    QListWidget, QTreeWidget, QTableWidget {{ background-color: {entries}; alternate-background-color: {row_even}; color: {fg}; border: 1px solid {border}; gridline-color: {grid}; }}
    QListWidget::item, QTreeWidget::item, QTableWidget::item {{ background-color: {row_odd}; color: {fg}; padding: 2px; }}
    QListWidget::item:alternate, QTreeWidget::item:alternate, QTableWidget::item:alternate {{ background-color: {row_even}; }}
    QListWidget::item:selected, QTreeWidget::item:selected, QTableWidget::item:selected {{ background-color: {acc}; color: {sel_fg}; }}
    QListWidget::item:hover, QTreeWidget::item:hover, QTableWidget::item:hover {{ background-color: {acc2}; color: {sel_fg}; }}
    QHeaderView::section {{ background-color: {btn}; color: {btn_fg}; border: none; border-right: 1px solid {border}; border-bottom: 1px solid {border}; padding: 3px 6px; font-weight: bold; }}
    QHeaderView::section:hover {{ background-color: {btn_h}; }}
    QLineEdit, QTextEdit, QPlainTextEdit {{ background-color: {bg2}; color: {fg}; border: 1px solid {border}; padding: 2px 4px; selection-background-color: {acc}; selection-color: {sel_fg}; }}
    QLineEdit:focus, QTextEdit:focus {{ border-color: {acc}; }}
    QLineEdit:disabled {{ background-color: {bg3}; color: {fg2}; }}
    QComboBox, QSpinBox, QDoubleSpinBox {{ background-color: {bg2}; color: {fg}; border: 1px solid {border}; padding: 2px 4px; }}
    QComboBox::drop-down {{ border-left: 1px solid {border}; }}
    QComboBox QAbstractItemView {{ background-color: {entries}; color: {fg}; selection-background-color: {acc}; selection-color: {sel_fg}; }}
    QTabWidget::pane {{ background-color: {panel}; border: 1px solid {border}; }}
    QTabBar::tab {{ background-color: {btn}; color: {btn_fg}; border: 1px solid {border}; padding: 4px 12px; margin-right: 2px; }}
    QTabBar::tab:selected {{ background-color: {panel}; color: {fg}; border-bottom-color: {panel}; }}
    QTabBar::tab:hover {{ background-color: {btn_h}; }}
    QScrollBar:vertical, QScrollBar:horizontal {{ background-color: {bg2}; border: none; }}
    QScrollBar::handle:vertical, QScrollBar::handle:horizontal {{ background-color: {bg3}; border-radius: 3px; min-height: 20px; }}
    QScrollBar::handle:hover {{ background-color: {btn_h}; }}
    QScrollBar::add-line, QScrollBar::sub-line {{ background: none; border: none; }}
    QGroupBox {{ border: 1px solid {border}; border-radius: 4px; margin-top: 8px; color: {fg}; }}
    QGroupBox::title {{ subcontrol-origin: margin; left: 8px; color: {fg_acc}; }}
    QSplitter::handle {{ background-color: {border}; }}
    QProgressBar {{ background-color: {bg2}; color: {fg}; border: 1px solid {border}; text-align: center; }}
    QProgressBar::chunk {{ background-color: {acc}; }}
    QSlider::groove:horizontal {{ background-color: {bg2}; height: 4px; border-radius: 2px; }}
    QSlider::handle:horizontal {{ background-color: {acc}; width: 14px; height: 14px; margin: -5px 0; border-radius: 7px; }}
    QMenu {{ background-color: {entries}; color: {fg}; border: 1px solid {border}; }}
    QMenu::item:selected {{ background-color: {acc}; color: {sel_fg}; }}
    QToolTip {{ background-color: {bg3}; color: {fg}; border: 1px solid {border}; padding: 2px; }}
    QCheckBox, QRadioButton {{ color: {fg}; background-color: transparent; }}
    QCheckBox::indicator, QRadioButton::indicator {{ border: 1px solid {border}; background-color: {bg2}; }}
    QCheckBox::indicator:checked {{ background-color: {acc}; border-color: {acc}; }}
    """


def apply_workshop_theme(widget, main_window=None): #vers 1
    """Apply the current app theme to a workshop widget."""
    try:
        colors = get_theme_colors(main_window)
        ss = build_stylesheet(colors)
        widget.setStyleSheet(ss)
        return True
    except Exception as e:
        print(f"apply_workshop_theme error: {e}")
        return False
