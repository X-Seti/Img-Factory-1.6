#this belongs in apps/methods/button_mode.py - Version: 1
# X-Seti - September27 2026 - IMG Factory 1.6 - Button display mode

"""
Shared button display mode (icons, text, both) for workshop buttons.
"""

##Methods list -
# apply_button_mode_to_button

from PyQt6.QtCore import QSize
from PyQt6.QtGui import QAction, QIcon


def apply_button_mode_to_button(button, text, mode): #vers 1
    """Apply icons/text/both mode to one QPushButton; skips QActions."""
    if isinstance(button, QAction) or not hasattr(button, 'setFixedSize'):
        return
    if not hasattr(button, '_original_icon'):
        button._original_icon = button.icon()

    if mode == 'icons':
        button.setText("")
        if not button._original_icon.isNull():
            button.setIcon(button._original_icon)
            button.setIconSize(QSize(20, 20))
        button.setFixedSize(40, 40)
    elif mode == 'text':
        button.setText(text)
        button.setIcon(QIcon())
        button.setMinimumWidth(60)
        button.setMaximumWidth(16777215)
        button.setMinimumHeight(0)
        button.setMaximumHeight(16777215)
    elif mode == 'both':
        button.setText(text)
        if not button._original_icon.isNull():
            button.setIcon(button._original_icon)
            button.setIconSize(QSize(20, 20))
        button.setMinimumWidth(0)
        button.setMaximumWidth(16777215)
        button.setMinimumHeight(0)
        button.setMaximumHeight(16777215)
