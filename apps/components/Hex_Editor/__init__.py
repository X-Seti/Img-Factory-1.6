"""
Hex Editor Component for IMG Factory
X-Seti - December 2025
"""

# This file makes Hex_Editor a Python package
from .Hex_Editor import HexEditorDialog, show_hex_editor_for_file, show_hex_editor_for_entry
from .Hex_Editor_Panel import HexEditorDialog as HexEditorDialogPanel, show_hex_editor_for_file as show_hex_editor_for_file_panel, show_hex_editor_for_entry as show_hex_editor_for_entry_panel

__all__ = [
    'HexEditorDialog', 
    'show_hex_editor_for_file', 
    'show_hex_editor_for_entry',
    'HexEditorDialogPanel',
    'show_hex_editor_for_file_panel',
    'show_hex_editor_for_entry_panel'
]