# Hex Editor — uses hex_workshop.py (standalone)
# Old Hex_Editor.py and Hex_Editor_Panel.py moved to bugs/
from .hex_workshop import (
    HexWorkshop,
    HexViewWidget,
    StructureView,
    show_hex_editor_for_file,
    show_hex_editor_for_entry,
    open_hex_workshop,
)
# Backward compat aliases
HexEditorDialog = HexWorkshop

__all__ = [
    'HexWorkshop', 'HexViewWidget', 'StructureView',
    'HexEditorDialog',
    'show_hex_editor_for_file', 'show_hex_editor_for_entry',
    'open_hex_workshop',
]
