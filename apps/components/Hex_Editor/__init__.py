# Hex Editor — uses hex_workshop.py (standalone)
# Old Hex_Editor.py and Hex_Editor_Panel.py moved to bugs/
from .hex_workshop import (
    HexWorkshop,
    show_hex_editor_for_file,
    show_hex_editor_for_entry,
    open_hex_workshop,
)
__all__ = [
    'HexWorkshop',
    'show_hex_editor_for_file', 'show_hex_editor_for_entry',
    'open_hex_workshop',
]
