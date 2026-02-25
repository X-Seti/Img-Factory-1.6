#this belongs in core/undo_system.py - Version: 2
# X-Seti - February25 2026 - IMG Factory 1.6 - Undo System
"""
Undo System - Undo/redo for rename, remove, move-up/down, import, replace.
Pinned entries are locked from rename, move, remove, replace.
"""

import copy
from typing import List, Optional
from PyQt6.QtWidgets import QMessageBox

##Methods list -
# check_pinned_lock
# integrate_undo_system
# refresh_after_undo

##Classes -
# UndoCommand
# RenameCommand
# RemoveCommand
# MoveCommand
# ImportCommand
# ReplaceCommand
# UndoManager


from datetime import datetime

DATE_FMT = "%b %d %Y"  # e.g. "Feb 25 2026"


def set_entry_date(entry): #vers 1
    """Stamp entry.date_modified with today's date string."""
    entry.date_modified = datetime.now().strftime(DATE_FMT)


def get_pin_row_colours(main_window): #vers 1
    """Return (QColor bg, QColor fg) for pinned rows based on current theme.
    Dark theme: text_primary darkened 10%.  Light theme: text_primary lightened 10%."""
    from PyQt6.QtGui import QColor
    try:
        settings = getattr(main_window, 'app_settings', None)
        if settings and hasattr(settings, 'get_theme_colors'):
            theme = getattr(settings, 'current_settings', {}).get('theme', 'default')
            colors = settings.get_theme_colors(theme)
        else:
            colors = {}
        bg_primary = colors.get('bg_primary', '#1e1e1e')
        text_primary = colors.get('text_primary', '#ffffff')

        # Detect dark theme by bg brightness
        bg = QColor(bg_primary)
        is_dark = bg.lightness() < 128

        tp = QColor(text_primary)
        if is_dark:
            # darken text_primary by 10% for bg, keep text amber
            factor = max(0, tp.lightness() - 25)
            bg_col = QColor.fromHsl(30, 180, factor)        # warm amber shade
            fg_col = QColor.fromHsl(40, 220, min(255, tp.lightness() + 60))
        else:
            # lighten text_primary by 10% for bg
            factor = min(255, tp.lightness() + 40)
            bg_col = QColor.fromHsl(40, 160, factor)        # pale amber
            fg_col = QColor.fromHsl(30, 200, max(0, tp.lightness() - 80))
        return bg_col, fg_col
    except Exception:
        from PyQt6.QtGui import QColor
        return QColor(80, 60, 20), QColor(255, 200, 80)


def check_pinned_lock(main_window, entries, operation: str) -> bool: #vers 1
    """Return True (blocked) if any entry is pinned. Shows warning."""
    locked = [getattr(e, 'name', '?') for e in entries
              if getattr(e, 'is_pinned', False)]
    if not locked:
        return False
    names = ', '.join(locked[:5]) + (', ...' if len(locked) > 5 else '')
    QMessageBox.warning(
        main_window, "Pinned - Locked",
        f"Cannot {operation}: the following entries are pinned and locked:\n\n{names}"
        f"\n\nUnpin them first."
    )
    return True


class UndoCommand:
    def __init__(self, name: str):
        self.name = name
    def execute(self): raise NotImplementedError
    def undo(self): raise NotImplementedError


class RenameCommand(UndoCommand): #vers 1
    def __init__(self, entry, old_name: str, new_name: str):
        super().__init__("Rename")
        self.entry = entry
        self.old_name = old_name
        self.new_name = new_name
    def execute(self): self.entry.name = self.new_name
    def undo(self): self.entry.name = self.old_name


class RemoveCommand(UndoCommand): #vers 1
    def __init__(self, file_object, entries_with_indices: list):
        super().__init__("Remove")
        self.file_object = file_object
        self.removed = [(idx, copy.deepcopy(entry)) for idx, entry in entries_with_indices]
    def execute(self):
        names = {e.name for _, e in self.removed}
        self.file_object.entries = [e for e in self.file_object.entries if e.name not in names]
    def undo(self):
        for idx, entry in sorted(self.removed, key=lambda x: x[0]):
            idx = min(idx, len(self.file_object.entries))
            self.file_object.entries.insert(idx, entry)


class MoveCommand(UndoCommand): #vers 1
    def __init__(self, file_object, old_order: list, new_order: list):
        super().__init__("Move")
        self.file_object = file_object
        self.old_order = list(old_order)
        self.new_order = list(new_order)
    def execute(self): self.file_object.entries[:] = self.new_order
    def undo(self): self.file_object.entries[:] = self.old_order


class ImportCommand(UndoCommand): #vers 1
    def __init__(self, file_object, imported_names: list):
        super().__init__("Import")
        self.file_object = file_object
        self.imported_names = set(imported_names)
    def execute(self): pass
    def undo(self):
        self.file_object.entries = [
            e for e in self.file_object.entries if e.name not in self.imported_names
        ]


class ReplaceCommand(UndoCommand): #vers 1
    def __init__(self, entry, old_data, new_data, old_size, new_size):
        super().__init__("Replace")
        self.entry = entry
        self.old_data = old_data
        self.new_data = new_data
        self.old_size = old_size
        self.new_size = new_size
    def execute(self):
        self.entry.data = self.new_data
        self.entry.size = self.new_size
    def undo(self):
        self.entry.data = self.old_data
        self.entry.size = self.old_size


class UndoManager: #vers 1
    def __init__(self, max_commands: int = 100):
        self.max_commands = max_commands
        self.commands: List[UndoCommand] = []
        self.current_index = -1

    def push(self, command: UndoCommand):
        self.commands = self.commands[:self.current_index + 1]
        self.commands.append(command)
        if len(self.commands) > self.max_commands:
            self.commands.pop(0)
        else:
            self.current_index += 1

    def push_command(self, command: UndoCommand):
        self.push(command)

    def undo(self) -> Optional[str]:
        if self.current_index >= 0:
            cmd = self.commands[self.current_index]
            cmd.undo()
            self.current_index -= 1
            return cmd.name
        return None

    def redo(self) -> Optional[str]:
        if self.current_index < len(self.commands) - 1:
            self.current_index += 1
            cmd = self.commands[self.current_index]
            cmd.execute()
            return cmd.name
        return None

    def can_undo(self) -> bool: return self.current_index >= 0
    def can_redo(self) -> bool: return self.current_index < len(self.commands) - 1
    def peek_undo(self) -> Optional[str]:
        return self.commands[self.current_index].name if self.current_index >= 0 else None
    def clear(self):
        self.commands.clear()
        self.current_index = -1


def refresh_after_undo(main_window): #vers 2
    """Repopulate active tab table after undo/redo, then reapply pins."""
    try:
        file_object = getattr(main_window, 'current_img', None) or getattr(main_window, 'current_col', None)
        if file_object and hasattr(main_window, '_populate_real_img_table'):
            main_window._populate_real_img_table(file_object)
        elif hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
        if file_object:
            file_object.modified = True
        # Reapply pin colours after table repopulation
        if file_object and hasattr(file_object, 'file_path') and file_object.file_path:
            if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'load_and_apply_pins'):
                main_window.gui_layout.load_and_apply_pins(file_object.file_path)
        if hasattr(main_window, 'update_img_status'):
            main_window.update_img_status(img_file=file_object)
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Undo refresh error: {str(e)}")


def integrate_undo_system(main_window) -> bool: #vers 2
    """Integrate undo system into main window."""
    try:
        main_window.undo_manager = UndoManager()

        def undo_action():
            name = main_window.undo_manager.undo()
            if name:
                refresh_after_undo(main_window)
                main_window.log_message(f"Undo: {name}")
                return True
            main_window.log_message("Nothing to undo")
            return False

        def redo_action():
            name = main_window.undo_manager.redo()
            if name:
                refresh_after_undo(main_window)
                main_window.log_message(f"Redo: {name}")
                return True
            main_window.log_message("Nothing to redo")
            return False

        main_window.undo = undo_action
        main_window.redo = redo_action
        main_window.clear_undo = lambda: main_window.undo_manager.clear()
        main_window.log_message("Undo system integrated")
        return True
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Undo system failed: {str(e)}")
        return False


__all__ = [
    'UndoCommand', 'RenameCommand', 'RemoveCommand',
    'MoveCommand', 'ImportCommand', 'ReplaceCommand',
    'UndoManager', 'check_pinned_lock',
    'set_entry_date',
    'get_pin_row_colours',
    'integrate_undo_system', 'refresh_after_undo',
]
