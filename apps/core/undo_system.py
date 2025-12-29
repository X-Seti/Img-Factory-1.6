#this belongs in core/undo_system.py - Version: 1
# X-Seti - December01 2025 - IMG Factory 1.5 - Undo System

"""
Undo System - Handles undo/redo functionality for IMG entries
"""

import copy
from typing import List, Dict, Any, Optional
from PyQt6.QtWidgets import QMessageBox

##Classes -
# UndoCommand
# UndoManager

class UndoCommand:
    """Base class for undo commands"""
    def __init__(self, name: str):
        self.name = name
    
    def execute(self):
        """Execute the command"""
        raise NotImplementedError
    
    def undo(self):
        """Undo the command"""
        raise NotImplementedError


class RenameCommand(UndoCommand):
    """Command for renaming entries"""
    def __init__(self, entry, old_name: str, new_name: str):
        super().__init__("Rename Entry")
        self.entry = entry
        self.old_name = old_name
        self.new_name = new_name
    
    def execute(self):
        """Execute rename (already done, just for consistency)"""
        self.entry.name = self.new_name
    
    def undo(self):
        """Undo rename"""
        self.entry.name = self.old_name


class UndoManager:
    """Manages undo/redo stack"""
    def __init__(self, max_commands: int = 50):
        self.max_commands = max_commands
        self.commands: List[UndoCommand] = []
        self.current_index = -1
    
    def push_command(self, command: UndoCommand):
        """Add a command to the undo stack"""
        # Remove any commands after current index (for branching undo)
        self.commands = self.commands[:self.current_index + 1]
        
        # Add new command
        self.commands.append(command)
        
        # Limit stack size
        if len(self.commands) > self.max_commands:
            self.commands.pop(0)
        else:
            self.current_index += 1
    
    def undo(self):
        """Execute undo"""
        if self.current_index >= 0:
            command = self.commands[self.current_index]
            command.undo()
            self.current_index -= 1
            return True
        return False
    
    def redo(self):
        """Execute redo"""
        if self.current_index < len(self.commands) - 1:
            self.current_index += 1
            command = self.commands[self.current_index]
            command.execute()
            return True
        return False
    
    def can_undo(self) -> bool:
        """Check if undo is possible"""
        return self.current_index >= 0
    
    def can_redo(self) -> bool:
        """Check if redo is possible"""
        return self.current_index < len(self.commands) - 1
    
    def clear(self):
        """Clear the undo stack"""
        self.commands.clear()
        self.current_index = -1


def integrate_undo_system(main_window) -> bool: #vers 1
    """Integrate undo system into main window"""
    try:
        # Create undo manager
        main_window.undo_manager = UndoManager()
        
        # Add undo/redo methods to main window
        def undo_action():
            if main_window.undo_manager.can_undo():
                success = main_window.undo_manager.undo()
                if success and hasattr(main_window, 'refresh_img_table'):
                    main_window.refresh_img_table()
                elif success and hasattr(main_window, 'refresh_table'):
                    main_window.refresh_table()
                return success
            return False
        
        def redo_action():
            if main_window.undo_manager.can_redo():
                success = main_window.undo_manager.redo()
                if success and hasattr(main_window, 'refresh_img_table'):
                    main_window.refresh_img_table()
                elif success and hasattr(main_window, 'refresh_table'):
                    main_window.refresh_table()
                return success
            return False
        
        def clear_undo():
            main_window.undo_manager.clear()
        
        # Add methods to main window
        main_window.undo = undo_action
        main_window.redo = redo_action
        main_window.clear_undo = clear_undo
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Undo system integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate undo system: {str(e)}")
        return False


# Export functions
__all__ = [
    'UndoCommand',
    'RenameCommand',
    'UndoManager',
    'integrate_undo_system'
]