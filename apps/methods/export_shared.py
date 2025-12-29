#this belongs in methods/export_shared.py - Version: 1
# X-Seti - Aug15 2025 - IMG Factory 1.5 - Shared Export Functions

"""
Shared Export Functions - Common functionality for all export operations
Contains ExportThread class and utility functions used by all export modules
"""

import os
from typing import List, Optional, Dict, Any
from PyQt6.QtWidgets import QMessageBox, QFileDialog, QProgressDialog
from PyQt6.QtCore import pyqtSignal, Qt, QThread

##Methods list -
# get_export_folder
# get_selected_entries
# organize_by_type
# validate_export_entries

##Classes -
# ExportThread

class ExportThread(QThread): #vers 2
    """Background thread for exporting files - FIXED threading issues"""
    
    progress_updated = pyqtSignal(int, str)  # progress %, message
    export_completed = pyqtSignal(bool, str, dict)  # success, message, stats
    
    def __init__(self, main_window, entries_to_export: List, export_dir: str, export_options: dict):
        super().__init__()
        self.main_window = main_window
        self.entries_to_export = entries_to_export
        self.export_dir = export_dir
        self.export_options = export_options
        self.stats = {'exported': 0, 'skipped': 0, 'failed': 0}
        self._stop_requested = False
        
    def run(self): #vers 2
        """Run export operation in background - FIXED to prevent freezing"""
        try:
            total_entries = len(self.entries_to_export)
            
            for i, entry in enumerate(self.entries_to_export):
                # Check if stop was requested
                if self._stop_requested:
                    break
                    
                entry_name = getattr(entry, 'name', f'entry_{i}')
                progress = int((i / total_entries) * 100)
                
                self.progress_updated.emit(progress, f"Exporting {entry_name}...")
                
                try:
                    # Determine output path
                    if self.export_options.get('organize_by_type', True):
                        subdir = self._get_type_subdir(entry_name)
                        output_dir = os.path.join(self.export_dir, subdir)
                    else:
                        output_dir = self.export_dir
                    
                    os.makedirs(output_dir, exist_ok=True)
                    output_path = os.path.join(output_dir, entry_name)
                    
                    # Skip if exists and not overwriting
                    if os.path.exists(output_path) and not self.export_options.get('overwrite', True):
                        self.stats['skipped'] += 1
                        continue
                    
                    # Get entry data
                    entry_data = self._get_entry_data(entry)
                    if entry_data is None:
                        self.stats['failed'] += 1
                        continue
                    
                    # Write file
                    with open(output_path, 'wb') as f:
                        f.write(entry_data)
                    
                    self.stats['exported'] += 1
                    
                except Exception as e:
                    self.stats['failed'] += 1
                    if hasattr(self.main_window, 'log_message'):
                        self.main_window.log_message(f"❌ Export error for {entry_name}: {e}")
            
            # CRITICAL: Emit final progress and completion signals
            self.progress_updated.emit(100, "Export completed!")
            
            # Create success message
            success_msg = f"Export completed: {self.stats['exported']} exported"
            if self.stats['skipped'] > 0:
                success_msg += f", {self.stats['skipped']} skipped"
            if self.stats['failed'] > 0:
                success_msg += f", {self.stats['failed']} failed"
            
            # CRITICAL: Always emit completion signal
            self.export_completed.emit(True, success_msg, self.stats)
            
        except Exception as e:
            # CRITICAL: Always emit completion signal even on error
            self.export_completed.emit(False, f"Export failed: {str(e)}", self.stats)
        finally:
            # CRITICAL: Ensure thread terminates properly
            self.quit()
            self.wait()
    
    def stop_export(self): #vers 2
        """Request export to stop and cleanup properly"""
        self._stop_requested = True
        # Force quit if running
        if self.isRunning():
            self.quit()
            if not self.wait(2000):  # Wait max 2 seconds
                self.terminate()  # Force terminate if needed
                self.wait(1000)   # Wait for termination
    
    def _get_type_subdir(self, filename: str) -> str: #vers 3
        """Get subdirectory based on file type - enhanced for Assists folder structure and COL support"""
        ext = os.path.splitext(filename)[1].lower()
        
        # Check if using assists folder structure
        if self.export_options.get('use_assists_structure', False):
            assists_mapping = {
                '.dff': 'Models',
                '.txd': 'Textures', 
                '.col': 'Collisions',  # COL files support
                '.ifp': 'Other',  # Animations go to Other for now
                '.wav': 'Audio',
                '.scm': 'Scripts',
                '.ipl': 'Maps',
                '.dat': 'Other',
                '.img': 'Other'
            }
            return assists_mapping.get(ext, 'Other')
        else:
            # Standard type mapping
            type_mapping = {
                '.dff': 'models',
                '.txd': 'textures', 
                '.col': 'collision',  # COL files support
                '.ifp': 'animations',
                '.wav': 'audio',
                '.scm': 'scripts',
                '.ipl': 'placement',
                '.dat': 'data',
                '.img': 'archives'
            }
            return type_mapping.get(ext, 'other')
    
    def _get_entry_data(self, entry) -> Optional[bytes]: #vers 1
        """Get entry data using various methods"""
        try:
            # Try different methods to get entry data
            if hasattr(entry, 'get_data'):
                return entry.get_data()
            elif hasattr(entry, '_cached_data') and entry._cached_data:
                return entry._cached_data
            elif hasattr(self.main_window, 'current_img') and hasattr(self.main_window.current_img, 'read_entry_data'):
                return self.main_window.current_img.read_entry_data(entry)
            else:
                return None
        except Exception:
            return None

def get_selected_entries(main_window) -> List: #vers 1
    """Get currently selected entries from table"""
    selected_entries = []
    
    try:
        # Try different table access methods
        table = None
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            table = main_window.gui_layout.table
        elif hasattr(main_window, 'entries_table'):
            table = main_window.entries_table
        elif hasattr(main_window, 'table'):
            table = main_window.table
        
        if not table:
            return selected_entries
        
        # Get selected rows
        selected_rows = set()
        for item in table.selectedItems():
            selected_rows.add(item.row())
        
        # Get entries for selected rows
        if hasattr(main_window, 'current_img') and main_window.current_img:
            for row in selected_rows:
                if row < len(main_window.current_img.entries):
                    selected_entries.append(main_window.current_img.entries[row])
                    
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error getting selected entries: {e}")
    
    return selected_entries

def get_export_folder(main_window, title: str = "Select Export Folder") -> Optional[str]: #vers 1
    """Get export destination folder"""
    try:
        folder = QFileDialog.getExistingDirectory(main_window, title)
        return folder if folder else None
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error selecting export folder: {e}")
        return None

def validate_export_entries(entries: List, main_window) -> bool: #vers 1
    """Validate entries before export"""
    try:
        if not entries:
            QMessageBox.information(main_window, "No Selection", "No entries selected for export")
            return False
        
        # Check if entries have required attributes
        valid_entries = 0
        for entry in entries:
            if hasattr(entry, 'name') and getattr(entry, 'name', ''):
                valid_entries += 1
        
        if valid_entries == 0:
            QMessageBox.warning(main_window, "Invalid Entries", "Selected entries appear to be invalid")
            return False
        
        if valid_entries < len(entries):
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"⚠️ {len(entries) - valid_entries} entries may have issues")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error validating entries: {e}")
        return False

def organize_by_type(entries: List) -> Dict[str, List]: #vers 1
    """Organize entries by file type"""
    organized = {}
    
    try:
        for entry in entries:
            entry_name = getattr(entry, 'name', '')
            if not entry_name:
                continue
                
            ext = os.path.splitext(entry_name)[1].lower()
            if ext not in organized:
                organized[ext] = []
            organized[ext].append(entry)
            
    except Exception as e:
        print(f"Error organizing entries: {e}")
    
    return organized

__all__ = [
    'ExportThread',
    'get_selected_entries', 
    'get_export_folder',
    'validate_export_entries',
    'organize_by_type'
]