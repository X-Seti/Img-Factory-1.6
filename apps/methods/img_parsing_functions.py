#this belongs in methods.img_parsing_functions.py - version 1
# X-Seti - July10 2025 - Img Factory 1.5
# Integration for IMG parsing fixes

from PyQt6.QtWidgets import QMessageBox, QDialog, QVBoxLayout, QTextEdit, QPushButton
from typing import List, Dict

def integrate_img_parsing_fixes(main_window):
    """Integrate IMG parsing fixes into main window"""
    try:
        from apps.components.img_parsing_fixes import patch_img_entry_class, create_filename_report
        
        # Apply patches
        if patch_img_entry_class():
            main_window.log_message("IMG parsing fixes applied")
            
            # Add parsing report method
            main_window.show_parsing_report = lambda: show_parsing_report_dialog(main_window)
            
            # Add to Tools menu if available
            add_parsing_tools_to_menu(main_window)
            
            return True
        else:
            main_window.log_message("Failed to apply IMG parsing fixes")
            return False
            
    except Exception as e:
        main_window.log_message(f"Error integrating IMG parsing fixes: {str(e)}")
        return False

def add_parsing_tools_to_menu(main_window):
    """Add parsing tools to main menu"""
    try:
        if not hasattr(main_window, 'menuBar') or not main_window.menuBar():
            return False
        
        menubar = main_window.menuBar()
        
        # Find or create Tools menu
        tools_menu = None
        for action in menubar.actions():
            if action.text() == "Tools":
                tools_menu = action.menu()
                break
        
        if not tools_menu:
            tools_menu = menubar.addMenu("Tools")
        
        # Add parsing submenu
        parsing_submenu = tools_menu.addMenu("🔧 IMG Parsing")
        
        # Parsing report
        report_action = QAction("Parsing Report", main_window)
        report_action.setStatusTip("Show filename parsing report for current IMG")
        report_action.triggered.connect(main_window.show_parsing_report)
        parsing_submenu.addAction(report_action)
        
        # Validate filenames
        validate_action = QAction("Validate Filenames", main_window)
        validate_action.setStatusTip("Validate and fix filename parsing issues")
        validate_action.triggered.connect(lambda: validate_current_img_filenames(main_window))
        parsing_submenu.addAction(validate_action)
        
        parsing_submenu.addSeparator()
        
        # Debug parsing
        debug_action = QAction("Debug Parsing", main_window)
        debug_action.setStatusTip("Show detailed parsing debug info")
        debug_action.triggered.connect(lambda: debug_img_parsing(main_window))
        parsing_submenu.addAction(debug_action)
        
        return True
        
    except Exception as e:
        main_window.log_message(f"Error adding parsing tools to menu: {str(e)}")
        return False

def show_parsing_report_dialog(main_window):
    """Show parsing report dialog"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            QMessageBox.information(main_window, "No IMG", "Please load an IMG file first.")
            return
        
        from apps.components.img_parsing_fixes import create_filename_report
        
        # Convert entries to dict format for report
        entry_dicts = []
        for entry in main_window.current_img.entries:
            entry_dict = {
                'name': getattr(entry, 'name', ''),
                'extension': getattr(entry, 'extension', ''),
                'full_name': getattr(entry, 'name', ''),
                'size': getattr(entry, 'size', 0),
                'offset': getattr(entry, 'offset', 0)
            }
            entry_dicts.append(entry_dict)
        
        # Generate report
        report_text = create_filename_report(entry_dicts)
        
        # Show in dialog
        dialog = ParsingReportDialog(report_text, main_window)
        dialog.exec()
        
    except Exception as e:
        QMessageBox.critical(main_window, "Report Error", f"Failed to generate parsing report:\n{str(e)}")

def validate_current_img_filenames(main_window):
    """Validate and fix filenames in current IMG"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            QMessageBox.information(main_window, "No IMG", "Please load an IMG file first.")
            return
        
        from apps.components.img_parsing_fixes import fix_vice_city_filename_parsing
        
        fixes_applied = 0
        issues_found = []
        
        for i, entry in enumerate(main_window.current_img.entries):
            original_name = getattr(entry, 'name', '')
            
            # Check for parsing issues
            if not original_name or len(original_name.strip()) == 0:
                issues_found.append(f"Entry {i}: Empty filename")
                continue
            
            # Try to fix the filename
            try:
                name_bytes = original_name.encode('ascii', errors='ignore')
                fixed_name, extension = fix_vice_city_filename_parsing(name_bytes)
                
                # Check if a fix was applied
                expected_full_name = f"{fixed_name}.{extension}" if extension else fixed_name
                
                if expected_full_name != original_name:
                    # Apply the fix
                    entry.name = expected_full_name
                    entry.extension = extension
                    fixes_applied += 1
                    
                    main_window.log_message(f"Fixed: {original_name} → {expected_full_name}")
                
            except Exception as e:
                issues_found.append(f"Entry {i} ({original_name}): {str(e)}")
        
        # Update table if fixes were applied
        if fixes_applied > 0:
            if hasattr(main_window, 'populate_entries_table'):
                main_window.populate_entries_table()
        
        # Show results
        result_msg = f"Validation complete!\n\n"
        result_msg += f"Fixes applied: {fixes_applied}\n"
        result_msg += f"Issues found: {len(issues_found)}\n\n"
        
        if issues_found:
            result_msg += "Issues:\n" + "\n".join(issues_found[:10])
            if len(issues_found) > 10:
                result_msg += f"\n... and {len(issues_found) - 10} more"
        
        QMessageBox.information(main_window, "Validation Results", result_msg)
        
    except Exception as e:
        QMessageBox.critical(main_window, "Validation Error", f"Failed to validate filenames:\n{str(e)}")

def debug_img_parsing(main_window):
    """Show detailed IMG parsing debug information"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            QMessageBox.information(main_window, "No IMG", "Please load an IMG file first.")
            return
        
        debug_info = []
        debug_info.append("IMG Parsing Debug Information")
        debug_info.append("=" * 50)
        debug_info.append("")
        
        # IMG file info
        img = main_window.current_img
        debug_info.append(f"File: {img.file_path}")
        debug_info.append(f"Version: {img.version}")
        debug_info.append(f"Entries: {len(img.entries)}")
        debug_info.append("")
        
        # Analyze first 10 entries in detail
        debug_info.append("Entry Analysis (first 10):")
        debug_info.append("-" * 30)
        
        for i, entry in enumerate(img.entries[:10]):
            debug_info.append(f"\nEntry {i}:")
            debug_info.append(f"  Name: '{getattr(entry, 'name', 'NO_NAME')}'")
            debug_info.append(f"  Extension: '{getattr(entry, 'extension', 'NO_EXT')}'")
            debug_info.append(f"  Size: {getattr(entry, 'size', 0):,} bytes")
            debug_info.append(f"  Offset: 0x{getattr(entry, 'offset', 0):08X}")
            
            # Check for corruption indicators
            name = getattr(entry, 'name', '')
            if any(char in name for char in ['\x00', '\xff', '\xcd']):
                debug_info.append("  Contains null/invalid characters")
            
            if len(name) > 20:
                debug_info.append("  Name longer than expected (>20 chars)")
            
            if not '.' in name and hasattr(entry, 'extension') and entry.extension:
                debug_info.append("  Extension detected without dot in name")
        
        # Extension statistics
        debug_info.append("\n\nExtension Statistics:")
        debug_info.append("-" * 20)
        
        ext_counts = {}
        for entry in img.entries:
            ext = getattr(entry, 'extension', 'NO_EXT')
            ext_counts[ext] = ext_counts.get(ext, 0) + 1
        
        for ext, count in sorted(ext_counts.items()):
            percentage = (count / len(img.entries)) * 100
            debug_info.append(f"  {ext}: {count} ({percentage:.1f}%)")
        
        # Show in dialog
        debug_text = "\n".join(debug_info)
        dialog = ParsingReportDialog(debug_text, main_window, "IMG Parsing Debug")
        dialog.exec()
        
    except Exception as e:
        QMessageBox.critical(main_window, "Debug Error", f"Failed to generate debug info:\n{str(e)}")

class ParsingReportDialog(QDialog):
    """Dialog for showing parsing reports"""
    
    def __init__(self, report_text: str, parent=None, title: str = "Parsing Report"):
        super().__init__(parent)
        self.setWindowTitle(title)
        self.setMinimumSize(600, 400)
        self.setup_ui(report_text)
    
    def setup_ui(self, report_text: str):
        """Setup the report dialog UI"""
        layout = QVBoxLayout(self)
        
        # Report text
        text_edit = QTextEdit()
        text_edit.setReadOnly(True)
        text_edit.setPlainText(report_text)
        text_edit.setFont(QFont("Consolas", 9))
        layout.addWidget(text_edit)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        copy_btn = QPushButton("📋 Copy to Clipboard")
        copy_btn.clicked.connect(lambda: self.copy_to_clipboard(report_text))
        button_layout.addWidget(copy_btn)
        
        button_layout.addStretch()
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)
    
    def copy_to_clipboard(self, text: str):
        """Copy report text to clipboard"""
        try:
            from PyQt6.QtWidgets import QApplication
            clipboard = QApplication.clipboard()
            clipboard.setText(text)
            
            # Show confirmation (brief)
            self.setWindowTitle(self.windowTitle() + " (Copied!)")
            QTimer.singleShot(2000, lambda: self.setWindowTitle(self.windowTitle().replace(" (Copied!)", "")))
            
        except Exception as e:
            QMessageBox.warning(self, "Copy Error", f"Failed to copy to clipboard:\n{str(e)}")

def detect_img_game_version(main_window) -> str:
    """Detect which GTA game the IMG file is from based on patterns"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            return "Unknown"
        
        img = main_window.current_img
        
        # Analyze filenames to detect game version
        filenames = [getattr(entry, 'name', '').lower() for entry in img.entries]
        filename_text = ' '.join(filenames)
        
        # GTA III indicators
        if any(name in filename_text for name in ['claude', 'catalina', 'luigi', 'salvatore']):
            return "GTA III"
        
        # GTA Vice City indicators  
        if any(name in filename_text for name in ['tommy', 'lance', 'diaz', 'vice', 'ocean']):
            return "GTA Vice City"
        
        # GTA San Andreas indicators
        if any(name in filename_text for name in ['cj', 'grove', 'ballas', 'aztecas', 'samp']):
            return "GTA San Andreas"
        
        # Check entry count patterns
        entry_count = len(img.entries)
        if entry_count < 1000:
            return "GTA III"
        elif entry_count < 5000:
            return "GTA Vice City" 
        else:
            return "GTA San Andreas"
            
    except Exception:
        return "Unknown"

# Main integration function
def setup_img_parsing_integration(main_window):
    """Setup complete IMG parsing integration"""
    try:
        # Apply parsing fixes
        if integrate_img_parsing_fixes(main_window):
            # Add game detection
            main_window.detect_img_game_version = lambda: detect_img_game_version(main_window)
            
            main_window.log_message("IMG parsing integration complete")
            return True
        
        return False
        
    except Exception as e:
        main_window.log_message(f"IMG parsing integration failed: {str(e)}")
        return False

# Export functions
__all__ = [
    'integrate_img_parsing_fixes',
    'setup_img_parsing_integration',
    'show_parsing_report_dialog',
    'validate_current_img_filenames',
    'debug_img_parsing',
    'ParsingReportDialog'
]
