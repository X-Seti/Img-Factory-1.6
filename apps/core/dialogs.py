#this belongs in core/dialogs.py - Version: 14
# X-Seti - Aug15 2025 - Img Factory 1.5 - Complete Dialog Classes with Assists Integration

"""
Complete dialog classes and threading for IMG Factory
Includes all existing dialogs with Assists folder integration added to ExportOptionsDialog
"""

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QLineEdit, 
    QPushButton, QCheckBox, QComboBox, QProgressBar, QMessageBox, 
    QFileDialog, QGroupBox, QGridLayout, QTextEdit, QDialogButtonBox,
    QProgressDialog
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal
from PyQt6.QtGui import QFont

##Methods list -
# ExportOptionsDialog._browse_folder
# ExportOptionsDialog._create_ui
# ExportOptionsDialog._use_assists_folder
# ExportOptionsDialog.get_options
# ExportThread.export_entry
# ExportThread.run
# ImportOptionsDialog._create_ui
# ImportOptionsDialog.get_options
# ImportThread.import_file
# ImportThread.run
# IMGPropertiesDialog._create_ui
# IMGPropertiesDialog._format_size
# IMGPropertiesDialog._get_file_size
# IMGPropertiesDialog._get_file_type_stats
# IMGPropertiesDialog._get_modification_time
# ValidationResultsDialog._create_ui
# get_export_directory
# get_img_file_filter
# get_import_files
# get_open_img_filename
# get_save_img_filename
# show_about_dialog
# show_error_dialog
# show_export_options_dialog
# show_img_properties_dialog
# show_import_options_dialog
# show_info_dialog
# show_progress_dialog
# show_question_dialog
# show_validation_results_dialog
# show_warning_dialog

##Classes -
# ExportOptionsDialog
# ExportThread
# ImportOptionsDialog
# ImportThread
# IMGPropertiesDialog
# ValidationResultsDialog

class IMGPropertiesDialog(QDialog): #vers 1
    """Dialog showing IMG file properties"""
    
    def __init__(self, img_file, parent=None):
        super().__init__(parent)
        self.img_file = img_file
        self.setWindowTitle("IMG Properties")
        self.setMinimumWidth(400)
        self._create_ui()
    
    def _create_ui(self): #vers 11
        """Create properties UI"""
        layout = QVBoxLayout(self)
        
        # File properties
        file_group = QGroupBox("File Information")
        file_layout = QVBoxLayout(file_group)
        
        properties = [
            ("File Path", getattr(self.img_file, 'file_path', 'Unknown')),
            ("File Size", self._get_file_size()),
            ("Entry Count", str(len(getattr(self.img_file, 'entries', [])))),
            ("IMG Version", getattr(self.img_file, 'version', 'Unknown')),
            ("Last Modified", self._get_modification_time())
        ]
        
        for label, value in properties:
            row = QHBoxLayout()
            row.addWidget(QLabel(f"{label}:"))
            value_label = QLabel(str(value))
            value_label.setStyleSheet("font-weight: bold;")
            row.addWidget(value_label)
            row.addStretch()
            file_layout.addLayout(row)
        
        layout.addWidget(file_group)
        
        # File type statistics
        stats_group = QGroupBox("File Type Statistics")
        stats_layout = QVBoxLayout(stats_group)
        
        file_stats = self._get_file_type_stats()
        if file_stats:
            for ext, count in sorted(file_stats.items()):
                stats_layout.addWidget(QLabel(f"{ext}: {count} files"))
        else:
            stats_layout.addWidget(QLabel("No file statistics available"))
        
        layout.addWidget(stats_group)
        
        # Close button
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        button_layout.addWidget(close_btn)
        layout.addLayout(button_layout)
    
    def _get_file_size(self): #vers 1
        """Get file size"""
        try:
            if hasattr(self.img_file, 'file_path') and self.img_file.file_path:
                size = os.path.getsize(self.img_file.file_path)
                return self._format_size(size)
        except:
            pass
        return "Unknown"
    
    def _format_size(self, size): #vers 1
        """Format size in human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size < 1024.0:
                return f"{size:.1f} {unit}"
            size /= 1024.0
        return f"{size:.1f} TB"
    
    def _get_modification_time(self): #vers 1
        """Get file modification time"""
        try:
            if hasattr(self.img_file, 'file_path'):
                import datetime
                mtime = os.path.getmtime(self.img_file.file_path)
                return datetime.datetime.fromtimestamp(mtime).strftime("%Y-%m-%d %H:%M:%S")
        except:
            pass
        return "Unknown"
    
    def _get_file_type_stats(self): #vers 8
        """Get file type statistics"""
        stats = {}
        for entry in self.img_file.entries:
            ext = getattr(entry, 'extension', 'Unknown')
            stats[ext] = stats.get(ext, 0) + 1
        return stats

class ExportOptionsDialog(QDialog): #vers 1
    """Export options dialog with Assists folder integration"""
    
    def __init__(self, parent=None, entry_count=0):
        super().__init__(parent)
        self.parent_window = parent
        self.entry_count = entry_count
        self.files_list = []
        self.operation_type = "export"
        self.file_count = entry_count
        self.setWindowTitle("Export Options")
        self.setMinimumWidth(450)
        self._create_ui()
        self._populate_assists_folder()
    
    def _create_ui(self): #vers 11
        """Create export options UI with Assists folder integration"""
        layout = QVBoxLayout(self)
        
        # Export destination
        dest_group = QGroupBox("Export Destination")
        dest_layout = QVBoxLayout(dest_group)
        
        # Assists folder quick access
        assists_layout = QHBoxLayout()
        assists_btn = QPushButton("Use Assists Folder")
        assists_btn.setStyleSheet("QPushButton { padding: 6px; font-weight: bold; }")
        assists_btn.clicked.connect(self._use_assists_folder)
        assists_layout.addWidget(assists_btn)
        
        self.assists_info_label = QLabel("Assists folder: Not configured")
        self.assists_info_label.setStyleSheet("font-size: 9pt;")
        assists_layout.addWidget(self.assists_info_label)
        dest_layout.addLayout(assists_layout)
        
        # Folder selection
        folder_layout = QHBoxLayout()
        self.folder_input = QLineEdit()
        self.folder_input.setPlaceholderText("Select export folder...")
        folder_layout.addWidget(self.folder_input)
        
        browse_btn = QPushButton("Browse...")
        browse_btn.clicked.connect(self._browse_folder)
        folder_layout.addWidget(browse_btn)
        dest_layout.addLayout(folder_layout)
        
        layout.addWidget(dest_group)
        
        # Export options
        options_group = QGroupBox("Export Options")
        options_layout = QVBoxLayout(options_group)
        
        self.organize_check = QCheckBox("Organize by file type")
        self.organize_check.setChecked(True)
        options_layout.addWidget(self.organize_check)
        
        self.overwrite_check = QCheckBox("Overwrite existing files")
        self.overwrite_check.setChecked(True)
        options_layout.addWidget(self.overwrite_check)
        
        self.create_log_check = QCheckBox("Create export log")
        options_layout.addWidget(self.create_log_check)
        
        layout.addWidget(options_group)
        
        # Info
        info_label = QLabel(f"Ready to export {self.entry_count} entries")
        info_label.setStyleSheet("font-style: italic;")
        layout.addWidget(info_label)
        
        # Buttons
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)
    
    def _populate_assists_folder(self): #vers 1
        """Auto-populate folder input with assists folder if available"""
        try:
            if self.parent_window and hasattr(self.parent_window, 'settings'):
                assists_folder = getattr(self.parent_window.settings, 'assists_folder', None)
                if assists_folder and os.path.exists(assists_folder):
                    self.folder_input.setText(assists_folder)
                    self.assists_info_label.setText(f"{assists_folder}")
                    self.assists_info_label.setStyleSheet("font-size: 9pt;")
                else:
                    self.assists_info_label.setText("Assists folder not configured")
                    self.assists_info_label.setStyleSheet("font-size: 9pt;")
        except Exception:
            self.assists_info_label.setText("Error checking assists folder")
            self.assists_info_label.setStyleSheet("font-size: 9pt;")
    
    def _use_assists_folder(self): #vers 1
        """Set export destination to assists folder"""
        try:
            if self.parent_window and hasattr(self.parent_window, 'settings'):
                assists_folder = getattr(self.parent_window.settings, 'assists_folder', None)
                if assists_folder and os.path.exists(assists_folder):
                    self.folder_input.setText(assists_folder)
                    QMessageBox.information(
                        self, 
                        "Assists Folder Selected", 
                        f"Export destination set to:\n{assists_folder}"
                    )
                else:
                    QMessageBox.warning(
                        self, 
                        "Assists Folder Not Found", 
                        "Assists folder is not configured or doesn't exist."
                    )
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Failed to set assists folder:\n{str(e)}")
    
    def _browse_folder(self): #vers 4
        """Browse for export folder"""
        start_dir = ""
        try:
            if self.parent_window and hasattr(self.parent_window, 'settings'):
                assists_folder = getattr(self.parent_window.settings, 'assists_folder', None)
                if assists_folder and os.path.exists(assists_folder):
                    start_dir = assists_folder
        except Exception:
            pass
        
        if not start_dir:
            start_dir = os.path.expanduser("~/Desktop")
        
        folder = QFileDialog.getExistingDirectory(self, "Select Export Folder", start_dir)
        if folder:
            self.folder_input.setText(folder)
    
    def get_options(self): #vers 5
        """Get export options"""
        return {
            'export_folder': self.folder_input.text(),
            'organize_by_type': self.organize_check.isChecked(),
            'overwrite_existing': self.overwrite_check.isChecked(),
            'create_log': self.create_log_check.isChecked(),
            'use_assists_structure': self.folder_input.text() == getattr(getattr(self.parent_window, 'settings', None), 'assists_folder', '')
        }

class ImportOptionsDialog(QDialog): #vers 1
    """Import options dialog"""
    
    def __init__(self, parent=None, file_count=0):
        super().__init__(parent)
        self.files_list = []
        self.operation_type = "import"
        self.file_count = file_count
        self.setWindowTitle("Import Options")
        self.setMinimumWidth(400)
        self._create_ui()
    
    def _create_ui(self): #vers 10
        """Create import options UI"""
        layout = QVBoxLayout(self)
        
        # Import options
        options_group = QGroupBox("Import Options")
        options_layout = QVBoxLayout(options_group)
        
        self.replace_check = QCheckBox("Replace existing entries")
        self.replace_check.setChecked(True)
        options_layout.addWidget(self.replace_check)
        
        self.validate_check = QCheckBox("Validate files before import")
        self.validate_check.setChecked(True)
        options_layout.addWidget(self.validate_check)
        
        self.backup_check = QCheckBox("Create backup before import")
        options_layout.addWidget(self.backup_check)
        
        self.create_log_check = QCheckBox("Create import log")
        options_layout.addWidget(self.create_log_check)
        
        layout.addWidget(options_group)
        
        # Info
        operation_text = {
            "import": "import",
            "folder": "import from folder",
            "files": "import"
        }.get(self.operation_type, "import")

        info_label = QLabel(f"Ready to {operation_text} {self.file_count} files")
        info_label.setStyleSheet("font-style: italic;")
        layout.addWidget(info_label)
        
        # Buttons
        buttons = QDialogButtonBox(
            QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel
        )
        buttons.accepted.connect(self.accept)
        buttons.rejected.connect(self.reject)
        layout.addWidget(buttons)
    
    def get_options(self): #vers 4
        """Get import options"""
        return {
            'replace_existing': self.replace_check.isChecked(),
            'validate_files': self.validate_check.isChecked(),
            'create_backup': self.backup_check.isChecked(),
            'create_log': self.create_log_check.isChecked()
        }

class ValidationResultsDialog(QDialog): #vers 1
    """Dialog showing IMG validation results"""
    
    def __init__(self, validation_result, parent=None):
        super().__init__(parent)
        self.validation_result = validation_result
        self.setWindowTitle("IMG Validation Results")
        self.setMinimumWidth(500)
        self.setMinimumHeight(400)
        self._create_ui()
    
    def _create_ui(self): #vers 10
        """Create validation results UI"""
        layout = QVBoxLayout(self)
        
        # Results text
        results_text = QTextEdit()
        results_text.setReadOnly(True)
        results_text.setFont(QFont("Courier", 9))
        
        # Format results
        result_text = "IMG Validation Results\n" + "="*50 + "\n\n"
        
        if self.validation_result.get('is_valid', False):
            result_text += "Validation PASSED\n\n"
        else:
            result_text += "Validation FAILED\n\n"
        
        # Show errors
        errors = self.validation_result.get('errors', [])
        if errors:
            result_text += f"Errors ({len(errors)}):\n"
            for error in errors:
                result_text += f"  • {error}\n"
            result_text += "\n"
        
        # Show warnings
        warnings = self.validation_result.get('warnings', [])
        if warnings:
            result_text += f"Warnings ({len(warnings)}):\n"
            for warning in warnings:
                result_text += f"  • {warning}\n"
            result_text += "\n"
        
        # Show statistics
        stats = self.validation_result.get('statistics', {})
        if stats:
            result_text += "Statistics:\n"
            for key, value in stats.items():
                result_text += f"  {key}: {value}\n"
        
        results_text.setText(result_text)
        layout.addWidget(results_text)
        
        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)

class ImportThread(QThread): #vers 1
    """Background import thread"""
    progress = pyqtSignal(int)
    finished = pyqtSignal(bool, str)
    
    def __init__(self, main_window, files_to_import, replace_existing=True, validate_files=True, create_backup=False, create_log=False):
        super().__init__()
        self.main_window = main_window
        self.files_to_import = files_to_import
        self.replace_existing = replace_existing
        self.validate_files = validate_files
        self.create_backup = create_backup
        self.create_log = create_log
    
    def run(self): #vers 1
        """Run import operation"""
        try:
            total_files = len(self.files_to_import)
            
            for i, file_path in enumerate(self.files_to_import):
                progress = int((i / total_files) * 100)
                self.progress.emit(progress)
                
                success = self.import_file(file_path)
                if not success and not self.replace_existing:
                    break
            
            self.finished.emit(True, f"Import completed: {total_files} files processed")
            
        except Exception as e:
            self.finished.emit(False, f"Import error: {str(e)}")
    
    def import_file(self, file_path): #vers 1
        """Import single file"""
        try:
            # Basic import logic - would be implemented based on IMG system
            return True
        except Exception:
            return False

class ExportThread(QThread): #vers 1
    """Background export thread"""
    progress = pyqtSignal(int)
    finished = pyqtSignal(bool, str)
    
    def __init__(self, main_window, entries_to_export, export_dir, export_options):
        super().__init__()
        self.main_window = main_window
        self.entries_to_export = entries_to_export
        self.export_dir = export_dir
        self.export_options = export_options
    
    def run(self): #vers 1
        """Run export operation"""
        try:
            total_entries = len(self.entries_to_export)
            
            for i, entry in enumerate(self.entries_to_export):
                progress = int((i / total_entries) * 100)
                self.progress.emit(progress)
                
                success = self.export_entry(entry)
                if not success:
                    break
            
            self.finished.emit(True, f"Export completed: {total_entries} entries processed")
            
        except Exception as e:
            self.finished.emit(False, f"Export error: {str(e)}")
    
    def export_entry(self, entry): #vers 1
        """Export single entry"""
        try:
            # Basic export logic - would be implemented based on IMG system
            return True
        except Exception:
            return False

# Dialog utility functions
def show_img_properties_dialog(img_file, parent=None): #vers 1
    """Show IMG properties dialog"""
    dialog = IMGPropertiesDialog(img_file, parent)
    dialog.exec()

def show_export_options_dialog(parent=None, entry_count=0): #vers 1
    """Show export options dialog"""
    dialog = ExportOptionsDialog(parent, entry_count)
    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.get_options()
    return None

def show_import_options_dialog(parent=None, file_count=0): #vers 2
    """Show import options dialog"""
    dialog = ImportOptionsDialog(parent, file_count)
    if dialog.exec() == QDialog.DialogCode.Accepted:
        return dialog.get_options()
    return None

def show_validation_results_dialog(validation_result, parent=None): #vers 2
    """Show validation results dialog"""
    dialog = ValidationResultsDialog(validation_result, parent)
    dialog.exec()

def show_error_dialog(parent, title, message, details=None): #vers 2
    """Show error dialog with optional details"""
    msg_box = QMessageBox(parent)
    msg_box.setIcon(QMessageBox.Icon.Critical)
    msg_box.setWindowTitle(title)
    msg_box.setText(message)
    
    if details:
        msg_box.setDetailedText(details)
    
    msg_box.setStandardButtons(QMessageBox.StandardButton.Ok)
    msg_box.exec()

def show_warning_dialog(parent, title, message): #vers 2
    """Show warning dialog"""
    msg_box = QMessageBox(parent)
    msg_box.setIcon(QMessageBox.Icon.Warning)
    msg_box.setWindowTitle(title)
    msg_box.setText(message)
    msg_box.setStandardButtons(QMessageBox.StandardButton.Ok)
    return msg_box.exec()

def show_question_dialog(parent, title, message): #vers 2
    """Show question dialog"""
    msg_box = QMessageBox(parent)
    msg_box.setIcon(QMessageBox.Icon.Question)
    msg_box.setWindowTitle(title)
    msg_box.setText(message)
    msg_box.setStandardButtons(QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
    return msg_box.exec()

def show_info_dialog(parent, title, message): #vers 2
    """Show info dialog"""
    msg_box = QMessageBox(parent)
    msg_box.setIcon(QMessageBox.Icon.Information)
    msg_box.setWindowTitle(title)
    msg_box.setText(message)
    msg_box.setStandardButtons(QMessageBox.StandardButton.Ok)
    msg_box.exec()

def show_progress_dialog(parent, title, text): #vers 2
    """Show progress dialog"""
    progress = QProgressDialog(text, "Cancel", 0, 100, parent)
    progress.setWindowTitle(title)
    progress.setWindowModality(Qt.WindowModality.WindowModal)
    return progress

def show_about_dialog(parent): #vers 2
    """Show about dialog"""
    QMessageBox.about(parent, "About IMG Factory 1.5", "IMG Factory 1.5\n\nA comprehensive IMG file manager for GTA games.")

# File dialog functions
def get_img_file_filter(): #vers 2
    """Get file filter for IMG files"""
    return "IMG Archives (*.img);;All Files (*)"

def get_export_directory(parent=None, title="Select Export Directory"): #vers 1
    """Get export directory from user"""
    return QFileDialog.getExistingDirectory(parent, title)

def get_import_files(parent=None, title="Select Files to Import"): #vers 3
    """Get files to import from user"""
    files, _ = QFileDialog.getOpenFileNames(
        parent,
        title,
        "",
        "All Files (*);;Models (*.dff);;Textures (*.txd);;Collision (*.col);;Animation (*.ifp);;Audio (*.wav);;Scripts (*.scm)"
    )
    return files

def get_save_img_filename(parent=None, title="Save IMG Archive"): #vers 2
    """Get filename for saving IMG archive"""
    filename, _ = QFileDialog.getSaveFileName(
        parent,
        title,
        "",
        get_img_file_filter()
    )
    return filename

def get_open_img_filename(parent=None, title="Open IMG Archive"): #vers 2
    """Get filename for opening IMG archive"""
    filename, _ = QFileDialog.getOpenFileName(
        parent,
        title,
        "",
        get_img_file_filter()
    )
    return filename

# Export all functions
__all__ = [
    'IMGPropertiesDialog',
    'ExportOptionsDialog', 
    'ImportOptionsDialog',
    'ValidationResultsDialog',
    'ImportThread',
    'ExportThread',
    'show_img_properties_dialog',
    'show_export_options_dialog',
    'show_import_options_dialog',
    'show_validation_results_dialog',
    'show_error_dialog',
    'show_warning_dialog',
    'show_question_dialog',
    'show_info_dialog',
    'show_progress_dialog',
    'show_about_dialog',
    'get_img_file_filter',
    'get_export_directory',
    'get_import_files',
    'get_save_img_filename',
    'get_open_img_filename'
]
