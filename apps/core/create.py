#this belongs in apps/core/creator.py - Version: 17
# X-Seti - August27 2025 - IMG Factory 1.5 - IMG Creator Dialog UI Only
# Credit MexUK 2007 IMG Factory 1.2

"""
IMG Creator - Dialog UI Component Only
All creation logic moved to core/create_img.py
"""
import os
import shutil
from typing import Optional, Dict, Any, List, Tuple
from PyQt6.QtCore import QThread, pyqtSignal, Qt
from PyQt6.QtWidgets import (QDialog, QVBoxLayout, QHBoxLayout, QPushButton, 
                           QLabel, QComboBox, QSpinBox, QCheckBox, QLineEdit, 
                           QFileDialog, QProgressBar, QGroupBox, QGridLayout,
                           QMessageBox, QTextEdit, QApplication, QProgressDialog)

from PyQt6.QtGui import QFont
from pathlib import Path

# Import base creation functions from core
from apps.methods.img_core_classes import IMGVersion
from apps.core.img_formats import GameSpecificIMGDialog, IMGCreator

##Methods list -
# create_new_img_dialog
# get_default_output_path
# show_creation_dialog
#create_new_img
#detect_and_open_file
#open_file_dialog #old
#detect_file_type

##Classes -
# NewIMGDialog

# STUB: create new tab after IMG creation

class NewIMGDialog(QDialog): #vers 11
    """Simple New IMG dialog - filename + version only."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Create New IMG Archive")
        self.setModal(True)
        self.setFixedSize(420, 160)
        self.output_path = ""
        self._build_ui()
        from apps.core.theme_utils import apply_dialog_theme
        apply_dialog_theme(self)

    def _build_ui(self):
        layout = QVBoxLayout(self)

        # Output path row
        path_row = QHBoxLayout()
        path_row.addWidget(QLabel("Save as:"))
        self.path_edit = QLineEdit()
        self.path_edit.setPlaceholderText("Select output path...")
        self.path_edit.textChanged.connect(self._update_create_btn)
        path_row.addWidget(self.path_edit)
        browse_btn = QPushButton("Browse")
        browse_btn.clicked.connect(self._browse)
        path_row.addWidget(browse_btn)
        layout.addLayout(path_row)

        # Version selection
        ver_row = QHBoxLayout()
        ver_row.addWidget(QLabel("Version:"))
        self._ver_group = QButtonGroup(self)
        self.v1_radio = QRadioButton("Version 1  (GTA III / VC - DIR+IMG pair)")
        self.v2_radio = QRadioButton("Version 2  (GTA SA / IV)")
        self.v2_radio.setChecked(True)
        self._ver_group.addButton(self.v1_radio)
        self._ver_group.addButton(self.v2_radio)
        ver_col = QVBoxLayout()
        ver_col.addWidget(self.v1_radio)
        ver_col.addWidget(self.v2_radio)
        ver_row.addLayout(ver_col)
        layout.addLayout(ver_row)

        # Buttons
        btn_row = QHBoxLayout()
        btn_row.addStretch()
        cancel = QPushButton("Cancel")
        cancel.clicked.connect(self.reject)
        btn_row.addWidget(cancel)
        self.create_btn = QPushButton("Create")
        self.create_btn.setDefault(True)
        self.create_btn.setEnabled(False)
        self.create_btn.clicked.connect(self._create)
        btn_row.addWidget(self.create_btn)
        layout.addLayout(btn_row)

    def _update_create_btn(self):
        self.create_btn.setEnabled(bool(self.path_edit.text().strip()))

    def _browse(self):
        path, _ = QFileDialog.getSaveFileName(
            self, "Create New IMG Archive", "",
            "IMG Archives (*.img);;All Files (*)")
        if path:
            self.path_edit.setText(path)

    def _create(self):
        output_path = self.path_edit.text().strip()
        if not output_path:
            QMessageBox.warning(self, "No Path", "Please specify an output path.")
            return
        if os.path.exists(output_path):
            if QMessageBox.question(self, "Overwrite?",
                    f"File already exists:\n{output_path}\n\nOverwrite?",
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
                    ) != QMessageBox.StandardButton.Yes:
                return
        version = IMGVersion.VERSION_1 if self.v1_radio.isChecked() else IMGVersion.VERSION_2
        from apps.methods.img_core_classes import IMGFile
        img = IMGFile()
        if img.create_new(output_path, version):
            self.output_path = img.file_path  # V1 stores .dir path
            self.accept()
        else:
            QMessageBox.critical(self, "Failed", f"Could not create IMG file:\n{output_path}")
def create_new_img_dialog(parent=None) -> NewIMGDialog: #vers 1
    """Create new IMG dialog instance"""
    return NewIMGDialog(parent)


def get_default_output_path(parent=None) -> str: #vers 1
    """Get default output path from settings"""
    try:
        if parent and hasattr(parent, 'settings'):
            assists_folder = parent.settings.get('assists_folder', '/home/x2')
            return os.path.join(assists_folder, 'new_archive.img')
    except Exception:
        pass
    return '/home/x2/new_archive.img'


def show_creation_dialog(parent=None) -> Optional[str]: #vers 1
    """Show creation dialog and return created file path"""
    try:
        dialog = NewIMGDialog(parent)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            return dialog.output_path
        return None
    except Exception:
        return None

# In open.py and create.py, use tab_system directly:
def _load_img_file(main_window, file_path):
    from apps.methods.tab_system import create_tab
    tab_index = create_tab(main_window, file_path, 'IMG', None)

def create_new_img(self): #vers 6 Fixed
    """Show simple new IMG dialog - filename + version only."""
    try:
        dialog = NewIMGDialog(self)
        if dialog.exec() == QDialog.DialogCode.Accepted and dialog.output_path:
            self.log_message(f"Created: {os.path.basename(dialog.output_path)}")
            self._load_img_file_in_new_tab(dialog.output_path)
    except Exception as e:
        self.log_message(f"Error creating new IMG: {str(e)}")



# Export list for external imports
__all__ = [
    'NewIMGDialog',
    'create_new_img_dialog',
    'create_new_img',
    'get_default_output_path', 
    'show_creation_dialog'
]
