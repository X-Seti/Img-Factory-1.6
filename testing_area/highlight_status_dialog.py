#!/usr/bin/env python3
#this belongs in components/File_Browser/highlight_status_dialog.py - Version: 1
# X-Seti - December4 2025 - IMG Factory 1.5 - Highlight Status Dialog

"""
Highlight Status Dialog - Shows status of highlighted files in IMG file
Replaces middle tab browse folder functionality with status display
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton, 
    QFrame, QGroupBox, QTableWidget, QTableWidgetItem, QHeaderView
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont, QIcon
import os


class HighlightStatusDialog(QDialog):
    """Dialog showing status of highlighted files in IMG file"""
    
    def __init__(self, parent=None, highlighted_files=None):
        super().__init__(parent)
        self.highlighted_files = highlighted_files or []
        self.setWindowTitle("IMG Highlight Status")
        self.setMinimumWidth(500)
        self.setMinimumHeight(400)
        self._setup_ui()
    
    def _setup_ui(self):
        """Setup dialog UI with highlight status"""
        layout = QVBoxLayout(self)
        
        # Title
        title_label = QLabel("IMG File Highlight Status")
        title_font = QFont()
        title_font.setPointSize(12)
        title_font.setBold(True)
        title_label.setFont(title_font)
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(title_label)
        
        # Status info
        status_frame = QFrame()
        status_frame.setFrameStyle(QFrame.Shape.StyledPanel)
        status_layout = QVBoxLayout(status_frame)
        
        # Total highlighted count
        count_label = QLabel(f"Total Highlighted Files: {len(self.highlighted_files)}")
        count_font = QFont()
        count_font.setBold(True)
        count_label.setFont(count_font)
        status_layout.addWidget(count_label)
        
        # Additional info
        if self.highlighted_files:
            info_label = QLabel("Files currently highlighted in the IMG table:")
            status_layout.addWidget(info_label)
        else:
            info_label = QLabel("No files are currently highlighted.")
            status_layout.addWidget(info_label)
        
        layout.addWidget(status_frame)
        
        # Table showing highlighted files (if any)
        if self.highlighted_files:
            table_label = QLabel("Highlighted Files:")
            layout.addWidget(table_label)
            
            self.file_table = QTableWidget()
            self.file_table.setColumnCount(2)
            self.file_table.setHorizontalHeaderLabels(["File Name", "Status"])
            self.file_table.horizontalHeader().setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
            self.file_table.horizontalHeader().setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
            
            self.file_table.setRowCount(len(self.highlighted_files))
            for i, (filename, status) in enumerate(self.highlighted_files):
                self.file_table.setItem(i, 0, QTableWidgetItem(filename))
                self.file_table.setItem(i, 1, QTableWidgetItem(status))
            
            layout.addWidget(self.file_table)
        
        # Buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)


def show_highlight_status_dialog(parent=None, highlighted_files=None):
    """Show highlight status dialog"""
    dialog = HighlightStatusDialog(parent, highlighted_files)
    return dialog.exec()


if __name__ == "__main__":
    from PyQt6.QtWidgets import QApplication
    import sys
    
    app = QApplication(sys.argv)
    
    # Test with some sample highlighted files
    sample_files = [
        ("texture1.txd", "Imported"),
        ("model1.dff", "Replaced"),
        ("vehicle.col", "Imported")
    ]
    
    dialog = HighlightStatusDialog(None, sample_files)
    dialog.exec()