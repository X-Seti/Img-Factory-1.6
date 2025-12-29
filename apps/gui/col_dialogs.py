#this belongs in gui/col_dialogs.py - Version: 1
# X-Seti - August13 2025 - IMG Factory 1.5 - COL GUI Dialogs

"""
COL GUI Dialogs - Additional dialogs for COL operations
Analysis dialogs, info displays, and other COL-specific GUI components
"""

import os
from typing import Dict, Any, List
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTextEdit, QPushButton,
    QLabel, QGroupBox, QFormLayout, QScrollArea, QTabWidget,
    QTableWidget, QTableWidgetItem, QMessageBox, QProgressDialog
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal
from PyQt6.QtGui import QFont, QPixmap, QIcon

from apps.methods.img_core_classes import format_file_size
from apps.debug.debug_functions import img_debugger

##Methods list -
# show_col_analysis_dialog
# show_col_info_dialog
# show_col_validation_results

##Classes -
# COLAnalysisDialog
# COLInfoDialog
# COLValidationDialog

class COLAnalysisDialog(QDialog): #vers 1
    """Advanced COL analysis dialog with detailed information"""
    
    def __init__(self, parent=None, analysis_data: Dict[str, Any] = None, file_name: str = ""):
        super().__init__(parent)
        self.analysis_data = analysis_data or {}
        self.file_name = file_name
        
        self.setWindowTitle(f"COL Analysis - {file_name}")
        self.setModal(True)
        self.resize(600, 500)
        
        self.setup_ui()
        self.populate_data()
    
    def setup_ui(self):
        """Setup analysis dialog UI"""
        layout = QVBoxLayout(self)
        
        # Title
        title_label = QLabel(f"COL Analysis Report: {self.file_name}")
        title_font = QFont()
        title_font.setBold(True)
        title_font.setPointSize(12)
        title_label.setFont(title_font)
        layout.addWidget(title_label)
        
        # Tab widget for different analysis views
        self.tab_widget = QTabWidget()
        layout.addWidget(self.tab_widget)
        
        # Overview tab
        self.overview_tab = QWidget()
        self.setup_overview_tab()
        self.tab_widget.addTab(self.overview_tab, "📊 Overview")
        
        # Models tab
        self.models_tab = QWidget()
        self.setup_models_tab()
        self.tab_widget.addTab(self.models_tab, "🎯 Models")
        
        # Validation tab
        self.validation_tab = QWidget()
        self.setup_validation_tab()
        self.tab_widget.addTab(self.validation_tab, "✅ Validation")
        
        # Buttons
        button_layout = QHBoxLayout()
        
        self.copy_button = QPushButton("📋 Copy Report")
        self.copy_button.clicked.connect(self.copy_report)
        button_layout.addWidget(self.copy_button)
        
        button_layout.addStretch()
        
        self.close_button = QPushButton("Close")
        self.close_button.clicked.connect(self.accept)
        button_layout.addWidget(self.close_button)
        
        layout.addLayout(button_layout)
    
    def setup_overview_tab(self):
        """Setup overview tab"""
        layout = QVBoxLayout(self.overview_tab)
        
        # Basic info group
        info_group = QGroupBox("File Information")
        info_layout = QFormLayout(info_group)
        
        self.file_name_label = QLabel("-")
        info_layout.addRow("File Name:", self.file_name_label)
        
        self.file_size_label = QLabel("-")
        info_layout.addRow("File Size:", self.file_size_label)
        
        self.version_label = QLabel("-")
        info_layout.addRow("COL Version:", self.version_label)
        
        self.model_count_label = QLabel("-")
        info_layout.addRow("Model Count:", self.model_count_label)
        
        layout.addWidget(info_group)
        
        # Statistics group
        stats_group = QGroupBox("Collision Statistics")
        stats_layout = QFormLayout(stats_group)
        
        self.total_spheres_label = QLabel("-")
        stats_layout.addRow("Total Spheres:", self.total_spheres_label)
        
        self.total_boxes_label = QLabel("-")
        stats_layout.addRow("Total Boxes:", self.total_boxes_label)
        
        self.total_faces_label = QLabel("-")
        stats_layout.addRow("Total Faces:", self.total_faces_label)
        
        self.total_vertices_label = QLabel("-")
        stats_layout.addRow("Total Vertices:", self.total_vertices_label)
        
        layout.addWidget(stats_group)
        
        layout.addStretch()
    
    def setup_models_tab(self):
        """Setup models tab"""
        layout = QVBoxLayout(self.models_tab)
        
        # Models table
        self.models_table = QTableWidget()
        self.models_table.setColumnCount(6)
        self.models_table.setHorizontalHeaderLabels([
            "Model Name", "Version", "Spheres", "Boxes", "Faces", "Vertices"
        ])
        
        # Set column widths
        self.models_table.setColumnWidth(0, 150)  # Name
        self.models_table.setColumnWidth(1, 80)   # Version
        self.models_table.setColumnWidth(2, 80)   # Spheres
        self.models_table.setColumnWidth(3, 80)   # Boxes
        self.models_table.setColumnWidth(4, 80)   # Faces
        self.models_table.setColumnWidth(5, 80)   # Vertices
        
        layout.addWidget(self.models_table)
    
    def setup_validation_tab(self):
        """Setup validation tab"""
        layout = QVBoxLayout(self.validation_tab)
        
        # Validation results
        self.validation_text = QTextEdit()
        self.validation_text.setReadOnly(True)
        self.validation_text.setFont(QFont("Consolas", 9))
        layout.addWidget(self.validation_text)
    
    def populate_data(self):
        """Populate dialog with analysis data"""
        try:
            # Overview tab
            self.file_name_label.setText(self.file_name)
            
            if 'size' in self.analysis_data:
                self.file_size_label.setText(format_file_size(self.analysis_data['size']))
            
            if 'version' in self.analysis_data:
                self.version_label.setText(str(self.analysis_data['version']))
            
            if 'model_count' in self.analysis_data:
                self.model_count_label.setText(str(self.analysis_data['model_count']))
            
            if 'total_spheres' in self.analysis_data:
                self.total_spheres_label.setText(str(self.analysis_data['total_spheres']))
            
            if 'total_boxes' in self.analysis_data:
                self.total_boxes_label.setText(str(self.analysis_data['total_boxes']))
            
            if 'total_faces' in self.analysis_data:
                self.total_faces_label.setText(str(self.analysis_data['total_faces']))
            
            if 'total_vertices' in self.analysis_data:
                self.total_vertices_label.setText(str(self.analysis_data['total_vertices']))
            
            # Models tab
            if 'models' in self.analysis_data:
                models = self.analysis_data['models']
                self.models_table.setRowCount(len(models))
                
                for i, model in enumerate(models):
                    self.models_table.setItem(i, 0, QTableWidgetItem(str(model.get('name', 'Unknown'))))
                    self.models_table.setItem(i, 1, QTableWidgetItem(str(model.get('version', 'Unknown'))))
                    self.models_table.setItem(i, 2, QTableWidgetItem(str(model.get('spheres', 0))))
                    self.models_table.setItem(i, 3, QTableWidgetItem(str(model.get('boxes', 0))))
                    self.models_table.setItem(i, 4, QTableWidgetItem(str(model.get('faces', 0))))
                    self.models_table.setItem(i, 5, QTableWidgetItem(str(model.get('vertices', 0))))
            
            # Validation tab
            validation_text = "COL Validation Results\n"
            validation_text += "=" * 30 + "\n\n"
            
            if 'valid' in self.analysis_data:
                validation_text += f"Valid: {'✅ Yes' if self.analysis_data['valid'] else '❌ No'}\n\n"
            
            if 'errors' in self.analysis_data and self.analysis_data['errors']:
                validation_text += "Errors:\n"
                for error in self.analysis_data['errors']:
                    validation_text += f"  ❌ {error}\n"
                validation_text += "\n"
            
            if 'warnings' in self.analysis_data and self.analysis_data['warnings']:
                validation_text += "Warnings:\n"
                for warning in self.analysis_data['warnings']:
                    validation_text += f"  ⚠️ {warning}\n"
                validation_text += "\n"
            
            if 'info' in self.analysis_data and self.analysis_data['info']:
                validation_text += "Information:\n"
                for info in self.analysis_data['info']:
                    validation_text += f"  ℹ️ {info}\n"
                validation_text += "\n"
            
            self.validation_text.setPlainText(validation_text)
            
        except Exception as e:
            img_debugger.error(f"Error populating analysis dialog: {str(e)}")
    
    def copy_report(self):
        """Copy analysis report to clipboard"""
        try:
            from PyQt6.QtWidgets import QApplication
            
            report = self.generate_text_report()
            clipboard = QApplication.clipboard()
            clipboard.setText(report)
            
            QMessageBox.information(self, "Copied", "Analysis report copied to clipboard!")
            
        except Exception as e:
            QMessageBox.warning(self, "Error", f"Failed to copy report: {str(e)}")
    
    def generate_text_report(self) -> str:
        """Generate text report of analysis"""
        report = f"COL Analysis Report: {self.file_name}\n"
        report += "=" * 50 + "\n\n"
        
        # Overview
        report += "OVERVIEW:\n"
        report += f"File Name: {self.file_name}\n"
        if 'size' in self.analysis_data:
            report += f"File Size: {format_file_size(self.analysis_data['size'])}\n"
        if 'version' in self.analysis_data:
            report += f"COL Version: {self.analysis_data['version']}\n"
        if 'model_count' in self.analysis_data:
            report += f"Model Count: {self.analysis_data['model_count']}\n"
        report += "\n"
        
        # Statistics
        report += "STATISTICS:\n"
        if 'total_spheres' in self.analysis_data:
            report += f"Total Spheres: {self.analysis_data['total_spheres']}\n"
        if 'total_boxes' in self.analysis_data:
            report += f"Total Boxes: {self.analysis_data['total_boxes']}\n"
        if 'total_faces' in self.analysis_data:
            report += f"Total Faces: {self.analysis_data['total_faces']}\n"
        if 'total_vertices' in self.analysis_data:
            report += f"Total Vertices: {self.analysis_data['total_vertices']}\n"
        report += "\n"
        
        # Models
        if 'models' in self.analysis_data and self.analysis_data['models']:
            report += "MODELS:\n"
            for i, model in enumerate(self.analysis_data['models']):
                report += f"  {i+1}. {model.get('name', 'Unknown')} "
                report += f"(S:{model.get('spheres', 0)} B:{model.get('boxes', 0)} "
                report += f"F:{model.get('faces', 0)} V:{model.get('vertices', 0)})\n"
            report += "\n"
        
        return report

class COLInfoDialog(QDialog): #vers 1
    """Simple COL info dialog for quick viewing"""
    
    def __init__(self, parent=None, info_text: str = "", title: str = "COL Information"):
        super().__init__(parent)
        self.setWindowTitle(title)
        self.setModal(True)
        self.resize(400, 300)
        
        layout = QVBoxLayout(self)
        
        # Info text
        self.info_text = QTextEdit()
        self.info_text.setReadOnly(True)
        self.info_text.setPlainText(info_text)
        self.info_text.setFont(QFont("Consolas", 9))
        layout.addWidget(self.info_text)
        
        # Close button
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        close_button = QPushButton("Close")
        close_button.clicked.connect(self.accept)
        button_layout.addWidget(close_button)
        
        layout.addLayout(button_layout)

# Convenience functions
def show_col_analysis_dialog(parent, analysis_data: Dict[str, Any], file_name: str = "") -> bool: #vers 1
    """Show COL analysis dialog"""
    try:
        dialog = COLAnalysisDialog(parent, analysis_data, file_name)
        return dialog.exec() == QDialog.DialogCode.Accepted
    except Exception as e:
        img_debugger.error(f"Error showing COL analysis dialog: {str(e)}")
        return False

def show_col_info_dialog(parent, info_text: str, title: str = "COL Information") -> bool: #vers 1
    """Show simple COL info dialog"""
    try:
        dialog = COLInfoDialog(parent, info_text, title)
        return dialog.exec() == QDialog.DialogCode.Accepted
    except Exception as e:
        img_debugger.error(f"Error showing COL info dialog: {str(e)}")
        return False

def show_col_validation_results(parent, validation_data: Dict[str, Any], file_name: str = "") -> bool: #vers 1
    """Show COL validation results in analysis dialog"""
    try:
        # Convert validation data to analysis format
        analysis_data = {
            'valid': validation_data.get('valid', False),
            'errors': validation_data.get('errors', []),
            'warnings': validation_data.get('warnings', []),
            'info': validation_data.get('info', [])
        }
        
        return show_col_analysis_dialog(parent, analysis_data, file_name)
    except Exception as e:
        img_debugger.error(f"Error showing COL validation results: {str(e)}")
        return False

# Export functions
__all__ = [
    'COLAnalysisDialog',
    'COLInfoDialog', 
    'show_col_analysis_dialog',
    'show_col_info_dialog',
    'show_col_validation_results'
]