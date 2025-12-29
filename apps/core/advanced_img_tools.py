#this belongs in core/advanced_img_tools.py - Version: 1
# X-Seti - December04 2025 - IMG Factory 1.5 - Advanced IMG Tools

"""
Advanced IMG Tools - Comprehensive checking and analysis tools for IMG files
"""

import os
from typing import List, Dict, Any, Optional
from PyQt6.QtWidgets import (
    QMessageBox, QDialog, QVBoxLayout, QHBoxLayout, 
    QLabel, QPushButton, QTextEdit, QGroupBox, QCheckBox,
    QTabWidget, QWidget, QTableWidget, QTableWidgetItem,
    QHeaderView, QProgressBar
)
from PyQt6.QtCore import Qt
from apps.methods.tab_system import get_current_file_from_active_tab, validate_tab_before_operation
from apps.methods.img_analyze import analyze_img_corruption, analyze_img_structure, IMGAnalysisDialog
from apps.methods.find_dups_functions import find_duplicates_by_hash, find_duplicates_by_name, find_duplicates_by_size
from apps.core.sort_via_ide import parse_ide_for_model_order, sort_img_entries_by_ide

##Methods list -
# advanced_img_check
# comprehensive_img_analysis
# img_optimization_check
# integrate_advanced_img_tools

def advanced_img_check(main_window):
    """Run comprehensive checks on the current IMG file"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Advanced IMG Check"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Run comprehensive analysis
        analysis_result = comprehensive_img_analysis(file_object, main_window)
        
        if analysis_result:
            # Show detailed analysis dialog
            dialog = AdvancedAnalysisDialog(main_window, analysis_result)
            dialog.exec()
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Advanced IMG check error: {str(e)}")
        QMessageBox.critical(main_window, "Advanced Check Error", f"Advanced IMG check failed: {str(e)}")
        return False


def comprehensive_img_analysis(img_file, main_window=None) -> Dict[str, Any]:
    """Perform comprehensive analysis of IMG file"""
    try:
        analysis_result = {
            'corruption': analyze_img_corruption(img_file, main_window),
            'structure': analyze_img_structure(img_file, main_window),
            'duplicates': {
                'by_hash': find_duplicates_by_hash(img_file),
                'by_name': find_duplicates_by_name(img_file),
                'by_size': find_duplicates_by_size(img_file)
            },
            'file_types': analyze_file_types(img_file),
            'size_distribution': analyze_size_distribution(img_file),
            'health_score': calculate_health_score(img_file)
        }
        
        return analysis_result
        
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Comprehensive analysis failed: {str(e)}")
        return {'error': str(e)}


def analyze_file_types(img_file) -> Dict[str, int]:
    """Analyze distribution of file types in IMG"""
    try:
        type_counts = {}
        
        for entry in img_file.entries:
            ext = entry.name.split('.')[-1].lower() if '.' in entry.name else 'no_ext'
            type_counts[ext] = type_counts.get(ext, 0) + 1
        
        return type_counts
    except Exception:
        return {}


def analyze_size_distribution(img_file) -> Dict[str, Any]:
    """Analyze size distribution of entries"""
    try:
        sizes = [entry.size * 2048 for entry in img_file.entries if hasattr(entry, 'size')]
        
        if not sizes:
            return {}
        
        return {
            'total_entries': len(sizes),
            'total_size': sum(sizes),
            'avg_size': sum(sizes) / len(sizes),
            'min_size': min(sizes),
            'max_size': max(sizes),
            'size_ranges': {
                'tiny': len([s for s in sizes if s < 1024]),      # < 1KB
                'small': len([s for s in sizes if 1024 <= s < 1024*100]),  # 1KB - 100KB
                'medium': len([s for s in sizes if 1024*100 <= s < 1024*1000]),  # 100KB - 1MB
                'large': len([s for s in sizes if s >= 1024*1000])  # >= 1MB
            }
        }
    except Exception:
        return {}


def calculate_health_score(img_file) -> int:
    """Calculate overall health score for IMG file (0-100)"""
    try:
        # Base score
        score = 100
        
        # Check for corruption
        corrupted_count = sum(1 for entry in img_file.entries if _has_corruption_issues(entry))
        corruption_percentage = (corrupted_count / len(img_file.entries)) * 100 if img_file.entries else 0
        score -= min(corruption_percentage * 2, 50)  # Max 50 point deduction for corruption
        
        # Check for duplicates
        dup_counts = len(find_duplicates_by_hash(img_file))
        duplicate_percentage = (dup_counts / len(img_file.entries)) * 100 if img_file.entries else 0
        score -= min(duplicate_percentage, 20)  # Max 20 point deduction for duplicates
        
        # Check for invalid filenames
        invalid_count = sum(1 for entry in img_file.entries if not _is_valid_filename(entry.name))
        invalid_percentage = (invalid_count / len(img_file.entries)) * 100 if img_file.entries else 0
        score -= min(invalid_percentage, 30)  # Max 30 point deduction for invalid names
        
        return max(0, int(score))  # Ensure score is between 0 and 100
    except Exception:
        return 50  # Default score if calculation fails


def _has_corruption_issues(entry) -> bool:
    """Check if entry has corruption issues"""
    try:
        # Check for null bytes in name
        if b'\x00' in entry.name.encode('utf-8', errors='ignore'):
            return True
        # Check for invalid characters
        if any(c in entry.name for c in '<>:"/\\|?*'):
            return True
        # Check for excessive length
        if len(entry.name) > 24:  # IMG filename limit
            return True
        return False
    except Exception:
        return True


def _is_valid_filename(name: str) -> bool:
    """Check if filename is valid for IMG files"""
    try:
        # Check for invalid characters
        invalid_chars = '<>:"/\\|?*'
        if any(char in name for char in invalid_chars):
            return False
        # Check length
        if len(name) > 24:
            return False
        return True
    except Exception:
        return False


class AdvancedAnalysisDialog(QDialog):
    """Dialog for displaying advanced IMG analysis results"""
    
    def __init__(self, parent=None, analysis_data=None):
        super().__init__(parent)
        self.setWindowTitle("Advanced IMG Analysis")
        self.setModal(True)
        self.setFixedSize(900, 700)
        self.analysis_data = analysis_data or {}
        
        self.setup_ui()
        self.populate_data()
    
    def setup_ui(self):
        """Setup analysis dialog UI"""
        layout = QVBoxLayout(self)
        
        # Title
        title_label = QLabel("🔍 Advanced IMG Analysis Report")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_label.setStyleSheet("font-size: 16px; font-weight: bold; margin: 10px;")
        layout.addWidget(title_label)
        
        # Tabs for different analysis views
        self.tabs = QTabWidget()
        
        # Corruption analysis tab
        self.create_corruption_tab()
        
        # Structure analysis tab
        self.create_structure_tab()
        
        # Duplicates analysis tab
        self.create_duplicates_tab()
        
        # File types analysis tab
        self.create_file_types_tab()
        
        # Health analysis tab
        self.create_health_tab()
        
        layout.addWidget(self.tabs)
        
        # Action buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        export_btn = QPushButton("Export Report...")
        export_btn.clicked.connect(self.export_report)
        button_layout.addWidget(export_btn)
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)
    
    def create_corruption_tab(self):
        """Create corruption analysis tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        summary = QLabel("Analyzing corruption...")
        layout.addWidget(summary)
        
        table = QTableWidget()
        table.setColumnCount(3)
        table.setHorizontalHeaderLabels(["Entry Name", "Issue", "Severity"])
        
        header = table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        
        layout.addWidget(table)
        
        self.corruption_summary = summary
        self.corruption_table = table
        self.tabs.addTab(tab, "Corruption")
    
    def create_structure_tab(self):
        """Create structure analysis tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        summary = QLabel("Analyzing structure...")
        layout.addWidget(summary)
        
        details = QTextEdit()
        details.setReadOnly(True)
        layout.addWidget(details)
        
        self.structure_summary = summary
        self.structure_details = details
        self.tabs.addTab(tab, "Structure")
    
    def create_duplicates_tab(self):
        """Create duplicates analysis tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        summary = QLabel("Analyzing duplicates...")
        layout.addWidget(summary)
        
        details = QTextEdit()
        details.setReadOnly(True)
        layout.addWidget(details)
        
        self.duplicates_summary = summary
        self.duplicates_details = details
        self.tabs.addTab(tab, "Duplicates")
    
    def create_file_types_tab(self):
        """Create file types analysis tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        summary = QLabel("Analyzing file types...")
        layout.addWidget(summary)
        
        details = QTextEdit()
        details.setReadOnly(True)
        layout.addWidget(details)
        
        self.types_summary = summary
        self.types_details = details
        self.tabs.addTab(tab, "File Types")
    
    def create_health_tab(self):
        """Create health analysis tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        summary = QLabel("Analyzing health...")
        layout.addWidget(summary)
        
        details = QTextEdit()
        details.setReadOnly(True)
        layout.addWidget(details)
        
        self.health_summary = summary
        self.health_details = details
        self.tabs.addTab(tab, "Health")
    
    def populate_data(self):
        """Populate analysis data"""
        if not self.analysis_data:
            return
        
        # Populate corruption tab
        corruption_data = self.analysis_data.get('corruption', {})
        corrupted_entries = corruption_data.get('corrupted_entries', [])
        
        if corrupted_entries:
            self.corruption_summary.setText(
                f"Found {len(corrupted_entries)} corrupted entries. "
                f"Severity: {corruption_data.get('severity', 'Unknown')}"
            )
            
            self.corruption_table.setRowCount(len(corrupted_entries))
            for i, entry in enumerate(corrupted_entries):
                self.corruption_table.setItem(i, 0, QTableWidgetItem(entry.get('name', 'Unknown')))
                self.corruption_table.setItem(i, 1, QTableWidgetItem(entry.get('issue', 'Unknown issue')))
                self.corruption_table.setItem(i, 2, QTableWidgetItem(entry.get('severity', 'Medium')))
        else:
            self.corruption_summary.setText("No corruption detected in this IMG file.")
        
        # Populate structure tab
        structure_data = self.analysis_data.get('structure', {})
        structure_text = f"""IMG Structure Analysis:

Version: {structure_data.get('version', 'Unknown')}
Entry Count: {structure_data.get('entry_count', 0)}
File Size: {structure_data.get('file_size_mb', 0):.1f} MB
Directory Size: {structure_data.get('directory_size', 0)} bytes
Data Size: {structure_data.get('data_size_mb', 0):.1f} MB

Fragmentation: {structure_data.get('fragmentation_percent', 0):.1f}%
Wasted Space: {structure_data.get('wasted_space_mb', 0):.1f} MB
Alignment Issues: {structure_data.get('alignment_issues', 0)}
"""
        self.structure_details.setText(structure_text)
        
        # Populate duplicates tab
        duplicates_data = self.analysis_data.get('duplicates', {})
        total_duplicates = sum(len(d) for d in duplicates_data.values() if isinstance(d, dict))
        
        duplicates_text = f"""Duplicate Analysis:

By Hash: {len(duplicates_data.get('by_hash', {}))} groups
By Name: {len(duplicates_data.get('by_name', {}))} groups  
By Size: {len(duplicates_data.get('by_size', {}))} groups
Total Duplicates: {total_duplicates}

Potential space savings from removing duplicates: ~{(total_duplicates * 1024) / (1024*1024):.1f} MB
"""
        self.duplicates_details.setText(duplicates_text)
        
        # Populate file types tab
        types_data = self.analysis_data.get('file_types', {})
        types_text = "File Type Distribution:\n\n"
        for file_type, count in sorted(types_data.items(), key=lambda x: x[1], reverse=True):
            types_text += f"{file_type.upper()}: {count} files\n"
        self.types_details.setText(types_text)
        
        # Populate health tab
        health_score = self.analysis_data.get('health_score', 50)
        health_text = f"""IMG Health Report:

Overall Health Score: {health_score}/100 ({self._get_health_rating(health_score)})

The health score is calculated based on:
- Corruption level: {(100 - corruption_data.get('corrupted_entries', [0]) if len(corruption_data.get('corrupted_entries', [])) > 0 else 100) if 'corruption' in self.analysis_data else 'N/A'}
- Duplicate entries: {(100 - total_duplicates) if 'duplicates' in self.analysis_data else 'N/A'}
- Invalid filenames: To be implemented

Recommendations:
{self._get_recommendations(health_score, corrupted_entries, total_duplicates)}
"""
        self.health_details.setText(health_text)
    
    def _get_health_rating(self, score: int) -> str:
        """Get health rating based on score"""
        if score >= 90:
            return "Excellent"
        elif score >= 75:
            return "Good"
        elif score >= 50:
            return "Fair"
        elif score >= 25:
            return "Poor"
        else:
            return "Critical"
    
    def _get_recommendations(self, score: int, corrupted_entries: list, duplicate_count: int) -> str:
        """Get recommendations based on analysis"""
        recommendations = []
        
        if corrupted_entries:
            recommendations.append(f"• Fix {len(corrupted_entries)} corrupted entries")
        
        if duplicate_count > 0:
            recommendations.append(f"• Remove {duplicate_count} duplicate entries to save space")
        
        if score < 50:
            recommendations.append("• Consider rebuilding the IMG file")
        
        if not recommendations:
            recommendations.append("• No specific issues detected")
        
        return '\n'.join(recommendations)
    
    def export_report(self):
        """Export analysis report to file"""
        try:
            from PyQt6.QtWidgets import QFileDialog
            
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Save Analysis Report", "", 
                "Text Files (*.txt);;All Files (*.*)"
            )
            
            if file_path:
                report_content = self.generate_report_text()
                
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(report_content)
                
                QMessageBox.information(self, "Export Complete", 
                                      f"Analysis report saved to:\n{file_path}")
        
        except Exception as e:
            QMessageBox.critical(self, "Export Failed", f"Failed to export report:\n{str(e)}")
    
    def generate_report_text(self) -> str:
        """Generate complete report text"""
        report = "IMG FACTORY 1.5 - ADVANCED ANALYSIS REPORT\n"
        report += "=" * 60 + "\n\n"
        
        # Add all analysis data
        corruption_data = self.analysis_data.get('corruption', {})
        structure_data = self.analysis_data.get('structure', {})
        duplicates_data = self.analysis_data.get('duplicates', {})
        types_data = self.analysis_data.get('file_types', {})
        health_score = self.analysis_data.get('health_score', 50)
        
        report += f"CORRUPTION ANALYSIS:\n"
        report += f"Corrupted Entries: {len(corruption_data.get('corrupted_entries', []))}\n"
        report += f"Severity: {corruption_data.get('severity', 'Unknown')}\n\n"
        
        report += f"STRUCTURE ANALYSIS:\n"
        report += f"Version: {structure_data.get('version', 'Unknown')}\n"
        report += f"Entry Count: {structure_data.get('entry_count', 0)}\n"
        report += f"File Size: {structure_data.get('file_size_mb', 0):.1f} MB\n\n"
        
        report += f"DUPLICATES ANALYSIS:\n"
        report += f"By Hash: {len(duplicates_data.get('by_hash', {}))} groups\n"
        report += f"By Name: {len(duplicates_data.get('by_name', {}))} groups\n"
        report += f"By Size: {len(duplicates_data.get('by_size', {}))} groups\n\n"
        
        report += f"FILE TYPES:\n"
        for file_type, count in sorted(types_data.items(), key=lambda x: x[1], reverse=True):
            report += f"  {file_type.upper()}: {count} files\n"
        report += "\n"
        
        report += f"HEALTH ANALYSIS:\n"
        report += f"Overall Health Score: {health_score}/100\n"
        
        return report


def integrate_advanced_img_tools(main_window) -> bool:
    """Integrate advanced IMG tools into main window"""
    try:
        main_window.advanced_img_check = lambda: advanced_img_check(main_window)
        main_window.comprehensive_analysis = lambda: advanced_img_check(main_window)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Advanced IMG tools integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate advanced IMG tools: {str(e)}")
        return False