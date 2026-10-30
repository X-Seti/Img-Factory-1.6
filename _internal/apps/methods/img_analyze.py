#this belongs in methods/img_analyze.py - Version: 3
# X-Seti - August27 2025 - IMG Factory 1.5 - IMG Analysis Functions
# Consolidated from img_corruption_analyzer.py and img_manager.py

"""
IMG Analysis Functions - Corruption analysis, structure validation, and health checks
"""

import os
import re
import struct
from typing import Dict, List, Any, Optional, Tuple
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QLabel, QPushButton,
    QTableWidget, QTableWidgetItem, QTextEdit, QProgressBar,
    QMessageBox, QGroupBox, QTabWidget, QWidget, QHeaderView
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QFont

##Methods list -
# analyze_img_corruption
# analyze_img_structure
# analyze_img_health
# validate_img_integrity
# detect_filename_corruption
# fix_filename_corruption
# show_analysis_dialog

##Classes -
# IMGAnalysisDialog

class IMGAnalysisDialog(QDialog): #vers 1
    """Dialog for comprehensive IMG analysis"""
    
    def __init__(self, parent=None, analysis_data=None):
        super().__init__(parent)
        self.setWindowTitle("IMG Analysis Report")
        self.setModal(True)
        self.setFixedSize(800, 600)
        self.analysis_data = analysis_data or {}
        
        self.setup_ui()
        self.populate_data()
    
    def setup_ui(self):
        """Setup analysis dialog UI"""
        layout = QVBoxLayout(self)
        
        # Title
        title_label = QLabel("IMG File Analysis")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        title_label.setStyleSheet("font-size: 16px; font-weight: bold; margin: 10px;")
        layout.addWidget(title_label)
        
        # Tab widget for different analysis views
        self.tabs = QTabWidget()
        
        # Corruption analysis tab
        self.create_corruption_tab()
        
        # Structure analysis tab
        self.create_structure_tab()
        
        # Health analysis tab
        self.create_health_tab()
        
        layout.addWidget(self.tabs)
        
        # Action buttons
        button_layout = QHBoxLayout()
        button_layout.addStretch()
        
        export_btn = QPushButton("Export Report...")
        export_btn.clicked.connect(self.export_report)
        button_layout.addWidget(export_btn)
        
        fix_btn = QPushButton("Fix Issues...")
        fix_btn.clicked.connect(self.fix_issues)
        button_layout.addWidget(fix_btn)
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)
    
    def create_corruption_tab(self):
        """Create corruption analysis tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        # Summary
        self.corruption_summary = QLabel("Analyzing corruption...")
        layout.addWidget(self.corruption_summary)
        
        # Corrupted entries table
        self.corruption_table = QTableWidget()
        self.corruption_table.setColumnCount(3)
        self.corruption_table.setHorizontalHeaderLabels(["Entry Name", "Issue", "Severity"])
        
        header = self.corruption_table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.Stretch)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        
        layout.addWidget(self.corruption_table)
        
        self.tabs.addTab(tab, "Corruption")
    
    def create_structure_tab(self):
        """Create structure analysis tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        self.structure_summary = QLabel("Analyzing structure...")
        layout.addWidget(self.structure_summary)
        
        self.structure_details = QTextEdit()
        self.structure_details.setReadOnly(True)
        layout.addWidget(self.structure_details)
        
        self.tabs.addTab(tab, "Structure")
    
    def create_health_tab(self):
        """Create health analysis tab"""
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        self.health_summary = QLabel("Analyzing health...")
        layout.addWidget(self.health_summary)
        
        self.health_details = QTextEdit()
        self.health_details.setReadOnly(True)
        layout.addWidget(self.health_details)
        
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
        
        # Populate health tab
        health_data = self.analysis_data.get('health', {})
        health_text = f"""IMG Health Report:

Overall Health: {health_data.get('overall_health', 'Unknown')}
Integrity Score: {health_data.get('integrity_score', 0)}/100

Issues Found:
- Corrupted Entries: {len(corrupted_entries)}
- Size Mismatches: {health_data.get('size_mismatches', 0)}
- Offset Errors: {health_data.get('offset_errors', 0)}
- Invalid Names: {health_data.get('invalid_names', 0)}

Recommendations:
{chr(10).join(health_data.get('recommendations', ['No recommendations available']))}
"""
        self.health_details.setText(health_text)
    
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
        report = "IMG FACTORY 1.5 - ANALYSIS REPORT\n"
        report += "=" * 50 + "\n\n"
        
        # Add corruption data
        corruption_data = self.analysis_data.get('corruption', {})
        report += "CORRUPTION ANALYSIS:\n"
        report += f"Corrupted Entries: {len(corruption_data.get('corrupted_entries', []))}\n"
        report += f"Severity: {corruption_data.get('severity', 'Unknown')}\n\n"
        
        # Add structure data
        structure_data = self.analysis_data.get('structure', {})
        report += "STRUCTURE ANALYSIS:\n"
        report += f"Version: {structure_data.get('version', 'Unknown')}\n"
        report += f"Entry Count: {structure_data.get('entry_count', 0)}\n"
        report += f"File Size: {structure_data.get('file_size_mb', 0):.1f} MB\n\n"
        
        # Add health data
        health_data = self.analysis_data.get('health', {})
        report += "HEALTH ANALYSIS:\n"
        report += f"Overall Health: {health_data.get('overall_health', 'Unknown')}\n"
        report += f"Integrity Score: {health_data.get('integrity_score', 0)}/100\n"
        
        return report
    
    def fix_issues(self):
        """Handle fixing detected issues"""
        corruption_data = self.analysis_data.get('corruption', {})
        corrupted_entries = corruption_data.get('corrupted_entries', [])
        
        if not corrupted_entries:
            QMessageBox.information(self, "No Issues", "No fixable issues detected.")
            return
        
        reply = QMessageBox.question(
            self, "Fix Issues",
            f"Attempt to fix {len(corrupted_entries)} detected issues?\n\n"
            "This may remove corrupted entries and rebuild the IMG file.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            self.accept()
            # Signal to parent that fixes should be applied
            self.apply_fixes = True


def analyze_img_corruption(img_file, main_window=None) -> Dict[str, Any]: #vers 2
    """Comprehensive IMG corruption analysis - MOVED FROM img_corruption_analyzer.py"""
    try:
        report = {
            'corrupted_entries': [],
            'suspicious_entries': [],
            'filename_issues': [],
            'size_mismatches': [],
            'offset_errors': [],
            'severity': 'unknown',
            'total_entries': len(img_file.entries) if img_file.entries else 0
        }
        
        corrupted_entries = []
        
        for entry in img_file.entries:
            entry_issues = []
            
            # Check for filename corruption
            if _has_filename_corruption(entry.name):
                entry_issues.append("Corrupted filename")
                report['filename_issues'].append({
                    'name': entry.name,
                    'original': _extract_original_filename(entry.name)
                })
            
            # Check for size consistency
            try:
                data = entry.get_data() if hasattr(entry, 'get_data') else b''
                expected_size = entry.size * 2048  # IMG uses 2048-byte sectors
                
                if len(data) != expected_size and len(data) != entry.size:
                    entry_issues.append("Size mismatch")
                    report['size_mismatches'].append({
                        'name': entry.name,
                        'expected': expected_size,
                        'actual': len(data)
                    })
                
            except Exception as e:
                entry_issues.append(f"Data read error: {str(e)}")
            
            # Check for offset alignment
            if hasattr(entry, 'offset') and entry.offset % 2048 != 0:
                entry_issues.append("Misaligned offset")
                report['offset_errors'].append({
                    'name': entry.name,
                    'offset': entry.offset,
                    'expected_alignment': 2048
                })
            
            # Check for invalid characters in filename
            if not _is_valid_filename(entry.name):
                entry_issues.append("Invalid filename characters")
            
            # If any issues found, add to corrupted entries
            if entry_issues:
                corrupted_entries.append({
                    'name': entry.name,
                    'issues': entry_issues,
                    'issue': ', '.join(entry_issues),
                    'severity': _determine_severity(entry_issues)
                })
        
        report['corrupted_entries'] = corrupted_entries
        
        # Determine overall severity
        corruption_percentage = len(corrupted_entries) / len(img_file.entries) * 100 if img_file.entries else 0
        
        if corruption_percentage == 0:
            report['severity'] = "No corruption detected"
        elif corruption_percentage < 5:
            report['severity'] = "Minor corruption"
        elif corruption_percentage < 25:
            report['severity'] = "Moderate corruption"
        else:
            report['severity'] = "Severe corruption"
        
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Analysis complete: {len(corrupted_entries)} corrupted entries found ({corruption_percentage:.1f}%)")
        
        return report
        
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Corruption analysis failed: {str(e)}")
        return {'error': str(e)}


def analyze_img_structure(img_file, main_window=None) -> Dict[str, Any]: #vers 1
    """Analyze IMG file structure and organization"""
    try:
        analysis = {
            'version': 'Unknown',
            'entry_count': len(img_file.entries) if img_file.entries else 0,
            'file_size_mb': 0,
            'directory_size': 0,
            'data_size_mb': 0,
            'fragmentation_percent': 0,
            'wasted_space_mb': 0,
            'alignment_issues': 0
        }
        
        if hasattr(img_file, 'version'):
            analysis['version'] = str(img_file.version)
        
        if hasattr(img_file, 'file_path') and os.path.exists(img_file.file_path):
            file_size = os.path.getsize(img_file.file_path)
            analysis['file_size_mb'] = file_size / (1024 * 1024)
        
        # Calculate directory size (32 bytes per entry for V2)
        if img_file.entries:
            analysis['directory_size'] = len(img_file.entries) * 32
            
            # Calculate data size and fragmentation
            total_data_size = sum(entry.size * 2048 for entry in img_file.entries if hasattr(entry, 'size'))
            analysis['data_size_mb'] = total_data_size / (1024 * 1024)
            
            # Check for alignment issues
            alignment_issues = sum(1 for entry in img_file.entries 
                                 if hasattr(entry, 'offset') and entry.offset % 2048 != 0)
            analysis['alignment_issues'] = alignment_issues
            
            # Estimate fragmentation (simplified)
            if len(img_file.entries) > 1:
                sorted_entries = sorted(img_file.entries, key=lambda e: getattr(e, 'offset', 0))
                gaps = 0
                for i in range(1, len(sorted_entries)):
                    prev_end = getattr(sorted_entries[i-1], 'offset', 0) + getattr(sorted_entries[i-1], 'size', 0) * 2048
                    current_start = getattr(sorted_entries[i], 'offset', 0)
                    if current_start > prev_end:
                        gaps += current_start - prev_end
                
                analysis['fragmentation_percent'] = (gaps / analysis['file_size_mb'] * 1024 * 1024) * 100 if analysis['file_size_mb'] > 0 else 0
                analysis['wasted_space_mb'] = gaps / (1024 * 1024)
        
        return analysis
        
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Structure analysis failed: {str(e)}")
        return {'error': str(e)}


def analyze_img_health(img_file, main_window=None) -> Dict[str, Any]: #vers 1
    """Comprehensive health analysis of IMG file"""
    try:
        # Get corruption and structure data
        corruption_data = analyze_img_corruption(img_file, main_window)
        structure_data = analyze_img_structure(img_file, main_window)
        
        health = {
            'overall_health': 'Unknown',
            'integrity_score': 0,
            'size_mismatches': len(corruption_data.get('size_mismatches', [])),
            'offset_errors': len(corruption_data.get('offset_errors', [])),
            'invalid_names': len(corruption_data.get('filename_issues', [])),
            'recommendations': []
        }
        
        # Calculate integrity score (0-100)
        total_entries = len(img_file.entries) if img_file.entries else 1
        corrupted_entries = len(corruption_data.get('corrupted_entries', []))
        
        integrity_score = max(0, 100 - (corrupted_entries / total_entries * 100))
        health['integrity_score'] = int(integrity_score)
        
        # Determine overall health
        if integrity_score >= 95:
            health['overall_health'] = 'Excellent'
        elif integrity_score >= 80:
            health['overall_health'] = 'Good'
        elif integrity_score >= 60:
            health['overall_health'] = 'Fair'
        elif integrity_score >= 30:
            health['overall_health'] = 'Poor'
        else:
            health['overall_health'] = 'Critical'
        
        # Generate recommendations
        recommendations = []
        
        if corrupted_entries > 0:
            recommendations.append(f"Fix {corrupted_entries} corrupted entries")
        
        if structure_data.get('fragmentation_percent', 0) > 20:
            recommendations.append("Consider rebuilding IMG to reduce fragmentation")
        
        if structure_data.get('wasted_space_mb', 0) > 10:
            recommendations.append("Rebuild IMG to reclaim wasted space")
        
        if structure_data.get('alignment_issues', 0) > 0:
            recommendations.append("Fix offset alignment issues")
        
        if not recommendations:
            recommendations.append("IMG file is in good condition")
        
        health['recommendations'] = recommendations
        
        return health
        
    except Exception as e:
        if main_window and hasattr(main_window, 'log_message'):
            main_window.log_message(f"Health analysis failed: {str(e)}")
        return {'error': str(e)}


def validate_img_integrity(img_file, main_window=None) -> bool: #vers 1
    """Validate overall IMG file integrity"""
    try:
        corruption_data = analyze_img_corruption(img_file, main_window)
        return len(corruption_data.get('corrupted_entries', [])) == 0
    except Exception:
        return False


def detect_filename_corruption(filename: str) -> bool: #vers 1
    """Detect if filename has corruption indicators"""
    return _has_filename_corruption(filename)


def fix_filename_corruption(filename: str) -> str: #vers 1
    """Attempt to fix corrupted filename"""
    return _extract_original_filename(filename)


def show_analysis_dialog(main_window) -> Optional[Dict]: #vers 1
    """Show comprehensive IMG analysis dialog"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            QMessageBox.warning(main_window, "No IMG File", "Please open an IMG file first.")
            return None
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Starting comprehensive IMG analysis...")
        
        # Perform all analyses
        corruption_data = analyze_img_corruption(main_window.current_img, main_window)
        structure_data = analyze_img_structure(main_window.current_img, main_window)
        health_data = analyze_img_health(main_window.current_img, main_window)
        
        analysis_data = {
            'corruption': corruption_data,
            'structure': structure_data,
            'health': health_data
        }
        
        # Show dialog
        dialog = IMGAnalysisDialog(main_window, analysis_data)
        
        if dialog.exec() == QDialog.DialogCode.Accepted:
            if hasattr(dialog, 'apply_fixes') and dialog.apply_fixes:
                return analysis_data
        
        return None
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Analysis dialog failed: {str(e)}")
        QMessageBox.critical(main_window, "Analysis Failed", f"Failed to show analysis dialog:\n{str(e)}")
        return None


# Helper functions
def _has_filename_corruption(filename: str) -> bool: #vers 2
    """Check if filename shows signs of corruption"""
    # Common corruption patterns
    corruption_patterns = [
        r'\.dff[a-z]{2,10}',   # .dff followed by random chars
        r'\.txd[a-z]{2,10}',   # .txd followed by random chars
        r'[a-zA-Z0-9]+\x00+',  # Null bytes in filename
        r'^[^a-zA-Z0-9]',      # Starting with special chars
        r'[^\x20-\x7E]',       # Non-printable characters
    ]
    
    return any(re.search(pattern, filename) for pattern in corruption_patterns)


def _extract_original_filename(corrupted_filename: str) -> str: #vers 1
    """Try to extract original filename from corrupted one"""
    # Remove common corruption suffixes
    clean_name = corrupted_filename
    
    # Remove trailing corruption after valid extensions
    extensions = ['.dff', '.txd', '.col', '.ifp', '.dat', '.cfg']
    for ext in extensions:
        if ext in clean_name:
            pos = clean_name.find(ext)
            if pos != -1:
                clean_name = clean_name[:pos + len(ext)]
                break
    
    # Remove null bytes and non-printable characters
    clean_name = ''.join(char for char in clean_name if ord(char) >= 32 and ord(char) <= 126)
    
    return clean_name if clean_name else corrupted_filename


def _is_valid_filename(filename: str) -> bool: #vers 1
    """Check if filename contains only valid characters"""
    # Check for null bytes, control characters, and invalid path characters
    invalid_chars = '\x00<>:"|?*'
    return not any(char in filename for char in invalid_chars) and len(filename) <= 24


def _determine_severity(issues: List[str]) -> str: #vers 1
    """Determine severity based on issue types"""
    high_severity_issues = ['Data read error', 'Size mismatch', 'Misaligned offset']
    medium_severity_issues = ['Corrupted filename', 'Invalid filename characters']
    
    if any(issue.startswith(high) for issue in issues for high in high_severity_issues):
        return 'High'
    elif any(issue.startswith(med) for issue in issues for med in medium_severity_issues):
        return 'Medium'
    else:
        return 'Low'


# Export list for external imports
__all__ = [
    'analyze_img_corruption',
    'analyze_img_structure', 
    'analyze_img_health',
    'validate_img_integrity',
    'detect_filename_corruption',
    'fix_filename_corruption',
    'show_analysis_dialog',
    'IMGAnalysisDialog'
]
