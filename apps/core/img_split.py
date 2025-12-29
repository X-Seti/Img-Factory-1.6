#this belongs in core/img_split.py - Version: 3
# X-Seti - July31 2025 - IMG Factory 1.5 - Complete Split Functions

"""
IMG Factory Split Functions
Split IMG files using IDE data to organize models and textures
"""

import os
import struct
from typing import Dict, List, Set, Optional, Tuple
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QGroupBox, QLabel, QPushButton,
    QLineEdit, QComboBox, QCheckBox, QTextEdit, QProgressDialog, QFileDialog,
    QMessageBox, QTabWidget, QWidget, QFormLayout, QSpinBox, QListWidget,
    QListWidgetItem, QSplitter
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal
from PyQt6.QtGui import QFont

##Methods list -
# split_img
# split_img_via
# integrate_split_functions
# IDEParser
# SplitDialog
# SplitThread

##Classes -
# IDEParser
# SplitDialog
# SplitThread

def split_img(main_window): #vers 2
    """Basic split IMG file into smaller parts"""
    if not hasattr(main_window, 'current_img') or not main_window.current_img:
        QMessageBox.warning(main_window, "No IMG File", "No IMG file is currently loaded")
        return

    try:
        # Show basic split dialog
        dialog = SplitDialog(main_window, basic_mode=True)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            main_window.log_message("🔄 Starting IMG split operation...")
            
    except Exception as e:
        main_window.log_message(f"❌ Error in split_img: {str(e)}")

def split_img_via(main_window): #vers 1
    """Split IMG file using IDE file for organization"""
    if not hasattr(main_window, 'current_img') or not main_window.current_img:
        QMessageBox.warning(main_window, "No IMG File", "No IMG file is currently loaded")
        return

    try:
        # Show advanced split dialog with IDE support
        dialog = SplitDialog(main_window, basic_mode=False)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            main_window.log_message("🔄 Starting advanced IMG split with IDE data...")
            
    except Exception as e:
        main_window.log_message(f"❌ Error in split_img_via: {str(e)}")

def integrate_split_functions(main_window): #vers 1
    """Integrate split functions into main window"""
    try:
        # Add split methods to main window
        main_window.split_img = lambda: split_img(main_window)
        main_window.split_img_via = lambda: split_img_via(main_window)
        
        main_window.log_message("✅ Split functions integrated")
        return True
        
    except Exception as e:
        main_window.log_message(f"❌ Error integrating split functions: {str(e)}")
        return False

class IDEParser:
    """Parser for IDE files to extract model and texture relationships"""
    
    def __init__(self): #vers 1
        self.models = {}  # model_id -> {name, txd, dff, type}
        self.textures = {}  # txd_name -> [texture_files]
        self.sections = {}  # section_name -> [entries]
        
    def parse_ide_file(self, ide_path: str) -> bool: #vers 1
        """Parse IDE file and extract definitions"""
        try:
            with open(ide_path, 'r', encoding='ascii', errors='ignore') as f:
                content = f.read()
            
            lines = content.split('\n')
            current_section = None
            
            for line_num, line in enumerate(lines, 1):
                line = line.strip()
                
                # Skip empty lines and comments
                if not line or line.startswith('#') or line.startswith('//'):
                    continue
                
                # Check for section headers
                if line.lower() in ['objs', 'tobj', 'weap', 'hier', 'anim', 'cars', 'peds', 'path', 'ped', 'end']:
                    if line.lower() == 'end':
                        current_section = None
                    else:
                        current_section = line.lower()
                        if current_section not in self.sections:
                            self.sections[current_section] = []
                    continue
                
                # Parse section entries
                if current_section:
                    try:
                        self._parse_section_entry(current_section, line, line_num)
                    except Exception as e:
                        print(f"Warning: Error parsing line {line_num}: {e}")
            
            return True
            
        except Exception as e:
            print(f"Error parsing IDE file {ide_path}: {e}")
            return False
    
    def _remove_split_files_from_original(self, file_groups: Dict[str, List]): #vers 1
        """Remove split files from original IMG if preserve_structure is unchecked"""
        try:
            # Collect all files that were split out
            split_files = set()
            for group_name, files in file_groups.items():
                for entry in files:
                    split_files.add(entry.name.lower())
            
            # Remove split files from original IMG
            original_count = len(self.img_file.entries)
            self.img_file.entries = [
                entry for entry in self.img_file.entries 
                if entry.name.lower() not in split_files
            ]
            
            removed_count = original_count - len(self.img_file.entries)
            
            # Save the modified original IMG
            if hasattr(self.img_file, 'save_img_file'):
                self.img_file.save_img_file()
            elif hasattr(self.img_file, '_rebuild_version1') and hasattr(self.img_file, '_rebuild_version2'):
                if getattr(self.img_file, 'version', 2) == 1:
                    self.img_file._rebuild_version1()
                else:
                    self.img_file._rebuild_version2()
            
            self.progress_updated.emit(100, f"Removed {removed_count} files from original IMG")
            self.stats['removed_from_original'] = removed_count
            
        except Exception as e:
            self.progress_updated.emit(100, f"Warning: Could not modify original IMG: {str(e)}")
            print(f"Error removing files from original IMG: {e}")
    
    def _parse_section_entry(self, section: str, line: str, line_num: int): #vers 1
        """Parse individual section entry"""
        parts = [part.strip() for part in line.split(',')]
        
        if section in ['objs', 'tobj']:
            # Objects: ID, ModelName, TxdName, [DrawDist, Flags, ...]
            if len(parts) >= 3:
                model_id = int(parts[0])
                model_name = parts[1].strip()
                txd_name = parts[2].strip()
                
                self.models[model_id] = {
                    'name': model_name,
                    'txd': txd_name,
                    'dff': f"{model_name}.dff",
                    'type': 'object',
                    'section': section
                }
                
                self.sections[section].append({
                    'id': model_id,
                    'model': model_name,
                    'txd': txd_name,
                    'line': line_num
                })
        
        elif section == 'cars':
            # Vehicles: ID, ModelName, TxdName, Type, HandlingId, ...
            if len(parts) >= 3:
                model_id = int(parts[0])
                model_name = parts[1].strip()
                txd_name = parts[2].strip()
                
                self.models[model_id] = {
                    'name': model_name,
                    'txd': txd_name,
                    'dff': f"{model_name}.dff",
                    'type': 'vehicle',
                    'section': section
                }
                
                self.sections[section].append({
                    'id': model_id,
                    'model': model_name,
                    'txd': txd_name,
                    'line': line_num
                })
        
        elif section == 'peds':
            # Pedestrians: ID, ModelName, TxdName, PedType, ...
            if len(parts) >= 3:
                model_id = int(parts[0])
                model_name = parts[1].strip()
                txd_name = parts[2].strip()
                
                self.models[model_id] = {
                    'name': model_name,
                    'txd': txd_name,
                    'dff': f"{model_name}.dff",
                    'type': 'pedestrian',
                    'section': section
                }
                
                self.sections[section].append({
                    'id': model_id,
                    'model': model_name,
                    'txd': txd_name,
                    'line': line_num
                })
    
    def get_model_relationships(self) -> Dict[str, List[str]]: #vers 1
        """Get model -> texture relationships"""
        relationships = {}
        
        for model_id, model_data in self.models.items():
            model_name = model_data['name']
            txd_name = model_data['txd']
            
            if model_name not in relationships:
                relationships[model_name] = []
            
            if txd_name not in relationships[model_name]:
                relationships[model_name].append(txd_name)
        
        return relationships
    
    def get_summary(self) -> str: #vers 1
        """Get parsing summary"""
        summary = f"IDE File Analysis:\n"
        summary += f"Total Models: {len(self.models)}\n"
        summary += f"Sections Found: {', '.join(self.sections.keys())}\n\n"
        
        for section, entries in self.sections.items():
            summary += f"{section.upper()}: {len(entries)} entries\n"
        
        return summary

class SplitDialog(QDialog):
    """Dialog for splitting IMG files with IDE support"""
    
    def __init__(self, main_window, basic_mode=False): #vers 1
        super().__init__(main_window)
        self.main_window = main_window
        self.basic_mode = basic_mode
        self.ide_parser = IDEParser()
        self.split_thread = None
        
        self.setWindowTitle("Split IMG File" if basic_mode else "Split IMG via IDE")
        self.setMinimumSize(600, 500)
        self.setup_ui()
        
    def setup_ui(self): #vers 1
        """Setup dialog UI"""
        layout = QVBoxLayout(self)
        
        # Create tab widget for different modes
        self.tabs = QTabWidget()
        layout.addWidget(self.tabs)
        
        if not self.basic_mode:
            # IDE Analysis tab
            self.setup_ide_tab()
        
        # Split Options tab
        self.setup_options_tab()
        
        # Progress tab
        self.setup_progress_tab()
        
        # Buttons
        button_layout = QHBoxLayout()
        
        self.analyze_btn = QPushButton("📋 Analyze IDE - Coming Soon")
        self.analyze_btn.clicked.connect(self.analyze_ide)
        self.analyze_btn.setVisible(not self.basic_mode)
        self.analyze_btn.setEnabled(False)  # Disabled until IDE editor ready
        self.analyze_btn.setStyleSheet("color: #888888;")  # Gray text
        
        self.split_btn = QPushButton("🔄 Start Split")
        self.split_btn.clicked.connect(self.start_split)
        
        self.cancel_btn = QPushButton("❌ Cancel")
        self.cancel_btn.clicked.connect(self.reject)
        
        button_layout.addWidget(self.analyze_btn)
        button_layout.addStretch()
        button_layout.addWidget(self.split_btn)
        button_layout.addWidget(self.cancel_btn)
        
        layout.addLayout(button_layout)
    
    def setup_ide_tab(self): #vers 1
        """Setup IDE analysis tab"""
        ide_tab = QWidget()
        layout = QVBoxLayout(ide_tab)
        
        # IDE file selection - DISABLED until IDE editor ready
        ide_group = QGroupBox("IDE File Selection - Coming Soon")
        ide_group.setEnabled(False)  # Gray out entire section
        ide_group.setStyleSheet("QGroupBox { color: #888888; }")
        ide_layout = QFormLayout(ide_group)
        
        self.ide_path_input = QLineEdit()
        ide_browse_btn = QPushButton("Browse...")
        ide_browse_btn.clicked.connect(self.browse_ide_file)
        
        ide_file_layout = QHBoxLayout()
        ide_file_layout.addWidget(self.ide_path_input)
        ide_file_layout.addWidget(ide_browse_btn)
        
        ide_layout.addRow("IDE File:", ide_file_layout)
        layout.addWidget(ide_group)
        
        # Analysis results - DISABLED
        analysis_group = QGroupBox("Analysis Results - Coming Soon")
        analysis_group.setEnabled(False)
        analysis_group.setStyleSheet("QGroupBox { color: #888888; }")
        analysis_layout = QVBoxLayout(analysis_group)
        
        self.analysis_text = QTextEdit()
        self.analysis_text.setReadOnly(True)
        self.analysis_text.setFont(QFont("Courier", 9))
        self.analysis_text.setMaximumHeight(200)
        analysis_layout.addWidget(self.analysis_text)
        
        layout.addWidget(analysis_group)
        
        # Model relationships - DISABLED
        relationships_group = QGroupBox("Model → Texture Relationships - Coming Soon")
        relationships_group.setEnabled(False)
        relationships_group.setStyleSheet("QGroupBox { color: #888888; }")
        relationships_layout = QVBoxLayout(relationships_group)
        
        self.relationships_list = QListWidget()
        relationships_layout.addWidget(self.relationships_list)
        
        layout.addWidget(relationships_group)
        
        self.tabs.addTab(ide_tab, "📋 IDE Analysis")
    
    def setup_options_tab(self): #vers 1
        """Setup split options tab"""
        options_tab = QWidget()
        layout = QVBoxLayout(options_tab)
        
        # Output options
        output_group = QGroupBox("Output Options")
        output_layout = QFormLayout(output_group)
        
        self.output_format = QComboBox()
        self.output_format.addItems([
            "Folder Structure",
            "Multiple IMG v1 Files",
            "Multiple IMG v2 Files"
        ])
        output_layout.addRow("Output Format:", self.output_format)
        
        self.output_dir_input = QLineEdit()
        # Default to project folder from settings, fallback to Desktop
        default_path = self.get_project_folder_default()
        self.output_dir_input.setText(default_path)
        output_browse_btn = QPushButton("Browse...")
        output_browse_btn.clicked.connect(self.browse_output_dir)
        
        output_dir_layout = QHBoxLayout()
        output_dir_layout.addWidget(self.output_dir_input)
        output_dir_layout.addWidget(output_browse_btn)
        
        output_layout.addRow("Output Directory:", output_dir_layout)
        layout.addWidget(output_group)
        
        # Split options
        split_group = QGroupBox("Split Options")
        split_layout = QVBoxLayout(split_group)
        
        self.group_by_type = QCheckBox("Group by file type (DFF, TXD, COL, etc.)")
        self.group_by_type.setChecked(True)
        split_layout.addWidget(self.group_by_type)
        
        self.group_by_model = QCheckBox("Group by model (using IDE data) - Coming Soon")
        self.group_by_model.setChecked(False)
        self.group_by_model.setEnabled(False)  # Grayed out until IDE editor is added
        self.group_by_model.setStyleSheet("color: #888888;")  # Gray text
        split_layout.addWidget(self.group_by_model)
        
        self.create_index = QCheckBox("Create index file for each group")
        self.create_index.setChecked(True)
        split_layout.addWidget(self.create_index)
        
        self.preserve_structure = QCheckBox("Preserve original IMG structure (untick to remove split files from original)")
        self.preserve_structure.setChecked(True)  # Default to safe mode
        split_layout.addWidget(self.preserve_structure)
        
        layout.addWidget(split_group)
        
        # IDE Status - Shows at bottom when IDE is loaded
        self.ide_status_label = QLabel("")
        self.ide_status_label.setStyleSheet("color: #666; font-size: 9pt; font-style: italic;")
        self.ide_status_label.setVisible(False)  # Hidden until IDE loaded
        layout.addWidget(self.ide_status_label)
        
        self.tabs.addTab(options_tab, "⚙️ Split Options")
    
    def setup_progress_tab(self): #vers 1
        """Setup progress monitoring tab"""
        progress_tab = QWidget()
        layout = QVBoxLayout(progress_tab)
        
        # Progress info
        self.progress_label = QLabel("Ready to split...")
        layout.addWidget(self.progress_label)
        
        # Progress log
        self.progress_log = QTextEdit()
        self.progress_log.setReadOnly(True)
        self.progress_log.setFont(QFont("Courier", 9))
        layout.addWidget(self.progress_log)
        
        self.tabs.addTab(progress_tab, "📊 Progress")
    
    def get_project_folder_default(self) -> str: #vers 1
        """Get default project folder from settings"""
        try:
            if hasattr(self.main_window, 'settings'):
                # Try assists_folder first
                assists_folder = getattr(self.main_window.settings, 'assists_folder', None)
                if assists_folder and os.path.exists(assists_folder):
                    return os.path.join(assists_folder, "IMG_Split")
                
                # Try project_folder for compatibility
                project_folder = self.main_window.settings.get('project_folder')
                if project_folder and os.path.exists(project_folder):
                    return os.path.join(project_folder, "IMG_Split")
            
            # Fallback to Desktop
            return os.path.expanduser("~/Desktop/IMG_Split")
            
        except Exception:
            return os.path.expanduser("~/Desktop/IMG_Split")
    
    def load_project_folder_default(self): #vers 1
        """Load and display project folder info"""
        try:
            default_path = self.get_project_folder_default()
            if "Desktop" not in default_path:
                # We found a project folder
                folder_name = os.path.dirname(default_path)
                self.project_info_label = QLabel(f"📁 Using project folder: {folder_name}")
                self.project_info_label.setStyleSheet("color: #666; font-size: 9pt;")
            else:
                # Using Desktop fallback
                self.project_info_label = QLabel("⚠️ No project folder set - using Desktop")
                self.project_info_label.setStyleSheet("color: #ff9800; font-size: 9pt;")
                
        except Exception:
            self.project_info_label = QLabel("❌ Error loading project settings")
            self.project_info_label.setStyleSheet("color: #f44336; font-size: 9pt;")
    
    def browse_ide_file(self): #vers 1
        """Browse for IDE file"""
        file_path, _ = QFileDialog.getOpenFileName(
            self, "Select IDE File", "", "IDE Files (*.ide);;All Files (*)"
        )
        if file_path:
            self.ide_path_input.setText(file_path)
    
    def browse_output_dir(self): #vers 1
        """Browse for output directory"""
        dir_path = QFileDialog.getExistingDirectory(self, "Select Output Directory")
        if dir_path:
            self.output_dir_input.setText(dir_path)
    
    def analyze_ide(self): #vers 3
        """Analyze selected IDE file - DISABLED until IDE editor ready"""
        QMessageBox.information(
            self, 
            "Feature Coming Soon", 
            "IDE analysis will be available when the IDE editor is implemented.\n\nFor now, use 'Group by file type' to organize your split files."
        )
        return
        
        # Future implementation when IDE editor is ready:
        # ide_path = self.ide_path_input.text().strip()
        # if not ide_path or not os.path.exists(ide_path):
        #     QMessageBox.warning(self, "Invalid IDE File", "Please select a valid IDE file")
        #     return
        # 
        # if self.ide_parser.parse_ide_file(ide_path):
        #     ide_lines = sum(len(entries) for entries in self.ide_parser.sections.values())
        #     total_files = len(self.ide_parser.models) * 2  # Estimate DFF + TXD per model
        #     
        #     self.ide_status_label.setText(f"📋 {ide_lines} IDE lines, showing {total_files} files")
        #     self.ide_status_label.setVisible(True)
        #     
        #     # Enable model grouping
        #     self.group_by_model.setEnabled(True)
        #     self.group_by_model.setStyleSheet("")  # Remove gray styling
        ide_path = self.ide_path_input.text().strip()
        if not ide_path or not os.path.exists(ide_path):
            QMessageBox.warning(self, "Invalid IDE File", "Please select a valid IDE file")
            return
        
        try:
            self.progress_log.append("📋 Analyzing IDE file...")
            
            if self.ide_parser.parse_ide_file(ide_path):
                # Show analysis results
                summary = self.ide_parser.get_summary()
                self.analysis_text.setText(summary)
                
                # Show relationships
                self.relationships_list.clear()
                relationships = self.ide_parser.get_model_relationships()
                
                for model, textures in relationships.items():
                    item_text = f"{model} → {', '.join(textures)}"
                    item = QListWidgetItem(item_text)
                    self.relationships_list.addItem(item)
                
                self.progress_log.append(f"✅ IDE analysis complete: {len(self.ide_parser.models)} models found")
                
                # Enable model grouping
                self.group_by_model.setEnabled(True)
                
            else:
                QMessageBox.warning(self, "Parse Error", "Failed to parse IDE file")
                self.progress_log.append("❌ IDE analysis failed")
                
        except Exception as e:
            QMessageBox.critical(self, "Analysis Error", f"Error analyzing IDE file:\n{str(e)}")
            self.progress_log.append(f"❌ IDE analysis error: {str(e)}")
    
    def start_split(self): #vers 1
        """Start the split operation"""
        try:
            # Validate options
            output_dir = self.output_dir_input.text().strip()
            if not output_dir:
                QMessageBox.warning(self, "Invalid Output", "Please select an output directory")
                return
            
            # Create output directory
            os.makedirs(output_dir, exist_ok=True)
            
            # Prepare split options
            split_options = {
                'output_dir': output_dir,
                'output_format': self.output_format.currentText(),
                'group_by_type': self.group_by_type.isChecked(),
                'group_by_model': False,  # Disabled until IDE editor ready
                'create_index': self.create_index.isChecked(),
                'preserve_structure': self.preserve_structure.isChecked(),
                'ide_parser': None  # No IDE parser until feature is ready
            }
            
            # Switch to progress tab
            self.tabs.setCurrentIndex(self.tabs.count() - 1)
            
            # Start split thread
            self.split_thread = SplitThread(self.main_window.current_img, split_options)
            self.split_thread.progress_updated.connect(self.update_progress)
            self.split_thread.split_completed.connect(self.split_finished)
            self.split_thread.start()
            
            # Disable split button
            self.split_btn.setEnabled(False)
            self.split_btn.setText("⏳ Splitting...")
            
        except Exception as e:
            QMessageBox.critical(self, "Split Error", f"Error starting split:\n{str(e)}")
    
    def update_progress(self, progress: int, message: str): #vers 1
        """Update progress display"""
        self.progress_label.setText(f"Progress: {progress}% - {message}")
        self.progress_log.append(message)
        
        # Auto-scroll to bottom
        cursor = self.progress_log.textCursor()
        cursor.movePosition(cursor.MoveOperation.End)
        self.progress_log.setTextCursor(cursor)
    
    def split_finished(self, success: bool, message: str, stats: Dict): #vers 1
        """Handle split completion"""
        self.split_btn.setEnabled(True)
        self.split_btn.setText("🔄 Start Split")
        
        if success:
            result_msg = f"Split completed successfully!\n\n"
            result_msg += f"Files processed: {stats.get('total_files', 0)}\n"
            result_msg += f"Groups created: {stats.get('groups_created', 0)}\n"
            result_msg += f"Output location: {stats.get('output_dir', 'Unknown')}\n"
            
            # Show removal info if files were removed from original
            if stats.get('removed_from_original', 0) > 0:
                result_msg += f"\n⚠️ Removed {stats['removed_from_original']} files from original IMG"
            
            QMessageBox.information(self, "Split Complete", result_msg)
            self.accept()
        else:
            QMessageBox.critical(self, "Split Failed", f"Split operation failed:\n{message}")

class SplitThread(QThread):
    """Background thread for IMG splitting operations"""
    
    progress_updated = pyqtSignal(int, str)
    split_completed = pyqtSignal(bool, str, dict)
    
    def __init__(self, img_file, split_options): #vers 1
        super().__init__()
        self.img_file = img_file
        self.split_options = split_options
        self.stats = {
            'total_files': 0,
            'groups_created': 0,
            'output_dir': split_options['output_dir']
        }
    
    def run(self): #vers 1
        """Run the split operation"""
        try:
            self.progress_updated.emit(0, "Initializing split operation...")
            
            # Analyze files
            self.progress_updated.emit(10, "Analyzing IMG entries...")
            file_groups = self._analyze_files()
            
            # Create groups
            self.progress_updated.emit(30, f"Creating {len(file_groups)} file groups...")
            
            for i, (group_name, files) in enumerate(file_groups.items()):
                progress = 30 + int((i / len(file_groups)) * 60)
                self.progress_updated.emit(progress, f"Processing group: {group_name}")
                
                if not self._create_group(group_name, files):
                    self.split_completed.emit(False, f"Failed to create group: {group_name}", self.stats)
                    return
                
                self.stats['groups_created'] += 1
            
            self.progress_updated.emit(100, "Split operation completed!")
            
            # Handle original IMG modification if requested
            if not self.split_options['preserve_structure']:
                self.progress_updated.emit(100, "Removing split files from original IMG...")
                self._remove_split_files_from_original(file_groups)
            
            self.split_completed.emit(True, "Split completed successfully", self.stats)
            
        except Exception as e:
            self.split_completed.emit(False, str(e), self.stats)
    
    def _analyze_files(self) -> Dict[str, List]: #vers 1
        """Analyze IMG files and create groups"""
        file_groups = {}
        self.stats['total_files'] = len(self.img_file.entries)
        
        for entry in self.img_file.entries:
            group_names = self._determine_groups(entry)
            
            for group_name in group_names:
                if group_name not in file_groups:
                    file_groups[group_name] = []
                file_groups[group_name].append(entry)
        
        return file_groups
    
    def _determine_groups(self, entry) -> List[str]: #vers 1
        """Determine which groups a file belongs to"""
        groups = []
        
        # Get file extension
        file_ext = entry.name.split('.')[-1].upper() if '.' in entry.name else 'UNKNOWN'
        
        # Group by type
        if self.split_options['group_by_type']:
            groups.append(f"type_{file_ext}")
        
        # Group by model (using IDE data)
        if self.split_options['group_by_model'] and self.split_options['ide_parser']:
            model_group = self._find_model_group(entry.name)
            if model_group:
                groups.append(f"model_{model_group}")
        
        # Default group if no specific groups found
        if not groups:
            groups.append("other")
        
        return groups
    
    def _find_model_group(self, filename: str) -> Optional[str]: #vers 1
        """Find model group for a file using IDE data"""
        ide_parser = self.split_options['ide_parser']
        
        # Check if file matches any model
        for model_id, model_data in ide_parser.models.items():
            model_name = model_data['name']
            txd_name = model_data['txd']
            
            # Check for DFF match
            if filename.lower() == f"{model_name.lower()}.dff":
                return model_name
            
            # Check for TXD match
            if filename.lower() == f"{txd_name.lower()}.txd":
                return model_name
        
        return None
    
    def _create_group(self, group_name: str, files: List) -> bool: #vers 1
        """Create a file group based on output format"""
        try:
            output_format = self.split_options['output_format']
            
            if output_format == "Folder Structure":
                return self._create_folder_group(group_name, files)
            elif "IMG v1" in output_format:
                return self._create_img_group(group_name, files, version=1)
            elif "IMG v2" in output_format:
                return self._create_img_group(group_name, files, version=2)
            
            return False
            
        except Exception as e:
            print(f"Error creating group {group_name}: {e}")
            return False
    
    def _create_folder_group(self, group_name: str, files: List) -> bool: #vers 1
        """Create folder-based group"""
        try:
            group_dir = os.path.join(self.split_options['output_dir'], group_name)
            os.makedirs(group_dir, exist_ok=True)
            
            # Extract files to folder
            for entry in files:
                # FIXED: Use correct method to get entry data
                file_data = self._get_entry_data(entry)
                file_path = os.path.join(group_dir, entry.name)
                with open(file_path, 'wb') as f:
                    f.write(file_data)
            
            # Create index file if requested
            if self.split_options['create_index']:
                self._create_index_file(group_dir, files)
            
            return True
            
        except Exception as e:
            print(f"Error creating folder group: {e}")
            return False
    
    def _create_img_group(self, group_name: str, files: List, version: int) -> bool: #vers 2
        """Create IMG file group with version limits"""
        try:
            # Apply version-specific file limits
            max_files_v1 = 32652
            max_files_v2 = 130608  # 4x version 1
            
            max_files = max_files_v1 if version == 1 else max_files_v2
            
            # Use default limits since size controls were removed
            effective_max = max_files  # Use full version limit
            
            if len(files) > effective_max:
                # Split into multiple IMG files if too many files
                return self._create_multiple_img_files(group_name, files, version, effective_max)
            
            # Create single IMG file since within limits
            return self._create_single_img_file(group_name, files, version)
            
        except Exception as e:
            print(f"Error creating IMG group: {e}")
            return False
    
    def _create_multiple_img_files(self, base_name: str, files: List, version: int, max_files: int) -> bool: #vers 1
        """Create multiple IMG files when file count exceeds limits"""
        try:
            # Split files into chunks
            file_chunks = [files[i:i + max_files] for i in range(0, len(files), max_files)]
            
            for i, chunk in enumerate(file_chunks):
                chunk_name = f"{base_name}_part{i+1:02d}" if len(file_chunks) > 1 else base_name
                
                if not self._create_single_img_file(chunk_name, chunk, version):
                    return False
            
            return True
            
        except Exception as e:
            print(f"Error creating multiple IMG files: {e}")
            return False
    
    def _create_single_img_file(self, img_name: str, files: List, version: int) -> bool: #vers 2
        """Create a single IMG file with proper structure"""
        try:
            img_path = os.path.join(self.split_options['output_dir'], f"{img_name}.img")
            
            if version == 1:
                return self._create_version1_img(img_path, img_name, files)
            else:
                return self._create_version2_img(img_path, files)
            
        except Exception as e:
            print(f"Error creating single IMG file {img_name}: {e}")
            return False
    
    def _create_version1_img(self, img_path: str, img_name: str, files: List) -> bool: #vers 3
        """Create Version 1 IMG with proper entry detection - FIXED"""
        try:
            from apps.methods.detect_file_type_and_version import create_enhanced_entry
            
            dir_path = img_path.replace('.img', '.dir')
            
            # Collect entry data and create enhanced entries
            enhanced_entries = []
            entry_data_list = []
            current_offset = 0
            
            for file_entry in files:
                file_data = self._get_entry_data(file_entry)
                
                # Skip empty files
                if not file_data:
                    print(f"Warning: Skipping empty file {file_entry.name}")
                    continue
                
                # Create enhanced entry with detection capabilities
                enhanced_entry = create_enhanced_entry(
                    name=file_entry.name,
                    offset=current_offset,
                    size=len(file_data),
                    file_data=file_data,
                    img_file=None  # No parent IMG yet
                )
                
                enhanced_entries.append(enhanced_entry)
                entry_data_list.append(file_data)
                
                # Align to sector boundary (2048 bytes)
                aligned_size = ((len(file_data) + 2047) // 2048) * 2048
                current_offset += aligned_size
            
            if len(enhanced_entries) == 0:
                print("Error: No valid entries to write")
                return False
            
            # Write DIR file
            with open(dir_path, 'wb') as f:
                for entry in enhanced_entries:
                    # Convert to sectors
                    offset_sectors = entry.offset // 2048
                    size_sectors = ((entry.size + 2047) // 2048)
                    
                    # Pack entry: offset(4), size(4), name(24)
                    entry_data = struct.pack('<II', offset_sectors, size_sectors)
                    name_bytes = entry.name.encode('ascii')[:24].ljust(24, b'\x00')
                    entry_data += name_bytes
                    
                    f.write(entry_data)
            
            # Write IMG file
            with open(img_path, 'wb') as f:
                for i, data in enumerate(entry_data_list):
                    f.seek(enhanced_entries[i].offset)
                    f.write(data)
                    
                    # Pad to sector boundary
                    current_pos = f.tell()
                    sector_end = ((current_pos + 2047) // 2048) * 2048
                    if current_pos < sector_end:
                        f.write(b'\x00' * (sector_end - current_pos))
            
            dir_size = os.path.getsize(dir_path)
            img_size = os.path.getsize(img_path)
            print(f"✅ Created Version 1 IMG: {len(enhanced_entries)} entries, DIR: {dir_size:,} bytes, IMG: {img_size:,} bytes")
            return True
            
        except Exception as e:
            print(f"Error creating Version 1 IMG: {e}")
            import traceback
            traceback.print_exc()
            return False
    
    def _create_version2_img(self, img_path: str, files: List) -> bool: #vers 3
        """Create Version 2 IMG with proper entry detection - FIXED"""
        try:
            from apps.methods.detect_file_type_and_version import create_enhanced_entry
            
            entry_count = len(files)
            
            # Calculate directory size: VER2(4) + count(4) + entries(32 each)
            header_size = 8  # VER2 + entry count
            directory_size = header_size + (entry_count * 32)
            
            # Collect entry data and create enhanced entries
            enhanced_entries = []
            entry_data_list = []
            current_offset = directory_size
            
            for file_entry in files:
                file_data = self._get_entry_data(file_entry)
                
                # Skip empty files
                if not file_data:
                    print(f"Warning: Skipping empty file {file_entry.name}")
                    continue
                
                # Create enhanced entry with detection capabilities
                enhanced_entry = create_enhanced_entry(
                    name=file_entry.name,
                    offset=current_offset,
                    size=len(file_data),
                    file_data=file_data,
                    img_file=None  # No parent IMG yet
                )
                
                enhanced_entries.append(enhanced_entry)
                entry_data_list.append(file_data)
                
                # Align to sector boundary (2048 bytes)
                aligned_size = ((len(file_data) + 2047) // 2048) * 2048
                current_offset += aligned_size
            
            # Update entry count to match actual valid entries
            entry_count = len(enhanced_entries)
            
            if entry_count == 0:
                print("Error: No valid entries to write")
                return False
            
            # Write IMG file
            with open(img_path, 'wb') as f:
                # Write VER2 header
                f.write(b'VER2')  # Version identifier
                f.write(struct.pack('<I', entry_count))  # Entry count
                
                # Write directory
                for entry in enhanced_entries:
                    # Convert to sectors
                    offset_sectors = entry.offset // 2048
                    size_sectors = ((entry.size + 2047) // 2048)
                    
                    # Pack entry: offset(4), size(4), name(24)
                    entry_data = struct.pack('<II', offset_sectors, size_sectors)
                    name_bytes = entry.name.encode('ascii')[:24].ljust(24, b'\x00')
                    entry_data += name_bytes
                    
                    f.write(entry_data)
                
                # Write file data
                for i, data in enumerate(entry_data_list):
                    f.seek(enhanced_entries[i].offset)
                    f.write(data)
                    
                    # Pad to sector boundary
                    current_pos = f.tell()
                    sector_end = ((current_pos + 2047) // 2048) * 2048
                    if current_pos < sector_end:
                        f.write(b'\x00' * (sector_end - current_pos))
            
            print(f"✅ Created Version 2 IMG: {entry_count} entries, {os.path.getsize(img_path):,} bytes")
            return True
            
        except Exception as e:
            print(f"Error creating Version 2 IMG: {e}")
            import traceback
            traceback.print_exc()
            return False
    
    def _create_index_file(self, group_dir: str, files: List): #vers 1
        """Create index file for a group"""
        try:
            index_path = os.path.join(group_dir, "index.txt")
            
            with open(index_path, 'w', encoding='utf-8') as f:
                f.write(f"Group Index - {os.path.basename(group_dir)}\n")
                f.write(f"Generated by IMG Factory 1.5\n")
                f.write(f"Total files: {len(files)}\n\n")
                
                for entry in files:
                    file_data = self._get_entry_data(entry)
                    f.write(f"{entry.name} ({len(file_data):,} bytes)\n")
            
        except Exception as e:
            print(f"Error creating index file: {e}")
    
    def _get_entry_data(self, entry) -> bytes: #vers 1
        """Get entry data using correct method"""
        try:
            # Try cached data first
            if hasattr(entry, '_cached_data') and entry._cached_data:
                return entry._cached_data
            
            # Try read_data method
            if hasattr(entry, 'read_data'):
                return entry.read_data()
            
            # Try get_data method
            if hasattr(entry, 'get_data'):
                return entry.get_data()
            
            # Try direct data attribute
            if hasattr(entry, 'data'):
                return entry.data
            
            # Use IMG file's read_entry_data method
            if hasattr(entry, 'img_file') and hasattr(entry.img_file, 'read_entry_data'):
                return entry.img_file.read_entry_data(entry)
            
            # Fallback: empty data
            print(f"Warning: Could not read data for entry {entry.name}")
            return b''
            
        except Exception as e:
            print(f"Error reading entry data for {entry.name}: {e}")
            return b''

__all__ = [
    'split_img',
    'split_img_via', 
    'integrate_split_functions',
    'IDEParser',
    'SplitDialog',
    'SplitThread'
]
