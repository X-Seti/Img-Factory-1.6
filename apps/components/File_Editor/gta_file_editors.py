#this belongs in components.File_Editor.gta_file_editors.py - version 1
# X-Seti - July10 2025 - Img Factory 1.5
# GTA file editors for IDE, IPL, and DAT files with object ID management

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTabWidget, QTextEdit, 
    QTableWidget, QTableWidgetItem, QPushButton, QLabel, QLineEdit,
    QComboBox, QSpinBox, QGroupBox, QSplitter, QHeaderView,
    QAbstractItemView, QMessageBox, QFileDialog, QCheckBox,
    QTreeWidget, QTreeWidgetItem, QMenu, QFrame
)
from PyQt6.QtCore import Qt, pyqtSignal, QTimer
from PyQt6.QtGui import QAction, QFont, QColor
from typing import Dict, List, Set, Tuple, Optional
import re
import os

class IDEFileEditor(QDialog):
    """Editor for GTA IDE (Item Definition) files"""
    
    file_modified = pyqtSignal(bool)
    
    def __init__(self, parent=None, file_data: bytes = None, filename: str = ""):
        super().__init__(parent)
        self.file_data = file_data
        self.filename = filename
        self.parsed_data = {}
        self.used_ids = set()
        self.is_modified = False
        
        self.setWindowTitle(f"IDE Editor - {filename}")
        self.setMinimumSize(900, 600)
        self.setup_ui()
        
        if file_data:
            self.load_ide_data()
    
    def setup_ui(self):
        """Setup the IDE editor UI"""
        layout = QVBoxLayout(self)
        
        # Toolbar
        toolbar_layout = QHBoxLayout()
        
        self.save_btn = QPushButton("💾 Save")
        self.save_btn.clicked.connect(self.save_file)
        toolbar_layout.addWidget(self.save_btn)
        
        self.reload_btn = QPushButton("🔄 Reload")
        self.reload_btn.clicked.connect(self.reload_file)
        toolbar_layout.addWidget(self.reload_btn)
        
        toolbar_layout.addStretch()
        
        # ID Manager button
        self.id_manager_btn = QPushButton("🆔 ID Manager")
        self.id_manager_btn.clicked.connect(self.show_id_manager)
        toolbar_layout.addWidget(self.id_manager_btn)
        
        # Validate button
        self.validate_btn = QPushButton("✅ Validate")
        self.validate_btn.clicked.connect(self.validate_ide)
        toolbar_layout.addWidget(self.validate_btn)
        
        layout.addLayout(toolbar_layout)
        
        # Main content - splitter
        splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Left side - sections tree and table
        left_widget = QTabWidget()
        
        # Sections tab
        self.sections_tab = self.create_sections_tab()
        left_widget.addTab(self.sections_tab, "📋 Sections")
        
        # Objects tab  
        self.objects_tab = self.create_objects_tab()
        left_widget.addTab(self.objects_tab, "🏗️ Objects")
        
        # Vehicles tab
        self.vehicles_tab = self.create_vehicles_tab()
        left_widget.addTab(self.vehicles_tab, "🚗 Vehicles")
        
        splitter.addWidget(left_widget)
        
        # Right side - raw text editor
        right_widget = QGroupBox("📝 Raw Text Editor")
        right_layout = QVBoxLayout(right_widget)
        
        self.text_editor = QTextEdit()
        self.text_editor.setFont(QFont("Consolas", 9))
        self.text_editor.textChanged.connect(self.on_text_changed)
        right_layout.addWidget(self.text_editor)
        
        # Text editor controls
        text_controls = QHBoxLayout()
        
        self.find_input = QLineEdit()
        self.find_input.setPlaceholderText("Find...")
        text_controls.addWidget(QLabel("Find:"))
        text_controls.addWidget(self.find_input)
        
        find_btn = QPushButton("🔍")
        find_btn.clicked.connect(self.find_text)
        text_controls.addWidget(find_btn)
        
        right_layout.addLayout(text_controls)
        
        splitter.addWidget(right_widget)
        splitter.setSizes([600, 300])
        
        layout.addWidget(splitter)
        
        # Status bar
        self.status_label = QLabel("Ready")
        layout.addWidget(self.status_label)
    
    def create_sections_tab(self):
        """Create sections overview tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Sections tree
        self.sections_tree = QTreeWidget()
        self.sections_tree.setHeaderLabels(["Section", "Count", "ID Range"])
        self.sections_tree.itemDoubleClicked.connect(self.jump_to_section)
        layout.addWidget(self.sections_tree)
        
        return widget
    
    def create_objects_tab(self):
        """Create objects table tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Filter controls
        filter_layout = QHBoxLayout()
        
        filter_layout.addWidget(QLabel("Filter:"))
        self.obj_filter_combo = QComboBox()
        self.obj_filter_combo.addItems(["All Objects", "By ID Range", "By Model Name"])
        filter_layout.addWidget(self.obj_filter_combo)
        
        self.obj_filter_input = QLineEdit()
        self.obj_filter_input.setPlaceholderText("Enter ID range (e.g., 1000-2000) or model name")
        filter_layout.addWidget(self.obj_filter_input)
        
        filter_btn = QPushButton("Filter")
        filter_btn.clicked.connect(self.filter_objects)
        filter_layout.addWidget(filter_btn)
        
        layout.addLayout(filter_layout)
        
        # Objects table
        self.objects_table = QTableWidget()
        self.objects_table.setColumnCount(6)
        self.objects_table.setHorizontalHeaderLabels([
            "ID", "Model", "Texture", "Draw Dist", "Flags", "Actions"
        ])
        
        header = self.objects_table.horizontalHeader()
        header.setStretchLastSection(True)
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)
        
        self.objects_table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.objects_table.customContextMenuRequested.connect(self.show_object_context_menu)
        
        layout.addWidget(self.objects_table)
        
        return widget
    
    def create_vehicles_tab(self):
        """Create vehicles table tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Vehicles table
        self.vehicles_table = QTableWidget()
        self.vehicles_table.setColumnCount(8)
        self.vehicles_table.setHorizontalHeaderLabels([
            "ID", "Model", "Texture", "Type", "Handling", "Game", "Class", "Frequency"
        ])
        
        header = self.vehicles_table.horizontalHeader()
        header.setStretchLastSection(True)
        
        layout.addWidget(self.vehicles_table)
        
        return widget
    
    def load_ide_data(self):
        """Parse and load IDE file data"""
        try:
            # Decode file data
            content = self.file_data.decode('ascii', errors='ignore')
            self.text_editor.setPlainText(content)
            
            # Parse sections
            self.parse_ide_content(content)
            
            # Update UI
            self.update_sections_tree()
            self.update_objects_table()
            self.update_vehicles_table()
            
            self.status_label.setText(f"Loaded {len(self.used_ids)} objects, {len(self.parsed_data)} sections")
            
        except Exception as e:
            QMessageBox.critical(self, "Load Error", f"Failed to load IDE file:\n{str(e)}")
    
    def parse_ide_content(self, content: str):
        """Parse IDE file content into structured data"""
        self.parsed_data = {}
        self.used_ids = set()
        
        lines = content.split('\n')
        current_section = None
        
        for line_num, line in enumerate(lines):
            line = line.strip()
            
            # Skip empty lines and comments
            if not line or line.startswith('#'):
                continue
            
            # Check for section headers
            section_headers = ['objs', 'tobj', 'weap', 'hier', 'anim', 'cars', 'peds']
            if line.lower() in section_headers:
                current_section = line.lower()
                self.parsed_data[current_section] = []
                continue
            
            # End section
            if line.lower() == 'end':
                current_section = None
                continue
            
            # Parse section data
            if current_section:
                try:
                    parsed_line = self.parse_section_line(current_section, line, line_num)
                    if parsed_line:
                        self.parsed_data[current_section].append(parsed_line)
                        
                        # Extract ID if present
                        if 'id' in parsed_line:
                            self.used_ids.add(parsed_line['id'])
                            
                except Exception as e:
                    print(f"Error parsing line {line_num}: {line} - {e}")
    
    def parse_section_line(self, section: str, line: str, line_num: int) -> Optional[Dict]:
        """Parse a line within a specific section"""
        parts = line.split(',')
        
        if section == 'objs' and len(parts) >= 5:
            # Object definition: ID, ModelName, TxdName, DrawDistance, Flags, ...
            try:
                return {
                    'id': int(parts[0].strip()),
                    'model': parts[1].strip(),
                    'texture': parts[2].strip(),
                    'draw_dist': float(parts[3].strip()),
                    'flags': int(parts[4].strip()) if parts[4].strip() else 0,
                    'line_num': line_num,
                    'raw_line': line
                }
            except ValueError:
                return None
        
        elif section == 'cars' and len(parts) >= 8:
            # Vehicle definition: ID, ModelName, TxdName, Type, HandlingId, GameName, Class, Frequency
            try:
                return {
                    'id': int(parts[0].strip()),
                    'model': parts[1].strip(),
                    'texture': parts[2].strip(),
                    'type': parts[3].strip(),
                    'handling': parts[4].strip(),
                    'game_name': parts[5].strip(),
                    'class': int(parts[6].strip()) if parts[6].strip() else 0,
                    'frequency': int(parts[7].strip()) if parts[7].strip() else 0,
                    'line_num': line_num,
                    'raw_line': line
                }
            except ValueError:
                return None
        
        # For other sections, store raw data for now
        return {
            'raw_line': line,
            'line_num': line_num
        }
    
    def update_sections_tree(self):
        """Update the sections tree view"""
        self.sections_tree.clear()
        
        for section_name, items in self.parsed_data.items():
            # Get ID range for this section
            ids_in_section = []
            for item in items:
                if 'id' in item:
                    ids_in_section.append(item['id'])
            
            id_range = ""
            if ids_in_section:
                min_id = min(ids_in_section)
                max_id = max(ids_in_section)
                id_range = f"{min_id}-{max_id}" if min_id != max_id else str(min_id)
            
            # Create tree item
            section_item = QTreeWidgetItem([
                section_name.upper(),
                str(len(items)),
                id_range
            ])
            
            self.sections_tree.addTopLevelItem(section_item)
        
        self.sections_tree.expandAll()
    
    def update_objects_table(self):
        """Update the objects table"""
        if 'objs' not in self.parsed_data:
            return
        
        objects = self.parsed_data['objs']
        self.objects_table.setRowCount(len(objects))
        
        for row, obj in enumerate(objects):
            self.objects_table.setItem(row, 0, QTableWidgetItem(str(obj.get('id', ''))))
            self.objects_table.setItem(row, 1, QTableWidgetItem(obj.get('model', '')))
            self.objects_table.setItem(row, 2, QTableWidgetItem(obj.get('texture', '')))
            self.objects_table.setItem(row, 3, QTableWidgetItem(str(obj.get('draw_dist', ''))))
            self.objects_table.setItem(row, 4, QTableWidgetItem(str(obj.get('flags', ''))))
            
            # Actions column - edit/delete buttons
            actions_widget = QWidget()
            actions_layout = QHBoxLayout(actions_widget)
            actions_layout.setContentsMargins(2, 2, 2, 2)
            
            edit_btn = QPushButton("✏️")
            edit_btn.setMaximumWidth(25)
            edit_btn.clicked.connect(lambda checked, r=row: self.edit_object(r))
            actions_layout.addWidget(edit_btn)
            
            delete_btn = QPushButton("🗑️")
            delete_btn.setMaximumWidth(25)
            delete_btn.clicked.connect(lambda checked, r=row: self.delete_object(r))
            actions_layout.addWidget(delete_btn)
            
            self.objects_table.setCellWidget(row, 5, actions_widget)
    
    def update_vehicles_table(self):
        """Update the vehicles table"""
        if 'cars' not in self.parsed_data:
            return
        
        vehicles = self.parsed_data['cars']
        self.vehicles_table.setRowCount(len(vehicles))
        
        for row, vehicle in enumerate(vehicles):
            self.vehicles_table.setItem(row, 0, QTableWidgetItem(str(vehicle.get('id', ''))))
            self.vehicles_table.setItem(row, 1, QTableWidgetItem(vehicle.get('model', '')))
            self.vehicles_table.setItem(row, 2, QTableWidgetItem(vehicle.get('texture', '')))
            self.vehicles_table.setItem(row, 3, QTableWidgetItem(vehicle.get('type', '')))
            self.vehicles_table.setItem(row, 4, QTableWidgetItem(vehicle.get('handling', '')))
            self.vehicles_table.setItem(row, 5, QTableWidgetItem(vehicle.get('game_name', '')))
            self.vehicles_table.setItem(row, 6, QTableWidgetItem(str(vehicle.get('class', ''))))
            self.vehicles_table.setItem(row, 7, QTableWidgetItem(str(vehicle.get('frequency', ''))))

class IDManagerDialog(QDialog):
    """Dialog for managing object IDs and finding free ranges"""
    
    def __init__(self, used_ids: Set[int], parent=None):
        super().__init__(parent)
        self.used_ids = used_ids
        self.setWindowTitle("🆔 Object ID Manager")
        self.setMinimumSize(600, 500)
        self.setup_ui()
        self.analyze_ids()
    
    def setup_ui(self):
        """Setup ID manager UI"""
        layout = QVBoxLayout(self)
        
        # Summary info
        summary_layout = QHBoxLayout()
        
        self.total_ids_label = QLabel()
        summary_layout.addWidget(self.total_ids_label)
        
        summary_layout.addStretch()
        
        self.range_info_label = QLabel()
        summary_layout.addWidget(self.range_info_label)
        
        layout.addLayout(summary_layout)
        
        # Tab widget
        tab_widget = QTabWidget()
        
        # Used IDs tab
        self.used_ids_tab = self.create_used_ids_tab()
        tab_widget.addTab(self.used_ids_tab, "📋 Used IDs")
        
        # Free ranges tab
        self.free_ranges_tab = self.create_free_ranges_tab()
        tab_widget.addTab(self.free_ranges_tab, "🆓 Free Ranges")
        
        # ID finder tab
        self.id_finder_tab = self.create_id_finder_tab()
        tab_widget.addTab(self.id_finder_tab, "🔍 Find IDs")
        
        layout.addWidget(tab_widget)
        
        # Close button
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        layout.addWidget(close_btn)
    
    def create_used_ids_tab(self):
        """Create used IDs display tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Used IDs table
        self.used_ids_table = QTableWidget()
        self.used_ids_table.setColumnCount(3)
        self.used_ids_table.setHorizontalHeaderLabels(["ID", "Range", "Gap to Next"])
        
        header = self.used_ids_table.horizontalHeader()
        header.setStretchLastSection(True)
        
        layout.addWidget(self.used_ids_table)
        
        return widget
    
    def create_free_ranges_tab(self):
        """Create free ranges tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Info label
        info_label = QLabel("🆓 Available ID ranges for new objects:")
        layout.addWidget(info_label)
        
        # Free ranges table
        self.free_ranges_table = QTableWidget()
        self.free_ranges_table.setColumnCount(4)
        self.free_ranges_table.setHorizontalHeaderLabels(["Start ID", "End ID", "Count", "Recommended For"])
        
        header = self.free_ranges_table.horizontalHeader()
        header.setStretchLastSection(True)
        
        layout.addWidget(self.free_ranges_table)
        
        return widget
    
    def create_id_finder_tab(self):
        """Create ID finder tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Find controls
        find_layout = QHBoxLayout()
        
        find_layout.addWidget(QLabel("Find:"))
        
        self.find_count_spin = QSpinBox()
        self.find_count_spin.setRange(1, 1000)
        self.find_count_spin.setValue(1)
        find_layout.addWidget(self.find_count_spin)
        
        find_layout.addWidget(QLabel("free IDs starting from:"))
        
        self.find_start_spin = QSpinBox()
        self.find_start_spin.setRange(0, 65535)
        self.find_start_spin.setValue(1000)
        find_layout.addWidget(self.find_start_spin)
        
        find_btn = QPushButton("🔍 Find")
        find_btn.clicked.connect(self.find_free_ids)
        find_layout.addWidget(find_btn)
        
        find_layout.addStretch()
        
        layout.addLayout(find_layout)
        
        # Results
        self.find_results = QTextEdit()
        self.find_results.setMaximumHeight(200)
        layout.addWidget(self.find_results)
        
        # Quick suggestions
        suggestions_group = QGroupBox("💡 Quick Suggestions")
        suggestions_layout = QVBoxLayout(suggestions_group)
        
        suggestion_buttons = [
            ("🏗️ Objects (1000-9999)", lambda: self.suggest_range(1000, 9999, "Objects")),
            ("🚗 Vehicles (400-611)", lambda: self.suggest_range(400, 611, "Vehicles")),
            ("🔫 Weapons (300-399)", lambda: self.suggest_range(300, 399, "Weapons")),
            ("👤 Peds (1-299)", lambda: self.suggest_range(1, 299, "Pedestrians")),
        ]
        
        for text, callback in suggestion_buttons:
            btn = QPushButton(text)
            btn.clicked.connect(callback)
            suggestions_layout.addWidget(btn)
        
        layout.addWidget(suggestions_group)
        
        return widget
    
    def analyze_ids(self):
        """Analyze used IDs and update displays"""
        if not self.used_ids:
            self.total_ids_label.setText("No IDs used")
            self.range_info_label.setText("")
            return
        
        sorted_ids = sorted(self.used_ids)
        min_id = min(sorted_ids)
        max_id = max(sorted_ids)
        
        self.total_ids_label.setText(f"Total IDs used: {len(self.used_ids)}")
        self.range_info_label.setText(f"Range: {min_id} - {max_id}")
        
        # Update used IDs table
        self.update_used_ids_table(sorted_ids)
        
        # Update free ranges table
        self.update_free_ranges_table(sorted_ids)
    
    def update_used_ids_table(self, sorted_ids: List[int]):
        """Update the used IDs table"""
        self.used_ids_table.setRowCount(len(sorted_ids))
        
        for i, id_val in enumerate(sorted_ids):
            self.used_ids_table.setItem(i, 0, QTableWidgetItem(str(id_val)))
            
            # Find range this ID belongs to
            range_start = id_val
            range_end = id_val
            j = i
            while j + 1 < len(sorted_ids) and sorted_ids[j + 1] == sorted_ids[j] + 1:
                j += 1
                range_end = sorted_ids[j]
            
            if range_start == range_end:
                range_text = "Single"
            else:
                range_text = f"{range_start}-{range_end}"
            
            self.used_ids_table.setItem(i, 1, QTableWidgetItem(range_text))
            
            # Gap to next ID
            if i + 1 < len(sorted_ids):
                gap = sorted_ids[i + 1] - id_val - 1
                gap_text = str(gap) if gap > 0 else "Consecutive"
            else:
                gap_text = "Last ID"
            
            self.used_ids_table.setItem(i, 2, QTableWidgetItem(gap_text))
    
    def update_free_ranges_table(self, sorted_ids: List[int]):
        """Update the free ranges table"""
        free_ranges = []
        
        # Find gaps in used IDs
        for i in range(len(sorted_ids) - 1):
            current_id = sorted_ids[i]
            next_id = sorted_ids[i + 1]
            
            if next_id - current_id > 1:
                start_free = current_id + 1
                end_free = next_id - 1
                count = end_free - start_free + 1
                
                if count >= 1:  # Only show ranges with at least 1 free ID
                    free_ranges.append((start_free, end_free, count))
        
        # Add range before first used ID
        if sorted_ids and sorted_ids[0] > 1:
            free_ranges.insert(0, (1, sorted_ids[0] - 1, sorted_ids[0] - 1))
        
        # Add range after last used ID (up to reasonable limit)
        if sorted_ids and sorted_ids[-1] < 65535:
            start_free = sorted_ids[-1] + 1
            end_free = 65535
            count = end_free - start_free + 1
            free_ranges.append((start_free, end_free, count))
        
        # Update table
        self.free_ranges_table.setRowCount(len(free_ranges))
        
        for i, (start, end, count) in enumerate(free_ranges):
            self.free_ranges_table.setItem(i, 0, QTableWidgetItem(str(start)))
            self.free_ranges_table.setItem(i, 1, QTableWidgetItem(str(end)))
            self.free_ranges_table.setItem(i, 2, QTableWidgetItem(str(count)))
            
            # Recommendation based on ID range
            recommendation = self.get_id_range_recommendation(start, end)
            self.free_ranges_table.setItem(i, 3, QTableWidgetItem(recommendation))
    
    def get_id_range_recommendation(self, start: int, end: int) -> str:
        """Get recommendation for what to use an ID range for"""
        if start <= 299:
            return "👤 Pedestrians"
        elif start <= 399:
            return "🔫 Weapons"
        elif start <= 611:
            return "🚗 Vehicles"
        elif start <= 999:
            return "🏗️ Special Objects"
        elif start <= 9999:
            return "🏗️ World Objects"
        else:
            return "🎯 Custom Objects"
    
    def find_free_ids(self):
        """Find specified number of free consecutive IDs"""
        count_needed = self.find_count_spin.value()
        start_from = self.find_start_spin.value()
        
        sorted_ids = sorted(self.used_ids)
        
        # Find consecutive free IDs
        current_pos = start_from
        found_ranges = []
        
        while current_pos <= 65535 and len(found_ranges) < 10:  # Limit to 10 results
            consecutive_count = 0
            range_start = current_pos
            
            # Count consecutive free IDs from this position
            while current_pos not in self.used_ids and current_pos <= 65535:
                consecutive_count += 1
                if consecutive_count >= count_needed:
                    found_ranges.append((range_start, range_start + count_needed - 1))
                    break
                current_pos += 1
            
            # Skip to next position after used ID
            while current_pos in self.used_ids and current_pos <= 65535:
                current_pos += 1
        
        # Display results
        if found_ranges:
            results_text = f"Found {len(found_ranges)} ranges with {count_needed} consecutive free IDs:\n\n"
            for i, (start, end) in enumerate(found_ranges):
                recommendation = self.get_id_range_recommendation(start, end)
                results_text += f"{i+1}. IDs {start}-{end} ({recommendation})\n"
        else:
            results_text = f"No ranges found with {count_needed} consecutive free IDs starting from {start_from}"
        
        self.find_results.setPlainText(results_text)
    
    def suggest_range(self, range_start: int, range_end: int, object_type: str):
        """Suggest free IDs within a specific range"""
        free_in_range = []
        
        for id_val in range(range_start, range_end + 1):
            if id_val not in self.used_ids:
                free_in_range.append(id_val)
        
        if free_in_range:
            # Group consecutive IDs
            ranges = []
            current_start = free_in_range[0]
            current_end = free_in_range[0]
            
            for i in range(1, len(free_in_range)):
                if free_in_range[i] == current_end + 1:
                    current_end = free_in_range[i]
                else:
                    ranges.append((current_start, current_end))
                    current_start = free_in_range[i]
                    current_end = free_in_range[i]
            
            ranges.append((current_start, current_end))
            
            # Format results
            results_text = f"Free IDs for {object_type} ({range_start}-{range_end}):\n\n"
            results_text += f"Total free IDs: {len(free_in_range)}\n\n"
            
            results_text += "Available ranges:\n"
            for start, end in ranges[:20]:  # Show first 20 ranges
                if start == end:
                    results_text += f"• {start}\n"
                else:
                    results_text += f"• {start}-{end} ({end-start+1} IDs)\n"
            
            if len(ranges) > 20:
                results_text += f"... and {len(ranges)-20} more ranges\n"
        else:
            results_text = f"No free IDs available in {object_type} range ({range_start}-{range_end})"
        
        self.find_results.setPlainText(results_text)

# Add methods to IDEFileEditor class
def show_id_manager(self):
    """Show the ID manager dialog"""
    dialog = IDManagerDialog(self.used_ids, self)
    dialog.exec()

def jump_to_section(self, item, column):
    """Jump to section in text editor"""
    section_name = item.text(0).lower()
    content = self.text_editor.toPlainText()
    
    # Find section in text
    lines = content.split('\n')
    for i, line in enumerate(lines):
        if line.strip().lower() == section_name:
            cursor = self.text_editor.textCursor()
            cursor.movePosition(cursor.MoveOperation.Start)
            for _ in range(i):
                cursor.movePosition(cursor.MoveOperation.Down)
            self.text_editor.setTextCursor(cursor)
            self.text_editor.setFocus()
            break

def filter_objects(self):
    """Filter objects table based on criteria"""
    filter_type = self.obj_filter_combo.currentText()
    filter_text = self.obj_filter_input.text().strip()
    
    if not filter_text:
        # Show all rows
        for row in range(self.objects_table.rowCount()):
            self.objects_table.setRowHidden(row, False)
        return
    
    hidden_count = 0
    
    for row in range(self.objects_table.rowCount()):
        should_hide = True
        
        if filter_type == "By ID Range":
            # Parse ID range like "1000-2000"
            try:
                id_item = self.objects_table.item(row, 0)
                if id_item:
                    obj_id = int(id_item.text())
                    
                    if '-' in filter_text:
                        start_id, end_id = map(int, filter_text.split('-'))
                        should_hide = not (start_id <= obj_id <= end_id)
                    else:
                        target_id = int(filter_text)
                        should_hide = obj_id != target_id
            except ValueError:
                should_hide = True
                
        elif filter_type == "By Model Name":
            # Filter by model name
            model_item = self.objects_table.item(row, 1)
            if model_item:
                model_name = model_item.text().lower()
                should_hide = filter_text.lower() not in model_name
        
        self.objects_table.setRowHidden(row, should_hide)
        if should_hide:
            hidden_count += 1
    
    visible_count = self.objects_table.rowCount() - hidden_count
    self.status_label.setText(f"Showing {visible_count} of {self.objects_table.rowCount()} objects")

def show_object_context_menu(self, position):
    """Show context menu for objects table"""
    item = self.objects_table.itemAt(position)
    if not item:
        return
    
    row = item.row()
    menu = QMenu(self)
    
    # Edit object
    edit_action = QAction("✏️ Edit Object", self)
    edit_action.triggered.connect(lambda: self.edit_object(row))
    menu.addAction(edit_action)
    
    # Duplicate object
    duplicate_action = QAction("📋 Duplicate Object", self)
    duplicate_action.triggered.connect(lambda: self.duplicate_object(row))
    menu.addAction(duplicate_action)
    
    menu.addSeparator()
    
    # Delete object
    delete_action = QAction("🗑️ Delete Object", self)
    delete_action.triggered.connect(lambda: self.delete_object(row))
    menu.addAction(delete_action)
    
    menu.addSeparator()
    
    # Go to line in text editor
    goto_action = QAction("🔍 Go to Line in Editor", self)
    goto_action.triggered.connect(lambda: self.goto_object_line(row))
    menu.addAction(goto_action)
    
    menu.exec(self.objects_table.mapToGlobal(position))

def edit_object(self, row):
    """Edit object properties"""
    if 'objs' not in self.parsed_data or row >= len(self.parsed_data['objs']):
        return
    
    obj = self.parsed_data['objs'][row]
    dialog = ObjectEditDialog(obj, self.used_ids, self)
    
    if dialog.exec() == QDialog.DialogCode.Accepted:
        # Update object data
        updated_obj = dialog.get_object_data()
        self.parsed_data['objs'][row] = updated_obj
        
        # Update table
        self.update_objects_table()
        
        # Update text editor
        self.regenerate_text_content()
        
        self.mark_modified()

def duplicate_object(self, row):
    """Duplicate an object with new ID"""
    if 'objs' not in self.parsed_data or row >= len(self.parsed_data['objs']):
        return
    
    original_obj = self.parsed_data['objs'][row].copy()
    
    # Find next available ID
    new_id = self.find_next_free_id(original_obj['id'])
    original_obj['id'] = new_id
    
    # Open edit dialog for the duplicate
    dialog = ObjectEditDialog(original_obj, self.used_ids, self)
    dialog.setWindowTitle("Duplicate Object")
    
    if dialog.exec() == QDialog.DialogCode.Accepted:
        # Add new object
        new_obj = dialog.get_object_data()
        self.parsed_data['objs'].append(new_obj)
        self.used_ids.add(new_obj['id'])
        
        # Update displays
        self.update_objects_table()
        self.regenerate_text_content()
        self.mark_modified()

def delete_object(self, row):
    """Delete an object"""
    if 'objs' not in self.parsed_data or row >= len(self.parsed_data['objs']):
        return
    
    obj = self.parsed_data['objs'][row]
    
    reply = QMessageBox.question(self, "Delete Object", 
                                f"Delete object ID {obj['id']} ({obj['model']})?\n\nThis cannot be undone.",
                                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
    
    if reply == QMessageBox.StandardButton.Yes:
        # Remove from data
        removed_obj = self.parsed_data['objs'].pop(row)
        self.used_ids.discard(removed_obj['id'])
        
        # Update displays
        self.update_objects_table()
        self.regenerate_text_content()
        self.mark_modified()

def goto_object_line(self, row):
    """Go to object line in text editor"""
    if 'objs' not in self.parsed_data or row >= len(self.parsed_data['objs']):
        return
    
    obj = self.parsed_data['objs'][row]
    line_num = obj.get('line_num', 0)
    
    # Move cursor to line
    cursor = self.text_editor.textCursor()
    cursor.movePosition(cursor.MoveOperation.Start)
    for _ in range(line_num):
        cursor.movePosition(cursor.MoveOperation.Down)
    
    # Select the line
    cursor.select(cursor.SelectionType.LineUnderCursor)
    self.text_editor.setTextCursor(cursor)
    self.text_editor.setFocus()

def find_next_free_id(self, start_id):
    """Find next available ID starting from given ID"""
    current_id = start_id + 1
    while current_id in self.used_ids and current_id < 65536:
        current_id += 1
    return current_id if current_id < 65536 else start_id

def regenerate_text_content(self):
    """Regenerate text content from parsed data"""
    lines = []
    
    for section_name, items in self.parsed_data.items():
        lines.append(section_name)
        
        for item in items:
            if section_name == 'objs':
                # Regenerate object line
                line = f"{item['id']}, {item['model']}, {item['texture']}, {item['draw_dist']}, {item['flags']}"
                lines.append(line)
            elif section_name == 'cars':
                # Regenerate vehicle line
                line = f"{item['id']}, {item['model']}, {item['texture']}, {item['type']}, {item['handling']}, {item['game_name']}, {item['class']}, {item['frequency']}"
                lines.append(line)
            else:
                # Use raw line for other sections
                lines.append(item.get('raw_line', ''))
        
        lines.append('end')
        lines.append('')  # Empty line between sections
    
    self.text_editor.setPlainText('\n'.join(lines))

def find_text(self):
    """Find text in editor"""
    search_text = self.find_input.text()
    if not search_text:
        return
    
    cursor = self.text_editor.textCursor()
    found_cursor = self.text_editor.document().find(search_text, cursor)
    
    if not found_cursor.isNull():
        self.text_editor.setTextCursor(found_cursor)
    else:
        # Search from beginning
        found_cursor = self.text_editor.document().find(search_text)
        if not found_cursor.isNull():
            self.text_editor.setTextCursor(found_cursor)
        else:
            self.status_label.setText(f"Text '{search_text}' not found")

def on_text_changed(self):
    """Handle text editor changes"""
    if not self.is_modified:
        self.mark_modified()

def mark_modified(self):
    """Mark file as modified"""
    self.is_modified = True
    self.setWindowTitle(f"IDE Editor - {self.filename} *")
    self.file_modified.emit(True)

def save_file(self):
    """Save the IDE file"""
    try:
        content = self.text_editor.toPlainText()
        
        if self.filename:
            # Save to original file or export
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Save IDE File", self.filename, "IDE Files (*.ide);;All Files (*)"
            )
        else:
            file_path, _ = QFileDialog.getSaveFileName(
                self, "Save IDE File", "untitled.ide", "IDE Files (*.ide);;All Files (*)"
            )
        
        if file_path:
            with open(file_path, 'w', encoding='ascii', errors='ignore') as f:
                f.write(content)
            
            self.is_modified = False
            self.setWindowTitle(f"IDE Editor - {os.path.basename(file_path)}")
            self.status_label.setText(f"Saved to {file_path}")
            self.file_modified.emit(False)
    
    except Exception as e:
        QMessageBox.critical(self, "Save Error", f"Failed to save file:\n{str(e)}")

def reload_file(self):
    """Reload the original file"""
    if self.is_modified:
        reply = QMessageBox.question(self, "Reload File", 
                                    "File has been modified. Reload and lose changes?",
                                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
    
    if self.file_data:
        self.load_ide_data()
        self.is_modified = False
        self.setWindowTitle(f"IDE Editor - {self.filename}")

def validate_ide(self):
    """Validate IDE file structure"""
    issues = []
    
    # Check for duplicate IDs
    all_ids = []
    for section_name, items in self.parsed_data.items():
        for item in items:
            if 'id' in item:
                all_ids.append((item['id'], section_name))
    
    # Find duplicates
    id_counts = {}
    for obj_id, section in all_ids:
        if obj_id in id_counts:
            id_counts[obj_id].append(section)
        else:
            id_counts[obj_id] = [section]
    
    for obj_id, sections in id_counts.items():
        if len(sections) > 1:
            issues.append(f"Duplicate ID {obj_id} found in sections: {', '.join(sections)}")
    
    # Check for missing models/textures (basic validation)
    if 'objs' in self.parsed_data:
        for obj in self.parsed_data['objs']:
            if not obj.get('model', '').strip():
                issues.append(f"Object ID {obj['id']} has empty model name")
            if not obj.get('texture', '').strip():
                issues.append(f"Object ID {obj['id']} has empty texture name")
    
    # Show results
    if issues:
        issues_text = "Validation Issues Found:\n\n" + "\n".join(f"• {issue}" for issue in issues)
        QMessageBox.warning(self, "Validation Issues", issues_text)
    else:
        QMessageBox.information(self, "Validation Complete", "No issues found in IDE file!")

# Add these methods to IDEFileEditor class
IDEFileEditor.show_id_manager = show_id_manager
IDEFileEditor.jump_to_section = jump_to_section
IDEFileEditor.filter_objects = filter_objects
IDEFileEditor.show_object_context_menu = show_object_context_menu
IDEFileEditor.edit_object = edit_object
IDEFileEditor.duplicate_object = duplicate_object
IDEFileEditor.delete_object = delete_object
IDEFileEditor.goto_object_line = goto_object_line
IDEFileEditor.find_next_free_id = find_next_free_id
IDEFileEditor.regenerate_text_content = regenerate_text_content
IDEFileEditor.find_text = find_text
IDEFileEditor.on_text_changed = on_text_changed
IDEFileEditor.mark_modified = mark_modified
IDEFileEditor.save_file = save_file
IDEFileEditor.reload_file = reload_file
IDEFileEditor.validate_ide = validate_ide

class ObjectEditDialog(QDialog):
    """Dialog for editing individual object properties"""
    
    def __init__(self, obj_data: Dict, used_ids: Set[int], parent=None):
        super().__init__(parent)
        self.obj_data = obj_data.copy()
        self.used_ids = used_ids
        self.original_id = obj_data.get('id', 0)
        
        self.setWindowTitle("Edit Object")
        self.setMinimumSize(400, 300)
        self.setup_ui()
        self.load_data()
    
    def setup_ui(self):
        """Setup object edit dialog UI"""
        layout = QVBoxLayout(self)
        
        # Form fields
        form_layout = QVBoxLayout()
        
        # ID field
        id_layout = QHBoxLayout()
        id_layout.addWidget(QLabel("ID:"))
        self.id_spin = QSpinBox()
        self.id_spin.setRange(0, 65535)
        self.id_spin.valueChanged.connect(self.check_id_conflict)
        id_layout.addWidget(self.id_spin)
        
        self.id_status_label = QLabel()
        id_layout.addWidget(self.id_status_label)
        id_layout.addStretch()
        
        form_layout.addLayout(id_layout)
        
        # Model field
        model_layout = QHBoxLayout()
        model_layout.addWidget(QLabel("Model:"))
        self.model_input = QLineEdit()
        model_layout.addWidget(self.model_input)
        form_layout.addLayout(model_layout)
        
        # Texture field
        texture_layout = QHBoxLayout()
        texture_layout.addWidget(QLabel("Texture:"))
        self.texture_input = QLineEdit()
        texture_layout.addWidget(self.texture_input)
        form_layout.addLayout(texture_layout)
        
        # Draw distance field
        draw_dist_layout = QHBoxLayout()
        draw_dist_layout.addWidget(QLabel("Draw Distance:"))
        self.draw_dist_spin = QSpinBox()
        self.draw_dist_spin.setRange(0, 9999)
        draw_dist_layout.addWidget(self.draw_dist_spin)
        form_layout.addLayout(draw_dist_layout)
        
        # Flags field
        flags_layout = QHBoxLayout()
        flags_layout.addWidget(QLabel("Flags:"))
        self.flags_spin = QSpinBox()
        self.flags_spin.setRange(0, 999999)
        flags_layout.addWidget(self.flags_spin)
        form_layout.addLayout(flags_layout)
        
        layout.addLayout(form_layout)
        
        # Preview
        preview_group = QGroupBox("Preview")
        preview_layout = QVBoxLayout(preview_group)
        
        self.preview_label = QLabel()
        self.preview_label.setStyleSheet("font-family: monospace; background: #f0f0f0; padding: 5px;")
        preview_layout.addWidget(self.preview_label)
        
        layout.addWidget(preview_group)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        save_btn = QPushButton("💾 Save")
        save_btn.clicked.connect(self.accept)
        button_layout.addWidget(save_btn)
        
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
        
        # Connect signals for live preview
        self.id_spin.valueChanged.connect(self.update_preview)
        self.model_input.textChanged.connect(self.update_preview)
        self.texture_input.textChanged.connect(self.update_preview)
        self.draw_dist_spin.valueChanged.connect(self.update_preview)
        self.flags_spin.valueChanged.connect(self.update_preview)
    
    def load_data(self):
        """Load object data into form"""
        self.id_spin.setValue(self.obj_data.get('id', 0))
        self.model_input.setText(self.obj_data.get('model', ''))
        self.texture_input.setText(self.obj_data.get('texture', ''))
        self.draw_dist_spin.setValue(int(self.obj_data.get('draw_dist', 0)))
        self.flags_spin.setValue(self.obj_data.get('flags', 0))
        
        self.check_id_conflict()
        self.update_preview()
    
    def check_id_conflict(self):
        """Check for ID conflicts"""
        new_id = self.id_spin.value()
        
        if new_id != self.original_id and new_id in self.used_ids:
            self.id_status_label.setText("⚠️ ID already used!")
            self.id_status_label.setStyleSheet("color: red;")
        else:
            self.id_status_label.setText("✅ ID available")
            self.id_status_label.setStyleSheet("color: green;")
    
    def update_preview(self):
        """Update the preview line"""
        preview_line = f"{self.id_spin.value()}, {self.model_input.text()}, {self.texture_input.text()}, {self.draw_dist_spin.value()}, {self.flags_spin.value()}"
        self.preview_label.setText(preview_line)
    
    def get_object_data(self) -> Dict:
        """Get updated object data"""
        return {
            'id': self.id_spin.value(),
            'model': self.model_input.text(),
            'texture': self.texture_input.text(),
            'draw_dist': self.draw_dist_spin.value(),
            'flags': self.flags_spin.value(),
            'line_num': self.obj_data.get('line_num', 0),
            'raw_line': f"{self.id_spin.value()}, {self.model_input.text()}, {self.texture_input.text()}, {self.draw_dist_spin.value()}, {self.flags_spin.value()}"
        }

# Integration functions for main application
def open_ide_editor(main_window, file_data: bytes, filename: str):
    """Open IDE editor from main application"""
    try:
        editor = IDEFileEditor(main_window, file_data, filename)
        editor.show()
        
        main_window.log_message(f"📝 Opened IDE editor for {filename}")
        
        return editor
        
    except Exception as e:
        QMessageBox.critical(main_window, "IDE Editor Error", f"Failed to open IDE editor:\n{str(e)}")
        return None

def integrate_gta_file_editors(main_window):
    """Integrate GTA file editors into main application"""
    try:
        # Add IDE editor method to main window
        main_window.open_ide_editor = lambda data, name: open_ide_editor(main_window, data, name)
        
        main_window.log_message("✅ GTA file editors integrated")
        return True
        
    except Exception as e:
        main_window.log_message(f"❌ Failed to integrate GTA file editors: {str(e)}")
        return False

# Export functions
__all__ = [
    'IDEFileEditor',
    'IDManagerDialog', 
    'ObjectEditDialog',
    'open_ide_editor',
    'integrate_gta_file_editors'
]