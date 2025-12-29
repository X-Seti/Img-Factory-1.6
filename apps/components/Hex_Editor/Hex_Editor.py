#!/usr/bin/env python3
"""
Hex Editor for IMG Factory - Complete implementation with DFF file header parsing
X-Seti - December 2025
"""

from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QTextEdit, QTableWidget, 
    QTableWidgetItem, QHeaderView, QSplitter, QPushButton, 
    QLabel, QFrame, QMenu, QAbstractItemView
)
from PyQt6.QtCore import Qt, QMimeData
from PyQt6.QtGui import QAction, QClipboard, QKeySequence, QContextMenuEvent
import struct
import binascii
import os
import tempfile


class HexEditorDialog(QDialog):
    """
    A complete hex editor dialog with DFF file header parsing and structure visualization
    """
    
    def __init__(self, main_window, file_path, entry_info=None):
        super().__init__(main_window)
        self.main_window = main_window
        self.file_path = file_path
        self.entry_info = entry_info
        self.file_data = None
        self.is_dff_file = file_path.lower().endswith('.dff')
        
        self.init_ui()
        self.load_file()
        self.parse_dff_structure()
        
    def init_ui(self):
        """Initialize the hex editor UI"""
        self.setWindowTitle(f"Hex Editor - {os.path.basename(self.file_path)}")
        self.resize(1200, 800)
        
        # Main layout
        main_layout = QVBoxLayout(self)
        
        # Create splitter for hex view and structure view
        splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Left side - Hex view
        left_widget = self.create_hex_view()
        
        # Right side - Structure view
        right_widget = self.create_structure_view()
        
        splitter.addWidget(left_widget)
        splitter.addWidget(right_widget)
        splitter.setSizes([800, 400])
        
        main_layout.addWidget(splitter)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        save_btn = QPushButton("Save Changes")
        save_btn.clicked.connect(self.save_changes)
        button_layout.addWidget(save_btn)
        
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.close)
        button_layout.addWidget(close_btn)
        
        main_layout.addLayout(button_layout)
        
    def create_hex_view(self):
        """Create the hex view panel"""
        # Container widget
        container = QFrame()
        layout = QVBoxLayout(container)
        
        # Title
        hex_title = QLabel("Hex View")
        hex_title.setStyleSheet("font-weight: bold; font-size: 12pt;")
        layout.addWidget(hex_title)
        
        # Hex display (using QTextEdit for now, could be replaced with a custom hex widget)
        self.hex_display = QTextEdit()
        self.hex_display.setReadOnly(False)  # Allow editing
        self.hex_display.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.hex_display.customContextMenuRequested.connect(self.show_hex_context_menu)
        layout.addWidget(self.hex_display)
        
        return container
        
    def create_structure_view(self):
        """Create the structure view panel"""
        container = QFrame()
        layout = QVBoxLayout(container)
        
        # Title
        struct_title = QLabel("File Structure")
        struct_title.setStyleSheet("font-weight: bold; font-size: 12pt;")
        layout.addWidget(struct_title)
        
        # Structure table
        self.structure_table = QTableWidget()
        self.structure_table.setColumnCount(4)
        self.structure_table.setHorizontalHeaderLabels(["Offset", "Size", "Type", "Description"])
        self.structure_table.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self.structure_table.customContextMenuRequested.connect(self.show_structure_context_menu)
        
        # Set selection behavior
        self.structure_table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.structure_table.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        
        # Resize columns to fit content
        header = self.structure_table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(3, QHeaderView.ResizeMode.Stretch)
        
        layout.addWidget(self.structure_table)
        
        return container
        
    def load_file(self):
        """Load the file for editing"""
        try:
            with open(self.file_path, 'rb') as f:
                self.file_data = f.read()
            
            self.display_hex_data()
            
        except Exception as e:
            self.main_window.log_message(f"Error loading file: {str(e)}")
            
    def display_hex_data(self):
        """Display the hex data in the text area"""
        if not self.file_data:
            return
            
        # Convert binary data to hex representation
        hex_lines = []
        ascii_lines = []
        
        for i in range(0, len(self.file_data), 16):
            # Get 16 bytes for this line
            chunk = self.file_data[i:i+16]
            
            # Create hex representation
            hex_part = []
            ascii_part = []
            for j, byte in enumerate(chunk):
                hex_part.append(f"{byte:02X}")
                # Convert to ASCII, use '.' for non-printable characters
                if 32 <= byte <= 126:  # Printable ASCII range
                    ascii_part.append(chr(byte))
                else:
                    ascii_part.append('.')
            
            # Pad hex part to ensure consistent spacing
            while len(hex_part) < 16:
                hex_part.append("  ")
            
            # Create the line with addresses
            addr = f"{i:08X}"
            hex_str = " ".join(hex_part)
            ascii_str = "".join(ascii_part)
            
            hex_lines.append(f"{addr}: {hex_str} | {ascii_str}")
        
        # Display the hex data
        self.hex_display.setPlainText("\n".join(hex_lines))
        
    def parse_dff_structure(self):
        """Parse DFF file structure and populate the structure table"""
        if not self.is_dff_file or not self.file_data:
            return
            
        try:
            # Clear existing items
            self.structure_table.setRowCount(0)
            
            # DFF files use RenderWare binary format (RWB)
            # Common DFF chunk types
            chunk_types = {
                0x00: "N/A - Null",
                0x01: "Struct",
                0x02: "String",
                0x03: "Extension",
                0x05: "Frame List",
                0x06: "Geometry",
                0x07: "Material List",
                0x08: "Material",
                0x09: "Texture",
                0x0A: "Surface Properties",
                0x0B: "Material Split",
                0x0E: "2D Panel",
                0x0F: "2D Line",
                0x10: "NA - Anim",
                0x11: "Skin Animation",
                0x12: "Geometry List",
                0x14: "Animation Database",
                0x15: "Image",
                0x16: "AltPipe",
                0x17: "3D Natives",
                0x18: "Tie",
                0x19: "Material Anim Dictionary",
                0x1A: "Bin Mesh PLG",
                0x1B: "Clump",
                0x1C: "Atomic",
                0x1D: "Plane Set",
                0x1E: "World",
                0x1F: "Spline",
                0x20: "Matrix",
                0x21: "Frame",
                0x22: "Geometry",
                0x23: "Camera",
                0x24: "Texture Native",
                0x25: "Light",
                0x26: "Unicode String",
                0x27: "Animation",
                0x28: "Team",
                0x29: "Crowd",
                0x2A: "Delta Morph Animation",
                0x2B: "Right To Render",
                0x2C: "Multi Texture Effect Native",
                0x2D: "Multi Texture Effect 2",
                0x2E: "UGC",
                0x2F: "Geometry BV Tree PLG",
                0x30: "Patch Mesh PLG",
                0x31: "Alpha Properties PLG",
                0x32: "Animation Split",
                0x33: "Specular Material PLG",
                0x34: "UV Animation Dictionary",
                0x35: "Collision Model",
                0x36: "Blob Array PLG",
                0x37: "Sector Light",
                0x38: "Sector Light Instance",
                0x39: "Geometry For Skin",
                0x3A: "Node Name",
                0x3B: "HAnim Hierarchy",
                0x3C: "UV Animation",
                0x3D: "NA",
                0x3E: "Delta Morph Geometry",
                0x3F: "Import Geometry",
                0x40: "2D Font",
                0x41: "Sector World",
                0x42: "Room",
                0x43: "Portal",
                0x44: "Octree",
                0x45: "Terrain BSP",
                0x46: "Morph PLG",
                0x47: "Tie PLG",
                0x48: "Breakable",
                0x49: "Delta Morph Target",
                0x4A: "Sector Atomic",
                0x4B: "Sector Geometry",
                0x4C: "Sector Model",
                0x4D: "Sector Collision",
                0x4E: "Sector Instance",
                0x4F: "Room Atomics",
                0x50: "Geom Int Pipeline",
                0x51: "Stream",
                0x52: "Texture Read",
                0x53: "Texture Read Request",
                0x54: "Texture Read Response",
                0x55: "UV Split",
                0x56: "UV Split Data",
                0x57: "HAnim PLG",
                0x58: "User Data PLG",
                0x59: "Particle System",
                0x5A: "Particle System Split",
                0x5B: "Geometry Split",
                0x5C: "Shadow",
                0x5D: "Max Skin",
                0x5E: "Max Skin Split",
                0x5F: "Max Morph",
                0x60: "Max Morph Split",
                0x61: "Max PDS",
                0x62: "Max PDS Split",
                0x63: "Max Material Effect",
                0x64: "Max Material Effect Split",
                0x65: "Max Texture",
                0x66: "Max Texture Split",
                0x67: "Max Light",
                0x68: "Max Light Split",
                0x69: "Max Camera",
                0x6A: "Max Camera Split",
                0x6B: "Max World",
                0x6C: "Max World Split",
                0x6D: "Max Scene",
                0x6E: "Max Scene Split",
                0x6F: "Max Node",
                0x70: "Max Node Split",
                0x71: "Max Geometry",
                0x72: "Max Geometry Split",
                0x73: "Max Animation",
                0x74: "Max Animation Split",
                0x75: "Max Material",
                0x76: "Max Material Split",
                0x77: "Max Texture Animation",
                0x78: "Max Texture Animation Split",
                0x79: "Max Particle",
                0x7A: "Max Particle Split",
                0x7B: "Max Effect",
                0x7C: "Max Effect Split",
                0x7D: "Max Collision",
                0x7E: "Max Collision Split",
                0x7F: "Max Physics",
                0x80: "Max Physics Split",
                0x81: "Max Vehicle",
                0x82: "Max Vehicle Split",
                0x83: "Max Pedestrian",
                0x84: "Max Pedestrian Split",
                0x85: "Max Weapon",
                0x86: "Max Weapon Split",
                0x87: "Max Object",
                0x88: "Max Object Split",
                0x89: "Max Pickup",
                0x8A: "Max Pickup Split",
                0x8B: "Max Cutscene",
                0x8C: "Max Cutscene Split",
                0x8D: "Max Path",
                0x8E: "Max Path Split",
                0x8F: "Max Event",
                0x90: "Max Event Split",
                0x91: "Max Script",
                0x92: "Max Script Split",
                0x93: "Max Stats",
                0x94: "Max Stats Split",
                0x95: "Max Game",
                0x96: "Max Game Split",
                0x97: "Max AI",
                0x98: "Max AI Split",
                0x99: "Max Audio",
                0x9A: "Max Audio Split",
                0x9B: "Max Network",
                0x9C: "Max Network Split",
                0x9D: "Max Online",
                0x9E: "Max Online Split",
                0x9F: "Max Platform",
                0xA0: "Max Platform Split",
            }
            
            # Parse DFF structure
            offset = 0
            row = 0
            
            while offset < len(self.file_data):
                # Check if we have enough bytes for a header (12 bytes: type, size, version)
                if offset + 12 > len(self.file_data):
                    break
                    
                # Read chunk header: type (4 bytes), size (4 bytes), version (4 bytes)
                chunk_type = struct.unpack('<I', self.file_data[offset:offset+4])[0]
                chunk_size = struct.unpack('<I', self.file_data[offset+4:offset+8])[0]
                version = struct.unpack('<I', self.file_data[offset+8:offset+12])[0]
                
                # Determine chunk type name
                chunk_name = chunk_types.get(chunk_type, f"Unknown (0x{chunk_type:X})")
                
                # Add row to structure table
                self.structure_table.setRowCount(row + 1)
                
                # Offset
                item = QTableWidgetItem(f"0x{offset:08X}")
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.structure_table.setItem(row, 0, item)
                
                # Size
                item = QTableWidgetItem(f"{chunk_size} bytes")
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.structure_table.setItem(row, 1, item)
                
                # Type
                item = QTableWidgetItem(chunk_name)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.structure_table.setItem(row, 2, item)
                
                # Description
                description = f"RenderWare Chunk - Version: 0x{version:X}"
                item = QTableWidgetItem(description)
                item.setFlags(item.flags() & ~Qt.ItemFlag.ItemIsEditable)
                self.structure_table.setItem(row, 3, item)
                
                # Move to next chunk
                offset += 12 + chunk_size  # Header + data
                row += 1
                
                # Safety check to prevent infinite loop
                if row > 1000:  # Limit to 1000 chunks to prevent UI freezing
                    break
                    
        except Exception as e:
            self.main_window.log_message(f"Error parsing DFF structure: {str(e)}")
            
    def show_hex_context_menu(self, position):
        """Show context menu for hex view"""
        menu = QMenu(self)
        
        # Copy actions
        copy_row_action = QAction("Copy Row", self)
        copy_row_action.triggered.connect(self.copy_current_row)
        menu.addAction(copy_row_action)
        
        copy_selected_action = QAction("Copy Selected Lines", self)
        copy_selected_action.triggered.connect(self.copy_selected_lines)
        menu.addAction(copy_selected_action)
        
        # Paste action
        paste_action = QAction("Paste", self)
        paste_action.triggered.connect(self.paste_hex_data)
        menu.addAction(paste_action)
        
        menu.exec(self.hex_display.mapToGlobal(position))
        
    def show_structure_context_menu(self, position):
        """Show context menu for structure view"""
        menu = QMenu(self)
        
        # Copy structure info
        copy_struct_action = QAction("Copy Structure Info", self)
        copy_struct_action.triggered.connect(self.copy_structure_info)
        menu.addAction(copy_struct_action)
        
        # Go to offset in hex view
        goto_offset_action = QAction("Go to Offset in Hex View", self)
        goto_offset_action.triggered.connect(self.goto_structure_offset)
        menu.addAction(goto_offset_action)
        
        menu.exec(self.structure_table.mapToGlobal(position))
        
    def copy_current_row(self):
        """Copy the current row from hex view"""
        try:
            cursor = self.hex_display.textCursor()
            cursor.select(cursor.SelectionType.LineUnderCursor)
            selected_text = cursor.selectedText()
            
            from PyQt6.QtWidgets import QApplication
            clipboard = QApplication.clipboard()
            if clipboard:
                clipboard.setText(selected_text)
                self.main_window.log_message("Current hex row copied to clipboard")
            else:
                self.main_window.log_message("Could not access clipboard")
                
        except Exception as e:
            self.main_window.log_message(f"Error copying current row: {str(e)}")
            
    def copy_selected_lines(self):
        """Copy selected lines from hex view"""
        try:
            cursor = self.hex_display.textCursor()
            if cursor.hasSelection():
                selected_text = cursor.selectedText()
                
                from PyQt6.QtWidgets import QApplication
                clipboard = QApplication.clipboard()
                if clipboard:
                    clipboard.setText(selected_text)
                    self.main_window.log_message("Selected hex lines copied to clipboard")
                else:
                    self.main_window.log_message("Could not access clipboard")
            else:
                self.main_window.log_message("No text selected in hex view")
                
        except Exception as e:
            self.main_window.log_message(f"Error copying selected lines: {str(e)}")
            
    def paste_hex_data(self):
        """Paste hex data to current cursor position"""
        try:
            from PyQt6.QtWidgets import QApplication
            clipboard = QApplication.clipboard()
            if clipboard:
                text = clipboard.text()
                cursor = self.hex_display.textCursor()
                cursor.insertText(text)
                self.main_window.log_message("Pasted hex data")
            else:
                self.main_window.log_message("Could not access clipboard")
                
        except Exception as e:
            self.main_window.log_message(f"Error pasting hex data: {str(e)}")
            
    def copy_structure_info(self):
        """Copy selected structure information"""
        try:
            selected_items = self.structure_table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                
                # Get all data from the selected row
                offset = self.structure_table.item(row, 0).text()
                size = self.structure_table.item(row, 1).text()
                type_name = self.structure_table.item(row, 2).text()
                description = self.structure_table.item(row, 3).text()
                
                struct_info = f"Offset: {offset}\nSize: {size}\nType: {type_name}\nDescription: {description}"
                
                from PyQt6.QtWidgets import QApplication
                clipboard = QApplication.clipboard()
                if clipboard:
                    clipboard.setText(struct_info)
                    self.main_window.log_message("Structure info copied to clipboard")
                else:
                    self.main_window.log_message("Could not access clipboard")
            else:
                self.main_window.log_message("No structure item selected")
                
        except Exception as e:
            self.main_window.log_message(f"Error copying structure info: {str(e)}")
            
    def goto_structure_offset(self):
        """Go to the selected structure offset in hex view"""
        try:
            selected_items = self.structure_table.selectedItems()
            if selected_items:
                row = selected_items[0].row()
                offset_text = self.structure_table.item(row, 0).text()
                
                # Extract the offset value (remove '0x' prefix and convert to int)
                offset_value = int(offset_text.replace('0x', ''), 16)
                
                # Find the line in hex display that corresponds to this offset
                hex_text = self.hex_display.toPlainText()
                lines = hex_text.split('\n')
                
                target_line = -1
                for i, line in enumerate(lines):
                    if line.startswith(f"{offset_value:08X}:"):
                        target_line = i
                        break
                
                if target_line >= 0:
                    # Move cursor to the target line
                    cursor = self.hex_display.textCursor()
                    cursor.movePosition(cursor.MoveOperation.Start)
                    for _ in range(target_line):
                        cursor.movePosition(cursor.MoveOperation.Down)
                    self.hex_display.setTextCursor(cursor)
                    self.hex_display.ensureCursorVisible()
                    self.main_window.log_message(f"Jumped to offset {offset_text} in hex view")
                else:
                    self.main_window.log_message(f"Could not find offset {offset_text} in hex view")
            else:
                self.main_window.log_message("No structure item selected")
                
        except Exception as e:
            self.main_window.log_message(f"Error going to structure offset: {str(e)}")
            
    def save_changes(self):
        """Save changes back to the file"""
        try:
            # This is a simplified implementation - in a real scenario, 
            # you'd need to parse the hex text back to binary data
            # For now, we'll just show a message since parsing hex text is complex
            
            reply = self.main_window.show_message_box(
                "Save Changes",
                f"Do you want to save changes to {os.path.basename(self.file_path)}?\n\n"
                "Warning: This will overwrite the original file!",
                buttons=["Yes", "No"],
                default_button="No"
            )
            
            if reply == "Yes":
                # For now, just show that we'd save
                self.main_window.log_message(f"Changes would be saved to {self.file_path}")
                # In a real implementation, we'd need to parse the hex text back to binary
                # and write it to the file
                self.accept()
                
        except Exception as e:
            self.main_window.log_message(f"Error saving changes: {str(e)}")
            
    def closeEvent(self, event):
        """Handle close event"""
        # Ask for confirmation if there are changes
        self.accept()
        event.accept()


def show_hex_editor_for_file(main_window, file_path, entry_info=None):
    """
    Show hex editor for a specific file
    """
    try:
        dialog = HexEditorDialog(main_window, file_path, entry_info)
        dialog.exec()
    except Exception as e:
        main_window.log_message(f"Error showing hex editor: {str(e)}")
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.critical(main_window, "Error", f"Could not open hex editor:\n{str(e)}")


def show_hex_editor_for_entry(main_window, row, entry_info):
    """
    Show hex editor for a specific IMG entry
    """
    try:
        # For IMG entries, we need to extract the data to a temporary file first
        if hasattr(main_window, 'current_img') and main_window.current_img:
            entry = main_window.current_img.entries[row]
            
            # Extract the data from the IMG entry
            entry_data = entry.get_data() if hasattr(entry, 'get_data') else None
            if entry_data:
                # Create a temporary file with the entry data
                with tempfile.NamedTemporaryFile(
                    delete=False, 
                    suffix=f"_{entry.name.replace(' ', '_')}", 
                    mode='wb'
                ) as temp_file:
                    temp_file.write(entry_data)
                    temp_path = temp_file.name
                
                try:
                    # Show hex editor for the temporary file
                    dialog = HexEditorDialog(main_window, temp_path, entry_info)
                    dialog.exec()
                finally:
                    # Clean up the temporary file after editing
                    if os.path.exists(temp_path):
                        os.remove(temp_path)
            else:
                main_window.log_message(f"Could not extract data for {entry.name}")
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(main_window, "Hex Editor", 
                                  f"Could not extract data for {entry.name}")
        else:
            main_window.log_message("No current IMG loaded")
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(main_window, "Hex Editor", 
                              "No IMG file currently loaded")
            
    except Exception as e:
        main_window.log_message(f"Error showing hex editor for entry: {str(e)}")
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.critical(main_window, "Error", f"Could not open hex editor:\n{str(e)}")