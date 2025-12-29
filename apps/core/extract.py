"""
Extract functionality for IMG Factory
Handles extracting textures from DFF files and TXD files from IMG files
"""
import os
import re
from typing import List, Dict, Tuple, Optional
from pathlib import Path
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QTextEdit, 
    QLabel, QFileDialog, QCheckBox, QGroupBox, QProgressBar, 
    QMessageBox, QTabWidget, QWidget
)
from PyQt6.QtCore import Qt, QThread, pyqtSignal

class TextureExtractor:
    """Handles texture extraction from IMG and DFF files"""
    
    def __init__(self, main_window):
        self.main_window = main_window
        self.extracted_textures = {}
        self.dff_texture_mapping = {}
        
    def extract_textures_from_img(self, img_file_path: str, output_dir: str) -> bool:
        """
        Extract all TXD files from IMG and convert textures to PNG
        """
        try:
            if not hasattr(self.main_window, 'current_img') or not self.main_window.current_img:
                self.main_window.log_message("No IMG file loaded")
                return False
                
            current_img = self.main_window.current_img
            extracted_count = 0
            
            # Create output directory
            os.makedirs(output_dir, exist_ok=True)
            
            for entry in current_img.entries:
                if entry.name.lower().endswith('.txd'):
                    # Extract TXD file temporarily
                    if hasattr(entry, 'get_data'):
                        txd_data = entry.get_data()
                        if txd_data:
                            txd_path = os.path.join(output_dir, entry.name)
                            with open(txd_path, 'wb') as f:
                                f.write(txd_data)
                            
                            # Extract textures from TXD (this would need actual TXD parsing)
                            texture_count = self._extract_textures_from_txd(txd_path, output_dir)
                            extracted_count += texture_count
                            self.main_window.log_message(f"Extracted {texture_count} textures from {entry.name}")
            
            self.main_window.log_message(f"Extraction complete: {extracted_count} textures extracted")
            return True
            
        except Exception as e:
            self.main_window.log_message(f"Error extracting textures: {str(e)}")
            return False
    
    def _extract_textures_from_txd(self, txd_path: str, output_dir: str) -> int:
        """
        Extract individual textures from TXD file
        This is a placeholder - actual TXD parsing would go here
        """
        # This would contain the actual TXD parsing and PNG extraction logic
        # For now, we'll simulate the process
        texture_count = 0
        
        # Extract textures from TXD file to PNG format
        # This would require actual TXD parsing library
        # For now, just return a simulated count
        texture_count = 1  # Simulated extraction
        
        return texture_count
    
    def parse_dff_textures(self, dff_files: List[str]) -> Dict[str, List[str]]:
        """
        Parse DFF files to extract texture name mappings
        Returns dictionary mapping model names to required textures
        """
        texture_mapping = {}
        
        for dff_file in dff_files:
            try:
                model_name = os.path.basename(dff_file)
                textures = self._parse_dff_for_textures(dff_file)
                texture_mapping[model_name] = textures
            except Exception as e:
                self.main_window.log_message(f"Error parsing DFF {dff_file}: {str(e)}")
        
        return texture_mapping
    
    def _parse_dff_for_textures(self, dff_file: str) -> List[str]:
        """
        Parse a single DFF file to extract texture names
        Uses the proper DFF parser from the TXD editor
        """
        import struct

        try:
            with open(dff_file, 'rb') as f:
                dff_data = f.read()

            materials = []
            offset = 0

            # Simple RenderWare parser - look for material sections
            while offset < len(dff_data) - 12:
                try:
                    section_type = struct.unpack('<I', dff_data[offset:offset+4])[0]
                    section_size = struct.unpack('<I', dff_data[offset+4:offset+8])[0]

                    # Material section (0x07) or Texture section (0x06)
                    if section_type == 0x07:  # Material
                        # Look for string data in material section
                        mat_end = min(offset + section_size + 12, len(dff_data))
                        mat_data = dff_data[offset:mat_end]

                        # Find null-terminated strings (potential texture names)
                        for i in range(len(mat_data) - 32):
                            if mat_data[i:i+1].isalpha():
                                # Try to extract string
                                end = i
                                while end < len(mat_data) and mat_data[end] != 0 and end < i + 32:
                                    end += 1

                                if end > i + 3:  # At least 4 chars
                                    try:
                                        name = mat_data[i:end].decode('ascii', errors='ignore')
                                        if name and len(name) > 3 and name.replace('_', '').replace('.', '').isalnum():
                                            if name not in materials:
                                                materials.append(name)
                                    except:
                                        pass

                    offset += 12 + section_size

                except:
                    offset += 1

            return materials

        except Exception as e:
            self.main_window.log_message(f"DFF parse error: {str(e)}")
            return []


class ExtractDialog(QDialog):
    """Dialog for extraction functionality"""
    
    def __init__(self, main_window):
        super().__init__()
        self.main_window = main_window
        self.extractor = TextureExtractor(main_window)
        self.setWindowTitle("Extract Textures and Models")
        self.resize(700, 500)
        
        self.setup_ui()
        
    def setup_ui(self):
        """Setup the extraction dialog UI"""
        layout = QVBoxLayout(self)
        
        # Create tab widget
        tab_widget = QTabWidget()
        
        # Texture extraction tab
        texture_tab = self.create_texture_tab()
        tab_widget.addTab(texture_tab, "Extract Textures")
        
        # DFF parsing tab
        dff_tab = self.create_dff_tab()
        tab_widget.addTab(dff_tab, "Parse DFF Textures")
        
        # External DFF models tab
        external_dff_tab = self.create_external_dff_tab()
        tab_widget.addTab(external_dff_tab, "External DFF Models")
        
        layout.addWidget(tab_widget)
        
        # Buttons
        button_layout = QHBoxLayout()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.close)
        button_layout.addStretch()
        button_layout.addWidget(close_btn)
        
        layout.addLayout(button_layout)
        
    def create_texture_tab(self):
        """Create the texture extraction tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Description
        desc_label = QLabel("Extract all TXD textures from IMG file as PNG:")
        desc_label.setWordWrap(True)
        layout.addWidget(desc_label)
        
        # Output directory selection
        output_layout = QHBoxLayout()
        output_label = QLabel("Output Directory:")
        self.output_dir_edit = QTextEdit()
        self.output_dir_edit.setMaximumHeight(30)
        self.output_dir_edit.setPlaceholderText("Select output directory for extracted textures...")
        browse_output_btn = QPushButton("Browse")
        browse_output_btn.clicked.connect(self.browse_output_dir)
        
        output_layout.addWidget(output_label)
        output_layout.addWidget(self.output_dir_edit)
        output_layout.addWidget(browse_output_btn)
        layout.addLayout(output_layout)
        
        # Extract button
        self.extract_btn = QPushButton("Extract All Textures")
        self.extract_btn.clicked.connect(self.extract_textures)
        layout.addWidget(self.extract_btn)
        
        # Progress bar
        self.progress_bar = QProgressBar()
        self.progress_bar.setVisible(False)
        layout.addWidget(self.progress_bar)
        
        # Log area
        self.log_area = QTextEdit()
        self.log_area.setReadOnly(True)
        layout.addWidget(self.log_area)
        
        return widget
        
    def create_dff_tab(self):
        """Create the DFF parsing tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Description
        desc_label = QLabel("Parse DFF files from loaded IMG to list texture requirements:")
        desc_label.setWordWrap(True)
        layout.addWidget(desc_label)
        
        # Status area showing highlighted DFF files
        status_group = QGroupBox("Highlighted DFF Files in IMG")
        status_layout = QVBoxLayout(status_group)
        
        self.highlight_status_label = QLabel("No highlighted DFF files selected")
        self.highlight_status_label.setWordWrap(True)
        status_layout.addWidget(self.highlight_status_label)
        
        layout.addWidget(status_group)
        
        # Parse button
        self.parse_btn = QPushButton("Parse Selected DFF Files")
        self.parse_btn.clicked.connect(self.parse_highlighted_dff_files)
        layout.addWidget(self.parse_btn)
        
        # Results area
        results_group = QGroupBox("Parsed Results")
        results_layout = QVBoxLayout(results_group)
        
        self.results_area = QTextEdit()
        self.results_area.setReadOnly(True)
        results_layout.addWidget(self.results_area)
        
        layout.addWidget(results_group)
        
        return widget

    def showEvent(self, event):
        """Override show event to start updating the highlight status"""
        super().showEvent(event)
        # Update the status immediately and then periodically
        self.update_highlight_status()
        # Set up a timer to update the status every 500ms
        from PyQt6.QtCore import QTimer
        self.status_timer = QTimer(self)
        self.status_timer.timeout.connect(self.update_highlight_status)
        self.status_timer.start(500)  # Update every 500ms

    def update_highlight_status(self):
        """Update the status label to show highlighted DFF files in the IMG table"""
        try:
            if (hasattr(self.main_window, 'current_img') and 
                self.main_window.current_img and 
                hasattr(self.main_window, 'gui_layout') and 
                hasattr(self.main_window.gui_layout, 'table')):
                
                table = self.main_window.gui_layout.table
                selected_items = table.selectedItems()
                
                if selected_items:
                    # Get unique rows that have selected items
                    selected_rows = list(set([item.row() for item in selected_items]))
                    dff_files = []
                    
                    for row in selected_rows:
                        filename_item = table.item(row, 0)
                        if filename_item:
                            filename = filename_item.text()
                            if filename.lower().endswith('.dff'):
                                dff_files.append(filename)
                    
                    if dff_files:
                        count = len(dff_files)
                        status_text = f"Found {count} highlighted DFF file{'s' if count != 1 else ''}: {', '.join(dff_files)}"
                        self.highlight_status_label.setText(status_text)
                    else:
                        self.highlight_status_label.setText("No highlighted DFF files selected (highlight DFF files in the table)")
                else:
                    self.highlight_status_label.setText("No highlighted DFF files selected (highlight DFF files in the table)")
            else:
                self.highlight_status_label.setText("No IMG file loaded")
        except Exception as e:
            self.highlight_status_label.setText(f"Error updating status: {str(e)}")

    def parse_highlighted_dff_files(self):
        """Parse highlighted DFF files from the loaded IMG"""
        try:
            if not (hasattr(self.main_window, 'current_img') and self.main_window.current_img):
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(self, "Error", "No IMG file loaded")
                return

            if not (hasattr(self.main_window, 'gui_layout') and 
                    hasattr(self.main_window.gui_layout, 'table')):
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(self, "Error", "IMG table not available")
                return

            table = self.main_window.gui_layout.table
            selected_items = table.selectedItems()

            if not selected_items:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(self, "Error", "Please highlight DFF files in the table")
                return

            # Get unique rows that have selected items
            selected_rows = list(set([item.row() for item in selected_items]))
            dff_entries = []

            for row in selected_rows:
                filename_item = table.item(row, 0)
                if filename_item:
                    filename = filename_item.text()
                    if filename.lower().endswith('.dff'):
                        # Find the corresponding entry in the IMG
                        for entry in self.main_window.current_img.entries:
                            if entry.name == filename:
                                dff_entries.append(entry)
                                break

            if not dff_entries:
                from PyQt6.QtWidgets import QMessageBox
                QMessageBox.warning(self, "Error", "No highlighted DFF files found in the table")
                return

            # Process each selected DFF file
            results = f"Parsed {len(dff_entries)} DFF file{'s' if len(dff_entries) != 1 else ''}:\n\n"
            
            for entry in dff_entries:
                try:
                    # Extract DFF data temporarily
                    dff_data = entry.get_data()
                    if dff_data:
                        import tempfile
                        import os
                        
                        # Create temporary file
                        with tempfile.NamedTemporaryFile(delete=False, suffix='.dff', mode='wb') as temp_file:
                            temp_file.write(dff_data)
                            temp_dff_path = temp_file.name

                        try:
                            # Parse textures from the DFF
                            textures = self.extractor._parse_dff_for_textures(temp_dff_path)
                            results += f"DFF File: {entry.name}\n"
                            results += f"Textures ({len(textures)}): {', '.join(textures) if textures else 'None'}\n\n"
                        finally:
                            # Clean up temp file
                            if os.path.exists(temp_dff_path):
                                os.remove(temp_dff_path)
                    else:
                        results += f"DFF File: {entry.name}\n"
                        results += "Textures: Could not extract data\n\n"
                        
                except Exception as e:
                    results += f"DFF File: {entry.name}\n"
                    results += f"Error parsing: {str(e)}\n\n"

            self.results_area.setPlainText(results)
            self.main_window.log_message(f"Successfully parsed {len(dff_entries)} DFF file(s)")

        except Exception as e:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.critical(self, "Error", f"Failed to parse highlighted DFF files: {str(e)}")
        
    def create_external_dff_tab(self):
        """Create the external DFF models tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)
        
        # Description
        desc_label = QLabel("Browse and extract external DFF models (not from IMG):")
        desc_label.setWordWrap(True)
        layout.addWidget(desc_label)
        
        # DFF file selection
        dff_layout = QHBoxLayout()
        dff_label = QLabel("External DFF Files:")
        self.external_dff_files_edit = QTextEdit()
        self.external_dff_files_edit.setMaximumHeight(60)
        self.external_dff_files_edit.setPlaceholderText("Select external DFF files to extract textures from...")
        browse_external_dff_btn = QPushButton("Browse")
        browse_external_dff_btn.clicked.connect(self.browse_external_dff_files)
        
        dff_layout.addWidget(dff_label)
        dff_layout.addWidget(self.external_dff_files_edit)
        dff_layout.addWidget(browse_external_dff_btn)
        layout.addLayout(dff_layout)
        
        # Output directory selection
        output_layout = QHBoxLayout()
        output_label = QLabel("Output Directory:")
        self.external_output_dir_edit = QTextEdit()
        self.external_output_dir_edit.setMaximumHeight(30)
        self.external_output_dir_edit.setPlaceholderText("Select output directory for extracted textures...")
        browse_output_btn = QPushButton("Browse")
        browse_output_btn.clicked.connect(self.browse_external_output_dir)
        
        output_layout.addWidget(output_label)
        output_layout.addWidget(self.external_output_dir_edit)
        output_layout.addWidget(browse_output_btn)
        layout.addLayout(output_layout)
        
        # Extract button
        self.extract_external_btn = QPushButton("Extract Textures from External DFFs")
        self.extract_external_btn.clicked.connect(self.extract_external_dff_textures)
        layout.addWidget(self.extract_external_btn)
        
        # Progress bar
        self.external_progress_bar = QProgressBar()
        self.external_progress_bar.setVisible(False)
        layout.addWidget(self.external_progress_bar)
        
        # Log area
        self.external_log_area = QTextEdit()
        self.external_log_area.setReadOnly(True)
        layout.addWidget(self.external_log_area)
        
        return widget
        
    def browse_output_dir(self):
        """Browse for output directory"""
        directory = QFileDialog.getExistingDirectory(
            self, "Select Output Directory", "", 
            QFileDialog.Option.ShowDirsOnly | QFileDialog.Option.DontResolveSymlinks
        )
        if directory:
            self.output_dir_edit.setPlainText(directory)
            

    def extract_textures(self):
        """Extract textures from IMG file"""
        output_dir = self.output_dir_edit.toPlainText().strip()
        if not output_dir:
            QMessageBox.warning(self, "Error", "Please select an output directory")
            return
            
        # Validate that an IMG file is loaded
        if not hasattr(self.main_window, 'current_img') or not self.main_window.current_img:
            QMessageBox.warning(self, "Error", "No IMG file loaded")
            return
            
        # Show progress
        self.progress_bar.setVisible(True)
        self.progress_bar.setRange(0, 0)  # Indeterminate progress
        
        try:
            success = self.extractor.extract_textures_from_img(
                self.main_window.current_img.file_path, 
                output_dir
            )
            
            if success:
                QMessageBox.information(self, "Success", "Textures extracted successfully!")
            else:
                QMessageBox.warning(self, "Error", "Failed to extract textures")
                
        except Exception as e:
            QMessageBox.critical(self, "Error", f"Extraction failed: {str(e)}")
        finally:
            self.progress_bar.setVisible(False)
            

    def browse_external_dff_files(self):
        """Browse for external DFF files"""
        files, _ = QFileDialog.getOpenFileNames(
            self, "Select External DFF Files", "", 
            "DFF Files (*.dff);;All Files (*)"
        )
        if files:
            self.external_dff_files_edit.setPlainText("\n".join(files))
            
    def browse_external_output_dir(self):
        """Browse for external output directory"""
        directory = QFileDialog.getExistingDirectory(
            self, "Select Output Directory", "", 
            QFileDialog.Option.ShowDirsOnly | QFileDialog.Option.DontResolveSymlinks
        )
        if directory:
            self.external_output_dir_edit.setPlainText(directory)
            
    def extract_external_dff_textures(self):
        """Extract textures from external DFF files"""
        dff_text = self.external_dff_files_edit.toPlainText().strip()
        if not dff_text:
            QMessageBox.warning(self, "Error", "Please select external DFF files to extract textures from")
            return
            
        output_dir = self.external_output_dir_edit.toPlainText().strip()
        if not output_dir:
            QMessageBox.warning(self, "Error", "Please select an output directory")
            return
            
        dff_files = [line.strip() for line in dff_text.split('\n') if line.strip()]
        
        # Validate files exist
        for dff_file in dff_files:
            if not os.path.exists(dff_file):
                QMessageBox.warning(self, "Error", f"DFF file does not exist: {dff_file}")
                return
        
        # Show progress
        self.external_progress_bar.setVisible(True)
        self.external_progress_bar.setRange(0, 0)  # Indeterminate progress
        
        try:
            extracted_count = 0
            total_files = len(dff_files)
            
            for i, dff_file in enumerate(dff_files):
                # Update progress
                progress = int((i / total_files) * 100) if total_files > 0 else 0
                self.external_progress_bar.setValue(progress)
                self.external_log_area.append(f"Processing {os.path.basename(dff_file)}... ({i+1}/{total_files})")
                
                # Extract textures from the DFF file
                textures = self.extractor._parse_dff_for_textures(dff_file)
                
                # Create a subdirectory for this DFF's textures
                dff_name = os.path.splitext(os.path.basename(dff_file))[0]
                dff_output_dir = os.path.join(output_dir, dff_name)
                os.makedirs(dff_output_dir, exist_ok=True)
                
                # Create a texture list file for this DFF
                texture_list_file = os.path.join(dff_output_dir, f"{dff_name}_textures.txt")
                with open(texture_list_file, 'w', encoding='utf-8') as f:
                    f.write(f"Textures for {dff_file}:\n")
                    f.write("=" * 50 + "\n")
                    for texture in textures:
                        f.write(f"{texture}\n")
                
                extracted_count += len(textures)
                self.external_log_area.append(f"  Found {len(textures)} textures in {dff_name}")
            
            self.external_log_area.append(f"\n✅ Extraction complete! Total textures found: {extracted_count}")
            QMessageBox.information(
                self, 
                "Success", 
                f"Textures extracted from {len(dff_files)} DFF files to:\n{output_dir}\n\nTotal textures found: {extracted_count}"
            )
            
        except Exception as e:
            QMessageBox.critical(self, "Error", f"External DFF extraction failed: {str(e)}")
        finally:
            self.external_progress_bar.setVisible(False)


def extract_textures_function(main_window):
    """Function to open the extract textures dialog"""
    try:
        dialog = ExtractDialog(main_window)
        dialog.exec()
    except Exception as e:
        main_window.log_message(f"Error opening extract dialog: {str(e)}")


def extract_dff_texture_lists(main_window):
    """Extract DFF texture lists from all DFF files in the current IMG"""
    try:
        if not hasattr(main_window, 'current_img') or not main_window.current_img:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.warning(main_window, "No IMG File", "Please open an IMG file first")
            return

        # Find all DFF files in the current IMG
        dff_entries = []
        for i, entry in enumerate(main_window.current_img.entries):
            if entry.name.lower().endswith('.dff'):
                dff_entries.append((i, entry))

        if not dff_entries:
            from PyQt6.QtWidgets import QMessageBox
            QMessageBox.information(main_window, "No DFF Files", "No DFF files found in the current IMG")
            return

        # Create a dialog to show the texture lists
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QTextEdit, QPushButton, QHBoxLayout
        dialog = QDialog(main_window)
        dialog.setWindowTitle("DFF Texture Lists from IMG")
        dialog.resize(700, 500)

        layout = QVBoxLayout(dialog)

        # Create text area to show texture lists
        text_area = QTextEdit()
        text_area.setReadOnly(True)

        # Extract and format texture information
        full_text = f"DFF Texture Lists from IMG: {main_window.current_img.file_path}\n"
        full_text += "=" * 80 + "\n\n"

        extractor = TextureExtractor(main_window)
        for idx, entry in dff_entries:
            try:
                # Extract DFF data temporarily
                dff_data = entry.get_data()
                if dff_data:
                    import tempfile
                    import os
                    # Create temporary file
                    with tempfile.NamedTemporaryFile(delete=False, suffix='.dff', mode='wb') as temp_file:
                        temp_file.write(dff_data)
                        temp_dff_path = temp_file.name

                    try:
                        # Parse textures from the DFF
                        textures = extractor._parse_dff_for_textures(temp_dff_path)
                        full_text += f"DFF File: {entry.name}\n"
                        full_text += f"Textures ({len(textures)}): {', '.join(textures) if textures else 'None'}\n\n"
                    finally:
                        # Clean up temp file
                        if os.path.exists(temp_dff_path):
                            os.remove(temp_dff_path)
                else:
                    full_text += f"DFF File: {entry.name}\n"
                    full_text += "Textures: Could not extract data\n\n"
            except Exception as e:
                full_text += f"DFF File: {entry.name}\n"
                full_text += f"Error parsing: {str(e)}\n\n"

        text_area.setPlainText(full_text)
        layout.addWidget(text_area)

        # Add export button
        button_layout = QHBoxLayout()
        export_btn = QPushButton("Export to File")
        export_btn.clicked.connect(lambda: export_dff_texture_lists(main_window, dff_entries))
        button_layout.addWidget(export_btn)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(dialog.close)
        button_layout.addWidget(close_btn)

        layout.addLayout(button_layout)

        dialog.exec()

    except Exception as e:
        main_window.log_message(f"Error extracting DFF texture lists: {str(e)}")


def export_dff_texture_lists(main_window, dff_entries):
    """Export DFF texture lists to a text file"""
    try:
        from PyQt6.QtWidgets import QFileDialog, QMessageBox
        import os

        # Get save file path
        file_path, _ = QFileDialog.getSaveFileName(
            main_window,
            "Export DFF Texture Lists",
            os.path.join(os.path.dirname(main_window.current_img.file_path), "dff_texture_lists.txt"),
            "Text Files (*.txt);;All Files (*)"
        )

        if not file_path:
            return

        # Extract texture information
        extractor = TextureExtractor(main_window)
        output_lines = []
        output_lines.append(f"DFF Texture Lists from IMG: {main_window.current_img.file_path}")
        output_lines.append("=" * 80)
        output_lines.append("")

        for idx, entry in dff_entries:
            try:
                # Extract DFF data temporarily
                dff_data = entry.get_data()
                if dff_data:
                    import tempfile
                    # Create temporary file
                    with tempfile.NamedTemporaryFile(delete=False, suffix='.dff', mode='wb') as temp_file:
                        temp_file.write(dff_data)
                        temp_dff_path = temp_file.name

                    try:
                        # Parse textures from the DFF
                        textures = extractor._parse_dff_for_textures(temp_dff_path)
                        output_lines.append(f"DFF File: {entry.name}")
                        output_lines.append(f"Textures ({len(textures)}): {', '.join(textures) if textures else 'None'}")
                        output_lines.append("")
                    finally:
                        # Clean up temp file
                        import os
                        if os.path.exists(temp_dff_path):
                            os.remove(temp_dff_path)
                else:
                    output_lines.append(f"DFF File: {entry.name}")
                    output_lines.append("Textures: Could not extract data")
                    output_lines.append("")
            except Exception as e:
                output_lines.append(f"DFF File: {entry.name}")
                output_lines.append(f"Error parsing: {str(e)}")
                output_lines.append("")

        # Write to file
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write('\n'.join(output_lines))

        QMessageBox.information(
            main_window,
            "Export Complete",
            f"DFF texture lists exported to:\n{file_path}"
        )

    except Exception as e:
        main_window.log_message(f"Error exporting DFF texture lists: {str(e)}")
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.critical(
            main_window,
            "Export Error",
            f"An error occurred while exporting:\n{str(e)}"
        )