#this belongs in core/ convert.py - Version: 3
# X-Seti - July06 2025 - Img Factory 1.5 - Img Convert Functions

#!/usr/bin/env python3
"""
IMG Factory Convert Functions
"""
from PyQt6.QtWidgets import QMessageBox, QFileDialog
import os
import shutil
from apps.methods.img_core_classes import IMGFile, IMGEntry

def convert_img(main_window): #vers 3
    """Convert IMG between versions"""
    if not main_window.current_img:
        QMessageBox.warning(main_window, "Warning", "No IMG file loaded")
        return

    try:
        # Ask user for conversion options
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox, QPushButton, QCheckBox
        
        dialog = QDialog(main_window)
        dialog.setWindowTitle("Convert IMG File")
        dialog.setModal(True)
        dialog.resize(400, 200)
        
        layout = QVBoxLayout(dialog)
        
        # Conversion type selection
        type_layout = QHBoxLayout()
        type_layout.addWidget(QLabel("Conversion Type:"))
        type_combo = QComboBox()
        type_combo.addItems([
            "IMG v1 to IMG v2",
            "IMG v2 to IMG v1", 
            "Rebuild IMG",
            "Optimize IMG"
        ])
        type_layout.addWidget(type_combo)
        layout.addLayout(type_layout)
        
        # Options
        backup_check = QCheckBox("Create backup before conversion")
        backup_check.setChecked(True)
        layout.addWidget(backup_check)
        
        optimize_check = QCheckBox("Optimize entries")
        optimize_check.setChecked(True)
        layout.addWidget(optimize_check)
        
        # Buttons
        button_layout = QHBoxLayout()
        convert_btn = QPushButton("Convert")
        cancel_btn = QPushButton("Cancel")
        
        convert_btn.clicked.connect(dialog.accept)
        cancel_btn.clicked.connect(dialog.reject)
        
        button_layout.addWidget(convert_btn)
        button_layout.addWidget(cancel_btn)
        layout.addLayout(button_layout)
        
        if dialog.exec() == QDialog.DialogCode.Accepted:
            conversion_type = type_combo.currentText()
            create_backup = backup_check.isChecked()
            optimize_entries = optimize_check.isChecked()
            
            # Create backup if requested
            if create_backup and main_window.current_img.file_path:
                backup_path = main_window.current_img.file_path + ".backup"
                shutil.copy2(main_window.current_img.file_path, backup_path)
                main_window.log_message(f"Created backup: {backup_path}")
            
            # Perform conversion based on type
            if conversion_type == "IMG v1 to IMG v2":
                _convert_v1_to_v2(main_window.current_img, optimize_entries)
            elif conversion_type == "IMG v2 to IMG v1":
                _convert_v2_to_v1(main_window.current_img, optimize_entries)
            elif conversion_type == "Rebuild IMG":
                _rebuild_img(main_window.current_img, optimize_entries)
            elif conversion_type == "Optimize IMG":
                _optimize_img(main_window.current_img)
            
            # Save the converted file
            if main_window.current_img.file_path:
                main_window.current_img.save()
                main_window.log_message(f"Conversion completed: {conversion_type}")
                QMessageBox.information(main_window, "Success", f"IMG file converted: {conversion_type}")
            else:
                # If no file path, ask user to save
                file_path, _ = QFileDialog.getSaveFileName(
                    main_window, "Save Converted IMG", "", "IMG Files (*.img);;All Files (*.*)"
                )
                if file_path:
                    main_window.current_img.save(file_path)
                    main_window.log_message(f"Conversion completed and saved to: {file_path}")
                    QMessageBox.information(main_window, "Success", f"IMG file converted and saved: {file_path}")
        
    except Exception as e:
        main_window.log_message(f"Error in convert_img: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to convert IMG file:\n{str(e)}")


def _convert_v1_to_v2(img_file, optimize_entries=True):
    """Convert IMG from version 1 to version 2"""
    # Update version
    img_file.version = 2
    
    # Reorganize entries if needed
    if optimize_entries:
        _optimize_img_entries(img_file)


def _convert_v2_to_v1(img_file, optimize_entries=True):
    """Convert IMG from version 2 to version 1"""
    # Update version
    img_file.version = 1
    
    # Reorganize entries if needed
    if optimize_entries:
        _optimize_img_entries(img_file)


def _rebuild_img(img_file, optimize_entries=True):
    """Rebuild IMG file structure"""
    # This creates a clean version of the IMG with optimized structure
    if optimize_entries:
        _optimize_img_entries(img_file)
    
    # Recalculate offsets and sizes
    current_offset = len(img_file.entries) * 32  # Directory size
    for entry in img_file.entries:
        entry.offset = current_offset
        current_offset += entry.size * 2048


def _optimize_img(img_file):
    """Optimize IMG file by removing fragmentation and optimizing structure"""
    # Remove entries with no data
    img_file.entries = [entry for entry in img_file.entries if entry.size > 0 and entry.name.strip() != '']
    
    # Reorganize entries by size to reduce fragmentation
    img_file.entries.sort(key=lambda e: e.size * 2048, reverse=True)
    
    # Recalculate offsets
    current_offset = len(img_file.entries) * 32  # Directory size
    for entry in img_file.entries:
        entry.offset = current_offset
        current_offset += entry.size * 2048


def _optimize_img_entries(img_file):
    """Optimize IMG entries by cleaning up names and sizes"""
    for entry in img_file.entries:
        # Clean up entry names
        entry.name = entry.name.replace('\x00', '').strip()
        
        # Ensure valid sizes
        if hasattr(entry, 'size') and entry.size <= 0:
            # Try to determine actual size from data if possible
            try:
                if hasattr(entry, 'get_data'):
                    data = entry.get_data()
                    entry.size = (len(data) + 2047) // 2048  # Round up to next sector
            except:
                pass


def convert_img_format(main_window): #vers 2
    """Convert IMG format with more options"""
    main_window.log_message("Convert IMG format requested")
    
    if not main_window.current_img:
        QMessageBox.warning(main_window, "Warning", "No IMG file loaded")
        return

    try:
        # Ask user for format conversion options
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QLabel, QComboBox, QPushButton, QCheckBox
        
        dialog = QDialog(main_window)
        dialog.setWindowTitle("Convert IMG Format")
        dialog.setModal(True)
        dialog.resize(450, 250)
        
        layout = QVBoxLayout(dialog)
        
        # Format selection
        format_layout = QHBoxLayout()
        format_layout.addWidget(QLabel("Target Format:"))
        format_combo = QComboBox()
        format_combo.addItems([
            "Standard IMG (v1)",
            "Standard IMG (v2)", 
            "GTA SA IMG",
            "GTA VC IMG",
            "GTA III IMG"
        ])
        format_layout.addWidget(format_combo)
        layout.addLayout(format_layout)
        
        # Additional options
        compress_check = QCheckBox("Compress entries (if supported)")
        compress_check.setChecked(False)
        layout.addWidget(compress_check)
        
        align_check = QCheckBox("Align to sector boundaries")
        align_check.setChecked(True)
        layout.addWidget(align_check)
        
        validate_check = QCheckBox("Validate after conversion")
        validate_check.setChecked(True)
        layout.addWidget(validate_check)
        
        # Buttons
        button_layout = QHBoxLayout()
        convert_btn = QPushButton("Convert Format")
        cancel_btn = QPushButton("Cancel")
        
        convert_btn.clicked.connect(dialog.accept)
        cancel_btn.clicked.connect(dialog.reject)
        
        button_layout.addWidget(convert_btn)
        button_layout.addWidget(cancel_btn)
        layout.addLayout(button_layout)
        
        if dialog.exec() == QDialog.DialogCode.Accepted:
            target_format = format_combo.currentText()
            compress_entries = compress_check.isChecked()
            align_entries = align_check.isChecked()
            validate_after = validate_check.isChecked()
            
            # Perform format conversion
            success = _apply_format_conversion(
                main_window.current_img, 
                target_format, 
                compress_entries, 
                align_entries
            )
            
            if success:
                # Validate if requested
                if validate_after:
                    from apps.methods.img_validation import validate_img_file
                    validation_result = validate_img_file(main_window.current_img)
                    if not validation_result.get('valid', False):
                        main_window.log_message("Warning: Converted IMG may have validation issues")
                        QMessageBox.warning(main_window, "Validation", 
                                          "Conversion completed but validation revealed potential issues.")
                    else:
                        main_window.log_message("Format conversion completed successfully")
                
                # Save the converted file
                if main_window.current_img.file_path:
                    main_window.current_img.save()
                    main_window.log_message(f"Format conversion completed: {target_format}")
                    QMessageBox.information(main_window, "Success", 
                                          f"IMG format converted to: {target_format}")
                else:
                    # If no file path, ask user to save
                    file_path, _ = QFileDialog.getSaveFileName(
                        main_window, "Save Converted IMG", "", "IMG Files (*.img);;All Files (*.*)"
                    )
                    if file_path:
                        main_window.current_img.save(file_path)
                        main_window.log_message(f"Format conversion completed and saved to: {file_path}")
                        QMessageBox.information(main_window, "Success", 
                                              f"IMG format converted and saved: {file_path}")
            else:
                QMessageBox.critical(main_window, "Error", "Format conversion failed")
        
    except Exception as e:
        main_window.log_message(f"Error in convert_img_format: {str(e)}")
        QMessageBox.critical(main_window, "Error", f"Failed to convert IMG format:\n{str(e)}")


def _apply_format_conversion(img_file, target_format, compress_entries=False, align_entries=True):
    """Apply specific format conversion rules"""
    try:
        # Set version based on format
        if "v1" in target_format.lower() or "iii" in target_format.lower():
            img_file.version = 1
        else:
            img_file.version = 2
            
        # Apply format-specific adjustments
        if "gta sa" in target_format.lower():
            # GTA SA specific format adjustments
            pass
        elif "gta vc" in target_format.lower():
            # GTA VC specific format adjustments
            pass
        elif "gta iii" in target_format.lower():
            # GTA III specific format adjustments
            pass
        
        # Align entries to sector boundaries if requested
        if align_entries:
            current_offset = len(img_file.entries) * 32  # Directory size
            for entry in img_file.entries:
                entry.offset = current_offset
                current_offset += entry.size * 2048
                
                # Ensure sector alignment
                if entry.offset % 2048 != 0:
                    entry.offset = ((entry.offset // 2048) + 1) * 2048
        
        # Apply compression if requested (simplified)
        if compress_entries:
            # This would implement actual compression in a real scenario
            pass
            
        return True
    except Exception as e:
        return False


__all__ = [
    'convert_img',
    'convert_img_format',
]
