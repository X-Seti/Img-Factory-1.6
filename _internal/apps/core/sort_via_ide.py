#this belongs in core/sort_via_ide.py - Version: 1
# X-Seti - December01 2025 - IMG Factory 1.5 - Sort Via IDE Function

"""
Sort Via IDE Function - Sorts IMG entries based on an IDE file
"""

import os
from typing import List, Dict
from PyQt6.QtWidgets import (
    QMessageBox, QFileDialog, QDialog, QVBoxLayout, QHBoxLayout, 
    QLabel, QPushButton, QTextEdit, QGroupBox, QCheckBox
)
from PyQt6.QtCore import Qt
from apps.methods.tab_system import get_current_file_from_active_tab, validate_tab_before_operation
from apps.methods.ide_parser_functions import parse_ide_file

##Methods list -
# sort_via_ide
# parse_ide_for_model_order
# sort_img_entries_by_ide
# integrate_sort_via_ide

def parse_ide_for_model_order(ide_path: str) -> List[str]:
    """Parse IDE file to extract model names in order"""
    try:
        # Use the existing IDE parser
        ide_parser = parse_ide_file(ide_path)
        
        if not ide_parser:
            return []
        
        # Extract model names from all sections that contain objects in order
        model_order = []
        
        # Process sections in order: objs, tobj, weap, cars, peds, ped, etc.
        for section_name in ['objs', 'tobj', 'weap', 'cars', 'peds', 'ped']:
            if section_name in ide_parser.sections:
                for entry in ide_parser.sections[section_name]:
                    if 'model' in entry:  # This is the model name from the parsed entry
                        model_name = entry['model']
                        # Add the model name to order list if not already present
                        if model_name.lower() not in [m.lower() for m in model_order]:
                            model_order.append(model_name)
        
        return model_order
        
    except Exception as e:
        print(f"Error parsing IDE file: {str(e)}")
        return []


def sort_img_entries_by_ide(main_window, model_order: List[str]):
    """Sort IMG entries based on the IDE model order with TXD files in alphabetical order at the bottom"""
    try:
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            return False
        
        if not hasattr(file_object, 'entries') or not file_object.entries:
            return False
        
        # Separate entries into models (non-TXD) and TXD files
        model_entries = []
        txd_entries = []
        
        for entry in file_object.entries:
            if entry.name.lower().endswith('.txd'):
                txd_entries.append(entry)
            else:
                model_entries.append(entry)
        
        # Sort models based on IDE order
        ordered_models = []
        remaining_models = []
        
        # First, add models in the order they appear in the IDE
        # Check for exact matches (with or without .dff extension)
        for model_name in model_order:
            matched_entry = None
            for entry in model_entries:
                entry_name = entry.name.lower()
                # Check for exact match or match with .dff extension
                if (entry_name == model_name.lower() or 
                    entry_name == f"{model_name.lower()}.dff" or
                    entry_name.replace('.dff', '') == model_name.lower()):
                    matched_entry = entry
                    break
            
            if matched_entry:
                ordered_models.append(matched_entry)
                model_entries.remove(matched_entry)
        
        # Add any remaining models that weren't in the IDE
        remaining_models = model_entries
        
        # Sort TXD files alphabetically
        txd_entries.sort(key=lambda x: x.name.lower())
        
        # Sort remaining models alphabetically
        remaining_models.sort(key=lambda x: x.name.lower())
        
        # Combine all entries: ordered models + remaining models + TXD files
        sorted_entries = ordered_models + remaining_models + txd_entries
        
        # Update the file object's entries
        file_object.entries = sorted_entries
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Sorted entries: {len(ordered_models)} from IDE, {len(remaining_models)} remaining models, {len(txd_entries)} TXD files")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Sort via IDE error: {str(e)}")
        return False


def sort_via_ide(main_window):
    """Sort IMG entries using an IDE file"""
    try:
        # Validate tab and get file object
        if not validate_tab_before_operation(main_window, "Sort Via IDE"):
            return False
        
        file_object, file_type = get_current_file_from_active_tab(main_window)
        
        if file_type != 'IMG' or not file_object:
            QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
            return False
        
        # Open file dialog to select IDE file
        ide_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Select IDE File for Sorting",
            "",
            "IDE Files (*.ide);;All Files (*.*)"
        )
        
        if not ide_path:
            return False  # User cancelled
        
        if not os.path.exists(ide_path):
            QMessageBox.critical(main_window, "File Error", f"IDE file does not exist: {ide_path}")
            return False
        
        # Parse the IDE file to get model order
        model_order = parse_ide_for_model_order(ide_path)
        
        if not model_order:
            QMessageBox.warning(main_window, "No Models", "No models found in IDE file or failed to parse")
            return False
        
        # Sort the IMG entries based on the IDE model order
        success = sort_img_entries_by_ide(main_window, model_order)
        
        if success:
            # Refresh the table to show the new order
            if hasattr(main_window, 'refresh_img_table'):
                main_window.refresh_img_table()
            elif hasattr(main_window, 'refresh_table'):
                main_window.refresh_table()
            
            QMessageBox.information(
                main_window, 
                "Sort Complete", 
                f"Sorted IMG entries using IDE file:\n{os.path.basename(ide_path)}\n\n"
                f"Models from IDE: {len(model_order)}\n"
                f"Entries reordered successfully."
            )
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"Sorted IMG using IDE: {os.path.basename(ide_path)}")
        else:
            QMessageBox.critical(main_window, "Sort Failed", "Failed to sort IMG entries using IDE file")
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Sort via IDE error: {str(e)}")
        QMessageBox.critical(main_window, "Sort Via IDE Error", f"Sort via IDE failed: {str(e)}")
        return False


def integrate_sort_via_ide(main_window) -> bool:
    """Integrate sort via IDE function into main window"""
    try:
        main_window.sort_via_ide = lambda: sort_via_ide(main_window)
        main_window.sort_by_ide = lambda: sort_via_ide(main_window)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("Sort via IDE function integrated")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Failed to integrate sort via IDE: {str(e)}")
        return False