"""
Sorting functionality for IMG Factory
Handles sorting IMG entries to match IDE model order and other sorting options
"""

import os
from typing import List, Dict, Any, Optional, Tuple
from PyQt6.QtWidgets import QTableWidgetItem


def sort_img_entries_to_match_ide(img_entries: List[Dict], ide_entries: List[Dict]) -> List[Dict]:
    """
    Sort IMG entries to match IDE model order, with TXDs at the bottom
    
    Args:
        img_entries: List of IMG entry dictionaries
        ide_entries: List of IDE entry dictionaries
    
    Returns:
        List of sorted IMG entries
    """
    # Create a mapping of model names to their order in the IDE file
    ide_model_order = {}
    for idx, ide_entry in enumerate(ide_entries):
        model_name = ide_entry.get('model_name', '').lower()
        if model_name:
            # Store the order index for each model
            ide_model_order[model_name] = idx
    
    # Separate DFF models and other entries (TXDs, etc.)
    dff_models = []
    txd_files = []
    other_files = []
    
    for entry in img_entries:
        filename = entry.get('name', '').lower()
        if filename.endswith('.dff'):
            dff_models.append(entry)
        elif filename.endswith('.txd'):
            txd_files.append(entry)
        else:
            other_files.append(entry)
    
    # Sort DFF models based on IDE order
    def get_ide_order_key(dff_entry):
        model_name = dff_entry.get('name', '').rsplit('.', 1)[0].lower()  # Remove extension
        # If model exists in IDE, return its position; otherwise, return a high number
        return ide_model_order.get(model_name, float('inf'))
    
    # Sort DFF models by IDE order
    dff_models.sort(key=get_ide_order_key)
    
    # Combine all entries: DFF models first (sorted by IDE), then other files, then TXDs at the bottom
    sorted_entries = dff_models + other_files + txd_files
    
    return sorted_entries


def sort_img_entries_by_name(img_entries: List[Dict]) -> List[Dict]:
    """
    Sort IMG entries alphabetically by name, with TXDs at the bottom
    
    Args:
        img_entries: List of IMG entry dictionaries
    
    Returns:
        List of sorted IMG entries
    """
    # Separate DFF models and other entries (TXDs, etc.)
    dff_models = []
    txd_files = []
    other_files = []
    
    for entry in img_entries:
        filename = entry.get('name', '').lower()
        if filename.endswith('.dff'):
            dff_models.append(entry)
        elif filename.endswith('.txd'):
            txd_files.append(entry)
        else:
            other_files.append(entry)
    
    # Sort each group
    dff_models.sort(key=lambda x: x.get('name', '').lower())
    other_files.sort(key=lambda x: x.get('name', '').lower())
    txd_files.sort(key=lambda x: x.get('name', '').lower())
    
    # Combine: DFF models first, then other files, then TXDs at the bottom
    return dff_models + other_files + txd_files


def sort_entries_in_table(table_widget, sort_order: str = "name", ide_entries: Optional[List[Dict]] = None):
    """
    Sort entries in the table widget based on specified criteria
    
    Args:
        table_widget: QTableWidget containing the entries
        sort_order: Type of sorting ("name", "ide_order", etc.)
        ide_entries: List of IDE entries for IDE-based sorting
    """
    if not table_widget or table_widget.rowCount() == 0:
        return
    
    # Get all entries from the table
    entries = []
    for row in range(table_widget.rowCount()):
        entry = {}
        for col in range(table_widget.columnCount()):
            item = table_widget.item(row, col)
            if item:
                header = table_widget.horizontalHeaderItem(col).text() if table_widget.horizontalHeaderItem(col) else f"Col{col}"
                entry[header.lower()] = item.text()
            else:
                header = table_widget.horizontalHeaderItem(col).text() if table_widget.horizontalHeaderItem(col) else f"Col{col}"
                entry[header.lower()] = ""
        entries.append(entry)
    
    # Apply sorting based on the requested method
    if sort_order == "ide_order" and ide_entries:
        sorted_entries = sort_img_entries_to_match_ide(entries, ide_entries)
    else:
        sorted_entries = sort_img_entries_by_name(entries)
    
    # Clear the table and repopulate with sorted entries
    table_widget.setRowCount(0)  # Clear all rows
    
    for row_idx, entry in enumerate(sorted_entries):
        table_widget.insertRow(row_idx)
        for col_idx, header in enumerate([
            "Num", "Name", "Extension", "Size", "Hash", "Hex", "Version", "Compression", "Status"
        ]):
            value = entry.get(header.lower(), "")
            item = QTableWidgetItem(str(value))
            table_widget.setItem(row_idx, col_idx, item)


def get_associated_ide_file(img_path: str) -> Optional[str]:
    """
    Find the associated IDE file for an IMG file in the same directory
    
    Args:
        img_path: Path to the IMG file
    
    Returns:
        Path to the associated IDE file, or None if not found
    """
    if not img_path or not os.path.exists(img_path):
        return None
    
    img_dir = os.path.dirname(img_path)
    img_name = os.path.splitext(os.path.basename(img_path))[0]  # Get name without extension
    
    # Look for IDE file with same name in the same directory
    possible_ide_paths = [
        os.path.join(img_dir, f"{img_name}.ide"),
        os.path.join(img_dir, f"{img_name.upper()}.ide"),
        os.path.join(img_dir, f"{img_name.lower()}.ide")
    ]
    
    for ide_path in possible_ide_paths:
        if os.path.exists(ide_path):
            return ide_path
    
    return None


def parse_ide_file(ide_path: str) -> List[Dict]:
    """
    Parse an IDE file and extract model entries
    
    Args:
        ide_path: Path to the IDE file
    
    Returns:
        List of IDE entry dictionaries
    """
    ide_entries = []
    
    if not os.path.exists(ide_path):
        return ide_entries
    
    try:
        with open(ide_path, 'r', encoding='utf-8', errors='ignore') as f:
            lines = f.readlines()
        
        current_section = ""
        for line in lines:
            line = line.strip()
            if not line or line.startswith(';') or line.startswith('#'):
                continue  # Skip comments and empty lines
            
            # Check for section headers
            if line.upper() in ['OBJS', 'TOBJ', 'ANIM', 'WEAP', 'PEDS', 'CARS', '2DFX', 'PATH', 'GRGE', 'INST']:
                current_section = line.upper()
                continue
            
            # Skip end markers
            if line.upper() == 'END':
                current_section = ""
                continue
            
            # Parse entry based on section
            if current_section and line:
                parts = line.split(',')
                if len(parts) >= 2:  # At least model name and txd name
                    entry = {
                        'section': current_section,
                        'model_name': parts[0].strip().lower(),
                        'txd_name': parts[1].strip().lower() if len(parts) > 1 else "",
                        'line': line
                    }
                    ide_entries.append(entry)
    
    except Exception as e:
        print(f"Error parsing IDE file {ide_path}: {e}")
    
    return ide_entries