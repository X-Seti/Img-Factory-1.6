#this belongs in gui/ tearoff_integration.py - Version: 1  
# X-Seti - August27 2025 - Img Factory 1.5

"""
Tear-Off Integration for Main Window  
Adds tear-off functionality to main IMG Factory window
"""

from PyQt6.QtWidgets import QPushButton, QHBoxLayout, QFrame, QWidget
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QIcon, QTextCursor
from apps.gui.imgcol_tearoff import create_img_tearoff_window, create_col_tearoff_window, create_editor_tearoff_window

##Methods list -
# add_tearoff_button_to_main_window
# create_tearoff_button  
# _on_tearoff_clicked
# _get_current_file_type
# _copy_img_table_data
# _copy_col_table_data
# integrate_tearoff_system

def add_tearoff_button_to_main_window(main_window): #vers 1
    """Add tear-off button to main window (green arrow location)"""
    try:
        # Find the location where the green arrow should go
        if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
            
            # Create tear-off button
            tearoff_button = create_tearoff_button(main_window)
            
            # Find parent widget of the table to add button
            table_parent =  tab_widget.parent()
            
            if table_parent and hasattr(table_parent, 'layout'):
                layout = table_parent.layout()

                #self.tab_widget.addTab

                if layout:
                    # Create button container
                    button_container = QFrame()
                    button_layout = QHBoxLayout(button_container)
                    button_layout.setContentsMargins(2, 2, 2, 2)
                    button_layout.addStretch()  # Push button to right
                    button_layout.addWidget(tearoff_button)
                    
                    # Insert at top of layout (before table)
                    layout.insertWidget(0, button_container)
                    
                    main_window.tearoff_button = tearoff_button
                    main_window.log_message("✅ Tear-off button added to main window")
                    return True
                    
        main_window.log_message("❌ Could not add tear-off button - table parent not found")
        return False
        
    except Exception as e:
        main_window.log_message(f"❌ Error adding tear-off button: {str(e)}")
        return False

def create_tearoff_button(main_window): #vers 1
    """Create the tear-off button (green arrow)"""
    button = QPushButton("↗️")  # Green arrow-like symbol
    button.setToolTip("Tear off IMG/COL window")
    button.setFixedSize(20, 20)
    button.setStyleSheet("""
        QPushButton {
            background-color: #4CAF50;
            border: 1px solid #45a049;
            border-radius: 12px;
            color: white;
            font-weight: bold;
        }
        QPushButton:hover {
            background-color: #45a049;
        }
        QPushButton:pressed {
            background-color: #3d8b40;
        }
    """)
    
    # Connect click handler
    button.clicked.connect(lambda: _on_tearoff_clicked(main_window))
    
    return button

def _on_tearoff_clicked(main_window): #vers 1
    """Handle tear-off button click"""
    try:
        file_type = _get_current_file_type(main_window)
        
        if file_type == "img":
            # Create IMG tear-off window
            img_window = create_img_tearoff_window(main_window, "Main IMG Archive")
            
            # Copy current table data
            entries_data = _copy_img_table_data(main_window)
            if entries_data:
                img_window.update_table_data(entries_data)
                filename = "Unknown"
                if hasattr(main_window, 'current_img') and main_window.current_img:
                    filename = main_window.current_img.file_path.split('/')[-1]
                img_window.info_label.setText(f"IMG: {filename}")
                
            img_window.show()
            img_window.raise_()
            main_window.log_message("📱 IMG window torn off")
            
        elif file_type == "col":
            # Create COL tear-off window  
            col_window = create_col_tearoff_window(main_window, "COL Collision Data")
            
            # Copy current COL data
            entries_data = _copy_col_table_data(main_window)
            if entries_data:
                col_window.update_table_data(entries_data)
                col_window.info_label.setText("COL: Collision Data")
                
            col_window.show()
            col_window.raise_()
            main_window.log_message("🚗 COL window torn off")
            
        else:
            # Generic tear-off window
            generic_window = create_editor_tearoff_window(main_window, "generic", "File Editor")
            generic_window.show()
            generic_window.raise_()
            main_window.log_message("📋 Generic window torn off")
            
    except Exception as e:
        main_window.log_message(f"❌ Tear-off error: {str(e)}")

def _get_current_file_type(main_window): #vers 1
    """Determine current file type (img/col/other)"""
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            return "img"
        elif hasattr(main_window, 'current_col') and main_window.current_col:
            return "col"
        else:
            return "generic"
    except:
        return "generic"

def _copy_img_table_data(main_window): #vers 1
    """Copy IMG table data from main window"""
    entries_data = []
    
    try:
        if hasattr(main_window, 'current_img') and main_window.current_img:
            for entry in main_window.current_img.entries:
                # Get additional data from table if available
                rw_address = "0x1003FFFF"  # Default GTA VC
                rw_version = "3.1.0.0 GTA VC (PC)"
                info = "Compression..."
                
                # Try to get data from actual table
                if hasattr(main_window.gui_layout, 'table'):
                    table = main_window.gui_layout.table
                    for row in range(table.rowCount()):
                        name_item = table.item(row, 0)
                        if name_item and name_item.text() == entry.name:
                            # Get RW Address (column 4)
                            rw_item = table.item(row, 4)
                            if rw_item:
                                rw_address = rw_item.text()
                            
                            # Get RW Version (column 5)
                            version_item = table.item(row, 5)
                            if version_item:
                                rw_version = version_item.text()
                                
                            # Get Info (column 6)
                            info_item = table.item(row, 6)
                            if info_item:
                                info = info_item.text()
                            break
                
                entries_data.append({
                    'name': entry.name,
                    'type': entry.name.split('.')[-1].upper() if '.' in entry.name else 'Unknown',
                    'size': f"{entry.size:,}",
                    'offset': f"0x{entry.offset:08X}",
                    'rw_address': rw_address,
                    'rw_version': rw_version,
                    'info': info,
                    'status': 'Original • Model'
                })
                
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error copying IMG data: {str(e)}")
    
    return entries_data

def _copy_col_table_data(main_window): #vers 1
    """Copy COL table data from main window"""
    entries_data = []
    
    try:
        if hasattr(main_window, 'current_col') and main_window.current_col:
            if hasattr(main_window.current_col, 'models'):
                for model in main_window.current_col.models:
                    entries_data.append({
                        'model_name': model.name if hasattr(model, 'name') else 'Unknown',
                        'spheres': len(model.spheres) if hasattr(model, 'spheres') else 0,
                        'boxes': len(model.boxes) if hasattr(model, 'boxes') else 0,
                        'faces': len(model.faces) if hasattr(model, 'faces') else 0,
                        'vertices': len(model.vertices) if hasattr(model, 'vertices') else 0,
                        'status': 'Ready'
                    })
            else:
                # Fallback - create sample data
                entries_data.append({
                    'model_name': 'Sample Model',
                    'spheres': 0,
                    'boxes': 0,
                    'faces': 0,
                    'vertices': 0,
                    'status': 'Ready'
                })
                
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error copying COL data: {str(e)}")
    
    return entries_data

def integrate_tearoff_system(main_window): #vers 1
    """Complete integration of tear-off system"""
    try:
        # Add tear-off button to main window
        success = add_tearoff_button_to_main_window(main_window)
        
        if success:
            # Store reference for later use
            main_window.tearoff_system_active = True
            main_window.log_message("✅ Tear-off system integrated successfully")
            return True
        else:
            main_window.log_message("❌ Failed to integrate tear-off system")
            return False
            
    except Exception as e:
        main_window.log_message(f"❌ Tear-off integration error: {str(e)}")
        return False
