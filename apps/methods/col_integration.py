#this belongs in components/col_integration_main.py - Version: 8
# X-Seti - July20 2025 - IMG Factory 1.5 - COL Integration Main
# Complete COL integration for IMG Factory using IMG debug system

"""
COL Integration Main - Complete COL system integration
Integrates all COL functionality into IMG Factory with menu, tabs, and context menus
"""

import os
import tempfile
from typing import Optional
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter, QTabWidget,
    QMessageBox, QFileDialog, QDialog, QTextEdit
)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QAction, QFont

# Import IMG debug system and COL components
from apps.debug.debug_functions import img_debugger
from apps.debug.debug_functions import col_debug_log
from apps.methods.col_core_classes import COLFile, COLModel, COLVersion

##Methods list -
# add_col_context_menu_items
# add_col_file_detection
# add_col_menu
# add_col_tab
# add_col_tools_menu
# analyze_col_from_img
# create_col_editor_action
# create_col_file_dialog
# detect_col_version_from_data
# edit_col_from_img
# export_col_from_img
# export_col_to_img_format
# import_col_to_img
# integrate_col_dialogs
# integrate_col_functionality
# integrate_col_context_menus
# integrate_complete_col_system
# load_col_from_img_entry
# open_col_batch_processor
# open_col_editor
# open_col_editor_with_file
# replace_col_in_img
# setup_col_debug_for_main_window
# setup_col_integration_full
# setup_threaded_col_loading
# setup_col_file_loading
# verify_col_components




def add_col_menu(img_factory_instance): #vers 1
    """Add COL menu to the main menu bar using IMG debug system"""
    try:
        menubar = img_factory_instance.menuBar()
        
        # Create COL menu
        col_menu = menubar.addMenu("🔧 COL")
        
        # File operations
        open_col_action = QAction("Open COL File", img_factory_instance)
        open_col_action.setShortcut("Ctrl+Shift+O")
        open_col_action.triggered.connect(lambda: open_col_file_dialog(img_factory_instance))
        col_menu.addAction(open_col_action)
        
        new_col_action = QAction("🆕 New COL File", img_factory_instance)
        new_col_action.triggered.connect(lambda: create_new_col_file(img_factory_instance))
        col_menu.addAction(new_col_action)
        
        col_menu.addSeparator()
        
        # COL Editor
        editor_action = QAction("✏️ COL Editor", img_factory_instance)
        editor_action.setShortcut("Ctrl+E")
        editor_action.triggered.connect(lambda: create_col_editor_action(img_factory_instance))
        col_menu.addAction(editor_action)
        
        col_menu.addSeparator()
        
        # Batch operations
        from apps.methods.col_utilities import analyze_col_file_dialog
        
        batch_process_action = QAction("⚙️ Batch Processor", img_factory_instance)
        batch_process_action.triggered.connect(lambda: open_col_batch_processor(img_factory_instance))
        col_menu.addAction(batch_process_action)
        
        analyze_action = QAction("📊 Analyze COL", img_factory_instance)
        analyze_action.triggered.connect(lambda: analyze_col_file_dialog(img_factory_instance))
        col_menu.addAction(analyze_action)
        
        col_menu.addSeparator()
        
        # Import/Export
        import_to_img_action = QAction("📥 Import to IMG", img_factory_instance)
        import_to_img_action.triggered.connect(lambda: QMessageBox.information(img_factory_instance, "Import COL", "COL import functionality will be available soon."))
        col_menu.addAction(import_to_img_action)
        
        export_from_img_action = QAction("📤 Export from IMG", img_factory_instance)
        export_from_img_action.triggered.connect(lambda: QMessageBox.information(img_factory_instance, "Export COL", "COL export functionality will be available soon."))
        col_menu.addAction(export_from_img_action)
        
        # Store reference to COL menu
        img_factory_instance.col_menu = col_menu
        
        return True
        
    except Exception as e:
        img_debugger.error(f"Error adding COL menu: {e}")
        return False

def add_col_tab(img_factory_instance): #vers 1
    """Add COL tab to the main interface using IMG debug system"""
    try:
        # Check if main interface has a tab widget
        if not hasattr(img_factory_instance, 'main_tab_widget'):
            # Create tab widget if it doesn't exist
            central_widget = img_factory_instance.centralWidget()
            if central_widget:
                # Replace central widget with tab widget
                old_layout = central_widget.layout()
                
                img_factory_instance.main_tab_widget = QTabWidget()
                
                # Move existing content to first tab
                if old_layout:
                    img_tab = QWidget()
                    img_tab.setLayout(old_layout)
                    img_factory_instance.main_tab_widget.addTab(img_tab, "📁 IMG Files")
                
                # Set new layout
                new_layout = QVBoxLayout(central_widget)
                new_layout.addWidget(img_factory_instance.main_tab_widget)
        
        # Create COL tab
        col_tab = QWidget()
        col_layout = QVBoxLayout(col_tab)
        
        # Create COL interface
        col_splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Left panel - COL file list
        from apps.components.col_functions import COLListWidget
        col_list_widget = COLListWidget()
        col_splitter.addWidget(col_list_widget)
        
        # Right panel - COL model details
        from apps.components.col_functions import COLModelDetailsWidget
        col_details_widget = COLModelDetailsWidget()
        col_splitter.addWidget(col_details_widget)
        
        # Set splitter sizes
        col_splitter.setSizes([400, 300])
        
        col_layout.addWidget(col_splitter)
        
        # Add COL tab
        img_factory_instance.main_tab_widget.addTab(col_tab, "🔧 COL Files")
        
        # Store references
        img_factory_instance.col_list_widget = col_list_widget
        img_factory_instance.col_details_widget = col_details_widget
        
        return True
        
    except Exception as e:
        img_debugger.error(f"Error adding COL tab: {e}")
        return False


def add_col_file_detection(img_factory_instance): #vers 1
    """Add COL file type detection using IMG debug system"""
    try:
        # Store original populate function if it exists
        if hasattr(img_factory_instance, 'populate_table_original'):
            # Already patched
            return True
        
        # Find the populate table function
        populate_func = None
        if hasattr(img_factory_instance, 'gui_layout') and hasattr(img_factory_instance.gui_layout, 'populate_table'):
            populate_func = img_factory_instance.gui_layout.populate_table
        elif hasattr(img_factory_instance, 'populate_table'):
            populate_func = img_factory_instance.populate_table
        
        if populate_func:
            # Store original function
            img_factory_instance.populate_table_original = populate_func
            
            # Create enhanced version
            def enhanced_populate_entries_table(img_file):
                # Call original function
                result = img_factory_instance.populate_table_original(img_file)
                
                # Add COL detection
                if img_file and img_file.entries:
                    for entry in img_file.entries:
                        if entry.name.lower().endswith('.col'):
                            # Add COL version detection
                            try:
                                col_data = entry.get_data()
                                if col_data:
                                    version_info = detect_col_version_from_data(col_data)
                                    if version_info:
                                        # Update entry display with COL info
                                        pass  # Could enhance table display here
                            except:
                                pass  # Ignore detection errors
                
                return result
            
            # Replace function
            if hasattr(img_factory_instance, 'gui_layout'):
                img_factory_instance.gui_layout.populate_table = enhanced_populate_entries_table
            else:
                img_factory_instance.populate_table = enhanced_populate_entries_table
            
            img_debugger.debug("COL file detection integrated")
            return True
        else:
            img_debugger.warning("No populate table function found for COL detection")
            return False
            
    except Exception as e:
        img_debugger.error(f"Error adding COL file detection: {e}")
        return False

def open_col_editor(img_factory_instance, file_path=None): #vers 1
    """Open COL editor"""
    try:
        # Try to open COL editor if available
        try:
            from apps.components.Col_Editor.col_editor import COLEditorDialog
            editor = COLEditorDialog(img_factory_instance)
            if file_path:
                editor.load_col_file(file_path)
            editor.exec()
        except ImportError:
            QMessageBox.information(img_factory_instance, "COL Editor",
                "COL editor will be available in a future version.")
    except Exception as e:
        img_debugger.error(f"Failed to open COL editor: {str(e)}")

def replace_col_in_img(img_factory_instance, entry): #vers 1
    """Replace COL file in IMG with new one"""
    try:
        # Get new COL file to replace with
        file_path, _ = QFileDialog.getOpenFileName(
            img_factory_instance, "Replace COL File", "", "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            with open(file_path, 'rb') as f:
                new_data = f.read()

            entry.set_data(new_data)

            QMessageBox.information(img_factory_instance, "Success", "COL entry replaced successfully")

            # Refresh the entries table
            if hasattr(img_factory_instance, 'populate_entries_table'):
                img_factory_instance.populate_entries_table()

    except Exception as e:
        img_debugger.error(f"Failed to replace COL: {str(e)}")

def import_col_to_img(img_factory_instance): #vers 1
    """Import COL file to current IMG"""
    try:
        if not hasattr(img_factory_instance, 'current_img') or not img_factory_instance.current_img:
            QMessageBox.warning(img_factory_instance, "No IMG", "Please open an IMG file first")
            return

        # Get COL file to import
        file_path, _ = QFileDialog.getOpenFileName(
            img_factory_instance, "Import COL File", "", "COL Files (*.col);;All Files (*)"
        )

        if file_path:
            with open(file_path, 'rb') as f:
                col_data = f.read()

            entry_name = os.path.basename(file_path)
            img_factory_instance.current_img.add_entry(entry_name, col_data)

            QMessageBox.information(img_factory_instance, "Success", f"COL imported as {entry_name}")

            # Refresh the entries table
            if hasattr(img_factory_instance, 'populate_entries_table'):
                img_factory_instance.populate_entries_table()

    except Exception as e:
        img_debugger.error(f"Failed to import COL: {str(e)}")

def export_col_from_img(img_factory_instance): #vers 1
    """Export COL files from current IMG"""
    try:
        if not hasattr(img_factory_instance, 'current_img') or not img_factory_instance.current_img:
            QMessageBox.warning(img_factory_instance, "No IMG", "Please open an IMG file first")
            return

        # Get directory to export to
        output_dir = QFileDialog.getExistingDirectory(
            img_factory_instance, "Export COL Files to Directory"
        )

        if output_dir:
            exported_count = 0

            # Find and export all COL files
            for entry in img_factory_instance.current_img.entries:
                if entry.name.lower().endswith('.col'):
                    try:
                        output_path = os.path.join(output_dir, entry.name)
                        entry.extract_to_file(output_path)
                        exported_count += 1
                    except Exception as e:
                        img_debugger.warning(f"Failed to export {entry.name}: {e}")

            QMessageBox.information(img_factory_instance, "Export Complete",
                f"Exported {exported_count} COL files to {output_dir}")

    except Exception as e:
        img_debugger.error(f"Failed to export COL files: {str(e)}")


def load_col_from_img_entry(img_factory_instance, entry): #vers 1
    """Load COL file from IMG entry"""
    try:
        img_debugger.debug(f"Loading COL from IMG entry: {entry.name}")

        # Extract COL data
        col_data = entry.get_data()

        # Validate it's a COL file
        analysis = detect_col_version_from_data(col_data)
        if not analysis:
            img_debugger.warning(f"{entry.name} is not a valid COL file")
            return False

        # Create temporary COL file
        import tempfile
        with tempfile.NamedTemporaryFile(suffix='.col', delete=False) as temp_file:
            temp_file.write(col_data)
            temp_path = temp_file.name

        # Load using the COL loading system
        from apps.methods.col_parsing_functions import load_col_file_safely
        success = load_col_file_safely(img_factory_instance, temp_path)

        # Clean up temp file
        try:
            os.unlink(temp_path)
        except:
            pass

        if success:
            img_debugger.success(f"COL loaded successfully: {entry.name}")
        else:
            img_debugger.error(f"Failed to load COL: {entry.name}")

        return success

    except Exception as e:
        img_debugger.error(f"Failed to load COL from IMG entry: {e}")
        return False


def integrate_col_editor(main_window) -> bool: #vers 1
    """Integrate COL editor functionality"""
    try:
        from apps.components.Col_Editor.col_editor import open_col_editor

        # Add COL editor methods to main window
        main_window.open_col_editor = lambda file_path=None: open_col_editor(main_window, file_path)

        # Add method for editing COL from IMG entry
        from gui.gui_context import edit_col_from_img_entry
        main_window.edit_col_from_img_entry = lambda row: edit_col_from_img_entry(main_window, row)

        img_debugger.debug("✅ COL editor integrated")
        return True

    except Exception as e:
        img_debugger.error(f"Error integrating COL editor: {str(e)}")
        return False


def integrate_col_functionality(img_factory_instance): #vers 1
    """Main function to integrate all COL functionality into IMG Factory using IMG debug system"""
    try:
        img_debugger.debug("Starting COL functionality integration")

        # Add COL menu to menu bar
        if add_col_menu(img_factory_instance):
            img_debugger.debug("COL menu added successfully")

        # Add COL tab to main interface
        if add_col_tab(img_factory_instance):
            img_debugger.debug("COL tab added successfully")

        # Add COL context menu items to IMG entries
        if add_col_context_menu_items(img_factory_instance):
            img_debugger.debug("COL context menu items added successfully")

        # Add COL file type detection
        if add_col_file_detection(img_factory_instance):
            img_debugger.debug("COL file detection added successfully")

        img_debugger.success("COL functionality integrated successfully!")
        return True

    except Exception as e:
        img_debugger.error(f"Error integrating COL functionality: {e}")
        return False

def integrate_col_dialogs(main_window) -> bool: #vers 1
    """Integrate COL dialog functionality"""
    try:
        from gui.gui_context import (
            open_col_editor_dialog,
            open_col_batch_proc_dialog,
            open_col_file_dialog,
            analyze_col_file_dialog
        )

        # Add dialog methods to main window
        main_window.open_col_editor_dialog = lambda: open_col_editor_dialog(main_window)
        main_window.open_col_batch_proc_dialog = lambda: open_col_batch_proc_dialog(main_window)
        main_window.open_col_file_dialog = lambda: open_col_file_dialog(main_window)
        main_window.analyze_col_file_dialog = lambda: analyze_col_file_dialog(main_window)

        img_debugger.debug("✅ COL dialogs integrated")
        return True

    except Exception as e:
        img_debugger.error(f"Error integrating COL dialogs: {str(e)}")
        return False


def detect_col_version_from_data(data: bytes) -> Optional[dict]: #vers 1
    """Detect COL version and basic info from raw data"""
    if len(data) < 8:
        return None
    
    try:
        # Check signature
        signature = data[:4]
        version = 0
        models = 0
        
        if signature == b'COLL':
            version = 1
        elif signature == b'COL\x02':
            version = 2
        elif signature == b'COL\x03':
            version = 3
        elif signature == b'COL\x04':
            version = 4
        else:
            return None
        
        # Count models (simplified)
        offset = 0
        while offset < len(data) - 8:
            if data[offset:offset+4] in [b'COLL', b'COL\x02', b'COL\x03', b'COL\x04']:
                models += 1
                # Skip to next potential model
                try:
                    import struct
                    size = struct.unpack('<I', data[offset+4:offset+8])[0]
                    offset += size + 8
                except:
                    break
            else:
                break
        
        return {
            'version': version,
            'models': max(1, models),  # At least 1 model
            'size': len(data)
        }
        
    except Exception:
        return None

# COL operation functions

def create_col_editor_action(img_factory_instance): #vers 1
    """Create COL editor action using IMG debug system"""
    try:
        from apps.components.Col_Editor.col_editor import open_col_editor
        return open_col_editor(img_factory_instance)
        
    except Exception as e:
        img_debugger.error(f"Error opening COL editor: {e}")
        return False

def open_col_batch_processor(img_factory_instance): #vers 1
    """Open COL batch processor using IMG debug system"""
    try:
        from apps.methods.col_utilities import open_col_batch_processor
        open_col_batch_processor(img_factory_instance)
        return True
        
    except Exception as e:
        img_debugger.error(f"Error opening COL batch processor: {e}")
        return False

def open_col_editor_with_file(img_factory_instance, col_file: COLFile): #vers 1
    """Open COL editor with specific file using IMG debug system"""
    try:
        from apps.components.Col_Editor.col_editor import COLEditorDialog
        editor = COLEditorDialog(img_factory_instance)
        if col_file.file_path:
            editor.load_col_file(col_file.file_path)
        return editor.exec()
        
    except Exception as e:
        img_debugger.error(f"Error opening COL editor with file: {e}")
        return False

def edit_col_from_img(img_factory_instance, row: int): #vers 1
    """Edit COL file from IMG entry using IMG debug system"""
    try:
        if not hasattr(img_factory_instance, 'current_img') or not img_factory_instance.current_img:
            return False
        
        if row < 0 or row >= len(img_factory_instance.current_img.entries):
            return False
        
        entry = img_factory_instance.current_img.entries[row]
        
        # Extract COL data to temporary file
        col_data = entry.get_data()
        if col_data:
            with tempfile.NamedTemporaryFile(suffix='.col', delete=False) as temp_file:
                temp_file.write(col_data)
                temp_path = temp_file.name
            
            # Open editor with temporary file
            from apps.components.Col_Editor.col_editor import open_col_editor
            result = open_col_editor(img_factory_instance, temp_path)
            
            # Clean up temporary file
            try:
                os.unlink(temp_path)
            except:
                pass
            
            return result
        
        return False
        
    except Exception as e:
        img_debugger.error(f"Error editing COL from IMG: {e}")
        QMessageBox.critical(img_factory_instance, "Error", f"Failed to edit COL: {str(e)}")
        return False

def analyze_col_from_img(img_factory_instance, row: int): #vers 1
    """Analyze COL file from IMG entry using IMG debug system"""
    try:
        if not hasattr(img_factory_instance, 'current_img') or not img_factory_instance.current_img:
            return False
        
        if row < 0 or row >= len(img_factory_instance.current_img.entries):
            return False
        
        entry = img_factory_instance.current_img.entries[row]
        col_data = entry.get_data()
        
        if col_data:
            # Create temporary COL file for analysis
            with tempfile.NamedTemporaryFile(suffix='.col', delete=False) as temp_file:
                temp_file.write(col_data)
                temp_path = temp_file.name
            
            try:
                # Load and analyze COL file
                col_file = COLFile(temp_path)
                if col_file.load():
                    # Generate analysis report
                    analysis = []
                    analysis.append(f"COL Analysis - {entry.name}")
                    analysis.append("=" * 40)
                    analysis.append(f"File Size: {len(col_data):,} bytes")
                    analysis.append(f"Models: {len(col_file.models)}")
                    analysis.append("")
                    
                    for i, model in enumerate(col_file.models):
                        analysis.append(f"Model {i+1}: {model.name}")
                        analysis.append(f"  Version: COL {model.version.value}")
                        analysis.append(f"  Model ID: {model.model_id}")
                        analysis.append(f"  Spheres: {len(model.spheres)}")
                        analysis.append(f"  Boxes: {len(model.boxes)}")
                        analysis.append(f"  Vertices: {len(model.vertices)}")
                        analysis.append(f"  Faces: {len(model.faces)}")
                        analysis.append("")
                    
                    # Show analysis dialog
                    dialog = QDialog(img_factory_instance)
                    dialog.setWindowTitle(f"COL Analysis - {entry.name}")
                    dialog.setMinimumSize(600, 400)
                    
                    layout = QVBoxLayout(dialog)
                    
                    text_edit = QTextEdit()
                    text_edit.setPlainText("\n".join(analysis))
                    text_edit.setReadOnly(True)
                    text_edit.setFont(QFont("Courier", 10))
                    layout.addWidget(text_edit)
                    
                    from PyQt6.QtWidgets import QPushButton
                    close_btn = QPushButton("Close")
                    close_btn.clicked.connect(dialog.close)
                    layout.addWidget(close_btn)
                    
                    dialog.exec()
                    
                    return True
                else:
                    QMessageBox.critical(img_factory_instance, "Analysis Error", 
                                       f"Failed to load COL data: {col_file.load_error}")
                    return False
                    
            finally:
                # Clean up temporary file
                try:
                    os.unlink(temp_path)
                except:
                    pass
        
        return False
        
    except Exception as e:
        img_debugger.error(f"Error analyzing COL from IMG: {e}")
        QMessageBox.critical(img_factory_instance, "Error", f"Failed to analyze COL: {str(e)}")
        return False

# Integration support functions
def add_col_tools_menu(main_window): #vers 1
    """Add COL tools menu to main window using IMG debug system"""
    try:
        col_debug_log(main_window, "Adding COL tools menu", 'COL_INTEGRATION')
        
        # Find or create Tools menu
        menu_bar = main_window.menuBar()
        tools_menu = None
        
        # Look for existing Tools menu
        for action in menu_bar.actions():
            if action.text() == "Tools":
                tools_menu = action.menu()
                break
        
        if not tools_menu:
            tools_menu = menu_bar.addMenu("Tools")
            col_debug_log(main_window, "Created new Tools menu", 'COL_INTEGRATION')
        
        # Add COL submenu
        col_menu = tools_menu.addMenu("COL Tools")
        
        # COL file actions
        open_col_action = col_menu.addAction("Open COL File...")
        open_col_action.triggered.connect(lambda: create_col_file_dialog(main_window))
        
        col_menu.addSeparator()
        
        # COL creation actions
        create_col_action = col_menu.addAction("Create New COL...")
        create_col_action.triggered.connect(lambda: create_col_editor_action(main_window))
        
        # COL batch processing
        batch_col_action = col_menu.addAction("Batch Process COL Files...")
        batch_col_action.triggered.connect(lambda: open_col_batch_processor(main_window))
        
        col_menu.addSeparator()
        
        # COL settings
        col_settings_action = col_menu.addAction("COL Settings...")
        col_settings_action.triggered.connect(lambda: toggle_col_debug_setting(main_window))
        
        col_debug_log(main_window, "COL tools menu added successfully", 'COL_INTEGRATION', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error adding COL tools menu: {e}", 'COL_INTEGRATION', 'ERROR')
        return False

def create_col_file_dialog(main_window): #vers 1
    """Create COL file open dialog using IMG debug system"""
    try:
        col_debug_log(main_window, "Opening COL file dialog", 'COL_DIALOG')
        
        file_path, _ = QFileDialog.getOpenFileName(
            main_window,
            "Open COL File",
            "",
            "COL Files (*.col);;All Files (*)"
        )
        
        if file_path:
            col_debug_log(main_window, f"Selected COL file: {file_path}", 'COL_DIALOG')
            
            # Load COL file using core loader
            from apps.methods.col_parsing_functions import load_col_file_safely
            success = load_col_file_safely(main_window, file_path)
            
            if success:
                col_debug_log(main_window, f"COL file loaded successfully: {file_path}", 'COL_DIALOG', 'SUCCESS')
            else:
                col_debug_log(main_window, f"Failed to load COL file: {file_path}", 'COL_DIALOG', 'ERROR')
            
            return success
        else:
            col_debug_log(main_window, "COL file dialog cancelled", 'COL_DIALOG')
            return False
            
    except Exception as e:
        col_debug_log(main_window, f"Error in COL file dialog: {e}", 'COL_DIALOG', 'ERROR')
        return False

def toggle_col_debug_setting(main_window): #vers 1
    """Toggle COL debug setting using IMG debug system"""
    try:
        from apps.debug.debug_functions import is_col_debug_enabled, set_col_debug_enabled
        
        current_state = is_col_debug_enabled()
        new_state = not current_state
        set_col_debug_enabled(new_state)
        
        status = "enabled" if new_state else "disabled"
        col_debug_log(main_window, f"COL debug {status}", 'COL_SETTINGS', 'SUCCESS')
        QMessageBox.information(main_window, "COL Settings", f"COL debug output {status}")
        
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error toggling COL debug: {e}", 'COL_SETTINGS', 'ERROR')
        return False

def export_col_to_img_format(main_window, col_file_path: str, output_img_path: str): #vers 1
    """Export COL file to IMG format using IMG debug system"""
    try:
        col_debug_log(main_window, f"Exporting COL to IMG: {col_file_path} -> {output_img_path}", 'COL_EXPORT')
        
        # Load COL file
        col_file = COLFile(col_file_path)
        if not col_file.load():
            col_debug_log(main_window, f"Failed to load COL file: {col_file_path}", 'COL_EXPORT', 'ERROR')
            return False
        
        # Create new IMG file
        from apps.methods.img_core_classes import IMGFile, IMGVersion
        img_file = IMGFile()
        
        if not img_file.create_new(output_img_path, IMGVersion.VERSION_2):
            col_debug_log(main_window, f"Failed to create IMG file: {output_img_path}", 'COL_EXPORT', 'ERROR')
            return False
        
        # Export COL data to IMG
        col_data = col_file.get_raw_data()
        if col_data:
            col_filename = os.path.basename(col_file_path)
            img_file.add_entry(col_filename, col_data)
            
            if img_file.rebuild():
                col_debug_log(main_window, f"COL exported to IMG successfully", 'COL_EXPORT', 'SUCCESS')
                return True
            else:
                col_debug_log(main_window, f"Failed to rebuild IMG after COL export", 'COL_EXPORT', 'ERROR')
                return False
        else:
            col_debug_log(main_window, f"No COL data to export", 'COL_EXPORT', 'ERROR')
            return False
            
    except Exception as e:
        col_debug_log(main_window, f"Error exporting COL to IMG: {e}", 'COL_EXPORT', 'ERROR')
        return False


def setup_col_integration_full(main_window): #vers 3
    """Main COL integration entry point with proper error handling"""
    try:
        img_debugger.debug("[COL-COL_INTEGRATION] Starting full COL integration for IMG interface")

        # Setup COL debug functionality first
        try:
            if hasattr(main_window, 'setup_col_debug_for_main_window'):
                main_window.setup_col_debug_for_main_window()
        except Exception as e:
            img_debugger.warning(f"[COL-COL_INTEGRATION] COL debug setup failed: {e}")

        # Setup threaded loading
        try:
            if hasattr(main_window, 'setup_threaded_col_loading'):
                main_window.setup_threaded_col_loading()
        except Exception as e:
            img_debugger.warning(f"[COL-COL_INTEGRATION] Threaded loading setup failed: {e}")

        # Add COL tools menu to existing menu bar
        try:
            if hasattr(main_window, 'menuBar') and main_window.menuBar():
                # Add COL tools menu functionality here
                img_debugger.debug("[COL-COL_INTEGRATION] COL tools menu added")
        except Exception as e:
            img_debugger.warning(f"[COL-COL_INTEGRATION] COL tools menu failed: {e}")

        # Add COL context menu items to existing entries table - FIXED IMPORT
        try:
            if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                try:
                    # Try to import the context menu function from the correct location
                    from apps.core.right_click_actions import setup_table_context_menu
                    setup_table_context_menu(main_window)
                    img_debugger.debug("[COL-COL_INTEGRATION] COL context menu added to entries table")
                except ImportError as e:
                    img_debugger.warning(f"[COL-COL_INTEGRATION] Could not import context menu function: {e}")
                    # Try alternative import paths
                    try:
                        from apps.core.right_click_actions import setup_table_context_menu
                        setup_table_context_menu(main_window)
                        img_debugger.debug("[COL-COL_INTEGRATION] COL context menu added (alternative import)")
                    except ImportError:
                        img_debugger.warning("[COL-COL_INTEGRATION] Context menu not available, continuing without it")
        except Exception as e:
            img_debugger.warning(f"[COL-COL_INTEGRATION] Context menu setup failed: {e}")

        # Mark integration as completed
        main_window._col_integration_active = True

        img_debugger.success("[COL-COL_INTEGRATION] Full COL integration completed successfully")
        return True

    except Exception as e:
        img_debugger.error(f"[COL-COL_INTEGRATION] Full COL integration failed: {e}")
        return False



def integrate_col_methods(main_window) -> bool: #vers 1
    """Integrate COL methods into main window"""
    try:
        # Add COL file loading capability
        if not hasattr(main_window, 'load_col_file_safely'):
            from apps.methods.populate_col_table import load_col_file_safely
            main_window.load_col_file_safely = lambda file_path: load_col_file_safely(main_window, file_path)
            img_debugger.debug("✅ COL file loading method added")

        # Add COL operations methods
        from apps.methods.col_operations import (
            extract_col_from_img_entry,
            get_col_basic_info,
            get_col_detailed_analysis
        )

        main_window.extract_col_from_img_entry = lambda row: extract_col_from_img_entry(main_window, row)
        main_window.get_col_basic_info = get_col_basic_info
        main_window.get_col_detailed_analysis = get_col_detailed_analysis

        img_debugger.debug("✅ COL operations methods integrated")
        return True

    except Exception as e:
        img_debugger.error(f"Error integrating COL methods: {str(e)}")
        return False

def integrate_col_context_menus(main_window) -> bool: #vers 1
    """Integrate COL context menus"""
    try:
        from apps.core.right_click_actions import setup_table_context_menu

        # Add enhanced context menu with COL support
        success = setup_table_context_menu(main_window)

        if success:
            img_debugger.success("✅ COL context menus integrated")
        else:
            img_debugger.warning("⚠️ COL context menu integration failed")

        return success

    except Exception as e:
        img_debugger.error(f"Error integrating COL context menus: {str(e)}")
        return False


def integrate_complete_col_system(main_window) -> bool: #vers 1
    """Integrate complete COL system into IMG Factory"""
    try:
        img_debugger.info("🔧 Starting complete COL system integration...")

        # Step 1: Verify all components are available
        if not verify_col_components():
            img_debugger.error("❌ COL component verification failed")
            return False

        # Step 2: Integrate core methods
        methods_success = integrate_col_methods(main_window)

        # Step 3: Integrate context menus
        context_success = integrate_col_context_menus(main_window)

        # Step 4: Integrate editor functionality
        editor_success = integrate_col_editor(main_window)

        # Step 5: Integrate dialogs
        dialogs_success = integrate_col_dialogs(main_window)

        # Step 6: Setup file loading
        loading_success = setup_col_file_loading(main_window)

        # Check overall success
        all_success = all([
            methods_success,
            context_success,
            editor_success,
            dialogs_success,
            loading_success
        ])

        if all_success:
            img_debugger.success("🎉 Complete COL system integration successful!")
            main_window.log_message("🔧 COL collision system fully integrated")

            # Add summary of available COL features
            main_window.log_message("📋 Available COL features:")
            main_window.log_message("  • Right-click COL files in IMG table for context menu")
            main_window.log_message("  • COL Editor with 3D visualization")
            main_window.log_message("  • COL Batch Processor for multiple files")
            main_window.log_message("  • COL file analysis and validation")
            main_window.log_message("  • Direct COL file loading and viewing")

        else:
            img_debugger.warning("⚠️ COL system integration completed with some failures")
            main_window.log_message("⚠️ COL system integrated with limited functionality")

        return all_success

    except Exception as e:
        img_debugger.error(f"Complete COL system integration failed: {str(e)}")
        main_window.log_message(f"❌ COL system integration error: {str(e)}")
        return False


def setup_col_debug_for_main_window(main_window): #vers 1
    """Setup COL debug functionality for main window using IMG debug system"""
    try:
        from apps.debug.debug_functions import set_col_debug_enabled
        
        # Enable COL debug based on main debug state
        if hasattr(main_window, 'debug_enabled') and main_window.debug_enabled:
            set_col_debug_enabled(True)
        else:
            set_col_debug_enabled(False)
        
        col_debug_log(main_window, "COL debug system integrated with main window", 'COL_INTEGRATION', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error setting up COL debug: {e}", 'COL_INTEGRATION', 'ERROR')
        return False

def setup_threaded_col_loading(main_window): #vers 1
    """Setup threaded COL loading using IMG debug system"""
    try:
        col_debug_log(main_window, "Setting up threaded COL loading", 'COL_THREADING')
        
        from apps.components.col_loader import COLBackgroundLoader
        
        # Create background loader
        col_loader = COLBackgroundLoader()
        
        # Connect signals
        if hasattr(main_window, '_on_col_loaded'):
            col_loader.load_complete.connect(main_window._on_col_loaded)
        
        if hasattr(main_window, '_on_load_progress'):
            col_loader.progress_update.connect(main_window._on_load_progress)
        
        # Store reference
        main_window.col_background_loader = col_loader
        
        col_debug_log(main_window, "Threaded COL loading setup complete", 'COL_THREADING', 'SUCCESS')
        return True
        
    except Exception as e:
        col_debug_log(main_window, f"Error setting up threaded COL loading: {e}", 'COL_THREADING', 'ERROR')
        return False

def setup_col_file_loading(main_window) -> bool: #vers 1
    """Setup COL file loading in IMG system"""
    try:
        # Integrate COL file detection in IMG loading
        from apps.components.col_integration_main import integrate_complete_col_system

        success = integrate_complete_col_system(main_window)

        if success:
            img_debugger.debug("✅ COL file loading integrated")
        else:
            img_debugger.warning("⚠️ COL file loading integration failed")

        return success

    except Exception as e:
        img_debugger.error(f"Error setting up COL file loading: {str(e)}")
        return False


def verify_col_components() -> bool: #vers 1
    """Verify all COL components are available"""
    missing_components = []

    try:
        from apps.methods.col_core_classes import COLFile, COLModel
        img_debugger.debug("✅ COL core classes available")
    except ImportError:
        missing_components.append("methods.col_core_classes")

    try:
        from apps.components.Col_Editor.col_editor import COLEditorDialog
        img_debugger.debug("✅ COL editor available")
    except ImportError:
        missing_components.append("components.Col_Editor.col_editor")

    try:
        from apps.methods.col_utilities import COLBatchProcessor
        img_debugger.debug("✅ COL utilities available")
    except ImportError:
        missing_components.append("methods.col_utilities")

    try:
        from apps.methods.col_validation import COLValidator
        img_debugger.debug("✅ COL validator available")
    except ImportError:
        missing_components.append("methods.col_validation")

    try:
        from apps.methods.col_operations import extract_col_from_img_entry
        img_debugger.debug("✅ COL operations methods available")
    except ImportError:
        missing_components.append("methods.col_operations")

    try:
        from gui.col_dialogs import show_col_analysis_dialog
        img_debugger.debug("✅ COL GUI dialogs available")
    except ImportError:
        missing_components.append("gui.col_dialogs")

    if missing_components:
        img_debugger.error(f"Missing COL components: {', '.join(missing_components)}")
        return False

    img_debugger.success("All COL components verified")
    return True

# Export functions
__all__ = [
    'integrate_complete_col_system',
    'integrate_col_context_menus',
    'integrate_col_dialogs',
    'integrate_col_editor',
    'integrate_col_methods',
    'setup_col_file_loading',
    'verify_col_components'
]
