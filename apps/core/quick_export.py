#this belongs in core/quick_export.py - Version: 3
# X-Seti - Aug15 2025 - IMG Factory 1.5 - Quick Export Functions with COL Support

"""
Quick Export Functions - Streamlined export with minimal dialogs
Based on original implementation with COL file support added
Fast export operations with sensible defaults and reduced user interaction
"""

import os
import platform
import subprocess
from PyQt6.QtWidgets import QMessageBox, QProgressDialog
from PyQt6.QtCore import Qt, QSettings
from apps.methods.export_shared import ExportThread, get_selected_entries, get_export_folder, validate_export_entries

##Methods list -
# quick_export_function
# integrate_quick_export_functions

def quick_export_function(main_window): #vers 4
    """Fast export directly to Assists folder with automatic organization - FROM ORIGINAL with COL support"""
    try:
        file_type = get_current_file_type(main_window)
        
        imgcol_exists(main_window)
        # File selection dialog - export should work with both img and col files.

        # Get selected entries - FROM ORIGINAL
        selected_entries = get_selected_entries(main_window)
        if not validate_export_entries(selected_entries, main_window):
            return
            
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"⚡ Quick export: {len(selected_entries)} entries to Assists folder")
            
        elif file_type == 'COL':
            # COL quick export logic
            if not hasattr(main_window, 'current_col') or not main_window.current_col:
                QMessageBox.warning(main_window, "No COL File", "Please open a COL file first")
                return
            
            # Get selected COL models
            from apps.core.export import get_selected_col_models
            selected_entries = get_selected_col_models(main_window)
            if not selected_entries:
                QMessageBox.warning(main_window, "Nothing Selected", 
                    "Please select COL models for quick export")
                return
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"⚡ Quick export: {len(selected_entries)} COL models to Assists folder")
                
        else:
            QMessageBox.warning(main_window, "No File", "Please open an IMG or COL file first")
            return
        
        # Get Assists folder from settings - FROM ORIGINAL
        assists_folder = None
        if hasattr(main_window, 'settings'):
            assists_folder = getattr(main_window.settings, 'assists_folder', None)
        
        if not assists_folder or not os.path.exists(assists_folder):
            # Fallback to manual selection if no assists folder - FROM ORIGINAL
            QMessageBox.warning(
                main_window, 
                "Assists Folder Not Found", 
                "Assists folder not configured or doesn't exist.\nPlease select export destination manually."
            )
            assists_folder = get_export_folder(main_window, f"Quick Export {file_type} - Select Destination")
            if not assists_folder:
                return
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"📁 Quick export destination: {assists_folder}")
        
        # Quick export options - auto-organize to Assists structure - FROM ORIGINAL
        export_options = {
            'organize_by_type': True,
            'use_assists_structure': True,  # Use Assists folder structure
            'overwrite': True,              # Quick export assumes overwrite OK
            'create_log': False,            # Skip log for speed
            'open_folder_after': True       # Open folder when done
        }
        
        # Start quick export based on file type
        if file_type == 'IMG':
            _start_quick_export_with_progress(main_window, selected_entries, assists_folder, export_options, file_type)
        elif file_type == 'COL':
            _start_quick_col_export(main_window, selected_entries, assists_folder, export_options)
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Quick export error: {str(e)}")
        QMessageBox.critical(main_window, "Quick Export Error", f"Quick export failed: {str(e)}")

def _start_quick_export_with_progress(main_window, entries, assists_folder, export_options, file_type): #vers 3
    """Start quick IMG export with minimal progress display - FROM ORIGINAL"""
    try:
        # Create export thread - FROM ORIGINAL
        export_thread = ExportThread(main_window, entries, assists_folder, export_options)
        
        # Show progress dialog - FROM ORIGINAL
        progress_dialog = QProgressDialog("Quick Export in progress...", "Cancel", 0, 100, main_window)
        progress_dialog.setWindowModality(Qt.WindowModality.WindowModal)
        
        def update_progress(progress, message):
            progress_dialog.setValue(progress)
            progress_dialog.setLabelText(message)
        
        def export_finished(success, message, stats):
            progress_dialog.close()
            if success:
                # FROM ORIGINAL - Brief success message
                QMessageBox.information(main_window, "Quick Export Complete", message)
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"⚡ {message}")
                
                # Open assists folder if requested - FROM ORIGINAL
                if export_options.get('open_folder_after', True):
                    try:
                        if platform.system() == "Linux":
                            subprocess.run(["xdg-open", assists_folder])
                        elif platform.system() == "Windows":
                            subprocess.run(["explorer", assists_folder])
                        elif platform.system() == "Darwin":  # macOS
                            subprocess.run(["open", assists_folder])
                    except Exception:
                        pass  # Don't fail if can't open folder
            else:
                # FROM ORIGINAL - Still show errors
                QMessageBox.critical(main_window, "Quick Export Failed", message)
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"❌ Quick export failed: {message}")
        
        def handle_cancel():
            if export_thread.isRunning():
                export_thread.terminate()
                export_thread.wait()
                if hasattr(main_window, 'log_message'):
                    main_window.log_message("🚫 Quick export cancelled by user")
        
        # Connect signals - FROM ORIGINAL
        export_thread.progress_updated.connect(update_progress)
        export_thread.export_completed.connect(export_finished)
        progress_dialog.canceled.connect(handle_cancel)
        
        # Start export
        export_thread.start()
        progress_dialog.show()
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Quick export thread error: {str(e)}")
        QMessageBox.critical(main_window, "Quick Export Error", f"Failed to start quick export: {str(e)}")

def _start_quick_col_export(main_window, col_entries, assists_folder, export_options): #vers 1
    """Start quick COL export with minimal progress display"""
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"Quick COL export: {len(col_entries)} models")
        
        exported_count = 0
        failed_count = 0
        total_entries = len(col_entries)
        
        # Minimal progress dialog for quick export
        progress_dialog = QProgressDialog("Quick exporting COL models...", "Cancel", 0, total_entries, main_window)
        progress_dialog.setWindowModality(Qt.WindowModality.WindowModal)
        progress_dialog.show()
        
        # Create COL subfolder if organizing by type
        export_folder = assists_folder
        if export_options.get('organize_by_type', True):
            col_folder = os.path.join(assists_folder, 'Collisions')  # Use Assists structure naming
            os.makedirs(col_folder, exist_ok=True)
            export_folder = col_folder
        
        for i, col_entry in enumerate(col_entries):
            if progress_dialog.wasCanceled():
                break
                
            try:
                model_name = col_entry['name']
                output_path = os.path.join(export_folder, model_name)
                
                # Update progress - simplified for quick export
                progress_dialog.setValue(i)
                progress_dialog.setLabelText(f"Exporting {model_name}...")
                
                # Create individual COL file for this model
                from apps.core.export import _create_single_model_col_file
                if _create_single_model_col_file(col_entry, output_path):
                    exported_count += 1
                else:
                    failed_count += 1
                    
            except Exception as e:
                failed_count += 1
                if hasattr(main_window, 'log_message'):
                    main_window.log_message(f"❌ Error in quick COL export {col_entry['name']}: {str(e)}")
        
        progress_dialog.close()
        
        # Show results - match original quick export style
        if exported_count > 0:
            # Brief success message for quick export
            brief_msg = f"Quick COL export complete!\n{exported_count} models exported"
            if failed_count > 0:
                brief_msg += f"\n{failed_count} models failed"
            
            QMessageBox.information(main_window, "Quick COL Export Complete", brief_msg)
            
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"✅ Quick COL export: {exported_count} success, {failed_count} failed")
            
            # Open export folder if requested - FROM ORIGINAL
            if export_options.get('open_folder_after', True):
                try:
                    if platform.system() == "Linux":
                        subprocess.run(["xdg-open", export_folder])
                    elif platform.system() == "Windows":
                        subprocess.run(["explorer", export_folder])
                    elif platform.system() == "Darwin":  # macOS
                        subprocess.run(["open", export_folder])
                except Exception:
                    pass  # Don't fail if can't open folder
        else:
            QMessageBox.warning(main_window, "Quick COL Export Failed", "No COL models were exported successfully")
            
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Quick COL export error: {str(e)}")
        QMessageBox.critical(main_window, "Quick COL Export Error", f"Quick COL export failed: {str(e)}")

def integrate_quick_export_functions(main_window): #vers 2
    """Integrate quick export functions into main window with all aliases"""
    global file_object, file_type
    file_object = getattr(main_window, 'file_object', None)
    file_type = getattr(main_window, 'file_type', None)

    try:
        # Add main quick export function
        main_window.quick_export_function = lambda: quick_export_function(main_window)
        
        # Add aliases for different naming conventions that GUI might use
        main_window.quick_export = main_window.quick_export_function
        main_window.quick_export_selected = main_window.quick_export_function
        main_window.export_quick = main_window.quick_export_function
        main_window.fast_export = main_window.quick_export_function
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ Quick export functions integrated with COL support")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Failed to integrate quick export functions: {str(e)}")
        return False

__all__ = [
    'quick_export_function',
    'integrate_quick_export_functions'
]
