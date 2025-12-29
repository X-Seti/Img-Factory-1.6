#this belongs in core/analyze_rw.py - Version: 1
# X-Seti - September11 2025 - IMG Factory 1.5 - RW Analysis Trigger

"""
RW Analysis Trigger - Run RW detection on existing entries to fix blank columns
"""

from PyQt6.QtWidgets import QMessageBox
from apps.methods.file_validation import validate_img_file, validate_any_file, get_selected_entries_for_operation

##Methods list -
# analyze_current_img_rw_versions
# integrate_rw_analysis_trigger

def analyze_current_img_rw_versions(main_window): #vers 1
    """Analyze RW versions for all entries in current IMG"""
    if not validate_tab_before_operation(main_window, "Analyze RW Versions"):
        return False

    file_object, file_type = get_current_file_from_active_tab(main_window)

    if file_type != 'IMG' or not file_object:
        QMessageBox.warning(main_window, "No IMG File", "Current tab does not contain an IMG file")
        return False

    if not hasattr(file_object, 'entries') or not file_object.entries:
        QMessageBox.information(main_window, "No Entries", "No entries found in IMG file")
        return False

    total_entries = len(file_object.entries)
    
    if hasattr(main_window, 'log_message'):
        main_window.log_message(f"Starting RW analysis for {total_entries} entries...")

    # Try to use existing RW detection methods
    analyzed_count = 0
    
    try:
        # Method 1: Use analyze_all_entries_rw_versions_working if available
        if hasattr(main_window, 'analyze_all_entries_rw_versions_working'):
            analyzed_count = main_window.analyze_all_entries_rw_versions_working(file_object)
        # Method 2: Use file_object method if available
        elif hasattr(file_object, 'analyze_all_entries_rw_versions'):
            analyzed_count = file_object.analyze_all_entries_rw_versions()
        # Method 3: Manual analysis using individual entry methods
        else:
            for entry in file_object.entries:
                try:
                    if hasattr(main_window, 'analyze_entry_rw_version_working'):
                        if main_window.analyze_entry_rw_version_working(entry, file_object):
                            analyzed_count += 1
                    elif hasattr(entry, 'detect_file_type_and_version'):
                        entry.detect_file_type_and_version()
                        analyzed_count += 1
                    elif hasattr(entry, 'analyze_rw_version'):
                        if entry.analyze_rw_version(file_object):
                            analyzed_count += 1
                except Exception:
                    continue

        # Refresh the table to show RW data
        if hasattr(main_window, 'refresh_table'):
            main_window.refresh_table()
        
        if analyzed_count > 0:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"RW analysis complete: {analyzed_count}/{total_entries} entries analyzed")
            
            QMessageBox.information(
                main_window, 
                "RW Analysis Complete", 
                f"RW version analysis completed.\n\n"
                f"Analyzed: {analyzed_count}/{total_entries} entries\n"
                f"The RW columns should now be populated."
            )
            return True
        else:
            if hasattr(main_window, 'log_message'):
                main_window.log_message("RW analysis failed: No entries could be analyzed")
            
            QMessageBox.warning(
                main_window,
                "RW Analysis Failed",
                "Could not analyze RW versions for existing entries.\n\n"
                "The RW detection system may not be properly integrated."
            )
            return False
            
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"RW analysis error: {str(e)}")
        
        QMessageBox.critical(
            main_window,
            "RW Analysis Error",
            f"Error during RW analysis: {str(e)}"
        )
        return False

def integrate_rw_analysis_trigger(main_window) -> bool: #vers 1
    """Integrate RW analysis trigger function"""
    # Add RW analysis method to main window
    main_window.analyze_current_img_rw_versions = lambda: analyze_current_img_rw_versions(main_window)
    
    # Add alias
    main_window.analyze_rw_versions = main_window.analyze_current_img_rw_versions
    
    if hasattr(main_window, 'log_message'):
        main_window.log_message("RW analysis trigger integrated")
        main_window.log_message("   • Call analyze_current_img_rw_versions() to fix blank RW columns")
    
    return True

# Export functions
__all__ = [
    'analyze_current_img_rw_versions',
    'integrate_rw_analysis_trigger'
]
