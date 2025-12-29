# this belongs in apps/debug/debug_functions.py - version 5
#!/usr/bin/env python3
"""
X-Seti - June26 2025 - IMG Debug - Debugging utilities for IMG Factory
Provides comprehensive debugging and tracing for all operations
"""


import os
import sys
import traceback
import linecache
import inspect
import time

from PyQt6.QtWidgets import QLabel, QMessageBox
from PyQt6.QtGui import QAction, QShortcut, QKeySequence
from PyQt6.QtCore import QTimer
from typing import Any, Dict, List, Optional
from pathlib import Path

##Methods list -
# col_debug_log
# debug_col_creation_process
# debug_col_import_errors
# debug_col_loading_process
# debug_col_model_parsing
# debug_col_threading
# is_col_debug_enabled
# set_col_debug_enabled
# trace_col_function
# add_status_indicators
# apply_all_fixes_and_improvements
# create_debug_menu
# fix_search_dialog
# install_debug_control_system
# install_search_manager
# integrate_all_improvements
# setup_debug_convenience_methods

# Global debug state for COL operations
_col_debug_enabled = False


def debug_trace(func): #vers 1
    """Simple debug decorator to trace function calls."""
    def wrapper(*args, **kwargs):
        print(f"[DEBUG] Calling: {func.__name__} with args={args} kwargs={kwargs}")
        result = func(*args, **kwargs)
        print(f"[DEBUG] Finished: {func.__name__}")
        return result
    return wrapper

def apply_all_fixes_and_improvements(main_window): #vers 1
    """Apply all fixes and improvements to IMG Factory - CLEAN VERSION using IMG debug"""
    try:
        main_window.log_message("🔧 Applying comprehensive fixes...")

        # 1. Fix search functionality - simplified approach
        search_manager_success = install_search_manager(main_window)
        search_dialog_success = fix_search_dialog(main_window)

        if search_manager_success:
            main_window.log_message("✅ Search manager installed - live search now works")

        if search_dialog_success:
            main_window.log_message("✅ Search dialog functionality fixed")

        # 2. Install debug control system - simplified
        debug_system_success = install_debug_control_system(main_window)

        if debug_system_success:
            main_window.log_message("✅ Debug control system installed")

        # 3. COL debug control - NOW USES col_debug_functions.py
        col_debug_success = setup_img_debug_system_integration(main_window)

        if col_debug_success:
            main_window.log_message("✅ IMG debug system integrated for COL operations")

        # 4. Add convenience methods to main window
        setup_debug_convenience_methods(main_window)

        # 5. Create comprehensive debug menu
        create_debug_menu(main_window)

        # 6. Show summary of what was fixed
        overall_success = search_manager_success and debug_system_success and col_debug_success

        if overall_success:
            main_window.log_message("=" * 50)
            main_window.log_message("🎯 ALL FIXES APPLIED SUCCESSFULLY!")
            main_window.log_message("📖 Check Debug menu for all options")

        return overall_success

    except Exception as e:
        main_window.log_message(f"❌ Integration error: {e}")
        return False

def setup_img_debug_system_integration(main_window): #vers 1
    """Setup IMG debug system for COL operations - REPLACES old COL debug"""
    try:
        # Use the new col_debug_functions.py which uses IMG debug system
        from apps.debug.debug_functions import integrate_col_debug_with_main_window

        success = integrate_col_debug_with_main_window(main_window)

        if success:
            main_window.log_message("✅ COL operations now use IMG debug system")
        else:
            main_window.log_message("⚠️ COL debug integration had issues")

        return success

    except Exception as e:
        main_window.log_message(f"IMG debug integration error: {e}")
        return False

def install_search_manager(main_window): #vers 1
    """Install search manager - simplified approach"""
    try:
        # Try to install from apps.core.search system
        from apps.core.guisearch import install_search_system

        success = install_search_system(main_window)

        if success:
            main_window.log_message("✅ Search system installed")
        else:
            main_window.log_message("⚠️ Search system installation issues")

        return success

    except ImportError:
        main_window.log_message("⚠️ Search system not available")
        return False
    except Exception as e:
        main_window.log_message(f"Search installation error: {e}")
        return False

def fix_search_dialog(main_window): #vers 1
    """Fix search dialog functionality"""
    try:
        # Add search dialog method if missing
        if not hasattr(main_window, 'show_search_dialog'):
            def show_search_dialog():
                """Simple search dialog"""
                try:
                    from apps.core.dialogs import show_search_dialog as core_search
                    core_search(main_window)
                except ImportError:
                    QMessageBox.information(main_window, "Search",
                        "Use Ctrl+F for quick search in the entries table")

            main_window.show_search_dialog = show_search_dialog

        return True

    except Exception as e:
        main_window.log_message(f"Search dialog error: {e}")
        return False

def install_debug_control_system(main_window): #vers 1
    """Install debug control system - CLEAN VERSION"""
    try:
        # Add missing method to main window
        def show_debug_settings():
            """Simple debug settings dialog"""
            try:
                # Try to show proper debug settings if available
                from apps.utils.app_settings_system import SettingsDialog
                if hasattr(main_window, 'app_settings'):
                    dialog = SettingsDialog(main_window.app_settings, main_window)
                    dialog.exec()
                else:
                    QMessageBox.information(main_window, "Debug Settings",
                        "Debug settings: Use Ctrl+Shift+D to toggle COL debug")
            except ImportError:
                QMessageBox.information(main_window, "Debug Settings",
                    "Debug settings: Use Ctrl+Shift+D to toggle COL debug")

        main_window.show_debug_settings = show_debug_settings

        return True

    except Exception as e:
        main_window.log_message(f"Debug control error: {e}")
        return False

def setup_debug_convenience_methods(main_window): #vers 1
    """Add convenience debug methods to main window"""
    try:
        # Add debug info method
        def show_debug_info():
            """Show debug information"""
            try:
                from apps.debug.debug_functions import img_debugger
                from apps.debug.debug_functions import is_col_debug_enabled

                info = f"IMG Debug: {'Enabled' if img_debugger.debug_enabled else 'Disabled'}\n"
                info += f"COL Debug: {'Enabled' if is_col_debug_enabled() else 'Disabled'}\n"
                info += f"Debug Log: {img_debugger.log_file}\n"
                info += f"Error Count: {img_debugger.error_count}\n"
                info += f"Warning Count: {img_debugger.warning_count}"

                QMessageBox.information(main_window, "Debug Information", info)

            except Exception as e:
                QMessageBox.critical(main_window, "Debug Error", f"Error getting debug info: {e}")

        main_window.show_debug_info = show_debug_info

        # Add debug log viewer
        def view_debug_log():
            """View debug log file"""
            try:
                from apps.debug.debug_functions import img_debugger
                import os

                if os.path.exists(img_debugger.log_file):
                    os.system(f"xdg-open {img_debugger.log_file}")  # Linux
                else:
                    QMessageBox.information(main_window, "Debug Log", "No debug log file found")

            except Exception as e:
                QMessageBox.critical(main_window, "Debug Error", f"Error opening debug log: {e}")

        main_window.view_debug_log = view_debug_log

        return True

    except Exception as e:
        main_window.log_message(f"Debug convenience methods error: {e}")
        return False

def create_debug_menu(main_window): #vers 1
    """Create comprehensive debug menu"""
    try:
        menu_bar = main_window.menuBar()

        # Find or create Debug menu
        debug_menu = None
        for action in menu_bar.actions():
            if "debug" in action.text().lower():
                debug_menu = action.menu()
                break

        if not debug_menu:
            debug_menu = menu_bar.addMenu("🔧 Debug")

        # Clear existing debug actions
        debug_menu.clear()

        # IMG Debug controls
        img_debug_menu = debug_menu.addMenu("📁 IMG Debug")

        img_enable_action = img_debug_menu.addAction("Enable IMG Debug")
        img_enable_action.triggered.connect(lambda: setattr(__import__('apps.debug.img_debug_functions').img_debug_functions.img_debugger, 'debug_enabled', True))

        img_disable_action = img_debug_menu.addAction("Disable IMG Debug")
        img_disable_action.triggered.connect(lambda: setattr(__import__('apps.debug.img_debug_functions').img_debug_functions.img_debugger, 'debug_enabled', False))

        # COL Debug controls (using IMG debug system)
        col_debug_menu = debug_menu.addMenu("COL Debug")

        col_enable_action = col_debug_menu.addAction("Enable COL Debug")
        col_enable_action.triggered.connect(lambda: main_window.enable_col_debug() if hasattr(main_window, 'enable_col_debug') else None)

        col_disable_action = col_debug_menu.addAction("Disable COL Debug")
        col_disable_action.triggered.connect(lambda: main_window.disable_col_debug() if hasattr(main_window, 'disable_col_debug') else None)

        col_toggle_action = col_debug_menu.addAction("Toggle COL Debug")
        col_toggle_action.triggered.connect(lambda: main_window.toggle_col_debug() if hasattr(main_window, 'toggle_col_debug') else None)
        col_toggle_action.setShortcut(QKeySequence("Ctrl+Shift+D"))

        debug_menu.addSeparator()

        # Debug information
        debug_info_action = debug_menu.addAction("📊 Debug Information")
        debug_info_action.triggered.connect(lambda: main_window.show_debug_info() if hasattr(main_window, 'show_debug_info') else None)

        # Debug log
        debug_log_action = debug_menu.addAction("📋 View Debug Log")
        debug_log_action.triggered.connect(lambda: main_window.view_debug_log() if hasattr(main_window, 'view_debug_log') else None)

        debug_menu.addSeparator()

        # Debug settings
        debug_settings_action = debug_menu.addAction("⚙️ Debug Settings")
        debug_settings_action.triggered.connect(lambda: main_window.show_debug_settings() if hasattr(main_window, 'show_debug_settings') else None)

        main_window.log_message("✅ Debug menu created")
        return True

    except Exception as e:
        main_window.log_message(f"Debug menu error: {e}")
        return False

def add_status_indicators(main_window): #vers 1
    """Add debug status indicators to status bar"""
    try:
        # Create debug status label
        debug_status_label = QLabel("🎛️ Debug Ready")
        debug_status_label.setToolTip("Debug system status")

        def update_debug_status():
            """Update debug status display"""
            try:
                from apps.debug.debug_functions import img_debugger
                from apps.debug.debug_functions import is_col_debug_enabled

                img_debug = img_debugger.debug_enabled
                col_debug = is_col_debug_enabled()

                if img_debug and col_debug:
                    debug_status_label.setText("🔴 IMG+COL Debug")
                    debug_status_label.setStyleSheet("color: red; font-weight: bold;")
                elif img_debug:
                    debug_status_label.setText("🟡 IMG Debug")
                    debug_status_label.setStyleSheet("color: orange; font-weight: bold;")
                elif col_debug:
                    debug_status_label.setText("🟢 COL Debug")
                    debug_status_label.setStyleSheet("color: green; font-weight: bold;")
                else:
                    debug_status_label.setText("🎛️ Debug Ready")
                    debug_status_label.setStyleSheet("color: blue;")
            except:
                debug_status_label.setText("🎛️ Debug Ready")
                debug_status_label.setStyleSheet("color: blue;")

        update_debug_status()

        # Add to status bar
        main_window.statusBar().addPermanentWidget(debug_status_label)

        # Update periodically
        timer = QTimer()
        timer.timeout.connect(update_debug_status)
        timer.start(5000)  # Update every 5 seconds

        main_window.log_message("✅ Debug status indicators added")
        return True

    except Exception as e:
        main_window.log_message(f"❌ Status indicators error: {e}")
        return False

def integrate_all_improvements(main_window): #vers 1
    """Main function to integrate all improvements - call this from imgfactory.py"""
    try:
        # Apply core fixes
        core_success = apply_all_fixes_and_improvements(main_window)

        # Add additional UI improvements
        add_status_indicators(main_window)

        if core_success:
            main_window.log_message("🎉 IMG Factory 1.5 - All improvements integrated successfully!")
            main_window.log_message("📖 Check Debug menu for all options")

        return core_success

    except Exception as e:
        main_window.log_message(f"❌ Integration error: {e}")
        return False


# Usage examples and testing functions
def test_debug_system():
    """Test the debug system"""
    print("Testing IMG Debug System...")

    img_debugger.debug("This is a debug message")
    img_debugger.info("This is an info message")
    img_debugger.warning("This is a warning message")
    img_debugger.error("This is an error message")
    img_debugger.success("This is a success message")

    # Test object inspection
    test_obj = type('TestObj', (), {'attr1': 'value1', 'attr2': 42})()
    img_debugger.inspect_object(test_obj, "TestObject")

    # Test file checking
    img_debugger.check_file_operations("nonexistent_file.img")

    print(img_debugger.get_debug_summary())


class IMGDebugger:
    """Advanced debugging system for IMG Factory operations"""

    def __init__(self, log_file: str = "img_factory_debug.log"):
        self.log_file = log_file
        self.debug_enabled = True
        self.trace_calls = True
        self.log_to_console = True
        self.log_to_file = True

        # Create debug log file
        self._init_log_file()

        # Debug counters
        self.call_count = 0
        self.error_count = 0
        self.warning_count = 0

    def _init_log_file(self):
        """Initialize debug log file"""
        try:
            with open(self.log_file, 'w') as f:
                f.write(f"=== IMG Factory Debug Log ===\n")
                f.write(f"Started: {time.strftime('%Y-%m-%d %H:%M:%S')}\n")
                f.write(f"Python: {sys.version}\n")
                f.write(f"Platform: {sys.platform}\n")
                f.write("=" * 50 + "\n\n")
        except Exception as e:
            print(f"Warning: Could not create debug log file: {e}")
            self.log_to_file = False

    def log(self, level: str, message: str, caller_info: bool = True):
        """Log debug message"""
        if not self.debug_enabled:
            return

        timestamp = time.strftime('%H:%M:%S')

        # Get caller information
        caller_frame = inspect.currentframe().f_back
        caller_name = caller_frame.f_code.co_name
        caller_file = os.path.basename(caller_frame.f_code.co_filename)
        caller_line = caller_frame.f_lineno

        # Format message
        if caller_info:
            log_msg = f"[{timestamp}] {level}: {caller_file}:{caller_line} in {caller_name}() - {message}"
        else:
            log_msg = f"[{timestamp}] {level}: {message}"

        # Output to console
        if self.log_to_console:
            if level == "ERROR":
                print(f"🔴 {log_msg}")
            elif level == "WARNING":
                print(f"🟡 {log_msg}")
            elif level == "SUCCESS":
                print(f"🟢 {log_msg}")
            else:
                print(f"🔵 {log_msg}")

        # Output to file
        if self.log_to_file:
            try:
                with open(self.log_file, 'a') as f:
                    f.write(log_msg + "\n")
            except:
                pass

        # Update counters
        if level == "ERROR":
            self.error_count += 1
        elif level == "WARNING":
            self.warning_count += 1

    def debug(self, message: str):
        """Log debug message"""
        self.log("DEBUG", message)

    def info(self, message: str):
        """Log info message"""
        self.log("INFO", message)

    def warning(self, message: str):
        """Log warning message"""
        self.log("WARNING", message)

    def error(self, message: str):
        """Log error message"""
        self.log("ERROR", message)

    def success(self, message: str):
        """Log success message"""
        self.log("SUCCESS", message)

    def trace_method_call(self, obj: Any, method_name: str, *args, **kwargs):
        """Trace method call with parameters"""
        if not self.trace_calls:
            return

        self.call_count += 1

        # Format arguments
        arg_strs = []
        for i, arg in enumerate(args):
            if isinstance(arg, str) and len(arg) > 50:
                arg_strs.append(f"'{arg[:47]}...'")
            else:
                arg_strs.append(repr(arg))

        for key, value in kwargs.items():
            if isinstance(value, str) and len(value) > 50:
                arg_strs.append(f"{key}='{value[:47]}...'")
            else:
                arg_strs.append(f"{key}={repr(value)}")

        args_str = ", ".join(arg_strs)
        obj_name = obj.__class__.__name__ if hasattr(obj, '__class__') else str(type(obj))

        self.debug(f"CALL #{self.call_count}: {obj_name}.{method_name}({args_str})")

    def trace_method_result(self, result: Any, execution_time: float = None):
        """Trace method result"""
        if not self.trace_calls:
            return

        if isinstance(result, str) and len(result) > 100:
            result_str = f"'{result[:97]}...'"
        else:
            result_str = repr(result)

        time_str = f" (took {execution_time:.3f}s)" if execution_time else ""
        self.debug(f"RESULT #{self.call_count}: {result_str}{time_str}")

    def trace_exception(self, exception: Exception):
        """Trace exception with full traceback"""
        self.error(f"EXCEPTION: {type(exception).__name__}: {str(exception)}")
        self.error(f"TRACEBACK:\n{traceback.format_exc()}")

    def inspect_object(self, obj: Any, name: str = "object"):
        """Inspect object properties and methods"""
        self.debug(f"INSPECTING {name} ({type(obj).__name__}):")

        # Show attributes
        attributes = []
        methods = []

        for attr_name in dir(obj):
            if attr_name.startswith('_'):
                continue

            try:
                attr_value = getattr(obj, attr_name)
                if callable(attr_value):
                    methods.append(attr_name)
                else:
                    if isinstance(attr_value, str) and len(attr_value) > 50:
                        attributes.append(f"  {attr_name} = '{attr_value[:47]}...'")
                    else:
                        attributes.append(f"  {attr_name} = {repr(attr_value)}")
            except:
                attributes.append(f"  {attr_name} = <unable to access>")

        if attributes:
            self.debug(f"  Attributes:")
            for attr in attributes[:10]:  # Limit to first 10
                self.debug(attr)
            if len(attributes) > 10:
                self.debug(f"  ... and {len(attributes) - 10} more attributes")

        if methods:
            self.debug(f"  Methods: {', '.join(methods[:10])}")
            if len(methods) > 10:
                self.debug(f"  ... and {len(methods) - 10} more methods")

    def check_file_operations(self, file_path: str, operation: str = "access"):
        """Debug file operations"""
        self.debug(f"FILE CHECK: {operation} on '{file_path}'")

        path_obj = Path(file_path)

        # Check path components
        self.debug(f"  Absolute path: {path_obj.absolute()}")
        self.debug(f"  Parent directory: {path_obj.parent}")
        self.debug(f"  File name: {path_obj.name}")
        self.debug(f"  File extension: {path_obj.suffix}")

        # Check existence and permissions
        if path_obj.exists():
            self.debug(f"  ✓ File exists")
            self.debug(f"  Size: {path_obj.stat().st_size} bytes")
            self.debug(f"  Readable: {os.access(file_path, os.R_OK)}")
            self.debug(f"  Writable: {os.access(file_path, os.W_OK)}")
        else:
            self.debug(f"  ✗ File does not exist")

        # Check parent directory
        if path_obj.parent.exists():
            self.debug(f"  ✓ Parent directory exists")
            self.debug(f"  Parent writable: {os.access(path_obj.parent, os.W_OK)}")
        else:
            self.debug(f"  ✗ Parent directory does not exist")

    def debug_img_creation(self, img_file_obj: Any, **params):
        """Debug IMG file creation process"""
        self.debug("=== IMG CREATION DEBUG START ===")

        # Inspect the IMG file object
        self.inspect_object(img_file_obj, "IMGFile")

        # Debug creation parameters
        self.debug("Creation parameters:")
        for key, value in params.items():
            self.debug(f"  {key} = {repr(value)}")

        # Check if create_new method exists
        if hasattr(img_file_obj, 'create_new'):
            self.success("✓ create_new method found")

            # Get method signature
            try:
                sig = inspect.signature(img_file_obj.create_new)
                self.debug(f"Method signature: create_new{sig}")
            except:
                self.warning("Could not get method signature")
        else:
            self.error("✗ create_new method NOT found!")
            self.debug("Available methods:")
            methods = [attr for attr in dir(img_file_obj) if callable(getattr(img_file_obj, attr)) and not attr.startswith('_')]
            for method in methods:
                self.debug(f"  - {method}")

        # Check output path
        output_path = params.get('output_path')
        if output_path:
            self.check_file_operations(output_path, "create")

        self.debug("=== IMG CREATION DEBUG END ===")

    def get_debug_summary(self) -> str:
        """Get debug session summary"""
        return f"""
=== DEBUG SESSION SUMMARY ===
Total method calls: {self.call_count}
Errors encountered: {self.error_count}
Warnings issued: {self.warning_count}
Log file: {self.log_file}
================================
"""


# Global debugger instance

img_debugger = IMGDebugger()

def debug_img_creation_process(img_creator_dialog):
    """Debug the IMG creation process from dialog"""
    img_debugger.debug("=== DEBUGGING IMG CREATION PROCESS ===")

    # Debug dialog state
    img_debugger.inspect_object(img_creator_dialog, "NewIMGDialog")

    # Check if dialog has the necessary components
    required_attrs = ['filename_input', 'output_path', 'selected_game_type']
    for attr in required_attrs:
        if hasattr(img_creator_dialog, attr):
            value = getattr(img_creator_dialog, attr)
            img_debugger.success(f"✓ Dialog has {attr}: {repr(value)}")
        else:
            img_debugger.error(f"✗ Dialog missing {attr}")


def debug_import_errors():
    """Debug component import issues"""
    img_debugger.debug("=== DEBUGGING IMPORT ERRORS ===")

    components_to_check = [
        'img_core_classes',
        'img_creator',
        'img_validator',
        'img_templates',
        'img_manager'
    ]

    for component in components_to_check:
        try:
            __import__(component)
            img_debugger.success(f"✓ {component} imported successfully")
        except ImportError as e:
            img_debugger.error(f"✗ Failed to import {component}: {e}")
        except Exception as e:
            img_debugger.error(f"✗ Error importing {component}: {e}")


def trace_function(func):
    """Decorator to trace function calls"""
    def wrapper(*args, **kwargs):
        if img_debugger.trace_calls:
            start_time = time.time()

            # Log call
            img_debugger.trace_method_call(None, func.__name__, *args, **kwargs)

            try:
                result = func(*args, **kwargs)
                execution_time = time.time() - start_time
                img_debugger.trace_method_result(result, execution_time)
                return result
            except Exception as e:
                img_debugger.trace_exception(e)
                raise
        else:
            return func(*args, **kwargs)

    return wrapper



def enhanced_error_handler(exc_type, exc_value, exc_traceback): #vers 1
    """Enhanced error handler showing exact file and line"""
    try:
        # Get the full traceback
        tb_lines = traceback.format_exception(exc_type, exc_value, exc_traceback)

        # Look for format_file_size specifically
        if "format_file_size" in str(exc_value):
            print("\n🔴 FORMAT_FILE_SIZE ERROR DETECTED:")
            print("=" * 50)

            # Find the exact line that caused the error
            tb = exc_traceback
            while tb is not None:
                frame = tb.tb_frame
                filename = frame.f_code.co_filename
                line_number = tb.tb_lineno
                function_name = frame.f_code.co_name

                # Get the actual line of code
                line_text = linecache.getline(filename, line_number).strip()

                if "format_file_size" in line_text:
                    print(f"📁 FILE: {filename}")
                    print(f"📍 LINE: {line_number}")
                    print(f"🔧 FUNCTION: {function_name}")
                    print(f"💥 CODE: {line_text}")
                    print(f"❌ ERROR: {exc_value}")
                    print("=" * 50)
                    break

                tb = tb.tb_next

        # Show full traceback for all errors
        print("\n🔍 FULL TRACEBACK:")
        for line in tb_lines:
            print(line.rstrip())

    except Exception as e:
        print(f"Error in enhanced error handler: {e}")
        # Fallback to default handler
        sys.__excepthook__(exc_type, exc_value, exc_traceback)

def install_enhanced_debug(main_window): #vers 1
    """Install enhanced debug error handler"""
    try:
        # Install the enhanced error handler
        sys.excepthook = enhanced_error_handler

        # Also patch img_debugger for more file info
        original_error = img_debugger.error

        def enhanced_img_error(message):
            """Enhanced error with stack trace info"""
            # Get caller information
            import inspect
            frame = inspect.currentframe().f_back
            filename = frame.f_code.co_filename
            line_number = frame.f_lineno
            function_name = frame.f_code.co_name

            enhanced_message = f"{message}\n   📁 {filename}:{line_number} in {function_name}()"
            original_error(enhanced_message)

        img_debugger.error = enhanced_img_error

        main_window.log_message("✅ Enhanced debug error handler installed")
        return True

    except Exception as e:
        main_window.log_message(f"❌ Failed to install enhanced debug: {e}")
        return False

# Quick integration function
def integrate_enhanced_debug_error(main_window): #vers 1
    """Quick integration for enhanced error debugging"""
    return install_enhanced_debug(main_window)



def set_col_debug_enabled(enabled: bool):
    """Enable/disable COL debug output using IMG debug system"""
    global _col_debug_enabled
    _col_debug_enabled = enabled

    if enabled:
        img_debugger.debug("COL debug system ENABLED")
    else:
        img_debugger.debug("COL debug system DISABLED for performance")

def is_col_debug_enabled() -> bool:
    """Check if COL debug is enabled"""
    global _col_debug_enabled
    return _col_debug_enabled and img_debugger.debug_enabled

def col_debug_log(main_window, message: str, category: str = 'COL_GENERAL', level: str = 'INFO'):
    """Log COL debug message using IMG debug system"""
    if not is_col_debug_enabled():
        return  # Skip for performance

    # Use IMG debugger with COL prefix
    prefixed_message = f"[COL-{category}] {message}"

    if level == 'ERROR':
        img_debugger.error(prefixed_message)
    elif level == 'WARNING':
        img_debugger.warning(prefixed_message)
    elif level == 'SUCCESS':
        img_debugger.success(prefixed_message)
    else:
        img_debugger.debug(prefixed_message)

    # Also log to main window if available
    try:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"[{category}] {message}")
    except:
        pass

def debug_col_creation_process(col_creator_dialog):
    """Debug the COL creation process from dialog using IMG debug system"""
    if not is_col_debug_enabled():
        return

    img_debugger.debug("=== DEBUGGING COL CREATION PROCESS ===")

    # Debug dialog state
    img_debugger.inspect_object(col_creator_dialog, "NewCOLDialog")

    # Check if dialog has the necessary components
    required_attrs = ['filename_input', 'output_path', 'selected_col_version']
    for attr in required_attrs:
        if hasattr(col_creator_dialog, attr):
            value = getattr(col_creator_dialog, attr)
            img_debugger.success(f"✓ Dialog has {attr}: {repr(value)}")
        else:
            img_debugger.error(f"✗ Dialog missing {attr}")

def debug_col_import_errors():
    """Debug COL component import issues using IMG debug system"""
    if not is_col_debug_enabled():
        return

    img_debugger.debug("=== DEBUGGING COL IMPORT ERRORS ===")

    components_to_check = [
        'methods.col_core_classes',
        'components.Col_Creator.col_creator',
        'methods.col_validation',
        'methods.col_parsing_functions',
        'components.col_display'
    ]

    for component in components_to_check:
        try:
            __import__(component)
            img_debugger.success(f"✓ {component} imported successfully")
        except ImportError as e:
            img_debugger.error(f"✗ Failed to import {component}: {e}")
        except Exception as e:
            img_debugger.error(f"✗ Error importing {component}: {e}")

def debug_col_loading_process(col_file_obj: Any, **params):
    """Debug COL file loading process using IMG debug system"""
    if not is_col_debug_enabled():
        return

    img_debugger.debug("=== COL LOADING DEBUG START ===")

    # Inspect the COL file object
    img_debugger.inspect_object(col_file_obj, "COLFile")

    # Debug loading parameters
    img_debugger.debug("Loading parameters:")
    for key, value in params.items():
        img_debugger.debug(f"  {key} = {repr(value)}")

    # Check if load method exists
    if hasattr(col_file_obj, 'load'):
        img_debugger.success("✓ load method found")

        # Get method signature
        try:
            sig = inspect.signature(col_file_obj.load)
            img_debugger.debug(f"Method signature: load{sig}")
        except:
            img_debugger.warning("Could not get method signature")
    else:
        img_debugger.error("✗ load method NOT found!")

    # Check file path
    file_path = params.get('file_path') or getattr(col_file_obj, 'file_path', None)
    if file_path:
        img_debugger.check_file_operations(file_path, "read")

    img_debugger.debug("=== COL LOADING DEBUG END ===")

def debug_col_model_parsing(col_model_obj: Any, model_index: int = 0):
    """Debug COL model parsing using IMG debug system"""
    if not is_col_debug_enabled():
        return

    img_debugger.debug(f"=== COL MODEL {model_index} PARSING DEBUG ===")

    # Inspect model object
    img_debugger.inspect_object(col_model_obj, f"COLModel_{model_index}")

    # Check common model attributes
    model_attrs = ['name', 'version', 'spheres', 'boxes', 'vertices', 'faces']
    for attr in model_attrs:
        if hasattr(col_model_obj, attr):
            value = getattr(col_model_obj, attr)
            if isinstance(value, list):
                img_debugger.debug(f"  {attr}: {len(value)} items")
            else:
                img_debugger.debug(f"  {attr}: {repr(value)}")
        else:
            img_debugger.warning(f"  Missing attribute: {attr}")

    img_debugger.debug(f"=== COL MODEL {model_index} DEBUG END ===")

def debug_col_threading(thread_obj: Any, operation: str = "unknown"):
    """Debug COL threading operations using IMG debug system"""
    if not is_col_debug_enabled():
        return

    img_debugger.debug(f"=== COL THREADING DEBUG: {operation.upper()} ===")

    # Inspect thread object
    img_debugger.inspect_object(thread_obj, f"COLThread_{operation}")

    # Check thread state
    if hasattr(thread_obj, 'isRunning'):
        img_debugger.debug(f"Thread running: {thread_obj.isRunning()}")

    if hasattr(thread_obj, 'isFinished'):
        img_debugger.debug(f"Thread finished: {thread_obj.isFinished()}")

    img_debugger.debug(f"=== COL THREADING DEBUG END ===")

def trace_col_function(func):
    """Decorator to trace COL function calls using IMG debug system"""
    def wrapper(*args, **kwargs):
        if is_col_debug_enabled() and img_debugger.trace_calls:
            start_time = time.time()

            # Log call with COL prefix
            img_debugger.debug(f"COL CALL: {func.__name__}")

            try:
                result = func(*args, **kwargs)
                execution_time = time.time() - start_time
                img_debugger.debug(f"COL RESULT: {func.__name__} completed in {execution_time:.3f}s")
                return result
            except Exception as e:
                img_debugger.error(f"COL ERROR in {func.__name__}: {e}")
                raise
        else:
            return func(*args, **kwargs)

    return wrapper

# Convenience functions for integration
def enable_col_debug():
    """Enable COL debug output"""
    set_col_debug_enabled(True)

def disable_col_debug():
    """Disable COL debug output for performance"""
    set_col_debug_enabled(False)

def toggle_col_debug():
    """Toggle COL debug state"""
    current = is_col_debug_enabled()
    set_col_debug_enabled(not current)
    return not current

# Integration function for main window
def integrate_col_debug_with_main_window(main_window):
    """Integrate COL debug functions into main window"""
    try:
        # Add methods to main window
        main_window.enable_col_debug = enable_col_debug
        main_window.disable_col_debug = disable_col_debug
        main_window.toggle_col_debug = toggle_col_debug
        main_window.is_col_debug_enabled = is_col_debug_enabled

        # Start with debug disabled for performance
        disable_col_debug()

        col_debug_log(main_window, "COL debug functions integrated with IMG debug system", 'COL_INTEGRATION', 'SUCCESS')
        return True

    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ COL debug integration error: {e}")
        else:
            print(f"❌ COL debug integration error: {e}")
        return False


if __name__ == "__main__":
    test_debug_system()


# Export main functions
__all__ = [
    'col_debug_log',
    'debug_col_creation_process',
    'debug_col_import_errors',
    'debug_col_loading_process',
    'debug_col_model_parsing',
    'debug_col_threading',
    'is_col_debug_enabled',
    'set_col_debug_enabled',
    'trace_col_function',
    'enable_col_debug',
    'disable_col_debug',
    'toggle_col_debug',
    'integrate_col_debug_with_main_window',
    'apply_all_fixes_and_improvements',
    'integrate_all_improvements',
    'install_search_manager',
    'fix_search_dialog',
    'install_debug_control_system',
    'setup_img_debug_system_integration',
    'create_debug_menu',
    'add_status_indicators',
    'setup_debug_convenience_methods'
]
