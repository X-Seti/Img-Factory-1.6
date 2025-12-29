# this belongs in debug.img_debug_functions.py - version 2
#!/usr/bin/env python3
"""
X-Seti - June26 2025 - IMG Debug - Debugging utilities for IMG Factory
Provides comprehensive debugging and tracing for IMG operations
"""

import os
import sys
import traceback
import inspect
import time
from typing import Any, Dict, List, Optional
from pathlib import Path

from apps.debug.unified_debug_functions import debug_trace

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

if __name__ == "__main__":
    test_debug_system()
