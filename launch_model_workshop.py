#!/usr/bin/env python3
"""
X-Seti - December14 2025 - Model_Workshop 1.5 - Root Launcher
#this belongs in root /launch_model_workshop.py - Version: 1
"""
import sys
from pathlib import Path

# Get the root directory (where this launcher is located)
root_dir = Path(__file__).parent.resolve()

# Add root to path so we can import from apps/
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

# Now import and run model_workshop from apps/components/Model_Editor/
if __name__ == "__main__":
    try:
        print("Model_Workshop 1.1 Starting...")
        
        # Import the main module
        from apps.components.Model_Editor import model_workshop
        
        # If col_workshop has a main() function, call it
        if hasattr(model_workshop, 'main'):
            sys.exit(col_workshop.main())
        else:
            # No main() function - run workshop directly
            from PyQt6.QtWidgets import QApplication
            
            app = QApplication(sys.argv)
            workshop = model_workshop.ModelWorkshop()
            workshop.setWindowTitle("Model Workshop 1.1 - Standalone")
            workshop.resize(1200, 800)
            workshop.show()
            
            sys.exit(app.exec())
            
    except ImportError as e:
        print(f"ERROR: Failed to import model_workshop: {e}")
        print(f"Root directory: {root_dir}")
        print(f"Expected path: {root_dir}/apps/components/Model_Editor/model_workshop.py")
        sys.exit(1)
    except Exception as e:
        print(f"ERROR: Failed to start Model_Workshop: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
