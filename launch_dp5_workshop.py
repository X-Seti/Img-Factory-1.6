#!/usr/bin/env python3
"""
X-Seti - December14 2025 - DP5_Workshop 1.5 - Root Launcher
#this belongs in root /launch_DP5_workshop.py - Version: 1
"""
import sys
from pathlib import Path

# Get the root directory (where this launcher is located)
root_dir = Path(__file__).parent.resolve()

# Add root to path so we can import from apps/
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

# Now import and run DP5_workshop from apps/components/DP5_Editor/
if __name__ == "__main__":
    try:
        print("DP5_Workshop 1.1 Starting...")
        
        # Import the main module
        from apps.components.DP5_Workshop import dp5_workshop
        
        # If col_workshop has a main() function, call it
        if hasattr(dp5_workshop, 'main'):
            sys.exit(col_workshop.main())
        else:
            # No main() function - run workshop directly
            from PyQt6.QtWidgets import QApplication
            
            app = QApplication(sys.argv)
            workshop = dp5_workshop.DP5Workshop()
            workshop.setWindowTitle("DP5 Workshop 1.1 - Standalone")
            workshop.resize(1200, 800)
            workshop.show()
            
            sys.exit(app.exec())
            
    except ImportError as e:
        print(f"ERROR: Failed to import DP5_workshop: {e}")
        print(f"Root directory: {root_dir}")
        print(f"Expected path: {root_dir}/apps/components/DP5_Editor/DP5_workshop.py")
        sys.exit(1)
    except Exception as e:
        print(f"ERROR: Failed to start DP5_Workshop: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
