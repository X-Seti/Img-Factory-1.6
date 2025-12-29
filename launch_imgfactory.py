#!/usr/bin/env python3
"""
X-Seti - November06 2025 - IMG Factory 1.5 - Root Launcher
#this belongs in root /launch_imgfactory.py - version 1
"""
import sys
from pathlib import Path

# Get the root directory (where this launcher is located)
root_dir = Path(__file__).parent.resolve()

# Add root to path so we can import from apps/
if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

# Now import and run imgfactory from apps/components/Img_Factory/
if __name__ == "__main__":
    try:
        # Import the main module
        from apps.components.Img_Factory import imgfactory
        
        # If imgfactory has a main() function, call it
        if hasattr(imgfactory, 'main'):
            sys.exit(imgfactory.main())
        else:
            print("ERROR: imgfactory.py must have a main() function")
            sys.exit(1)
            
    except ImportError as e:
        print(f"ERROR: Failed to import imgfactory: {e}")
        print(f"Root directory: {root_dir}")
        print(f"Expected path: {root_dir}/apps/components/Img_Factory/imgfactory.py")
        sys.exit(1)
    except Exception as e:
        print(f"ERROR: Failed to start IMG Factory: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
