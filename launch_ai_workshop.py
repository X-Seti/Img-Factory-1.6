#!/usr/bin/env python3
"""
X-Seti - March 2026 - AI Workshop 1.0 - Root Launcher
#this belongs in root /launch_ai_workshop.py - Version: 1
"""
import sys
from pathlib import Path

root_dir = Path(__file__).parent.resolve()

if str(root_dir) not in sys.path:
    sys.path.insert(0, str(root_dir))

if __name__ == "__main__":
    try:
        print("AI Workshop 1.0 Starting...")

        from apps.components.Ai_Workshop.ai_workshop import AIWorkshop
        from PyQt6.QtWidgets import QApplication

        app = QApplication(sys.argv)
        workshop = AIWorkshop()
        workshop.setWindowTitle("AI Workshop 1.0 - Standalone")
        workshop.resize(1300, 800)
        workshop.show()

        sys.exit(app.exec())

    except ImportError as e:
        print(f"ERROR: Failed to import ai_workshop: {e}")
        print(f"Root directory: {root_dir}")
        print(f"Expected path: {root_dir}/apps/components/Ai_Workshop/ai_workshop.py")
        sys.exit(1)
    except Exception as e:
        print(f"ERROR: Failed to start AI Workshop: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
