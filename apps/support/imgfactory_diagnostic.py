#!/usr/bin/env python3
"""
Quick diagnostic for IMG Factory split files
"""
import sys
from pathlib import Path

# Add paths
project_root = Path("/home/x2/Documents/GitHub/Img Factory 1.5")
sys.path.insert(0, str(project_root))
sys.path.insert(0, str(project_root / "components"))

print("Testing imports...\n")

files_to_test = [
    "components.Img_Factory.img_factory_logging",
    "components.Img_Factory.img_factory_corruption",
    "components.Img_Factory.img_factory_txd_workshop",
    "components.Img_Factory.img_factory_tab_system",
    "components.Img_Factory.img_factory_col_integration",
    "components.Img_Factory.img_factory_file_operations",
    "components.Img_Factory.img_factory_img_operations",
    "components.Img_Factory.img_factory_entry_operations",
    "components.Img_Factory.img_factory_ui_dialogs",
    "components.Img_Factory.img_factory_utility",
    "components.Img_Factory.img_factory_init",
    "components.Img_Factory.imgload_thread",
]

for module_name in files_to_test:
    try:
        __import__(module_name)
        print(f"✓ {module_name}")
    except Exception as e:
        print(f"✗ {module_name}")
        print(f"  Error: {str(e)}\n")

print("\nDone!")
