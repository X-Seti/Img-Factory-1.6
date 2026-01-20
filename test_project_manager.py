#!/usr/bin/env python3
"""
Test script to verify the enhanced project manager functionality
"""

import sys
import os
sys.path.insert(0, '/workspace')

from apps.components.Project_Manager.project_manager import ProjectManager
from unittest.mock import Mock
import tempfile
import shutil


def test_project_manager():
    """Test the enhanced project manager functionality"""
    print("Testing Enhanced Project Manager...")
    
    # Create a mock main window
    mock_main_window = Mock()
    mock_main_window.log_message = lambda msg: print(f"LOG: {msg}")
    
    # Create a temporary directory for testing
    with tempfile.TemporaryDirectory() as temp_dir:
        # Create test directories
        project_folder = os.path.join(temp_dir, "test_project")
        game_root = os.path.join(temp_dir, "gta_sa")
        assists_path = os.path.join(temp_dir, "assists")
        
        os.makedirs(project_folder, exist_ok=True)
        os.makedirs(game_root, exist_ok=True)
        os.makedirs(assists_path, exist_ok=True)
        
        # Initialize project manager
        pm = ProjectManager(mock_main_window)
        
        # Test creating a project with assists path
        success = pm.create_project(
            name="Test Project",
            project_folder=project_folder,
            game_root=game_root,
            assists_path=assists_path
        )
        
        if success:
            print("✓ Project created successfully with assists path")
        else:
            print("✗ Failed to create project")
            return False
            
        # Check if project exists
        if "Test Project" in pm.projects:
            project_data = pm.projects["Test Project"]
            print(f"✓ Project data: {project_data}")
            
            # Verify all paths are stored correctly
            if (project_data["project_folder"] == project_folder and
                project_data["game_root"] == game_root and
                project_data["assists_path"] == assists_path):
                print("✓ All paths stored correctly")
            else:
                print("✗ Path storage failed")
                return False
        else:
            print("✗ Project not found in projects list")
            return False
            
        # Test updating project settings
        updated_settings = {
            "project_folder": project_folder,
            "game_root": game_root,
            "assists_path": assists_path,
            "modified": True
        }
        
        pm.update_project_settings("Test Project", updated_settings)
        
        if pm.projects["Test Project"].get("modified"):
            print("✓ Project settings updated successfully")
        else:
            print("✗ Project settings update failed")
            return False
            
        # Test setting current project
        pm.set_current_project("Test Project")
        
        if pm.current_project == "Test Project":
            print("✓ Current project set successfully")
        else:
            print("✗ Failed to set current project")
            return False
            
        # Verify that the main window attributes were set
        if (hasattr(mock_main_window, 'project_folder') and
            hasattr(mock_main_window, 'game_root') and
            hasattr(mock_main_window, 'assists_path')):
            print("✓ Main window attributes set correctly")
        else:
            print("✗ Main window attributes not set properly")
            return False
            
        print("\n✓ All tests passed! The enhanced project manager is working correctly.")
        print(f"  - Project name: {pm.projects['Test Project']['name']}")
        print(f"  - Project folder: {mock_main_window.project_folder}")
        print(f"  - Game root: {mock_main_window.game_root}")
        print(f"  - Assists path: {mock_main_window.assists_path}")
        
        return True


if __name__ == "__main__":
    success = test_project_manager()
    if success:
        print("\n🎉 Enhanced Project Manager test completed successfully!")
    else:
        print("\n❌ Tests failed!")
        sys.exit(1)