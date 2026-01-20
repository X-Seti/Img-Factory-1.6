# Project Manager Updates

## Overview
Enhanced the Project Manager component to address user requirements for better path management and assist functionality.

## Changes Made

### 1. Added Assists Path Support
- Added `assists_path` field to project data structure
- Updated `create_project()` method to accept assists path parameter
- Updated `set_current_project()` method to handle assists path
- Created `create_assists_folder_structure()` function to generate standard folder structure

### 2. Enhanced Settings Tab with Edit/Save Functionality
- Replaced static JSON display with dynamic form fields
- Added input fields for:
  - Project Name
  - Project Folder
  - Game Root
  - Assists Path
- Added Browse buttons for each path field
- Implemented Edit/Save functionality with validation
- Added Cancel functionality

### 3. Improved Project Creation Workflow
- Added assists path selection during project creation
- Automatically creates assists folder structure upon project creation
- Maintains backward compatibility with existing projects

### 4. Folder Structure Created
The assists path automatically creates the following folder structure:
```
├── Models/      (for DFF files)
├── Maps/        (for IPL files)
├── Collisions/  (for COL files)
└── Textures/    (for TXD files)
```

## Files Modified
- `/workspace/apps/components/Project_Manager/project_manager.py`

## Key Features Added

### Edit Button Functionality
- Toggles between read-only and edit modes
- Fields become editable when in edit mode
- Browse buttons remain functional in edit mode

### Save Button Functionality
- Validates all path inputs
- Updates project settings in the JSON file
- Handles project renaming if name changes
- Refreshes UI after saving

### Browse Buttons
- Each path field has its own browse button
- Creates folder structure automatically when assists path is selected
- Uses appropriate dialog titles for each path type

## Benefits
1. **User Control**: Users can now specify their own paths instead of hardcoded defaults
2. **Assist Organization**: Dedicated path for importing/exporting models with proper folder structure
3. **Easy Editing**: Edit project settings without recreating the entire project
4. **Persistent Storage**: All paths are saved and restored with the project
5. **Consistent Structure**: Automatic folder structure creation ensures consistency