#this belongs in gui/file_menu_integration.py - Version: 4
# X-Seti - August10 2025 - IMG Factory 1.5 - File Menu Integration for Project System

"""
FILE MENU INTEGRATION
Adds Project Folder and Game Root options to the File menu.
Integrates with directory tree system.
"""

from PyQt6.QtWidgets import QMenuBar, QMenu, QFileDialog, QMessageBox
from PyQt6.QtGui import QAction
from PyQt6.QtCore import QSettings, Qt
import os
from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser

##Methods list -
# add_project_menu_items
# handle_set_project_folder
# handle_set_game_root_folder
# handle_project_settings
# save_project_settings
# load_project_settings
# create_project_folder_structure
# validate_game_root_folder

def add_project_menu_items(main_window): #vers 3
    """Add project-related items to File menu - WITH DIRECTORY BROWSER"""
    try:
        # Check if main window already has a menu system
        menubar = main_window.menuBar()
        if not menubar:
            main_window.log_message("❌ No menu bar found")
            return False

        # Find existing File menu
        file_menu = None
        for action in menubar.actions():
            menu_text = action.text().replace("&", "")  # Remove accelerator
            if menu_text == "File":
                file_menu = action.menu()
                break

        if not file_menu:
            # Create File menu if it doesn't exist
            file_menu = menubar.addMenu("&File")
            main_window.log_message("Created File menu")

        # Check if project items already exist (avoid duplicates)
        existing_actions = [action.text() for action in file_menu.actions()]
        if any("Project Folder" in text for text in existing_actions):
            main_window.log_message("Project menu items already exist")
            return True

        # Add separator before project items (if menu has other items)
        if file_menu.actions():
            file_menu.addSeparator()

        # Project Folder action
        project_folder_action = QAction("Set Project Folder...", main_window)
        project_folder_action.setToolTip("Set folder for organizing exported files")
        project_folder_action.triggered.connect(lambda: handle_set_project_folder(main_window))
        project_folder_action.setShortcut("Ctrl+Shift+P")
        file_menu.addAction(project_folder_action)

        # Game Root Folder action
        game_root_action = QAction("Set Game Root Folder...", main_window)
        game_root_action.setToolTip("Set GTA game installation directory")
        game_root_action.triggered.connect(lambda: handle_set_game_root_folder(main_window))
        game_root_action.setShortcut("Ctrl+Shift+G")
        file_menu.addAction(game_root_action)

        # NEW: Directory Browser action
        directory_browser_action = QAction("Browse Game Directory...", main_window)
        directory_browser_action.setToolTip("Browse game directory and switch to Directory Tree tab")
        directory_browser_action.triggered.connect(lambda: handle_browse_game_directory(main_window))
        directory_browser_action.setShortcut("Ctrl+Shift+B")
        file_menu.addAction(directory_browser_action)

        # Auto-detect Game action
        auto_detect_action = QAction("Auto-Detect Game...", main_window)
        auto_detect_action.setToolTip("Automatically find GTA installation")
        auto_detect_action.triggered.connect(lambda: handle_auto_detect_game(main_window))
        file_menu.addAction(auto_detect_action)

        file_menu.addSeparator()

        # Project Settings action
        project_settings_action = QAction("Project Settings...", main_window)
        project_settings_action.setToolTip("Configure project and export settings")
        project_settings_action.triggered.connect(lambda: handle_project_settings(main_window))
        file_menu.addAction(project_settings_action)

        # Store actions for later reference
        main_window.project_folder_action = project_folder_action
        main_window.game_root_action = game_root_action
        main_window.directory_browser_action = directory_browser_action
        main_window.auto_detect_action = auto_detect_action
        main_window.project_settings_action = project_settings_action

        # Load saved settings
        load_project_settings(main_window)

        main_window.log_message("Project menu items added to existing File menu")
        main_window.log_message("Use Ctrl+Shift+B for Directory Browser")

        return True

    except Exception as e:
        main_window.log_message(f"Error adding project menu items: {str(e)}")
        return False


def handle_browse_game_directory(main_window): #vers 1
    """Handle Browse Game Directory menu action - switches to Directory Tree tab"""
    try:
        # First check if we have a game root set
        if not hasattr(main_window, 'game_root') or not main_window.game_root:
            # No game root set, prompt user to set one
            result = QMessageBox.question(
                main_window,
                "No Game Root Set",
                "No game root directory is set.\n\nWould you like to set one now?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.Yes
            )

            if result == QMessageBox.StandardButton.Yes:
                # Open game root selection dialog
                if not handle_set_game_root_folder(main_window):
                    return False
            else:
                main_window.log_message("Directory browser cancelled - no game root set")
                return False

        # Now we should have a game root, switch to Directory Tree tab
        if not hasattr(main_window, 'gui_layout') or not hasattr(main_window.gui_layout, 'tab_widget'):
            main_window.log_message("Tab widget not found for directory browser")
            return False

        tab_widget = main_window.gui_layout.tab_widget

        # Find the Directory Tree tab
        directory_tab_index = -1
        for i in range(tab_widget.count()):
            tab_text = tab_widget.tabText(i)
            if "Directory Tree" in tab_text or "+" in tab_text:
                directory_tab_index = i
                break

        if directory_tab_index == -1:
            main_window.log_message("Directory Tree tab not found")
            QMessageBox.warning(
                main_window,
                "Directory Tree Not Available",
                "Directory Tree tab is not available.\n\nThe directory tree system may not be integrated."
            )
            return False

        # Switch to Directory Tree tab
        tab_widget.setCurrentIndex(directory_tab_index)
        main_window.log_message(f"Switched to Directory Tree tab")

        # Check if we have a directory tree widget and populate it
        if hasattr(main_window, 'directory_tree') and main_window.directory_tree:
            # Use existing directory tree widget (could be comprehensive browser or simple tree)
            directory_widget = main_window.directory_tree

            # Check if it's the comprehensive browser or simple tree
            if hasattr(directory_widget, 'browse_directory'):
                # Comprehensive file browser
                directory_widget.browse_directory(main_window.game_root)
                main_window.log_message(f"File browser populated with: {main_window.game_root}")
            elif hasattr(directory_widget, 'populate_tree'):
                # Simple directory tree
                directory_widget.populate_tree(main_window.game_root)
                main_window.log_message(f"Directory tree populated with: {main_window.game_root}")

            # Update the path display
            if hasattr(directory_widget, 'path_label'):
                directory_widget.path_label.setText(f"Root: {main_window.game_root}")
            elif hasattr(directory_widget, 'address_bar'):
                directory_widget.address_bar.setText(main_window.game_root)

        else:
            # No directory tree widget, try to integrate it first
            main_window.log_message("Directory tree widget not found, attempting integration...")

            try:
                from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                #from apps.gui.directory_tree_system import integrate_directory_tree_system
                if integrate_directory_tree_browser(main_window):
                    main_window.log_message("Directory tree integrated successfully")

                    # Now try to populate it
                    if hasattr(main_window, 'directory_tree') and main_window.directory_tree:
                        directory_widget = main_window.directory_tree
                        if hasattr(directory_widget, 'browse_directory'):
                            directory_widget.browse_directory(main_window.game_root)
                        elif hasattr(directory_widget, 'populate_tree'):
                            directory_widget.populate_tree(main_window.game_root)
                        main_window.log_message(f"Directory widget populated with: {main_window.game_root}")
                else:
                    main_window.log_message("Failed to integrate directory tree")
                    return False

            except ImportError:
                main_window.log_message("Directory tree system not available")
                QMessageBox.warning(
                    main_window,
                    "Directory Tree System Missing",
                    "The directory tree system is not available.\n\nPlease ensure all components are installed."
                )
                return

        # Show success message with appropriate features
        if hasattr(main_window, 'directory_tree') and hasattr(main_window.directory_tree, 'menubar'):
            # Comprehensive file browser
            success_message = f"File Browser activated!\n\nBrowsing: {main_window.game_root}\n\nYou can now:\n• Browse files with full menu system\n• Edit, View, and Settings menus\n• Cut, Copy, Paste operations\n• Create new files and folders\n• Search files and calculate sizes\n• Load IMG files by double-clicking"
        else:
            # Simple directory tree
            success_message = f"Directory Tree tab activated!\n\nBrowsing: {main_window.game_root}\n\nYou can now:\n• Browse game files\n• Load IMG files by double-clicking\n• Use context menus for file operations"

        QMessageBox.information(
            main_window,
            "Directory Browser",
            success_message
        )

        main_window.log_message("Directory browser activated successfully")
        return True

    except Exception as e:
        main_window.log_message(f"Error in directory browser: {str(e)}")
        QMessageBox.critical(
            main_window,
            "Directory Browser Error",
            f"An error occurred while opening the directory browser:\n\n{str(e)}"
        )
        return False


def handle_set_game_root_folder(main_window): #vers 4
    """Handle Set Game Root Folder menu action with directory tree integration and override support"""
    try:
        current_game_root = getattr(main_window, 'game_root', None)
        start_dir = current_game_root if current_game_root else os.path.expanduser("~")

        folder = QFileDialog.getExistingDirectory(
            main_window,
            "Select GTA Game Root Directory - Where your GTA installation is located",
            start_dir,
            QFileDialog.Option.ShowDirsOnly
        )

        if folder:
            # Temporarily set main_window reference for validation function to check override
            validate_game_root_folder._override_check = main_window
            # Validate game root
            game_info = validate_game_root_folder(folder)
            # Clear the reference after validation
            if hasattr(validate_game_root_folder, '_override_check'):
                delattr(validate_game_root_folder, '_override_check')

            if game_info:
                main_window.game_root = folder
                main_window.log_message(f"Game root set: {folder}")

                # Log detection info
                if game_info.get('game_name', '').endswith('(Override)'):
                    main_window.log_message(f"Game root set with override: {folder}")
                else:
                    main_window.log_message(f"Detected: {game_info['game_name']}")

                # Update directory tree if it exists
                if hasattr(main_window, 'directory_tree') and main_window.directory_tree:
                    main_window.directory_tree.game_root = folder
                    main_window.directory_tree.current_root = folder
                    if hasattr(main_window.directory_tree, 'path_label'):
                        main_window.directory_tree.path_label.setText(f"Root: {folder}")
                    # Auto-populate the tree
                    if hasattr(main_window.directory_tree, 'populate_tree'):
                        main_window.directory_tree.populate_tree(folder)
                        main_window.log_message("Directory tree auto-populated")

                # Save settings
                save_project_settings(main_window)

                # Show success dialog with option to browse
                if game_info.get('game_name', '').endswith('(Override)'):
                    message = f"Game root configured with override:\n{folder}\n\nThis folder will be used as your GTA installation directory regardless of standard file detection.\n\nWould you like to browse the directory now?"
                else:
                    message = f"Game root configured:\n{folder}\n\nDetected: {game_info['game_name']}\nEXE: {game_info['exe_file']}\nDAT: {game_info['dat_file']}\nIDE: {game_info['ide_file']}\n\nWould you like to browse the directory now?"

                result = QMessageBox.question(
                    main_window,
                    "Game Root Set Successfully",
                    message,
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.Yes
                )

                if result == QMessageBox.StandardButton.Yes:
                    # Auto-switch to directory browser
                    handle_browse_game_directory(main_window)

                return True
            else:
                # Invalid game root
                override_enabled = getattr(main_window, 'app_settings', None) and main_window.app_settings.current_settings.get('gta_root_override_enabled', False) if hasattr(main_window, 'app_settings') else False

                if override_enabled:
                    # If override is enabled but no standard files found, still allow setting
                    main_window.game_root = folder
                    main_window.log_message(f"Game root set with override: {folder}")

                    # Update directory tree if it exists
                    if hasattr(main_window, 'directory_tree') and main_window.directory_tree:
                        main_window.directory_tree.game_root = folder
                        main_window.directory_tree.current_root = folder
                        if hasattr(main_window.directory_tree, 'path_label'):
                            main_window.directory_tree.path_label.setText(f"Root: {folder}")
                        # Auto-populate the tree
                        if hasattr(main_window.directory_tree, 'populate_tree'):
                            main_window.directory_tree.populate_tree(folder)
                            main_window.log_message("Directory tree auto-populated")

                    # Save settings
                    save_project_settings(main_window)

                    # Show success message
                    result = QMessageBox.question(
                        main_window,
                        "Game Root Set Successfully (Override)",
                        f"Game root configured with override:\n{folder}\n\nThis folder will be used as your GTA installation directory regardless of standard file detection.\n\nWould you like to browse the directory now?",
                        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                        QMessageBox.StandardButton.Yes
                    )

                    if result == QMessageBox.StandardButton.Yes:
                        handle_browse_game_directory(main_window)

                    return True
                else:
                    QMessageBox.warning(
                        main_window,
                        "Invalid Game Directory",
                        f"The selected directory does not appear to be a valid GTA installation:\n{folder}\n\nPlease select the main GTA directory (where gta_sa.exe or similar is located).\n\nTo bypass this check, enable 'GTA Root Override' in Project Settings."
                    )
                    main_window.log_message(f"Invalid game root selected: {folder}")
                    return False
        else:
            main_window.log_message("Game root selection cancelled")
            return False

    except Exception as e:
        main_window.log_message(f"Error setting game root: {str(e)}")
        QMessageBox.critical(
            main_window,
            "Error Setting Game Root",
            f"An error occurred while setting the game root:\n\n{str(e)}"
        )
        return False


def handle_set_project_folder(main_window): #vers 1
    """Handle Set Project Folder menu action"""
    try:
        current_folder = getattr(main_window, 'project_folder', None)
        start_dir = current_folder if current_folder else os.path.expanduser("~")
        
        folder = QFileDialog.getExistingDirectory(
            main_window,
            "Select Project Folder - Where exported files will be organized",
            start_dir,
            QFileDialog.Option.ShowDirsOnly
        )
        
        if folder:
            main_window.project_folder = folder
            
            # Create project folder structure
            if create_project_folder_structure(main_window, folder):
                main_window.log_message(f"Project folder set: {folder}")
                
                # Update directory tree if it exists
                if hasattr(main_window, 'directory_tree'):
                    main_window.directory_tree.project_folder = folder
                    
                # Save settings
                save_project_settings(main_window)
                
                # Show success message
                QMessageBox.information(
                    main_window,
                    "Project Folder Set",
                    f"Project folder configured:\n{folder}\n\nFolder structure created:\n• Models/\n• Textures/\n• Collisions/\n• Maps/\n• Scripts/\n• Other/"
                )
            else:
                main_window.log_message(f"Project folder set but structure creation failed")
        else:
            main_window.log_message("Project folder selection cancelled")
            
    except Exception as e:
        main_window.log_message(f"Error setting project folder: {str(e)}")


def handle_auto_detect_game(main_window): #vers 3
    """Handle Auto-Detect Game menu action with EXE + DAT validation and override support"""
    try:
        main_window.log_message("Auto-detecting GTA installations...")
        
        detected_info = detect_gta_installations_with_info()
        
        # Check if override is enabled and no standard installations found
        override_enabled = getattr(main_window, 'app_settings', None) and main_window.app_settings.current_settings.get('gta_root_override_enabled', False) if hasattr(main_window, 'app_settings') else False
        
        if not detected_info and override_enabled:
            from PyQt6.QtWidgets import QMessageBox
            result = QMessageBox.question(
                main_window,
                "GTA Root Override Enabled",
                "No standard GTA installations detected, but GTA Root Override is enabled.\n\nWould you like to manually select a game root folder?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                QMessageBox.StandardButton.Yes
            )
            
            if result == QMessageBox.StandardButton.Yes:
                handle_set_game_root_folder(main_window)
                return
        
        if not detected_info:
            QMessageBox.information(
                main_window,
                "Auto-Detection Results",
                "No GTA installations detected automatically.\n\nChecked for valid combinations:\n• gta_vc.exe + gta_vc.dat + default.ide\n• gta_sa.exe + gta_sa.dat + default.ide\n• gta3.exe + gta3.dat + default.ide\n• gtasol.exe + gtasol.dat + default.ide/dat\n\nPlease use 'Set Game Root Folder' to browse manually."
            )
            main_window.log_message(" No valid GTA installations auto-detected")
            return
            
        if len(detected_info) == 1:
            # Single installation found
            game_path, game_info = detected_info[0]
            main_window.game_root = game_path
            
            # Update directory tree
            if hasattr(main_window, 'directory_tree'):
                main_window.directory_tree.game_root = game_path
                main_window.directory_tree.current_root = game_path
                main_window.directory_tree.path_label.setText(game_path)
                main_window.directory_tree.populate_tree(game_path)
                
            save_project_settings(main_window)
            
            QMessageBox.information(
                main_window,
                "Auto-Detection Success",
                f"GTA installation detected and configured:\n{game_path}\n\nDetected: {game_info['game_name']}\nEXE: {game_info['exe_file']}\nDAT: {game_info['dat_file']}\nIDE: {game_info['ide_file']}\n\nSwitch to 'Directory Tree' tab to browse game files."
            )
            main_window.log_message(f"Auto-detected {game_info['game_name']}: {game_path}")
            main_window.log_message(f"   EXE: {game_info['exe_file']}")
            main_window.log_message(f"   DAT: {game_info['dat_file']}")
            main_window.log_message(f"   IDE: {game_info['ide_file']}")
            
        else:
            # Multiple installations found - let user choose
            from PyQt6.QtWidgets import QDialog, QVBoxLayout, QListWidget, QPushButton, QLabel, QHBoxLayout, QListWidgetItem
            
            dialog = QDialog(main_window)
            dialog.setWindowTitle("Multiple GTA Installations Found")
            dialog.setMinimumSize(650, 400)
            
            layout = QVBoxLayout(dialog)
            
            label = QLabel("Multiple GTA installations detected with valid EXE + DAT combinations:")
            layout.addWidget(label)
            
            list_widget = QListWidget()
            for path, game_info in detected_info:
                item_text = f"{game_info['game_name']} - {path}\n   EXE: {game_info['exe_file']} | DAT: {game_info['dat_file']} | IDE: {game_info['ide_file']}"
                item = QListWidgetItem(item_text)
                item.setData(Qt.ItemDataRole.UserRole, (path, game_info))
                list_widget.addItem(item)
                
            list_widget.setCurrentRow(0)
            layout.addWidget(list_widget)
            
            button_layout = QHBoxLayout()
            ok_button = QPushButton("Select")
            cancel_button = QPushButton("Cancel")
            
            ok_button.clicked.connect(dialog.accept)
            cancel_button.clicked.connect(dialog.reject)
            
            button_layout.addWidget(ok_button)
            button_layout.addWidget(cancel_button)
            layout.addLayout(button_layout)
            
            if dialog.exec() == QDialog.DialogCode.Accepted:
                current_item = list_widget.currentItem()
                if current_item:
                    path, game_info = current_item.data(Qt.ItemDataRole.UserRole)
                    main_window.game_root = path
                    
                    # Update directory tree
                    if hasattr(main_window, 'directory_tree'):
                        main_window.directory_tree.game_root = path
                        main_window.directory_tree.current_root = path
                        main_window.directory_tree.path_label.setText(path)
                        main_window.directory_tree.populate_tree(path)
                        
                    save_project_settings(main_window)
                    main_window.log_message(f"Selected {game_info['game_name']}: {path}")
            
    except Exception as e:
        main_window.log_message(f"Error in auto-detection: {str(e)}")


def handle_project_settings(main_window): #vers 2
    """Handle Project Settings menu action with override option"""
    try:
        from PyQt6.QtWidgets import QDialog, QVBoxLayout, QFormLayout, QLineEdit, QPushButton, QLabel, QGroupBox, QHBoxLayout, QCheckBox
        
        dialog = QDialog(main_window)
        dialog.setWindowTitle("Project Settings")
        dialog.setMinimumSize(500, 500)
        
        layout = QVBoxLayout(dialog)
        
        # Current settings group
        current_group = QGroupBox("Current Settings")
        current_layout = QFormLayout(current_group)
        
        project_folder = getattr(main_window, 'project_folder', 'Not set')
        game_root = getattr(main_window, 'game_root', 'Not set')
        
        current_layout.addRow("Project Folder:", QLabel(project_folder))
        current_layout.addRow("Game Root:", QLabel(game_root))
        
        layout.addWidget(current_group)
        
        # Override settings group
        override_group = QGroupBox("Override Settings")
        override_layout = QVBoxLayout(override_group)
        
        # GTA Root Override checkbox
        gta_override_checkbox = QCheckBox("Bypass GTA installation validation (GTA Root Override)")
        override_enabled = getattr(main_window, 'app_settings', None) and main_window.app_settings.current_settings.get('gta_root_override_enabled', False) if hasattr(main_window, 'app_settings') else False
        gta_override_checkbox.setChecked(override_enabled)
        
        gta_override_label = QLabel("When enabled, allows setting any folder as GTA root without checking for standard game files (gta_sa.exe, gta_vc.exe, etc.)")
        gta_override_label.setWordWrap(True)
        gta_override_label.setStyleSheet("font-size: 8pt; color: #666666; margin-top: 5px;")
        
        override_layout.addWidget(gta_override_checkbox)
        override_layout.addWidget(gta_override_label)
        
        layout.addWidget(override_group)
        
        # Quick actions group
        actions_group = QGroupBox("Quick Actions")
        actions_layout = QVBoxLayout(actions_group)
        
        set_project_btn = QPushButton("Set Project Folder")
        set_project_btn.clicked.connect(lambda: handle_set_project_folder(main_window))
        actions_layout.addWidget(set_project_btn)
        
        set_game_btn = QPushButton("Set Game Root")
        set_game_btn.clicked.connect(lambda: handle_set_game_root_folder(main_window))
        actions_layout.addWidget(set_game_btn)
        
        auto_detect_btn = QPushButton("Auto-Detect Game")
        auto_detect_btn.clicked.connect(lambda: handle_auto_detect_game(main_window))
        actions_layout.addWidget(auto_detect_btn)
        
        layout.addWidget(actions_group)
        
        # Connect the checkbox to save the setting
        def on_override_changed(checked):
            if hasattr(main_window, 'app_settings') and main_window.app_settings:
                main_window.app_settings.current_settings['gta_root_override_enabled'] = checked
                # Save the setting to the settings file
                main_window.app_settings.save_settings()
                main_window.log_message(f"GTA Root Override setting changed to: {checked}")
        
        gta_override_checkbox.stateChanged.connect(on_override_changed)
        
        # Buttons
        button_layout = QHBoxLayout()
        close_button = QPushButton("Close")
        close_button.clicked.connect(dialog.accept)
        button_layout.addWidget(close_button)
        layout.addLayout(button_layout)
        
        dialog.exec()
        
    except Exception as e:
        main_window.log_message(f"Error showing project settings: {str(e)}")


def create_project_folder_structure(main_window, base_folder: str) -> bool: #vers 1
    """Create standard project folder structure"""
    try:
        folders_to_create = [
            "Models",      # DFF files
            "Textures",    # TXD files  
            "Collisions",  # COL files
            "Maps",        # IPL files
            "Scripts",     # IDE files
            "Audio",       # Audio files
            "Other"        # Everything else
        ]
        
        created_folders = []
        
        for folder in folders_to_create:
            folder_path = os.path.join(base_folder, folder)
            try:
                os.makedirs(folder_path, exist_ok=True)
                created_folders.append(folder)
            except Exception as e:
                main_window.log_message(f"Could not create folder {folder}: {str(e)}")
                
        main_window.log_message(f"Created project folders: {', '.join(created_folders)}")
        return len(created_folders) > 0
        
    except Exception as e:
        main_window.log_message(f"Error creating project structure: {str(e)}")
        return False


def validate_game_root_folder(folder_path: str, main_window=None) -> dict: #vers 6
    """Validate that folder contains GTA installation using EXE + DAT method"""
    try:
        # Check if override is enabled - if so, return basic validation
        if hasattr(validate_game_root_folder, '_override_check'):
            # If we have access to main_window, check the setting
            main_window = validate_game_root_folder._override_check
            if hasattr(main_window, 'app_settings') and main_window.app_settings:
                override_enabled = main_window.app_settings.current_settings.get('gta_root_override_enabled', False)
                if override_enabled and os.path.exists(folder_path):
                    return {
                        'game_name': 'Custom GTA Installation (Override)',
                        'exe_file': 'Unknown',
                        'dat_file': 'Unknown', 
                        'ide_file': 'Unknown',
                        'validated': True
                    }
        
        # Define GTA game signatures: (exe_name, dat_file, ide_file, game_name)
        gta_signatures = [
            # GTA Vice City
            ("gta_vc.exe", "gta_vc.dat", "default.ide", "GTA Vice City"),
            ("gta-vc.exe", "gta_vc.dat", "default.ide", "GTA Vice City"), 
            
            # GTA San Andreas
            ("gta_sa.exe", "gta_sa.dat", "default.ide", "GTA San Andreas"),
            ("gta-sa.exe", "gta_sa.dat", "default.ide", "GTA San Andreas"),
            
            # GTA III
            ("gta3.exe", "gta3.dat", "default.ide", "GTA III"),
            
            # GTA Sol (custom game) - Check for special.dat instead of gta3.ide
            ("solcore.exe", "SOL/gta_sol.dat", "SOL/special.dat", "GTA Sol"),
            ("gtasol.exe", "SOL/gta_sol.dat", "SOL/special.dat", "GTA Sol"),
            ("gta_sol.exe", "SOL/gta_sol.dat", "SOL/special.dat", "GTA Sol"),
            ("solcore.exe", "sol/gta_sol.dat", "sol/special.dat", "GTA Sol"),
            ("gtasol.exe", "sol/gta_sol.dat", "sol/special.dat", "GTA Sol"),
            ("gta_sol.exe", "sol/gta_sol.dat", "sol/special.dat", "GTA Sol"),
            ("solcore.exe", "SOL/gta_sol.dat", "data/special.dat", "GTA Sol"),
            ("gtasol.exe", "SOL/gta_sol.dat", "data/special.dat", "GTA Sol"),
            ("gta_sol.exe", "SOL/gta_sol.dat", "data/special.dat", "GTA Sol"),
            ("solcore.exe", "SOL/gta_sol.dat", "Data/special.dat", "GTA Sol"),
            ("gtasol.exe", "SOL/gta_sol.dat", "Data/special.dat", "GTA Sol"),
            ("gta_sol.exe", "SOL/gta_sol.dat", "Data/special.dat", "GTA Sol"),
            ("solcore.exe", "SOL/gta_sol.dat", "default.ide", "GTA Sol"),
            ("gtasol.exe", "SOL/gta_sol.dat", "default.ide", "GTA Sol"),
            ("gta_sol.exe", "SOL/gta_sol.dat", "default.ide", "GTA Sol"),
            # Also check root locations
            ("solcore.exe", "gta_sol.dat", "Data/default.dat", "GTA Sol"),
            ("gtasol.exe", "gta_sol.dat", "Data/default.dat", "GTA Sol"),
            ("gta_sol.exe", "gta_sol.dat", "Data/default.dat", "GTA Sol"),
            # Additional GTA Sol patterns that might exist
            ("solcore.exe", "gta_sol.dat", "default.ide", "GTA Sol"),
            ("gtasol.exe", "gta_sol.dat", "default.ide", "GTA Sol"),
            ("gta_sol.exe", "gta_sol.dat", "default.ide", "GTA Sol"),
            # Check for SOL folder structure
            ("solcore.exe", "SOL/gta_sol.dat", "SOL/default.ide", "GTA Sol"),
            ("gtasol.exe", "SOL/gta_sol.dat", "SOL/default.ide", "GTA Sol"),
            ("gta_sol.exe", "SOL/gta_sol.dat", "SOL/default.ide", "GTA Sol"),
            # Check for data folder
            ("solcore.exe", "gta_sol.dat", "data/default.dat", "GTA Sol"),
            ("gtasol.exe", "gta_sol.dat", "data/default.dat", "GTA Sol"),
            ("gta_sol.exe", "gta_sol.dat", "data/default.dat", "GTA Sol"),
        ]
        
        for exe_name, dat_file, ide_file, game_name in gta_signatures:
            exe_path = os.path.join(folder_path, exe_name)
            dat_path = os.path.join(folder_path, dat_file)
            ide_path = os.path.join(folder_path, ide_file)
            
            # Check if EXE exists
            if os.path.exists(exe_path):
                # Check if DAT file exists
                if os.path.exists(dat_path):
                    # Check if IDE file exists
                    if os.path.exists(ide_path):
                        return {
                            'game_name': game_name,
                            'exe_file': exe_name,
                            'dat_file': dat_file,
                            'ide_file': ide_file,
                            'validated': True
                        }
                        
        return None
        
    except Exception:
        return None


def detect_gta_installations_with_info() -> list: #vers 2 -  # TODO - add custom game paths, not just windows / linux path names like.
    """Detect GTA installations and return path + detailed info"""
    installations = []
    
    # Common installation paths
    common_paths = [
        # Steam
        "C:/Program Files (x86)/Steam/steamapps/common/Grand Theft Auto San Andreas",
        "C:/Program Files (x86)/Steam/steamapps/common/Grand Theft Auto Vice City",
        "C:/Program Files (x86)/Steam/steamapps/common/Grand Theft Auto III",
        
        # GTA SOL
        "/home/x2/Projects/GTASOLBeta70.5(FireBull)",
        "/home/x2/Projects/GTASOL65.9(Trilogy(NoMoreTatonka)",

        # Rockstar Games Launcher
        "C:/Program Files/Rockstar Games/Grand Theft Auto San Andreas",
        "C:/Program Files/Rockstar Games/Grand Theft Auto Vice City", 
        "C:/Program Files/Rockstar Games/Grand Theft Auto III",
        
        # Common manual installs
        "C:/Games/GTA San Andreas",
        "C:/Games/GTA Vice City",
        "C:/Games/GTA III",
        "C:/Games/GTA Sol",
        
        # Program Files
        "C:/Program Files/Grand Theft Auto San Andreas",
        "C:/Program Files/Grand Theft Auto Vice City",
        "C:/Program Files/Grand Theft Auto III",
        "C:/Program Files/GTA Sol",
    ]
    
    for path in common_paths:
        if os.path.exists(path):
            game_info = validate_game_root_folder(path)
            if game_info:
                installations.append((path, game_info))
                
    return installations


def detect_gta_installations() -> list: #vers 3 # TODO - add custom game paths, not just windows / linux path names like.
    """Detect GTA installations on system using EXE + DAT validation"""
    potential_paths = []
    
    # Common installation paths
    common_paths = [
        # Steam
        "C:/Program Files (x86)/Steam/steamapps/common/Grand Theft Auto San Andreas",
        "C:/Program Files (x86)/Steam/steamapps/common/Grand Theft Auto Vice City",
        "C:/Program Files (x86)/Steam/steamapps/common/Grand Theft Auto III",
        
        # GTA SOL
        "/home/x2/Projects/GTASOLBeta70.5(FireBull)",
        "/home/x2/Projects/GTASOL65.9(Trilogy(NoMoreTatonka)",

        # Rockstar Games Launcher
        "C:/Program Files/Rockstar Games/Grand Theft Auto San Andreas",
        "C:/Program Files/Rockstar Games/Grand Theft Auto Vice City", 
        "C:/Program Files/Rockstar Games/Grand Theft Auto III",
        
        # Common manual installs
        "C:/Games/GTA San Andreas",
        "C:/Games/GTA Vice City",
        "C:/Games/GTA III",
        "C:/Games/GTA Sol",
        
        # Program Files
        "C:/Program Files/Grand Theft Auto San Andreas",
        "C:/Program Files/Grand Theft Auto Vice City",
        "C:/Program Files/Grand Theft Auto III",
        "C:/Program Files/GTA Sol",
    ]
    
    # Check each path using EXE + DAT validation
    for path in common_paths:
        if os.path.exists(path):
            game_info = validate_game_root_folder(path)
            if game_info:
                potential_paths.append(path)
                
    return potential_paths


def save_project_settings(main_window): #vers 2
    """Save project settings to file - now also updates appfactory.settings.json"""
    try:
        # Use QSettings as before for compatibility
        settings = QSettings("IMG Factory", "Project Settings")
        
        if hasattr(main_window, 'project_folder'):
            settings.setValue("project_folder", main_window.project_folder)
            
        if hasattr(main_window, 'game_root'):
            settings.setValue("game_root", main_window.game_root)
        
        # NEW: Also save to appfactory.settings.json if available
        if hasattr(main_window, 'app_settings') and main_window.app_settings:
            # Update the current settings in the app_settings object
            if hasattr(main_window, 'game_root') and main_window.game_root:
                main_window.app_settings.current_settings['working_gta_folder'] = main_window.game_root
            # Save to the JSON file
            main_window.app_settings.save_settings()
        
        main_window.log_message("Project settings saved")
        
    except Exception as e:
        main_window.log_message(f"Error saving settings: {str(e)}")


def load_project_settings(main_window): #vers 1
    """Load project settings from file"""
    try:
        settings = QSettings("IMG Factory", "Project Settings")
        
        project_folder = settings.value("project_folder")
        if project_folder and os.path.exists(project_folder):
            main_window.project_folder = project_folder
            main_window.log_message(f"Loaded project folder: {project_folder}")
            
        game_root = settings.value("game_root")
        if game_root and os.path.exists(game_root):
            main_window.game_root = game_root
            main_window.log_message(f"Loaded game root: {game_root}")
            
            # Update directory tree if it exists
            if hasattr(main_window, 'directory_tree'):
                main_window.directory_tree.game_root = game_root
                main_window.directory_tree.current_root = game_root
                main_window.directory_tree.path_label.setText(game_root)
                main_window.directory_tree.populate_tree(game_root)
                
    except Exception as e:
        main_window.log_message(f"Error loading settings: {str(e)}")


__all__ = [
    'add_project_menu_items',
    'handle_set_project_folder',
    'handle_set_game_root_folder',
    'handle_project_settings',
    'save_project_settings',
    'load_project_settings'
]
            
