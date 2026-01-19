#this belongs in gui/gui_menu_custom.py - Version: 4
# X-Seti - January19 2026 - Img Factory 1.6 - Custom UI Menu System

"""
Custom UI Menu System - For gui_layout_custom.py
Uses global theme and SVG icons
"""

from PyQt6.QtWidgets import QMenu, QMessageBox, QFileDialog, QTextEdit, QLabel
from PyQt6.QtCore import Qt, QPoint, QSettings
from PyQt6.QtGui import QAction, QCursor, QKeySequence
import json

##Functions list -
# create_main_popup_menu
# show_popup_menu_at_button

##class CustomMenuManager: -
# __init__
# _get_themed_stylesheet
# _load_recent_files
# create_file_menu
# create_edit_menu
# create_view_menu
# create_tools_menu
# create_project_menu
# create_help_menu
# show_popup_menu


class CustomMenuManager:
    """Manages popup menus for custom UI"""
    
    def __init__(self, main_window): #vers 1
        self.main_window = main_window
        self.recent_files = self._load_recent_files()
    
    
    def _get_themed_stylesheet(self): #vers 1
        """Get menu stylesheet based on global theme"""
        if hasattr(self.main_window, 'app_settings') and self.main_window.app_settings:
            theme_colors = self.main_window.app_settings.get_theme_colors()
            bg_color = theme_colors.get('background', '#2b2b2b')
            text_color = theme_colors.get('text', '#ffffff')
            accent_color = theme_colors.get('accent_primary', '#0078d4')
            border_color = theme_colors.get('border', '#3d3d3d')
            disabled_color = theme_colors.get('disabled_text', '#808080')
        else:
            bg_color = '#2b2b2b'
            text_color = '#ffffff'
            accent_color = '#0078d4'
            border_color = '#3d3d3d'
            disabled_color = '#808080'
        
        return f"""
            QMenu {{
                background-color: {bg_color};
                color: {text_color};
                border: 1px solid {border_color};
                padding: 5px;
            }}
            QMenu::item {{
                background-color: transparent;
                padding: 5px 25px 5px 35px;
                border-radius: 3px;
                margin: 2px 5px;
            }}
            QMenu::item:selected {{
                background-color: {accent_color};
                color: #ffffff;
            }}
            QMenu::item:disabled {{
                color: {disabled_color};
            }}
            QMenu::separator {{
                height: 1px;
                background: {border_color};
                margin: 5px 10px;
            }}
            QMenu::icon {{
                padding-left: 10px;
            }}
        """
    
    
    def _load_recent_files(self): #vers 1
        """Load recent files from settings"""
        try:
            settings = QSettings("IMG-Factory", "IMG-Factory")
            recent = settings.value("recentFiles", [])
            return recent if recent else []
        except:
            return []
    
    
    def create_file_menu(self, menu): #vers 1
        """Add File menu actions with SVG icons"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        
        new_action = QAction(SVGIconFactory.get_add_icon(), "New IMG", self.main_window)
        new_action.setShortcut(QKeySequence("Ctrl+N"))
        new_action.triggered.connect(self._new_img)
        menu.addAction(new_action)
        
        open_action = QAction(SVGIconFactory.get_open_icon(), "Open IMG...", self.main_window)
        open_action.setShortcut(QKeySequence("Ctrl+O"))
        open_action.triggered.connect(self._open_img)
        menu.addAction(open_action)
        
        recent_menu = menu.addMenu(SVGIconFactory.folder_icon(), "Recent Files")
        if not self.recent_files:
            no_recent = QAction("No recent files", self.main_window)
            no_recent.setEnabled(False)
            recent_menu.addAction(no_recent)
        
        menu.addSeparator()
        
        save_action = QAction(SVGIconFactory.get_save_icon(), "Save", self.main_window)
        save_action.setShortcut(QKeySequence("Ctrl+S"))
        save_action.triggered.connect(self._save_img)
        menu.addAction(save_action)
        
        close_action = QAction(SVGIconFactory.get_close_icon(), "Close", self.main_window)
        close_action.setShortcut(QKeySequence("Ctrl+W"))
        close_action.triggered.connect(self._close_img)
        menu.addAction(close_action)
        
        menu.addSeparator()
        
        exit_action = QAction(SVGIconFactory.close_icon(), "Exit", self.main_window)
        exit_action.setShortcut(QKeySequence("Ctrl+Q"))
        exit_action.triggered.connect(lambda: self.main_window.close())
        menu.addAction(exit_action)
    
    
    def create_edit_menu(self, menu): #vers 1
        """Add Edit menu actions with SVG icons"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        
        import_action = QAction(SVGIconFactory.get_import_icon(), "Import Files...", self.main_window)
        import_action.setShortcut(QKeySequence("Ctrl+I"))
        import_action.triggered.connect(self._import_files)
        menu.addAction(import_action)
        
        export_action = QAction(SVGIconFactory.get_export_icon(), "Export Selected...", self.main_window)
        export_action.setShortcut(QKeySequence("Ctrl+E"))
        export_action.triggered.connect(self._export_selected)
        menu.addAction(export_action)
        
        menu.addSeparator()
        
        remove_action = QAction(SVGIconFactory.get_remove_icon(), "Remove Selected", self.main_window)
        remove_action.setShortcut(QKeySequence("Delete"))
        remove_action.triggered.connect(self._remove_selected)
        menu.addAction(remove_action)
        
        rename_action = QAction(SVGIconFactory.get_edit_icon(), "Rename...", self.main_window)
        rename_action.setShortcut(QKeySequence("F2"))
        rename_action.triggered.connect(self._rename_selected)
        menu.addAction(rename_action)
        
        menu.addSeparator()
        
        search_action = QAction(SVGIconFactory.get_search_icon(), "Search/Filter...", self.main_window)
        search_action.setShortcut(QKeySequence("Ctrl+F"))
        search_action.triggered.connect(self._search)
        menu.addAction(search_action)
    
    
    def create_view_menu(self, menu): #vers 1
        """Add View menu actions with SVG icons"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        
        refresh_action = QAction(SVGIconFactory.get_refresh_icon(), "Refresh", self.main_window)
        refresh_action.setShortcut(QKeySequence("F5"))
        refresh_action.triggered.connect(self._refresh_table)
        menu.addAction(refresh_action)
    
    
    def create_tools_menu(self, menu): #vers 1
        """Add Tools menu actions with SVG icons"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        
        rebuild_action = QAction(SVGIconFactory.get_rebuild_icon(), "Rebuild IMG", self.main_window)
        rebuild_action.setShortcut(QKeySequence("Ctrl+B"))
        rebuild_action.triggered.connect(self._rebuild_img)
        menu.addAction(rebuild_action)
        
        validate_action = QAction(SVGIconFactory.check_icon(), "Validate IMG", self.main_window)
        validate_action.triggered.connect(self._validate_img)
        menu.addAction(validate_action)
        
        menu.addSeparator()
        
        settings_action = QAction(SVGIconFactory.get_settings_icon(), "Settings...", self.main_window)
        settings_action.setShortcut(QKeySequence("Ctrl+,"))
        settings_action.triggered.connect(self._show_settings)
        menu.addAction(settings_action)
    
    
    def create_project_menu(self, menu): #vers 1
        """Add Project menu actions with SVG icons"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        
        project_folder_action = QAction(SVGIconFactory.folder_icon(), "Set Project Folder...", self.main_window)
        project_folder_action.setShortcut(QKeySequence("Ctrl+Shift+P"))
        project_folder_action.triggered.connect(lambda: self._set_project_folder())
        menu.addAction(project_folder_action)
        
        game_root_action = QAction(SVGIconFactory.folder_icon(), "Set Game Root Folder...", self.main_window)
        game_root_action.setShortcut(QKeySequence("Ctrl+Shift+G"))
        game_root_action.triggered.connect(lambda: self._set_game_root())
        menu.addAction(game_root_action)
        
        menu.addSeparator()

        # Initialize project manager
        main_window.project_manager = ProjectManager(main_window)

        project_settings_action = QAction(SVGIconFactory.get_settings_icon(), "Project Settings...", self.main_window)
        #project_settings_action.triggered.connect(self._project_settings)
        project_settings_action.triggered.connect(lambda: show_project_manager_dialog(self.main_window))
        menu.addAction(project_settings_action)
    
    
    def create_help_menu(self, menu): #vers 1
        """Add Help menu actions with SVG icons"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        
        help_action = QAction(SVGIconFactory.info_icon(), "Documentation", self.main_window)
        help_action.setShortcut(QKeySequence("F1"))
        help_action.triggered.connect(self._show_help)
        menu.addAction(help_action)
        
        menu.addSeparator()
        
        about_action = QAction(SVGIconFactory.info_icon(), "About IMG Factory", self.main_window)
        about_action.triggered.connect(self._show_about)
        menu.addAction(about_action)
    
    
    def show_popup_menu(self, position=None): #vers 1
        """Show popup menu with theme"""
        from apps.methods.imgfactory_svg_icons import SVGIconFactory
        
        menu = QMenu(self.main_window)
        menu.setStyleSheet(self._get_themed_stylesheet())
        
        file_menu = menu.addMenu(SVGIconFactory.folder_icon(), "File")
        self.create_file_menu(file_menu)
        
        edit_menu = menu.addMenu(SVGIconFactory.get_edit_icon(), "Edit")
        self.create_edit_menu(edit_menu)
        
        view_menu = menu.addMenu(SVGIconFactory.get_view_icon(), "View")
        self.create_view_menu(view_menu)
        
        tools_menu = menu.addMenu(SVGIconFactory.build_icon(), "Tools")
        self.create_tools_menu(tools_menu)
        
        project_menu = menu.addMenu(SVGIconFactory.folder_icon(), "Project")
        self.create_project_menu(project_menu)
        
        menu.addSeparator()
        
        help_menu = menu.addMenu(SVGIconFactory.info_icon(), "Help")
        self.create_help_menu(help_menu)
        
        if position:
            menu.exec(position)
        else:
            menu.exec(QCursor.pos())
    
    
    # Action methods
    def _new_img(self): #vers 1
        try:
            from apps.core.create import create_new_img
            create_new_img(self.main_window)
        except: pass
    
    def _open_img(self): #vers 1
        try:
            from apps.core.open import open_file_dialog
            open_file_dialog(self.main_window)
        except: pass
    
    def _save_img(self): #vers 1
        if hasattr(self.main_window, 'save_img_entry'):
            self.main_window.save_img_entry()
    
    def _close_img(self): #vers 1
        if hasattr(self.main_window, 'close_img_file'):
            self.main_window.close_img_file()
    
    def _import_files(self): #vers 1
        try:
            from apps.core.import import import_files_function
            import_files_function(self.main_window)
        except: pass
    
    def _export_selected(self): #vers 1
        if hasattr(self.main_window, 'export_selected'):
            self.main_window.export_selected()
    
    def _remove_selected(self): #vers 1
        try:
            from apps.core.remove import remove_selected_function
            remove_selected_function(self.main_window)
        except: pass
    
    def _rename_selected(self): #vers 1
        try:
            from apps.core.rename import rename_entry
            rename_entry(self.main_window)
        except: pass
    
    def _search(self): #vers 1
        if hasattr(self.main_window.gui_layout, 'show_search_dialog'):
            self.main_window.gui_layout.show_search_dialog()
    
    def _refresh_table(self): #vers 1
        try:
            from apps.core.reload import reload_current_file
            reload_current_file(self.main_window)
        except: pass
    
    def _rebuild_img(self): #vers 1
        try:
            from apps.core.rebuild import rebuild_current_img_native
            rebuild_current_img_native(self.main_window)
        except: pass
    
    def _validate_img(self): #vers 1
        if hasattr(self.main_window, 'validate_img'):
            self.main_window.validate_img()
    
    def _show_settings(self): #vers 1
        if hasattr(self.main_window, 'show_settings'):
            self.main_window.show_settings()
    
    def _set_project_folder(self): #vers 1
        try:
            from apps.gui.file_menu_integration import handle_set_project_folder
            handle_set_project_folder(self.main_window)
        except: pass
    
    def _set_game_root(self): #vers 1
        try:
            from apps.gui.file_menu_integration import handle_set_game_root_folder
            handle_set_game_root_folder(self.main_window)
        except: pass
    
    def _project_settings(self): #vers 1
        try:
            from apps.gui.file_menu_integration import handle_project_settings
            handle_project_settings(self.main_window)
        except: pass
    
    def _show_help(self): #vers 1
        QMessageBox.information(
            self.main_window,
            "Help",
            "IMG Factory 1.6\n\nCtrl+N - New\nCtrl+O - Open\nCtrl+S - Save\nF5 - Refresh"
        )
    
    def _show_about(self): #vers 1
        QMessageBox.about(
            self.main_window,
            "About IMG Factory 1.6",
            "<h2>IMG Factory 1.6</h2><p>X-Seti 2026</p>"
        )

def show_project_manager_dialog(main_window):
    """Show the project manager dialog"""
    from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QListWidget, QTabWidget, QWidget

    dialog = QDialog(main_window)
    dialog.setWindowTitle("Project Manager")
    dialog.setMinimumSize(600, 400)

    layout = QVBoxLayout(dialog)

    # Tabs for different project operations
    tabs = QTabWidget()

    # Projects list tab
    projects_tab = QWidget()
    projects_layout = QVBoxLayout(projects_tab)

    project_list = QListWidget()
    project_list.addItems(list(main_window.project_manager.projects.keys()))

    # Buttons for project operations
    btn_layout = QHBoxLayout()

    add_btn = QPushButton("Add Project")
    add_btn.clicked.connect(lambda: create_new_project(main_window, dialog))

    delete_btn = QPushButton("Delete Project")
    delete_btn.clicked.connect(lambda: delete_selected_project(main_window, project_list, dialog))

    rename_btn = QPushButton("Rename Project")
    rename_btn.clicked.connect(lambda: rename_selected_project(main_window, project_list, dialog))

    activate_btn = QPushButton("Activate Project")
    activate_btn.clicked.connect(lambda: activate_selected_project(main_window, project_list, dialog))

    btn_layout.addWidget(add_btn)
    btn_layout.addWidget(delete_btn)
    btn_layout.addWidget(rename_btn)
    btn_layout.addWidget(activate_btn)

    projects_layout.addWidget(QLabel("Projects:"))
    projects_layout.addWidget(project_list)
    projects_layout.addLayout(btn_layout)

    tabs.addTab(projects_tab, "Projects")

    # Current project settings tab
    settings_tab = QWidget()
    settings_layout = QVBoxLayout(settings_tab)

    # Show current project and its settings
    current_proj_label = QLabel(f"Current Project: {main_window.project_manager.current_project or 'None'}")
    settings_layout.addWidget(current_proj_label)

    if main_window.project_manager.current_project:
        proj_settings = main_window.project_manager.get_project_settings(main_window.project_manager.current_project)
        settings_text = QTextEdit()
        settings_text.setPlainText(json.dumps(proj_settings, indent=2))
        settings_text.setReadOnly(True)
        settings_layout.addWidget(settings_text)

    tabs.addTab(settings_tab, "Settings")

    layout.addWidget(tabs)

    # Load Project and Close buttons
    button_layout = QHBoxLayout()

    load_project_btn = QPushButton("Load Project")
    load_project_btn.clicked.connect(lambda: handle_set_game_root_folder(main_window))
    button_layout.addWidget(load_project_btn)

    close_btn = QPushButton("Close")
    close_btn.clicked.connect(dialog.accept)
    button_layout.addWidget(close_btn)

    layout.addLayout(button_layout)

    dialog.exec()


def create_new_project(main_window, parent_dialog=None):
    """Create a new project"""
    name, ok = QInputDialog.getText(
        main_window,
        "New Project",
        "Project Name:"
    )

    if ok and name:
        # Get project folder
        project_folder = QFileDialog.getExistingDirectory(
            main_window,
            "Select Project Folder",
            os.path.expanduser("~")
        )

        # Get game root
        game_root = QFileDialog.getExistingDirectory(
            main_window,
            "Select Game Root Folder",
            os.path.expanduser("~")
        )

        if main_window.project_manager.create_project(name, project_folder, game_root):
            # Refresh the project list in the dialog if it exists
            if parent_dialog and hasattr(parent_dialog, 'findChildren'):
                # Find the project list widget and refresh
                for widget in parent_dialog.findChildren(QListWidget):
                    if widget.count() == 0 or widget.item(0).text() != name:
                        widget.clear()
                        widget.addItems(list(main_window.project_manager.projects.keys()))

            QMessageBox.information(
                main_window,
                "Project Created",
                f"Project '{name}' created successfully!"
            )


def delete_selected_project(main_window, project_list, parent_dialog=None):
    """Delete the selected project"""
    current_item = project_list.currentItem()
    if not current_item:
        QMessageBox.warning(main_window, "No Selection", "Please select a project to delete.")
        return

    project_name = current_item.text()

    reply = QMessageBox.question(
        main_window,
        "Confirm Delete",
        f"Are you sure you want to delete project '{project_name}'?\nThis action cannot be undone.",
        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
    )

    if reply == QMessageBox.StandardButton.Yes:
        if main_window.project_manager.delete_project(project_name):
            project_list.takeItem(project_list.currentRow())
            QMessageBox.information(
                main_window,
                "Project Deleted",
                f"Project '{project_name}' deleted successfully."
            )


def rename_selected_project(main_window, project_list, parent_dialog=None):
    """Rename the selected project"""
    current_item = project_list.currentItem()
    if not current_item:
        QMessageBox.warning(main_window, "No Selection", "Please select a project to rename.")
        return

    old_name = current_item.text()
    new_name, ok = QInputDialog.getText(
        main_window,
        "Rename Project",
        "New Project Name:",
        text=old_name
    )

    if ok and new_name and new_name != old_name:
        if main_window.project_manager.rename_project(old_name, new_name):
            current_item.setText(new_name)
            QMessageBox.information(
                main_window,
                "Project Renamed",
                f"Project renamed from '{old_name}' to '{new_name}'."
            )


def activate_selected_project(main_window, project_list, parent_dialog=None):
    """Activate the selected project"""
    current_item = project_list.currentItem()
    if not current_item:
        QMessageBox.warning(main_window, "No Selection", "Please select a project to activate.")
        return

    project_name = current_item.text()

    if main_window.project_manager.set_current_project(project_name):
        # Refresh the settings tab if dialog exists
        if parent_dialog and hasattr(parent_dialog, 'findChildren'):
            for widget in parent_dialog.findChildren(QLabel):
                if "Current Project:" in widget.text():
                    widget.setText(f"Current Project: {project_name}")

        QMessageBox.information(
            main_window,
            "Project Activated",
            f"Project '{project_name}' is now active."
        )


def handle_set_project_folder(main_window):
    """Handle Set Project Folder menu action for current project"""
    try:
        # Check if we have a current project
        if not main_window.project_manager.current_project:
            result = QMessageBox.question(
                main_window,
                "No Active Project",
                "No project is currently active. Would you like to create a new project?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )

            if result == QMessageBox.StandardButton.Yes:
                create_new_project(main_window, None)
                return
            else:
                return

        current_folder = getattr(main_window, 'project_folder', None)
        start_dir = current_folder if current_folder else os.path.expanduser("~")

        folder = QFileDialog.getExistingDirectory(
            main_window,
            "Select Project Folder - Where exported files will be organized",
            start_dir,
            QFileDialog.Option.ShowDirsOnly
        )

        if folder:
            # Update current project settings
            main_window.project_manager.update_project_settings(
                main_window.project_manager.current_project,
                {"project_folder": folder}
            )

            # Also update main window attribute
            main_window.project_folder = folder

            # Create project folder structure
            if create_project_folder_structure(main_window, folder):
                main_window.log_message(f"Project folder set for {main_window.project_manager.current_project}: {folder}")

                # Update directory tree if it exists
                if hasattr(main_window, 'directory_tree'):
                    main_window.directory_tree.project_folder = folder

                # Show success message
                QMessageBox.information(
                    main_window,
                    "Project Folder Set",
                    f"Project folder configured for {main_window.project_manager.current_project}:\n{folder}\n\nFolder structure created:\n• Models/\n• Textures/\n• Collisions/\n• Maps/\n• Scripts/\n• Other/"
                )
            else:
                main_window.log_message(f"Project folder set but structure creation failed")
        else:
            main_window.log_message("Project folder selection cancelled")

    except Exception as e:
        main_window.log_message(f"Error setting project folder: {str(e)}")


def handle_set_game_root_folder(main_window):
    """Handle Set Game Root Folder menu action for current project"""
    try:
        # Check if we have a current project
        if not main_window.project_manager.current_project:
            result = QMessageBox.question(
                main_window,
                "No Active Project",
                "No project is currently active. Would you like to create a new project?",
                QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No
            )

            if result == QMessageBox.StandardButton.Yes:
                create_new_project(main_window, None)
                return
            else:
                return

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
            from apps.gui.file_menu_integration import validate_game_root_folder
            validate_game_root_folder._override_check = main_window
            # Validate game root
            game_info = validate_game_root_folder(folder, main_window)
            # Clear the reference after validation
            if hasattr(validate_game_root_folder, '_override_check'):
                delattr(validate_game_root_folder, '_override_check')

            if game_info:
                # Update current project settings
                main_window.project_manager.update_project_settings(
                    main_window.project_manager.current_project,
                    {"game_root": folder}
                )

                # Also update main window attribute
                main_window.game_root = folder
                main_window.log_message(f"Game root set for {main_window.project_manager.current_project}: {folder}")

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

                # Show success dialog with option to browse
                if game_info.get('game_name', '').endswith('(Override)'):
                    message = f"Game root configured for {main_window.project_manager.current_project}:\n{folder}\n\nThis folder will be used as your GTA installation directory regardless of standard file detection.\n\nWould you like to browse the directory now?"
                else:
                    message = f"Game root configured for {main_window.project_manager.current_project}:\n{folder}\n\nDetected: {game_info['game_name']}\nEXE: {game_info['exe_file']}\nDAT: {game_info['dat_file']}\nIDE: {game_info['ide_file']}\n\nWould you like to browse the directory now?"

                result = QMessageBox.question(
                    main_window,
                    "Game Root Set Successfully",
                    message,
                    QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                    QMessageBox.StandardButton.Yes
                )

                if result == QMessageBox.StandardButton.Yes:
                    # Handle browse game directory
                    from apps.gui.file_menu_integration import handle_browse_game_directory
                    handle_browse_game_directory(main_window)

            else:
                # Invalid game root
                override_enabled = getattr(main_window, 'app_settings', None) and main_window.app_settings.current_settings.get('gta_root_override_enabled', False) if hasattr(main_window, 'app_settings') else False

                if override_enabled:
                    # If override is enabled but no standard files found, still allow setting
                    main_window.project_manager.update_project_settings(
                        main_window.project_manager.current_project,
                        {"game_root": folder}
                    )

                    # Also update main window attribute
                    main_window.game_root = folder
                    main_window.log_message(f"Game root set with override for {main_window.project_manager.current_project}: {folder}")

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

                    # Show success message
                    result = QMessageBox.question(
                        main_window,
                        "Game Root Set Successfully (Override)",
                        f"Game root configured with override for {main_window.project_manager.current_project}:\n{folder}\n\nThis folder will be used as your GTA installation directory regardless of standard file detection.\n\nWould you like to browse the directory now?",
                        QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
                        QMessageBox.StandardButton.Yes
                    )

                    if result == QMessageBox.StandardButton.Yes:
                        from apps.gui.file_menu_integration import handle_browse_game_directory
                        handle_browse_game_directory(main_window)

                else:
                    QMessageBox.warning(
                        main_window,
                        "Invalid Game Directory",
                        f"The selected directory does not appear to be a valid GTA installation:\n{folder}\n\nPlease select the main GTA directory (where gta_sa.exe or similar is located).\n\nTo bypass this check, enable 'GTA Root Override' in Project Settings."
                    )
                    main_window.log_message(f"Invalid game root selected: {folder}")
        else:
            main_window.log_message("Game root selection cancelled")

    except Exception as e:
        main_window.log_message(f"Error setting game root: {str(e)}")
        QMessageBox.critical(
            main_window,
            "Error Setting Game Root",
            f"An error occurred while setting the game root:\n\n{str(e)}"
        )


def create_main_popup_menu(main_window): #vers 1
    """Create popup menu manager"""
    return CustomMenuManager(main_window)


def show_popup_menu_at_button(main_window, button): #vers 1
    """Show popup menu at button"""
    if hasattr(main_window, 'custom_menu_manager'):
        button_pos = button.mapToGlobal(button.rect().bottomLeft())
        main_window.custom_menu_manager.show_popup_menu(button_pos)


__all__ = ['CustomMenuManager', 'create_main_popup_menu', 'show_popup_menu_at_button']
