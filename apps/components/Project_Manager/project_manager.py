#!/usr/bin/env python3
#this belongs in components/Project_Manager/project_manager.py - Version: 2
# X-Seti - January08 2026 - IMG Factory 1.6 - Project Manager

"""
PROJECT MANAGER
Handles multiple projects with settings persistence and organization
"""

import sys, os
from pathlib import Path
_root = Path(__file__).resolve().parents[3]
if str(_root) not in sys.path: sys.path.insert(0, str(_root))

import os
import json
from typing import Dict, List, Optional
from pathlib import Path
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QTreeWidget, QTreeWidgetItem,
    QMenuBar, QMenu, QToolBar, QPushButton, QLineEdit, QLabel, QMessageBox,
    QSplitter, QTextEdit, QGroupBox, QInputDialog, QDialog, QFormLayout,
    QCheckBox, QListWidget, QFileDialog, QTabWidget
)
from PyQt6.QtCore import Qt, pyqtSignal
try:
    try:
        from PyQt6.QtGui import QAction
    except ImportError:
        from PyQt6.QtWidgets import QAction
except ImportError:
    from PyQt6.QtWidgets import QAction


##Methods list -
# ProjectManager
# create_project
# delete_project
# rename_project
# load_projects
# save_projects
# get_project_settings
# update_project_settings
# add_project_menu_items

class ProjectManager:
    """Manages multiple projects and their settings"""
    
    def __init__(self, main_window):
        self.main_window = main_window
        self.projects_file = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), "projects.json")
        self.projects = {}
        self.current_project = None
        self.load_projects()
        
    def load_projects(self):
        """Load projects from JSON file"""
        try:
            if os.path.exists(self.projects_file):
                with open(self.projects_file, 'r') as f:
                    self.projects = json.load(f)
                
                # Update legacy projects to include new fields
                for project_name, project_data in self.projects.items():
                    if "assists_path" not in project_data:
                        project_data["assists_path"] = ""
                    if "created_date" not in project_data:
                        project_data["created_date"] = str(Path.home() / "Documents")
                    if "last_used" not in project_data:
                        project_data["last_used"] = ""
                    
                    # Update the project in the dictionary
                    self.projects[project_name] = project_data
                
                # Save the updated projects structure
                self.save_projects()
            else:
                # Initialize with empty projects dict
                self.projects = {}
        except Exception as e:
            self.main_window.log_message(f"Error loading projects: {str(e)}")
            self.projects = {}
            
    def save_projects(self):
        """Save projects to JSON file"""
        try:
            with open(self.projects_file, 'w') as f:
                json.dump(self.projects, f, indent=2)
        except Exception as e:
            self.main_window.log_message(f"Error saving projects: {str(e)}")
            
    def create_project(self, name: str, project_folder: str = "", game_root: str = "", assists_path: str = "") -> bool:
        """Create a new project"""
        if name in self.projects:
            self.main_window.log_message(f"Project '{name}' already exists")
            return False
            
        from datetime import datetime
        self.projects[name] = {
            "name": name,
            "project_folder": project_folder,
            "game_root": game_root,
            "assists_path": assists_path,
            "created_date": datetime.now().isoformat(),
            "last_used": ""
        }
        self.save_projects()
        self.main_window.log_message(f"Created project: {name}")
        return True
        
    def delete_project(self, name: str) -> bool:
        """Delete a project"""
        if name in self.projects:
            del self.projects[name]
            self.save_projects()
            if self.current_project == name:
                self.current_project = None
            self.main_window.log_message(f"Deleted project: {name}")
            return True
        return False
        
    def rename_project(self, old_name: str, new_name: str) -> bool:
        """Rename a project"""
        if old_name not in self.projects:
            return False
        if new_name in self.projects:
            return False
            
        self.projects[new_name] = self.projects.pop(old_name)
        self.projects[new_name]["name"] = new_name
        if self.current_project == old_name:
            self.current_project = new_name
        self.save_projects()
        self.main_window.log_message(f"Renamed project: {old_name} -> {new_name}")
        return True
        
    def get_project_settings(self, name: str) -> Dict:
        """Get settings for a specific project"""
        return self.projects.get(name, {})
        
    def update_project_settings(self, name: str, settings: Dict):
        """Update settings for a specific project"""
        if name in self.projects:
            self.projects[name].update(settings)
            self.save_projects()
            
    def set_current_project(self, name: str): #vers 2
        """Set the current active project"""
        if name in self.projects:
            self.current_project = name
            # Load the project settings into the main window
            project_settings = self.projects[name]
            
            # Update last used timestamp
            from datetime import datetime
            self.projects[name]["last_used"] = datetime.now().isoformat()
            self.save_projects()

            # Record as the last active project in IMG Factory's own
            # settings (not the shared app_settings_system), so it can be
            # auto-restored at next startup without needing projects.json
            # to be re-parsed or the Project Manager opened manually.
            img_settings = getattr(self.main_window, 'img_settings', None)
            if img_settings is not None:
                img_settings.set('last_project_name', name)
                img_settings.set('last_game_root', project_settings.get('game_root', ''))
                img_settings.save_settings()
            
            if "game_root" in project_settings and project_settings["game_root"]:
                self.main_window.game_root = project_settings["game_root"]
                
                # Explicitly call browse_directory to ensure proper path setting and UI update
                if hasattr(self.main_window, 'directory_tree') and hasattr(self.main_window.directory_tree, 'browse_directory'):
                    self.main_window.directory_tree.browse_directory(project_settings["game_root"])

                # Ensure directory tree is integrated
                if not hasattr(self.main_window, 'directory_tree') or not self.main_window.directory_tree:
                    from apps.components.File_Editor.directory_tree_browser import integrate_directory_tree_browser
                    integrate_directory_tree_browser(self.main_window)
                
                # Update directory tree if it exists
                if hasattr(self.main_window, 'directory_tree') and self.main_window.directory_tree:
                    self.main_window.directory_tree.game_root = project_settings["game_root"]
                    self.main_window.directory_tree.current_path = project_settings["game_root"]  # Updated
                    if hasattr(self.main_window.directory_tree, 'path_label'):
                        self.main_window.directory_tree.path_label.setText(f"Root: {project_settings['game_root']}")
                    if hasattr(self.main_window.directory_tree, 'populate_tree'):
                        self.main_window.directory_tree.populate_tree(project_settings["game_root"])
            if "project_folder" in project_settings and project_settings["project_folder"]:
                self.main_window.project_folder = project_settings["project_folder"]
            if "assists_path" in project_settings and project_settings["assists_path"]:
                self.main_window.assists_path = project_settings["assists_path"]
                
            self.main_window.log_message(f"Switched to project: {name}")
            return True
        return False


def show_project_manager_dialog(main_window):
    """Show the project manager dialog"""
    from PyQt6.QtWidgets import QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QListWidget, QTabWidget, QWidget
    
    dialog = QDialog(main_window)
    dialog.setWindowTitle("Project Manager for 1.5")
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
    
    # Create editable settings area
    settings_group = QGroupBox("Project Settings")
    settings_form = QFormLayout(settings_group)
    
    # Project name field
    project_name_field = QLineEdit()
    settings_form.addRow("Project Name:", project_name_field)
    
    # Project folder field
    project_folder_field = QLineEdit()
    project_folder_btn = QPushButton("Browse...")
    project_folder_hbox = QHBoxLayout()
    project_folder_hbox.addWidget(project_folder_field)
    project_folder_hbox.addWidget(project_folder_btn)
    settings_form.addRow("Project Folder:", project_folder_hbox)
    
    # Game root field
    game_root_field = QLineEdit()
    game_root_btn = QPushButton("Browse...")
    game_root_hbox = QHBoxLayout()
    game_root_hbox.addWidget(game_root_field)
    game_root_hbox.addWidget(game_root_btn)
    settings_form.addRow("Game Root:", game_root_hbox)
    
    # Assists path field
    assists_path_field = QLineEdit()
    assists_path_btn = QPushButton("Browse...")
    assists_path_hbox = QHBoxLayout()
    assists_path_hbox.addWidget(assists_path_field)
    assists_path_hbox.addWidget(assists_path_btn)
    settings_form.addRow("Assists Path:", assists_path_hbox)
    
    settings_layout.addWidget(settings_group)
    
    # Edit and Save buttons
    edit_save_layout = QHBoxLayout()
    edit_btn = QPushButton("Edit Settings")
    save_btn = QPushButton("Save Settings")
    save_btn.setEnabled(False)
    edit_save_layout.addWidget(edit_btn)
    edit_save_layout.addWidget(save_btn)
    edit_save_layout.addStretch()
    settings_layout.addLayout(edit_save_layout)
    
    # Connect browse buttons
    def browse_project_folder():
        folder = QFileDialog.getExistingDirectory(main_window, "Select Project Folder", project_folder_field.text() or os.path.expanduser("~"))
        if folder:
            project_folder_field.setText(folder)
    
    def browse_game_root():
        folder = QFileDialog.getExistingDirectory(main_window, "Select Game Root Folder", game_root_field.text() or os.path.expanduser("~"))
        if folder:
            game_root_field.setText(folder)
    
    def browse_assists_path():
        folder = QFileDialog.getExistingDirectory(main_window, "Select Assists Folder", assists_path_field.text() or os.path.expanduser("~"))
        if folder:
            assists_path_field.setText(folder)
            # Create assists folder structure if not already created
            create_assists_folder_structure(main_window, folder)
    
    project_folder_btn.clicked.connect(browse_project_folder)
    game_root_btn.clicked.connect(browse_game_root)
    assists_path_btn.clicked.connect(browse_assists_path)
    
    # Toggle edit mode
    def toggle_edit_mode():
        is_editing = not project_name_field.isReadOnly()
        project_name_field.setReadOnly(is_editing)
        project_folder_field.setReadOnly(is_editing)
        game_root_field.setReadOnly(is_editing)
        assists_path_field.setReadOnly(is_editing)
        edit_btn.setText("Edit Settings" if is_editing else "Cancel Edit")
        save_btn.setEnabled(not is_editing)
    
    edit_btn.clicked.connect(toggle_edit_mode)
    
    # Save settings
    def save_project_settings():
        if not main_window.project_manager.current_project:
            QMessageBox.warning(main_window, "No Project", "No project is currently active.")
            return
        
        # Collect updated settings
        updated_settings = {
            "name": project_name_field.text().strip(),
            "project_folder": project_folder_field.text().strip(),
            "game_root": game_root_field.text().strip(),
            "assists_path": assists_path_field.text().strip()
        }
        
        # Update project settings
        old_name = main_window.project_manager.current_project
        main_window.project_manager.update_project_settings(old_name, updated_settings)
        
        # If project name changed, rename the project
        if updated_settings["name"] != old_name:
            main_window.project_manager.rename_project(old_name, updated_settings["name"])
        
        # Update main window attributes
        main_window.project_manager.set_current_project(updated_settings["name"])
        
        # Refresh UI
        toggle_edit_mode()  # Return to read-only mode
        QMessageBox.information(main_window, "Settings Saved", "Project settings have been saved successfully!")
    
    save_btn.clicked.connect(save_project_settings)
    
    # Load current project settings if available
    if main_window.project_manager.current_project:
        proj_settings = main_window.project_manager.get_project_settings(main_window.project_manager.current_project)
        project_name_field.setText(proj_settings.get("name", ""))
        project_folder_field.setText(proj_settings.get("project_folder", ""))
        game_root_field.setText(proj_settings.get("game_root", ""))
        assists_path_field.setText(proj_settings.get("assists_path", ""))
        # Make fields read-only initially
        project_name_field.setReadOnly(True)
        project_folder_field.setReadOnly(True)
        game_root_field.setReadOnly(True)
        assists_path_field.setReadOnly(True)
    
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


class NewProjectFlowDialog(QDialog): #vers 1
    """Streamlined New Project dialog (Aug 1 2026), replacing the old
    flow of three back-to-back native folder pickers (project folder,
    game root, assets folder, each with no way to skip cleanly) with a
    single dialog matching Keith's own proposed simplification, from
    his own forum reply on the subject: "Pick the game folder
    (optional) pick the assets folder or (skip). otherwise (Create in
    root) assets folder in the game folder. Once you press [save],
    you have the option to activate the game paths; this takes you to
    the Dat_Browser."

    Drops the separate "project folder" step from the old flow
    entirely (Keith's own description of the simplified flow only
    mentions game folder + assets folder) - create_project's own
    project_folder parameter still exists and still gets passed
    through, just always empty from this dialog now, matching what
    Keith actually described wanting."""

    def __init__(self, main_window, parent=None):
        super().__init__(parent)
        self.main_window = main_window
        self.setWindowTitle("New Project")
        self.setMinimumWidth(480)
        self._build_ui()

    def _build_ui(self): #vers 1
        from PyQt6.QtWidgets import QRadioButton, QButtonGroup
        lay = QVBoxLayout(self)

        form = QFormLayout()
        self._name_edit = QLineEdit()
        self._name_edit.setPlaceholderText("Enter project name...")
        form.addRow("Project Name:", self._name_edit)

        game_row = QHBoxLayout()
        self._game_edit = QLineEdit()
        self._game_edit.setPlaceholderText("(optional) - browse to your GTA install folder")
        game_browse_btn = QPushButton("Browse…")
        game_browse_btn.clicked.connect(self._browse_game_folder)
        game_row.addWidget(self._game_edit, stretch=1)
        game_row.addWidget(game_browse_btn)
        form.addRow("Game Folder:", game_row)

        lay.addLayout(form)

        lay.addWidget(QLabel("Assets Folder (for importing/exporting models, textures, maps):"))
        self._assets_group = QButtonGroup(self)
        self._assets_pick_radio = QRadioButton("Pick a folder…")
        self._assets_root_radio = QRadioButton("Create in game folder (an \"Assets\" subfolder)")
        self._assets_skip_radio = QRadioButton("Skip")
        for i, rb in enumerate((self._assets_pick_radio, self._assets_root_radio, self._assets_skip_radio)):
            self._assets_group.addButton(rb, i)
            lay.addWidget(rb)
        self._assets_root_radio.setChecked(True)   # sensible default matching Keith's "otherwise (Create in root)"

        assets_row = QHBoxLayout()
        self._assets_edit = QLineEdit()
        self._assets_edit.setEnabled(False)
        assets_browse_btn = QPushButton("Browse…")
        assets_browse_btn.clicked.connect(self._browse_assets_folder)
        assets_browse_btn.setEnabled(False)
        self._assets_browse_btn = assets_browse_btn
        assets_row.addWidget(self._assets_edit, stretch=1)
        assets_row.addWidget(assets_browse_btn)
        lay.addLayout(assets_row)
        self._assets_pick_radio.toggled.connect(
            lambda checked: (self._assets_edit.setEnabled(checked), assets_browse_btn.setEnabled(checked)))

        lay.addStretch()

        btn_row = QHBoxLayout()
        btn_row.addStretch()
        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        save_btn = QPushButton("Save")
        save_btn.clicked.connect(self._on_save)
        btn_row.addWidget(cancel_btn)
        btn_row.addWidget(save_btn)
        lay.addLayout(btn_row)

    def _browse_game_folder(self): #vers 1
        path = QFileDialog.getExistingDirectory(
            self, "Select Game Root Folder", os.path.expanduser("~"))
        if path:
            self._game_edit.setText(path)
            # Once a game folder is picked, "create in game folder"
            # becomes meaningful - nudge the radio selection there if
            # the user hadn't already deliberately picked something
            # else, since that's the sensible default with a game
            # folder now known.
            if not self._assets_pick_radio.isChecked():
                self._assets_root_radio.setChecked(True)

    def _browse_assets_folder(self): #vers 1
        path = QFileDialog.getExistingDirectory(
            self, "Select Assets Folder", os.path.expanduser("~"))
        if path:
            self._assets_edit.setText(path)

    def _on_save(self): #vers 1
        name = self._name_edit.text().strip()
        if not name:
            QMessageBox.warning(self, "Warning", "Please enter a project name.")
            return
        game_root = self._game_edit.text().strip()

        if self._assets_pick_radio.isChecked():
            assets_path = self._assets_edit.text().strip()
        elif self._assets_root_radio.isChecked():
            # "otherwise (Create in root) assets folder in the game
            # folder" - only meaningful with a game folder actually
            # set; falls back to skipped if there isn't one, same as
            # explicitly choosing Skip, rather than erroring out.
            assets_path = os.path.join(game_root, "Assets") if game_root else ""
        else:
            assets_path = ""

        ok = self.main_window.project_manager.create_project(name, "", game_root, assets_path)
        if not ok:
            QMessageBox.critical(self, "Error", f"Failed to create project '{name}' - a project with that name may already exist.")
            return

        if assets_path:
            create_assists_folder_structure(self.main_window, assets_path)

        # "Once you press [save], you have the option to activate the
        # game paths; this takes you to the Dat_Browser" - streamlines
        # the old New Project -> Open Project -> Activate -> DAT
        # Browser -> load root tree chain (from Keith's own forum
        # reply describing the current flow) into one step right here.
        activate = QMessageBox.question(
            self, "Project Created",
            f"Project '{name}' created successfully.\n\n"
            "Activate this project's game paths now?\nThis will open the DAT Browser.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.Yes)
        if activate == QMessageBox.StandardButton.Yes:
            self.main_window.project_manager.set_current_project(name)
            if game_root:
                self.main_window.game_root = game_root
            # Close this dialog BEFORE opening the DAT Browser (Aug 1
            # 2026, per Keith: "DAT Browser re-opened, stayed on the
            # intro screen, i had to open the dat_browser manullu") -
            # show_dat_browser's own tab.setCurrentIndex() call was
            # running while this dialog was still open and modal on
            # top of the main window; the tab switch happened
            # internally, but with this dialog still covering
            # everything and closing only afterward via accept(), the
            # DAT Browser tab never became visibly current - the
            # previously-active tab (the welcome/intro screen) stayed
            # on screen even though the underlying tab index had
            # already changed underneath it.
            self.accept()
            try:
                from apps.components.Dat_Browser.dat_browser import show_dat_browser
                show_dat_browser(self.main_window)
            except Exception as e:
                if hasattr(self.main_window, 'log_message'):
                    self.main_window.log_message(f"Could not open DAT Browser: {e}")
            return

        self.accept()


def create_new_project(main_window, parent_dialog=None):
    """Create a new project - streamlined single-dialog flow (Aug 1
    2026), replacing three sequential native folder pickers with no
    clean way to skip any of them. See NewProjectFlowDialog for the
    full story. parent_dialog kept for call-site compatibility (the
    project list refresh below still needs it) even though the new
    flow no longer threads it through to the dialog construction the
    way the old one implicitly did via being called inline."""
    dlg = NewProjectFlowDialog(main_window, parent=parent_dialog or main_window)
    if dlg.exec() == QDialog.DialogCode.Accepted:
        name = dlg._name_edit.text().strip()
        # Refresh the project list in the dialog if it exists
        if parent_dialog and hasattr(parent_dialog, 'findChildren'):
            for widget in parent_dialog.findChildren(QListWidget):
                if widget.count() == 0 or widget.item(0).text() != name:
                    widget.clear()
                    widget.addItems(list(main_window.project_manager.projects.keys()))


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


def add_project_menu_items(main_window):
    """Add project management menu items to the main window"""
    try:
        # Check if main window already has a menu system
        menubar = main_window.menuBar()
        if not menubar:
            main_window.log_message("❌ No menu bar found")
            return False

        # Find existing Project menu or create one
        project_menu = None
        for action in menubar.actions():
            menu_text = action.text().replace("&", "")  # Remove accelerator
            if menu_text == "Project":
                project_menu = action.menu()
                break

        if not project_menu:
            # Create Project menu if it doesn't exist
            project_menu = menubar.addMenu("&Project")
            main_window.log_message("Created Project menu")

        # Add project management items
        manage_projects_action = QAction("Manage Projects...", main_window)
        manage_projects_action.setToolTip("Manage multiple projects")
        manage_projects_action.triggered.connect(lambda: show_project_manager_dialog(main_window))
        project_menu.addAction(manage_projects_action)

        project_menu.addSeparator()

        # Add quick actions for current project
        set_project_folder_action = QAction("Set Current Project Folder...", main_window)
        set_project_folder_action.setToolTip("Set folder for current project's exported files")
        set_project_folder_action.triggered.connect(lambda: handle_set_project_folder(main_window))
        project_menu.addAction(set_project_folder_action)

        set_game_root_action = QAction("Set Current Game Root...", main_window)
        set_game_root_action.setToolTip("Set GTA game installation directory for current project")
        set_game_root_action.triggered.connect(lambda: handle_set_game_root_folder(main_window))
        project_menu.addAction(set_game_root_action)

        # Set Current Assets Folder (Aug 1 2026, per Keith: "option
        # would be needed to setup an assets folder later, is that
        # covered") - the New Project dialog's own Skip option meant
        # someone could genuinely have no assets folder set at all,
        # with no direct, discoverable way back to setting one later -
        # only Set Current Game Root/Project Folder had this menu-
        # level equivalent before now.
        set_assets_folder_action = QAction("Set Current Assets Folder...", main_window)
        set_assets_folder_action.setToolTip("Set (or change) the assets folder for the current project")
        set_assets_folder_action.triggered.connect(lambda: handle_set_assets_folder(main_window))
        project_menu.addAction(set_assets_folder_action)

        # Store actions for later reference
        main_window.manage_projects_action = manage_projects_action
        main_window.set_project_folder_action = set_project_folder_action
        main_window.set_game_root_action = set_game_root_action
        main_window.set_assets_folder_action = set_assets_folder_action

        # Initialize project manager
        main_window.project_manager = ProjectManager(main_window)

        main_window.log_message("Project management menu items added")
        return True

    except Exception as e:
        main_window.log_message(f"Error adding project menu items: {str(e)}")
        return False


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


def handle_set_assets_folder(main_window): #vers 1
    """Handle Set Assets Folder menu action for current project - per
    Keith: "option would be needed to setup an assets folder later, is
    that covered." The New Project dialog's Skip option meant a
    project could genuinely have no assets folder at all; this is the
    direct, menu-level way back to setting (or changing) one later,
    matching handle_set_project_folder's own pattern exactly."""
    try:
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

        current_assets = getattr(main_window, 'assists_path', None)
        start_dir = current_assets if current_assets else os.path.expanduser("~")

        folder = QFileDialog.getExistingDirectory(
            main_window,
            "Select Assets Folder - For importing/exporting models, textures, maps",
            start_dir,
            QFileDialog.Option.ShowDirsOnly
        )

        if folder:
            main_window.project_manager.update_project_settings(
                main_window.project_manager.current_project,
                {"assists_path": folder}
            )
            main_window.assists_path = folder

            if create_assists_folder_structure(main_window, folder):
                main_window.log_message(f"Assets folder set for {main_window.project_manager.current_project}: {folder}")
                QMessageBox.information(
                    main_window,
                    "Assets Folder Set",
                    f"Assets folder configured for {main_window.project_manager.current_project}:\n{folder}\n\nFolder structure created:\n• Models/\n• Maps/\n• Collisions/\n• Textures/"
                )
            else:
                main_window.log_message("Assets folder set but structure creation failed")
        else:
            main_window.log_message("Assets folder selection cancelled")

    except Exception as e:
        main_window.log_message(f"Error setting assets folder: {str(e)}")



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


def create_project_folder_structure(main_window, base_folder: str) -> bool:
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


def create_assists_folder_structure(main_window, base_folder: str) -> bool:
    """Create standard assists folder structure for importing/exporting models"""
    try:
        folders_to_create = [
            "Models",      # DFF files
            "Maps",        # IPL files  
            "Collisions",  # COL files
            "Textures"     # TXD files
        ]

        created_folders = []

        for folder in folders_to_create:
            folder_path = os.path.join(base_folder, folder)
            try:
                os.makedirs(folder_path, exist_ok=True)
                created_folders.append(folder)
            except Exception as e:
                main_window.log_message(f"Could not create folder {folder}: {str(e)}")

        main_window.log_message(f"Created assists folders: {', '.join(created_folders)}")
        return len(created_folders) > 0

    except Exception as e:
        main_window.log_message(f"Error creating assists structure: {str(e)}")
        return False


__all__ = [
    'ProjectManager',
    'add_project_menu_items',
    'show_project_manager_dialog',
    'handle_set_project_folder',
    'handle_set_game_root_folder',
    'handle_set_assets_folder',
    'create_assists_folder_structure'
]
