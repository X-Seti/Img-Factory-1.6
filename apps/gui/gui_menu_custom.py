#this belongs in gui/gui_menu_custom.py - Version: 4
# X-Seti - January19 2026 - Img Factory 1.6 - Custom UI Menu System

"""
Custom UI Menu System - For gui_layout_custom.py
Uses global theme and SVG icons
"""

from PyQt6.QtWidgets import QMenu, QMessageBox, QFileDialog, QTextEdit, QLabel, QDialog, QVBoxLayout, QHBoxLayout, QPushButton, QListWidget, QTabWidget, QWidget, QInputDialog
from PyQt6.QtCore import Qt, QPoint, QSettings
from PyQt6.QtGui import QAction, QCursor, QKeySequence
import json
from apps.components.Project_Manager.project_manager import ProjectManager, show_project_manager_dialog, create_new_project, delete_selected_project, rename_selected_project, activate_selected_project
from apps.gui.file_menu_integration import handle_set_project_folder, handle_set_game_root_folder, create_project_folder_structure

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

        # Initialize project manager if it doesn't exist
        if not hasattr(self.main_window, 'project_manager'):
            self.main_window.project_manager = ProjectManager(self.main_window)

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
            from apps.core.impotr import import_files_function
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
        "<h2>IMG Factory 1.6</h2><p>X-Seti 2026</p>",
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
