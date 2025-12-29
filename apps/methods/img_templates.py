#this belongs in components/ img_templates.py - version 20

#!/usr/bin/env python3
"""
X-Seti - June25 2025 - IMG Factory 1.5
Template Manager - Robust template system with error handling
"""

import json
import os
import time
from pathlib import Path
from typing import Dict, List, Optional, Any
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QListWidget, QListWidgetItem,
    QPushButton, QLabel, QTextEdit, QFormLayout, QLineEdit, QComboBox,
    QCheckBox, QSpinBox, QGroupBox, QMessageBox, QInputDialog,
    QSplitter, QFrame, QTabWidget, QScrollArea, QWidget
)
from PyQt6.QtCore import Qt, pyqtSignal, QTimer
from PyQt6.QtGui import QIcon, QFont


class IMGTemplateManager:
    """Manages IMG creation templates with robust error handling"""
    
    def __init__(self, settings_path: str = None):
        if settings_path is None:
            # Create templates directory in user's home
            templates_dir = Path.home() / ".imgfactory" / "templates"
            templates_dir.mkdir(parents=True, exist_ok=True)
            self.settings_path = templates_dir / "templates.json"
        else:
            self.settings_path = Path(settings_path)
        
        self.templates = self._load_templates()
    
    def _load_templates(self) -> Dict:
        """Load templates with robust error handling"""
        default_templates = {
            "version": "1.0",
            "user_templates": [],
            "recent_settings": [],
            "default_templates": [
                {
                    "name": "GTA San Andreas - Standard",
                    "game_type": "gtasa",
                    "description": "Standard GTA SA IMG archive",
                    "settings": {
                        "initial_size_mb": 100,
                        "create_structure": True,
                        "compression_enabled": False,
                        "encryption_enabled": False
                    },
                    "created_date": time.strftime('%Y-%m-%d %H:%M:%S'),
                    "is_default": True
                },
                {
                    "name": "GTA III - Basic",
                    "game_type": "gta3",
                    "description": "Basic GTA III IMG archive",
                    "settings": {
                        "initial_size_mb": 50,
                        "create_structure": False,
                        "compression_enabled": False,
                        "encryption_enabled": False
                    },
                    "created_date": time.strftime('%Y-%m-%d %H:%M:%S'),
                    "is_default": True
                }
            ]
        }
        
        try:
            if self.settings_path.exists():
                with open(self.settings_path, 'r', encoding='utf-8') as f:
                    content = f.read().strip()
                    
                    # Check if file is empty
                    if not content:
                        print("⚠ Template file is empty, using defaults")
                        return default_templates
                    
                    # Try to parse JSON
                    templates = json.loads(content)
                    
                    # Validate structure
                    if not isinstance(templates, dict):
                        print("⚠ Invalid template file format, using defaults")
                        return default_templates
                    
                    # Ensure required keys exist
                    if "user_templates" not in templates:
                        templates["user_templates"] = []
                    if "recent_settings" not in templates:
                        templates["recent_settings"] = []
                    if "default_templates" not in templates:
                        templates["default_templates"] = default_templates["default_templates"]
                    
                    print(f"✓ Loaded {len(templates.get('user_templates', []))} user templates")
                    return templates
                    
            else:
                print("⚠ No template file found, creating with defaults")
                # Create default file
                self._save_templates(default_templates)
                return default_templates
                
        except json.JSONDecodeError as e:
            print(f"⚠ Template file corrupted (JSON error: {e}), using defaults")
            # Backup corrupted file
            try:
                backup_path = self.settings_path.with_suffix('.json.backup')
                if self.settings_path.exists():
                    self.settings_path.rename(backup_path)
                    print(f"✓ Corrupted file backed up to {backup_path}")
            except Exception as backup_error:
                print(f"⚠ Could not backup corrupted file: {backup_error}")
                
        except Exception as e:
            print(f"⚠ Error loading templates: {e}")
        
        # Return defaults and save them
        self._save_templates(default_templates)
        return default_templates
    
    def _save_templates(self, templates: Dict = None):
        """Save templates to file with error handling"""
        if templates is None:
            templates = self.templates
        
        try:
            # Ensure directory exists
            self.settings_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Create backup of existing file
            if self.settings_path.exists():
                backup_path = self.settings_path.with_suffix('.json.backup')
                try:
                    self.settings_path.rename(backup_path)
                except Exception:
                    pass  # Backup failed, continue anyway
            
            # Write new file
            with open(self.settings_path, 'w', encoding='utf-8') as f:
                json.dump(templates, f, indent=2, ensure_ascii=False)
            
            print(f"✓ Templates saved to {self.settings_path}")
            
        except Exception as e:
            print(f"⚠ Error saving templates: {e}")
    
    def get_user_templates(self) -> List[Dict]:
        """Get user-defined templates"""
        return self.templates.get("user_templates", [])
    
    def get_default_templates(self) -> List[Dict]:
        """Get default templates"""
        return self.templates.get("default_templates", [])
    
    def get_all_templates(self) -> List[Dict]:
        """Get all templates (default + user)"""
        defaults = self.get_default_templates()
        users = self.get_user_templates()
        return defaults + users
    
    def save_template(self, name: str, game_type: str, settings: Dict, description: str = "") -> bool:
        """Save a new template"""
        try:
            template = {
                "name": name,
                "game_type": game_type,
                "description": description,
                "settings": settings,
                "created_date": time.strftime('%Y-%m-%d %H:%M:%S'),
                "usage_count": 0,
                "is_default": False
            }
            
            # Check for duplicate names
            existing_names = {t.get("name", "") for t in self.templates.get("user_templates", [])}
            if name in existing_names:
                return False
            
            # Add to user templates
            if "user_templates" not in self.templates:
                self.templates["user_templates"] = []
            
            self.templates["user_templates"].append(template)
            self._save_templates()
            
            print(f"✓ Template '{name}' saved")
            return True
            
        except Exception as e:
            print(f"⚠ Error saving template: {e}")
            return False
    
    def delete_template(self, name: str) -> bool:
        """Delete a user template"""
        try:
            user_templates = self.templates.get("user_templates", [])
            original_count = len(user_templates)
            
            # Remove template with matching name
            self.templates["user_templates"] = [
                t for t in user_templates 
                if t.get("name", "") != name
            ]
            
            if len(self.templates["user_templates"]) < original_count:
                self._save_templates()
                print(f"✓ Template '{name}' deleted")
                return True
            else:
                print(f"⚠ Template '{name}' not found")
                return False
                
        except Exception as e:
            print(f"⚠ Error deleting template: {e}")
            return False
    
    def import_templates(self, file_path: str, overwrite_existing: bool = False) -> Dict:
        """Import templates from file"""
        result = {"imported": 0, "skipped": 0, "errors": []}
        
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                import_data = json.load(f)
            
            imported_templates = import_data.get("user_templates", [])
            existing_names = {t.get("name", "") for t in self.get_user_templates()}
            
            for template in imported_templates:
                template_name = template.get("name", "")
                
                if not template_name:
                    result["errors"].append("Template missing name")
                    continue
                
                if template_name in existing_names and not overwrite_existing:
                    result["skipped"] += 1
                    continue
                
                # Add import metadata
                template["imported"] = True
                template["import_date"] = time.strftime('%Y-%m-%d %H:%M:%S')
                
                if self.save_template(
                    template_name,
                    template.get("game_type", "custom"),
                    template.get("settings", {}),
                    template.get("description", "")
                ):
                    result["imported"] += 1
                else:
                    result["errors"].append(f"Failed to save template: {template_name}")
            
        except Exception as e:
            result["errors"].append(f"Import error: {str(e)}")
        
        return result
    
    def export_templates(self, file_path: str, template_names: Optional[List[str]] = None) -> bool:
        """Export templates to file"""
        try:
            if template_names is None:
                # Export all user templates
                templates_to_export = self.get_user_templates()
            else:
                # Export specific templates
                templates_to_export = [
                    t for t in self.get_user_templates() 
                    if t.get("name") in template_names
                ]
            
            export_data = {
                "user_templates": templates_to_export,
                "exported_on": time.strftime('%Y-%m-%d %H:%M:%S'),
                "exported_by": "IMG Factory 1.5",
                "version": self.templates.get("version", "1.0")
            }
            
            with open(file_path, 'w', encoding='utf-8') as f:
                json.dump(export_data, f, indent=2, ensure_ascii=False)
            
            print(f"✓ Exported {len(templates_to_export)} templates to {file_path}")
            return True
            
        except Exception as e:
            print(f"⚠ Error exporting templates: {e}")
            return False


class TemplateManagerDialog(QDialog):
    """Dialog for managing IMG creation templates"""
    
    template_selected = pyqtSignal(dict)  # Emits selected template data
    
    def __init__(self, template_manager: IMGTemplateManager, parent=None):
        super().__init__(parent)
        self.template_manager = template_manager
        self.setWindowTitle("IMG Template Manager")
        self.setMinimumSize(800, 600)
        self.setModal(True)
        
        self._create_ui()
        self._load_templates()
    
    def _create_ui(self):
        """Create the template manager UI"""
        layout = QVBoxLayout(self)
        
        # Header
        header = QLabel("IMG Template Manager")
        font = QFont()
        font.setPointSize(16)
        font.setBold(True)
        header.setFont(font)
        header.setAlignment(Qt.AlignmentFlag.AlignCenter)
        header.setStyleSheet("color: #2E7D32; margin: 10px;")
        layout.addWidget(header)
        
        # Template list
        self.template_list = QListWidget()
        self.template_list.currentItemChanged.connect(self._on_template_selected)
        layout.addWidget(self.template_list)
        
        # Buttons
        button_layout = QHBoxLayout()
        
        self.use_btn = QPushButton("Use Template")
        self.use_btn.clicked.connect(self._use_template)
        self.use_btn.setEnabled(False)
        button_layout.addWidget(self.use_btn)
        
        self.delete_btn = QPushButton("Delete")
        self.delete_btn.clicked.connect(self._delete_template)
        self.delete_btn.setEnabled(False)
        button_layout.addWidget(self.delete_btn)
        
        button_layout.addStretch()
        
        self.close_btn = QPushButton("Close")
        self.close_btn.clicked.connect(self.accept)
        button_layout.addWidget(self.close_btn)
        
        layout.addLayout(button_layout)
    
    def _load_templates(self):
        """Load templates into list"""
        self.template_list.clear()
        
        # Add default templates
        for template in self.template_manager.get_default_templates():
            item = QListWidgetItem(f" {template.get('name', 'Unnamed')} (Default)")
            item.setData(Qt.ItemDataRole.UserRole, template)
            self.template_list.addItem(item)
        
        # Add user templates
        for template in self.template_manager.get_user_templates():
            item = QListWidgetItem(f" {template.get('name', 'Unnamed')}")
            item.setData(Qt.ItemDataRole.UserRole, template)
            self.template_list.addItem(item)
    
    def _on_template_selected(self, current, previous):
        """Handle template selection"""
        has_selection = current is not None
        self.use_btn.setEnabled(has_selection)
        
        # Only enable delete for user templates
        if current:
            template = current.data(Qt.ItemDataRole.UserRole)
            is_user_template = not template.get("is_default", False)
            self.delete_btn.setEnabled(is_user_template)
        else:
            self.delete_btn.setEnabled(False)
    
    def _use_template(self):
        """Use selected template"""
        current = self.template_list.currentItem()
        if current:
            template = current.data(Qt.ItemDataRole.UserRole)
            self.template_selected.emit(template)
            self.accept()
    
    def _delete_template(self):
        """Delete selected template"""
        current = self.template_list.currentItem()
        if not current:
            return
        
        template = current.data(Qt.ItemDataRole.UserRole)
        template_name = template.get("name", "")
        
        if template.get("is_default", False):
            QMessageBox.warning(self, "Cannot Delete", "Cannot delete default templates.")
            return
        
        reply = QMessageBox.question(
            self, "Delete Template",
            f"Delete template '{template_name}'?",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No,
            QMessageBox.StandardButton.No
        )
        
        if reply == QMessageBox.StandardButton.Yes:
            if self.template_manager.delete_template(template_name):
                self._load_templates()
                QMessageBox.information(self, "Success", "Template deleted successfully!")
            else:
                QMessageBox.warning(self, "Error", "Failed to delete template.")


# For backward compatibility
IMGTemplateManager = IMGTemplateManager
TemplateManagerDialog = TemplateManagerDialog


if __name__ == "__main__":
    # Test the template manager
    print("Template Manager Test")
    
    manager = IMGTemplateManager()
    templates = manager.get_all_templates()
    
    print(f"Found {len(templates)} templates:")
    for template in templates:
        name = template.get("name", "Unnamed")
        game_type = template.get("game_type", "unknown")
        is_default = template.get("is_default", False)
        print(f"  - {name} ({game_type}) {'[Default]' if is_default else '[User]'}")
    
    print("Template Manager ready!")
