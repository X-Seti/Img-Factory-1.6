#this belongs in gui/ panel_manager.py - Version: 6
# X-Seti - July12 2025 - Img Factory 1.5

#!/usr/bin/env python3
"""
Panel Manager for IMG Factory 1.5
Manages all panels and their layouts
"""

import json
from pathlib import Path
from typing import Dict, List, Optional, Callable, Tuple
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QGridLayout, QGroupBox,
    QSplitter, QFrame, QLabel, QPushButton, QComboBox, QCheckBox,
    QSpinBox, QDialog, QListWidget, QListWidgetItem, QMessageBox,
    QApplication, QMenu, QLineEdit
)
from PyQt6.QtCore import Qt, pyqtSignal, QPoint, QRect, QTimer
from PyQt6.QtGui import QDrag, QPixmap, QPainter, QCursor, QIcon, QAction

# Import required classes
from .tear_off import TearOffPanel

def lighten_color(color, factor):
    """Lighten a hex color by factor"""
    try:
        color = color.lstrip('#')
        rgb = tuple(int(color[i:i+2], 16) for i in (0, 2, 4))
        rgb = tuple(min(255, int(c + (255 - c) * factor)) for c in rgb)
        return f"#{rgb[0]:02x}{rgb[1]:02x}{rgb[2]:02x}"
    except:
        return color


def darken_color(color, factor):
    """Darken a hex color by factor"""
    try:
        color = color.lstrip('#')
        rgb = tuple(int(color[i:i+2], 16) for i in (0, 2, 4))
        rgb = tuple(max(0, int(c * (1 - factor))) for c in rgb)
        return f"#{rgb[0]:02x}{rgb[1]:02x}{rgb[2]:02x}"
    except:
        return color


class PanelManager:
    """Manages all panels and their layouts"""
    
    def __init__(self, main_window):
        self.main_window = main_window
        self.panels = {}  # Initialize panels dict
        self.panel_settings_path = Path.home() / ".imgfactory" / "panel_layout.json"
        self.preset_manager = ButtonPresetManager()  # Initialize preset manager
        
        self._load_panel_settings()
    
    def create_default_panels(self, callbacks: Dict[str, Callable]) -> Dict[str, TearOffPanel]:
        """Create default panels"""
        panels = {}
        
        # IMG Operations Panel
        img_panel = ButtonPanel("img_ops", "IMG Operations", self.preset_manager, callbacks)
        panels["img_ops"] = img_panel
        
        # Entries Operations Panel  
        entries_panel = ButtonPanel("entries_ops", "Entries Operations", self.preset_manager, callbacks)
        # Set to entries preset
        entries_panel.apply_preset("Entries Operations")
        panels["entries_ops"] = entries_panel
        
        # Filter & Search Panel

        # Store panels
        self.panels.update(panels)
        
        # Connect signals
        for panel in panels.values():
            panel.panel_closed.connect(self._on_panel_closed)
            panel.panel_moved.connect(self._on_panel_moved)
            panel.panel_resized.connect(self._on_panel_resized)
        
        return panels
    
    def get_panel(self, panel_id: str) -> Optional[TearOffPanel]:
        """Get panel by ID"""
        return self.panels.get(panel_id)
    
    def show_panel(self, panel_id: str):
        """Show a panel (create if needed)"""
        panel = self.panels.get(panel_id)
        if panel:
            if panel.is_torn_off:
                panel.show()
                panel.raise_()
            else:
                panel.setVisible(True)
    
    def hide_panel(self, panel_id: str):
        """Hide a panel"""
        panel = self.panels.get(panel_id)
        if panel:
            panel.setVisible(False)
    
    def tear_off_panel(self, panel_id: str):
        """Tear off a panel"""
        panel = self.panels.get(panel_id)
        if panel and not panel.is_torn_off:
            panel.tear_off_panel()
    
    def dock_panel(self, panel_id: str):
        """Dock a panel"""
        panel = self.panels.get(panel_id)
        if panel and panel.is_torn_off:
            panel.dock_panel()
    
    def reset_layout(self):
        """Reset panel layout to default"""
        for panel in self.panels.values():
            if panel.is_torn_off:
                panel.dock_panel()
            panel.setVisible(True)
    
    def _on_panel_closed(self, panel_id: str):
        """Handle panel closed"""
        # For now, just hide it
        self.hide_panel(panel_id)
    
    def _on_panel_moved(self, panel_id: str, position: QPoint):
        """Handle panel moved"""
        self._save_panel_settings()
    
    def _on_panel_resized(self, panel_id: str, size: tuple):
        """Handle panel resized"""
        self._save_panel_settings()
    
    def _load_panel_settings(self):
        """Load panel settings"""
        try:
            if self.panel_settings_path.exists():
                with open(self.panel_settings_path, 'r') as f:
                    self.panel_settings = json.load(f)
            else:
                self.panel_settings = {}
        except Exception as e:
            print(f"Error loading panel settings: {e}")
            self.panel_settings = {}
    
    def _save_panel_settings(self):
        """Save panel settings"""
        try:
            self.panel_settings_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Collect current panel states
            settings = {}
            for panel_id, panel in self.panels.items():
                settings[panel_id] = {
                    "is_torn_off": panel.is_torn_off,
                    "visible": panel.isVisible(),
                    "geometry": {
                        "x": panel.x(),
                        "y": panel.y(), 
                        "width": panel.width(),
                        "height": panel.height()
                    }
                }
            
            with open(self.panel_settings_path, 'w') as f:
                json.dump(settings, f, indent=2)
                
        except Exception as e:
            print(f"Error saving panel settings: {e}")
    
    def restore_panel_layout(self):
        """Restore saved panel layout"""
        for panel_id, settings in self.panel_settings.items():
            panel = self.panels.get(panel_id)
            if not panel:
                continue
            
            # Restore visibility
            panel.setVisible(settings.get("visible", True))
            
            # Restore tear-off state
            if settings.get("is_torn_off", False):
                panel.tear_off_panel()
                
                # Restore geometry
                geom = settings.get("geometry", {})
                if geom:
                    panel.setGeometry(
                        geom.get("x", 100),
                        geom.get("y", 100),
                        geom.get("width", 200),
                        geom.get("height", 150)
                    )


# Export main classes
__all__ = [
    'PanelManager'
]
