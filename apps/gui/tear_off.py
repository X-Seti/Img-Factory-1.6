#this belongs in gui/ tear_off.py - Version: 2
# X-Seti - July12 2025 - Img Factory 1.5
# Credit MexUK 2007 Img Factory 1.2

#!/usr/bin/env python3
"""
IMG Factory Tear-off Panel System
Handles panel tear-off functionality and management
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


class TearOffPanel(QFrame):
    """Panel that can be torn off from main window"""
    
    panel_closed = pyqtSignal(str)  # panel_id
    panel_moved = pyqtSignal(str, QPoint)  # panel_id, position
    panel_resized = pyqtSignal(str, tuple)  # panel_id, (width, height)
    
    def __init__(self, panel_id: str, title: str, parent=None):
        super().__init__(parent)
        self.panel_id = panel_id
        self.title = title
        self.is_torn_off = False
        self.original_parent = parent
        self.original_layout_position = None
        
        # Setup frame
        self.setFrameStyle(QFrame.Shape.StyledPanel)
        self.setLineWidth(1)
        self.setMinimumSize(200, 100)
        
        # Create layout
        self.main_layout = QVBoxLayout(self)
        self.main_layout.setContentsMargins(5, 5, 5, 5)
        self.main_layout.setSpacing(3)
        
        # Title bar
        self._create_title_bar()
        
        # Content area
        self.content_widget = QWidget()
        self.content_layout = QVBoxLayout(self.content_widget)
        self.content_layout.setContentsMargins(0, 0, 0, 0)
        self.main_layout.addWidget(self.content_widget)
        
        # Drag handling
        self.drag_start_position = QPoint()
        self.setAcceptDrops(True)
    
    def _create_title_bar(self):
        """Create title bar with controls"""
        self.title_bar = QFrame()
        self.title_bar.setFixedHeight(24)
        self.title_bar.setStyleSheet("""
            QFrame {
                background-color: #e0e0e0;
                border: 1px solid #c0c0c0;
                border-radius: 3px;
            }
        """)
        
        title_layout = QHBoxLayout(self.title_bar)
        title_layout.setContentsMargins(5, 2, 2, 2)
        
        # Title label
        self.title_label = QLabel(self.title)
        self.title_label.setStyleSheet("font-weight: bold; font-size: 10px;")
        title_layout.addWidget(self.title_label)
        
        title_layout.addStretch()
        
        # Customize button
        self.customize_btn = QPushButton("⚙")
        self.customize_btn.setFixedSize(16, 16)
        self.customize_btn.setToolTip("Customize panel")
        self.customize_btn.clicked.connect(self._show_customize_menu)
        title_layout.addWidget(self.customize_btn)
        
        # Tear off button
        self.tear_off_btn = QPushButton("📌" if not self.is_torn_off else "🔗")
        self.tear_off_btn.setFixedSize(16, 16)
        self.tear_off_btn.setToolTip("Tear off panel")
        self.tear_off_btn.clicked.connect(self.toggle_tear_off)
        title_layout.addWidget(self.tear_off_btn)
        
        # Close button (only when torn off)
        self.close_btn = QPushButton("✕")
        self.close_btn.setFixedSize(16, 16)
        self.close_btn.setToolTip("Close panel")
        self.close_btn.clicked.connect(self._close_panel)
        self.close_btn.setVisible(False)
        title_layout.addWidget(self.close_btn)
        
        self.main_layout.addWidget(self.title_bar)
    
    def _show_customize_menu(self):
        """Show customization menu"""
        menu = QMenu(self)
        
        # Change preset
        preset_menu = menu.addMenu("Change Preset")
        if hasattr(self, 'preset_manager'):
            for preset_name in self.preset_manager.get_preset_names():
                action = preset_menu.addAction(preset_name)
                action.triggered.connect(lambda checked, name=preset_name: self.apply_preset(name))
        
        # Customize layout
        customize_action = menu.addAction("Customize Layout...")
        customize_action.triggered.connect(self._show_customization_dialog)
        
        menu.addSeparator()
        
        # Reset to default
        reset_action = menu.addAction("Reset to Default")
        reset_action.triggered.connect(self.reset_to_default)
        
        menu.exec(QCursor.pos())
    
    def apply_preset(self, preset_name: str):
        """Apply preset (to be overridden by subclasses)"""
        pass
    
    def reset_to_default(self):
        """Reset to default (to be overridden by subclasses)"""
        pass
    
    def toggle_tear_off(self):
        """Toggle tear off state"""
        if self.is_torn_off:
            self.dock_panel()
        else:
            self.tear_off_panel()
    
    def tear_off_panel(self):
        """Tear off panel to separate window"""
        if self.is_torn_off:
            return
        
        # Store original position
        self.original_parent = self.parent()
        self.original_layout_position = self._get_layout_position()
        
        # Remove from parent layout
        if self.parent():
            layout = self.parent().layout()
            if layout:
                layout.removeWidget(self)
        
        # Make it a top-level window
        self.setParent(None)
        self.setWindowFlags(Qt.WindowType.Tool | Qt.WindowType.WindowStaysOnTopHint)
        self.setWindowTitle(f"IMG Factory - {self.title}")
        
        # Update UI
        self.is_torn_off = True
        self.tear_off_btn.setText("🔗")
        self.tear_off_btn.setToolTip("Dock panel")
        self.close_btn.setVisible(True)
        
        # Show as window
        self.show()
        self.raise_()
        
        # Position near cursor
        cursor_pos = QCursor.pos()
        self.move(cursor_pos.x() - 50, cursor_pos.y() - 50)
        
        # Emit signal
        self.panel_moved.emit(self.panel_id, self.pos())
    
    def dock_panel(self):
        """Dock panel back to main window"""
        if not self.is_torn_off or not self.original_parent:
            return
        
        # Hide window
        self.hide()
        
        # Restore to original parent
        self.setParent(self.original_parent)
        self.setWindowFlags(Qt.WindowType.Widget)
        
        # Add back to layout
        if self.original_layout_position:
            self._restore_layout_position()
        
        # Update UI
        self.is_torn_off = False
        self.tear_off_btn.setText("📌")
        self.tear_off_btn.setToolTip("Tear off panel")
        self.close_btn.setVisible(False)
        
        # Show in parent
        self.show()
    
    def _close_panel(self):
        """Close panel (only when torn off)"""
        if self.is_torn_off:
            self.hide()
            self.panel_closed.emit(self.panel_id)
    
    def _get_layout_position(self) -> Optional[Dict]:
        """Get current position in layout"""
        if not self.parent():
            return None
        
        layout = self.parent().layout()
        if not layout:
            return None
        
        # Find position in layout
        for i in range(layout.count()):
            item = layout.itemAt(i)
            if item and item.widget() == self:
                if hasattr(layout, 'getItemPosition'):
                    # Grid layout
                    row, col, rowspan, colspan = layout.getItemPosition(i)
                    return {
                        'type': 'grid',
                        'row': row,
                        'col': col,
                        'rowspan': rowspan,
                        'colspan': colspan
                    }
                else:
                    # Box layout
                    return {
                        'type': 'box',
                        'index': i
                    }
        
        return None
    
    def _restore_layout_position(self):
        """Restore position in layout"""
        if not self.original_parent or not self.original_layout_position:
            return
        
        layout = self.original_parent.layout()
        if not layout:
            return
        
        pos = self.original_layout_position
        
        if pos['type'] == 'grid' and hasattr(layout, 'addWidget'):
            # Grid layout
            layout.addWidget(self, pos['row'], pos['col'], pos['rowspan'], pos['colspan'])
        elif pos['type'] == 'box':
            # Box layout
            layout.insertWidget(pos['index'], self)
    
    def mousePressEvent(self, event):
        """Handle mouse press for dragging"""
        if event.button() == Qt.MouseButton.LeftButton:
            self.drag_start_position = event.pos()
    
    def mouseMoveEvent(self, event):
        """Handle mouse move for dragging"""
        if not (event.buttons() & Qt.MouseButton.LeftButton):
            return
        
        if ((event.pos() - self.drag_start_position).manhattanLength() < 
            QApplication.startDragDistance()):
            return
        
        # If not torn off, tear off on drag
        if not self.is_torn_off:
            self.tear_off_panel()
    
    def moveEvent(self, event):
        """Handle move event"""
        super().moveEvent(event)
        if self.is_torn_off:
            self.panel_moved.emit(self.panel_id, self.pos())
    
    def resizeEvent(self, event):
        """Handle resize event"""
        super().resizeEvent(event)
        if self.is_torn_off:
            self.panel_resized.emit(self.panel_id, (self.width(), self.height()))


class TearOffPanelManager:
    """Manages tear-off panels and their state"""
    
    def __init__(self, main_window):
        self.main_window = main_window
        self.panels: Dict[str, TearOffPanel] = {}
        self.panel_settings_path = Path("config/panel_settings.json")
        self.panel_settings = {}
        
        # Load settings
        self._load_panel_settings()
    
    def register_panel(self, panel: TearOffPanel):
        """Register a panel with the manager"""
        self.panels[panel.panel_id] = panel
        
        # Connect signals
        panel.panel_closed.connect(self._on_panel_closed)
        panel.panel_moved.connect(self._on_panel_moved)
        panel.panel_resized.connect(self._on_panel_resized)
    
    def show_panel(self, panel_id: str):
        """Show a panel"""
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
    
    def dock_all_panels(self):
        """Dock all panels"""
        for panel in self.panels.values():
            if panel.is_torn_off:
                panel.dock_panel()
    
    def _on_panel_closed(self, panel_id: str):
        """Handle panel closed"""
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
        """Save panel settings to file"""
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
    
    def reset_layout(self):
        """Reset panel layout to default"""
        # Dock all panels
        self.dock_all_panels()
        
        # Show all panels
        for panel in self.panels.values():
            panel.setVisible(True)
        
        # Clear saved settings
        self.panel_settings = {}
        self._save_panel_settings()


# ============================================================================
# MENU INTEGRATION FUNCTIONS
# ============================================================================

def add_panel_menu_to_menubar(menu_bar, panel_manager: TearOffPanelManager):
    """Add panels menu to menu bar"""
    panels_menu = menu_bar.addMenu("Panels")
    
    # Show/Hide panels
    show_hide_menu = panels_menu.addMenu("Show/Hide")
    
    for panel_id, panel in panel_manager.panels.items():
        action = QAction(panel.title, menu_bar.parent())
        action.setCheckable(True)
        action.setChecked(panel.isVisible())
        action.triggered.connect(
            lambda checked, pid=panel_id:
            panel_manager.show_panel(pid) if checked
            else panel_manager.hide_panel(pid)
        )
        show_hide_menu.addAction(action)
    
    panels_menu.addSeparator()
    
    # Tear off/Dock panels
    tearoff_menu = panels_menu.addMenu("Tear Off")
    dock_menu = panels_menu.addMenu("Dock")
    
    for panel_id, panel in panel_manager.panels.items():
        # Tear off action
        tearoff_action = QAction(f"Tear Off {panel.title}", menu_bar.parent())
        tearoff_action.setEnabled(not panel.is_torn_off)
        tearoff_action.triggered.connect(
            lambda checked, pid=panel_id: panel_manager.tear_off_panel(pid)
        )
        tearoff_menu.addAction(tearoff_action)
        
        # Dock action
        dock_action = QAction(f"Dock {panel.title}", menu_bar.parent())
        dock_action.setEnabled(panel.is_torn_off)
        dock_action.triggered.connect(
            lambda checked, pid=panel_id: panel_manager.dock_panel(pid)
        )
        dock_menu.addAction(dock_action)
    
    panels_menu.addSeparator()
    
    # Reset layout
    reset_action = QAction("Reset Layout", menu_bar.parent())
    reset_action.triggered.connect(panel_manager.reset_layout)
    panels_menu.addAction(reset_action)
    
    # Save layout
    save_action = QAction("Save Layout", menu_bar.parent())
    save_action.triggered.connect(panel_manager._save_panel_settings)
    panels_menu.addAction(save_action)


def setup_tear_off_system(main_window):
    """Setup tear-off system for main window"""
    try:
        # Create tear-off panel manager
        tearoff_manager = TearOffPanelManager(main_window)
        main_window.tearoff_manager = tearoff_manager
        
        # Setup default panels if needed
        # This can be expanded later when panel creation is implemented
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ Tear-off system setup complete")
        
        return tearoff_manager
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Error setting up tear-off system: {str(e)}")
        return None


# ============================================================================
# EXPORTS
# ============================================================================

__all__ = [
    'TearOffPanel',
    'TearOffPanelManager',
    'add_panel_menu_to_menubar',
    'setup_tear_off_system'
]
