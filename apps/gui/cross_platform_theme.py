#this belongs in gui/cross_platform_theme.py - Version: 1
# X-Seti - July22 2025 - IMG Factory 1.5 - Cross-Platform Theme System

"""
Cross-Platform Theme System - Handles platform styling consistently
Fixes grey bleeding and ensures proper widget styling across Linux/Windows/Mac
"""

import sys
import platform
from PyQt6.QtWidgets import QApplication
from PyQt6.QtCore import Qt
from typing import Dict, Any, Optional

##Methods list -
# apply_cross_platform_theme
# detect_platform_theme_needs
# fix_tabwidget_styling
# fix_table_widget_styling  
# get_platform_specific_overrides
# initialize_enhanced_theme_system

class CrossPlatformThemeSystem:
    """Cross-platform theme system with proper fallbacks"""
    
    def __init__(self):
        self.platform = platform.system().lower()
        self.desktop_environment = self._detect_desktop_environment()
        self.qt_style = QApplication.style().objectName().lower()
        
    def _detect_desktop_environment(self) -> str:
        """Detect Linux desktop environment"""
        import os
        de = os.environ.get('XDG_CURRENT_DESKTOP', '').lower()
        if not de:
            de = os.environ.get('DESKTOP_SESSION', '').lower()
        return de
    
    def get_platform_specific_overrides(self) -> Dict[str, str]:
        """Get platform-specific CSS overrides"""
        
        # Base universal styles that work everywhere
        base_styles = {
            'main_window': """
                QMainWindow {
                    background-color: #f5f5f5;
                    color: #333333;
                }
            """,
            
            'tabwidget': """
                QTabWidget::pane {
                    background-color: white;
                    border: 1px solid #cccccc;
                    border-top: none;
                }
                QTabWidget::tab-bar {
                    alignment: left;
                }
                QTabBar::tab {
                    background-color: #e8e8e8;
                    border: 1px solid #cccccc;
                    border-bottom: none;
                    padding: 6px 12px;
                    margin-right: 2px;
                }
                QTabBar::tab:selected {
                    background-color: white;
                    border-bottom: 1px solid white;
                }
                QTabBar::tab:hover:!selected {
                    background-color: #f0f0f0;
                }
            """,
            
            'tablewidget': """
                QTableWidget {
                    background-color: white;
                    color: #333333;
                    alternate-background-color: #f8f8f8;
                    border: 1px solid #cccccc;
                    border-radius: 3px;
                    gridline-color: #e0e0e0;
                    font-size: 9pt;
                    selection-background-color: #e3f2fd;
                    selection-color: #1976d2;
                }
                QTableWidget::item {
                    padding: 5px;
                    border: none;
                    color: #333333;
                }
                QTableWidget::item:selected {
            QTableWidget::item:hover {
                background-color: rgba(100, 150, 255, 0.25);
            }
            QTableWidget::item:selected:hover {
                background-color: rgba(90, 150, 250, 0.5);
            }
                    background-color: #e3f2fd;
                    color: #1976d2;
                }
                QHeaderView::section {
                    background-color: #f0f0f0;
                    color: #333333;
                    padding: 5px;
                    border: 1px solid #cccccc;
                    font-weight: bold;
                    font-size: 9pt;
                }
            """,
            
            'groupbox': """
                QGroupBox {
                    background-color: white;
                    border: 2px solid #cccccc;
                    border-radius: 5px;
                    margin: 10px 0px;
                    padding-top: 10px;
                    font-weight: bold;
                }
                QGroupBox::title {
                    subcontrol-origin: margin;
                    subcontrol-position: top center;
                    padding: 0 5px;
                    background-color: white;
                }
            """,
            
            'textedit': """
                QTextEdit {
                    background-color: white;
                    color: #333333;
                    border: 1px solid #cccccc;
                    border-radius: 3px;
                    padding: 5px;
                    font-family: 'Consolas', 'Monaco', 'Liberation Mono', monospace;
                    font-size: 9pt;
                }
            """
        }
        
        # Platform-specific overrides
        if self.platform == 'linux':
            # Linux-specific fixes for common DEs
            if 'kde' in self.desktop_environment or 'plasma' in self.desktop_environment:
                base_styles['kde_fixes'] = """
                    QWidget {
                        background-color: white;
                    }
                    QTabWidget QWidget {
                        background-color: white;
                    }
                """
            elif 'gnome' in self.desktop_environment:
                base_styles['gnome_fixes'] = """
                    QTabWidget::pane {
                        background-color: white !important;
                        border: 1px solid #cccccc !important;
                    }
                """
            elif 'xfce' in self.desktop_environment:
                base_styles['xfce_fixes'] = """
                    QWidget {
                        background-color: white;
                    }
                """
                
        elif self.platform == 'windows':
            base_styles['windows_fixes'] = """
                QTabWidget::pane {
                    background-color: white;
                    border: 1px solid #adadad;
                }
            """
            
        elif self.platform == 'darwin':  # macOS
            base_styles['macos_fixes'] = """
                QTabWidget::pane {
                    background-color: white;
                    border: 1px solid #d0d0d0;
                }
            """
        
        return base_styles
    
    def apply_enhanced_styling(self, main_window) -> bool:
        """Apply enhanced styling to main window and all widgets"""
        try:
            # Get platform-specific styles
            styles = self.get_platform_specific_overrides()
            
            # Combine all styles
            combined_css = ""
            for style_name, css in styles.items():
                combined_css += f"\n/* {style_name} */\n{css}\n"
            
            # Apply to main window
            main_window.setStyleSheet(combined_css)
            
            # Force specific widget updates
            self._fix_specific_widgets(main_window)
            
            # Log the applied theme info
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"✅ Enhanced theme applied for {self.platform}/{self.desktop_environment}")
            
            return True
            
        except Exception as e:
            if hasattr(main_window, 'log_message'):
                main_window.log_message(f"❌ Enhanced theme failed: {e}")
            return False
    
    def _fix_specific_widgets(self, main_window):
        """Fix specific widgets that might have styling issues"""
        try:
            # Fix TabWidget if it exists
            if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'main_tab_widget'):
                tab_widget = main_window.gui_layout.main_tab_widget
                self._force_tabwidget_styling(tab_widget)
            
            # Fix table widget if it exists
            if hasattr(main_window, 'gui_layout') and hasattr(main_window.gui_layout, 'table'):
                table = main_window.gui_layout.table
                self._force_table_styling(table)
                
        except Exception as e:
            print(f"Warning: Could not fix specific widgets: {e}")
    
    def _force_tabwidget_styling(self, tab_widget):
        """Force proper TabWidget styling"""
        tab_widget.setStyleSheet("""
            QTabWidget::pane {
                background-color: white !important;
                border: 1px solid #cccccc !important;
                border-top: none !important;
            }
            QTabBar::tab {
                background-color: #e8e8e8 !important;
                border: 1px solid #cccccc !important;
                border-bottom: none !important;
                padding: 6px 12px !important;
            }
            QTabBar::tab:selected {
                background-color: white !important;
                border-bottom: 1px solid white !important;
            }
        """)
        
        # Also fix the content widget
        for i in range(tab_widget.count()):
            widget = tab_widget.widget(i)
            if widget:
                widget.setStyleSheet("background-color: white;")
    
    def _force_table_styling(self, table):
        """Force proper table styling"""
        table.setStyleSheet("""
            QTableWidget {
                background-color: white !important;
                color: #333333 !important;
                alternate-background-color: #f8f8f8 !important;
                border: 1px solid #cccccc !important;
                gridline-color: #e0e0e0 !important;
            }
            QTableWidget::item {
                background-color: white !important;
                color: #333333 !important;
                padding: 5px !important;
            }
            QTableWidget::item:alternate {
                background-color: #f8f8f8 !important;
                color: #333333 !important;
            }
            QTableWidget::item:selected {
            QTableWidget::item:hover {
                background-color: rgba(100, 150, 255, 0.25);
            }
            QTableWidget::item:selected:hover {
                background-color: rgba(90, 150, 250, 0.5);
            }
                background-color: #e3f2fd !important;
                color: #1976d2 !important;
            }
            QHeaderView::section {
                background-color: #f0f0f0 !important;
                color: #333333 !important;
            }
        """)

def initialize_cross_platform_theme_system(main_window) -> bool:
    """Initialize the cross-platform theme system for the main window"""
    try:
        theme_system = CrossPlatformThemeSystem()
        success = theme_system.apply_enhanced_styling(main_window)
        
        # Store reference for later use
        main_window._cross_platform_theme_system = theme_system
        
        return success
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Enhanced theme system failed: {e}")
        return False

def force_readable_text_colors(main_window) -> bool:
    """Force readable text colors on all widgets"""
    try:
        # Very aggressive text color override
        readable_css = """
            QWidget {
                color: #333333 !important;
            }
            QTableWidget {
                color: #333333 !important;
                background-color: white !important;
            }
            QTableWidget::item {
                color: #333333 !important;
                background-color: white !important;
            }
            QLabel {
                color: #333333 !important;
            }
            QTabWidget {
                color: #333333 !important;
            }
        """
        
        main_window.setStyleSheet(main_window.styleSheet() + readable_css)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ Readable text colors forced")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Text color fix failed: {e}")
        return False
    """Quick fix for grey bleeding issue specifically"""
    try:
        # Force white backgrounds on problematic widgets
        force_white_css = """
            QTabWidget::pane {
                background-color: white !important;
            }
            QWidget {
                background-color: white;
            }
            QTableWidget {
                background-color: white !important;
            }
            QGroupBox {
                background-color: white !important;
            }
        """
        
        main_window.setStyleSheet(force_white_css)
        
        if hasattr(main_window, 'log_message'):
            main_window.log_message("✅ Grey bleeding fix applied")
        
        return True
        
    except Exception as e:
        if hasattr(main_window, 'log_message'):
            main_window.log_message(f"❌ Grey bleeding fix failed: {e}")
        return False

# Quick integration function
def integrate_cross_platform_theme_system(main_window) -> bool:
    """Quick integration for cross-platform theme system"""
    return initialize_cross_platform_theme_system(main_window)