#!/usr/bin/env python3
"""
Amiga MUI Demo Application
Demonstrates advanced MUI features including:
- Multiple tabs with various gadget types
- Transparency controls
- Shadow effects
- Background image support
- Advanced gadget types
"""

import sys
from PyQt6.QtWidgets import (
    QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
    QTabWidget, QScrollArea, QGroupBox, QFormLayout, QGridLayout,
    QPushButton, QSlider, QLabel, QLineEdit, QSpinBox, QDoubleSpinBox,
    QCheckBox, QRadioButton, QComboBox, QColorDialog, QListWidget,
    QListView, QTableView, QProgressBar, QFrame, QToolButton,
    QStyle, QStyleOptionSlider, QAbstractSlider
)
from PyQt6.QtCore import Qt, pyqtSignal, QRect, QSize, QTimer
from PyQt6.QtGui import (
    QColor, QPainter, QPalette, QBrush, QPixmap, QFont, QLinearGradient,
    QRadialGradient, QPainterPath, QPen, QPolygon
)
import math


class MUISlider(QWidget):
    """Custom MUI-style slider widget"""
    valueChanged = pyqtSignal(int)
    
    def __init__(self, minimum=0, maximum=100, value=50, orientation=Qt.Orientation.Horizontal):
        super().__init__()
        self.minimum = minimum
        self.maximum = maximum
        self.value = value
        self.orientation = orientation
        self.is_pressed = False
        self.knob_pos = 0
        
        self.setMinimumSize(100, 20 if orientation == Qt.Orientation.Horizontal else 100)
        
        # Calculate initial knob position
        self._update_knob_position()
    
    def _update_knob_position(self):
        range_val = self.maximum - self.minimum
        if range_val == 0:
            self.knob_pos = 0
        else:
            pos_ratio = (self.value - self.minimum) / range_val
            if self.orientation == Qt.Orientation.Horizontal:
                self.knob_pos = int(pos_ratio * (self.width() - 20))
            else:
                self.knob_pos = int(pos_ratio * (self.height() - 20))
    
    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        # Draw track
        if self.orientation == Qt.Orientation.Horizontal:
            track_rect = QRect(10, self.height()//2 - 3, self.width() - 20, 6)
        else:
            track_rect = QRect(self.width()//2 - 3, 10, 6, self.height() - 20)
        
        # Track gradient
        if self.orientation == Qt.Orientation.Horizontal:
            gradient = QLinearGradient(track_rect.left(), track_rect.top(), track_rect.right(), track_rect.top())
        else:
            gradient = QLinearGradient(track_rect.left(), track_rect.top(), track_rect.left(), track_rect.bottom())
        
        gradient.setColorAt(0, QColor(100, 100, 100))
        gradient.setColorAt(1, QColor(180, 180, 180))
        
        painter.fillRect(track_rect, gradient)
        
        # Draw border around track
        painter.setPen(QColor(80, 80, 80))
        painter.drawRect(track_rect)
        
        # Draw knob
        knob_size = 20
        if self.orientation == Qt.Orientation.Horizontal:
            knob_x = 10 + self.knob_pos
            knob_y = self.height() // 2 - knob_size // 2
        else:
            knob_x = self.width() // 2 - knob_size // 2
            knob_y = 10 + self.knob_pos
            
        knob_rect = QRect(knob_x, knob_y, knob_size, knob_size)
        
        # Knob gradient
        radial_gradient = QRadialGradient(
            knob_rect.center(),
            knob_size // 2,
            knob_rect.center()
        )
        radial_gradient.setColorAt(0, QColor(220, 220, 220))
        radial_gradient.setColorAt(1, QColor(150, 150, 150))
        
        painter.fillRect(knob_rect, radial_gradient)
        
        # Knob border
        painter.setPen(QColor(70, 70, 70))
        painter.drawRect(knob_rect)
        
        # Draw value indicator
        painter.setPen(QColor(255, 255, 255))
        painter.drawText(knob_rect, Qt.AlignmentFlag.AlignCenter, str(self.value))
    
    def mousePressEvent(self, event):
        self.is_pressed = True
        self._update_value_from_mouse(event.position().x() if self.orientation == Qt.Orientation.Horizontal else event.position().y())
        self.update()
    
    def mouseMoveEvent(self, event):
        if self.is_pressed:
            self._update_value_from_mouse(event.position().x() if self.orientation == Qt.Orientation.Horizontal else event.position().y())
            self.update()
    
    def mouseReleaseEvent(self, event):
        self.is_pressed = False
    
    def _update_value_from_mouse(self, pos):
        if self.orientation == Qt.Orientation.Horizontal:
            track_start = 10
            track_length = self.width() - 20
            pos = max(track_start, min(pos - 10, track_length - 10))
            pos_ratio = pos / track_length if track_length > 0 else 0
        else:
            track_start = 10
            track_length = self.height() - 20
            pos = max(track_start, min(pos - 10, track_length - 10))
            pos_ratio = pos / track_length if track_length > 0 else 0
            
        new_value = int(self.minimum + pos_ratio * (self.maximum - self.minimum))
        new_value = max(self.minimum, min(new_value, self.maximum))
        
        if new_value != self.value:
            self.value = new_value
            self._update_knob_position()
            self.valueChanged.emit(self.value)
    
    def setValue(self, value):
        self.value = max(self.minimum, min(value, self.maximum))
        self._update_knob_position()
        self.update()
    
    def getValue(self):
        return self.value


class MUIKnob(QWidget):
    """Custom MUI-style knob widget"""
    valueChanged = pyqtSignal(float)
    
    def __init__(self, minimum=0.0, maximum=100.0, value=50.0):
        super().__init__()
        self.minimum = minimum
        self.maximum = maximum
        self.value = value
        self.is_pressed = False
        
        self.setMinimumSize(50, 50)
    
    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        center = self.rect().center()
        radius = min(self.width(), self.height()) // 2 - 5
        
        # Draw knob body
        gradient = QRadialGradient(center, radius, center)
        gradient.setColorAt(0, QColor(200, 200, 200))
        gradient.setColorAt(1, QColor(100, 100, 100))
        
        painter.setBrush(gradient)
        painter.setPen(QPen(QColor(50, 50, 50), 2))
        painter.drawEllipse(center, radius, radius)
        
        # Draw indicator line
        angle_range = 270  # 270 degrees of rotation
        angle_start = 135  # Start at top-left
        
        value_ratio = (self.value - self.minimum) / (self.maximum - self.minimum)
        current_angle = angle_start + (value_ratio * angle_range)
        
        radian_angle = math.radians(current_angle)
        line_length = radius * 0.7
        
        start_x = center.x()
        start_y = center.y()
        end_x = start_x + line_length * math.cos(radian_angle)
        end_y = start_y + line_length * math.sin(radian_angle)
        
        painter.setPen(QPen(QColor(255, 255, 255), 3))
        painter.drawLine(int(start_x), int(start_y), int(end_x), int(end_y))
        
        # Draw value text
        painter.setPen(QColor(0, 0, 0))
        font = painter.font()
        font.setPointSize(8)
        painter.setFont(font)
        painter.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, f"{self.value:.1f}")
    
    def mousePressEvent(self, event):
        self.is_pressed = True
        self.last_pos = event.position()
    
    def mouseMoveEvent(self, event):
        if self.is_pressed:
            delta_x = event.position().x() - self.last_pos.x()
            delta_y = event.position().y() - self.last_pos.y()
            
            # Adjust value based on movement
            value_change = (delta_x - delta_y) * (self.maximum - self.minimum) / 200.0
            new_value = self.value + value_change
            new_value = max(self.minimum, min(new_value, self.maximum))
            
            if abs(new_value - self.value) > 0.1:  # Only emit if changed significantly
                self.value = new_value
                self.valueChanged.emit(self.value)
                self.update()
            
            self.last_pos = event.position()
    
    def mouseReleaseEvent(self, event):
        self.is_pressed = False
    
    def setValue(self, value):
        self.value = max(self.minimum, min(value, self.maximum))
        self.valueChanged.emit(self.value)
        self.update()
    
    def getValue(self):
        return self.value


class MUILevelMeter(QWidget):
    """Custom MUI-style level meter widget"""
    
    def __init__(self, orientation=Qt.Orientation.Vertical, num_bars=10):
        super().__init__()
        self.orientation = orientation
        self.num_bars = num_bars
        self.levels = [0.0] * num_bars  # Each bar has its own level
        
        if orientation == Qt.Orientation.Vertical:
            self.setMinimumSize(20, 100)
        else:
            self.setMinimumSize(100, 20)
    
    def setLevel(self, index, level):
        if 0 <= index < self.num_bars:
            self.levels[index] = max(0.0, min(level, 1.0))
            self.update()
    
    def setAllLevels(self, levels):
        for i, level in enumerate(levels):
            if i >= self.num_bars:
                break
            self.levels[i] = max(0.0, min(level, 1.0))
        self.update()
    
    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        # Draw background
        painter.fillRect(self.rect(), QColor(50, 50, 50))
        
        # Calculate bar dimensions
        if self.orientation == Qt.Orientation.Vertical:
            bar_width = self.width() - 4
            bar_height = (self.height() - 4 - (self.num_bars - 1) * 2) // self.num_bars
            spacing = 2
        else:
            bar_height = self.height() - 4
            bar_width = (self.width() - 4 - (self.num_bars - 1) * 2) // self.num_bars
            spacing = 2
        
        # Draw each bar
        for i in range(self.num_bars):
            if self.orientation == Qt.Orientation.Vertical:
                x = 2
                y = 2 + i * (bar_height + spacing)
                filled_height = int(bar_height * self.levels[i])
                
                # Determine color based on level
                if self.levels[i] > 0.8:
                    color = QColor(255, 0, 0)  # Red for high levels
                elif self.levels[i] > 0.5:
                    color = QColor(255, 255, 0)  # Yellow for medium levels
                else:
                    color = QColor(0, 255, 0)  # Green for low levels
                
                # Draw unfilled portion
                unfilled_rect = QRect(x, y, bar_width, bar_height - filled_height)
                painter.fillRect(unfilled_rect, QColor(30, 30, 30))
                
                # Draw filled portion
                filled_rect = QRect(x, y + (bar_height - filled_height), bar_width, filled_height)
                painter.fillRect(filled_rect, color)
                
                # Draw border
                painter.setPen(QColor(80, 80, 80))
                painter.drawRect(x, y, bar_width, bar_height)
            else:
                x = 2 + i * (bar_width + spacing)
                y = 2
                filled_width = int(bar_width * self.levels[i])
                
                # Determine color based on level
                if self.levels[i] > 0.8:
                    color = QColor(255, 0, 0)  # Red for high levels
                elif self.levels[i] > 0.5:
                    color = QColor(255, 255, 0)  # Yellow for medium levels
                else:
                    color = QColor(0, 255, 0)  # Green for low levels
                
                # Draw unfilled portion
                unfilled_rect = QRect(x + filled_width, y, bar_width - filled_width, bar_height)
                painter.fillRect(unfilled_rect, QColor(30, 30, 30))
                
                # Draw filled portion
                filled_rect = QRect(x, y, filled_width, bar_height)
                painter.fillRect(filled_rect, color)
                
                # Draw border
                painter.setPen(QColor(80, 80, 80))
                painter.drawRect(x, y, bar_width, bar_height)


class MUIPaletteWidget(QWidget):
    """Custom MUI-style palette widget"""
    
    def __init__(self, colors=None):
        super().__init__()
        if colors is None:
            self.colors = [
                QColor(255, 0, 0), QColor(0, 255, 0), QColor(0, 0, 255),
                QColor(255, 255, 0), QColor(255, 0, 255), QColor(0, 255, 255),
                QColor(255, 128, 0), QColor(128, 0, 255), QColor(255, 0, 128),
                QColor(0, 128, 255), QColor(128, 255, 0), QColor(0, 255, 128)
            ]
        else:
            self.colors = colors
            
        self.selected_index = 0
        self.columns = 4
        self.cell_size = 30
        
        rows = (len(self.colors) + self.columns - 1) // self.columns
        width = self.columns * self.cell_size + 10
        height = rows * self.cell_size + 10
        self.setFixedSize(width, height)
    
    def paintEvent(self, event):
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        
        for i, color in enumerate(self.colors):
            row = i // self.columns
            col = i % self.columns
            
            x = 5 + col * self.cell_size
            y = 5 + row * self.cell_size
            
            rect = QRect(x, y, self.cell_size - 2, self.cell_size - 2)
            
            # Draw color cell
            painter.fillRect(rect, color)
            
            # Draw border
            if i == self.selected_index:
                painter.setPen(QPen(QColor(255, 255, 255), 3))
            else:
                painter.setPen(QPen(QColor(80, 80, 80), 1))
            
            painter.drawRect(rect)
    
    def mousePressEvent(self, event):
        pos = event.position()
        x = int(pos.x())
        y = int(pos.y())
        
        # Calculate which cell was clicked
        col = (x - 5) // self.cell_size
        row = (y - 5) // self.cell_size
        
        index = row * self.columns + col
        
        if 0 <= index < len(self.colors):
            self.selected_index = index
            self.update()
    
    def getSelectedColor(self):
        if 0 <= self.selected_index < len(self.colors):
            return self.colors[self.selected_index]
        return None


class MUITabWidget(QTabWidget):
    """Custom MUI-styled tab widget with transparency and shadow support"""
    
    def __init__(self):
        super().__init__()
        self.transparency = 1.0
        self.shadow_depth = 0
        self.setStyleSheet("""
            QTabWidget::pane {
                border: 2px solid #555;
                border-radius: 5px;
                background: rgba(80, 80, 80, 200);
            }
            QTabBar::tab {
                background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                          stop: 0 #aaa, stop: 1 #888);
                border: 1px solid #555;
                border-bottom-color: #555;
                border-top-left-radius: 4px;
                border-top-right-radius: 4px;
                min-width: 8ex;
                padding: 2px;
            }
            QTabBar::tab:selected, QTabBar::tab:hover {
                background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                          stop: 0 #fff, stop: 1 #ddd);
            }
            QTabBar::tab:selected {
                border-color: #9B9B9B;
                border-bottom-color: #C2C7CB;
            }
        """)
    
    def setTransparency(self, alpha):
        self.transparency = max(0.0, min(alpha, 1.0))
        # Update style based on transparency
        self.setStyleSheet(f"""
            QTabWidget::pane {{
                border: 2px solid #555;
                border-radius: 5px;
                background: rgba(80, 80, 80, {int(self.transparency * 200)});
            }}
            QTabBar::tab {{
                background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                          stop: 0 rgba(170, 170, 170, {int(self.transparency * 255)}), 
                                          stop: 1 rgba(136, 136, 136, {int(self.transparency * 255)}));
                border: 1px solid #555;
                border-bottom-color: #555;
                border-top-left-radius: 4px;
                border-top-right-radius: 4px;
                min-width: 8ex;
                padding: 2px;
            }}
            QTabBar::tab:selected, QTabBar::tab:hover {{
                background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                          stop: 0 rgba(255, 255, 255, {int(self.transparency * 255)}), 
                                          stop: 1 rgba(221, 221, 221, {int(self.transparency * 255)}));
            }}
            QTabBar::tab:selected {{
                border-color: #9B9B9B;
                border-bottom-color: #C2C7CB;
            }}
        """)
    
    def setShadowDepth(self, depth):
        self.shadow_depth = max(0, min(depth, 10))
        # Apply shadow effect if needed
        if self.shadow_depth > 0:
            self.setStyleSheet(self.styleSheet() + f"""
                QTabWidget::pane {{
                    margin: {self.shadow_depth}px;
                }}
            """)


class MUIWindow(QMainWindow):
    """Main MUI-styled application window"""
    
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Amiga MUI Demo")
        self.setGeometry(100, 100, 1000, 700)
        
        # Set window background
        self.setStyleSheet("""
            QMainWindow {
                background-color: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 1,
                                                stop: 0 #222, stop: 1 #444);
            }
        """)
        
        # Create central widget and layout
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        main_layout = QVBoxLayout(central_widget)
        
        # Create tab widget
        self.tab_widget = MUITabWidget()
        main_layout.addWidget(self.tab_widget)
        
        # Create menu bar
        self.create_menus()
        
        # Create tabs
        self.create_basic_gadgets_tab()
        self.create_advanced_gadgets_tab()
        self.create_visual_effects_tab()
        self.create_image_backgrounds_tab()
        self.create_transparency_tab()
        
        # Status bar
        self.statusBar().showMessage("Amiga MUI Demo Ready")
    
    def create_menus(self):
        menubar = self.menuBar()
        
        # File menu
        file_menu = menubar.addMenu('File')
        
        exit_action = file_menu.addAction('Exit')
        exit_action.triggered.connect(self.close)
        
        # View menu
        view_menu = menubar.addMenu('View')
        
        toggle_transparency_action = view_menu.addAction('Toggle Transparency')
        toggle_transparency_action.triggered.connect(self.toggle_transparency)
        
        toggle_shadows_action = view_menu.addAction('Toggle Shadows')
        toggle_shadows_action.triggered.connect(self.toggle_shadows)
        
        # Help menu
        help_menu = menubar.addMenu('Help')
        
        about_action = help_menu.addAction('About')
        about_action.triggered.connect(self.show_about)
    
    def create_basic_gadgets_tab(self):
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        # Create group boxes for different gadget types
        string_group = QGroupBox("String Gadget")
        string_layout = QHBoxLayout(string_group)
        string_layout.addWidget(QLabel("Text Input:"))
        string_layout.addWidget(QLineEdit("Sample Text"))
        layout.addWidget(string_group)
        
        numeric_group = QGroupBox("Numeric Gadget")
        numeric_layout = QHBoxLayout(numeric_group)
        numeric_layout.addWidget(QLabel("Number:"))
        spin_box = QSpinBox()
        spin_box.setRange(0, 100)
        spin_box.setValue(50)
        numeric_layout.addWidget(spin_box)
        layout.addWidget(numeric_group)
        
        double_numeric_group = QGroupBox("Double Numeric Gadget")
        double_numeric_layout = QHBoxLayout(double_numeric_group)
        double_numeric_layout.addWidget(QLabel("Decimal:"))
        double_spin = QDoubleSpinBox()
        double_spin.setRange(0.0, 100.0)
        double_spin.setValue(25.5)
        double_spin.setSingleStep(0.1)
        double_numeric_layout.addWidget(double_spin)
        layout.addWidget(double_numeric_group)
        
        color_group = QGroupBox("Color Field Gadget")
        color_layout = QHBoxLayout(color_group)
        color_button = QPushButton("Choose Color")
        color_button.clicked.connect(lambda: self.choose_color(color_button))
        color_layout.addWidget(color_button)
        color_layout.addWidget(QLabel("Click to select color"))
        layout.addWidget(color_group)
        
        list_group = QGroupBox("List Gadget")
        list_layout = QVBoxLayout(list_group)
        list_widget = QListWidget()
        list_widget.addItems(["Item 1", "Item 2", "Item 3", "Item 4", "Item 5"])
        list_layout.addWidget(list_widget)
        layout.addWidget(list_group)
        
        checkbox_group = QGroupBox("Checkbox Gadget")
        checkbox_layout = QVBoxLayout(checkbox_group)
        checkbox_layout.addWidget(QCheckBox("Option 1"))
        checkbox_layout.addWidget(QCheckBox("Option 2"))
        checkbox_layout.addWidget(QCheckBox("Option 3"))
        layout.addWidget(checkbox_group)
        
        radio_group = QGroupBox("Radio Button Gadget")
        radio_layout = QVBoxLayout(radio_group)
        radio_layout.addWidget(QRadioButton("Choice A"))
        radio_layout.addWidget(QRadioButton("Choice B"))
        radio_layout.addWidget(QRadioButton("Choice C"))
        layout.addWidget(radio_group)
        
        cycle_group = QGroupBox("Cycle Gadget")
        cycle_layout = QHBoxLayout(cycle_group)
        cycle_combo = QComboBox()
        cycle_combo.addItems(["Option 1", "Option 2", "Option 3", "Option 4"])
        cycle_layout.addWidget(QLabel("Select:"))
        cycle_layout.addWidget(cycle_combo)
        layout.addWidget(cycle_group)
        
        self.tab_widget.addTab(tab, "Basic Gadgets")
    
    def create_advanced_gadgets_tab(self):
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        # Custom MUI gadgets
        slider_group = QGroupBox("Scale/Slider Gadget")
        slider_layout = QVBoxLayout(slider_group)
        
        # Horizontal slider
        h_slider = MUISlider(minimum=0, maximum=100, value=50, orientation=Qt.Orientation.Horizontal)
        h_slider.valueChanged.connect(lambda v: self.statusBar().showMessage(f"Horizontal Slider: {v}"))
        slider_layout.addWidget(QLabel("Horizontal Slider:"))
        slider_layout.addWidget(h_slider)
        
        # Vertical slider
        v_slider_layout = QHBoxLayout()
        v_slider = MUISlider(minimum=0, maximum=100, value=75, orientation=Qt.Orientation.Vertical)
        v_slider.valueChanged.connect(lambda v: self.statusBar().showMessage(f"Vertical Slider: {v}"))
        v_slider_layout.addWidget(v_slider)
        v_slider_layout.addWidget(QLabel("Vertical Slider"))
        slider_layout.addLayout(v_slider_layout)
        
        layout.addWidget(slider_group)
        
        knob_group = QGroupBox("Knob Gadget")
        knob_layout = QHBoxLayout(knob_group)
        knob = MUIKnob(minimum=0.0, maximum=100.0, value=50.0)
        knob.valueChanged.connect(lambda v: self.statusBar().showMessage(f"Knob Value: {v:.1f}"))
        knob_layout.addWidget(knob)
        knob_layout.addWidget(QLabel("Adjust the knob"))
        layout.addWidget(knob_group)
        
        gauge_group = QGroupBox("Gauge Gadget")
        gauge_layout = QVBoxLayout(gauge_group)
        progress = QProgressBar()
        progress.setRange(0, 100)
        progress.setValue(65)
        gauge_layout.addWidget(QLabel("Progress Gauge:"))
        gauge_layout.addWidget(progress)
        layout.addWidget(gauge_group)
        
        level_meter_group = QGroupBox("Level Meter Gadget")
        level_meter_layout = QHBoxLayout(level_meter_group)
        level_meter = MUILevelMeter(orientation=Qt.Orientation.Vertical, num_bars=8)
        level_meter.setAllLevels([0.2, 0.4, 0.6, 0.8, 0.9, 0.7, 0.5, 0.3])
        level_meter_layout.addWidget(level_meter)
        
        # Timer to animate level meter
        timer = QTimer()
        timer.timeout.connect(lambda: self.animate_level_meter(level_meter))
        timer.start(200)
        
        level_meter_layout.addWidget(QLabel("Audio Level Meter"))
        layout.addWidget(level_meter_group)
        
        palette_group = QGroupBox("Palette Gadget")
        palette_layout = QHBoxLayout(palette_group)
        palette_widget = MUIPaletteWidget()
        palette_layout.addWidget(palette_widget)
        palette_layout.addWidget(QLabel("Click to select color"))
        layout.addWidget(palette_group)
        
        popstring_group = QGroupBox("Popstring Gadget")
        popstring_layout = QHBoxLayout(popstring_group)
        popstring_combo = QComboBox()
        popstring_combo.addItems(["Value A", "Value B", "Value C", "Value D"])
        popstring_layout.addWidget(QLabel("Popstring:"))
        popstring_layout.addWidget(popstring_combo)
        layout.addWidget(popstring_group)
        
        self.tab_widget.addTab(tab, "Advanced Gadgets")
    
    def animate_level_meter(self, meter):
        import random
        levels = [random.random() for _ in range(meter.num_bars)]
        meter.setAllLevels(levels)
    
    def create_visual_effects_tab(self):
        tab = QWidget()
        layout = QGridLayout(tab)
        
        # Buttons with different visual effects
        button1 = QPushButton("Normal Button")
        button1.setStyleSheet("""
            QPushButton {
                background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                                stop: 0 #aaa, stop: 1 #888);
                border: 2px solid #555;
                border-radius: 5px;
                padding: 5px;
            }
            QPushButton:pressed {
                background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                                stop: 0 #888, stop: 1 #aaa);
            }
        """)
        layout.addWidget(button1, 0, 0)
        
        button2 = QPushButton("Raised Button")
        button2.setStyleSheet("""
            QPushButton {
                background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                                stop: 0 #ccc, stop: 1 #aaa);
                border: 2px outset #888;
                border-radius: 5px;
                padding: 5px;
            }
            QPushButton:pressed {
                border: 2px inset #888;
            }
        """)
        layout.addWidget(button2, 0, 1)
        
        button3 = QPushButton("Sunken Button")
        button3.setStyleSheet("""
            QPushButton {
                background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                                stop: 0 #888, stop: 1 #666);
                border: 2px inset #444;
                border-radius: 5px;
                padding: 5px;
            }
            QPushButton:pressed {
                border: 2px outset #444;
            }
        """)
        layout.addWidget(button3, 1, 0)
        
        button4 = QPushButton("3D Button")
        button4.setStyleSheet("""
            QPushButton {
                background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                                stop: 0 #eee, stop: 1 #ccc);
                border: 3px solid;
                border-top-color: #fff;
                border-left-color: #fff;
                border-right-color: #888;
                border-bottom-color: #888;
                border-radius: 5px;
                padding: 5px;
            }
            QPushButton:pressed {
                border-top-color: #888;
                border-left-color: #888;
                border-right-color: #fff;
                border-bottom-color: #fff;
            }
        """)
        layout.addWidget(button4, 1, 1)
        
        # Panel examples with different styles
        panel1 = QFrame()
        panel1.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Raised)
        panel1.setStyleSheet("""
            QFrame {
                background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                                stop: 0 #ddd, stop: 1 #bbb);
                border: 2px solid #888;
                border-radius: 5px;
            }
        """)
        panel1.setMinimumHeight(100)
        layout.addWidget(panel1, 2, 0, 1, 2)
        
        label = QLabel("Various Visual Effects Panel")
        label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        panel1.layout().addWidget(label) if panel1.layout() else None
        
        self.tab_widget.addTab(tab, "Visual Effects")
    
    def create_image_backgrounds_tab(self):
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        # Background type selector
        bg_selector_layout = QHBoxLayout()
        bg_selector_layout.addWidget(QLabel("Background Type:"))
        bg_combo = QComboBox()
        bg_combo.addItems([
            "panel_bg", "bg_primary", "bg_secondary", 
            "shaded", "stretched", "tiled"
        ])
        bg_selector_layout.addWidget(bg_combo)
        layout.addLayout(bg_selector_layout)
        
        # Image display area
        image_frame = QFrame()
        image_frame.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Sunken)
        image_frame.setMinimumHeight(400)
        image_layout = QVBoxLayout(image_frame)
        
        # Placeholder for image
        image_label = QLabel("Image Display Area")
        image_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        image_label.setStyleSheet("""
            QLabel {
                background-color: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 1,
                                                stop: 0 #aaa, stop: 1 #eee);
                border: 2px dashed #888;
                padding: 20px;
            }
        """)
        image_layout.addWidget(image_label)
        
        layout.addWidget(image_frame)
        
        # Controls for image manipulation
        controls_layout = QHBoxLayout()
        
        load_btn = QPushButton("Load Image")
        load_btn.clicked.connect(lambda: self.load_image(image_label))
        controls_layout.addWidget(load_btn)
        
        apply_btn = QPushButton("Apply Background")
        apply_btn.clicked.connect(lambda: self.apply_background(image_label, bg_combo.currentText()))
        controls_layout.addWidget(apply_btn)
        
        layout.addLayout(controls_layout)
        
        self.tab_widget.addTab(tab, "Image Backgrounds")
    
    def create_transparency_tab(self):
        tab = QWidget()
        layout = QVBoxLayout(tab)
        
        # Transparency controls
        transparency_group = QGroupBox("Transparency Controls")
        transparency_layout = QFormLayout(transparency_group)
        
        # Titlebar transparency
        titlebar_slider = QSlider(Qt.Orientation.Horizontal)
        titlebar_slider.setRange(0, 100)
        titlebar_slider.setValue(100)
        titlebar_slider.valueChanged.connect(
            lambda v: self.setWindowOpacity(v / 100.0)
        )
        transparency_layout.addRow("Titlebar Opacity:", titlebar_slider)
        
        # Panel transparency
        panel_slider = QSlider(Qt.Orientation.Horizontal)
        panel_slider.setRange(0, 100)
        panel_slider.setValue(100)
        panel_slider.valueChanged.connect(
            lambda v: self.tab_widget.setTransparency(v / 100.0)
        )
        transparency_layout.addRow("Panel Opacity:", panel_slider)
        
        layout.addWidget(transparency_group)
        
        # Shadow controls
        shadow_group = QGroupBox("Shadow Effects")
        shadow_layout = QFormLayout(shadow_group)
        
        shadow_slider = QSlider(Qt.Orientation.Horizontal)
        shadow_slider.setRange(0, 10)
        shadow_slider.setValue(0)
        shadow_slider.valueChanged.connect(
            lambda v: self.tab_widget.setShadowDepth(v)
        )
        shadow_layout.addRow("Shadow Depth:", shadow_slider)
        
        layout.addWidget(shadow_group)
        
        # UI Management Components
        components_group = QGroupBox("UI Management Components")
        components_layout = QVBoxLayout(components_group)
        
        # Group example
        group_box = QGroupBox("Example Group")
        group_layout = QVBoxLayout(group_box)
        group_layout.addWidget(QLabel("This is content inside a group"))
        group_layout.addWidget(QPushButton("Button in Group"))
        components_layout.addWidget(group_box)
        
        # Scrollbar example
        scrollbar_layout = QHBoxLayout()
        scrollbar_layout.addWidget(QLabel("Scrollbar:"))
        scrollbar = QSlider(Qt.Orientation.Horizontal)
        scrollbar.setRange(0, 100)
        scrollbar_layout.addWidget(scrollbar)
        components_layout.addLayout(scrollbar_layout)
        
        # Listview example
        listview = QListView()
        listview.setMaximumHeight(100)
        components_layout.addWidget(QLabel("Listview:"))
        components_layout.addWidget(listview)
        
        # Register example (using tab widget as register metaphor)
        register_tab = QTabWidget()
        register_tab.addTab(QLabel("Page 1 Content"), "Page 1")
        register_tab.addTab(QLabel("Page 2 Content"), "Page 2")
        components_layout.addWidget(QLabel("Register (Tabbed Interface):"))
        components_layout.addWidget(register_tab)
        
        layout.addWidget(components_group)
        
        self.tab_widget.addTab(tab, "Transparency & Components")
    
    def choose_color(self, button):
        color = QColorDialog.getColor()
        if color.isValid():
            button.setStyleSheet(f"background-color: {color.name()};")
            self.statusBar().showMessage(f"Selected color: {color.name()}")
    
    def load_image(self, label):
        # In a real implementation, this would open a file dialog
        # For demo purposes, we'll just change the label text
        label.setText("Image Loaded Successfully!")
        self.statusBar().showMessage("Image loaded")
    
    def apply_background(self, label, bg_type):
        # Apply different background styles based on type
        styles = {
            "panel_bg": """
                QLabel {
                    background-color: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 1,
                                                    stop: 0 #888, stop: 1 #666);
                    border: 2px solid #444;
                    padding: 20px;
                }
            """,
            "bg_primary": """
                QLabel {
                    background-color: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 1,
                                                    stop: 0 #aaa, stop: 1 #888);
                    border: 2px solid #666;
                    padding: 20px;
                }
            """,
            "bg_secondary": """
                QLabel {
                    background-color: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 1,
                                                    stop: 0 #666, stop: 1 #444);
                    border: 2px solid #222;
                    padding: 20px;
                }
            """,
            "shaded": """
                QLabel {
                    background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,
                                                    stop: 0 #ccc, stop: 0.5 #aaa, stop: 1 #888);
                    border: 2px solid #666;
                    padding: 20px;
                }
            """,
            "stretched": """
                QLabel {
                    background-color: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 1,
                                                    stop: 0 #555, stop: 1 #999);
                    border: 2px solid #333;
                    padding: 20px;
                }
            """,
            "tiled": """
                QLabel {
                    background-color: qlineargradient(x1: 0, y1: 0, x2: 1, y2: 1,
                                                    stop: 0 #777, stop: 1 #bbb);
                    border: 2px solid #555;
                    padding: 20px;
                }
            """
        }
        
        if bg_type in styles:
            label.setStyleSheet(styles[bg_type])
            self.statusBar().showMessage(f"Applied {bg_type} background style")
    
    def toggle_transparency(self):
        current_alpha = self.tab_widget.transparency
        new_alpha = 0.5 if current_alpha == 1.0 else 1.0
        self.tab_widget.setTransparency(new_alpha)
        self.statusBar().showMessage(f"Transparency set to {new_alpha}")
    
    def toggle_shadows(self):
        current_shadow = self.tab_widget.shadow_depth
        new_shadow = 5 if current_shadow == 0 else 0
        self.tab_widget.setShadowDepth(new_shadow)
        self.statusBar().showMessage(f"Shadow depth set to {new_shadow}")
    
    def show_about(self):
        from PyQt6.QtWidgets import QMessageBox
        QMessageBox.information(
            self, 
            "About Amiga MUI Demo", 
            "Amiga MUI Demo Application\n\n"
            "Features implemented:\n"
            "- Multiple tabs with various gadget types\n"
            "- String, Gauge, Scale, Colorfield, List, Numeric, Knob, Levelmeter, Slider, Radio, Cycle, Palette, Popstring gadgets\n"
            "- Transparency controls for titlebar and panels\n"
            "- Shadow effects for buttons and panels\n"
            "- UI Management Components (Group, Scrollbar, Listview, Register, etc.)\n"
            "- Image background support (panel_bg, bg_primary, bg_secondary, shaded, stretched, tiled)\n\n"
            "This demo showcases Amiga MUI styling concepts implemented in PyQt."
        )


def main():
    app = QApplication(sys.argv)
    
    # Set application properties
    app.setApplicationName("Amiga MUI Demo")
    app.setApplicationVersion("1.0")
    
    # Create and show the main window
    window = MUIWindow()
    window.show()
    
    sys.exit(app.exec())


if __name__ == "__main__":
    main()