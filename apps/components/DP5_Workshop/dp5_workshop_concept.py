#!/usr/bin/env python3
import sys
from PyQt5.QtWidgets import (
    QApplication, QMainWindow, QWidget, QHBoxLayout, QVBoxLayout,
    QPushButton, QLabel, QColorDialog, QGridLayout
)
from PyQt5.QtGui import QColor, QPainter
from PyQt5.QtCore import Qt

# =========================
# THEME SYSTEM (from your editor idea)
# =========================
THEMES = {
    "blue_panels_dark": {
        "bg": "#2b2b2b",
        "panel": "#3c3f41",
        "border": "#555",
        "text": "#dddddd"
    }
}

# User palette presets
USER_PALETTES = {
    "retro": [
        "#000000", "#FFFFFF", "#FF0000", "#00FF00",
        "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF"
    ]
}

# =========================
# INDEXED IMAGE ENGINE
# =========================
class IndexedImage:
    def __init__(self, w, h, palette):
        self.w = w
        self.h = h
        self.palette = palette
        self.pixels = [0] * (w * h)

    def set_pixel(self, x, y, c):
        if 0 <= x < self.w and 0 <= y < self.h:
            self.pixels[y * self.w + x] = c

    def get_pixel(self, x, y):
        return self.pixels[y * self.w + x]

# =========================
# CANVAS
# =========================
class Canvas(QWidget):
    def __init__(self):
        super().__init__()
        self.setMinimumSize(320, 200)

        self.palette = [QColor(c) for c in USER_PALETTES["retro"]]
        self.palette_map = {c.name(): i for i, c in enumerate(self.palette)}

        self.image = IndexedImage(64, 64, self.palette)
        self.current_color = 1

    def mousePressEvent(self, e):
        x = int(e.x() / 5)
        y = int(e.y() / 5)
        self.image.set_pixel(x, y, self.current_color)
        self.update()

    def paintEvent(self, e):
        p = QPainter(self)
        for y in range(self.image.h):
            for x in range(self.image.w):
                idx = self.image.get_pixel(x, y)
                p.fillRect(x*5, y*5, 5, 5, self.palette[idx])

# =========================
# PALETTE WIDGET
# =========================
class PaletteWidget(QWidget):
    def __init__(self, colors, click_callback=None):
        super().__init__()
        self.colors = colors
        self.click_callback = click_callback

        layout = QGridLayout()
        layout.setSpacing(2)
        self.setLayout(layout)

        self.buttons = []
        self.build()

    def build(self):
        for i, col in enumerate(self.colors):
            btn = QPushButton()
            btn.setFixedSize(18, 18)
            btn.setStyleSheet(f"background:{col.name()}; border:1px solid #555;")

            def make_cb(c=col):
                return lambda: self.click_callback(c) if self.click_callback else None

            btn.clicked.connect(make_cb())
            self.layout().addWidget(btn, i // 8, i % 8)
            self.buttons.append(btn)

    def update_colors(self, colors):
        self.colors = colors
        for i, col in enumerate(colors):
            self.buttons[i].setStyleSheet(f"background:{col.name()}; border:1px solid #555;")

# =========================
# MAIN WINDOW (DP5 LAYOUT)
# =========================
class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("DP5 Workshop")

        root = QHBoxLayout()

        self.canvas = Canvas()

        left = self.build_left_toolbar()
        right = self.build_right_panel()

        root.addWidget(left)
        root.addWidget(self.canvas, 1)
        root.addWidget(right)

        container = QWidget()
        container.setLayout(root)
        self.setCentralWidget(container)

    # =========================
    # LEFT TOOLBAR (DPaint grid)
    # =========================
    def build_left_toolbar(self):
        widget = QWidget()
        grid = QGridLayout()
        grid.setSpacing(2)

        tools = ["P", "L", "F", "S"]
        for i, t in enumerate(tools):
            btn = QPushButton(t)
            btn.setFixedSize(32, 32)
            grid.addWidget(btn, i // 2, i % 2)

        widget.setLayout(grid)
        return widget

    # =========================
    # RIGHT PANEL (DUAL PALETTE)
    # =========================
    def build_right_panel(self):
        widget = QWidget()
        layout = QVBoxLayout()
        layout.setSpacing(6)

        # Actions
        actions = QHBoxLayout()
        for name in ["Copy", "Cut", "Paste"]:
            actions.addWidget(QPushButton(name))
        layout.addLayout(actions)

        # Image palette
        layout.addWidget(QLabel("Image Palette"))
        self.image_palette = PaletteWidget(
            self.canvas.palette,
            self.set_current_color
        )
        layout.addWidget(self.image_palette)

        # User palette
        layout.addWidget(QLabel("User Palette"))
        user_colors = [QColor(c) for c in USER_PALETTES["retro"]]
        self.user_palette = PaletteWidget(
            user_colors,
            self.inject_color
        )
        layout.addWidget(self.user_palette)

        widget.setLayout(layout)
        return widget

    # =========================
    # PALETTE LOGIC
    # =========================
    def set_current_color(self, color):
        idx = self.canvas.palette_map.get(color.name(), 0)
        self.canvas.current_color = idx

    def inject_color(self, color):
        if color.name() not in self.canvas.palette_map:
            self.canvas.palette.append(color)
            self.canvas.palette_map[color.name()] = len(self.canvas.palette)-1
            self.image_palette.update_colors(self.canvas.palette)

# =========================
# THEME APPLY
# =========================
def apply_theme(app, theme):
    app.setStyleSheet(f"""
        QWidget {{
            background: {theme['bg']};
            color: {theme['text']};
        }}
        QPushButton {{
            background: {theme['panel']};
            border: 1px solid {theme['border']};
        }}
    """)

# =========================
# ENTRY POINT
# =========================
if __name__ == "__main__":
    app = QApplication(sys.argv)

    apply_theme(app, THEMES["blue_panels_dark"])

    win = MainWindow()
    win.show()

    sys.exit(app.exec())
