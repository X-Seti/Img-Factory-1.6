#this belongs in apps/methods/corner_handles.py - Version: 1
# X-Seti - October 20 2026 - IMG Factory 1.6 - Window Corner Handles

"""
Window Corner Handles - COL Workshop style resize triangles for every workshop.
Call paint_corner_handles(self) from the window's paintEvent after super().
"""

##Methods list -
# paint_corner_handles

from PyQt6.QtCore import Qt
from PyQt6.QtGui import QBrush, QColor, QPainter, QPainterPath


def paint_corner_handles(widget): #vers 1
    """Accent triangles in the four corners, under the content; hover brighter."""
    if not widget.isWindow():                             # docked: no corner handles
        return
    painter = QPainter(widget)
    if not painter.isActive():
        return
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)
    w, h = widget.width(), widget.height()
    size = getattr(widget, 'corner_size', 20)
    corners = {
        'top-left': [(0, 0), (size, 0), (0, size)],
        'top-right': [(w, 0), (w - size, 0), (w, size)],
        'bottom-left': [(0, h), (size, h), (0, h - size)],
        'bottom-right': [(w, h), (w - size, h), (w, h - size)],
    }
    app_settings = getattr(widget, 'app_settings', None)
    if app_settings:
        accent = QColor(app_settings.get_theme_colors().get('accent_primary', '#1976d2'))
        accent.setAlpha(180)
    else:
        accent = QColor(100, 150, 255, 180)
    hover = QColor(accent)
    hover.setAlpha(255)
    hover_corner = getattr(widget, 'hover_corner', None)
    painter.setPen(Qt.PenStyle.NoPen)
    for name, pts in corners.items():
        path = QPainterPath()
        path.moveTo(*pts[0])
        path.lineTo(*pts[1])
        path.lineTo(*pts[2])
        path.closeSubpath()
        painter.setBrush(QBrush(hover if hover_corner == name else accent))
        painter.drawPath(path)
    painter.end()
