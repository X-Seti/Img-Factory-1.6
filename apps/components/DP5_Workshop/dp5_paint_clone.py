# DP5_Workshop.py
# Deluxe Paint V inspired editor (expanded)
# Added:
# - Full tool system (pencil, line, fill)
# - Brush system (import/export multiple formats)
# - Palette editor
# - Improved DPaint-style UI structure (theme-ready hooks)

import sys
from PyQt5.QtWidgets import *
from PyQt5.QtGui import *
from PyQt5.QtCore import *
from dp5_paint_editor import theme

# -----------------------------
# Indexed Image Core
# -----------------------------

class IndexedImage:
    def __init__(self, w, h, palette):
        self.w, self.h = w, h
        self.palette = palette
        self.pixels = [0]*(w*h)

    def set_pixel(self, x, y, i):
        if 0 <= x < self.w and 0 <= y < self.h:
            self.pixels[y*self.w+x] = i

    def get_pixel(self, x, y):
        return self.pixels[y*self.w+x]

    def to_qimage(self):
        img = QImage(self.w, self.h, QImage.Format_RGB32)
        for y in range(self.h):
            for x in range(self.w):
                img.setPixelColor(x,y,self.palette[self.get_pixel(x,y)])
        return img

# -----------------------------
# Tool System
# -----------------------------

class Tool:
    def __init__(self, canvas):
        self.canvas = canvas

    def press(self, p): pass
    def move(self, p): pass
    def release(self, p): pass

class PencilTool(Tool):
    def press(self, p): self.draw(p)
    def move(self, p): self.draw(p)
    def draw(self, p):
        self.canvas.image.set_pixel(p.x(), p.y(), self.canvas.color)
        self.canvas.update()

class LineTool(Tool):
    def press(self, p): self.start = p
    def release(self, p):
        painter = QPainter(self.canvas.buffer)
        painter.setPen(self.canvas.palette[self.canvas.color])
        painter.drawLine(self.start, p)
        painter.end()

class FillTool(Tool):
    def press(self, p):
        target = self.canvas.image.get_pixel(p.x(), p.y())
        replacement = self.canvas.color
        if target == replacement: return
        stack = [(p.x(), p.y())]
        while stack:
            x,y = stack.pop()
            if self.canvas.image.get_pixel(x,y)==target:
                self.canvas.image.set_pixel(x,y,replacement)
                stack += [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
        self.canvas.update()

# -----------------------------
# Brush System
# -----------------------------

class Brush:
    def __init__(self, image=None):
        self.image = image

    def load(self, path):
        img = QImage(path)
        if img.isNull(): return False
        self.image = img
        return True

    def save(self, path):
        if self.image:
            self.image.save(path)

# -----------------------------
# Canvas
# -----------------------------

class Canvas(QWidget):
    def __init__(self):
        super().__init__()
        self.setFixedSize(512,512)

        self.palette = [QColor(i,i,i) for i in range(256)]
        self.palette_map = {c.name():i for i,c in enumerate(self.palette)}

        self.image = IndexedImage(128,128,self.palette)
        self.scale = 4
        self.color = 1

        self.tools = {
            "pencil": PencilTool(self),
            "line": LineTool(self),
            "fill": FillTool(self)
        }
        self.current_tool = self.tools["pencil"]

        self.brush = Brush()

    def paintEvent(self,e):
        p = QPainter(self)
        img = self.image.to_qimage().scaled(self.image.w*self.scale,self.image.h*self.scale)
        p.drawImage(0,0,img)

        if self.brush.image:
            p.drawImage(0,0,self.brush.image)

    def mousePressEvent(self,e):
        p = e.pos()//self.scale
        self.current_tool.press(p)

    def mouseMoveEvent(self,e):
        p = e.pos()//self.scale
        self.current_tool.move(p)

    def mouseReleaseEvent(self,e):
        p = e.pos()//self.scale
        self.current_tool.release(p)

# -----------------------------
# Palette Editor
# -----------------------------

class PaletteEditor(QDialog):
    def __init__(self, canvas):
        super().__init__()
        self.canvas = canvas
        self.setWindowTitle("Palette Editor")
        layout = QGridLayout()

        for i,c in enumerate(canvas.palette[:64]):
            btn = QPushButton()
            btn.setFixedSize(20,20)
            btn.setStyleSheet(f"background:{c.name()}")
            btn.clicked.connect(lambda _,i=i:self.edit(i))
            layout.addWidget(btn,i//8,i%8)

        self.setLayout(layout)

    def edit(self,i):
        color = QColorDialog.getColor()
        if color.isValid():
            self.canvas.palette[i] = color
            self.canvas.palette_map[color.name()] = i
            self.canvas.update()

# -----------------------------
# Main Window (DPaint-like)
# -----------------------------

class Main(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("DP5 Workshop")

        self.canvas = Canvas()

        right = QVBoxLayout()

        # Tool buttons
        for name in ["pencil","line","fill"]:
            b = QPushButton(name)
            b.clicked.connect(lambda _,n=name:self.set_tool(n))
            right.addWidget(b)

        # Brush IO
        load_btn = QPushButton("Load Brush")
        save_btn = QPushButton("Save Brush")
        load_btn.clicked.connect(self.load_brush)
        save_btn.clicked.connect(self.save_brush)
        right.addWidget(load_btn)
        right.addWidget(save_btn)

        # Palette
        pal_btn = QPushButton("Palette Editor")
        pal_btn.clicked.connect(self.open_palette)
        right.addWidget(pal_btn)

        layout = QHBoxLayout()
        layout.addWidget(self.canvas)
        layout.addLayout(right)

        w = QWidget()
        w.setLayout(layout)
        self.setCentralWidget(w)

    def set_tool(self,n):
        self.canvas.current_tool = self.canvas.tools[n]

    def load_brush(self):
        path,_ = QFileDialog.getOpenFileName(self,"Load Brush","","Images (*.png *.bmp *.jpg *.ico *.dds *.iff)")
        if path:
            self.canvas.brush.load(path)
            self.canvas.update()

    def save_brush(self):
        path,_ = QFileDialog.getSaveFileName(self,"Save Brush","","Images (*.png *.bmp *.jpg *.ico *.dds)")
        if path:
            self.canvas.brush.save(path)

    def open_palette(self):
        PaletteEditor(self.canvas).exec_()

# -----------------------------
# Run
# -----------------------------

if __name__=='__main__':
    app = QApplication(sys.argv)
    m = Main()
    m.show()
    sys.exit(app.exec_())
