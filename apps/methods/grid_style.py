#this belongs in apps/methods/grid_style.py - Version: 1
# X-Seti - November 09 2026 - IMG Factory 1.6 - Viewport grid styles

"""
Viewport grid styles shared by Ped and Vehicle Workshop: presets
(lines, holodeck, checker, honeycomb, texture, none), a custom grid
(pattern, line / fill colour, thickness, cell size) and its Settings tab.
"""

##Methods list -
# _cell
# apply_grid_custom
# apply_grid_preset
# grid_settings_page

GRID_PATTERNS = {'squares': ('lines', "Squares"), 'hex': ('honeycomb', "Hex"),     # key: (viewport type, label)
                 'solid': ('squares', "Solid"), 'checker': ('checker', "Checkerboard")}
GRID_PRESETS = {'lines': ('lines', (40, 40, 60), (76, 76, 102), "Lines"),
                'holodeck': ('lines', (0, 0, 0), (255, 200, 40), "Holodeck"),
                'checker': ('checker', (60, 60, 72), (118, 118, 132), "Checker"),
                'honeycomb': ('honeycomb', (40, 40, 60), (76, 76, 102), "Honeycomb"),
                'texture': ('squares', (40, 40, 60), (76, 76, 102), "Texture..."),
                'none': ('none', (40, 40, 60), (76, 76, 102), "None")}
GRID_DEFAULT = {'pattern': 'squares', 'fill': [40, 40, 60], 'line': [76, 76, 102], 'width': 1, 'cell': 0}


def _cell(vp, cell: int) -> None: #vers 1
    """Cell size in metres; 0 = follows the zoom."""
    if cell > 0:
        vp.set_grid_fixed_step(cell)
        vp.set_grid_scale_mode('fixed')
    else:
        vp.set_grid_scale_mode('zoom_relative')


def apply_grid_preset(vp, kind: str, path: str = '', cell: int = 0) -> None: #vers 1
    """Preset grid style onto a DFF viewport (texture: tiles the image at path)."""
    gtype, fill, line, _label = GRID_PRESETS.get(kind, GRID_PRESETS['lines'])
    vp.set_grid_colors(fill, line)
    vp.set_squares_fill('texture' if kind == 'texture' else 'color', path=path or None, tile_size=2.0)
    vp.set_squares_fill_alpha(0.12)
    vp.set_grid_show_lines(True)
    vp.set_grid_line_size(4)
    vp.set_grid_type(gtype)
    _cell(vp, cell)


def apply_grid_custom(vp, cfg: dict) -> None: #vers 1
    """Custom grid: pattern, colours, thickness, cell size."""
    pat = cfg.get('pattern', 'squares')
    vp.set_grid_colors(tuple(cfg['fill']), tuple(cfg['line']))
    vp.set_squares_fill('color')
    vp.set_grid_line_size(int(cfg.get('width', 1)))
    vp.set_squares_fill_alpha(0.9 if pat == 'solid' else 0.12)
    vp.set_grid_show_lines(pat != 'solid')
    vp.set_grid_type(GRID_PATTERNS.get(pat, ('lines', ''))[0])
    _cell(vp, int(cfg.get('cell', 0)))


def grid_settings_page(parent, cfg: dict, on_apply): #vers 1
    """Settings 'Grid' tab: pattern, line / fill colour, thickness, cell size; on_apply(cfg) live."""
    from PyQt6.QtGui import QColor
    from PyQt6.QtWidgets import QColorDialog, QComboBox, QFormLayout, QPushButton, QSpinBox, QWidget
    page = QWidget()
    form = QFormLayout(page)
    combo = QComboBox()
    for key, (_t, label) in GRID_PATTERNS.items():
        combo.addItem(label, key)
    combo.setCurrentIndex(max(0, combo.findData(cfg['pattern'])))
    width = QSpinBox()
    width.setRange(1, 10)
    width.setSuffix(" px")
    width.setValue(int(cfg['width']))
    cell = QSpinBox()
    cell.setRange(0, 100)
    cell.setSuffix(" m")
    cell.setSpecialValueText("Auto (zoom)")
    cell.setValue(int(cfg.get('cell', 0)))

    def apply():
        cfg['pattern'], cfg['width'], cfg['cell'] = combo.currentData(), width.value(), cell.value()
        on_apply(cfg)

    def colour_btn(key, title):
        btn = QPushButton()
        btn.setToolTip(f"Grid {title.lower()} colour")

        def paint():
            r, g, b = cfg[key]
            btn.setText(f"#{r:02X}{g:02X}{b:02X}")
            btn.setStyleSheet(f"background: rgb({r},{g},{b}); color: {'#000' if r + g + b > 380 else '#FFF'};")

        def pick():
            c = QColorDialog.getColor(QColor(*cfg[key]), parent, f"Grid {title}")
            if c.isValid():
                cfg[key] = [c.red(), c.green(), c.blue()]
                paint()
                apply()
        paint()
        btn.clicked.connect(pick)
        return btn

    combo.currentIndexChanged.connect(apply)
    width.valueChanged.connect(apply)
    cell.valueChanged.connect(apply)
    form.addRow("Pattern:", combo)
    form.addRow("Line colour:", colour_btn('line', "Line"))
    form.addRow("Fill colour:", colour_btn('fill', "Fill"))
    form.addRow("Thickness:", width)
    form.addRow("Cell size:", cell)
    return page
