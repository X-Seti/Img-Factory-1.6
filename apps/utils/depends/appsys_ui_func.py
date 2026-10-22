#this belongs in apps/utils/depends/appsys_ui_func.py - Version: 5
# X-Seti - October 20 2026 - IMG Factory 1.6 - Settings Dialog UI

"""
Settings dialog tab and widget builders.
Mixin for SettingsDialog in apps/utils/app_settings_system.py.
"""

##class AppsysUIMixin: -
# _create_advanced_gadgets_tab
# _create_background_tab
# _create_button_panel_editor
# _create_buttons_tab_v2
# _create_color_picker_tab
# _create_debug_tab
# _create_demo_tab
# _create_font_control_group
# _create_fonts_tab
# _create_gadgets_tab
# _create_sizes_tab
# _create_interface_tab
# _create_localisation_tab
# _create_panels_tab
# _create_shadows_tab
# _create_transparency_tab
# _create_ui
# _create_ui_management_tab
# _create_ui_management_tab_v2

import json
import os
import random
from pathlib import Path

from PyQt6.QtCore import (Qt, pyqtSignal, QTimer, QThread, pyqtSlot, QRect, QDateTime,
                          QByteArray, QPoint, QSize)
from PyQt6.QtGui import QFont, QColor, QPixmap, QPainter, QCursor, QIcon

from apps.methods.dock_panels import add_outer_dock, make_outer_mw
from PyQt6.QtWidgets import (
    QSizePolicy, QApplication, QMainWindow, QWidget, QVBoxLayout, QHBoxLayout,
    QGridLayout, QButtonGroup, QRadioButton, QLabel, QPushButton,
    QComboBox, QCheckBox, QSpinBox, QMenu, QSlider, QSplitter,
    QGroupBox, QTabWidget, QDialog, QMessageBox,
    QFileDialog, QColorDialog, QFontDialog, QTextEdit,
    QTableWidget, QTableWidgetItem, QHeaderView, QFontComboBox,
    QScrollArea, QFrame, QLineEdit, QListWidget
)


class AppsysUIMixin: #vers 1
    """Settings dialog tab and widget builders."""

    def _create_ui(self): #vers 10
        """Create the settings dialog UI — restructured tabs"""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(5, 5, 5, 5)   # corner handles show, COL style
        layout.setSpacing(0)

        self._original_theme = self.app_settings.current_settings.get("theme", "App_Factory")

        # Apply theme stylesheet immediately — FramelessWindowHint dialogs
        # don't inherit from QApplication so we must set it explicitly.
        try:
            _ss = self.app_settings.get_stylesheet()
            self.setStyleSheet(_ss)
            # Debug: confirm colour being applied
            _theme = self.app_settings.current_settings.get('theme','?')
            _tc = self.app_settings.get_theme_colors(_theme) or {}
            print(f"[SettingsDialog] theme={_theme} "
                  f"bg={_tc.get('bg_primary','?')} text={_tc.get('text_primary','?')} "
                  f"dialog_bg={_tc.get('dialog_bg','MISSING')} "
                  f"dialog_text={_tc.get('dialog_text','MISSING')}")
        except Exception as _sse:
            print(f"[SettingsDialog] stylesheet error: {_sse}")

        # Settings dialog always has its own titlebar — [Menu] [Settings] | title | [i] [—] [□] [✕]
        # This makes it draggable on both X11 and Wayland, and consistent regardless of ui_mode.
        try:
            self.setWindowFlags(Qt.WindowType.FramelessWindowHint |
                                Qt.WindowType.Dialog)
            self._create_dialog_titlebar()
            layout.addWidget(self.dialog_titlebar)
        except Exception as _e:
            import traceback
            print(f"Settings titlebar error: {_e}")
            traceback.print_exc()

        content_widget = QWidget()
        content_layout = QVBoxLayout(content_widget)

        self.tabs = QTabWidget()

        self.color_picker_tab = self._create_color_picker_tab()
        self.tabs.addTab(self.color_picker_tab, "Colors")

        self.fonts_tab = self._create_fonts_tab()
        self.tabs.addTab(self.fonts_tab, "Fonts")

        self.sizes_tab = self._create_sizes_tab()
        self.tabs.addTab(self.sizes_tab, "Sizes")

        self.buttons_tab = self._create_buttons_tab_v2()
        self.tabs.addTab(self.buttons_tab, "Buttons")

        self.panels_tab = self._create_panels_tab()
        self.tabs.addTab(self.panels_tab, "Panels")

        self.gadgets_tab = self._create_gadgets_tab()
        self.tabs.addTab(self.gadgets_tab, "Gadgets")

        self.shadows_tab = self._create_shadows_tab()
        self.tabs.addTab(self.shadows_tab, "Shadows")

        self.ui_management_tab = self._create_ui_management_tab_v2()
        self.tabs.addTab(self.ui_management_tab, "UI Management")

        self.interface_tab = self._create_interface_tab()
        self.tabs.addTab(self.interface_tab, "Interface")

        self.localisation_tab = self._create_localisation_tab()
        self.tabs.addTab(self.localisation_tab, "Localisation")

        self.debug_tab = self._create_debug_tab()
        self.tabs.addTab(self.debug_tab, "Debug")

        # Extra tabs contributed by any docked tool currently open as
        # a main-window tab (Map Workshop, and any future tool doing the same)
        self._extra_apply_callbacks = []
        try:
            for label, widget in self._collect_settings_contributions():
                self.tabs.addTab(widget, label)
        except Exception as e:
            print(f"[Settings] Could not collect docked-tool settings tabs: {e}")

        content_layout.addWidget(self.tabs)

        button_layout = QHBoxLayout()
        button_layout = QHBoxLayout()

        reset_btn = QPushButton("Reset to Defaults")
        reset_btn.clicked.connect(self._reset_to_defaults)
        button_layout.addWidget(reset_btn)

        button_layout.addStretch()

        apply_btn = QPushButton("Apply")
        apply_btn.clicked.connect(self._apply_settings)
        button_layout.addWidget(apply_btn)

        ok_btn = QPushButton("OK")
        ok_btn.clicked.connect(self._ok_clicked)
        button_layout.addWidget(ok_btn)

        cancel_btn = QPushButton("Cancel")
        cancel_btn.clicked.connect(self.reject)
        button_layout.addWidget(cancel_btn)

        content_layout.addLayout(button_layout)

        layout.addWidget(content_widget)


    def _create_color_picker_tab(self): #vers 10
        """Create color picker and theme editor tab - Final layout with logical flow"""
        from apps.utils.app_settings_system import ALPHA_COLOR_KEYS, ColorPickerWidget, PanelControlRow, PanelSectionHeader, ThemeColorEditor
        tab = QWidget()
        _outer = QVBoxLayout(tab)          # VBox so tab fills its container
        _outer.setContentsMargins(0, 0, 0, 0)
        _outer.setSpacing(0)

        # ========== LEFT PANEL ==========
        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_layout.setContentsMargins(4, 4, 4, 4)
        left_panel.setMinimumWidth(220)
        left_panel.setMaximumWidth(420)   # slightly wider cap

        # Screen Color Picker Group
        picker_group = QGroupBox("Color Picker")
        picker_group.setAutoFillBackground(True)
        picker_layout = QVBoxLayout(picker_group)

        self.color_picker = ColorPickerWidget()
        picker_layout.addWidget(self.color_picker)

        instructions = QLabel("""
    <b>How to use:</b><br>
    1. Click 'Pick Color from Screen'<br>
    2. Move mouse over any color<br>
    3. Left-click to select or "ESC" to cancel<br>
    <br>
    <i>Picked colors can be applied to theme elements</i>
        """)
        instructions.setWordWrap(True)
        instructions.setStyleSheet("padding: 1px; border-radius: 1px;")
        picker_layout.addWidget(instructions)

        left_layout.addWidget(picker_group)


    # - PALETTE COLORS GROUP
        palette_group = QGroupBox("Quick Colors")
        palette_group.setAutoFillBackground(True)
        palette_layout = QVBoxLayout(palette_group)

        # Top bar: Grid toggle + Retro menu
        top_bar = QHBoxLayout()
        palette_layout.addLayout(top_bar)

    # - GRID TOGGLE BUTTON
        self.palette_toggle_btn = QPushButton("Grid: 6x8")
        self.palette_toggle_btn.setCheckable(False)
        top_bar.addWidget(self.palette_toggle_btn)

    # - RETRO MENU BUTTON
        self.retro_btn = QPushButton("Retro ▼")
        top_bar.addWidget(self.retro_btn)

    # - PALETTE GRID AREA
        self.palette_grid = QGridLayout()
        palette_layout.addLayout(self.palette_grid)
        left_layout.addWidget(palette_group)

    # - RETRO PALETTE DEFINITIONS
        zx_spectrum = [
            "#000000", "#0000D7", "#D70000", "#D700D7",
            "#00D700", "#00D7D7", "#D7D700", "#D7D7D7",
            "#000000", "#0000FF", "#FF0000", "#FF00FF",
            "#00FF00", "#00FFFF", "#FFFF00", "#FFFFFF"
        ]

        commodore_64 = [
            "#000000", "#FFFFFF", "#813338", "#75CEC8",
            "#8E3C97", "#56AC4D", "#2E2C9B", "#EDF171",
            "#8E5029", "#553800", "#C46C71", "#4A4A4A",
            "#7B7B7B", "#A9FF9F", "#706DEB", "#B2B2B2"
        ]

        amstrad_cpc = [
            "#000000", "#000080", "#0000FF", "#800000", "#800080", "#8000FF",
            "#FF0000", "#FF0080", "#FF00FF", "#008000", "#008080", "#0080FF",
            "#808000", "#808080", "#8080FF", "#00FF00", "#00FF80", "#00FFFF",
            "#FF8000", "#FF8080", "#FF80FF", "#FFFF00", "#FFFF80", "#FFFFFF",
            "#008000", "#00C000", "#C0C000"
        ]

        amiga_default = [
            "#000000", "#111111", "#222222", "#333333", "#444444", "#555555", "#666666", "#777777",
            "#888888", "#999999", "#AAAAAA", "#BBBBBB", "#CCCCCC", "#DDDDDD", "#EEEEEE", "#FFFFFF",
            "#0000AA", "#AA0000", "#00AA00", "#AAAA00", "#00AAAA", "#AA00AA", "#AAAAAA", "#FF0000",
            "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", "#FF8800", "#FF0088", "#8888FF"
        ]

        atari_800 = [
            "#000000", "#404040", "#6C6C6C", "#909090", "#B0B0B0", "#C8C8C8", "#DCDCDC", "#ECECEC",
            "#444400", "#646410", "#848424", "#A0A034", "#B8B840", "#D0D050", "#E8E85C", "#FCFC68",
            "#702800", "#844414", "#985C28", "#AC783C", "#BC8C4C", "#CCA05C", "#DCB468", "#ECC878",
            "#841800", "#983418", "#AC502C", "#C06840", "#D07C50", "#E09060", "#F0A070", "#FFB480",
            "#880000", "#9C2020", "#B03C3C", "#C05858", "#D07070", "#E08888", "#F0A0A0", "#FFB8B8"
        ]

        amiga_aga = [
            "#A69F9E", "#CECECE", "#B9B9B9", "#949694", "#838583", "#777371", "#5C4B4A", "#685555",
            "#A8A2A1", "#898988", "#919291", "#8D8E8D", "#868886", "#818280", "#7C7B79", "#7E7C7B",
            "#8A8A89", "#B1B2B1", "#999594", "#EFEEEF", "#B5A9AA", "#FFFEFF", "#F1EDEE", "#CABFC0",
            "#B2A3A3", "#D4CBCC", "#FAF8F9", "#ECEBEC", "#D6D6D6", "#D2D3D2", "#C8C3C4", "#B8ACAC",
            "#877071", "#DED6D7", "#826A6B", "#482525", "#998585", "#F2EFF0", "#D1D2D1", "#9D9F9D",
            "#959695", "#7C7271", "#563939", "#3F1A1A", "#D8CFD0", "#6E5152", "#290000", "#2E0707",
            "#3E1F1E", "#492F2E", "#5D4242", "#725757", "#B1A2A2", "#F6F3F4", "#CACACA", "#767170",
            "#665A59", "#635655", "#6A605F", "#7A7776", "#8B8D8B", "#A1A2A1", "#AFB0AF", "#9A9C9A",
            "#848684", "#6F7E96", "#5876AC", "#5575AF", "#6079A4", "#76818F", "#828482", "#818281",
            "#808180", "#828382", "#8A8B8A", "#919391", "#8E8F8E", "#878987", "#868682", "#8D8881",
            "#918980", "#8A8782", "#988B86", "#AD9189", "#AF9289", "#A38E87", "#8D8884", "#898B89",
            "#898A89", "#957D6C", "#A87455", "#A97455", "#9E7962", "#8B8179", "#859079", "#8AA565",
            "#8CB05B", "#8BAD5D", "#879871", "#838582", "#968E6B", "#A99755", "#9D9263", "#8B897A",
            "#7E808F", "#7678A4", "#7274AF", "#7375AC", "#7C7E96", "#838484", "#88729A", "#8C61AF",
            "#896DA1", "#857F8B", "#769184", "#5FA785", "#55B086", "#59AC86", "#709784", "#3A1818",
            "#351212", "#5B4A49", "#75706F", "#A49F9E", "#D8D1D1", "#EDE8E9", "#FCFBFC", "#371514",
            "#472D2C", "#6D6463", "#969796", "#BBBBBB", "#5274B1", "#1C62E4", "#145FEC", "#2E68D3",
            "#657BA0", "#7E7E7E", "#7C7B7C", "#808080", "#939493", "#A4A5A4", "#A6A7A6", "#9D9E9D",
            "#8C8E8C", "#9C8C7E", "#A68F7C", "#A58F7C", "#948A80", "#B4938A", "#E8A291", "#ECA391",
            "#D09B8D", "#9C8C86", "#8D8F8D", "#AF714D", "#DC5C16", "#DD5C14", "#C36834", "#977C6B",
            "#889F6A", "#93D13B", "#98EB22", "#97E627", "#8DB457", "#848681", "#B19C4A", "#DDB114",
            "#C2A336", "#958E6D", "#787AA0", "#6365D3", "#5A5CEC", "#5C5EE5", "#7173B1", "#848287",
            "#8E58BA", "#9830EC", "#914BCA", "#877696", "#63A285", "#2CD588", "#14EB8A", "#1EE289",
            "#A5A7A5", "#C1C3C1", "#ACADAC", "#371515", "#645756", "#BBBCBB", "#F8F7F8", "#5D4D4C",
            "#310C0C", "#3C1D1C", "#695E5D", "#999B99", "#D7D7D7", "#C5C5C5", "#4A71B9", "#095BF6",
            "#0058FF", "#1E62E2", "#5F79A5", "#818381", "#7D7D7D", "#7B797B", "#7F7F7F", "#AAABAA",
            "#A2A3A2", "#8C8881", "#A08E7D", "#AC917B", "#978B7F", "#BE968B", "#FBA893", "#FFA994",
            "#DE9F8F", "#A18E87", "#8F918F", "#949594", "#B86D43", "#EC5502", "#EE5400", "#CF6226",
            "#9A7A66", "#89A466", "#96DF2D", "#9CFE10", "#9BF816", "#8EBC4E", "#858681", "#BAA040",
            "#EEB900", "#CDA928", "#988F69", "#7678A5", "#5D5FE2", "#5254FF", "#5557F7", "#6E70B9",
            "#848187", "#9050C4", "#9C20FF", "#9441D7", "#877399", "#5DA885", "#1CE489", "#00FE8B",
            "#0BF48A", "#4CB886", "#ABADAB", "#CDCECD", "#B4B5B4", "#310909", "#624747", "#857B7A"

        ]

    # → 256 colors extracted from saveamiga_pal.png, formatted 8 per row.

        ula_plus = [
            "#000000", "#000154", "#0000AA", "#0000FE", "#270100", "#270055", "#2700A9", "#2800FF",
            "#4A0000", "#4B0055", "#4C00AA", "#4B00FF", "#6E0000", "#700056", "#7000AA", "#6F00FF",
            "#920000", "#940056", "#9300A9", "#9300FF", "#B70100", "#B70055", "#B700AA", "#B800FF",
            "#DA0000", "#DB0056", "#DC00AA", "#DC00FF", "#FE0000", "#FF0054", "#FF00AA", "#FF00FE",
            "#012700", "#012756", "#0027AA", "#0027FF", "#272800", "#262755", "#2727A9", "#2728FF",
            "#4A2700", "#4B2755", "#4B28AA", "#4A27FF", "#6F2700", "#6F2755", "#6E27A9", "#6E27FF",
            "#932700", "#932856", "#9227A9", "#9227FF", "#B72800", "#B62755", "#B727AA", "#B728FF",
            "#DA2700", "#DB2756", "#DB28AA", "#DA27FF", "#FF2700", "#FF2756", "#FF28AA", "#FE27FF",
            "#014B00", "#014B56", "#004AA9", "#004BFF", "#274B01", "#264B54", "#274BAB", "#274CFF",
            "#4A4B00", "#4B4B55", "#4B4CA9", "#4B4CFF", "#6F4B00", "#6F4B55", "#6F4CAA", "#6E4BFF",
            "#934B00", "#934B56", "#924BA9", "#924BFF", "#B74B00", "#B64B55", "#B64BA9", "#B74BFF",
            "#DB4C00", "#DB4B55", "#DB4BAA", "#DB4CFF", "#FF4B00", "#FF4B56", "#FF4CAA", "#FE4BFF",
            "#016F00", "#016F56", "#016FAA", "#006FFF", "#276F01", "#277055", "#276FAA", "#276FFF",
            "#4B6F01", "#4B6F55", "#4B6FAB", "#4B70FF", "#6F6F00", "#6F6F55", "#6F70A9", "#6E6FFF",
            "#936F00", "#936F55", "#926FA9", "#926FFF", "#B76F00", "#B76F56", "#B66FA9", "#B76FFF",
            "#DB6F00", "#DB6F55", "#DB6FA9", "#DB6FFF", "#FF6F00", "#FF6F55", "#FF6FAA", "#FE6FFF",
            "#009300", "#019355", "#0193AA", "#0092FF", "#279301", "#279355", "#2693AA", "#2793FF",
            "#4B9301", "#4B9354", "#4B93AB", "#4B93FF", "#6F9200", "#6F9355", "#6F93AB", "#6F94FF",
            "#939300", "#939355", "#9394A9", "#9293FF", "#B79300", "#B79355", "#B693A9", "#B793FF",
            "#DB9300", "#DA9355", "#DB93A9", "#DB93FF", "#FF9400", "#FF9355", "#FF93AA", "#FF93FF",
            "#00B700", "#00B755", "#01B7AA", "#00B6FF", "#27B700", "#27B755", "#26B7AA", "#27B7FE",
            "#4BB701", "#4CB855", "#4BB7AA", "#4BB7FF", "#70B701", "#6FB754", "#6FB7AB", "#6FB7FF",
            "#93B600", "#93B755", "#93B7AB", "#92B7FE", "#B7B700", "#B7B755", "#B7B8AA", "#B7B7FF",
            "#DBB700", "#DBB756", "#DBB7A9", "#DBB7FF", "#FFB700", "#FFB755", "#FFB7A9", "#FFB7FF",
            "#00DC00", "#00DB55", "#00DCA9", "#01DBFF", "#27DB00", "#27DB54", "#27DBAB", "#27DBFE",
            "#4BDB00", "#4BDB55", "#4BDBAA", "#4BDBFE", "#70DB01", "#6FDB54", "#6FDBAA", "#6FDBFF",
            "#94DB01", "#93DB55", "#93DBAB", "#93DCFF", "#B7DA00", "#B7DB55", "#B7DBAB", "#B7DBFF",
            "#DBDB00", "#DBDB55", "#DBDBA9", "#DBDBFF", "#FFDB00", "#FFDA55", "#FFDBA9", "#FFDBFF",
            "#00FF01", "#00FF55", "#00FFAB", "#00FFFF", "#27FE00", "#27FF54", "#27FFAA", "#27FFFE",
            "#4BFF00", "#4BFF55", "#4BFEAA", "#4BFFFE", "#6FFF00", "#70FF55", "#6FFFAA", "#6FFFFF",
            "#94FF01", "#93FF54", "#93FFAA", "#93FFFF", "#B7FE00", "#B7FF55", "#B7FFAB", "#B7FFFE",
            "#DBFE01", "#DBFF55", "#DCFFAB", "#DBFFFF", "#FFFF00", "#FFFF55", "#FFFFA9", "#FFFFFF"

        ]

        amiga_aga_ex = [
            "#828781", "#2D0001", "#FFFFFF", "#0157FF", "#7C797A", "#ACACAC", "#AB917A", "#FDAA92",
            "#959597", "#EE5500", "#9EFE10", "#EABB00", "#4F53FD", "#9B20FF", "#04FD87", "#CECECE",
            "#000000", "#320001", "#000000", "#CBD800", "#414440", "#4F5551", "#646560", "#747570",
            "#898989", "#9E9B9E", "#A8ABAA", "#BAB9C0", "#D0CDCE", "#E0DFE2", "#F2EDEF", "#FFFFFF",
            "#747174", "#1F4C44", "#7B6AFF", "#FEFEAC", "#7B6D42", "#B4945B", "#FFFFFF", "#DEFEFF",
            "#C5FAFF", "#A5F5FF", "#8CF6FF", "#69F2FF", "#50EFFF", "#32EEFF", "#19E9FE", "#00E6FF",
            "#393939", "#313131", "#292929", "#1F201D", "#1A1818", "#090C08", "#0000FD", "#00029C",
            "#020055", "#010902", "#02DEFC", "#02B8FC", "#FE45FE", "#FB39D3", "#FF2DAB", "#FF2082",
            "#FF1452", "#FF072E", "#FF0002", "#FF0C01", "#FC1800", "#FF2300", "#FF2F02", "#FF4004",
            "#FF4C00", "#FF5802", "#FD6500", "#FF7300", "#FF7F01", "#FF8C04", "#FE9905", "#FDA604",
            "#FFB400", "#FFBB03", "#FCCB00", "#FDD904", "#FFE801", "#FEF102", "#FFFD00", "#F6FD03",
            "#E8FC04", "#D5FF00", "#C4FD04", "#BBFF00", "#ACFE01", "#98FC00", "#8AFF00", "#82FE00",
            "#73FF00", "#62FD01", "#53FE00", "#41FF00", "#39FD00", "#2AFE00", "#18FE00", "#06FE02",
            "#01FE00", "#01F600", "#00EC00", "#00E700", "#00DF00", "#00D600", "#03CF01", "#02C600",
            "#05BC01", "#00B801", "#01AC00", "#01A500", "#029E00", "#019600", "#018D00", "#048500",
            "#027F05", "#007600", "#036D01", "#006405", "#005C04", "#005201", "#004901", "#004600",
            "#033B00", "#003400", "#012D01", "#052300", "#021C03", "#001402", "#280101", "#3F0102",
            "#500100", "#6A0100", "#870002", "#940004", "#AC0104", "#C50004", "#D30200", "#EE0102",
            "#FD0100", "#EB0015", "#E20232", "#CC0046", "#BE025F", "#AB0076", "#9F0092", "#8C00A7",
            "#7602C9", "#6102E1", "#65CF64", "#264522", "#AFFFB0", "#D2C20D", "#BBAB13", "#AD9414",
            "#948115", "#846A1A", "#71541A", "#634222", "#522D21", "#003CF4", "#024AF6", "#004EF3",
            "#015DF4", "#0367F7", "#0771F6", "#0378F4", "#0781F7", "#068DF8", "#0695F7", "#059FF8",
            "#292222", "#F9B0B2", "#EAFEAE", "#CCFE9E", "#9DF975", "#83FB58", "#71F83B", "#53F41A",
            "#A9A6AA", "#F0F0F0", "#200021", "#2B052B", "#470734", "#540F3D", "#6F1042", "#800F4C",
            "#8D1A4E", "#9C1C57", "#B1205F", "#BE2468", "#D82A73", "#E52F83", "#5BA5A9", "#7EEAE8",
            "#F8D093", "#8B6748", "#FCFED9", "#C5996A", "#3B3004", "#FFFFBA", "#FFFFF4", "#8EC993",
            "#50674D", "#CDF0CE", "#6A936A", "#2E362F", "#B2EEAF", "#E5F0E1", "#C7C898", "#67694F",
            "#F5F2CC", "#969369", "#3B3732", "#F3F2B3", "#CCD0D3", "#52BF8C", "#334457", "#30498C",
            "#726764", "#939094", "#A08E7F", "#A49891", "#D25B8A", "#7F3F4E", "#D475B7", "#AE4969",
            "#4C3037", "#D467D2", "#D482CB", "#8A5A88", "#4C3C4C", "#B470B0", "#654967", "#37203A",
            "#976996", "#BD81BC", "#AF383C", "#A63A3F", "#9E3836", "#913637", "#893534", "#7E3B32",
            "#753333", "#6F3733", "#5C95A4", "#8E8F8A", "#827F80", "#9F9C9D", "#968E7C", "#C59A8B",
        ]

        atari_2600_ntsc = [
            "#000000", "#404040", "#6c6c6c", "#909090", "#b0b0b0", "#c8c8c8", "#dcdcdc", "#ececec",
            "#444400", "#646410", "#848424", "#a0a034", "#b8b840", "#d0d050", "#e8e85c", "#fcfc68",
            "#702800", "#844414", "#985c28", "#ac783c", "#bc8c4c", "#cca05c", "#dcb468", "#e8cc7c",
            "#841800", "#983418", "#ac5030", "#c06848", "#d0805c", "#e09470", "#eca880", "#fcbc94",
            "#880000", "#9c2020", "#b03c3c", "#c05858", "#d07070", "#e08888", "#eca0a0", "#fcb4b4",
            "#78005c", "#8c2074", "#a03c88", "#b0589c", "#c070b0", "#d084c0", "#dc9cd0", "#ecb0e0",
            "#480078", "#602090", "#783ca4", "#8c58b8", "#a070cc", "#b484dc", "#c49cec", "#d4b0fc",
            "#140084", "#302098", "#4c3cac", "#6858c0", "#7c70d0", "#9488e0", "#a8a0ec", "#bcb4fc",
            "#000088", "#1c209c", "#3840b0", "#505cc0", "#6874d0", "#7c8ce0", "#90a4ec", "#a4b8fc",
            "#00187c", "#1c3890", "#3854a8", "#5070bc", "#6888cc", "#7c9cdc", "#90b4ec", "#a4c8fc",
            "#002c5c", "#1c4c78", "#386890", "#5084ac", "#689cc0", "#7cb0d0", "#90c4e0", "#a4d4ec",
            "#00402c", "#1c5c48", "#387c64", "#509c80", "#68b494", "#7cc8a8", "#90d8bc", "#a4e8d0",
            "#003c00", "#205c20", "#407c40", "#5c9c5c", "#74b474", "#88cc88", "#9ce09c", "#b0f4b0",
            "#143800", "#345c1c", "#507c38", "#6c9850", "#84b468", "#9ccc7c", "#b0e090", "#c4f4a4",
            "#2c3000", "#4c501c", "#687034", "#848c4c", "#9ca864", "#b0bc78", "#c4d08c", "#d8e4a0",
            "#442800", "#644818", "#846830", "#a08444", "#b8a058", "#ccb46c", "#e0c880", "#f4dc94",
        ]

    # - RETRO PALETTE REGISTRY
        self.retro_palettes = {
            "Amiga OCS": amiga_default,    # 32 colors
            "Amiga AGA": amiga_aga,        # 256 colors
            "Amiga AGA WB": amiga_aga_ex,  # 256 colors
            "C64": commodore_64,           # 16 colors
            "ZX Spectrum": zx_spectrum,    # 16 colors
            "Amstrad CPC": amstrad_cpc,    # 27 colors
            "Atari 800": atari_800,        # 40 colors
            "Atari 2600 NTSC": atari_2600_ntsc,  # 128 colors
            "ULA Plus": ula_plus           # 256 colors
        }

    # - PALETTE-SPECIFIC GRID SIZES

        # Each retro palette has optimal grid dimensions based on its color count
        self.retro_palette_grids = {
            "Amiga OCS": (4, 8),      # 32 colors = 4×8
            "Amiga AGA": (16, 16),    # 256 colors = 16×16
            "Amiga AGA WB": (16, 16),  # 160 colors = 10×16
            "C64": (4, 4),            # 16 colors = 4×4
            "ZX Spectrum": (4, 4),    # 16 colors = 4×4
            "Amstrad CPC": (3, 9),    # 27 colors = 3×9
            "Atari 800": (5, 8),      # 40 colors = 5×8
            "Atari 2600 NTSC": (16, 8),  # 128 colors = 16×8
            "ULA Plus": (16, 16)      # 256 colors = 16×16
        }

    # - GRID SETTINGS
        self.palette_sizes = [(4, 6), (6, 8), (8, 10), (10,12), (8, 16), (12, 12), (16, 16)]
        self.current_palette_index = 1  # start 6x8
        self.current_retro_palette = None  # Track if a retro palette is active

    # - POPULATE GRID FUNCTION
        def populate_palette_grid(colors=None):
            # Clear grid
            while self.palette_grid.count():
                item = self.palette_grid.takeAt(0)
                widget = item.widget()
                if widget:
                    widget.deleteLater()

            # Determine rows and cols based on whether we're using a retro palette
            if self.current_retro_palette and self.current_retro_palette in self.retro_palette_grids:
                rows, cols = self.retro_palette_grids[self.current_retro_palette]
            else:
                rows, cols = self.palette_sizes[self.current_palette_index]

            if colors is None:
                # Default gradient-based palette
                base_colors = [
                    "#000000", "#1A1A1A", "#333333", "#4D4D4D", "#666666", "#808080", "#B3B3B3", "#FFFFFF",
                    "#330000", "#660000", "#990000", "#CC0000", "#FF0000", "#FF4000", "#FF8000", "#FFB366",
                    "#332600", "#664D00", "#997300", "#CC9900", "#FFBF00", "#FFD633", "#FFE680", "#FFF2B3",
                    "#003300", "#006600", "#009900", "#00CC00", "#00FF00", "#40FF40", "#80FF80", "#B3FFB3",
                    "#000033", "#000066", "#000099", "#0000CC", "#0000FF", "#0080FF", "#00BFFF", "#66D9FF",
                    "#330033", "#660066", "#990099", "#CC00CC", "#FF00FF", "#FF33FF", "#FF66FF", "#FF99FF"
                ]
                colors = (base_colors * ((rows * cols // len(base_colors)) + 1))[:rows * cols]
            else:
                colors = (colors * ((rows * cols // len(colors)) + 1))[:rows * cols]

            # Populate buttons
            for i, color in enumerate(colors):
                btn = QPushButton()
                btn.setFixedSize(25, 25)
                btn.setStyleSheet(f"background-color: {color}; border: 1px solid #999;")
                btn.setToolTip(color)
                btn.clicked.connect(lambda checked, c=color: self._apply_palette_color(c))
                r, c_ = divmod(i, cols)
                self.palette_grid.addWidget(btn, r, c_)

    # - TOGGLE GRID SIZE
        def toggle_palette_size():
            self.current_palette_index = (self.current_palette_index + 1) % len(self.palette_sizes)
            rows, cols = self.palette_sizes[self.current_palette_index]
            self.palette_toggle_btn.setText(f"Grid: {rows}x{cols}")
            self.current_retro_palette = None  # Reset retro palette when manually changing grid
            populate_palette_grid()

    # - LOAD RETRO PALETTE
        def load_retro_palette(palette_name):
            self.current_retro_palette = palette_name
            colors = self.retro_palettes[palette_name]
            rows, cols = self.retro_palette_grids[palette_name]
            self.palette_toggle_btn.setText(f"Grid: {rows}x{cols}")
            populate_palette_grid(colors)

        self.palette_toggle_btn.clicked.connect(toggle_palette_size)

        # --- RETRO MENU SETUP ---
        retro_menu = QMenu(self)
        for palette_name in self.retro_palettes.keys():
            action = retro_menu.addAction(palette_name)
            action.triggered.connect(lambda checked, name=palette_name: load_retro_palette(name))
        self.retro_btn.setMenu(retro_menu)

    # - INITIAL POPULATE
        populate_palette_grid()

        # Apply Picked Color Group - MOVED TO LEFT PANEL
        # Color mapping (needed before creating combo)
        self.theme_colors = {
            "bg_primary": "Background - Primary",
            "bg_secondary": "Background - Secondary",
            "bg_tertiary": "Background - Tertiary",
            "panel_bg": "Panel Background",
            "text_primary": "Text - Primary",
            "text_secondary": "Text - Secondary",
            "text_accent": "Text - Accent",
            "accent_primary": "Accent - Primary",
            "accent_secondary": "Accent - Secondary",
            "border": "Border - Cell Color",
            "button_normal": "Button - Normal",
            "button_hover": "Button - Hover",
            "button_pressed": "Button - Pressed",
            "selection_background": "Selection - Background",
            "selection_text": "Selection - Text",
            "menu_bg": "Menu - Background",
            "menu_highlight_bg": "Menu - Highlight Background",
            "menu_highlight_text": "Menu - Highlight Text",
            "table_row_even": "Table Row - Even",
            "table_row_odd": "Table Row - Odd",
            "alternate_row": "Alternate Row",
            "success": "Status - Success",
            "warning": "Status - Warning",
            "error": "Status - Error",
            "grid": "Grid Lines",
            "pin_default": "Pin - Default",
            "pin_highlight": "Pin - Highlight",
            "action_import": "Action - Import",
            "action_export": "Action - Export",
            "action_remove": "Action - Remove",
            "action_update": "Action - Update",
            "action_convert": "Action - Convert",
            "panel_entries": "Panel - Entries",
            "panel_filter": "Panel - Filter",
            "toolbar_bg": "Toolbar Background",
            "gadgetbar_bg": "Gadget Bar Background",
            "gadgetbar_text": "Gadget Bar Text",
            "titlebar_bg": "Title Bar Background",
            "titlebar_text": "Title Bar Text",
            "button_text_color": "Button Text - Normal",
            "button_text_hover": "Button Text - Hover",
            "button_text_pressed": "Button Text - Pressed",
            "splitter_color_background": "Splitter - Background",
            "splitter_color_shine": "Splitter - Shine",
            "splitter_color_shadow": "Splitter - Shadow",
            "scrollbar_background": "Scrollbar - Background",
            "scrollbar_handle": "Scrollbar - Handle",
            "scrollbar_handle_hover": "Scrollbar - Handle Hover",
            "scrollbar_handle_pressed": "Scrollbar - Handle Pressed",
            "scrollbar_border": "Scrollbar - Border",
            "dialog_bg":         "Dialog - Background",
            "dialog_text":       "Dialog - Text",
            # - Panel & Hero colours
            "panel_fill_a":              "Two-Tone - Colour A",
            "panel_fill_b":              "Two-Tone - Colour B",
            "hero_bg":                   "Hero Banner - Background",
            "hero_gradient_dark_start":  "Hero Dark - Start",
            "hero_gradient_dark_end":    "Hero Dark - End",
            "hero_gradient_light_start": "Hero Light - Start",
            "hero_gradient_light_end":   "Hero Light - End",
            "panel_grad_stop1":          "Gradient - Stop 1",
            "panel_grad_stop2":          "Gradient - Stop 2",
            "panel_grad_stop3":          "Gradient - Stop 3",
            "panel_pattern_light":       "Pattern - Foreground",
            "panel_pattern_dark":        "Pattern - Background",
            "copper_light":              "Copper - Light",
            "copper_dark":               "Copper - Dark",
            # - System UI
            "window_bg":                 "Window - Background",
            "window_text":               "Window - Text",
            "base":                      "Base",
            "alternate_base":            "Alternate - Base",
            "tooltip_bg":                "Tooltip - Background",
            "tooltip_text":              "Tooltip - Text",
            "placeholder_text":          "Placeholder - Text",
            "disabled_text":             "Disabled - Text"
        }

        selection_group = QGroupBox("Apply Picked Color")
        selection_group.setAutoFillBackground(True)
        selection_layout = QVBoxLayout(selection_group)

        self.selected_element_combo = QComboBox()
        for color_key, color_name in self.theme_colors.items():
            self.selected_element_combo.addItem(color_name, color_key)
        selection_layout.addWidget(self.selected_element_combo)

        apply_color_btn = QPushButton("Apply Picked Color to Element")
        apply_color_btn.clicked.connect(self._apply_picked_color)
        selection_layout.addWidget(apply_color_btn)

        left_layout.addWidget(selection_group)
        left_layout.addStretch()

    # = RIGHT PANEL
        right_panel = QWidget()
        right_panel.setSizePolicy(
            QSizePolicy.Policy.Expanding,
            QSizePolicy.Policy.Expanding,
        )
        right_layout = QVBoxLayout(right_panel)
        right_layout.setContentsMargins(4, 4, 4, 4)

        # Theme Selector
        theme_selector_layout = QHBoxLayout()
        theme_selector_layout.setSpacing(4)

        # Tick box on its own, label beside it (clicking the label toggles it)
        self.instant_apply_check = QCheckBox()
        self.instant_apply_check.setChecked(True)
        self.instant_apply_check.setToolTip("Apply theme changes live")
        apply_lbl = QLabel("Apply Theme")
        apply_lbl.setStyleSheet("font-weight: bold; padding: 2px 6px;")
        apply_lbl.setToolTip("Apply theme changes live")
        apply_lbl.mousePressEvent = lambda e: self.instant_apply_check.toggle()
        apply_lbl.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        self.instant_apply_check.setSizePolicy(QSizePolicy.Policy.Fixed, QSizePolicy.Policy.Fixed)
        theme_selector_layout.addWidget(self.instant_apply_check)
        theme_selector_layout.addWidget(apply_lbl)

        self.theme_selector_combo = QComboBox()
        for theme_key, theme_data in self.app_settings.themes.items():
            display_name = theme_data.get("name", theme_key)
            self.theme_selector_combo.addItem(display_name, theme_key)

        current_theme = self.app_settings.current_settings.get("theme")
        if current_theme:
            for i in range(self.theme_selector_combo.count()):
                if self.theme_selector_combo.itemData(i) == current_theme:
                    self.theme_selector_combo.setCurrentIndex(i)
                    break

        self.theme_selector_combo.currentTextChanged.connect(self._on_theme_changed)
        theme_selector_layout.addWidget(self.theme_selector_combo, 1)
        refresh_btn = QPushButton("Refresh")
        refresh_btn.setToolTip("Reload themes from disk")
        refresh_btn.clicked.connect(lambda _c=False: self._refresh_themes())
        theme_selector_layout.addWidget(refresh_btn)


        right_layout.addLayout(theme_selector_layout)

        # Scrollable Color Editors - MAIN CONTENT
        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)   # MUST be True — widget grows with space
        scroll_area.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        scroll_area.setSizePolicy(
            QSizePolicy.Policy.Expanding,
            QSizePolicy.Policy.Expanding,
        )

        scroll_widget = QWidget()
        scroll_widget.setSizePolicy(
            QSizePolicy.Policy.Expanding,
            QSizePolicy.Policy.Preferred,
        )
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setContentsMargins(2, 2, 2, 2)
        scroll_layout.setSpacing(1)

        # Create color editors — panel effect controls inserted inline as section headers + control rows
        self.color_editors = {}
        current_theme_key = self.theme_selector_combo.currentData()
        current_colors = self.app_settings.themes.get(current_theme_key, {}).get("colors", {})

        def _show_btn(mode):
            b = QPushButton("Show")
            b.setFixedWidth(46)
            b.setFixedHeight(22)
            b.clicked.connect(lambda _, m=mode: self._show_effect_preview_popup(m))
            return b

        def _combo(items, key, changed_fn=None):
            c = QComboBox()
            for item in items: c.addItem(item)
            c.setCurrentIndex(self.app_settings.current_settings.get(key, 0))
            def _on(i, k=key):
                self.app_settings.current_settings[k] = i
                self._refresh_panel_previews()
                if changed_fn: changed_fn(i)
            c.currentIndexChanged.connect(_on)
            return c

        # Keys that get a section header or control row inserted before/after them
        PANEL_SECTIONS = {
            "dialog_bg":    ("header", "Dialog Colours"),
            "panel_fill_a": ("header", "Two-Tone Fill"),
            "panel_fill_b": ("control_after", "fill_dir"),
            "hero_bg":                  ("header", "Hero Banner"),
            "hero_gradient_dark_start": ("header_skip", "Hero Banner Gradient"),
            "hero_gradient_dark_end":   ("control_after", "hero_dark_show"),
            "hero_gradient_light_end":  ("control_after", "hero_light_show"),
            "panel_grad_stop1":         ("header", "Gradient"),
            "panel_grad_stop3":         ("control_after", "grad_dir"),
            "panel_pattern_light":      ("header", "Pattern"),
            "panel_pattern_dark":       ("control_after", "pat_show"),
            "copper_light":             ("header", "Copper Effect"),
            "copper_dark":              ("control_after", "cop_show"),
        }

        for color_key, color_name in self.theme_colors.items():
            # Insert section header BEFORE this key if needed
            sec = PANEL_SECTIONS.get(color_key)
            if sec and sec[0] == "header":
                scroll_layout.addWidget(PanelSectionHeader(sec[1]))

            # Normal colour editor row
            current_value = current_colors.get(color_key, "#ffffff")
            if color_key == "menu_bg" and color_key not in current_colors:
                current_value = current_colors.get("bg_secondary", current_value)
            alpha = (self.app_settings.current_settings.get("color_alpha", {}).get(color_key, 100)
                     if color_key in ALPHA_COLOR_KEYS else None)
            editor = ThemeColorEditor(color_key, color_name, current_value, self, alpha)
            editor.colorChanged.connect(self._on_theme_color_changed)
            editor.alphaChanged.connect(self._on_color_alpha_changed)
            editor.lockChanged.connect(lambda key, locked: None)
            self.color_editors[color_key] = editor
            scroll_layout.addWidget(editor)

            # Insert control row AFTER this key if needed
            if sec and sec[0] == "control_after":
                ctrl_key = sec[1]
                if ctrl_key == "fill_dir":
                    self._panel_fill_dir = _combo([
                        "Solid (A only)", "H Left→Right", "V Top→Bottom",
                        "BL→TR", "BR→TL", "TL→BR", "TR→BL"], "panel_fill_dir")
                    scroll_layout.addWidget(PanelControlRow(
                        "Apply to panels:", self._panel_fill_dir, _show_btn("fill")))
                elif ctrl_key == "hero_dark_show":
                    scroll_layout.addWidget(PanelControlRow(
                        "Dark theme effect:", _show_btn("hero")))
                elif ctrl_key == "hero_light_show":
                    scroll_layout.addWidget(PanelControlRow(
                        "Light theme effect:", _show_btn("hero")))
                elif ctrl_key == "grad_dir":
                    self._grad_dir_combo = _combo([
                        "H Left→Right", "V Top→Bottom", "45° TL→BR",
                        "45° TR→BL", "45° BL→TR", "45° BR→TL"], "panel_grad_dir")
                    scroll_layout.addWidget(PanelControlRow(
                        "Direction:", self._grad_dir_combo, _show_btn("gradient")))
                elif ctrl_key == "pat_show":
                    self._pat_style_combo = _combo([
                        "None","Dots","Lines H","Lines V","Lines Diagonal",
                        "Check","Waves","Tartan","Picnic","Odd/Even rows"],
                        "panel_pattern_style")
                    self._pat_scale = QSlider(Qt.Orientation.Horizontal)
                    self._pat_scale.setRange(2, 32)
                    self._pat_scale.setFixedWidth(70)
                    self._pat_scale.setValue(
                        self.app_settings.current_settings.get("panel_pattern_scale", 8))
                    self._pat_scale.valueChanged.connect(
                        lambda v: [self.app_settings.current_settings.__setitem__(
                            "panel_pattern_scale", v), self._refresh_panel_previews()])
                    scroll_layout.addWidget(PanelControlRow(
                        "Style:", self._pat_style_combo))
                    scroll_layout.addWidget(PanelControlRow(
                        "Scale:", self._pat_scale, _show_btn("pattern")))
                elif ctrl_key == "cop_show":
                    scroll_layout.addWidget(PanelControlRow(
                        "Copper effect:", _show_btn("copper")))

        # Active panel effect selector at bottom of list
        scroll_layout.addWidget(PanelSectionHeader("Active Panel Effect"))
        self._panel_effect_combo = QComboBox()
        self._panel_effect_combo.addItems(["None", "Fill", "Gradient", "Pattern"])
        eff_map = {"none": 0, "fill": 1, "gradient": 2, "pattern": 3}
        self._panel_effect_combo.setCurrentIndex(
            eff_map.get(self.app_settings.current_settings.get("panel_effect_type","none"), 0))
        self._panel_effect_combo.currentIndexChanged.connect(
            lambda i: self.app_settings.current_settings.__setitem__(
                "panel_effect_type", ["none","fill","gradient","pattern"][i]))
        scroll_layout.addWidget(PanelControlRow("Apply to panels:", self._panel_effect_combo))

        # No addStretch() here — scroll_widget Expanding policy handles it
        scroll_area.setWidget(scroll_widget)
        right_layout.addWidget(scroll_area, 1)  # stretch=1 so it fills available space

        # GLOBAL THEME SLIDERS - MOVED TO RIGHT PANEL BOTTOM (above Theme Actions)
        global_sliders_group = QGroupBox("Global Theme Sliders")
        global_sliders_group.setAutoFillBackground(True)
        global_sliders_layout = QVBoxLayout(global_sliders_group)

        info_label = QLabel("<b>Adjust ALL colors globally:</b>")
        global_sliders_layout.addWidget(info_label)

        # Global Hue Slider
        hue_layout = QHBoxLayout()
        hue_layout.addWidget(QLabel("Hue:"))
        self.global_hue_slider = QSlider(Qt.Orientation.Horizontal)
        self.global_hue_slider.setMinimum(-180)
        self.global_hue_slider.setMaximum(180)
        self.global_hue_slider.setValue(0)
        hue_layout.addWidget(self.global_hue_slider)
        self.global_hue_value = QLabel("0")
        self.global_hue_value.setFixedWidth(40)
        hue_layout.addWidget(self.global_hue_value)
        global_sliders_layout.addLayout(hue_layout)

        # Global Saturation Slider
        sat_layout = QHBoxLayout()
        sat_layout.addWidget(QLabel("Sat:"))
        self.global_sat_slider = QSlider(Qt.Orientation.Horizontal)
        self.global_sat_slider.setMinimum(-100)
        self.global_sat_slider.setMaximum(100)
        self.global_sat_slider.setValue(0)
        sat_layout.addWidget(self.global_sat_slider)
        self.global_sat_value = QLabel("0")
        self.global_sat_value.setFixedWidth(40)
        sat_layout.addWidget(self.global_sat_value)
        global_sliders_layout.addLayout(sat_layout)

        # Global Brightness Slider
        bri_layout = QHBoxLayout()
        bri_layout.addWidget(QLabel("Bri:"))
        self.global_bri_slider = QSlider(Qt.Orientation.Horizontal)
        self.global_bri_slider.setMinimum(-100)
        self.global_bri_slider.setMaximum(100)
        self.global_bri_slider.setValue(0)
        bri_layout.addWidget(self.global_bri_slider)
        self.global_bri_value = QLabel("0")
        self.global_bri_value.setFixedWidth(40)
        bri_layout.addWidget(self.global_bri_value)
        global_sliders_layout.addLayout(bri_layout)

        # Slider control buttons
        slider_buttons_layout = QHBoxLayout()

        reset_sliders_btn = QPushButton("Reset Sliders")
        reset_sliders_btn.clicked.connect(self._reset_global_sliders)
        slider_buttons_layout.addWidget(reset_sliders_btn)

        lock_all_btn = QPushButton("Lock All")
        lock_all_btn.clicked.connect(self._lock_all_colors)
        slider_buttons_layout.addWidget(lock_all_btn)

        unlock_all_btn = QPushButton("Unlock All")
        unlock_all_btn.clicked.connect(self._unlock_all_colors)
        slider_buttons_layout.addWidget(unlock_all_btn)

        global_sliders_layout.addLayout(slider_buttons_layout)

        right_layout.addWidget(global_sliders_group)

        # THEME ACTIONS GROUP - AT BOTTOM OF RIGHT PANEL
        theme_layout = QHBoxLayout()  # was QHBoxLayout(self) which corrupted dialog layout
        theme_actions_group = QGroupBox("Theme Actions")
        theme_actions_group.setAutoFillBackground(True)

        # Use horizontal layout instead of vertical
        theme_actions_layout = QHBoxLayout(theme_actions_group)

        # Random Theme button
        random_theme_btn = QPushButton("Random Theme")
        random_theme_btn.setToolTip("Apply a random theme from available themes")
        random_theme_btn.clicked.connect(self._random_theme_from_picker)
        theme_actions_layout.addWidget(random_theme_btn)

        # Save button (saves current theme modifications)
        save_theme_btn = QPushButton("Save")
        save_theme_btn.setToolTip("Save modifications to current theme")
        save_theme_btn.clicked.connect(self._save_current_theme)
        theme_actions_layout.addWidget(save_theme_btn)

        # Save Theme As button
        save_theme_as_btn = QPushButton("Save Theme As...")
        save_theme_as_btn.setToolTip("Save as a new theme file")
        save_theme_as_btn.clicked.connect(self._save_theme_as)
        theme_actions_layout.addWidget(save_theme_as_btn)

        right_layout.addWidget(theme_actions_group)

        # Add panels to main layout
        from PyQt6.QtWidgets import QSizePolicy as _SP2
        left_panel.setSizePolicy(_SP2.Policy.Preferred, _SP2.Policy.Expanding)
        right_panel.setSizePolicy(_SP2.Policy.Expanding, _SP2.Policy.Expanding)
        self._colors_mw = make_outer_mw(right_panel)   # left pane dockable
        _sc = QScrollArea()                   # small min height: pane can re-dock
        _sc.setWidgetResizable(True)
        _sc.setFrameShape(QFrame.Shape.NoFrame)
        _sc.setWidget(left_panel)
        _dock = add_outer_dock(self, self._colors_mw, "Theme Colours", _sc, 260)
        self._colors_mw.resizeDocks([_dock], [280], Qt.Orientation.Horizontal)
        _outer.addWidget(self._colors_mw, 1)

        # IMPORTANT: Connect sliders AFTER all widgets are created
        self.global_hue_slider.valueChanged.connect(self._on_global_hue_changed)
        self.global_sat_slider.valueChanged.connect(self._on_global_sat_changed)
        self.global_bri_slider.valueChanged.connect(self._on_global_bri_changed)

        return tab


    def _create_gadgets_tab(self): #vers 8
        """Create gadgets styling tab with LIVE PREVIEW and proper splitter"""
        tab = QWidget()
        main_layout = QVBoxLayout(tab)

        main_layout.setContentsMargins(4, 4, 4, 4)


        # ========== LEFT SIDE - CONTROLS ==========
        left_widget = QWidget()
        layout = QVBoxLayout(left_widget)
        layout.setSpacing(10)
        layout.setContentsMargins(5, 5, 5, 5)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(4)

        # ========== BUTTON STYLING ==========
        button_group = QGroupBox("Button Styling")
        button_group.setAutoFillBackground(True)
        button_layout = QVBoxLayout(button_group)

        # Button Shape
        shape_layout = QHBoxLayout()
        shape_layout.addWidget(QLabel("Shape:"))
        self.button_shape_combo = QComboBox()
        self.button_shape_combo.addItems(["Rounded", "Square", "Pill", "Beveled"])
        self.button_shape_combo.currentTextChanged.connect(self._update_gadget_preview)
        shape_layout.addWidget(self.button_shape_combo)
        shape_layout.addStretch()
        button_layout.addLayout(shape_layout)

        # Border Radius
        radius_layout = QHBoxLayout()
        radius_layout.addWidget(QLabel("Border Radius:"))
        self.button_radius_slider = QSlider(Qt.Orientation.Horizontal)
        self.button_radius_slider.setRange(0, 20)
        self.button_radius_slider.setValue(4)
        self.button_radius_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_radius_slider.setTickInterval(5)
        self.button_radius_slider.valueChanged.connect(self._update_gadget_preview)
        radius_layout.addWidget(self.button_radius_slider)
        self.button_radius_label = QLabel("4px")
        self.button_radius_label.setFixedWidth(40)
        radius_layout.addWidget(self.button_radius_label)
        button_layout.addLayout(radius_layout)

        self.button_radius_slider.valueChanged.connect(
            lambda v: self.button_radius_label.setText(f"{v}px")
        )

        # Button Height, H and V padding on one row
        height_layout = QHBoxLayout()
        height_layout.addWidget(QLabel("Min Height:"))
        self.button_height_spin = QSpinBox()
        self.button_height_spin.setRange(20, 50)
        self.button_height_spin.setValue(30)
        self.button_height_spin.setSuffix("px")
        self.button_height_spin.valueChanged.connect(self._update_gadget_preview)
        height_layout.addWidget(self.button_height_spin)
        button_layout.addLayout(height_layout)

        # Horizontal Padding
        h_padding_layout = height_layout
        h_padding_layout.addWidget(QLabel("H Pad:"))
        self.button_h_padding_spin = QSpinBox()
        self.button_h_padding_spin.setRange(2, 30)
        self.button_h_padding_spin.setValue(12)
        self.button_h_padding_spin.setSuffix("px")
        self.button_h_padding_spin.valueChanged.connect(self._update_gadget_preview)
        h_padding_layout.addWidget(self.button_h_padding_spin)

        # Vertical Padding
        v_padding_layout = height_layout
        v_padding_layout.addWidget(QLabel("V Pad:"))
        self.button_v_padding_spin = QSpinBox()
        self.button_v_padding_spin.setRange(2, 20)
        self.button_v_padding_spin.setValue(6)
        self.button_v_padding_spin.setSuffix("px")
        self.button_v_padding_spin.valueChanged.connect(self._update_gadget_preview)
        v_padding_layout.addWidget(self.button_v_padding_spin)
        v_padding_layout.addStretch()

        scroll_layout.addWidget(button_group)

        # ========== SLIDER STYLING ==========
        slider_group = QGroupBox("Slider Styling")
        slider_group.setAutoFillBackground(True)
        slider_layout = QVBoxLayout(slider_group)

        # Slider Height
        slider_height_layout = QHBoxLayout()
        slider_height_layout.addWidget(QLabel("Slider Height:"))
        self.slider_height_spin = QSpinBox()
        self.slider_height_spin.setRange(4, 20)
        self.slider_height_spin.setValue(8)
        self.slider_height_spin.setSuffix("px")
        self.slider_height_spin.valueChanged.connect(self._update_gadget_preview)
        slider_height_layout.addWidget(self.slider_height_spin)
        slider_layout.addLayout(slider_height_layout)

        # Handle Size
        handle_size_layout = slider_height_layout
        handle_size_layout.addWidget(QLabel("Handle Size:"))
        self.slider_handle_size = QSpinBox()
        self.slider_handle_size.setRange(12, 30)
        self.slider_handle_size.setValue(16)
        self.slider_handle_size.setSuffix("px")
        self.slider_handle_size.valueChanged.connect(self._update_gadget_preview)
        handle_size_layout.addWidget(self.slider_handle_size)
        handle_size_layout.addStretch()

        # Handle Radius
        slider_radius_layout = QHBoxLayout()
        slider_radius_layout.addWidget(QLabel("Handle Radius:"))
        self.slider_handle_radius = QSlider(Qt.Orientation.Horizontal)
        self.slider_handle_radius.setRange(0, 15)
        self.slider_handle_radius.setValue(8)
        self.slider_handle_radius.valueChanged.connect(self._update_gadget_preview)
        slider_radius_layout.addWidget(self.slider_handle_radius)
        self.slider_handle_radius_label = QLabel("8px")
        self.slider_handle_radius_label.setFixedWidth(40)
        slider_radius_layout.addWidget(self.slider_handle_radius_label)
        slider_layout.addLayout(slider_radius_layout)

        self.slider_handle_radius.valueChanged.connect(
            lambda v: self.slider_handle_radius_label.setText(f"{v}px")
        )

        scroll_layout.addWidget(slider_group)

        # ========== CHECKBOX STYLING ==========
        checkbox_group = QGroupBox("Checkbox Styling")
        checkbox_group.setAutoFillBackground(True)
        checkbox_layout = QVBoxLayout(checkbox_group)

        # Checkbox Size
        cb_size_layout = QHBoxLayout()
        cb_size_layout.addWidget(QLabel("Checkbox Size:"))
        self.checkbox_size_spin = QSpinBox()
        self.checkbox_size_spin.setRange(12, 30)
        self.checkbox_size_spin.setValue(18)
        self.checkbox_size_spin.setSuffix("px")
        self.checkbox_size_spin.valueChanged.connect(self._update_gadget_preview)
        cb_size_layout.addWidget(self.checkbox_size_spin)
        cb_size_layout.addStretch()
        checkbox_layout.addLayout(cb_size_layout)

        # Checkbox Radius
        cb_radius_layout = QHBoxLayout()
        cb_radius_layout.addWidget(QLabel("Border Radius:"))
        self.checkbox_radius_slider = QSlider(Qt.Orientation.Horizontal)
        self.checkbox_radius_slider.setRange(0, 9)
        self.checkbox_radius_slider.setValue(3)
        self.checkbox_radius_slider.valueChanged.connect(self._update_gadget_preview)
        cb_radius_layout.addWidget(self.checkbox_radius_slider)
        self.checkbox_radius_label = QLabel("3px")
        self.checkbox_radius_label.setFixedWidth(40)
        cb_radius_layout.addWidget(self.checkbox_radius_label)
        checkbox_layout.addLayout(cb_radius_layout)

        self.checkbox_radius_slider.valueChanged.connect(
            lambda v: self.checkbox_radius_label.setText(f"{v}px")
        )

        scroll_layout.addWidget(checkbox_group)

        # ========== SCROLLBAR STYLING ==========
        scrollbar_group = QGroupBox("Scrollbar Styling")
        scrollbar_group.setAutoFillBackground(True)
        scrollbar_layout = QVBoxLayout(scrollbar_group)

        # Scrollbar Width
        sb_width_layout = QHBoxLayout()
        sb_width_layout.addWidget(QLabel("Width:"))
        self.scrollbar_width_slider = QSlider(Qt.Orientation.Horizontal)
        self.scrollbar_width_slider.setRange(6, 20)
        self.scrollbar_width_slider.setValue(12)
        self.scrollbar_width_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.scrollbar_width_slider.setTickInterval(2)
        self.scrollbar_width_slider.valueChanged.connect(self._update_gadget_preview)
        sb_width_layout.addWidget(self.scrollbar_width_slider)
        self.scrollbar_width_label = QLabel("12px")
        self.scrollbar_width_label.setFixedWidth(40)
        sb_width_layout.addWidget(self.scrollbar_width_label)
        scrollbar_layout.addLayout(sb_width_layout)

        self.scrollbar_width_slider.valueChanged.connect(
            lambda v: self.scrollbar_width_label.setText(f"{v}px")
        )

        # Handle Radius
        handle_radius_layout = QHBoxLayout()
        handle_radius_layout.addWidget(QLabel("Handle Radius:"))
        self.scrollbar_handle_radius = QSlider(Qt.Orientation.Horizontal)
        self.scrollbar_handle_radius.setRange(0, 10)
        self.scrollbar_handle_radius.setValue(3)
        self.scrollbar_handle_radius.valueChanged.connect(self._update_gadget_preview)
        handle_radius_layout.addWidget(self.scrollbar_handle_radius)
        self.scrollbar_handle_radius_label = QLabel("3px")
        self.scrollbar_handle_radius_label.setFixedWidth(40)
        handle_radius_layout.addWidget(self.scrollbar_handle_radius_label)
        scrollbar_layout.addLayout(handle_radius_layout)

        self.scrollbar_handle_radius.valueChanged.connect(
            lambda v: self.scrollbar_handle_radius_label.setText(f"{v}px")
        )

        scroll_layout.addWidget(scrollbar_group)

        # ========== SPLITTER STYLING ==========
        splitter_group = QGroupBox("Splitter Styling")
        splitter_group.setAutoFillBackground(True)
        splitter_layout = QVBoxLayout(splitter_group)

        # Splitter Width
        sp_width_layout = QHBoxLayout()
        sp_width_layout.addWidget(QLabel("Handle Width:"))
        self.splitter_width_slider = QSlider(Qt.Orientation.Horizontal)
        self.splitter_width_slider.setRange(4, 16)
        self.splitter_width_slider.setValue(8)
        self.splitter_width_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.splitter_width_slider.setTickInterval(2)
        self.splitter_width_slider.valueChanged.connect(self._update_gadget_preview)
        sp_width_layout.addWidget(self.splitter_width_slider)
        self.splitter_width_label = QLabel("8px")
        self.splitter_width_label.setFixedWidth(40)
        sp_width_layout.addWidget(self.splitter_width_label)
        splitter_layout.addLayout(sp_width_layout)

        self.splitter_width_slider.valueChanged.connect(
            lambda v: self.splitter_width_label.setText(f"{v}px")
        )

        # Splitter Radius
        sp_radius_layout = QHBoxLayout()
        sp_radius_layout.addWidget(QLabel("Handle Radius:"))
        self.splitter_radius_slider = QSlider(Qt.Orientation.Horizontal)
        self.splitter_radius_slider.setRange(0, 8)
        self.splitter_radius_slider.setValue(3)
        self.splitter_radius_slider.valueChanged.connect(self._update_gadget_preview)
        sp_radius_layout.addWidget(self.splitter_radius_slider)
        self.splitter_radius_label = QLabel("3px")
        self.splitter_radius_label.setFixedWidth(40)
        sp_radius_layout.addWidget(self.splitter_radius_label)
        splitter_layout.addLayout(sp_radius_layout)

        self.splitter_radius_slider.valueChanged.connect(
            lambda v: self.splitter_radius_label.setText(f"{v}px")
        )

        # Show Grip
        grip_layout = QHBoxLayout()
        self.splitter_show_grip = QCheckBox("Show Grip Handle")
        self.splitter_show_grip.setChecked(True)
        self.splitter_show_grip.stateChanged.connect(self._update_gadget_preview)
        grip_layout.addWidget(self.splitter_show_grip)
        splitter_layout.addLayout(grip_layout)

        # Handle Style — 4 types
        hs_layout = QHBoxLayout()
        hs_layout.addWidget(QLabel("Handle Style:"))
        self.handle_style_combo = QComboBox()
        self.handle_style_combo.addItems(["line", "gradient", "dots", "invisible"])
        cs_now = self.app_settings.current_settings
        self.handle_style_combo.setCurrentText(cs_now.get('handle_style', 'line'))
        self.handle_style_combo.currentTextChanged.connect(self._update_gadget_preview)
        self.handle_style_combo.currentTextChanged.connect(
            lambda v: self.app_settings.current_settings.update({'handle_style': v}))
        hs_layout.addWidget(self.handle_style_combo, 1)
        splitter_layout.addLayout(hs_layout)

        # Hide handles when docked (same row as grip)
        hd_layout = grip_layout
        self.handle_hide_docked = QCheckBox("Hide handles when docked")
        self.handle_hide_docked.setChecked(cs_now.get('handle_hide_docked', False))
        self.handle_hide_docked.stateChanged.connect(
            lambda v: self.app_settings.current_settings.update({'handle_hide_docked': bool(v)}))
        hd_layout.addWidget(self.handle_hide_docked)
        hd_layout.addStretch()

        scroll_layout.addWidget(splitter_group)

        # ========== ADVANCED STYLING ==========
        advanced_group = QGroupBox("Advanced Styling")
        advanced_group.setAutoFillBackground(True)
        advanced_layout = QVBoxLayout(advanced_group)

        # Panel Opacity
        opacity_layout = QHBoxLayout()
        opacity_layout.addWidget(QLabel("Panel Opacity:"))
        self.panel_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.panel_opacity_slider.setRange(50, 100)
        self.panel_opacity_slider.setValue(95)
        self.panel_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_opacity_slider.setTickInterval(10)
        self.panel_opacity_slider.valueChanged.connect(self._update_gadget_preview)
        opacity_layout.addWidget(self.panel_opacity_slider)
        self.panel_opacity_label = QLabel("95%")
        self.panel_opacity_label.setFixedWidth(40)
        opacity_layout.addWidget(self.panel_opacity_label)
        advanced_layout.addLayout(opacity_layout)

        self.panel_opacity_slider.valueChanged.connect(
            lambda v: self.panel_opacity_label.setText(f"{v}%")
        )

        # Enable Animations
        anim_layout = QHBoxLayout()
        self.enable_animations = QCheckBox("Enable Hover Animations")
        self.enable_animations.setChecked(True)
        self.enable_animations.stateChanged.connect(self._update_gadget_preview)
        anim_layout.addWidget(self.enable_animations)
        anim_layout.addStretch()
        advanced_layout.addLayout(anim_layout)

        scroll_layout.addWidget(advanced_group)

        scroll_layout.addStretch()
        for grp in scroll_widget.findChildren(QGroupBox):        # condensed rows
            grp.layout().setSpacing(3)
            grp.layout().setContentsMargins(6, 4, 6, 4)
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        # Reset button
        reset_btn = QPushButton("Reset to Defaults")
        reset_btn.clicked.connect(self._reset_gadget_styles)
        layout.addWidget(reset_btn)


        # ========== RIGHT SIDE - LIVE PREVIEW PANEL ==========
        preview_panel = QWidget()
        preview_layout = QVBoxLayout(preview_panel)
        preview_layout.setContentsMargins(4, 0, 4, 4)
        preview_layout.addWidget(QLabel("<b>Live Preview</b>"))

        # Preview frame (scrolls, so samples never overlap)
        self.preview_frame = QFrame()
        self.preview_frame.setFrameStyle(QFrame.Shape.Box | QFrame.Shadow.Sunken)
        preview_frame_layout = QVBoxLayout(self.preview_frame)
        preview_frame_layout.setSpacing(4)

        # Sample buttons in one row
        preview_frame_layout.addWidget(QLabel("Buttons:"))
        btn_row = QHBoxLayout()
        self.preview_btn_normal = QPushButton("Normal")
        self.preview_btn_primary = QPushButton("Primary")
        self.preview_btn_danger = QPushButton("Remove")
        for b in (self.preview_btn_normal, self.preview_btn_primary, self.preview_btn_danger):
            btn_row.addWidget(b)
        preview_frame_layout.addLayout(btn_row)

        # Sample sliders
        preview_frame_layout.addWidget(QLabel("Slider:"))
        self.preview_slider_h = QSlider(Qt.Orientation.Horizontal)
        self.preview_slider_h.setRange(0, 100)
        self.preview_slider_h.setValue(50)
        preview_frame_layout.addWidget(self.preview_slider_h)

        # Sample checkboxes in one row
        cb_row = QHBoxLayout()
        self.preview_checkbox1 = QCheckBox("Feature A")
        self.preview_checkbox1.setChecked(True)
        self.preview_checkbox2 = QCheckBox("Feature B")
        cb_row.addWidget(self.preview_checkbox1)
        cb_row.addWidget(self.preview_checkbox2)
        cb_row.addStretch()
        preview_frame_layout.addLayout(cb_row)

        # Sample text area with scrollbar
        preview_frame_layout.addWidget(QLabel("Scrollbar:"))
        self.preview_text = QTextEdit()
        self.preview_text.setPlainText(
            "IMG Factory File Browser\n"
            "======================\n\n"
            "gta3.img - 1,245 entries\n"
            "gta_int.img - 892 entries\n"
            "player.img - 156 entries\n\n"
            "DFF Models:\n"
            "• admiral.dff\n"
            "• banshee.dff\n"
            "• infernus.dff\n"
            "• patriot.dff\n"
            "• rhino.dff\n\n"
            "TXD Textures:\n"
            "• admiral.txd\n"
            "• banshee.txd\n"
            "• infernus.txd\n\n"
            "COL Collision:\n"
            "• vehicles.col\n"
            "• buildings.col\n"
            "• peds.col\n\n"
            "Total files: 2,293\n"
            "Total size: 1.2 GB\n"
            "RW Version: 3.6.0.0\n"
        )
        self.preview_text.setFixedHeight(90)
        preview_frame_layout.addWidget(self.preview_text)

        # Sample splitter
        preview_frame_layout.addWidget(QLabel("Splitter:"))
        self.preview_splitter = QSplitter(Qt.Orientation.Horizontal)
        left_sample = QLabel("Left Panel")
        left_sample.setFrameStyle(QFrame.Shape.Box)
        left_sample.setAlignment(Qt.AlignmentFlag.AlignCenter)
        left_sample.setMinimumHeight(50)
        right_sample = QLabel("Right Panel")
        right_sample.setFrameStyle(QFrame.Shape.Box)
        right_sample.setAlignment(Qt.AlignmentFlag.AlignCenter)
        right_sample.setMinimumHeight(50)
        self.preview_splitter.addWidget(left_sample)
        self.preview_splitter.addWidget(right_sample)
        preview_frame_layout.addWidget(self.preview_splitter)

        preview_frame_layout.addStretch()

        pv_scroll = QScrollArea()
        pv_scroll.setWidgetResizable(True)
        pv_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        pv_scroll.setWidget(self.preview_frame)
        preview_layout.addWidget(pv_scroll)

        self._gadgets_mw = make_outer_mw(preview_panel)   # controls pane dockable
        _sc = QScrollArea()                   # small min height: pane can re-dock
        _sc.setWidgetResizable(True)
        _sc.setFrameShape(QFrame.Shape.NoFrame)
        _sc.setWidget(left_widget)
        _dock = add_outer_dock(self, self._gadgets_mw, "Gadget Controls", _sc, 300)
        self._gadgets_mw.resizeDocks([_dock], [460], Qt.Orientation.Horizontal)
        main_layout.addWidget(self._gadgets_mw)

        # Initial preview update
        self._update_gadget_preview()

        return tab


    def _create_buttons_tab_v2(self): #vers 3
        """Buttons tab — style selector with live preview + per-panel tint colours."""
        from PyQt6.QtWidgets import (QScrollArea, QGridLayout, QFrame, QSizePolicy)
        from PyQt6.QtGui import QColor, QPainter, QLinearGradient, QPen
        from PyQt6.QtCore import QRect

        tab = QWidget()
        root = QVBoxLayout(tab)
        root.setSpacing(6)

        #    Size controls                                                  
        size_group = QGroupBox("Button and Titlebar Sizes")
        size_group.setAutoFillBackground(True)
        szl = QGridLayout(size_group)
        szl.setColumnStretch(1, 1)

        def _spinbox(key, lo, hi, suffix="px"):
            sp = QSpinBox()
            sp.setRange(lo, hi)
            sp.setSuffix(suffix)
            sp.setValue(self.app_settings.current_settings.get(key, (hi+lo)//2))
            def _on(v, k=key):
                self.app_settings.current_settings[k] = v
                self._apply_button_sizes()
            sp.valueChanged.connect(_on)
            return sp

        szl.setColumnStretch(1, 0)
        self._tb_height_spin = _spinbox("titlebar_button_height", 24, 56)
        self._tb_btn_size_spin = _spinbox("titlebar_button_size", 18, 48)
        self._tb_icon_spin = _spinbox("titlebar_icon_size", 12, 36)
        self._btn_height_spin = _spinbox("button_size", 18, 48)
        for i, (lbl, sp) in enumerate([("Titlebar height:", self._tb_height_spin),
                                       ("Titlebar button size:", self._tb_btn_size_spin),
                                       ("Titlebar icon size:", self._tb_icon_spin),
                                       ("Panel button height:", self._btn_height_spin)]):
            sp.setMinimumWidth(80)
            szl.addWidget(QLabel(lbl), i // 2, (i % 2) * 3)
            szl.addWidget(sp, i // 2, (i % 2) * 3 + 1)
        szl.setColumnStretch(2, 1)
        szl.setColumnStretch(5, 1)

        root.addWidget(size_group)

        #   Style selector + workflow tint switch on one row
        style_group = QGroupBox("Button Style")
        style_group.setAutoFillBackground(True)
        sg_lay = QHBoxLayout(style_group)

        STYLES = [
            ("flat",        "Flat",          "No effect — theme colour only"),
            ("gradient_h",  "Gradient H",    "Left → Right gradient"),
            ("gradient_v",  "Gradient V",    "Top → Bottom gradient"),
            ("gradient_45", "Gradient 45°",  "Diagonal shine (TL → BR)"),
            ("banded",      "Banded",        "Windows ME style — horizontal highlight band"),
            ("zen",         "Zen",           "Subtle inner glow, soft rounded edges"),
            ("indented",    "Indented",      "Sunken / pressed appearance"),
            ("bump",        "Bump",          "Raised 3-D bevel"),
            ("amiga_wb",    "Amiga WB",      "Classic Workbench full shine top→bottom"),
            ("half_shine",  "Half-shine",    "Shine top half, flat bottom"),
            ("shadow_dark", "Shadow-dark",   "Dark shadow BR, light highlight TL"),
        ]

        current_style = self.app_settings.current_settings.get("button_style", "flat")

        # Dummy combo for backward compat with _create_button_panel_editor
        if not hasattr(self, "button_theme_type_combo"):
            self.button_theme_type_combo = QComboBox()
            self.button_theme_type_combo.addItems(["Light Theme Buttons", "Dark Theme Buttons"])
            self.button_theme_type_combo.setVisible(False)

        self._btn_style_combo = QComboBox()
        for i, (key, label, tip) in enumerate(STYLES):
            self._btn_style_combo.addItem(label, key)
            self._btn_style_combo.setItemData(i, tip, Qt.ItemDataRole.ToolTipRole)
        idx = self._btn_style_combo.findData(current_style)
        self._btn_style_combo.setCurrentIndex(max(0, idx))
        self._btn_style_combo.currentIndexChanged.connect(
            lambda i: self._on_btn_style_changed(self._btn_style_combo.itemData(i)))
        sg_lay.addWidget(QLabel("Style:"))
        sg_lay.addWidget(self._btn_style_combo, 1)
        sg_lay.addSpacing(20)
        root.addWidget(style_group)

        #   Live preview
        preview_group = QGroupBox("Preview")
        preview_group.setAutoFillBackground(True)
        pg_lay = QHBoxLayout(preview_group)
        pg_lay.setSpacing(8)

        self._preview_btns = []
        for label, colour in [
            ("Normal",   "#4a7a9b"),
            ("Hover",    "#5a9abf"),
            ("Pressed",  "#3a6a8b"),
            ("Disabled", "#888888"),
            ("Save",     "#4a2d4a"),
            ("Danger",   "#7a2d2d"),
        ]:
            b = QPushButton(label)
            b.setFixedHeight(28)
            b.setEnabled(label != "Disabled")
            self._preview_btns.append((b, colour))
            pg_lay.addWidget(b)

        root.addWidget(preview_group)
        self._update_btn_style_preview()

        #   Tint on/off (in the style row)
        tg_lay = sg_lay
        self._tint_enabled_cb = QCheckBox("Workflow colour tints on buttons")
        self._tint_enabled_cb.setChecked(
            self.app_settings.current_settings.get("use_pastel_buttons", True))
        self._tint_enabled_cb.setToolTip(
            "Colours aid workflow — e.g. pink=save, blue=open. Can be disabled here.")
        tg_lay.addWidget(self._tint_enabled_cb)

        #   Per-panel tint colours
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        sw = QWidget(); sl = QVBoxLayout(sw); sl.setSpacing(8)

        self.button_panels_tabs = QTabWidget()

        img_files_tab = self._create_button_panel_editor("img_files", [
            ("Create",       "create_action"),
            ("Open",         "open_action"),
            ("Hybrid Load",  "open_action"),
            ("Scan Folder",  "open_action"),
            ("Reload",       "reload_action"),
            ("Encrypt",      "build_action"),
            ("Close",        "close_action"),
            ("Rebuild",      "build_action"),
            ("Save Entry",   "save_action"),
            ("Merge",        "merge_action"),
            ("Convert",      "convert_action"),
        ])
        self.button_panels_tabs.addTab(img_files_tab, "IMG Files")

        file_entries_tab = self._create_button_panel_editor("file_entries", [
            ("Import",       "import_action"),
            ("Export",       "export_action"),
            ("Remove",       "remove_action"),
            ("Refresh",      "reload_action"),
            ("Dump",         "merge_action"),
            ("Extract",      "export_action"),
            ("Rename",       "edit_action"),
            ("Select",       "select_action"),
        ])
        self.button_panels_tabs.addTab(file_entries_tab, "File Entries")

        editing_tab = self._create_button_panel_editor("editing_options", [
            ("Col Edit",     "editor_col"),
            ("Txd Edit",     "editor_txd"),
            ("Dff Edit",     "editor_dff"),
            ("Ipf Edit",     "editor_data"),
            ("IDE Edit",     "editor_data"),
            ("IPL Edit",     "editor_map"),
            ("Dat Edit",     "editor_data"),
            ("Radar Map",    "editor_map"),
            ("Paths Map",    "editor_map"),
            ("Waterpro",     "editor_data"),
            ("Weather",      "editor_data"),
            ("Handling",     "editor_vehicle"),
        ])
        self.button_panels_tabs.addTab(editing_tab, "Editing Options")

        sl.addWidget(self.button_panels_tabs)
        scroll.setWidget(sw)
        root.addWidget(scroll, 1)

        #   Actions
        act = QHBoxLayout()
        rst = QPushButton("Reset to Theme Defaults")
        rst.clicked.connect(self._reset_button_colors_to_defaults)
        act.addWidget(rst)
        act.addStretch()
        root.addLayout(act)

        return tab


    def _create_button_panel_editor(self, panel_id, buttons): #vers 2
        """Tint editor for a button panel: rows of name, swatch, hex, Pick in two columns."""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        scroll_area = QScrollArea()
        scroll_area.setWidgetResizable(True)
        scroll_widget = QWidget()
        from PyQt6.QtWidgets import QGridLayout
        scroll_layout = QGridLayout(scroll_widget)
        scroll_layout.setHorizontalSpacing(6)
        scroll_layout.setVerticalSpacing(2)
        row_n = 0

        # Store button color editors
        if not hasattr(self, 'button_color_editors'):
            self.button_color_editors = {}

        self.button_color_editors[panel_id] = {}

        for button_text, button_action in buttons:
            button_key = f"{panel_id}_{button_action}"

            # Get default color
            default_colors = self._get_default_button_colors()
            try:
                from PyQt6.QtGui import QColor
                _tc = self.app_settings.get_theme_colors() or {}
                _bg = _tc.get("bg_primary", "#1a1a2e")
                is_light = QColor(_bg).lightness() > 128
            except Exception:
                is_light = False
            color_key = f"button_{panel_id}_{button_action}_{'light' if is_light else 'dark'}"
            default_color = self.app_settings.current_settings.get(color_key, default_colors.get(button_action, "#E3F2FD"))

            # Row: name, swatch, hex, Pick (two rows per grid line)
            editor_group = QWidget()
            editor_layout = QHBoxLayout(editor_group)
            editor_layout.setContentsMargins(2, 0, 2, 0)
            name_lbl = QLabel(button_text)
            name_lbl.setFixedWidth(110)
            editor_layout.addWidget(name_lbl)

            # Color preview
            color_preview = QLabel()
            color_preview.setFixedSize(28, 22)
            color_preview.setStyleSheet(f"background-color: {default_color}; border: 1px solid #999;")
            editor_layout.addWidget(color_preview)

            # Color input
            color_input = QLineEdit(default_color)
            color_input.setMaximumWidth(100)
            color_input.textChanged.connect(
                lambda c, key=button_key, prev=color_preview: self._on_button_color_changed(key, c, prev)
            )
            editor_layout.addWidget(color_input)

            # Pick button
            pick_btn = QPushButton("Pick")
            pick_btn.setFixedWidth(54)
            pick_btn.clicked.connect(
                lambda checked, inp=color_input: self._pick_button_color(inp)
            )
            editor_layout.addWidget(pick_btn)

            editor_layout.addStretch()

            scroll_layout.addWidget(editor_group, row_n // 2, row_n % 2)
            row_n += 1

            # Store for later access
            self.button_color_editors[panel_id][button_action] = {
                'preview': color_preview,
                'input': color_input,
                'key': button_key
            }

        scroll_area.setWidget(scroll_widget)
        layout.addWidget(scroll_area)

        return widget


    def _create_demo_tab(self) -> QWidget:
        """Create improved demo tab with better layout"""
        tab = QWidget()
        main_layout = QHBoxLayout(tab)  # Changed to horizontal for better space usage

        # Left column - Controls
        left_widget = QWidget()
        left_layout = QVBoxLayout(left_widget)
        left_widget.setMaximumWidth(300)

        # Theme Selection Group
        theme_group = QGroupBox("Theme Selection")
        theme_group.setAutoFillBackground(True)
        theme_layout = QVBoxLayout(theme_group)

        # Current theme display
        current_layout = QHBoxLayout()
        current_layout.addWidget(QLabel("Active Theme:"))
        self.current_theme_label = QLabel(self.app_settings.current_settings["theme"])
        self.current_theme_label.setStyleSheet("font-weight: bold; color: #2E7D32;")
        current_layout.addWidget(self.current_theme_label)
        current_layout.addStretch()
        theme_layout.addLayout(current_layout)

        # Theme selector
        preview_layout = QHBoxLayout()
        preview_layout.addWidget(QLabel("Preview:"))
        self.demo_theme_combo = QComboBox()
        available_themes = list(self.app_settings.themes.keys())
        self.demo_theme_combo.addItems(available_themes)
        refresh_themes_btn = QPushButton("Refresh Themes")
        refresh_themes_btn.setToolTip("Reload themes from apps.themes. folder")
        refresh_themes_btn.clicked.connect(self.refresh_themes_in_dialog)
        self.demo_theme_combo.setCurrentText(self.app_settings.current_settings["theme"])
        self.demo_theme_combo.currentTextChanged.connect(self._preview_theme_instantly)

        preview_layout.addWidget(self.demo_theme_combo)
        theme_layout.addLayout(preview_layout)

        left_layout.addWidget(theme_group)

        # Real-time Controls Group
        controls_group = QGroupBox("Live Controls")
        controls_group.setAutoFillBackground(True)
        controls_layout = QVBoxLayout(controls_group)

        # Instant apply toggle
        self.instant_apply_check = QCheckBox("Apply changes instantly")
        self.instant_apply_check.setChecked(True)
        self.instant_apply_check.toggled.connect(self._toggle_instant_apply)
        controls_layout.addWidget(self.instant_apply_check)

        # Auto-preview toggle
        self.auto_preview_check = QCheckBox("Auto-preview on selection")
        self.auto_preview_check.setChecked(True)
        controls_layout.addWidget(self.auto_preview_check)

        # Preview scope
        scope_layout = QHBoxLayout()
        scope_layout.addWidget(QLabel("Preview Scope:"))
        self.preview_scope_combo = QComboBox()
        self.preview_scope_combo.addItems(["Demo Only", "Dialog Only", "Full Application"])
        self.preview_scope_combo.setCurrentIndex(2)  # Full Application
        self.preview_scope_combo.currentTextChanged.connect(self._change_preview_scope)
        scope_layout.addWidget(self.preview_scope_combo)
        controls_layout.addLayout(scope_layout)

        left_layout.addWidget(controls_group)

        # Quick Themes Group
        quick_group = QGroupBox("🚀 Quick Themes")
        quick_group.setAutoFillBackground(True)
        quick_layout = QVBoxLayout(quick_group)

        # Popular themes
        popular_themes = ["LCARS", "App_Factory", "Deep_Purple", "Cyberpunk", "Matrix"]
        for theme_name in popular_themes:
            if theme_name in self.app_settings.themes:
                quick_btn = QPushButton(f" {theme_name}")
                quick_btn.clicked.connect(lambda checked, t=theme_name: self._apply_quick_theme(t))
                quick_btn.setMinimumHeight(35)
                quick_layout.addWidget(quick_btn)

        # Reset and randomize buttons
        button_layout = QHBoxLayout()
        reset_btn = QPushButton("Reset")
        reset_btn.clicked.connect(self._reset_demo_theme)
        random_btn = QPushButton("Random")
        random_btn.clicked.connect(self._random_theme)
        button_layout.addWidget(reset_btn)
        button_layout.addWidget(random_btn)
        quick_layout.addLayout(button_layout)

        left_layout.addWidget(quick_group)

        # Theme Info Group
        info_group = QGroupBox("Theme Info")
        info_group.setAutoFillBackground(True)
        info_layout = QVBoxLayout(info_group)

        self.theme_info_label = QLabel()
        self.theme_info_label.setWordWrap(True)
        self.theme_info_label.setMinimumHeight(100)
        #self.theme_info_label.setStyleSheet("padding: 8px;  border-radius: 4px;")
        info_layout.addWidget(self.theme_info_label)

        left_layout.addWidget(info_group)
        left_layout.addStretch()

        main_layout.addWidget(left_widget)

        # Right column - Preview Area
        right_widget = QWidget()
        right_layout = QVBoxLayout(right_widget)

        # Preview Header
        preview_header = QGroupBox("Live Preview - App Factory Interface")
        preview_header.setAutoFillBackground(True)
        header_layout = QHBoxLayout(preview_header)

        self.preview_status = QLabel("Ready for preview")
        self.preview_status.setStyleSheet("color: #4CAF50; font-weight: bold;")
        header_layout.addWidget(self.preview_status)
        header_layout.addStretch()

        # Preview controls
        self.full_preview_btn = QPushButton("Full Preview")
        self.full_preview_btn.clicked.connect(self._show_full_preview)
        header_layout.addWidget(self.full_preview_btn)

        right_layout.addWidget(preview_header)

        # Sample App Factory Toolbar
        toolbar_group = QGroupBox("Sample Toolbar")
        toolbar_group.setAutoFillBackground(True)
        toolbar_layout = QGridLayout(toolbar_group)

        self.demo_buttons = []
        toolbar_buttons = [
            ("Open IMG", "import", "Open IMG archive"),
            ("Import Files", "import", "Import files to archive"),
            ("Export Selected", "export", "Export selected entries"),
            ("Remove Entry", "remove", "Remove selected entry"),
            ("Refresh", "update", "Refresh entry list"),
            ("Convert Format", "convert", "Convert file format"),
            ("Save Archive", None, "Save current archive"),
            ("Settings", None, "Open settings dialog")
        ]

        for i, (text, action_type, tooltip) in enumerate(toolbar_buttons):
            btn = QPushButton(text)
            if action_type:
                btn.setProperty("action-type", action_type)
            btn.setToolTip(tooltip)
            btn.setMinimumHeight(35)
            self.demo_buttons.append(btn)
            toolbar_layout.addWidget(btn, i // 4, i % 4)

        right_layout.addWidget(toolbar_group)

        # Sample Table
        table_group = QGroupBox("Sample IMG Entries Table")
        table_group.setAutoFillBackground(True)
        table_layout = QVBoxLayout(table_group)

        self.demo_table = QTableWidget(5, 5)
        self.demo_table.setHorizontalHeaderLabels(["Filename", "Type", "Size", "Version", "Status"])
        self.demo_table.setMaximumHeight(180)

        # Auto-resize columns
        self.demo_table.resizeColumnsToContents()
        table_layout.addWidget(self.demo_table)

        right_layout.addWidget(table_group)

        # Sample Log Output
        log_group = QGroupBox("Sample Activity Log")
        log_group.setAutoFillBackground(True)
        log_layout = QVBoxLayout(log_group)

        self.demo_log = QTextEdit()
        self.demo_log.setMaximumHeight(120)
        self.demo_log.setReadOnly(True)

        # Enhanced log content
        initial_log = """App Factory 1.0 - Live Theme Preview
Current IMG: sample_archive.img (150 MB)
Entries loaded: 1,247 files
Active theme: """ + self.app_settings.current_settings["theme"] + """
Live preview mode: ACTIVE
Ready for operations..."""

        self.demo_log.setPlainText(initial_log)
        log_layout.addWidget(self.demo_log)

        right_layout.addWidget(log_group)

        # Preview Statistics
        stats_group = QGroupBox("Preview Statistics")
        stats_group.setAutoFillBackground(True)
        stats_layout = QGridLayout(stats_group)

        self.stats_labels = {}
        stats_data = [
            ("Themes Available:", str(len(available_themes))),
            ("Preview Changes:", "0"),
            ("Last Applied:", "None"),
            ("Performance:", "Excellent")
        ]

        for i, (label, value) in enumerate(stats_data):
            stats_layout.addWidget(QLabel(label), i, 0)
            value_label = QLabel(value)
            value_label.setStyleSheet("font-weight: bold; color: #1976D2;")
            self.stats_labels[label] = value_label
            stats_layout.addWidget(value_label, i, 1)

        right_layout.addWidget(stats_group)

        main_layout.addWidget(right_widget)

        # Initialize preview
        self._update_theme_info()
        self._apply_demo_theme(self.app_settings.current_settings["theme"])

        return tab


    def _create_debug_tab(self): #vers 2
        """Create debug settings tab"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # Debug Mode Group
        debug_group = QGroupBox("Debug Mode")
        debug_group.setAutoFillBackground(True)
        debug_layout = QVBoxLayout(debug_group)

        self.debug_enabled_check = QCheckBox("Enable debug mode")
        self.debug_enabled_check.setChecked(
            self.app_settings.current_settings.get('debug_mode', False)
        )
        debug_layout.addWidget(self.debug_enabled_check)

        # Debug Level
        level_layout = QHBoxLayout()
        level_layout.addWidget(QLabel("Debug Level:"))
        self.debug_level_combo = QComboBox()
        self.debug_level_combo.addItems(["ERROR", "WARNING", "INFO", "DEBUG", "VERBOSE"])
        self.debug_level_combo.setCurrentText(
            self.app_settings.current_settings.get('debug_level', 'INFO')
        )
        level_layout.addWidget(self.debug_level_combo)
        level_layout.addStretch()
        debug_layout.addLayout(level_layout)

        layout.addWidget(debug_group)

        # Debug Categories
        categories_group = QGroupBox("Debug Categories")
        categories_group.setAutoFillBackground(True)
        categories_layout = QGridLayout(categories_group)

        self.debug_categories = {}
        default_categories = [
            ('IMG_LOADING', 'IMG file loading'),
            ('TABLE_POPULATION', 'Table population'),
            ('BUTTON_ACTIONS', 'Button actions'),
            ('FILE_OPERATIONS', 'File operations'),
            ('COL_PARSING', 'COL parsing'),
            ('THEME_SYSTEM', 'Theme system')
        ]

        enabled_cats = self.app_settings.current_settings.get('debug_categories', [])
        for i, (cat_id, cat_name) in enumerate(default_categories):
            checkbox = QCheckBox(cat_name)
            checkbox.setChecked(cat_id in enabled_cats)
            self.debug_categories[cat_id] = checkbox
            row = i // 2
            col = i % 2
            categories_layout.addWidget(checkbox, row, col)

        layout.addWidget(categories_group)

        # Clear log button
        clear_btn = QPushButton("Clear Debug Log")
        clear_btn.clicked.connect(self._clear_debug_log)
        layout.addWidget(clear_btn)

        layout.addStretch()
        return widget


    def _create_fonts_tab(self): #vers 3
        """Create fonts settings tab with multiple font type controls"""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel("Configure fonts for different UI elements. Changes are saved to appfactory.settings.json")
        info_label.setWordWrap(True)
        info_label.setStyleSheet("color: palette(mid); font-style: italic; padding: 8px;")
        layout.addWidget(info_label)

        # Scroll area for font groups
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QFrame.Shape.NoFrame)
        scroll_widget = QGroupBox("Fonts  (font, size, weight)")
        scroll_widget.setAutoFillBackground(True)
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(2)

        # Font type configurations
        self.font_controls = {}

        font_types = [
            ("default", "Default Font", "General UI text and labels (was Font Family/Size)", "Segoe UI", 9, 8, 24),
            ("title", "Title Font", "Window titles and main headers", "Arial", 14, 10, 32),
            ("panel", "Panel Headers Font", "Group box titles and section headers", "Arial", 10, 8, 18),
            ("button", "Button Font", "All button text", "Arial", 10, 8, 16),
            ("menu", "Menu Font", "Menu bar and menu items", "Segoe UI", 9, 8, 14),
            ("infobar", "Info Bar Font", "Status bar and info display text", "Courier New", 9, 7, 14),
            ("table", "Table/List Font", "Data tables and list views", "Segoe UI", 9, 7, 14),
            ("tooltip", "Tooltip Font", "Hover tooltip text", "Segoe UI", 8, 7, 12)
        ]

        for font_id, title, description, default_family, default_size, min_size, max_size in font_types:
            group = self._create_font_control_group(
                font_id, title, description,
                default_family, default_size,
                min_size, max_size
            )
            scroll_layout.addWidget(group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        # Action buttons row
        actions_layout = QHBoxLayout()

        reset_fonts_btn = QPushButton("Reset All Fonts to Defaults")
        reset_fonts_btn.clicked.connect(self._reset_all_fonts)
        actions_layout.addWidget(reset_fonts_btn)

        actions_layout.addStretch()

        preview_btn = QPushButton("Preview Font Changes")
        preview_btn.clicked.connect(self._preview_font_changes)
        actions_layout.addWidget(preview_btn)

        layout.addLayout(actions_layout)

        return tab


    def _create_sizes_tab(self): #vers 2
        """Global height rules (heightrules.md) - shared Sizes panel."""
        from apps.methods.ui_sizes import sizes_panel
        panel = sizes_panel(self.app_settings.current_settings.get('ui_sizes', {}), self._on_size_changed)
        self.size_controls = panel.spins
        return panel


    def _create_font_control_group(self, font_id, title, description,
                                default_family, default_size,
                                min_size, max_size): #vers 3
        """One row: Title - [font] [size] [weight]; description as tooltip."""
        group = QWidget()
        group.setToolTip(description)
        controls_layout = QHBoxLayout(group)
        controls_layout.setContentsMargins(4, 1, 4, 1)
        name_lbl = QLabel(f"{title} -")
        name_lbl.setFixedWidth(150)
        name_lbl.setToolTip(description)
        controls_layout.addWidget(name_lbl)

        # Font family
        font_combo = QFontComboBox()

        # Load current font setting from appfactory.settings.json
        current_family = self.app_settings.current_settings.get(
            f'{font_id}_font_family', default_family
        )
        font_combo.setCurrentFont(QFont(current_family))
        font_combo.currentFontChanged.connect(
            lambda f, fid=font_id: self._on_font_changed(fid, 'family', f.family())
        )
        controls_layout.addWidget(font_combo, 1)

        # Font size
        size_spin = QSpinBox()
        size_spin.setRange(min_size, max_size)
        current_size = self.app_settings.current_settings.get(
            f'{font_id}_font_size', default_size
        )
        size_spin.setValue(current_size)
        size_spin.setSuffix(" pt")
        size_spin.setFixedWidth(80)
        size_spin.valueChanged.connect(
            lambda v, fid=font_id: self._on_font_changed(fid, 'size', v)
        )
        controls_layout.addWidget(size_spin)

        # Font weight
        weight_combo = QComboBox()
        weight_combo.addItems(["Normal", "Bold", "Light"])
        current_weight = self.app_settings.current_settings.get(
            f'{font_id}_font_weight', 'Normal'
        )
        weight_combo.setCurrentText(current_weight)
        weight_combo.currentTextChanged.connect(
            lambda w, fid=font_id: self._on_font_changed(fid, 'weight', w)
        )
        weight_combo.setFixedWidth(100)
        controls_layout.addWidget(weight_combo)

        # Store controls for later access
        self.font_controls[font_id] = {
            'family': font_combo,
            'size': size_spin,
            'weight': weight_combo,
            'group': group,
            'default_family': default_family,
            'default_size': default_size
        }

        return group


    def _create_interface_tab(self): #vers 4
        """Create interface settings tab - UI display options"""
        widget = QWidget()
        layout = QVBoxLayout(widget)

        # Button Display Mode
        button_display_group = QGroupBox("Button Display Mode")
        button_display_group.setAutoFillBackground(True)
        button_display_layout = QVBoxLayout(button_display_group)

        self.button_display_combo = QComboBox()
        self.button_display_combo.addItems(["Icons + Text", "Icons Only", "Text Only"])

        # Load current setting
        current_mode = self.app_settings.current_settings.get("button_display_mode", "both")
        mode_map = {"both": 0, "icons": 1, "text": 2}
        self.button_display_combo.setCurrentIndex(mode_map.get(current_mode, 0))

        # Tab content mode
        try:
            tab_mode_row = QHBoxLayout()
            tab_mode_row.addWidget(QLabel("Tab content:"))
            self.tab_content_mode_combo = QComboBox()
            self.tab_content_mode_combo.addItems(["Icon + Text", "Icon Only", "Text Only"])
            tab_mode_rev = {"both": 0, "icon": 1, "text": 2}
            _tab_mode = self.app_settings.current_settings.get("tab_content_mode", "both")
            self.tab_content_mode_combo.setCurrentIndex(tab_mode_rev.get(_tab_mode, 0))
            tab_mode_row.addWidget(self.tab_content_mode_combo)
            tab_mode_row.addStretch()
        except Exception:
            self.tab_content_mode_combo = None

        button_display_layout.addWidget(self.button_display_combo)

        hint_label = QLabel("Controls how toolbar buttons are displayed")
        hint_label.setStyleSheet("color: #888; font-style: italic;")
        button_display_layout.addWidget(hint_label)

        layout.addWidget(button_display_group)

        # Window Controls
        window_group = QGroupBox("Window Controls")
        window_group.setAutoFillBackground(True)
        window_layout = QVBoxLayout(window_group)

        self.custom_gadgets_check = QCheckBox("Use custom window gadgets (TXD Workshop style)")
        self.custom_gadgets_check.setChecked(
            self.app_settings.current_settings.get("use_custom_gadgets", False)
        )
        window_layout.addWidget(self.custom_gadgets_check)

        self.corner_resize_check = QCheckBox("Enable corner resize (all 4 corners)")
        self.corner_resize_check.setChecked(
            self.app_settings.current_settings.get("enable_corner_resize", True)
        )
        window_layout.addWidget(self.corner_resize_check)

        gadget_hint = QLabel("Custom gadgets disable system title bar. Corner resize works in both modes.")
        gadget_hint.setStyleSheet("color: #888; font-style: italic;")
        gadget_hint.setWordWrap(True)
        window_layout.addWidget(gadget_hint)

        layout.addWidget(window_group)

        # Interface Options
        interface_group = QGroupBox("Interface Options")
        interface_group.setAutoFillBackground(True)
        interface_layout = QVBoxLayout(interface_group)

        self.tooltips_check = QCheckBox("Show tooltips")
        interface_layout.addWidget(self.tooltips_check)

        self.menu_icons_check = QCheckBox("Show menu icons")
        interface_layout.addWidget(self.menu_icons_check)

        self.use_svg_icons_check = QCheckBox("Use SVG icons (never emojis)")
        self.use_svg_icons_check.setChecked(
            self.app_settings.current_settings.get("use_svg_icons", True)
        )
        interface_layout.addWidget(self.use_svg_icons_check)

        layout.addWidget(interface_group)

        # Pin Warnings
        pin_warn_group = QGroupBox("Pinned Entry Warnings")
        pin_warn_group.setAutoFillBackground(True)
        pin_warn_layout = QVBoxLayout(pin_warn_group)

        self.pin_warn_popup_check = QCheckBox("Show popup when a pinned entry is protected")
        self.pin_warn_popup_check.setChecked(
            self.app_settings.current_settings.get("pin_warn_popup", True))
        pin_warn_layout.addWidget(self.pin_warn_popup_check)

        self.pin_warn_log_check = QCheckBox("Log pin protection messages to activity log")
        self.pin_warn_log_check.setChecked(
            self.app_settings.current_settings.get("pin_warn_log", True))
        pin_warn_layout.addWidget(self.pin_warn_log_check)

        pin_hint = QLabel("Applies to: remove, rename, replace, move, import.")
        pin_hint.setStyleSheet("color: #888; font-style: italic;")
        pin_warn_layout.addWidget(pin_hint)

        layout.addWidget(pin_warn_group)

        # Rename Notifications
        rename_notify_group = QGroupBox("Rename Notifications")
        rename_notify_group.setAutoFillBackground(True)
        rename_notify_layout = QVBoxLayout(rename_notify_group)

        self.rename_notify_popup_check = QCheckBox("Show popup on successful rename")
        self.rename_notify_popup_check.setChecked(
            self.app_settings.current_settings.get("rename_notify_popup", True))
        rename_notify_layout.addWidget(self.rename_notify_popup_check)

        self.rename_notify_log_check = QCheckBox("Log rename to activity log")
        self.rename_notify_log_check.setChecked(
            self.app_settings.current_settings.get("rename_notify_log", True))
        rename_notify_layout.addWidget(self.rename_notify_log_check)

        layout.addWidget(rename_notify_group)
        layout.addStretch()

        return widget


    def _create_panels_tab(self): #vers 4
        """Panels tab: four live previews stacked on the left; background image,
        transparency and gadgets settings on the right.
        Fill / Gradient / Pattern colours are in the Colors tab."""
        from apps.utils.app_settings_system import PanelPreviewWidget
        from PyQt6.QtWidgets import QScrollArea, QGridLayout

        tab = QWidget()
        root = QHBoxLayout(tab)
        root.setContentsMargins(4, 4, 4, 4)
        cs = self.app_settings.current_settings

        #    Left: previews
        prev_col = QWidget()
        prev_col.setFixedWidth(270)
        pl = QVBoxLayout(prev_col); pl.setContentsMargins(0, 0, 0, 0); pl.setSpacing(2)
        eff = {"fill": "fill", "gradient": "gradient", "pattern": "pattern"}.get(
            cs.get("panel_effect_type", "none"), "fill")
        self._img_preview = PanelPreviewWidget(self, "image")
        self._effect_preview = PanelPreviewWidget(self, eff)
        self._trans_preview = PanelPreviewWidget(self, "transparency")
        self._hero_side_preview = PanelPreviewWidget(self, "hero")
        for title, pv in (("Image", self._img_preview), ("Colour effect", self._effect_preview),
                          ("Transparency", self._trans_preview), ("Hero banner", self._hero_side_preview)):
            t = QLabel(title); t.setStyleSheet("font-size: 8pt;")
            pv.setMinimumHeight(70)
            pl.addWidget(t)
            pl.addWidget(pv, 1)
        root.addWidget(prev_col)

        #    Right: settings
        side = QTabWidget()
        bg_ctrl = QWidget()
        il = QVBoxLayout(bg_ctrl)
        il.setSpacing(6)

        img_group = QGroupBox("Panel Background Image")
        img_group.setAutoFillBackground(True)
        igl = QGridLayout(img_group)
        igl.setColumnStretch(1, 1)

        self._panel_img_path = QLineEdit()
        self._panel_img_path.setPlaceholderText("Path to image file…")
        self._panel_img_path.setText(cs.get("panel_bg_image", ""))
        browse_btn = QPushButton("Browse…")
        browse_btn.clicked.connect(self._browse_panel_bg_image)
        clear_btn = QPushButton("Clear")
        clear_btn.clicked.connect(lambda: self._panel_img_path.clear())
        path_lay = QHBoxLayout()
        path_lay.addWidget(self._panel_img_path, 1)
        path_lay.addWidget(browse_btn)
        path_lay.addWidget(clear_btn)
        igl.addWidget(QLabel("Image:"), 0, 0)
        igl.addLayout(path_lay, 0, 1, 1, 2)

        self._panel_img_mode = QComboBox()
        self._panel_img_mode.addItems([
            "Tiled", "Stretched", "Centred", "Scaled fit", "Scaled fill", "Across window"])
        self._panel_img_mode.setCurrentIndex(cs.get("panel_bg_image_mode", 0))
        igl.addWidget(QLabel("Display:"), 1, 0)
        igl.addWidget(self._panel_img_mode, 1, 1)

        self._panel_img_opacity = QSlider(Qt.Orientation.Horizontal)
        self._panel_img_opacity.setRange(0, 100)
        self._panel_img_opacity.setValue(cs.get("panel_bg_image_opacity", 100))
        self._panel_img_opacity_lbl = QLabel(f"{self._panel_img_opacity.value()}%")
        self._panel_img_opacity_lbl.setFixedWidth(40)
        self._panel_img_opacity.valueChanged.connect(
            lambda v: self._panel_img_opacity_lbl.setText(f"{v}%"))
        self._panel_img_blend_lbl = QLabel("Blend opacity:")
        igl.addWidget(self._panel_img_blend_lbl, 2, 0)
        igl.addWidget(self._panel_img_opacity, 2, 1)
        igl.addWidget(self._panel_img_opacity_lbl, 2, 2)

        self._panel_img_all = QCheckBox("Show through all panels, ribbons, tabs and splitters (not 3D views)")
        self._panel_img_all.setToolTip("Tint strength per part: Transparency below (Panels, Widgets / Ribbons, Tabs, Splitters)")
        self._panel_img_all.setChecked(bool(cs.get("panel_bg_image_all", False)))
        self._panel_img_all.toggled.connect(
            lambda on: self.app_settings.current_settings.__setitem__("panel_bg_image_all", on))
        igl.addWidget(self._panel_img_all, 3, 0, 1, 3)
        self._panel_img_titlebar = QComboBox()
        self._panel_img_titlebar.addItems(["Panels and titlebar", "Panels only (not titlebar)", "Titlebar only"])
        self._panel_img_titlebar.setCurrentIndex(int(cs.get("image_titlebar", 0)))
        self._panel_img_titlebar.currentIndexChanged.connect(
            lambda i: self.app_settings.current_settings.__setitem__("image_titlebar", i))
        igl.addWidget(QLabel("Titlebar:"), 4, 0)
        igl.addWidget(self._panel_img_titlebar, 4, 1)
        il.addWidget(img_group)

        def _img_set():  #vers 1
            """Image-only controls follow whether an image is chosen."""
            has = bool(self._panel_img_path.text().strip())
            for w in (self._panel_img_mode, self._panel_img_opacity, self._panel_img_opacity_lbl,
                      self._panel_img_blend_lbl, self._panel_img_all):
                w.setEnabled(has)
        self._panel_img_path.textChanged.connect(
            lambda t: [self.app_settings.current_settings.__setitem__("panel_bg_image", t),
                       _img_set(), self._img_preview.refresh()])
        self._panel_img_mode.currentIndexChanged.connect(
            lambda i: [self.app_settings.current_settings.__setitem__("panel_bg_image_mode", i),
                       self._img_preview.refresh()])
        self._panel_img_opacity.valueChanged.connect(
            lambda v: [self.app_settings.current_settings.__setitem__("panel_bg_image_opacity", v),
                       self._img_preview.refresh()])
        _img_set()

        trans_group = QGroupBox("Transparency (opacity)")
        trans_group.setAutoFillBackground(True)
        tgl = QGridLayout(trans_group)
        tgl.setColumnStretch(1, 1)
        for r, (label, key, default) in enumerate([
                ("Titlebar", "titlebar_opacity", 100), ("Panels", "panel_opacity", 100),
                ("Buttons", "button_opacity", 100), ("Widgets / Ribbons", "widget_opacity", 100),
                ("Tabs", "tabs_opacity", 100), ("Splitters", "splitter_opacity", 100)]):
            sl = QSlider(Qt.Orientation.Horizontal)
            sl.setRange(0, 100)
            sl.setValue(cs.get(key, default))
            lbl = QLabel(f"{sl.value()}%")
            lbl.setFixedWidth(40)
            sl.valueChanged.connect(lambda v, l=lbl, k=key: [
                l.setText(f"{v}%"),
                self.app_settings.current_settings.__setitem__(k, v),
                self._trans_preview.refresh()])
            tgl.addWidget(QLabel(f"{label}:"), r, 0)
            tgl.addWidget(sl, r, 1)
            tgl.addWidget(lbl, r, 2)
            setattr(self, f"_{key}_slider", sl)
        self._see_through_chk = QCheckBox("See-through windows when no image is set (needs a compositor)")
        self._see_through_chk.setToolTip("Desktop shows through panels, ribbons, tabs, splitters and buttons "
                                         "at the opacities above; 3D views stay solid")
        self._see_through_chk.setChecked(bool(cs.get("see_through", False)))
        self._see_through_chk.toggled.connect(
            lambda on: self.app_settings.current_settings.__setitem__("see_through", on))
        tgl.addWidget(self._see_through_chk, 6, 0, 1, 3)
        self._viewport_solid_chk = QCheckBox("Keep viewports solid (no bleed-through)")
        self._viewport_solid_chk.setToolTip("Off: 2D canvases and previews also show the image / desktop; "
                                            "3D views always stay solid")
        self._viewport_solid_chk.setChecked(bool(cs.get("viewport_solid", True)))
        self._viewport_solid_chk.toggled.connect(
            lambda on: self.app_settings.current_settings.__setitem__("viewport_solid", on))
        tgl.addWidget(self._viewport_solid_chk, 7, 0, 1, 3)
        il.addWidget(trans_group)
        il.addStretch()

        bg_scroll = QScrollArea(); bg_scroll.setWidgetResizable(True)
        bg_scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        bg_scroll.setWidget(bg_ctrl)
        side.addTab(bg_scroll, "Background")
        side.addTab(self._create_advanced_gadgets_tab(), "Gadgets")
        root.addWidget(side, 1)
        return tab


    def _create_ui_management_tab_v2(self): #vers 3
        """UI Management tab — existing components + Progress Bar styles."""
        from PyQt6.QtWidgets import QScrollArea

        tab = QWidget()
        root = QVBoxLayout(tab)

        sub = QTabWidget()

        #   Existing UI Management content in a scroll
        orig = self._create_ui_management_tab()
        sub.addTab(orig, "Components")

        #   Progress Bar styles
        pb_tab = QWidget()
        pl = QVBoxLayout(pb_tab)

        style_group = QGroupBox("Progress Bar")
        style_group.setAutoFillBackground(True)
        sgl = QGridLayout(style_group)
        sgl.setHorizontalSpacing(8)

        PB_STYLES = [
            ("system",      "System default"),
            ("flat",        "Flat — solid colour fill"),
            ("gradient_h",  "Gradient — Left → Right"),
            ("gradient_v",  "Gradient — Top → Bottom"),
            ("banded",      "Banded — pulsing highlight bands"),
            ("amiga",       "Amiga — chunky block segments"),
            ("glow",        "Glow — centre-bright fill"),
            ("striped",     "Striped — diagonal stripes"),
        ]

        current_pb = self.app_settings.current_settings.get("progressbar_style", "system")
        self._pb_style_combo = QComboBox()
        for key, label in PB_STYLES:
            self._pb_style_combo.addItem(label, key)
        self._pb_style_combo.setCurrentIndex(max(0, self._pb_style_combo.findData(current_pb)))
        self._pb_style_combo.currentIndexChanged.connect(
            lambda i: self.app_settings.current_settings.__setitem__(
                "progressbar_style", self._pb_style_combo.itemData(i)))
        sgl.addWidget(QLabel("Style -"), 0, 0)
        sgl.addWidget(self._pb_style_combo, 0, 1, 1, 6)

        sgl.addWidget(QLabel("Colours -"), 1, 0)
        for k, (label, key, default) in enumerate([
            ("Fill colour",       "progressbar_fill",       "#4a7a9b"),
            ("Background colour", "progressbar_bg",         "#1a1a2e"),
            ("Text colour",       "progressbar_text",       "#ffffff"),
            ("Stripe colour",     "progressbar_stripe",     "#5a9abf"),
        ]):
            btn = QPushButton()
            btn.setFixedSize(40, 22)
            val = self.app_settings.current_settings.get(key, default)
            btn.setStyleSheet(f"background:{val};")
            btn.setToolTip(f"{label}: click to pick")
            btn.clicked.connect(lambda _, k=key, b=btn: self._pick_panel_colour(k, b))
            cell = QHBoxLayout()
            cell.addWidget(QLabel(label))
            cell.addWidget(btn)
            sgl.addLayout(cell, 1 + k // 2, 1 + (k % 2) * 3, 1, 3)
        sgl.setColumnStretch(7, 1)

        hgl = QHBoxLayout()
        sgl.addWidget(QLabel("Height -"), 3, 0)
        sgl.addLayout(hgl, 3, 1, 1, 7)
        pl.addWidget(style_group)
        self._pb_height_slider = QSlider(Qt.Orientation.Horizontal)
        self._pb_height_slider.setRange(8, 32)
        self._pb_height_slider.setValue(
            self.app_settings.current_settings.get("progressbar_height", 18))
        self._pb_height_lbl = QLabel(f"{self._pb_height_slider.value()}px")
        self._pb_height_slider.valueChanged.connect(
            lambda v: self._pb_height_lbl.setText(f"{v}px"))
        hgl.addWidget(self._pb_height_slider)
        hgl.addWidget(self._pb_height_lbl)
        pl.addStretch()

        sub.addTab(pb_tab, "Progress Bars")

        root.addWidget(sub)
        return tab


    def _create_localisation_tab(self): #vers 1
        """Localisation tab — locale detection, language, date/number formats."""
        import locale as _locale
        from PyQt6.QtWidgets import QScrollArea

        tab = QWidget()
        root = QVBoxLayout(tab)
        root.setSpacing(8)

        #   Auto-detect
        detect_group = QGroupBox("System Locale Detection")
        detect_group.setAutoFillBackground(True)
        dgl = QVBoxLayout(detect_group)

        self._locale_auto_cb = QCheckBox("Auto-detect locale from system on startup")
        self._locale_auto_cb.setChecked(
            self.app_settings.current_settings.get("locale_auto_detect", True))
        dgl.addWidget(self._locale_auto_cb)

        try:
            import sys as _sys
            if _sys.version_info >= (3, 15):
                sys_locale = _locale.getlocale()[0] or "Unknown"
            else:
                import warnings
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore", DeprecationWarning)
                    sys_locale = _locale.getdefaultlocale()[0] or "Unknown"
        except Exception:
            sys_locale = "Unknown"
        dgl.addWidget(QLabel(f"Detected system locale:  <b>{sys_locale}</b>"))
        root.addWidget(detect_group)

        #   Language
        lang_group = QGroupBox("Language")
        lang_group.setAutoFillBackground(True)
        lgl = QGridLayout(lang_group)

        LANGUAGES = [
            ("en_GB", "English (UK)"),
            ("en_US", "English (US)"),
            ("de_DE", "Deutsch"),
            ("fr_FR", "Français"),
            ("es_ES", "Español"),
            ("it_IT", "Italiano"),
            ("pt_PT", "Português"),
            ("nl_NL", "Nederlands"),
            ("pl_PL", "Polski"),
            ("ru_RU", "Русский"),
            ("ja_JP", "日本語"),
            ("zh_CN", "中文 (简体)"),
        ]

        lgl.addWidget(QLabel("Application language:"), 0, 0)
        self._lang_combo = QComboBox()
        for code, name in LANGUAGES:
            self._lang_combo.addItem(name, code)
        current_lang = self.app_settings.current_settings.get("language", "en_GB")
        for i, (code, _) in enumerate(LANGUAGES):
            if code == current_lang:
                self._lang_combo.setCurrentIndex(i)
                break
        lgl.addWidget(self._lang_combo, 0, 1)

        note = QLabel("Note: Language change takes effect on next launch. "
                      "Translations are community-contributed — coverage varies.")
        note.setWordWrap(True)
        note.setStyleSheet("color: palette(mid); font-size: 8pt;")
        lgl.addWidget(note, 1, 0, 1, 2)
        root.addWidget(lang_group)

        #   Date & Number formats
        fmt_group = QGroupBox("Date & Number Formats")
        fmt_group.setAutoFillBackground(True)
        fgl = QGridLayout(fmt_group)

        fgl.addWidget(QLabel("Date format:"), 0, 0)
        self._date_fmt_combo = QComboBox()
        self._date_fmt_combo.addItems([
            "DD/MM/YYYY  (UK/EU)",
            "MM/DD/YYYY  (US)",
            "YYYY-MM-DD  (ISO 8601)",
            "DD.MM.YYYY  (DE/RU)",
        ])
        self._date_fmt_combo.setCurrentIndex(
            self.app_settings.current_settings.get("date_format_idx", 0))
        fgl.addWidget(self._date_fmt_combo, 0, 1)

        fgl.addWidget(QLabel("Number format:"), 1, 0)
        self._num_fmt_combo = QComboBox()
        self._num_fmt_combo.addItems([
            "1,234.56  (UK/US — comma thousands, dot decimal)",
            "1.234,56  (EU — dot thousands, comma decimal)",
            "1 234,56  (FR/RU — space thousands, comma decimal)",
            "1234.56   (No separator)",
        ])
        self._num_fmt_combo.setCurrentIndex(
            self.app_settings.current_settings.get("number_format_idx", 0))
        fgl.addWidget(self._num_fmt_combo, 1, 1)
        root.addWidget(fmt_group)

        #   Per-app overrides
        override_group = QGroupBox("Per-Workshop Language Override")
        override_group.setAutoFillBackground(True)
        ogl = QVBoxLayout(override_group)
        ogl.addWidget(QLabel(
            "Future: each workshop can use a different language file.\n"
            "Leave blank to inherit the application language above."))

        WORKSHOPS = ["IMG Factory", "TXD Workshop", "COL Workshop",
                     "Model Workshop", "DP5 Workshop", "Radar Workshop"]
        self._workshop_lang_combos = {}
        for ws in WORKSHOPS:
            row_lay = QHBoxLayout()
            row_lay.addWidget(QLabel(f"{ws}:"))
            cb = QComboBox()
            cb.addItem("— Inherit —", "")
            for code, name in LANGUAGES:
                cb.addItem(name, code)
            self._workshop_lang_combos[ws] = cb
            row_lay.addWidget(cb)
            ogl.addLayout(row_lay)
        root.addWidget(override_group)

        root.addStretch()
        return tab


    def _create_background_tab(self): #vers 1
        """Create background settings tab - Image backgrounds for panels, buttons, etc."""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Background Images:</b><br>"
            "Set background images for various UI elements with positioning options."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # Panel Background
        panel_group = QGroupBox("Panel Background")
        panel_group.setAutoFillBackground(True)
        panel_layout = QVBoxLayout(panel_group)

        # Panel Background Image
        panel_bg_layout = QHBoxLayout()
        panel_bg_layout.addWidget(QLabel("Image Path:"))
        self.panel_bg_path = QLineEdit()
        self.panel_bg_path.setPlaceholderText("Path to background image")
        panel_bg_layout.addWidget(self.panel_bg_path)

        browse_panel_btn = QPushButton("Browse")
        browse_panel_btn.clicked.connect(lambda: self._browse_background_image("panel"))
        panel_bg_layout.addWidget(browse_panel_btn)

        clear_panel_btn = QPushButton("Clear")
        clear_panel_btn.clicked.connect(lambda: self._clear_background_image("panel"))
        panel_bg_layout.addWidget(clear_panel_btn)

        panel_layout.addLayout(panel_bg_layout)

        # Panel Background Mode
        panel_mode_layout = QHBoxLayout()
        panel_mode_layout.addWidget(QLabel("Display Mode:"))
        self.panel_bg_mode = QComboBox()
        self.panel_bg_mode.addItems(["Tiled", "Stretched", "Shaded"])
        panel_mode_layout.addWidget(self.panel_bg_mode)
        panel_mode_layout.addStretch()
        panel_layout.addLayout(panel_mode_layout)

        scroll_layout.addWidget(panel_group)

        # Primary Background
        primary_group = QGroupBox("Primary Background")
        primary_group.setAutoFillBackground(True)
        primary_layout = QVBoxLayout(primary_group)

        # Primary Background Image
        primary_bg_layout = QHBoxLayout()
        primary_bg_layout.addWidget(QLabel("Image Path:"))
        self.primary_bg_path = QLineEdit()
        self.primary_bg_path.setPlaceholderText("Path to primary background image")
        primary_bg_layout.addWidget(self.primary_bg_path)

        browse_primary_btn = QPushButton("Browse")
        browse_primary_btn.clicked.connect(lambda: self._browse_background_image("primary"))
        primary_bg_layout.addWidget(browse_primary_btn)

        clear_primary_btn = QPushButton("Clear")
        clear_primary_btn.clicked.connect(lambda: self._clear_background_image("primary"))
        primary_bg_layout.addWidget(clear_primary_btn)

        primary_layout.addLayout(primary_bg_layout)

        # Primary Background Mode
        primary_mode_layout = QHBoxLayout()
        primary_mode_layout.addWidget(QLabel("Display Mode:"))
        self.primary_bg_mode = QComboBox()
        self.primary_bg_mode.addItems(["Tiled", "Stretched", "Shaded"])
        primary_mode_layout.addWidget(self.primary_bg_mode)
        primary_mode_layout.addStretch()
        primary_layout.addLayout(primary_mode_layout)

        scroll_layout.addWidget(primary_group)

        # Secondary Background
        secondary_group = QGroupBox("Secondary Background")
        secondary_group.setAutoFillBackground(True)
        secondary_layout = QVBoxLayout(secondary_group)

        # Secondary Background Image
        secondary_bg_layout = QHBoxLayout()
        secondary_bg_layout.addWidget(QLabel("Image Path:"))
        self.secondary_bg_path = QLineEdit()
        self.secondary_bg_path.setPlaceholderText("Path to secondary background image")
        secondary_bg_layout.addWidget(self.secondary_bg_path)

        browse_secondary_btn = QPushButton("Browse")
        browse_secondary_btn.clicked.connect(lambda: self._browse_background_image("secondary"))
        secondary_bg_layout.addWidget(browse_secondary_btn)

        clear_secondary_btn = QPushButton("Clear")
        clear_secondary_btn.clicked.connect(lambda: self._clear_background_image("secondary"))
        secondary_bg_layout.addWidget(clear_secondary_btn)

        secondary_layout.addLayout(secondary_bg_layout)

        # Secondary Background Mode
        secondary_mode_layout = QHBoxLayout()
        secondary_mode_layout.addWidget(QLabel("Display Mode:"))
        self.secondary_bg_mode = QComboBox()
        self.secondary_bg_mode.addItems(["Tiled", "Stretched", "Shaded"])
        secondary_mode_layout.addWidget(self.secondary_bg_mode)
        secondary_mode_layout.addStretch()
        secondary_layout.addLayout(secondary_mode_layout)

        scroll_layout.addWidget(secondary_group)

        # Button Background
        button_group = QGroupBox("Button Background")
        button_group.setAutoFillBackground(True)
        button_layout = QVBoxLayout(button_group)

        # Button Background Image
        button_bg_layout = QHBoxLayout()
        button_bg_layout.addWidget(QLabel("Image Path:"))
        self.button_bg_path = QLineEdit()
        self.button_bg_path.setPlaceholderText("Path to button background image")
        button_bg_layout.addWidget(self.button_bg_path)

        browse_button_btn = QPushButton("Browse")
        browse_button_btn.clicked.connect(lambda: self._browse_background_image("button"))
        button_bg_layout.addWidget(browse_button_btn)

        clear_button_btn = QPushButton("Clear")
        clear_button_btn.clicked.connect(lambda: self._clear_background_image("button"))
        button_bg_layout.addWidget(clear_button_btn)

        button_layout.addLayout(button_bg_layout)

        # Button Background Mode
        button_mode_layout = QHBoxLayout()
        button_mode_layout.addWidget(QLabel("Display Mode:"))
        self.button_bg_mode = QComboBox()
        self.button_bg_mode.addItems(["Tiled", "Stretched", "Shaded"])
        button_mode_layout.addWidget(self.button_bg_mode)
        button_mode_layout.addStretch()
        button_layout.addLayout(button_mode_layout)

        scroll_layout.addWidget(button_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab


    def _create_transparency_tab(self): #vers 1
        """Create transparency settings tab - Opacity controls for UI elements."""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Transparency Settings:</b><br>"
            "Configure opacity levels for various UI elements (0.0 = fully transparent, 1.0 = fully opaque)."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # Titlebar Transparency
        titlebar_group = QGroupBox("Titlebar Transparency")
        titlebar_group.setAutoFillBackground(True)
        titlebar_layout = QVBoxLayout(titlebar_group)

        # Titlebar Opacity
        titlebar_opacity_layout = QHBoxLayout()
        titlebar_opacity_layout.addWidget(QLabel("Opacity:"))
        self.titlebar_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.titlebar_opacity_slider.setRange(0, 100)
        self.titlebar_opacity_slider.setValue(100)  # Fully opaque by default
        self.titlebar_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.titlebar_opacity_slider.setTickInterval(10)
        titlebar_opacity_layout.addWidget(self.titlebar_opacity_slider)
        self.titlebar_opacity_label = QLabel("1.0")
        self.titlebar_opacity_label.setFixedWidth(40)
        titlebar_opacity_layout.addWidget(self.titlebar_opacity_label)
        titlebar_layout.addLayout(titlebar_opacity_layout)

        self.titlebar_opacity_slider.valueChanged.connect(
            lambda v: self.titlebar_opacity_label.setText(f"{v/100:.1f}")
        )

        scroll_layout.addWidget(titlebar_group)

        # Panel Transparency
        panel_group = QGroupBox("Panel Transparency")
        panel_group.setAutoFillBackground(True)
        panel_layout = QVBoxLayout(panel_group)

        # Panel Opacity
        panel_opacity_layout = QHBoxLayout()
        panel_opacity_layout.addWidget(QLabel("Opacity:"))
        self.panel_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.panel_opacity_slider.setRange(0, 100)
        self.panel_opacity_slider.setValue(100)  # Fully opaque by default
        self.panel_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_opacity_slider.setTickInterval(10)
        panel_opacity_layout.addWidget(self.panel_opacity_slider)
        self.panel_opacity_label = QLabel("1.0")
        self.panel_opacity_label.setFixedWidth(40)
        panel_opacity_layout.addWidget(self.panel_opacity_label)
        panel_layout.addLayout(panel_opacity_layout)

        self.panel_opacity_slider.valueChanged.connect(
            lambda v: self.panel_opacity_label.setText(f"{v/100:.1f}")
        )

        scroll_layout.addWidget(panel_group)

        # Button Transparency
        button_group = QGroupBox("Button Transparency")
        button_group.setAutoFillBackground(True)
        button_layout = QVBoxLayout(button_group)

        # Button Opacity
        button_opacity_layout = QHBoxLayout()
        button_opacity_layout.addWidget(QLabel("Opacity:"))
        self.button_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.button_opacity_slider.setRange(0, 100)
        self.button_opacity_slider.setValue(100)  # Fully opaque by default
        self.button_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_opacity_slider.setTickInterval(10)
        button_opacity_layout.addWidget(self.button_opacity_slider)
        self.button_opacity_label = QLabel("1.0")
        self.button_opacity_label.setFixedWidth(40)
        button_opacity_layout.addWidget(self.button_opacity_label)
        button_layout.addLayout(button_opacity_layout)

        self.button_opacity_slider.valueChanged.connect(
            lambda v: self.button_opacity_label.setText(f"{v/100:.1f}")
        )

        scroll_layout.addWidget(button_group)

        # Widget Transparency
        widget_group = QGroupBox("Widget Transparency")
        widget_group.setAutoFillBackground(True)
        widget_layout = QVBoxLayout(widget_group)

        # Widget Opacity
        widget_opacity_layout = QHBoxLayout()
        widget_opacity_layout.addWidget(QLabel("Opacity:"))
        self.widget_opacity_slider = QSlider(Qt.Orientation.Horizontal)
        self.widget_opacity_slider.setRange(0, 100)
        self.widget_opacity_slider.setValue(100)  # Fully opaque by default
        self.widget_opacity_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.widget_opacity_slider.setTickInterval(10)
        widget_opacity_layout.addWidget(self.widget_opacity_slider)
        self.widget_opacity_label = QLabel("1.0")
        self.widget_opacity_label.setFixedWidth(40)
        widget_opacity_layout.addWidget(self.widget_opacity_label)
        widget_layout.addLayout(widget_opacity_layout)

        self.widget_opacity_slider.valueChanged.connect(
            lambda v: self.widget_opacity_label.setText(f"{v/100:.1f}")
        )

        scroll_layout.addWidget(widget_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab


    def _create_shadows_tab(self): #vers 1
        """Create shadow effects settings tab - Depth and shadow controls for UI elements."""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Shadow Effects:</b><br>"
            "Configure shadow depth and appearance for buttons, panels, and other UI elements."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # Button Shadow Settings
        button_shadow_group = QGroupBox("Button Shadows")
        button_shadow_group.setAutoFillBackground(True)
        button_shadow_layout = QVBoxLayout(button_shadow_group)

        # Shadow Depth
        shadow_depth_layout = QHBoxLayout()
        shadow_depth_layout.addWidget(QLabel("Shadow Depth:"))
        self.button_shadow_depth = QSlider(Qt.Orientation.Horizontal)
        self.button_shadow_depth.setRange(0, 20)
        self.button_shadow_depth.setValue(3)
        self.button_shadow_depth.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_shadow_depth.setTickInterval(5)
        shadow_depth_layout.addWidget(self.button_shadow_depth)
        self.button_shadow_depth_label = QLabel("3px")
        self.button_shadow_depth_label.setFixedWidth(40)
        shadow_depth_layout.addWidget(self.button_shadow_depth_label)
        button_shadow_layout.addLayout(shadow_depth_layout)

        self.button_shadow_depth.valueChanged.connect(
            lambda v: self.button_shadow_depth_label.setText(f"{v}px")
        )

        # Shadow Blur
        shadow_blur_layout = QHBoxLayout()
        shadow_blur_layout.addWidget(QLabel("Shadow Blur:"))
        self.button_shadow_blur = QSlider(Qt.Orientation.Horizontal)
        self.button_shadow_blur.setRange(0, 20)
        self.button_shadow_blur.setValue(5)
        self.button_shadow_blur.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_shadow_blur.setTickInterval(5)
        shadow_blur_layout.addWidget(self.button_shadow_blur)
        self.button_shadow_blur_label = QLabel("5px")
        self.button_shadow_blur_label.setFixedWidth(40)
        shadow_blur_layout.addWidget(self.button_shadow_blur_label)
        button_shadow_layout.addLayout(shadow_blur_layout)

        self.button_shadow_blur.valueChanged.connect(
            lambda v: self.button_shadow_blur_label.setText(f"{v}px")
        )

        # Shadow Opacity
        shadow_opacity_layout = QHBoxLayout()
        shadow_opacity_layout.addWidget(QLabel("Shadow Opacity:"))
        self.button_shadow_opacity = QSlider(Qt.Orientation.Horizontal)
        self.button_shadow_opacity.setRange(0, 100)
        self.button_shadow_opacity.setValue(30)
        self.button_shadow_opacity.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.button_shadow_opacity.setTickInterval(25)
        shadow_opacity_layout.addWidget(self.button_shadow_opacity)
        self.button_shadow_opacity_label = QLabel("30%")
        self.button_shadow_opacity_label.setFixedWidth(40)
        shadow_opacity_layout.addWidget(self.button_shadow_opacity_label)
        button_shadow_layout.addLayout(shadow_opacity_layout)

        self.button_shadow_opacity.valueChanged.connect(
            lambda v: self.button_shadow_opacity_label.setText(f"{v}%")
        )

        scroll_layout.addWidget(button_shadow_group)

        # Panel Shadow Settings
        panel_shadow_group = QGroupBox("Panel Shadows")
        panel_shadow_group.setAutoFillBackground(True)
        panel_shadow_layout = QVBoxLayout(panel_shadow_group)

        # Shadow Depth
        panel_shadow_depth_layout = QHBoxLayout()
        panel_shadow_depth_layout.addWidget(QLabel("Shadow Depth:"))
        self.panel_shadow_depth = QSlider(Qt.Orientation.Horizontal)
        self.panel_shadow_depth.setRange(0, 20)
        self.panel_shadow_depth.setValue(5)
        self.panel_shadow_depth.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_shadow_depth.setTickInterval(5)
        panel_shadow_depth_layout.addWidget(self.panel_shadow_depth)
        self.panel_shadow_depth_label = QLabel("5px")
        self.panel_shadow_depth_label.setFixedWidth(40)
        panel_shadow_depth_layout.addWidget(self.panel_shadow_depth_label)
        panel_shadow_layout.addLayout(panel_shadow_depth_layout)

        self.panel_shadow_depth.valueChanged.connect(
            lambda v: self.panel_shadow_depth_label.setText(f"{v}px")
        )

        # Shadow Blur
        panel_shadow_blur_layout = QHBoxLayout()
        panel_shadow_blur_layout.addWidget(QLabel("Shadow Blur:"))
        self.panel_shadow_blur = QSlider(Qt.Orientation.Horizontal)
        self.panel_shadow_blur.setRange(0, 20)
        self.panel_shadow_blur.setValue(8)
        self.panel_shadow_blur.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_shadow_blur.setTickInterval(5)
        panel_shadow_blur_layout.addWidget(self.panel_shadow_blur)
        self.panel_shadow_blur_label = QLabel("8px")
        self.panel_shadow_blur_label.setFixedWidth(40)
        panel_shadow_blur_layout.addWidget(self.panel_shadow_blur_label)
        panel_shadow_layout.addLayout(panel_shadow_blur_layout)

        self.panel_shadow_blur.valueChanged.connect(
            lambda v: self.panel_shadow_blur_label.setText(f"{v}px")
        )

        # Shadow Opacity
        panel_shadow_opacity_layout = QHBoxLayout()
        panel_shadow_opacity_layout.addWidget(QLabel("Shadow Opacity:"))
        self.panel_shadow_opacity = QSlider(Qt.Orientation.Horizontal)
        self.panel_shadow_opacity.setRange(0, 100)
        self.panel_shadow_opacity.setValue(25)
        self.panel_shadow_opacity.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.panel_shadow_opacity.setTickInterval(25)
        panel_shadow_opacity_layout.addWidget(self.panel_shadow_opacity)
        self.panel_shadow_opacity_label = QLabel("25%")
        self.panel_shadow_opacity_label.setFixedWidth(40)
        panel_shadow_opacity_layout.addWidget(self.panel_shadow_opacity_label)
        panel_shadow_layout.addLayout(panel_shadow_opacity_layout)

        self.panel_shadow_opacity.valueChanged.connect(
            lambda v: self.panel_shadow_opacity_label.setText(f"{v}%")
        )

        scroll_layout.addWidget(panel_shadow_group)

        # General Shadow Settings
        general_shadow_group = QGroupBox("General Shadow Settings")
        general_shadow_group.setAutoFillBackground(True)
        general_shadow_layout = QVBoxLayout(general_shadow_group)

        # Enable Shadows
        self.enable_shadows_check = QCheckBox("Enable shadows for all UI elements")
        self.enable_shadows_check.setChecked(True)
        general_shadow_layout.addWidget(self.enable_shadows_check)

        # Shadow Color
        shadow_color_layout = QHBoxLayout()
        shadow_color_layout.addWidget(QLabel("Shadow Color:"))
        self.shadow_color_button = QPushButton("Select Color")
        self.shadow_color_button.clicked.connect(self._select_shadow_color)
        shadow_color_layout.addWidget(self.shadow_color_button)
        shadow_color_layout.addStretch()
        general_shadow_layout.addLayout(shadow_color_layout)

        scroll_layout.addWidget(general_shadow_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab


    def _create_advanced_gadgets_tab(self): #vers 1
        """Create advanced gadgets support tab - String, Gauge, Scale, Colorfield, List, Numeric, Knob, Levelmeter, Slider, Radio, Cycle, Palette, Popstring"""
        tab = QWidget()
        layout = QVBoxLayout(tab)

        # Instructions
        info_label = QLabel(
            "<b>Advanced Gadgets:</b><br>"
            "Configure Amiga MUI style gadgets with proper styling."
        )
        info_label.setWordWrap(True)
        info_label.setStyleSheet("padding: 8px; border-radius: 4px;")
        layout.addWidget(info_label)

        # Scroll area for controls
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)

        scroll_widget = QWidget()
        scroll_layout = QVBoxLayout(scroll_widget)
        scroll_layout.setSpacing(15)

        # String Gadgets
        string_group = QGroupBox("String Gadgets")
        string_group.setAutoFillBackground(True)
        string_layout = QVBoxLayout(string_group)

        # String Background Color
        string_bg_layout = QHBoxLayout()
        string_bg_layout.addWidget(QLabel("Background Color:"))
        self.string_bg_color = QPushButton("Select Color")
        self.string_bg_color.clicked.connect(lambda: self._select_color_for_gadget("string_bg"))
        string_bg_layout.addWidget(self.string_bg_color)
        string_bg_layout.addStretch()
        string_layout.addLayout(string_bg_layout)

        # String Text Color
        string_text_layout = QHBoxLayout()
        string_text_layout.addWidget(QLabel("Text Color:"))
        self.string_text_color = QPushButton("Select Color")
        self.string_text_color.clicked.connect(lambda: self._select_color_for_gadget("string_text"))
        string_text_layout.addWidget(self.string_text_color)
        string_text_layout.addStretch()
        string_layout.addLayout(string_text_layout)

        # String Border Color
        string_border_layout = QHBoxLayout()
        string_border_layout.addWidget(QLabel("Border Color:"))
        self.string_border_color = QPushButton("Select Color")
        self.string_border_color.clicked.connect(lambda: self._select_color_for_gadget("string_border"))
        string_border_layout.addWidget(self.string_border_color)
        string_border_layout.addStretch()
        string_layout.addLayout(string_border_layout)

        scroll_layout.addWidget(string_group)

        # Gauge Gadgets
        gauge_group = QGroupBox("Gauge Gadgets")
        gauge_group.setAutoFillBackground(True)
        gauge_layout = QVBoxLayout(gauge_group)

        # Gauge Background Color
        gauge_bg_layout = QHBoxLayout()
        gauge_bg_layout.addWidget(QLabel("Background Color:"))
        self.gauge_bg_color = QPushButton("Select Color")
        self.gauge_bg_color.clicked.connect(lambda: self._select_color_for_gadget("gauge_bg"))
        gauge_bg_layout.addWidget(self.gauge_bg_color)
        gauge_bg_layout.addStretch()
        gauge_layout.addLayout(gauge_bg_layout)

        # Gauge Fill Color
        gauge_fill_layout = QHBoxLayout()
        gauge_fill_layout.addWidget(QLabel("Fill Color:"))
        self.gauge_fill_color = QPushButton("Select Color")
        self.gauge_fill_color.clicked.connect(lambda: self._select_color_for_gadget("gauge_fill"))
        gauge_fill_layout.addWidget(self.gauge_fill_color)
        gauge_fill_layout.addStretch()
        gauge_layout.addLayout(gauge_fill_layout)

        # Gauge Border Color
        gauge_border_layout = QHBoxLayout()
        gauge_border_layout.addWidget(QLabel("Border Color:"))
        self.gauge_border_color = QPushButton("Select Color")
        self.gauge_border_color.clicked.connect(lambda: self._select_color_for_gadget("gauge_border"))
        gauge_border_layout.addWidget(self.gauge_border_color)
        gauge_border_layout.addStretch()
        gauge_layout.addLayout(gauge_border_layout)

        scroll_layout.addWidget(gauge_group)

        # Scale Gadgets
        scale_group = QGroupBox("Scale Gadgets")
        scale_group.setAutoFillBackground(True)
        scale_layout = QVBoxLayout(scale_group)

        # Scale Background Color
        scale_bg_layout = QHBoxLayout()
        scale_bg_layout.addWidget(QLabel("Background Color:"))
        self.scale_bg_color = QPushButton("Select Color")
        self.scale_bg_color.clicked.connect(lambda: self._select_color_for_gadget("scale_bg"))
        scale_bg_layout.addWidget(self.scale_bg_color)
        scale_bg_layout.addStretch()
        scale_layout.addLayout(scale_bg_layout)

        # Scale Handle Color
        scale_handle_layout = QHBoxLayout()
        scale_handle_layout.addWidget(QLabel("Handle Color:"))
        self.scale_handle_color = QPushButton("Select Color")
        self.scale_handle_color.clicked.connect(lambda: self._select_color_for_gadget("scale_handle"))
        scale_handle_layout.addWidget(self.scale_handle_color)
        scale_handle_layout.addStretch()
        scale_layout.addLayout(scale_handle_layout)

        scroll_layout.addWidget(scale_group)

        # Colorfield Gadgets
        colorfield_group = QGroupBox("Colorfield Gadgets")
        colorfield_group.setAutoFillBackground(True)
        colorfield_layout = QVBoxLayout(colorfield_group)

        # Colorfield Background Color
        colorfield_bg_layout = QHBoxLayout()
        colorfield_bg_layout.addWidget(QLabel("Background Color:"))
        self.colorfield_bg_color = QPushButton("Select Color")
        self.colorfield_bg_color.clicked.connect(lambda: self._select_color_for_gadget("colorfield_bg"))
        colorfield_bg_layout.addWidget(self.colorfield_bg_color)
        colorfield_bg_layout.addStretch()
        colorfield_layout.addLayout(colorfield_bg_layout)

        # Colorfield Border Color
        colorfield_border_layout = QHBoxLayout()
        colorfield_border_layout.addWidget(QLabel("Border Color:"))
        self.colorfield_border_color = QPushButton("Select Color")
        self.colorfield_border_color.clicked.connect(lambda: self._select_color_for_gadget("colorfield_border"))
        colorfield_border_layout.addWidget(self.colorfield_border_color)
        colorfield_border_layout.addStretch()
        colorfield_layout.addLayout(colorfield_border_layout)

        scroll_layout.addWidget(colorfield_group)

        # List Gadgets
        list_group = QGroupBox("List Gadgets")
        list_group.setAutoFillBackground(True)
        list_layout = QVBoxLayout(list_group)

        # List Background Color
        list_bg_layout = QHBoxLayout()
        list_bg_layout.addWidget(QLabel("Background Color:"))
        self.list_bg_color = QPushButton("Select Color")
        self.list_bg_color.clicked.connect(lambda: self._select_color_for_gadget("list_bg"))
        list_bg_layout.addWidget(self.list_bg_color)
        list_bg_layout.addStretch()
        list_layout.addLayout(list_bg_layout)

        # List Text Color
        list_text_layout = QHBoxLayout()
        list_text_layout.addWidget(QLabel("Text Color:"))
        self.list_text_color = QPushButton("Select Color")
        self.list_text_color.clicked.connect(lambda: self._select_color_for_gadget("list_text"))
        list_text_layout.addWidget(self.list_text_color)
        list_text_layout.addStretch()
        list_layout.addLayout(list_text_layout)

        # List Selection Color
        list_select_layout = QHBoxLayout()
        list_select_layout.addWidget(QLabel("Selection Color:"))
        self.list_select_color = QPushButton("Select Color")
        self.list_select_color.clicked.connect(lambda: self._select_color_for_gadget("list_select"))
        list_select_layout.addWidget(self.list_select_color)
        list_select_layout.addStretch()
        list_layout.addLayout(list_select_layout)

        scroll_layout.addWidget(list_group)

        # Numeric Gadgets
        numeric_group = QGroupBox("Numeric Gadgets")
        numeric_group.setAutoFillBackground(True)
        numeric_layout = QVBoxLayout(numeric_group)

        # Numeric Background Color
        numeric_bg_layout = QHBoxLayout()
        numeric_bg_layout.addWidget(QLabel("Background Color:"))
        self.numeric_bg_color = QPushButton("Select Color")
        self.numeric_bg_color.clicked.connect(lambda: self._select_color_for_gadget("numeric_bg"))
        numeric_bg_layout.addWidget(self.numeric_bg_color)
        numeric_bg_layout.addStretch()
        numeric_layout.addLayout(numeric_bg_layout)

        # Numeric Text Color
        numeric_text_layout = QHBoxLayout()
        numeric_text_layout.addWidget(QLabel("Text Color:"))
        self.numeric_text_color = QPushButton("Select Color")
        self.numeric_text_color.clicked.connect(lambda: self._select_color_for_gadget("numeric_text"))
        numeric_text_layout.addWidget(self.numeric_text_color)
        numeric_text_layout.addStretch()
        numeric_layout.addLayout(numeric_text_layout)

        scroll_layout.addWidget(numeric_group)

        # Knob Gadgets
        knob_group = QGroupBox("Knob Gadgets")
        knob_group.setAutoFillBackground(True)
        knob_layout = QVBoxLayout(knob_group)

        # Knob Background Color
        knob_bg_layout = QHBoxLayout()
        knob_bg_layout.addWidget(QLabel("Background Color:"))
        self.knob_bg_color = QPushButton("Select Color")
        self.knob_bg_color.clicked.connect(lambda: self._select_color_for_gadget("knob_bg"))
        knob_bg_layout.addWidget(self.knob_bg_color)
        knob_bg_layout.addStretch()
        knob_layout.addLayout(knob_bg_layout)

        # Knob Handle Color
        knob_handle_layout = QHBoxLayout()
        knob_handle_layout.addWidget(QLabel("Handle Color:"))
        self.knob_handle_color = QPushButton("Select Color")
        self.knob_handle_color.clicked.connect(lambda: self._select_color_for_gadget("knob_handle"))
        knob_handle_layout.addWidget(self.knob_handle_color)
        knob_handle_layout.addStretch()
        knob_layout.addLayout(knob_handle_layout)

        scroll_layout.addWidget(knob_group)

        # Levelmeter Gadgets
        levelmeter_group = QGroupBox("Levelmeter Gadgets")
        levelmeter_group.setAutoFillBackground(True)
        levelmeter_layout = QVBoxLayout(levelmeter_group)

        # Levelmeter Background Color
        levelmeter_bg_layout = QHBoxLayout()
        levelmeter_bg_layout.addWidget(QLabel("Background Color:"))
        self.levelmeter_bg_color = QPushButton("Select Color")
        self.levelmeter_bg_color.clicked.connect(lambda: self._select_color_for_gadget("levelmeter_bg"))
        levelmeter_bg_layout.addWidget(self.levelmeter_bg_color)
        levelmeter_bg_layout.addStretch()
        levelmeter_layout.addLayout(levelmeter_bg_layout)

        # Levelmeter Fill Color
        levelmeter_fill_layout = QHBoxLayout()
        levelmeter_fill_layout.addWidget(QLabel("Fill Color:"))
        self.levelmeter_fill_color = QPushButton("Select Color")
        self.levelmeter_fill_color.clicked.connect(lambda: self._select_color_for_gadget("levelmeter_fill"))
        levelmeter_fill_layout.addWidget(self.levelmeter_fill_color)
        levelmeter_fill_layout.addStretch()
        levelmeter_layout.addLayout(levelmeter_fill_layout)

        scroll_layout.addWidget(levelmeter_group)

        # Radio Gadgets
        radio_group = QGroupBox("Radio Gadgets")
        radio_group.setAutoFillBackground(True)
        radio_layout = QVBoxLayout(radio_group)

        # Radio Background Color
        radio_bg_layout = QHBoxLayout()
        radio_bg_layout.addWidget(QLabel("Background Color:"))
        self.radio_bg_color = QPushButton("Select Color")
        self.radio_bg_color.clicked.connect(lambda: self._select_color_for_gadget("radio_bg"))
        radio_bg_layout.addWidget(self.radio_bg_color)
        radio_bg_layout.addStretch()
        radio_layout.addLayout(radio_bg_layout)

        # Radio Text Color
        radio_text_layout = QHBoxLayout()
        radio_text_layout.addWidget(QLabel("Text Color:"))
        self.radio_text_color = QPushButton("Select Color")
        self.radio_text_color.clicked.connect(lambda: self._select_color_for_gadget("radio_text"))
        radio_text_layout.addWidget(self.radio_text_color)
        radio_text_layout.addStretch()
        radio_layout.addLayout(radio_text_layout)

        scroll_layout.addWidget(radio_group)

        # Cycle Gadgets
        cycle_group = QGroupBox("Cycle Gadgets")
        cycle_group.setAutoFillBackground(True)
        cycle_layout = QVBoxLayout(cycle_group)

        # Cycle Background Color
        cycle_bg_layout = QHBoxLayout()
        cycle_bg_layout.addWidget(QLabel("Background Color:"))
        self.cycle_bg_color = QPushButton("Select Color")
        self.cycle_bg_color.clicked.connect(lambda: self._select_color_for_gadget("cycle_bg"))
        cycle_bg_layout.addWidget(self.cycle_bg_color)
        cycle_bg_layout.addStretch()
        cycle_layout.addLayout(cycle_bg_layout)

        # Cycle Text Color
        cycle_text_layout = QHBoxLayout()
        cycle_text_layout.addWidget(QLabel("Text Color:"))
        self.cycle_text_color = QPushButton("Select Color")
        self.cycle_text_color.clicked.connect(lambda: self._select_color_for_gadget("cycle_text"))
        cycle_text_layout.addWidget(self.cycle_text_color)
        cycle_text_layout.addStretch()
        cycle_layout.addLayout(cycle_text_layout)

        scroll_layout.addWidget(cycle_group)

        # Palette Gadgets
        palette_group = QGroupBox("Palette Gadgets")
        palette_group.setAutoFillBackground(True)
        palette_layout = QVBoxLayout(palette_group)

        # Palette Background Color
        palette_bg_layout = QHBoxLayout()
        palette_bg_layout.addWidget(QLabel("Background Color:"))
        self.palette_bg_color = QPushButton("Select Color")
        self.palette_bg_color.clicked.connect(lambda: self._select_color_for_gadget("palette_bg"))
        palette_bg_layout.addWidget(self.palette_bg_color)
        palette_bg_layout.addStretch()
        palette_layout.addLayout(palette_bg_layout)

        # Palette Border Color
        palette_border_layout = QHBoxLayout()
        palette_border_layout.addWidget(QLabel("Border Color:"))
        self.palette_border_color = QPushButton("Select Color")
        self.palette_border_color.clicked.connect(lambda: self._select_color_for_gadget("palette_border"))
        palette_border_layout.addWidget(self.palette_border_color)
        palette_border_layout.addStretch()
        palette_layout.addLayout(palette_border_layout)

        scroll_layout.addWidget(palette_group)

        # Popstring Gadgets
        popstring_group = QGroupBox("Popstring Gadgets")
        popstring_group.setAutoFillBackground(True)
        popstring_layout = QVBoxLayout(popstring_group)

        # Popstring Background Color
        popstring_bg_layout = QHBoxLayout()
        popstring_bg_layout.addWidget(QLabel("Background Color:"))
        self.popstring_bg_color = QPushButton("Select Color")
        self.popstring_bg_color.clicked.connect(lambda: self._select_color_for_gadget("popstring_bg"))
        popstring_bg_layout.addWidget(self.popstring_bg_color)
        popstring_bg_layout.addStretch()
        popstring_layout.addLayout(popstring_bg_layout)

        # Popstring Text Color
        popstring_text_layout = QHBoxLayout()
        popstring_text_layout.addWidget(QLabel("Text Color:"))
        self.popstring_text_color = QPushButton("Select Color")
        self.popstring_text_color.clicked.connect(lambda: self._select_color_for_gadget("popstring_text"))
        popstring_text_layout.addWidget(self.popstring_text_color)
        popstring_text_layout.addStretch()
        popstring_layout.addLayout(popstring_text_layout)

        scroll_layout.addWidget(popstring_group)

        scroll_layout.addStretch()
        scroll.setWidget(scroll_widget)
        layout.addWidget(scroll)

        return tab


    def _create_ui_management_tab(self): #vers 2
        """UI Management components as a compact table like Fonts:
        Name - Background colour [swatch]  Border colour [swatch] ..."""
        from PyQt6.QtWidgets import QGridLayout
        tab = QWidget()
        layout = QVBoxLayout(tab)
        layout.setContentsMargins(4, 4, 4, 4)

        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        table = QGroupBox("UI Management Components")
        table.setAutoFillBackground(True)
        grid = QGridLayout(table)
        grid.setHorizontalSpacing(8)
        grid.setVerticalSpacing(4)
        grid.setColumnStretch(7, 1)

        COMPONENTS = [
            ("Group",       [("group_bg", "Background colour"), ("group_border", "Border colour")]),
            ("Scrollbar",   [("scrollbar_bg", "Background colour"), ("scrollbar_handle", "Handle colour")]),
            ("Listview",    [("listview_bg", "Background colour"), ("listview_text", "Text colour"),
                             ("listview_select", "Selection colour")]),
            ("Register",    [("register_bg", "Background colour"), ("register_text", "Text colour")]),
            ("Virtgroup",   [("virtgroup_bg", "Background colour"), ("virtgroup_border", "Border colour")]),
            ("Scrollgroup", [("scrollgroup_bg", "Background colour"), ("scrollgroup_border", "Border colour")]),
            ("Popobject",   [("popobject_bg", "Background colour"), ("popobject_border", "Border colour")]),
        ]
        tc = self.app_settings.get_theme_colors() or {}
        THEME_FALLBACK = {"group_bg": "panel_bg", "group_border": "border",
                          "scrollbar_bg": "scrollbar_background", "scrollbar_handle": "scrollbar_handle",
                          "listview_bg": "bg_primary", "listview_text": "text_primary",
                          "listview_select": "selection_background", "register_bg": "bg_secondary",
                          "register_text": "text_primary", "virtgroup_bg": "bg_tertiary",
                          "virtgroup_border": "border", "scrollgroup_bg": "bg_secondary",
                          "scrollgroup_border": "border", "popobject_bg": "bg_secondary",
                          "popobject_border": "border"}
        row = 0
        for name, colours in COMPONENTS:
            grid.addWidget(QLabel(f"{name} Components -"), row, 0)
            for k, (comp, label) in enumerate(colours):
                grid.addWidget(QLabel(label), row, 1 + k * 2)
                sw = QPushButton()
                sw.setFixedSize(40, 22)
                sw.setToolTip(f"{name} {label.lower()}: click to pick")
                val = getattr(self, f"{comp}_color_value", None) or tc.get(THEME_FALLBACK.get(comp, ''), '')
                sw.setStyleSheet(f"background-color: {val};" if val else "")
                sw.clicked.connect(lambda _=False, c=comp: self._select_color_for_component(c))
                setattr(self, f"{comp}_color", sw)
                grid.addWidget(sw, row, 2 + k * 2)
            row += 1
            if name == "Scrollbar":
                wl = QHBoxLayout()
                wl.addWidget(QLabel("Width"))
                self.ui_scrollbar_width_slider = QSlider(Qt.Orientation.Horizontal)
                self.ui_scrollbar_width_slider.setRange(8, 20)
                self.ui_scrollbar_width_slider.setValue(12)
                self.ui_scrollbar_width_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
                self.ui_scrollbar_width_slider.setTickInterval(2)
                wl.addWidget(self.ui_scrollbar_width_slider, 1)
                self.ui_scrollbar_width_label = QLabel("12 px")
                self.ui_scrollbar_width_label.setFixedWidth(44)
                self.ui_scrollbar_width_slider.valueChanged.connect(
                    lambda v: self.ui_scrollbar_width_label.setText(f"{v} px"))
                wl.addWidget(self.ui_scrollbar_width_label)
                grid.addLayout(wl, row, 1, 1, 7)
                row += 1

        outer = QWidget()
        ol = QVBoxLayout(outer)
        ol.setContentsMargins(0, 0, 0, 0)
        ol.addWidget(table)
        ol.addStretch()
        scroll.setWidget(outer)
        layout.addWidget(scroll)
        return tab
