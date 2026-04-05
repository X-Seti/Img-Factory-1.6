#!/usr/bin/env python3
# apps/components/DP5_Workshop/color_pal_presets.py - Version: 1
# X-Seti - April 2026 - DP5 Workshop colour palette presets mixin
"""
Colour picker tab and retro palette presets for DP5Workshop.
This module provides _create_color_picker_tab() and related methods
as a mixin class to be merged into DP5Workshop.
"""
from PyQt6.QtWidgets import (
    QWidget, QVBoxLayout, QHBoxLayout, QSplitter,
    QGroupBox, QLabel, QPushButton, QGridLayout, QFrame
)
from PyQt6.QtCore import Qt


class ColorPalPresetsMixin:
    """Mixin providing the colour picker tab for DP5Workshop."""

    def _create_color_picker_tab(self): #vers 7
        """Create color picker and theme editor tab - Final layout with logical flow"""
        tab = QWidget()
        _outer = QVBoxLayout(tab)          # VBox so tab fills its container
        _outer.setContentsMargins(0, 0, 0, 0)
        _outer.setSpacing(0)
        main_splitter = QSplitter(Qt.Orientation.Horizontal)
        main_splitter.setChildrenCollapsible(False)
        from PyQt6.QtWidgets import QSizePolicy as _SP
        main_splitter.setSizePolicy(_SP.Policy.Expanding, _SP.Policy.Expanding)
        _outer.addWidget(main_splitter, 1)  # stretch=1: splitter fills all space
        main_layout = main_splitter

        # ========== LEFT PANEL ==========
        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_layout.setContentsMargins(4, 4, 4, 4)
        left_panel.setMinimumWidth(220)
        left_panel.setMaximumWidth(420)   # slightly wider cap

        # Screen Color Picker Group
        picker_group = QGroupBox("Color Picker")
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


    def _refresh_retro_palette(self, *args):
        """Stub — implemented when palette grid is wired up."""
        pass

    def _toggle_palette_grid(self, *args):
        """Stub — implemented when palette grid is wired up."""
        pass


__all__ = ['ColorPalPresetsMixin']
