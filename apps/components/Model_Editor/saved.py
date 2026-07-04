        # Information group below viewport
        info_group = QGroupBox("")
        info_group.setFont(self.title_font)
        info_layout = QVBoxLayout(info_group)
        info_group.setMaximumHeight(180)


        name_layout = QHBoxLayout()
        name_label = QLabel("Model Name:")
        name_label.setFont(self.panel_font)
        name_layout.addWidget(name_label)
        self.info_name = QLineEdit()
        self.info_name.setText("Click to edit...")
        self.info_name.setFont(self.panel_font)
        self.info_name.setReadOnly(True)
        self.info_name.setStyleSheet("padding: px; border: 1px solid palette(mid);")
        self.info_name.mousePressEvent = lambda e: self._enable_name_edit(e, False)
        name_layout.addWidget(self.info_name)
        info_layout.addLayout(name_layout)

        ide_layout = QHBoxLayout()
        ide_layout.setSpacing(4)
        ide_lbl = QLabel("IDE:")
        ide_lbl.setFont(self.panel_font)
        ide_lbl.setFixedWidth(28)
        ide_layout.addWidget(ide_lbl)
        self.info_ide_section = QLabel("—")
        self.info_ide_section.setFont(self.panel_font)
        self.info_ide_section.setFixedWidth(90)
        ide_layout.addWidget(self.info_ide_section)
        self.info_model_id = QLabel("ID: —")
        self.info_model_id.setFont(self.panel_font)
        self.info_model_id.setFixedWidth(70)
        ide_layout.addWidget(self.info_model_id)
        txd_lbl = QLabel("TXD:")
        txd_lbl.setFont(self.panel_font)
        txd_lbl.setFixedWidth(32)
        ide_layout.addWidget(txd_lbl)
        self.info_txd_name = QLabel("—")
        self.info_txd_name.setFont(self.panel_font)
        ide_layout.addWidget(self.info_txd_name)




        self.load_txd_btn = QPushButton("Open TXD")
        self.load_txd_btn.setFont(self.button_font)
        self.load_txd_btn.setIcon(self.icon_factory.open_icon(color=icon_color))
        self.load_txd_btn.setIconSize(QSize(16, 16))
        self.load_txd_btn.setFixedHeight(26)
        self.load_txd_btn.setMinimumWidth(80)
        self.load_txd_btn.setToolTip("Open TXD — uses IDE link if available, else browse")
        self.load_txd_btn.clicked.connect(self._open_txd_smart)
        ide_layout.addWidget(self.load_txd_btn)

        self.find_in_ide_btn = QPushButton("IDE Ref")
        self.find_in_ide_btn.setFont(self.button_font)
        self.find_in_ide_btn.setIcon(self.icon_factory.search_icon(color=icon_color))
        self.find_in_ide_btn.setIconSize(QSize(16, 16))
        self.find_in_ide_btn.setFixedHeight(26)
        self.find_in_ide_btn.setMinimumWidth(72)
        self.find_in_ide_btn.setToolTip("Look up model in DAT Browser IDE entries")
        self.find_in_ide_btn.clicked.connect(self._find_in_ide)
        ide_layout.addWidget(self.find_in_ide_btn)

        self.export_ojs_btn = QPushButton("Objs/Col")
        self.export_ojs_btn.setFont(self.button_font)
        self.export_ojs_btn.setFixedHeight(26)
        self.export_ojs_btn.setToolTip("Export geometry / COL")
        self.export_ojs_btn.clicked.connect(self.export_all)
        try:
            self.export_ojs_btn.setIcon(self.icon_factory.package_icon(color=icon_color))
            self.export_ojs_btn.setIconSize(QSize(14, 14))
        except Exception: pass
        ide_layout.addWidget(self.export_ojs_btn)

        self.gl_viewer_btn = QPushButton("3D View")
        self.gl_viewer_btn.setFont(self.button_font)
        self.gl_viewer_btn.setFixedHeight(26)
        self.gl_viewer_btn.setToolTip("Open GL Model Viewer")
        self.gl_viewer_btn.clicked.connect(self._open_gl_viewer)
        try:
            self.gl_viewer_btn.setIcon(self.icon_factory.cube_icon(color=icon_color))
            self.gl_viewer_btn.setIconSize(QSize(14, 14))
        except Exception: pass
        ide_layout.addWidget(self.gl_viewer_btn)
        info_layout.addLayout(ide_layout)

        self._bottom_text_row = QWidget()
        tr_lay = QVBoxLayout(self._bottom_text_row)
        tr_lay.setContentsMargins(0, 0, 0, 0)
        tr_lay.setSpacing(2)
        fmt_lay = QHBoxLayout()
        fmt_lay.setSpacing(5)
        self.format_combo = QComboBox()
        self.format_combo.setFont(self.panel_font)
        self.format_combo.addItems(["COL", "COL2", "COL3", "COL4"])
        self.format_combo.currentTextChanged.connect(self._change_format)
        self.format_combo.setMaximumWidth(100)
        self.format_combo.setVisible(False)
        self.format_combo.setToolTip("COL export format")
        fmt_lay.addWidget(self.format_combo)
        fmt_lay.addStretch()
        tr_lay.addLayout(fmt_lay)
        self.prelight_apply_btn = None
        self.prelight_setup_btn = None
        self.info_format = None
        self.show_shadow_btn   = None
        self.create_shadow_btn = None
        self.remove_shadow_btn = None
        info_layout.addWidget(self._bottom_text_row)
