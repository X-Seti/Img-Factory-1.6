#this belongs in apps/methods/ui_sizes.py - Version: 5
# X-Seti - October 22 2026 - IMG Factory 1.6 - UI size rules

"""
Global UI heights (heightrules.md). Defaults here; user overrides come
from App Settings > Sizes. A workshop can use its own sizes (its Settings >
Sizes) instead; those are kept in ui_sizes.json in the user config folder.
"""

##Methods list -
# _owner
# _owner_key
# _registry
# _rule_of
# apply_owner_sizes
# add_sizes_tab
# fit_inputs
# install_size_rules
# refresh_all
# icon_size
# owner_sizes_tab
# rule_label
# set_overrides
# size
# size_stylesheet
# sizes_panel
#
##class _SizeRules: -
# __init__

from typing import Dict

# key, label, default px, tooltip
SIZES = [
    ('dock_title',     "Dock title bar",        26, "Title bar of every dock pane"),
    ('title_button',   "Title bar buttons",     22, "Square buttons in a dock title bar"),
    ('title_icon',     "Title bar icons",       16, "Icons on dock title bar buttons"),
    ('title_toggle',   "Title bar toggles",     20, "Toggle buttons sitting in a title bar"),
    ('action_button',  "Action buttons",        26, "Load, Apply, Pick, OK, Cancel, Reset"),
    ('action_icon',    "Action button icons",   18, "Icons on action buttons"),
    ('small_button',   "Small row buttons",     18, "Inline open / new / delete / save buttons"),
    ('small_icon',     "Small button icons",    16, "Icons on small row buttons"),
    ('toolbar_button', "Toolbar buttons",       28, "Left and top toolbar icon buttons"),
    ('toolbar_icon',   "Toolbar icons",         20, "Icons on toolbar buttons"),
    ('small_widget',   "Small widgets",         22, "Gauges and small custom widgets"),
    ('search_box',     "Search box",            26, "Search and filter boxes"),
    ('input',          "Inputs",                24, "Line edits, combo boxes, spin boxes, labels beside them"),
    ('checkbox',       "Checkbox",              18, "Check boxes"),
    ('table_header',   "Table headers",         26, "Column header row"),
    ('table_row',      "Table rows (text)",     20, "Rows without icons; icon rows use icon + 4"),
    ('picker_tile',    "Picker grid tiles",     48, "Texture / material / asset tiles"),
    ('tab_bar',        "Tab bar",               22, "Tabs on tab widgets"),
    ('status_bar',     "Status bar",            22, "Bottom status bar"),
    ('number_width',   "Number field width",    80, "Width of spin boxes and number fields"),
]

DEFAULTS: Dict[str, int] = {k: d for k, _l, d, _t in SIZES}
_overrides: Dict[str, int] = {}


def set_overrides(values) -> None: #vers 1
    """User sizes from settings; unknown keys ignored."""
    _overrides.clear()
    for k, v in (values or {}).items():
        if k in DEFAULTS:
            _overrides[k] = int(v)


_workshops = None                                   # {owner key: {'own': bool, 'sizes': {}}}


def _registry() -> dict: #vers 1
    """Per-workshop sizes from ui_sizes.json (read once)."""
    global _workshops
    if _workshops is None:
        import json
        from apps.methods.img_factory_settings import get_user_config_dir
        path = get_user_config_dir() / 'ui_sizes.json'
        _workshops = json.loads(path.read_text()) if path.exists() else {}
    return _workshops


def _owner_key(obj) -> str: #vers 1
    """Registry key of a workshop: module.class."""
    return f"{type(obj).__module__}.{type(obj).__qualname__}"


def _owner(widget): #vers 2
    """Nearest ancestor workshop using its own sizes, else None."""
    reg = _registry()
    w = widget
    while w is not None:
        cfg = reg.get(_owner_key(w))
        if cfg and cfg.get('own'):
            return cfg
        w = w.parent() if hasattr(w, 'parent') else None
    return None


def size(key: str, widget=None) -> int: #vers 3
    """Pixel size for a rule key; the widget's workshop may override."""
    if widget is not None:
        cfg = _owner(widget)
        if cfg is not None:
            return int(cfg.get('sizes', {}).get(key, DEFAULTS[key]))
    return _overrides.get(key, DEFAULTS[key])


def icon_size(key: str, widget=None): #vers 2
    """QSize for an icon rule key."""
    from PyQt6.QtCore import QSize
    s = size(key, widget)
    return QSize(s, s)


def size_stylesheet() -> str: #vers 2
    """Global QSS for input, checkbox, tab and header heights."""
    install_size_rules()
    inp = size('input') - 2                     # min-height excludes the 1 px border
    tab = size('tab_bar') - 3                   # tab borders: 1 px top, 2-3 px accent bottom
    return f"""
        QLineEdit, QComboBox, QSpinBox, QDoubleSpinBox, QTimeEdit, QDateEdit {{
            min-height: {inp}px; max-height: {inp}px;
            padding-top: 0px; padding-bottom: 0px;
        }}
        QCheckBox {{ min-height: {size('checkbox')}px; }}
        QTabBar::tab {{ height: {tab}px; padding-top: 0px; padding-bottom: 0px; }}
        QHeaderView::section {{ height: {size('table_header')}px; padding-top: 0px; padding-bottom: 0px; }}
        QStatusBar {{ min-height: {size('status_bar')}px; max-height: {size('status_bar')}px; }}
        """


def rule_label(text: str, tip: str = '', widget=None): #vers 2
    """Input row label: right-aligned, vertically centred, input height."""
    from PyQt6.QtCore import Qt
    from PyQt6.QtWidgets import QLabel
    lbl = QLabel(text)
    lbl.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
    lbl.setFixedHeight(size('input', widget))
    if tip:
        lbl.setToolTip(tip)
    return lbl


def fit_inputs(root) -> None: #vers 4
    """Input height on every edit / combo / spin box under root."""
    from PyQt6.QtWidgets import QAbstractItemView, QAbstractSpinBox, QComboBox, QLineEdit
    for w in root.findChildren((QLineEdit, QComboBox, QAbstractSpinBox)):
        p = w.parentWidget()
        if isinstance(p, (QComboBox, QAbstractSpinBox)):                   # skip inner edits
            continue
        if p is not None and isinstance(p.parentWidget(), QAbstractItemView):   # table cell widgets
            continue
        w.setFixedHeight(size('input', w))                                 # w's own workshop


def _rule_of(btn): #vers 1
    """Size rule of a push button, decided once from its first height."""
    rule = btn.property('size_rule')
    if rule is None:
        h = btn.height() if btn.minimumHeight() == btn.maximumHeight() else 0
        rule = '' if not h or h > 32 else ('action_button' if h >= 22 else 'small_button')
        btn.setProperty('size_rule', rule)
    return rule


def apply_owner_sizes(owner) -> None: #vers 2
    """Re-fit a workshop: dock title bars, title / push buttons, inputs, headers."""
    from PyQt6.QtCore import Qt
    from PyQt6.QtWidgets import QDockWidget, QHeaderView, QPushButton, QStatusBar, QToolButton
    bar_h, btn = size('dock_title', owner), size('title_button', owner)
    titles = set()
    for d in owner.findChildren(QDockWidget):
        bar = d.titleBarWidget()
        if bar is None or not hasattr(d, '_title_label'):
            continue
        bar.setFixedHeight(bar_h)
        for b in bar.findChildren(QToolButton):
            b.setFixedSize(btn, btn)
            b.setIconSize(icon_size('title_icon', owner))
        titles.update(id(w) for w in bar.findChildren(QPushButton))
    fit_inputs(owner)
    if hasattr(owner, '_show_workshop_settings'):    # IMG Factory's own buttons: Buttons tab
        for b in owner.findChildren(QPushButton):
            rule = '' if id(b) in titles else _rule_of(b)
            if rule:
                b.setFixedHeight(size(rule, b))
                ic = 'action_icon' if rule == 'action_button' else 'small_icon'
                if not b.icon().isNull():
                    b.setIconSize(icon_size(ic, b))
    for h in owner.findChildren(QHeaderView):
        if h.orientation() == Qt.Orientation.Horizontal and not h.isHidden():
            h.setFixedHeight(size('table_header', h))
    for sb in owner.findChildren(QStatusBar):
        sb.setFixedHeight(size('status_bar', sb))


class _SizeRules: #vers 1
    """App event filter: size rules on each workshop's first show."""

    def __init__(self): #vers 1
        from PyQt6.QtCore import QObject

        class _F(QObject):
            def eventFilter(self, obj, event): #vers 1
                from PyQt6.QtCore import QEvent
                if event.type() == QEvent.Type.Show and not getattr(obj, '_size_rules_done', False) \
                        and (hasattr(obj, '_show_workshop_settings') or hasattr(obj, 'main_tab_widget')):
                    obj._size_rules_done = True
                    apply_owner_sizes(obj)
                return False
        self.filter = _F()


_rules = None


def install_size_rules() -> None: #vers 1
    """Hook workshops' first show (once per app)."""
    global _rules
    from PyQt6.QtWidgets import QApplication
    app = QApplication.instance()
    if _rules is None and app is not None:
        _rules = _SizeRules()
        app.installEventFilter(_rules.filter)


def refresh_all() -> None: #vers 1
    """Re-fit every open workshop (after a sizes change)."""
    from PyQt6.QtWidgets import QApplication
    for w in QApplication.allWidgets():
        if getattr(w, '_size_rules_done', False):
            apply_owner_sizes(w)


def sizes_panel(values: dict, on_change, own=None, on_own=None): #vers 1
    """Sizes grid (one spin per rule); optional 'own sizes' tick box.
    values: current sizes; on_change(key, value); on_own(bool)."""
    from PyQt6.QtCore import Qt
    from PyQt6.QtWidgets import (QCheckBox, QGridLayout, QGroupBox, QHBoxLayout, QPushButton,
                                 QSpinBox, QVBoxLayout, QWidget)
    panel = QWidget()
    lay = QVBoxLayout(panel)
    lay.setContentsMargins(6, 4, 6, 4)
    lay.setSpacing(4)
    if own is not None:
        chk = QCheckBox("Use own sizes (override global)")
        chk.setToolTip("Ticked: this workshop uses the sizes below.\n"
                       "Not ticked: it follows App Settings > Sizes.")
        chk.setChecked(own)
        chk.toggled.connect(on_own)
        lay.addWidget(chk)
        panel.own_check = chk
    box = QGroupBox("Sizes  (pixels)")
    grid = QGridLayout(box)
    grid.setHorizontalSpacing(4)
    grid.setVerticalSpacing(2)
    panel.spins = {}
    for n, (key, label, default, tip) in enumerate(SIZES):
        spin = QSpinBox()
        spin.setRange(8, 200)
        spin.setSuffix(" px")
        spin.setFixedWidth(size('number_width'))
        spin.setToolTip(f"{tip} (default {default})")
        spin.setValue(int(values.get(key, size(key))))
        spin.valueChanged.connect(lambda v, k=key: on_change(k, v))
        r, c = n % 10, (n // 10) * 2
        grid.addWidget(rule_label(label, f"{tip} (default {default})"), r, c)
        grid.addWidget(spin, r, c + 1)
        panel.spins[key] = spin
    grid.setColumnStretch(4, 1)
    grid.setRowStretch(10, 1)
    lay.addWidget(box, 1)
    if own is not None:
        box.setEnabled(own)
        chk.toggled.connect(box.setEnabled)
    row = QHBoxLayout()
    reset = QPushButton("Reset Sizes to Defaults")
    reset.setToolTip("Back to the heightrules.md values")

    def _reset(): #vers 1
        for k, sp in panel.spins.items():
            sp.setValue(DEFAULTS[k])
    reset.clicked.connect(_reset)
    row.addWidget(reset)
    row.addStretch()
    lay.addLayout(row)
    return panel


def owner_sizes_tab(owner): #vers 3
    """Workshop Settings > Sizes tab; changes apply and save at once."""
    import json
    from apps.methods.img_factory_settings import get_user_config_dir
    key = _owner_key(owner)
    cfg = _registry().get(key, {'own': False, 'sizes': {}})
    work = {'own': bool(cfg.get('own')), 'sizes': {k: size(k) for k in DEFAULTS}}
    work['sizes'].update({k: int(v) for k, v in cfg.get('sizes', {}).items() if k in DEFAULTS})

    def apply(): #vers 3
        _registry()[key] = {'own': work['own'],
                            'sizes': {k: v for k, v in work['sizes'].items() if v != DEFAULTS[k]}}
        path = get_user_config_dir() / 'ui_sizes.json'
        path.write_text(json.dumps(_registry(), indent=2))
        apply_owner_sizes(owner)

    def _set(k, v): #vers 2
        work['sizes'][k] = v
        apply()

    def _own(on): #vers 2
        work['own'] = on
        apply()
    panel = sizes_panel(work['sizes'], _set, work['own'], _own)
    return panel, apply


def add_sizes_tab(owner, tabs) -> None: #vers 1
    """Add the workshop's own Sizes tab to its settings QTabWidget."""
    tabs.addTab(owner_sizes_tab(owner)[0], "Sizes")
