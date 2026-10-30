#this belongs in apps/methods/toolbar_menu.py - Version: 1
# X-Seti - October 28 2026 - IMG Factory 1.6 - Shared ribbon right-click menu

"""
One right-click menu for every tool's ribbons / toolbars (Map Workshop layout).
Entries appear only when the tool has them (icon sets, icon size, docks ...).
"""

##Methods list -
# standard_toolbar_menu

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QDockWidget, QMenu, QSlider, QWidgetAction

ICON_SETS = [("Default (theme-aware)", 'default'), ("3ds Max style", '3dsmax')]


def standard_toolbar_menu(owner, toolbars, icon_scale=20) -> QMenu: #vers 2
    """Right-click menu for every tool's ribbons (Map Workshop layout).
    Icon Set / Icon Size / Ribbon Manager / Key & Mouse / Panels / Lock."""
    menu = QMenu(owner)
    if hasattr(owner, '_switch_icon_set') and hasattr(owner, '_get_icon_set'):
        sets = menu.addMenu("Icon Set")
        current = owner._get_icon_set()
        for label, key in ICON_SETS:
            act = sets.addAction(label)
            act.setCheckable(True)
            act.setChecked(current == key)
            act.triggered.connect(lambda _c, k=key: owner._switch_icon_set(k))
    if hasattr(owner, '_apply_icon_scale'):
        size = menu.addMenu("Icon Size")
        slider = QSlider(Qt.Orientation.Horizontal)
        slider.setRange(14, 40)
        slider.setSingleStep(2)
        slider.setValue(icon_scale)
        slider.valueChanged.connect(owner._apply_icon_scale)
        wa = QWidgetAction(menu)
        wa.setDefaultWidget(slider)
        size.addAction(wa)
    menu.addSeparator()
    manager = getattr(owner, 'open_ribbon_manager', None) or getattr(owner, '_open_ribbon_manager', None)
    if manager:
        menu.addAction("Ribbon Manager...", manager)
    if hasattr(owner, 'open_bindings_editor'):
        menu.addAction("Key && Mouse Bindings...", owner.open_bindings_editor)
    menu.addSeparator()
    panels = menu.addMenu("Panels")
    docks = getattr(owner, '_outer_docks', None)
    if docks is None and getattr(owner, '_outer_mw', None) is not None:
        docks = owner._outer_mw.findChildren(QDockWidget)
    for dock in sorted(docks or [], key=lambda d: d.windowTitle()):
        panels.addAction(dock.toggleViewAction())
    if panels.isEmpty():
        panels.addAction("(no panels)").setEnabled(False)
    menu.addSeparator()
    menu.addAction("Lock All Toolbars", lambda: [tb.setMovable(False) for tb in toolbars])
    menu.addAction("Unlock All Toolbars", lambda: [tb.setMovable(True) for tb in toolbars])
    save = getattr(owner, 'ribbon_save_state', None) or getattr(owner, '_save_toolbar_state', None)
    if save:
        menu.addAction("Save Ribbon Config", save)
    return menu
