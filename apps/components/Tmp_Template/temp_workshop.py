#!/usr/bin/env python3
# apps/components/Tmp_Template/temp_workshop.py - Version: 1
# X-Seti - Apr 2026 - IMG Factory 1.6
#
# TEMPLATE FILE — copy this, rename the class, fill in the stubs.
#
# What you get:
#   - Frameless window with corner resize triangles
#   - Toolbar: [Menu] [Settings] <title> [Open][Save][Export][Import][Undo][ℹ][⚙][_][⬜][✕]
#   - Three-panel layout: left list | centre tabs | right sidebar
#   - Right sidebar: 2-col 36px tool buttons (pencil/fill/line/rect/picker/etc)
#   - Workshop settings dialog (Fonts / Display / Menu / About)
#   - Global theme via AppSettings (cog button)
#   - Per-app JSON settings at ~/.config/imgfactory/{config_key}.json
#   - Keyboard shortcuts: Ctrl+O/S/Z/Y/C/V, P/F/L/R/K/Z/X/V
#   - Undo/redo stacks (implement _undo/_redo)
#   - Standalone launcher via __main__
#
# HOW TO USE:
#   1. Copy this file to your new workshop directory
#   2. Rename the class (e.g. PathWorkshop, ColourWorkshop)
#   3. Set the class attributes (App_name, App_author, config_key, etc.)
#   4. Override _build_menus_into_qmenu to add your File/Edit/View entries
#   5. Override _create_left_panel, _create_centre_panel if needed
#      (or just add widgets to the existing placeholder lists/tabs)
#   6. Fill in _open_file, _save_file, _undo, _redo, and any other stubs
#   7. Run standalone: python3 your_workshop.py

import sys
from pathlib import Path
from PyQt6.QtWidgets import (
    QApplication, QWidget, QVBoxLayout, QHBoxLayout, QLabel,
    QListWidgetItem, QFileDialog, QMessageBox, QMenu, QFrame,
    QSplitter, QTabWidget, QScrollArea
)
from PyQt6.QtGui import QFont, QKeySequence, QShortcut
from PyQt6.QtCore import Qt

# ── Import the base class ─────────────────────────────────────────────────────
try:
    from apps.components.Tmp_Template.gui_workshop import GUIWorkshop
except ImportError:
    # Allow running from the template directory directly
    import os
    sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))
    from apps.components.Tmp_Template.gui_workshop import GUIWorkshop


# TempWorkshop — rename this class when you copy the template

class TempWorkshop(GUIWorkshop):
    """Template workshop — rename class, fill in stubs, add your logic.

    All UI chrome, theme, settings, and window management is inherited.
    Only override what you need.
    """

    # - Identity — CHANGE THESE
    App_name        = "Temp Workshop"
    App_build       = "Build 1"
    App_author      = "X-Seti"
    App_year        = "2026"
    App_description = "Template — copy and rename for a new workshop tool."
    config_key      = "temp_workshop"   # → ~/.config/imgfactory/temp_workshop.json

    # ── Menu entries — override to add your File / Edit / View items ──────────
    def _build_menus_into_qmenu(self, pm):
        """Called by [Menu] button dropdown and top menubar.
        Add your own actions here — call super() to keep defaults, or replace."""
        fm = pm.addMenu("File")
        fm.addAction("Open…   Ctrl+O",   self._open_file)
        fm.addAction("Save…   Ctrl+S",   self._save_file)
        fm.addSeparator()
        fm.addAction("Export…",          self._export_file)
        fm.addAction("Import…",          self._import_file)
        fm.addSeparator()
        # Recent files from settings
        recent = self.WS.get_recent()
        if recent:
            rm = fm.addMenu("Recent Files")
            for rp in recent:
                act = rm.addAction(Path(rp).name)
                act.setToolTip(rp)
                act.triggered.connect(lambda checked=False, p=rp: self._open_file(p))
            rm.addSeparator()
            rm.addAction("Clear Recent", self._clear_recent)

        em = pm.addMenu("Edit")
        em.addAction("Undo   Ctrl+Z",    self._undo)
        em.addAction("Redo   Ctrl+Y",    self._redo)

        vm = pm.addMenu("View")
        vm.addAction("Zoom In  +",       lambda: self._zoom(1.25))
        vm.addAction("Zoom Out  -",      lambda: self._zoom(0.8))
        vm.addAction("Fit   Ctrl+0",     self._fit)
        vm.addSeparator()
        vm.addAction("About " + self.App_name, self._show_about)

    # - Optional: override panels
    # Uncomment and modify any of these to replace the placeholder panels.
    # If you don't override them you get placeholder list/tabs from GUIWorkshop.

    # def _create_left_panel(self):
    #     """Replace the left panel entirely."""
    #     panel = QFrame()
    #     panel.setFrameStyle(QFrame.Shape.StyledPanel)
    #     ll = QVBoxLayout(panel)
    #     ll.setContentsMargins(*self.get_panel_margins())
    #     # ... add your widgets
    #     return panel

    # def _create_centre_panel(self):
    #     """Replace the centre panel entirely."""
    #     panel = QFrame()
    #     panel.setFrameStyle(QFrame.Shape.StyledPanel)
    #     # ... add your canvas, tabs, etc.
    #     return panel

    # def _populate_sidebar(self):
    #     """Replace the sidebar tool buttons."""
    #     sl = self._sidebar_layout
    #     # ... add your tool rows using _nb() / _row() pattern from gui_workshop

    # - File operations — fill these in

    def _open_file(self, path=None):
        """Open a file. Called by toolbar Open button and Ctrl+O."""
        if not path:
            path, _ = QFileDialog.getOpenFileName(
                self, "Open File", "",
                "All Files (*)")
        if not path:
            return
        # TODO: load your file here
        # self._my_data = MyParser().load(path)
        # self._refresh_list()
        self.WS.add_recent(path)
        self._set_status(f"Opened: {Path(path).name}")
        self.save_btn.setEnabled(True)

    def _save_file(self):
        """Save current file. Called by toolbar Save button and Ctrl+S."""
        path, _ = QFileDialog.getSaveFileName(
            self, "Save File", "",
            "All Files (*)")
        if not path:
            return
        # TODO: save your file here
        # self._my_data.save(path)
        self._set_status(f"Saved: {Path(path).name}")

    def _export_file(self):
        """Export. Called by toolbar Export button."""
        # TODO: implement export
        self._set_status("Export — not yet implemented")

    def _import_file(self):
        """Import. Called by toolbar Import button."""
        # TODO: implement import
        self._set_status("Import — not yet implemented")


    # - Undo / redo — fill these in

    def _undo(self):
        """Undo last action. Ctrl+Z."""
        # TODO: pop from your undo stack and restore state
        self._set_status("Undo — not yet implemented")

    def _redo(self):
        """Redo last undone action. Ctrl+Y."""
        # TODO: pop from your redo stack and restore state
        self._set_status("Redo — not yet implemented")


    # - Left panel callbacks — fill these in

    def _on_list_selection_changed(self, row: int):
        """Called when the left panel list selection changes."""
        if row < 0:
            return
        # item = self._item_list.item(row)
        # TODO: update centre panel for selected item
        self._set_status(f"Selected row {row}")

    def _on_add_item(self):
        """Add button in left panel clicked."""
        # TODO: add a new item to self._item_list
        self._item_list.addItem(QListWidgetItem(f"Item {self._item_list.count()}"))

    def _on_remove_item(self):
        """Remove button in left panel clicked."""
        row = self._item_list.currentRow()
        if row >= 0:
            self._item_list.takeItem(row)


    # - Centre panel callbacks

    def _on_tab_changed(self, idx: int):
        """Called when centre tab changes."""
        # TODO: update state for tab idx
        pass

    # - Toolbar action callbacks

    def _zoom(self, factor: float):
        """Zoom the main view. Called by sidebar zoom buttons."""
        # TODO: apply zoom to your canvas
        pass

    def _fit(self):
        """Fit view to content. Ctrl+0."""
        # TODO: reset zoom/pan on your canvas
        pass

    def _jump(self):
        """Jump to current selection."""
        # TODO: scroll/pan to show selected item
        pass

    def _on_toolbar_action(self, action: str):
        """Generic toolbar action (rotate_cw, rotate_ccw, flip_h, flip_v, edit).
        Called by sidebar transform buttons."""
        # TODO: apply action to selected item
        self._set_status(f"Action: {action}")

    def _copy_item(self):
        """Ctrl+C — copy selection to clipboard."""
        # TODO: copy selected item
        pass

    def _paste_item(self):
        """Ctrl+V — paste clipboard."""
        # TODO: paste
        pass

    # - Tool selection
    def _set_active_tool(self, tool: str):
        """Override to add cursor changes or tool state for your canvas."""
        super()._set_active_tool(tool)   # updates sidebar button states
        # TODO: set cursor on your canvas widget
        self._set_status(f"Tool: {tool}")




# Standalone launcher — keep this at the bottom
if __name__ == "__main__":
    import traceback
    print(f"{TempWorkshop.App_name} {TempWorkshop.App_build} starting...")
    try:
        app = QApplication(sys.argv)
        w = TempWorkshop()
        w.setWindowTitle(f"{TempWorkshop.App_name} — Standalone")
        w.resize(1300, 800)
        w.show()
        if len(sys.argv) > 1 and Path(sys.argv[1]).is_file():
            w._open_file(sys.argv[1])
        sys.exit(app.exec())
    except Exception as e:
        print(f"ERROR: {e}")
        traceback.print_exc()
        sys.exit(1)
