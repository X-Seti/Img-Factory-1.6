#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_logic_func.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Vehicle Workshop logic stubs

"""
Default file/edit actions overridden by VehicleWorkshop.
"""

##class _LogicStubsMixin: -
# _build_menus_into_qmenu
# _clear_recent
# _copy_item
# _export_file
# _fit
# _import_file
# _jump
# _on_add_item
# _on_list_selection_changed
# _on_remove_item
# _on_tab_changed
# _on_toolbar_action
# _open_file
# _paste_item
# _redo
# _save_file
# _set_active_tool
# _undo
# _zoom
# get_menu_title

from pathlib import Path

from PyQt6.QtWidgets import QListWidgetItem, QMessageBox


class _LogicStubsMixin:
    """Stub methods for subclass override.
    All return immediately or show a 'not implemented' status message.
    Replace these with your actual file format, drawing, and undo logic.
    """

    #    ToolMenuMixin protocol
    def get_menu_title(self) -> str: #vers 1
        return self.App_name

    def _build_menus_into_qmenu(self, pm): #vers 1
        """Override to populate File / Edit / View menus for your app."""
        fm = pm.addMenu("File")
        fm.addAction("Open…  Ctrl+O",  self._open_file)
        fm.addAction("Save…  Ctrl+S",  self._save_file)
        fm.addSeparator()
        fm.addAction("Export…",        self._export_file)
        fm.addAction("Import…",        self._import_file)
        fm.addSeparator()
        recent = self.WS.get_recent()
        if recent:
            rm = fm.addMenu("Recent Files")
            for rp in recent:
                act = rm.addAction(Path(rp).name); act.setToolTip(rp)
                act.triggered.connect(
                    lambda checked=False, p=rp: self._open_file(p))
            rm.addSeparator()
            rm.addAction("Clear Recent", self._clear_recent)
        em = pm.addMenu("Edit")
        em.addAction("Undo  Ctrl+Z",   self._undo)
        em.addAction("Redo  Ctrl+Y",   self._redo)
        vm = pm.addMenu("View")
        vm.addAction("Zoom In  +",     lambda: self._zoom(1.25))
        vm.addAction("Zoom Out  -",    lambda: self._zoom(0.8))
        vm.addAction("Fit  Ctrl+0",    self._fit)
        vm.addSeparator()
        vm.addAction("About " + self.App_name, self._show_about)

    #    File operations
    def _open_file(self, path=None):   pass   # override: load your format  #vers 1
    def _save_file(self):              pass   # override: save your format  #vers 1
    def _export_file(self): #vers 1
        QMessageBox.information(self, "Export", "Export not yet implemented.")
    def _import_file(self): #vers 1
        QMessageBox.information(self, "Import", "Import not yet implemented.")
    def _clear_recent(self): #vers 1
        self.WS._data["recent_files"] = []; self.WS.save()
        self._set_status("Recent files cleared")

    #    Edit operations
    def _undo(self):         self._set_status("Undo — override in subclass")  #vers 1
    def _redo(self):         self._set_status("Redo — override in subclass")  #vers 1
    def _copy_item(self):    pass   # override: copy selection  #vers 1
    def _paste_item(self):   pass   # override: paste clipboard  #vers 1

    #    View operations
    def _zoom(self, factor: float): pass   # override: zoom your canvas  #vers 1
    def _fit(self):                 pass   # override: fit view  #vers 1
    def _jump(self):                pass   # override: jump to selection  #vers 1

    #    Panel callbacks
    def _on_list_selection_changed(self, row: int): pass  #vers 1
    def _on_tab_changed(self, idx: int):            pass  #vers 1
    def _on_add_item(self):  #vers 1
        self._item_list.addItem(
            QListWidgetItem(f"Item {self._item_list.count()}"))

    def _on_remove_item(self): #vers 1
        row = self._item_list.currentRow()
        if row >= 0: self._item_list.takeItem(row)

    #    Toolbar actions
    def _on_toolbar_action(self, action: str): #vers 1
        pass  # rotate/flip/edit etc.

    #    Tool management
    def _set_active_tool(self, tool: str): #vers 1
        self._active_tool = tool
        for name, btn in self._draw_btns.items():
            btn.setChecked(name == tool)


# GUIWorkshop — assembles all four sections
