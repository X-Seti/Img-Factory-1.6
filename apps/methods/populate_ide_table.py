#this belongs in apps/methods/populate_ide_table.py - Version: 1

##Methods list -
# populate_table_with_ide_data
# setup_ide_table_structure

"""populate_ide_table.py - populate a table with a single .ide file's
own real objects, the same way an .img archive's own entries or a
.col file's own models are shown in IMG Factory's main table (Sep 5
2026, per Keith: "can we show the game_vc.ide in the same format as
the col, img, show the ide in a row table, with odd and even pattern
lines"). Reuses the same real, established column convention DAT
Browser's own "Objects (IDE)" table already uses (ID/Model/TXD/Type/
Section/Draw Dist/Flags/Source IDE), so a single file's own view
matches the app's own existing aggregate one exactly. Alternating
row colours come for free from create_tab's own table setup
(setAlternatingRowColors(True)), not from anything in here."""

from apps.methods.export_shared import get_active_table


def populate_table_with_ide_data(main_window, ide_objects): #vers 1
    """Populate the active tab's own table with one .ide file's own
    real objects. ide_objects is a list of IDEObject (IDEParser's own
    .objects list, or any other list of the same dataclass)."""
    try:
        if not ide_objects:
            return False

        table = get_active_table(main_window)
        if table is None:
            return False

        setup_ide_table_structure(main_window)

        table.setRowCount(len(ide_objects))
        from PyQt6.QtWidgets import QTableWidgetItem
        from PyQt6.QtCore import Qt as _Qt
        for row, obj in enumerate(ide_objects):
            for col, val in enumerate([
                str(obj.model_id), obj.model_name, obj.txd_name,
                obj.obj_type, obj.section,
                str(obj.extra.get("draw_dist", "")),
                str(obj.extra.get("flags", "")),
                obj.source_ide,
            ]):
                item = QTableWidgetItem(val)
                item.setFlags(item.flags() & ~_Qt.ItemFlag.ItemIsEditable)
                table.setItem(row, col, item)

        return True
    except Exception:
        return False


def setup_ide_table_structure(main_window): #vers 1
    """Set up the real, established Objects (IDE) column layout -
    same headers DAT Browser's own aggregate Objects (IDE) table
    already uses."""
    try:
        table = get_active_table(main_window)
        if table is None:
            return False

        headers = ["ID", "Model", "TXD", "Type", "Section", "Draw Dist", "Flags", "Source IDE"]
        table.setColumnCount(len(headers))
        table.setHorizontalHeaderLabels(headers)
        table.setColumnWidth(0, 60)
        table.setColumnWidth(1, 200)
        table.setColumnWidth(2, 150)
        table.setColumnWidth(3, 80)
        table.setColumnWidth(4, 100)
        table.setColumnWidth(5, 80)
        table.setColumnWidth(6, 80)
        return True
    except Exception:
        return False
