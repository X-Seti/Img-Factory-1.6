#this belongs in apps/components/Master_Ide/txd_dedup_dialog.py - Version: 1
# X-Seti - September 12 2026 - IMG Factory 1.6 - TXD Duplicate Check Dialog

"""txd_dedup_dialog.py - real UI for txd_dedup.py's near-duplicate
TXD detection (Sep 12 2026, per Keith's own real example: buildhous.
txd/buildhoushi.txd/buildhous112.txd differing by one texture each).
Opened from Master IDE Workshop so the "Redirect References..."
step (updating every real objs/tobj entry's own txd_name once TXDs
are consolidated) has access to the loaded merge - a standalone
scan (no Master IDE result given) still works, just without that
last step."""

##Methods list -
# TXDDedupDialog

import os
from PyQt6.QtWidgets import (
    QDialog, QVBoxLayout, QHBoxLayout, QFormLayout, QLabel, QSpinBox,
    QPushButton, QListWidget, QFileDialog, QMessageBox, QTabWidget, QWidget,
    QLineEdit,
)

from apps.methods.txd_dedup import (
    load_txd_info, load_txd_info_from_img, find_near_duplicate_txds,
    cluster_near_duplicate_txds, find_same_name_size_mismatches,
)


class TXDDedupDialog(QDialog): #vers 1
    def __init__(self, parent, master_ide_result=None): #vers 1
        super().__init__(parent)
        self.master_ide_result = master_ide_result
        self.txd_infos = {}
        self.clusters = []
        self.mismatches = []
        self.setWindowTitle("TXD Duplicate Check")
        self.resize(640, 520)
        self._build_ui()

    def _build_ui(self): #vers 1
        lay = QVBoxLayout(self)

        source_row = QHBoxLayout()
        scan_folder_btn = QPushButton("Scan Folder...")
        scan_folder_btn.clicked.connect(self._on_scan_folder)
        source_row.addWidget(scan_folder_btn)
        scan_img_btn = QPushButton("Scan IMG...")
        scan_img_btn.setToolTip("Scan TXD entries embedded inside a real IMG archive.")
        scan_img_btn.clicked.connect(self._on_scan_img)
        source_row.addWidget(scan_img_btn)
        source_row.addStretch()
        source_row.addWidget(QLabel("Max name diff:"))
        self.max_diff_spin = QSpinBox()
        self.max_diff_spin.setRange(0, 999)
        self.max_diff_spin.setValue(1)
        self.max_diff_spin.setToolTip(
            "How many texture names two TXDs may differ by and still be "
            "flagged as likely mergeable (Keith's own '-1/+1, combine; "
            "5 different, ignore' idea).")
        source_row.addWidget(self.max_diff_spin)
        rescan_btn = QPushButton("Re-run Comparison")
        rescan_btn.clicked.connect(self._on_compare)
        source_row.addWidget(rescan_btn)
        lay.addLayout(source_row)

        self.summary_label = QLabel("Scan a folder or an IMG to begin.")
        lay.addWidget(self.summary_label)

        self.tabs = QTabWidget()
        lay.addWidget(self.tabs, 1)
        self.cluster_list = QListWidget()
        self.cluster_list.itemDoubleClicked.connect(self._on_cluster_activated)
        self.tabs.addTab(self.cluster_list, "Near-Duplicate Groups")
        self.mismatch_list = QListWidget()
        self.tabs.addTab(self.mismatch_list, "Same-Name Size Mismatches")

        btn_row = QHBoxLayout()
        export_btn = QPushButton("Export Report...")
        export_btn.clicked.connect(self._on_export)
        btn_row.addWidget(export_btn)
        btn_row.addStretch()
        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.reject)
        btn_row.addWidget(close_btn)
        lay.addLayout(btn_row)

    def _on_scan_folder(self): #vers 1
        folder = QFileDialog.getExistingDirectory(self, "Select folder to scan for .txd files")
        if not folder:
            return
        txd_paths = []
        for root, _dirs, files in os.walk(folder):
            for f in files:
                if f.lower().endswith('.txd'):
                    txd_paths.append(os.path.join(root, f))
        if not txd_paths:
            QMessageBox.information(self, "TXD Duplicate Check", "No .txd files found in that folder.")
            return
        self.txd_infos = load_txd_info(txd_paths)
        self._on_compare()

    def _on_scan_img(self): #vers 1
        img_path, _ = QFileDialog.getOpenFileName(
            self, "Select IMG archive to scan", "", "IMG Files (*.img)")
        if not img_path:
            return
        self.txd_infos = load_txd_info_from_img(img_path)
        if not self.txd_infos:
            QMessageBox.information(self, "TXD Duplicate Check",
                "No real TXD entries found (or the archive could not be opened).")
            return
        self._on_compare()

    def _on_compare(self): #vers 1
        if not self.txd_infos:
            return
        near_dupes = find_near_duplicate_txds(self.txd_infos, max_diff=self.max_diff_spin.value())
        self.clusters = cluster_near_duplicate_txds(near_dupes)
        self.mismatches = find_same_name_size_mismatches(self.txd_infos)

        self.summary_label.setText(
            f"Scanned {len(self.txd_infos)} real TXD(s) - {len(self.clusters)} near-duplicate "
            f"group(s), {len(self.mismatches)} same-name size mismatch(es).")

        self.cluster_list.clear()
        for i, cluster in enumerate(self.clusters):
            names = ", ".join(self._display_name(p) for p in sorted(cluster))
            self.cluster_list.addItem(f"Group {i + 1} ({len(cluster)} files): {names}")

        self.mismatch_list.clear()
        for m in self.mismatches:
            largest = m.largest
            lines = "; ".join(f"{self._display_name(p)} {w}x{h}" for p, w, h in m.occurrences)
            self.mismatch_list.addItem(
                f"{m.texture_name}: {lines}  (largest: {self._display_name(largest[0])} "
                f"{largest[1]}x{largest[2]})")

    def _display_name(self, path): #vers 1
        return os.path.basename(path.split("::")[-1]) if "::" in path else os.path.basename(path)

    def _on_cluster_activated(self, item): #vers 1
        index = self.cluster_list.row(item)
        if index < 0 or index >= len(self.clusters):
            return
        cluster = sorted(self.clusters[index])
        dlg = _ClusterDetailDialog(self, cluster, self.txd_infos, self.master_ide_result)
        dlg.exec()

    def _on_export(self): #vers 1
        path, _ = QFileDialog.getSaveFileName(
            self, "Export Report", "txd_dedup_report.txt", "Text Files (*.txt)")
        if not path:
            return
        lines = [f"TXD Duplicate Check Report", f"{len(self.txd_infos)} real TXD(s) scanned", ""]
        lines.append(f"Near-duplicate groups ({len(self.clusters)}):")
        for i, cluster in enumerate(self.clusters):
            lines.append(f"  Group {i + 1}:")
            lines.extend(f"    {self._display_name(p)}" for p in sorted(cluster))
        lines.append("")
        lines.append(f"Same-name size mismatches ({len(self.mismatches)}):")
        for m in self.mismatches:
            lines.append(f"  {m.texture_name}:")
            lines.extend(f"    {self._display_name(p)}  {w}x{h}" for p, w, h in m.occurrences)
        try:
            with open(path, "w", encoding="utf-8", errors="ignore") as f:
                f.write("\n".join(lines))
            QMessageBox.information(self, "Export Report", f"Saved to:\n{path}")
        except Exception as e:
            QMessageBox.warning(self, "Export Failed", str(e))


class _ClusterDetailDialog(QDialog): #vers 1
    """One near-duplicate group's own detail - which TXD is the real
    superset (the natural consolidation target), and, when opened
    from Master IDE, a Redirect References action."""
    def __init__(self, parent, cluster, txd_infos, master_ide_result): #vers 1
        super().__init__(parent)
        self.cluster = cluster
        self.txd_infos = txd_infos
        self.master_ide_result = master_ide_result
        self.setWindowTitle("Near-Duplicate Group")
        self.resize(520, 400)
        lay = QVBoxLayout(self)

        keeper = max(cluster, key=lambda p: len(txd_infos[p].texture_names))
        lay.addWidget(QLabel(f"Suggested keeper (most textures): "
                              f"{os.path.basename(keeper.split('::')[-1])}"))

        lst = QListWidget()
        for p in cluster:
            info = txd_infos[p]
            marker = " (keeper)" if p == keeper else ""
            lst.addItem(f"{os.path.basename(p.split('::')[-1])} - "
                        f"{len(info.texture_names)} textures{marker}")
        lay.addWidget(lst, 1)

        if master_ide_result is not None:
            redirect_row = QHBoxLayout()
            redirect_row.addWidget(QLabel("New TXD name for redirected references:"))
            self.new_name_edit = QLineEdit(os.path.splitext(os.path.basename(keeper.split('::')[-1]))[0])
            redirect_row.addWidget(self.new_name_edit)
            lay.addLayout(redirect_row)
            redirect_btn = QPushButton("Redirect References in Master IDE...")
            redirect_btn.setToolTip(
                "Update every real objs/tobj entry declaring one of this group's "
                "OTHER txd_names to point at the new name instead.")
            redirect_btn.clicked.connect(lambda: self._on_redirect(keeper))
            lay.addWidget(redirect_btn)

        close_btn = QPushButton("Close")
        close_btn.clicked.connect(self.accept)
        lay.addWidget(close_btn)

    def _on_redirect(self, keeper): #vers 1
        from apps.methods.master_ide_edit import (
            find_txd_name_references, redirect_txd_references, write_source_file)

        old_names = [os.path.splitext(os.path.basename(p.split("::")[-1]))[0]
                     for p in self.cluster if p != keeper]
        new_name = self.new_name_edit.text().strip()
        if not new_name:
            QMessageBox.warning(self, "Redirect Failed", "Enter a real new TXD name first.")
            return
        refs = find_txd_name_references(self.master_ide_result, old_names)
        if not refs:
            QMessageBox.information(self, "Redirect References",
                "No real objs/tobj entries declare any of this group's other names.")
            return
        reply = QMessageBox.question(
            self, "Redirect References",
            f"Update {len(refs)} real entrie(s) declaring {', '.join(old_names)} to "
            f"declare '{new_name}' instead? A backup is made before writing.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return
        touched = redirect_txd_references(self.master_ide_result, old_names, new_name)
        failures = []
        for basename in touched:
            source_path = next((p for p in self.master_ide_result.source_files
                                 if os.path.basename(p) == basename), None)
            if not source_path or not write_source_file(self.master_ide_result, source_path):
                failures.append(basename)
        if failures:
            QMessageBox.warning(self, "Write Failed", f"Failed to write: {', '.join(failures)}")
            return
        QMessageBox.information(self, "Redirected",
            f"Updated {len(refs)} entrie(s). File(s) written: {', '.join(touched)}")
        self.accept()
