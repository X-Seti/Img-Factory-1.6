#this belongs in apps/components/Capture_Workshop/depends/capture_bugs_func.py - Version: 1
# X-Seti - October 14 2026 - IMG Factory 1.6 - Capture Workshop bug tracker

"""
Bug tracker on the bugs folder (TODO_bugs). Each bug is a file named after it
(text = description); screenshots are '<title> N.png' beside it. Priority,
status and the comment thread (replies, images) live in .bugtracker.json there.
"""

##Methods list -
# load_tracker
# save_tracker
# scan_bugs

##class CaptureBugsMixin: -
# _add_comment
# _add_images
# _bug_colour
# _bug_meta
# _bug_menu
# _bug_selected
# _bugs_dir
# _changelog_path
# _choose_bugs_folder
# _clean_fixed
# _comment_selected
# _create_bug_view
# _create_bugs_tab
# _current_bug
# _load_bugs
# _new_bug
# _next_image_name
# _refresh_thread
# _save_description
# _save_meta
# _set_priority
# _set_status_fixed
# _write_changelog

import json
import re
import shutil
from datetime import datetime
from pathlib import Path

from PyQt6.QtCore import QFile, QSize, Qt, QUrl
from PyQt6.QtGui import QBrush, QColor, QDesktopServices, QFont, QIcon, QPixmap
from PyQt6.QtWidgets import (
    QComboBox, QDialog, QDialogButtonBox, QFileDialog, QFormLayout, QFrame, QHBoxLayout,
    QInputDialog, QLabel, QLineEdit, QListWidget, QListWidgetItem, QMenu, QMessageBox,
    QPlainTextEdit, QPushButton, QSplitter, QTreeWidget, QTreeWidgetItem, QVBoxLayout)

from apps.methods.imgfactory_svg_icons import SVGIconFactory

IMAGE_EXT = ('.png', '.jpg', '.jpeg', '.bmp')
TRACKER = '.bugtracker.json'
PRIORITIES = ('high', 'mid', 'low', 'none')
PRIORITY_LABEL = {'high': "High Priority", 'mid': "Mid Priority", 'low': "Low Priority", 'none': ""}
PRIORITY_COLOUR = {'high': '#e05555', 'mid': '#e0a030', 'low': '#5aa0e0', 'none': None}
FIXED_COLOUR = '#50b050'


def scan_bugs(folder): #vers 1
    """[{'title', 'text', 'images', 'dir'}] - images join the bug whose name
    they start with ('map bug 2.png' joins 'map bug'); folders listed as-is."""
    folder = Path(folder)
    entries = sorted(p for p in folder.iterdir() if not p.name.startswith('.'))
    bugs, by_key = [], {}
    for p in entries:
        if p.is_dir():
            bugs.append({'title': p.name, 'text': None, 'images': [], 'dir': p})
        elif p.suffix.lower() not in IMAGE_EXT:
            title = p.stem if p.suffix.lower() == '.txt' else p.name
            bug = {'title': title, 'text': p, 'images': [], 'dir': None}
            bugs.append(bug)
            by_key[title.lower()] = bug
    for p in entries:
        if p.is_file() and p.suffix.lower() in IMAGE_EXT:
            base = re.sub(r'\s*\d+$', '', p.stem).strip()
            bug = by_key.get(base.lower())
            if bug is None:
                bug = {'title': base, 'text': None, 'images': [], 'dir': None}
                bugs.append(bug)
                by_key[base.lower()] = bug
            bug['images'].append(p)
    return sorted(bugs, key=lambda b: b['title'].lower())


def load_tracker(folder): #vers 1
    p = Path(folder) / TRACKER
    return json.loads(p.read_text()) if p.exists() else {'bugs': {}}


def save_tracker(folder, data): #vers 1
    (Path(folder) / TRACKER).write_text(json.dumps(data, indent=2))


class CaptureBugsMixin: #vers 1
    """Bugs tab + bug view (priority, status, description, comment thread)."""

    # UI

    def _create_bugs_tab(self): #vers 1
        page = QFrame()
        lay = QVBoxLayout(page)
        lay.setContentsMargins(0, 2, 0, 0)
        self._bug_filter = QComboBox()
        self._bug_filter.addItems(["Open", "Fixed", "All"])
        self._bug_filter.currentIndexChanged.connect(lambda _i: self._load_bugs())
        lay.addWidget(self._bug_filter)
        self._bugs_list = QListWidget()
        self._bugs_list.setIconSize(QSize(48, 27))
        self._bugs_list.setWordWrap(True)
        self._bugs_list.currentItemChanged.connect(self._bug_selected)
        self._bugs_list.setContextMenuPolicy(Qt.ContextMenuPolicy.CustomContextMenu)
        self._bugs_list.customContextMenuRequested.connect(self._bug_menu)
        lay.addWidget(self._bugs_list)
        self._bug_count = QLabel()
        self._bug_count.setFont(self.infobar_font)
        lay.addWidget(self._bug_count)
        row = QHBoxLayout()
        b = QPushButton("New Bug")
        b.clicked.connect(self._new_bug)
        row.addWidget(b)
        b = QPushButton("Fixed")
        b.setToolTip("Mark the selected bug fixed (ChangeLog line)")
        b.clicked.connect(self._set_status_fixed)
        row.addWidget(b)
        lay.addLayout(row)
        return page

    def _create_bug_view(self): #vers 1
        """Centre page: title, priority, status, description, thread, comment box."""
        page = QFrame()
        lay = QVBoxLayout(page)
        lay.setContentsMargins(6, 6, 6, 6)
        self._bug_title = QLabel()
        f = QFont(self.title_font)
        f.setBold(True)
        self._bug_title.setFont(f)
        self._bug_title.setWordWrap(True)
        lay.addWidget(self._bug_title)
        row = QHBoxLayout()
        row.addWidget(QLabel("Priority:"))
        self._bug_prio = QComboBox()
        for k in PRIORITIES:
            self._bug_prio.addItem(PRIORITY_LABEL[k] or "None", k)
        self._bug_prio.activated.connect(lambda _i: self._set_priority(self._bug_prio.currentData()))
        row.addWidget(self._bug_prio)
        self._bug_state = QLabel()
        row.addWidget(self._bug_state)
        row.addStretch()
        b = QPushButton("Mark Fixed")
        b.clicked.connect(self._set_status_fixed)
        self._bug_fix_btn = b
        row.addWidget(b)
        lay.addLayout(row)

        sp = QSplitter(Qt.Orientation.Vertical)
        top = QFrame()
        tl = QVBoxLayout(top)
        tl.setContentsMargins(0, 0, 0, 0)
        tl.addWidget(QLabel("Description"))
        self._bug_desc = QPlainTextEdit()
        tl.addWidget(self._bug_desc)
        b = QPushButton("Save Description")
        b.clicked.connect(self._save_description)
        tl.addWidget(b, 0, Qt.AlignmentFlag.AlignRight)
        sp.addWidget(top)

        mid = QFrame()
        ml = QVBoxLayout(mid)
        ml.setContentsMargins(0, 0, 0, 0)
        ml.addWidget(QLabel("Comments"))
        self._bug_thread = QTreeWidget()
        self._bug_thread.setHeaderLabels(["Comment", "Date"])
        self._bug_thread.setColumnWidth(0, 420)
        self._bug_thread.setWordWrap(True)
        self._bug_thread.currentItemChanged.connect(self._comment_selected)
        ml.addWidget(self._bug_thread)
        self._bug_images = QListWidget()
        self._bug_images.setViewMode(QListWidget.ViewMode.IconMode)
        self._bug_images.setIconSize(QSize(160, 90))
        self._bug_images.setFixedHeight(124)
        self._bug_images.setToolTip("Screenshots of the selected comment (double-click to open)")
        self._bug_images.itemDoubleClicked.connect(
            lambda it: QDesktopServices.openUrl(QUrl.fromLocalFile(it.data(Qt.ItemDataRole.UserRole))))
        ml.addWidget(self._bug_images)
        sp.addWidget(mid)

        bot = QFrame()
        bl = QVBoxLayout(bot)
        bl.setContentsMargins(0, 0, 0, 0)
        self._bug_reply_lbl = QLabel("New comment")
        bl.addWidget(self._bug_reply_lbl)
        self._bug_comment = QPlainTextEdit()
        self._bug_comment.setPlaceholderText("Write a comment, or select a comment above to reply")
        bl.addWidget(self._bug_comment)
        row = QHBoxLayout()
        self._pending_images = []
        self._pending_lbl = QLabel()
        row.addWidget(self._pending_lbl)
        row.addStretch()
        b = QPushButton("Add Images...")
        b.clicked.connect(lambda: self._add_images(False))
        row.addWidget(b)
        b = QPushButton("Add Selected Capture")
        b.clicked.connect(lambda: self._add_images(True))
        row.addWidget(b)
        b = QPushButton("Post")
        b.clicked.connect(self._add_comment)
        row.addWidget(b)
        bl.addLayout(row)
        sp.addWidget(bot)
        sp.setSizes([120, 320, 140])
        lay.addWidget(sp, 1)
        return page

    # Data

    def _bugs_dir(self): #vers 1
        """Bugs folder, or None until chosen."""
        d = self.WS.get("bugs_folder", "")
        return Path(d) if d and Path(d).is_dir() else None

    def _choose_bugs_folder(self): #vers 1
        d = QFileDialog.getExistingDirectory(self, "Bugs folder (TODO_bugs)", self.WS.get("bugs_folder", ""))
        if d:
            self.WS.set("bugs_folder", d)
            self.WS.save()
            self._load_bugs()

    def _changelog_path(self): #vers 1
        """ChangeLog.md: saved choice, else the project root one."""
        p = Path(self.WS.get("changelog_file", "") or Path(__file__).resolve().parents[4] / 'ChangeLog.md')
        return p if p.is_file() else None

    def _bug_meta(self, bug, create=True): #vers 1
        """Tracker entry; new entries take priority from the file name prefix."""
        bugs = self._tracker['bugs']
        if bug['title'] not in bugs and create:
            m = re.match(r'(High|Mid|Low) Priority', bug['title'], re.I)
            bugs[bug['title']] = {'priority': m.group(1).lower() if m else 'none', 'status': 'open',
                                  'created': datetime.now().strftime('%Y-%m-%d %H:%M'),
                                  'comments': []}
        return bugs.get(bug['title'])

    def _save_meta(self): #vers 1
        save_tracker(self._bugs_dir(), self._tracker)

    def _current_bug(self): #vers 1
        item = self._bugs_list.currentItem()
        return item.data(Qt.ItemDataRole.UserRole) if item else None

    def _bug_colour(self, meta): #vers 1
        return FIXED_COLOUR if meta['status'] == 'fixed' else PRIORITY_COLOUR[meta['priority']]

    def _load_bugs(self, select=None): #vers 1
        """Fill the Bugs list: filter Open/Fixed/All, coloured by priority."""
        self._bugs_list.clear()
        d = self._bugs_dir()
        if d is None:
            self._tracker = {'bugs': {}}
            self._bug_count.setText("Choose the bugs folder (Bugs ribbon)")
            return
        self._tracker = load_tracker(d)
        want = self._bug_filter.currentText()
        order = {k: i for i, k in enumerate(PRIORITIES)}
        bugs = scan_bugs(d)
        metas = {b['title']: self._bug_meta(b) for b in bugs}
        n_open = 0
        for bug in sorted(bugs, key=lambda b: (metas[b['title']]['status'] == 'fixed',
                                               order[metas[b['title']]['priority']], b['title'].lower())):
            meta = metas[bug['title']]
            n_open += meta['status'] == 'open'
            if want != "All" and meta['status'] != want.lower():
                continue
            item = QListWidgetItem(bug['title'])
            item.setData(Qt.ItemDataRole.UserRole, bug)
            colour = self._bug_colour(meta)
            if colour:
                item.setForeground(QBrush(QColor(colour)))
            f = item.font()
            f.setBold(meta['priority'] == 'high' and meta['status'] == 'open')
            f.setStrikeOut(meta['status'] == 'fixed')
            item.setFont(f)
            n_comments = len(meta['comments'])
            item.setToolTip(f"{PRIORITY_LABEL[meta['priority']] or 'No priority'}  |  {meta['status']}"
                            f"  |  {n_comments} comment(s)")
            if bug['images']:
                item.setIcon(QIcon(QPixmap(str(bug['images'][0])).scaled(
                    self._bugs_list.iconSize(), Qt.AspectRatioMode.KeepAspectRatio)))
            elif bug['dir']:
                item.setIcon(SVGIconFactory.folder_icon(20, self._get_icon_color()))
            else:
                item.setIcon(SVGIconFactory.bug_report_icon(20, colour or self._get_icon_color()))
            self._bugs_list.addItem(item)
            if select and bug['title'] == select:
                self._bugs_list.setCurrentItem(item)
        self._save_meta()
        self._bug_count.setText(f"{n_open} open, {len(bugs) - n_open} fixed")

    # Selection / view

    def _bug_selected(self, item, _prev=None): #vers 1
        """Show the bug in the centre view."""
        if item is None:
            return
        bug = item.data(Qt.ItemDataRole.UserRole)
        meta = self._bug_meta(bug)
        self._centre_stack.setCurrentIndex(1)
        self._bug_title.setText(bug['title'])
        self._bug_title.setStyleSheet(f"color: {self._bug_colour(meta)};" if self._bug_colour(meta) else "")
        self._bug_prio.setCurrentIndex(PRIORITIES.index(meta['priority']))
        fixed = meta['status'] == 'fixed'
        self._bug_state.setText(f"Fixed {meta.get('fixed', '')}" if fixed else f"Open since {meta['created']}")
        self._bug_fix_btn.setText("Reopen" if fixed else "Mark Fixed")
        self._bug_desc.setPlainText(bug['text'].read_text(errors='replace') if bug['text'] else "")
        self._bug_desc.setReadOnly(bug['text'] is None and bug['dir'] is not None)
        self._pending_images = []
        self._pending_lbl.setText("")
        self._refresh_thread(bug, meta)

    def _refresh_thread(self, bug, meta): #vers 1
        """Thread: screenshots not in a comment first, then comments with replies."""
        t = self._bug_thread
        t.clear()
        used = {i for c in meta['comments'] for i in c.get('images', [])}
        loose = [p.name for p in bug['images'] if p.name not in used]
        if loose:
            it = QTreeWidgetItem(["Screenshots", ""])
            it.setData(0, Qt.ItemDataRole.UserRole, {'images': loose})
            t.addTopLevelItem(it)
        nodes = {}
        for c in meta['comments']:
            text = c['text'] + (f"   [{len(c['images'])} image(s)]" if c.get('images') else "")
            it = QTreeWidgetItem([text, c['date']])
            it.setData(0, Qt.ItemDataRole.UserRole, c)
            it.setToolTip(0, c['text'])
            parent = nodes.get(c.get('parent'))
            (parent.addChild(it) if parent else t.addTopLevelItem(it))
            nodes[c['id']] = it
        t.expandAll()
        self._bug_images.clear()
        self._bug_reply_lbl.setText("New comment")

    def _comment_selected(self, item, _prev=None): #vers 1
        """Show the comment's images; replies go under it."""
        self._bug_images.clear()
        if item is None:
            self._bug_reply_lbl.setText("New comment")
            return
        c = item.data(0, Qt.ItemDataRole.UserRole)
        d = self._bugs_dir()
        for name in c.get('images', []):
            p = d / name
            li = QListWidgetItem(QIcon(QPixmap(str(p)).scaled(160, 90, Qt.AspectRatioMode.KeepAspectRatio)), name)
            li.setData(Qt.ItemDataRole.UserRole, str(p))
            self._bug_images.addItem(li)
        if 'id' in c:
            self._bug_reply_lbl.setText(f"Reply to: {c['text'][:60]}")
        else:
            self._bug_reply_lbl.setText("New comment")

    # Edits

    def _set_priority(self, prio): #vers 1
        bug = self._current_bug()
        if bug is None:
            return
        self._bug_meta(bug)['priority'] = prio
        self._save_meta()
        self._load_bugs(select=bug['title'])

    def _save_description(self): #vers 1
        bug = self._current_bug()
        if bug is None or bug['dir']:
            return
        path = bug['text'] or (self._bugs_dir() / bug['title'])
        path.write_text(self._bug_desc.toPlainText())
        self._load_bugs(select=bug['title'])
        self._set_status("Description saved")

    def _next_image_name(self, title, suffix): #vers 1
        """'<title> N<suffix>' not yet used in the bugs folder."""
        d, n = self._bugs_dir(), 1
        while (d / f"{title} {n}{suffix}").exists():
            n += 1
        return f"{title} {n}{suffix}"

    def _add_images(self, from_capture): #vers 1
        """Queue images for the next comment (files or the selected capture)."""
        if self._current_bug() is None:
            return
        if from_capture:
            cap = self._list.currentItem()
            if cap is None or not cap.text().lower().endswith(IMAGE_EXT):
                self._set_status("Select an image in the Captures tab first")
                return
            paths = [cap.data(Qt.ItemDataRole.UserRole)]
        else:
            paths, _f = QFileDialog.getOpenFileNames(self, "Images", str(self._output_dir()),
                                                     "Images (*.png *.jpg *.jpeg *.bmp)")
        self._pending_images += paths
        self._pending_lbl.setText(f"{len(self._pending_images)} image(s) to attach")

    def _add_comment(self): #vers 1
        """Post the comment (a reply when a comment is selected) with queued images."""
        bug = self._current_bug()
        text = self._bug_comment.toPlainText().strip()
        if bug is None or not (text or self._pending_images):
            return
        meta = self._bug_meta(bug)
        sel = self._bug_thread.currentItem()
        parent = sel.data(0, Qt.ItemDataRole.UserRole).get('id') if sel else None
        names = []
        for src in self._pending_images:
            name = self._next_image_name(bug['title'], Path(src).suffix.lower())
            shutil.copy2(src, self._bugs_dir() / name)
            names.append(name)
        cid = max((c['id'] for c in meta['comments']), default=0) + 1
        meta['comments'].append({'id': cid, 'parent': parent, 'text': text,
                                 'date': datetime.now().strftime('%Y-%m-%d %H:%M'), 'images': names})
        self._save_meta()
        self._bug_comment.clear()
        self._load_bugs(select=bug['title'])

    def _new_bug(self): #vers 1
        """Dialog: priority, title, description, attach the selected capture."""
        d = self._bugs_dir()
        if d is None:
            self._choose_bugs_folder()
            d = self._bugs_dir()
            if d is None:
                return
        dlg = QDialog(self)
        dlg.setWindowTitle("New Bug")
        form = QFormLayout(dlg)
        prio = QComboBox()
        for k in PRIORITIES:
            prio.addItem(PRIORITY_LABEL[k] or "None", k)
        prio.setCurrentIndex(PRIORITIES.index('none'))
        title = QLineEdit()
        title.setPlaceholderText("Workshop - what goes wrong")
        desc = QPlainTextEdit()
        cap = self._list.currentItem()
        attach = QComboBox()
        attach.addItem("No screenshot")
        if cap and cap.text().lower().endswith(IMAGE_EXT):
            attach.addItem(f"Attach {cap.text()}")
            attach.setCurrentIndex(1)
        form.addRow("Priority:", prio)
        form.addRow("Title:", title)
        form.addRow("Description:", desc)
        form.addRow("Screenshot:", attach)
        bb = QDialogButtonBox(QDialogButtonBox.StandardButton.Ok | QDialogButtonBox.StandardButton.Cancel)
        bb.accepted.connect(dlg.accept)
        bb.rejected.connect(dlg.reject)
        form.addRow(bb)
        if dlg.exec() != QDialog.DialogCode.Accepted or not title.text().strip():
            return
        p = prio.currentData()
        name = re.sub(r'[\\/:*?"<>|]', '-', title.text().strip())
        if p != 'none':
            name = f"{PRIORITY_LABEL[p]} - {name}"
        if (d / name).exists():
            QMessageBox.warning(self, "New Bug", f"'{name}' already exists")
            return
        (d / name).write_text(desc.toPlainText().strip())
        if attach.currentIndex() == 1:
            src = Path(cap.data(Qt.ItemDataRole.UserRole))
            shutil.copy2(src, d / f"{name} 1{src.suffix.lower()}")
        self._bug_filter.setCurrentText("Open")
        self._load_bugs(select=name)
        self._set_status(f"Bug added: {name}")

    def _set_status_fixed(self): #vers 1
        """Mark fixed (ChangeLog line) or reopen."""
        bug = self._current_bug()
        if bug is None:
            return
        meta = self._bug_meta(bug)
        if meta['status'] == 'fixed':
            meta['status'] = 'open'
            meta.pop('fixed', None)
        else:
            log = self._changelog_path()
            note, ok = QInputDialog.getText(self, "Bug fixed", "ChangeLog line (30 words max):",
                                            text=re.sub(r'^(High|Mid|Low) Priority - ', '', bug['title']))
            if not ok:
                return
            if log and note.strip():
                self._write_changelog(log, bug['title'], note.strip())
            meta['status'] = 'fixed'
            meta['fixed'] = datetime.now().strftime('%Y-%m-%d %H:%M')
        self._save_meta()
        self._load_bugs(select=bug['title'])

    def _write_changelog(self, path, title, note): #vers 1
        """Insert '## <date> - <title>' + '- note' above the first ChangeLog entry."""
        lines = path.read_text().split('\n')
        at = next((i for i, l in enumerate(lines) if l.startswith('## ')), len(lines))
        head = f"## {datetime.now().strftime('%b %d %Y')} - {re.sub(r'^(High|Mid|Low) Priority - ', '', title)}"
        lines[at:at] = [head, f"- {note}", ""]
        path.write_text('\n'.join(lines))

    def _clean_fixed(self): #vers 1
        """Move every fixed bug's files to the trash (job done)."""
        d = self._bugs_dir()
        fixed = [b for b in scan_bugs(d) if self._bug_meta(b)['status'] == 'fixed']
        if not fixed or QMessageBox.question(
                self, "Clean Up", f"Move {len(fixed)} fixed bug(s) and their screenshots to the trash?"
        ) != QMessageBox.StandardButton.Yes:
            return
        for b in fixed:
            for p in ([b['text']] if b['text'] else []) + b['images'] + ([b['dir']] if b['dir'] else []):
                QFile.moveToTrash(str(p))
            self._tracker['bugs'].pop(b['title'], None)
        self._save_meta()
        self._load_bugs()

    def _bug_menu(self, pos): #vers 1
        """Right-click on the bugs list."""
        item = self._bugs_list.itemAt(pos)
        menu = QMenu(self)
        if item:
            self._bugs_list.setCurrentItem(item)
            meta = self._bug_meta(item.data(Qt.ItemDataRole.UserRole))
            pm = menu.addMenu("Priority")
            for k in PRIORITIES:
                act = pm.addAction(PRIORITY_LABEL[k] or "None", lambda k=k: self._set_priority(k))
                act.setCheckable(True)
                act.setChecked(meta['priority'] == k)
            menu.addAction("Reopen" if meta['status'] == 'fixed' else "Mark Fixed", self._set_status_fixed)
            menu.addSeparator()
        menu.addAction("New Bug...", self._new_bug)
        menu.addAction("Clean Up Fixed Bugs...", self._clean_fixed)
        menu.addAction("Open Bugs Folder", lambda: self._bugs_dir() and QDesktopServices.openUrl(
            QUrl.fromLocalFile(str(self._bugs_dir()))))
        menu.addAction("Choose Bugs Folder...", self._choose_bugs_folder)
        menu.addAction("Refresh", self._load_bugs)
        menu.exec(self._bugs_list.mapToGlobal(pos))
