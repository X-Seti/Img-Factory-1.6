#this belongs in apps/components/Vehicle_Workshop/depends/vehicle_drive_func.py - Version: 2
# X-Seti - November 07 2026 - IMG Factory 1.6 - Vehicle Workshop drive mode

"""
Drive ribbon: Drive on / off (keyboard or controller), hour of the day
(lit from the game's data/timecyc.dat) and a ground image. The car's
handling.cfg line sets top speed, acceleration, braking and steer lock.
Drive logic: apps/methods/vehicle_drive.py.
"""

##class VehicleDriveMixin: -
# _build_drive_ribbon
# _drive_ground
# _drive_hour
# _drive_sim
# _drive_toggle

import os

from PyQt6.QtWidgets import QFileDialog, QSpinBox


class VehicleDriveMixin: #vers 1
    """Drive the loaded car in the 3D preview."""

    def _build_drive_ribbon(self): #vers 2
        B = self.ribbon_button
        tb = self.ribbon_toolbar("Drive")
        self._drive_btn = B(tb, "play_icon", "Drive the car: W/S A/D Space, or a controller (R2 / L2 / stick)",
                            self._drive_toggle, checkable=True, text="Drive")
        self.ribbon_label(tb, "Hour")
        self._drive_hour_spin = QSpinBox()
        self._drive_hour_spin.setRange(0, 23)
        self._drive_hour_spin.setValue(12)
        self._drive_hour_spin.setToolTip("Time of day from the game's timecyc.dat")
        self._drive_hour_spin.valueChanged.connect(self._drive_hour)
        tb.addWidget(self._drive_hour_spin)
        B(tb, "import_icon", "Ground image for drive mode (tiles every 8 m)", self._drive_ground, text="Ground")
        tb.addSeparator()
        tb.addWidget(self._vw_grid_button())                     # grid style, as Ped Workshop

    def _drive_sim(self): #vers 1
        """DriveSim from the car's handling.cfg line (defaults when none)."""
        from apps.methods.vehicle_drive import DriveSim
        sim = DriveSim()
        stem = os.path.splitext(os.path.basename(self._current_dff_path or ''))[0].lower()
        p = getattr(getattr(self, '_tab_handling', None), '_parser', None)
        entry = next((e for e in (getattr(p, 'entries', None) or []) if e.name.lower() == stem), None)
        if entry is None:
            return sim, "default handling"
        v = entry.values
        sa = len(v) <= 36                                     # SA lines are shorter than III / VC
        mv, acc = (11, 12) if sa else (12, 13)
        try:
            sim.max_speed = max(10.0, float(v[mv]) / 3.6)
            sim.accel = min(16.0, max(3.0, float(v[acc]) * (0.6 if sa else 0.3)))
            sim.brake = min(30.0, max(6.0, float(v[16]) * 2.0))
            sim.steer_lock = min(50.0, max(20.0, float(v[19])))
        except (ValueError, IndexError):
            return DriveSim(), "default handling"
        return sim, f"handling: {sim.max_speed * 3.6:.0f} km/h"

    def _drive_toggle(self, on: bool): #vers 1
        vp = self.viewport
        if not on:
            self._set_status(vp.set_drive(False))
            vp.set_timecyc_playing(False)
            return
        if not getattr(vp, '_all_geoms', None):
            self._set_status("Load a vehicle DFF first")
            self._drive_btn.setChecked(False)
            return
        sim, note = self._drive_sim()
        root = self._find_game_root()
        tc = next((os.path.join(root, 'data', n) for n in ('timecyc.dat', 'TIMECYC.DAT')
                   if root and os.path.isfile(os.path.join(root, 'data', n))), '')
        if tc:
            vp.set_timecyc_path(tc)
            vp.set_timecyc_hour(self._drive_hour_spin.value())
            vp.set_timecyc_playing(True)
        msg = vp.set_drive(True, sim)
        if getattr(self, '_ground_img', None):
            vp._drive.set_ground_image(*self._ground_img)
        self._set_status(f"{msg} | {note}" + (" | timecyc lit" if tc else ""))

    def _drive_hour(self, hour: int): #vers 1
        vp = self.viewport
        vp.set_timecyc_hour(hour)
        vp.update()

    def _drive_ground(self): #vers 1
        """Pick an image as the drive ground tile."""
        from PyQt6.QtGui import QImage
        p, _ = QFileDialog.getOpenFileName(self, "Ground image", '', "Images (*.png *.jpg *.jpeg *.bmp)")
        if not p:
            return
        img = QImage(p).convertToFormat(QImage.Format.Format_RGBA8888)
        if img.isNull():
            return
        drive = getattr(self.viewport, '_drive', None)
        if drive is None:
            self._set_status("Ground set - press Drive")
        rgba = bytes(img.constBits().asstring(img.sizeInBytes()))
        self._ground_img = (rgba, img.width(), img.height())
        if drive is not None:
            drive.set_ground_image(*self._ground_img)
