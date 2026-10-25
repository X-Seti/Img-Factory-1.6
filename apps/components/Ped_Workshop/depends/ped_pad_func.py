#this belongs in apps/components/Ped_Workshop/depends/ped_pad_func.py - Version: 1
# X-Seti - October 22 2026 - IMG Factory 1.6 - Ped Workshop controller walk

"""
Walk the ped around with a game controller: left stick walks / runs in
that direction (ped turns to face it, grid slides under the feet), right
stick orbits, L2 / R2 zoom, Cross back to idle at the start.
"""

##class PedPadMixin: -
# _ped_pad_anim
# _ped_pad_state
# _ped_pad_toggle

import math

from PyQt6.QtCore import Qt
from PyQt6.QtWidgets import QMessageBox

from apps.components.Ped_Workshop.depends.ped_anim_func import FPS

WALK_ANIMS = ('walk_player', 'walk_civi', 'walk_gang1', 'walk_old')
RUN_ANIMS = ('run_player', 'run_civi', 'run_gang1', 'run_old')
IDLE_ANIMS = ('idle_stance', 'idle_hbhb', 'idle_armed')
WALK_SPEED, RUN_SPEED = 1.4, 4.5            # metres per second


class PedPadMixin: #vers 1
    """Controller walk mode for PedWorkshop."""

    def _ped_pad_toggle(self, on: bool): #vers 1
        """Start / stop the controller walk."""
        from apps.methods.gamepad_input import GamepadPoller
        pad = getattr(self, '_ped_pad', None)
        if on and pad is None:
            if self._animator is None:
                self._set_status("Controller walk: open a ped first")
                self._pad_btn.setChecked(False)
                return
            pad = GamepadPoller(self)
            pad.connected.connect(lambda n: self._set_status(
                f"Controller connected: {n}" if n else "Controller disconnected"))
            pad.state.connect(self._ped_pad_state)
            try:
                pad.start()
            except ImportError:
                QMessageBox.warning(self, "Game Controller", "Controller support needs pygame 2:\n\npip install pygame")
                self._pad_btn.setChecked(False)
                return
            self._ped_pad = pad
            self._animator.pad_walk = True
            self._anim_play(False)
            self._play_btn.setChecked(False)
            self._inplace_chk.setChecked(True)
            self._set_status("Controller walk: left stick walk / run, right stick orbit, L2 / R2 zoom")
        elif not on and pad is not None:
            pad.stop()
            self._ped_pad = None
            if self._animator is not None:
                self._animator.walk_pos, self._animator.pad_walk = (0.0, 0.0), False
                self._anim_render()

    def _ped_pad_anim(self, names) -> bool: #vers 1
        """Select the first anim found from names (case-insensitive)."""
        cur = self._anim_list.currentItem()
        if cur is not None and cur.text().lower() in names:
            return True
        for n in names:
            items = self._anim_list.findItems(n, Qt.MatchFlag.MatchFixedString)
            if items:
                self._anim_list.setCurrentItem(items[0])
                return True
        return False

    def _ped_pad_state(self, st: dict): #vers 1
        """One controller tick: move, face, animate, camera."""
        an = self._animator
        if an is None:
            return
        vp, dt = self.preview_widget, st['dt']
        vp._yaw += st['rx'] * 120.0 * dt                  # right stick orbit
        vp._pitch = max(5.0, min(175.0, vp._pitch + st['ry'] * 90.0 * dt))
        if st['lt'] or st['rt']:
            vp._dist *= 1.0 + (st['lt'] - st['rt']) * dt
        mag = min(1.0, math.hypot(st['lx'], st['ly']))
        if 'a' in st['pressed']:                          # Cross: idle at the start
            an.walk_pos = (0.0, 0.0)
            self._ped_pad_anim(IDLE_ANIMS)
            self._anim_set_time(0.0)
            return
        if mag < 0.1:
            vp.update()
            return
        running = mag > 0.7
        if not self._ped_pad_anim(RUN_ANIMS if running else WALK_ANIMS):
            self._set_status("Controller walk: no walk / run anim in this IFP")
            return
        an.heading = math.degrees(math.atan2(-st['lx'], -st['ly']))   # stick up = +Y
        speed = (RUN_SPEED if running else WALK_SPEED) * mag
        h = math.radians(an.heading)
        x, y = an.walk_pos
        an.walk_pos = (x - math.sin(h) * speed * dt, y + math.cos(h) * speed * dt)
        dur = an.anim.duration() if an.anim else 0.0
        t = (an.time + dt * self._speed_spin.value()) % dur if dur else 0.0
        self._time_slider.blockSignals(True)
        self._time_slider.setValue(int(round(t * FPS)))
        self._time_slider.blockSignals(False)
        self._anim_set_time(t)
