#!/usr/bin/env python3
#this belongs in apps/methods/gamepad_input.py - Version: 1
# X-Seti - September24 2026 - IMG Factory 1.6 - Game controller input (SDL via pygame)

"""
Game controller input - polls one SDL game controller (PS5 DualSense, PS4,
Xbox, Switch Pro) through pygame's SDL2 GameController API and emits a
normalised state ~60 times a second. Also drives rumble for feedback.
Requires pygame 2 (pip install pygame).
"""

##Methods list -

##class GamepadPoller: -
# __init__
# _connect
# _poll
# rumble
# start
# stop

import os
import time

from PyQt6.QtCore import QObject, QTimer, pyqtSignal

# SDL game controller button names (PS labels in brackets)
BUTTONS = {
    'a': 'CONTROLLER_BUTTON_A',             # Cross
    'b': 'CONTROLLER_BUTTON_B',             # Circle
    'x': 'CONTROLLER_BUTTON_X',             # Square
    'y': 'CONTROLLER_BUTTON_Y',             # Triangle
    'back': 'CONTROLLER_BUTTON_BACK',       # Create
    'start': 'CONTROLLER_BUTTON_START',     # Options
    'l3': 'CONTROLLER_BUTTON_LEFTSTICK',
    'r3': 'CONTROLLER_BUTTON_RIGHTSTICK',
    'l1': 'CONTROLLER_BUTTON_LEFTSHOULDER',
    'r1': 'CONTROLLER_BUTTON_RIGHTSHOULDER',
    'up': 'CONTROLLER_BUTTON_DPAD_UP',
    'down': 'CONTROLLER_BUTTON_DPAD_DOWN',
    'left': 'CONTROLLER_BUTTON_DPAD_LEFT',
    'right': 'CONTROLLER_BUTTON_DPAD_RIGHT',
}
TOUCHPAD_BUTTON = 20                        # SDL_CONTROLLER_BUTTON_TOUCHPAD (SDL 2.0.14+)


class GamepadPoller(QObject):
    """state signal: dict(lx, ly, rx, ry, lt, rt, held=set, pressed=set, dt, name)."""
    state = pyqtSignal(dict)
    connected = pyqtSignal(str)             # controller name, '' when unplugged

    def __init__(self, parent=None, deadzone=0.15): #vers 1
        super().__init__(parent)
        self.deadzone = deadzone
        self._pg = None
        self._sdlc = None
        self._pad = None
        self._held = set()
        self._t = time.monotonic()
        self._next_scan = 0.0
        self._timer = QTimer(self)
        self._timer.timeout.connect(self._poll)

    def start(self): #vers 1
        """Initialise SDL controller support; raises ImportError if pygame is missing."""
        os.environ.setdefault('SDL_VIDEODRIVER', 'dummy')                 # no SDL window
        os.environ['SDL_JOYSTICK_ALLOW_BACKGROUND_EVENTS'] = '1'           # read input while Qt has focus
        os.environ.setdefault('PYGAME_HIDE_SUPPORT_PROMPT', '1')
        import pygame
        from pygame._sdl2 import controller as sdlc
        pygame.display.init()
        pygame.joystick.init()
        sdlc.init()
        self._pg, self._sdlc = pygame, sdlc
        self._btn_ids = {k: getattr(pygame, v) for k, v in BUTTONS.items()}
        self._btn_ids['touchpad'] = getattr(pygame, 'CONTROLLER_BUTTON_TOUCHPAD', TOUCHPAD_BUTTON)
        self._axes = [getattr(pygame, 'CONTROLLER_AXIS_' + n) for n in
                      ('LEFTX', 'LEFTY', 'RIGHTX', 'RIGHTY', 'TRIGGERLEFT', 'TRIGGERRIGHT')]
        self._connect()
        self._timer.start(16)

    def stop(self): #vers 1
        self._timer.stop()
        self._pad = None
        self._held = set()

    def _connect(self): #vers 1
        """Open the first SDL game controller, if any."""
        self._pg.event.pump()
        for i in range(self._sdlc.get_count()):
            if self._sdlc.is_controller(i):
                self._pad = self._sdlc.Controller(i)
                name = getattr(self._pad, 'name', '') or f"Controller {i}"
                self.connected.emit(name)
                return
        self._pad = None

    def _poll(self): #vers 1
        pg = self._pg
        pg.event.pump()
        now = time.monotonic()
        dt, self._t = min(0.1, now - self._t), now
        pad = self._pad
        if pad is None or not pad.attached():
            if pad is not None:
                self._pad = None
                self.connected.emit('')
            if now >= self._next_scan:
                self._next_scan = now + 1.0
                self._connect()
            return
        dz = self.deadzone

        def stick(v):
            v = max(-1.0, v / 32767.0)
            if abs(v) < dz:
                return 0.0
            return (v - dz if v > 0 else v + dz) / (1.0 - dz)
        a = [pad.get_axis(k) for k in self._axes]
        held = {k for k, b in self._btn_ids.items() if pad.get_button(b)}
        pressed = held - self._held
        self._held = held
        st = {'lx': stick(a[0]), 'ly': stick(a[1]), 'rx': stick(a[2]), 'ry': stick(a[3]),
              'lt': max(0.0, a[4] / 32767.0), 'rt': max(0.0, a[5] / 32767.0),
              'held': held, 'pressed': pressed, 'dt': dt}
        if pressed or any(abs(st[k]) > 0 for k in ('lx', 'ly', 'rx', 'ry', 'lt', 'rt')) or held:
            self.state.emit(st)

    def rumble(self, low=0.3, high=0.6, ms=80): #vers 1
        """Short vibration; ignored if the pad has no rumble."""
        if self._pad is not None:
            try:
                self._pad.rumble(low, high, ms)
            except (AttributeError, self._pg.error):
                pass
