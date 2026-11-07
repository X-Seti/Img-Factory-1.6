#this belongs in apps/methods/vehicle_drive.py - Version: 1
# X-Seti - November 07 2026 - IMG Factory 1.6 - Vehicle drive mode

"""
Drive the loaded vehicle in the viewport: a textured ground with lane
lines, cones / boxes / barrels / posts to drive around, simple arcade
car physics, keyboard (WASD / arrows, Space handbrake) or a controller
(R2 throttle, L2 brake / reverse, left stick steer, Cross handbrake).
The car stays at the origin; the ground moves under it.
"""

##Methods list -
# ground_rgba
# make_props
#
##class DriveSim: -
# __init__
# collide
# step
#
##class DriveController: -
# __init__
# _draw_props
# _ground_tex
# _inputs
# _tick
# draw
# key
# set_ground_image
# start
# stop

import math
import random
from typing import List, Optional, Tuple

import numpy as np

KEYS_FWD, KEYS_BACK, KEYS_LEFT, KEYS_RIGHT, KEYS_HAND = 'fwd', 'back', 'left', 'right', 'hand'
TILE = 8.0                                       # metres per ground texture repeat
FIELD = 300.0                                    # half size of the ground


def ground_rgba(size: int = 256) -> Tuple[bytes, int, int]: #vers 1
    """Asphalt tile with a dashed centre line (RGBA, size x size)."""
    rng = np.random.default_rng(7)
    g = rng.normal(70, 9, (size, size)).clip(30, 120)
    g += rng.normal(0, 4, (size // 8, size // 8)).repeat(8, 0).repeat(8, 1)
    img = np.stack([g, g, g * 1.04], -1).clip(0, 255)
    c = size // 2
    img[:size // 2, c - 3:c + 3] = (225, 200, 70)                 # dashed lane line
    a = np.full((size, size, 1), 255.0)
    return np.concatenate([img, a], -1).astype(np.uint8).tobytes(), size, size


def make_props(seed: int = 1, count: int = 90) -> List[dict]: #vers 1
    """Cones, boxes, barrels and posts scattered round the start (clear of the car)."""
    rnd = random.Random(seed)
    kinds = (('cone', 0.35, (1.0, 0.45, 0.05)), ('box', 0.9, (0.55, 0.42, 0.25)),
             ('barrel', 0.45, (0.15, 0.35, 0.75)), ('post', 0.2, (0.85, 0.85, 0.85)))
    out = []
    for i in range(count):
        kind, r, col = kinds[i % len(kinds)]
        while True:
            x, y = rnd.uniform(-FIELD * 0.6, FIELD * 0.6), rnd.uniform(-FIELD * 0.6, FIELD * 0.6)
            if math.hypot(x, y) > 12:
                break
        out.append({'kind': kind, 'x': x, 'y': y, 'r': r, 'col': col})
    for k in range(-6, 7):                                        # slalom line of cones ahead
        out.append({'kind': 'cone', 'x': (2.5 if k % 2 else -2.5), 'y': 20 + k * 6 + 40, 'r': 0.35,
                    'col': (1.0, 0.45, 0.05)})
    return out


class DriveSim: #vers 1
    """Arcade car: speed along heading, bicycle-model turning."""

    def __init__(self, max_speed: float = 45.0, accel: float = 9.0, brake: float = 16.0,
                 steer_lock: float = 35.0, wheelbase: float = 2.6): #vers 1
        self.max_speed, self.accel, self.brake = max_speed, accel, brake
        self.steer_lock, self.wheelbase = steer_lock, wheelbase
        self.x = self.y = 0.0
        self.heading = 0.0                     # radians, 0 = facing +Y
        self.speed = 0.0                       # m/s, negative = reversing
        self.steer = 0.0                       # degrees at the wheels

    def step(self, dt: float, throttle: float, brake: float, steer_in: float, hand: bool) -> None: #vers 1
        """Advance dt seconds; inputs throttle / brake 0..1, steer -1..1 (right +)."""
        if throttle > 0:
            self.speed += self.accel * throttle * dt
        if brake > 0:
            if self.speed > 0.5:
                self.speed -= self.brake * brake * dt
            else:
                self.speed -= self.accel * 0.6 * brake * dt          # reverse
        drag = 0.4 + 0.012 * abs(self.speed) + (6.0 if hand else 0.0)
        self.speed -= math.copysign(min(abs(self.speed), drag * dt), self.speed)
        self.speed = max(-self.max_speed * 0.3, min(self.max_speed, self.speed))
        lock = self.steer_lock * (1.0 - min(0.6, abs(self.speed) / self.max_speed * 0.7))
        target = steer_in * lock
        self.steer += (target - self.steer) * min(1.0, dt * 8.0)
        turn = self.speed / self.wheelbase * math.tan(math.radians(self.steer)) * dt
        if hand:
            turn *= 1.6                                                # handbrake swing
        self.heading += turn
        self.x += math.sin(self.heading) * self.speed * dt
        self.y += math.cos(self.heading) * self.speed * dt

    def collide(self, props: List[dict], radius: float = 1.6) -> Optional[dict]: #vers 1
        """Push out of the first prop touched; the car loses most of its speed."""
        for p in props:
            dx, dy = self.x - p['x'], self.y - p['y']
            d = math.hypot(dx, dy)
            lim = radius + p['r']
            if d < lim:
                d = d or 0.01
                self.x, self.y = p['x'] + dx / d * lim, p['y'] + dy / d * lim
                self.speed *= -0.25
                return p
        return None


class DriveController: #vers 1
    """Drive mode for a VehicleViewport: inputs, timer, ground and props drawing."""

    def __init__(self, vp, sim: Optional[DriveSim] = None): #vers 1
        self.vp = vp
        self.sim = sim or DriveSim()
        self.props = make_props()
        self.keys = set()
        self.pad = None
        self.pad_state = {}
        self._tex = None
        self._img = ground_rgba()
        self._timer = None
        self._last_steer = 0.0
        self._t = 0.0

    def start(self) -> str: #vers 1
        """Begin driving; returns a status line (controller found or keyboard)."""
        import time
        from PyQt6.QtCore import QTimer
        self._t = time.monotonic()
        self._timer = QTimer(self.vp)
        self._timer.timeout.connect(self._tick)
        self._timer.start(16)
        msg = "Drive: W/S or arrows, A/D steer, Space handbrake"
        try:
            from apps.methods.gamepad_input import GamepadPoller
            self.pad = GamepadPoller(self.vp)
            self.pad.state.connect(lambda st: self.pad_state.update(st))
            self.pad.start()
            msg += " - controller: R2 / L2 / left stick, Cross handbrake"
        except ImportError:
            self.pad = None
            msg += " (pip install pygame for a controller)"
        return msg

    def stop(self) -> None: #vers 1
        if self._timer:
            self._timer.stop()
        if self.pad:
            self.pad.stop()
        self.pad = None
        if self._tex is not None:
            try:
                from OpenGL.GL import glDeleteTextures
                self.vp.makeCurrent(); glDeleteTextures([self._tex]); self.vp.doneCurrent()
            except Exception:
                pass
        self._tex = None

    def key(self, k: str, down: bool) -> None: #vers 1
        (self.keys.add if down else self.keys.discard)(k)

    def _inputs(self) -> Tuple[float, float, float, bool]: #vers 1
        """(throttle, brake, steer, handbrake) from keys and the controller."""
        st = self.pad_state
        thr = max(1.0 if KEYS_FWD in self.keys else 0.0, st.get('rt', 0.0))
        brk = max(1.0 if KEYS_BACK in self.keys else 0.0, st.get('lt', 0.0))
        steer = (1.0 if KEYS_RIGHT in self.keys else 0.0) - (1.0 if KEYS_LEFT in self.keys else 0.0)
        if abs(st.get('lx', 0.0)) > abs(steer):
            steer = st['lx']
        hand = KEYS_HAND in self.keys or 'a' in st.get('held', set())
        if st and not st.get('held') and not any(abs(st.get(k, 0)) > 0 for k in ('lx', 'lt', 'rt')):
            st.clear()                                              # pad released
        return thr, brk, steer, hand

    def _tick(self) -> None: #vers 1
        import time
        now = time.monotonic()
        dt, self._t = min(0.05, now - self._t), now
        thr, brk, steer, hand = self._inputs()
        self.sim.step(dt, thr, brk, steer, hand)
        hit = self.sim.collide(self.props)
        if hit is not None and self.pad is not None:
            self.pad.rumble(0.5, 0.8, 120)
        if abs(self.sim.steer - self._last_steer) > 1.5:               # front wheels follow
            self._last_steer = self.sim.steer
            self.vp.set_wheel_heading(-self.sim.steer)
        self.vp.update()

    def set_ground_image(self, rgba: bytes, w: int, h: int) -> None: #vers 1
        """Use a picked image as the ground tile."""
        self._img = (rgba, w, h)
        if self._tex is not None:
            from OpenGL.GL import glDeleteTextures
            glDeleteTextures([self._tex])
        self._tex = None
        self.vp.update()

    def _ground_tex(self) -> int: #vers 1
        from OpenGL.GL import (GL_LINEAR, GL_LINEAR_MIPMAP_LINEAR, GL_REPEAT, GL_RGBA, GL_TEXTURE_2D,
                               GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MIN_FILTER, GL_TEXTURE_WRAP_S,
                               GL_TEXTURE_WRAP_T, GL_UNSIGNED_BYTE, glBindTexture, glGenerateMipmap,
                               glGenTextures, glTexImage2D, glTexParameteri)
        if self._tex is None:
            rgba, w, h = self._img
            self._tex = glGenTextures(1)
            glBindTexture(GL_TEXTURE_2D, self._tex)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT)
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, rgba)
            glGenerateMipmap(GL_TEXTURE_2D)
        return self._tex

    def draw(self, ground_z: float = 0.0) -> None: #vers 1
        """Ground and props in car space (car at the origin facing +Y)."""
        from OpenGL.GL import (GL_LIGHTING, GL_QUADS, GL_TEXTURE_2D, glBegin, glBindTexture, glColor3f,
                               glDisable, glEnable, glEnd, glPopMatrix, glPushMatrix, glRotatef,
                               glTexCoord2f, glTranslatef, glVertex3f)
        s = self.sim
        glPushMatrix()
        glDisable(GL_LIGHTING)
        glTranslatef(0, 0, ground_z)
        glRotatef(math.degrees(s.heading), 0, 0, 1)
        glTranslatef(-s.x, -s.y, 0)
        glEnable(GL_TEXTURE_2D)
        glBindTexture(GL_TEXTURE_2D, self._ground_tex())
        tint = getattr(self.vp, '_ambient_tint', (1.0, 1.0, 1.0))
        glColor3f(*tint)
        glBegin(GL_QUADS)
        f, t = FIELD, FIELD / TILE
        for (x, y, u, v) in ((-f, -f, -t, -t), (f, -f, t, -t), (f, f, t, t), (-f, f, -t, t)):
            glTexCoord2f(u, v)
            glVertex3f(x, y, -0.01)
        glEnd()
        glBindTexture(GL_TEXTURE_2D, 0)
        glDisable(GL_TEXTURE_2D)
        self._draw_props(tint)
        glPopMatrix()

    def _draw_props(self, tint) -> None: #vers 1
        from OpenGL.GL import GL_QUADS, GL_TRIANGLES, glBegin, glColor3f, glEnd, glVertex3f
        glBegin(GL_QUADS)
        for p in self.props:                                           # boxes, barrels, posts
            if p['kind'] == 'cone':
                continue
            x, y, r = p['x'], p['y'], p['r']
            h = {'box': 1.2, 'barrel': 1.0, 'post': 3.5}[p['kind']]
            sides = 4 if p['kind'] == 'box' else 8
            pts = [(x + r * math.cos(2 * math.pi * k / sides + math.pi / 4),
                    y + r * math.sin(2 * math.pi * k / sides + math.pi / 4)) for k in range(sides)]
            for k in range(sides):
                shade = 0.65 + 0.35 * abs(math.cos(2 * math.pi * k / sides))
                glColor3f(*(c * shade * t for c, t in zip(p['col'], tint)))
                (ax, ay), (bx, by) = pts[k], pts[(k + 1) % sides]
                glVertex3f(ax, ay, 0); glVertex3f(bx, by, 0); glVertex3f(bx, by, h); glVertex3f(ax, ay, h)
            if sides == 4:
                glColor3f(*(c * t for c, t in zip(p['col'], tint)))
                for ax, ay in pts:
                    glVertex3f(ax, ay, h)
        glEnd()
        glBegin(GL_TRIANGLES)
        for p in self.props:                                           # cones
            if p['kind'] != 'cone':
                continue
            x, y, r = p['x'], p['y'], p['r']
            for k in range(8):
                a0, a1 = 2 * math.pi * k / 8, 2 * math.pi * (k + 1) / 8
                shade = 0.6 + 0.4 * abs(math.cos(a0))
                glColor3f(*(c * shade * t for c, t in zip(p['col'], tint)))
                glVertex3f(x + r * math.cos(a0), y + r * math.sin(a0), 0)
                glVertex3f(x + r * math.cos(a1), y + r * math.sin(a1), 0)
                glVertex3f(x, y, 0.8)
        glEnd()
