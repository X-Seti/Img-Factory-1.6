# X-Seti - May13 2026 - IMG Factory 1.6 - Vehicle Workshop DFF Viewport
# this belongs in apps/components/Vehicle_Workshop/methods/dff_viewport.py - Version: 1
"""
VehicleViewport - DFFViewport subclass with vehicle-specific animation.
Used standalone. Docked mode imports from apps.methods.dff_viewport.

##Methods list -
# VehicleViewport.__init__
# VehicleViewport._anim_tick
# VehicleViewport._get_anim_rotation
# VehicleViewport._get_wheel_geom_data
# VehicleViewport._rebuild_anim_geoms
# VehicleViewport.load_wheels_dff
# VehicleViewport.set_animation
# VehicleViewport.set_animation_speed
# VehicleViewport.set_wheel_heading
# VehicleViewport.toggle_door
"""

import math

try:
    from apps.methods.dff_viewport import DFFViewport, OPENGL_AVAILABLE
except ImportError:
    import sys, os
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', '..', '..'))
    from apps.methods.dff_viewport import DFFViewport, OPENGL_AVAILABLE


class VehicleViewport(DFFViewport):
    """DFFViewport + vehicle animation (doors, rotors, wheels)."""

    def __init__(self, parent=None): #vers 1
        super().__init__(parent)
        self._anim_enabled      = False
        self._anim_timer        = None
        self._anim_speed        = 1.0
        self._anim_rates        = {'moving_rotor': 360.0, 'moving_rotor2': 360.0,
                                   'prop': 360.0, 'misc_a': 180.0, 'misc_b': 180.0}
        self._anim_frame_angles = {}
        self._anim_door_open    = {}
        self._wheel_heading     = 0.0
        self._dragging          = False
        from PyQt6.QtCore import Qt
        self._drag_btn          = Qt.MouseButton.NoButton

    def set_animation(self, enabled: bool): #vers 1
        self._anim_enabled = enabled
        if enabled:
            if self._anim_timer is None:
                from PyQt6.QtCore import QTimer
                self._anim_timer = QTimer(self)
                self._anim_timer.timeout.connect(self._anim_tick)
            self._anim_timer.start(33)
        else:
            if self._anim_timer: self._anim_timer.stop()
            self.update()

    def _anim_tick(self): #vers 1
        if not self._anim_enabled or not self._assembly_mode: return
        for fname, rate in self._anim_rates.items():
            cur = self._anim_frame_angles.get(fname, 0.0)
            self._anim_frame_angles[fname] = (cur + rate * self._anim_speed / 30.0) % 360.0
        self._rebuild_anim_geoms()

    def _rebuild_anim_geoms(self): #vers 1
        m = getattr(self, '_dff_model', None)
        if not m: return
        self.load_all_geometries(
            m.geometries, [g.materials for g in m.geometries],
            m.frames, m.atomics,
            damaged=getattr(self, '_damaged', False))

    def toggle_door(self, door_name: str): #vers 1
        self._anim_door_open[door_name] = not self._anim_door_open.get(door_name, False)
        self._rebuild_anim_geoms()

    def _get_anim_rotation(self, frame_name: str): #vers 1
        name = frame_name.lower()
        for key in ('moving_rotor', 'moving_rotor2', 'prop', 'misc_a', 'misc_b'):
            if key in name:
                angle = self._anim_frame_angles.get(key, 0.0)
                ca=math.cos(math.radians(angle)); sa=math.sin(math.radians(angle))
                return [ca,-sa,0, sa,ca,0, 0,0,1]
        for key in ('door_lf','door_rf','door_lr','door_rr','bonnet','boot'):
            if key in name:
                is_open = self._anim_door_open.get(name, False)
                angle = 70.0 if is_open else 0.0
                ca=math.cos(math.radians(angle)); sa=math.sin(math.radians(angle))
                return [1,0,0, 0,ca,-sa, 0,sa,ca]
        return None

    def set_animation_speed(self, speed: float): #vers 1
        self._anim_speed = max(0.1, speed)

    def set_wheel_heading(self, angle_deg: float): #vers 1
        self._wheel_heading = angle_deg
        if getattr(self,'_assembly_mode',False) and getattr(self,'_dff_model',None):
            m = self._dff_model
            self.load_all_geometries(m.geometries,[g.materials for g in m.geometries],
                                     m.frames, m.atomics, getattr(self,'_damaged',False))

    def load_wheels_dff(self, path: str, wheel_type: str = 'wheel_saloon_l0'): #vers 1
        try:
            try:
                from apps.methods.dff_parser import load_dff
            except ImportError:
                from apps.components.Vehicle_Workshop.depends.dff_parser import load_dff
            self._wheels_model = load_dff(path)
            self._wheel_type   = wheel_type
        except Exception as e:
            print(f'[VehicleViewport] wheels.DFF load fail: {e}')

    def _get_wheel_geom_data(self): #vers 1
        m = getattr(self, '_wheels_model', None)
        if not m: return None
        wtype = getattr(self, '_wheel_type', 'wheel_saloon_l0').lower()
        for a in m.atomics:
            fi = a.frame_index
            fname = (m.frames[fi].name or '').lower() if fi < len(m.frames) else ''
            if fname == wtype:
                g = m.geometries[a.geometry_index]
                return (
                    [(v.x,v.y,v.z) for v in g.vertices],
                    [(n.x,n.y,n.z) for n in g.normals] if g.normals else [],
                    [(u.u,u.v) for u in g.uv_layers[0]] if g.uv_layers else [],
                    [(t.v1,t.v2,t.v3,t.material_id) for t in g.triangles],
                    g.materials,
                    [(c.r,c.g,c.b,c.a) for c in g.colors] if g.colors else []
                )
        return None

    def mousePressEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        self._last_pos = event.pos(); self._dragging=True; self._drag_btn=event.button()

    def mouseMoveEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        if not self._dragging: return
        dx = event.pos().x()-self._last_pos.x()
        dy = event.pos().y()-self._last_pos.y()
        if self._drag_btn == Qt.MouseButton.LeftButton:
            self._yaw   += dx*0.5
            self._pitch  = max(-89, min(89, self._pitch+dy*0.5))
        elif self._drag_btn == Qt.MouseButton.MiddleButton:
            s = self._dist/500.0
            self._pan_x += dx*s; self._pan_y -= dy*s
        self._last_pos = event.pos(); self.update()

    def mouseReleaseEvent(self, event): #vers 1
        from PyQt6.QtCore import Qt
        self._dragging=False; self._drag_btn=Qt.MouseButton.NoButton

    def wheelEvent(self, event): #vers 1
        f = 0.88 if event.angleDelta().y()>0 else 1.13
        self._dist = max(0.1, min(50000.0, self._dist*f)); self.update()
