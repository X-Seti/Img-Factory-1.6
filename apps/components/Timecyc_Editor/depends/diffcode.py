#!/usr/bin/env python3
# apps/components/Timecyc_Editor/depends/diffcode.py - Version: 1
# X-Seti - Sep 2026 - IMG Factory 1.6
# Timecyc_Editor differences from the shared apps/methods/gui_workshop.py base.
# Keep changes here so the shared file stays identical in every app.

from apps.methods.gui_workshop import GUIWorkshop as _BaseGUIWorkshop


class GUIWorkshop(_BaseGUIWorkshop):
    SHOW_LEFT_PANEL   = False
    SHOW_RIGHT_PANEL  = False
    SPLITTER_SIZES    = [700, 450]
    SPLITTER_STRETCH  = [3, 2]
    TOOLBAR_CONVERT   = True
