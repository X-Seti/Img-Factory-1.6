#!/usr/bin/env python3
# apps/components/Handling_Editor/depends/diffcode.py - Version: 1
# X-Seti - Sep 2026 - IMG Factory 1.6
# Handling_Editor differences from the shared apps/methods/gui_workshop.py base.
# Keep changes here so the shared file stays identical in every app.

from apps.methods.gui_workshop import GUIWorkshop as _BaseGUIWorkshop


class GUIWorkshop(_BaseGUIWorkshop):
    SHOW_RIGHT_PANEL = False
    SPLITTER_SIZES   = [200, 950]
