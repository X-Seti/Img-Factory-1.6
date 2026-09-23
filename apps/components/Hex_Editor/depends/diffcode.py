#!/usr/bin/env python3
# apps/components/Hex_Editor/depends/diffcode.py - Version: 2
# X-Seti - Sep 2026 - IMG Factory 1.6
# Hex_Editor differences from the shared apps/methods/gui_workshop.py base.
# Keep changes here so the shared file stays identical in every app.

from apps.methods.gui_workshop import GUIWorkshop as _BaseGUIWorkshop


class GUIWorkshop(_BaseGUIWorkshop):
    pass    # HexWorkshop builds its own layout in setup_ui
