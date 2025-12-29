"""
Globals.py - Python conversion of Globals.h and Globals.cpp

Original C++ header content:
#ifndef Globals_H
#define Globals_H

class CMvecIMGManager;

extern CMvecIMGManager*		g_pMvecIMGManager;

CMvecIMGManager*			getMvecIMGManager(void);

#endif
"""

import os
import glob
from pathlib import Path

class Globals:
    # CMvecIMGManager*			g_pMvecIMGManager = nullptr;
    # CMvecIMGManager*			getMvecIMGManager(void) { return g_pMvecIMGManager; }