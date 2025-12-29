"""
CDebugger.py - Python conversion of CDebugger.h and CDebugger.cpp

Original C++ header content:
#ifndef CDebugger_H
#define CDebugger_H

#include <string>

class CDebugger
{
public:
	static void			log(std::string strData);
};

#endif
"""

import os
import glob
from pathlib import Path

class CDebugger:
    # void		CDebugger::log(string strData)