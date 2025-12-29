"""
CCOLFile.py - Python conversion of CCOLFile.h and CCOLFile.cpp

Original C++ header content:
#ifndef CCOLFile_H
#define CCOLFile_H

#include <string>
#include <vector>

struct CCOLEntry;

struct CCOLFile
{
	std::vector<std::string>		getModelNames(void);

	std::string						m_strFilePath;
	std::vector<CCOLEntry*>			m_vecEntries;
};

#endif
"""

import os
import glob
from pathlib import Path

class CCOLFile:
    # vector<string>		CCOLFile::getModelNames(void)
    # 	vector<string> vecModelNames;
    # 	for (auto pCOLEntry : m_vecEntries)
    # 		vecModelNames.push_back(pCOLEntry->m_strModelName);
    # 	return vecModelNames;