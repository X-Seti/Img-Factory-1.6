"""
CIMGFile.py - Python conversion of CIMGFile.h and CIMGFile.cpp

Original C++ header content:
#ifndef CIMGFile_H
#define CIMGFile_H

#include <string>
#include <vector>

struct CIMGEntry;

enum eIMGVersion
{
	IMG_1				= 0,
	IMG_2				= 1,
	IMG_3_ENCRYPTED		= 2,
	IMG_3_UNENCRYPTED	= 3,
	IMG_UNKNOWN			= 4
};

struct CIMGFile
{
	CIMGFile(void) :
		m_bFileFound(false)
	{};

	void							unload(void);

	CIMGEntry*						getEntryByHighestOffset(void);
	unsigned long					getVersion3NamesLength(void);
	CIMGEntry*						getEntryByName(std::string strEntryName);
	CIMGEntry*						getEntryByNameWithoutExtension(std::string strEntryNameWithoutExtension);
	unsigned long					getEntryIndex(CIMGEntry *pIMGEntry);

	bool							m_bFileFound;
	std::string						m_strPath;

	eIMGVersion						m_eVersion;
	unsigned long					m_uiEntryCount;
	std::vector<CIMGEntry*>			m_vecEntries;

	//std::string						m_strIMGContent;
	//std::string*					m_pIMGContent;
};

#endif
"""

import os
import glob
from pathlib import Path

class CIMGFile:
    # void					CIMGFile::unload(void)
    # 	for (auto pEntry : m_vecEntries)
    # 		delete pEntry;
    # CIMGEntry*				CIMGFile::getEntryByHighestOffset(void)
    # 	unsigned long uiHighestOffset = 0;
    # 	CIMGEntry *pHighestOffsetIMGEntry = nullptr;
    # 	for(auto pIMGEntry : m_vecEntries)
    # 		if(pIMGEntry->m_uiFileOffset > uiHighestOffset)
    # 			uiHighestOffset = pIMGEntry->m_uiFileOffset;
    # 			pHighestOffsetIMGEntry = pIMGEntry;
    # 	return pHighestOffsetIMGEntry;
    # unsigned long			CIMGFile::getVersion3NamesLength(void)
    # 	unsigned long uiLength = 0;
    # 	for (auto pEntry : m_vecEntries)
    # 		uiLength += pEntry->m_strFileName.length();
    # 	uiLength += m_vecEntries.size();
    # 	return uiLength;
    # CIMGEntry*				CIMGFile::getEntryByName(string strEntryName)
    # 	for (auto pEntry : m_vecEntries)
    # 		if (CStringUtility::toUpperCase(strEntryName) == CStringUtility::toUpperCase(pEntry->m_strFileName))
    # 			return pEntry;
    # 	return nullptr;
    # CIMGEntry*				CIMGFile::getEntryByNameWithoutExtension(string strEntryNameWithoutExtension)
    # 	for (auto pEntry : m_vecEntries)
    # 		if (CStringUtility::toUpperCase(strEntryNameWithoutExtension) == CStringUtility::toUpperCase(CPathUtility::removeExtension(pEntry->m_strFileName)))
    # 			return pEntry;
    # 	return nullptr;
    # unsigned long			CIMGFile::getEntryIndex(CIMGEntry *pIMGEntry)
    # 	for (unsigned long i = 0; i < m_vecEntries.size(); i++)
    # 		if (m_vecEntries[i] == pIMGEntry)
    # 			return i;
    # 	return -1;