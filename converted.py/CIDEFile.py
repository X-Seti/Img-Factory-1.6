"""
CIDEFile.py - Python conversion of CIDEFile.h and CIDEFile.cpp

Original C++ header content:
#ifndef CIDEFile_H
#define CIDEFile_H

#include <string>
#include <vector>

struct CIDESectionEntry_OBJS;
struct CIDESectionEntry_TOBJ;
struct CIDESectionEntry_ANIM;
struct CIDESectionEntry_PEDS;
struct CIDESectionEntry_WEAP;
struct CIDESectionEntry_CARS;
struct CIDESectionEntry_HIER;
struct CIDESectionEntry_TXDP;
struct CIDESectionEntry_2DFX_Lights;
struct CIDESectionEntry_2DFX_Particles;
struct CIDESectionEntry_2DFX_Unknown1;
struct CIDESectionEntry_2DFX_Peds;
struct CIDESectionEntry_2DFX_SunReflections;
struct CIDESectionEntry_PATH;
struct CIDESectionEntry_HAND;

struct CIDEFile
{
	std::vector<std::string>							getModelNames(void);
	std::vector<std::string>							getTextureNames(void);

	std::string											m_strFilePath;
	std::vector<CIDESectionEntry_OBJS*>					m_vecSectionEntries_OBJS;
	std::vector<CIDESectionEntry_TOBJ*>					m_vecSectionEntries_TOBJ;
	std::vector<CIDESectionEntry_ANIM*>					m_vecSectionEntries_ANIM;
	std::vector<CIDESectionEntry_PEDS*>					m_vecSectionEntries_PEDS;
	std::vector<CIDESectionEntry_WEAP*>					m_vecSectionEntries_WEAP;
	std::vector<CIDESectionEntry_CARS*>					m_vecSectionEntries_CARS;
	std::vector<CIDESectionEntry_HIER*>					m_vecSectionEntries_HIER;
	std::vector<CIDESectionEntry_TXDP*>					m_vecSectionEntries_TXDP;
	std::vector<CIDESectionEntry_2DFX_Lights*>			m_vecSectionEntries_2DFX_Lights;
	std::vector<CIDESectionEntry_2DFX_Particles*>		m_vecSectionEntries_2DFX_Particles;
	std::vector<CIDESectionEntry_2DFX_Unknown1*>		m_vecSectionEntries_2DFX_Unknown1;
	std::vector<CIDESectionEntry_2DFX_Peds*>			m_vecSectionEntries_2DFX_Peds;
	std::vector<CIDESectionEntry_2DFX_SunReflections*>	m_vecSectionEntries_2DFX_SunReflections;
	std::vector<CIDESectionEntry_PATH*>					m_vecSectionEntries_PATH;
	std::vector<CIDESectionEntry_HAND*>					m_vecSectionEntries_HAND;

	std::vector<std::string*>							m_vecCommentLines;
	std::vector<std::string>							m_vecCommentLinesAtEndOfFile;
};

#endif
"""

import os
import glob
from pathlib import Path

class CIDEFile:
    # vector<string>		CIDEFile::getModelNames(void)
    # 	vector<string> vecModelNames;
    # 	for (auto pEntry : m_vecSectionEntries_OBJS) vecModelNames.push_back(pEntry->m_strModelName);
    # 	for (auto pEntry : m_vecSectionEntries_TOBJ) vecModelNames.push_back(pEntry->m_strModelName);
    # 	for (auto pEntry : m_vecSectionEntries_ANIM) vecModelNames.push_back(pEntry->m_strModelName);
    # 	for (auto pEntry : m_vecSectionEntries_PEDS) vecModelNames.push_back(pEntry->m_strModelName);
    # 	for (auto pEntry : m_vecSectionEntries_WEAP) vecModelNames.push_back(pEntry->m_strModelName);
    # 	for (auto pEntry : m_vecSectionEntries_CARS) vecModelNames.push_back(pEntry->m_strModelName);
    # 	for (auto pEntry : m_vecSectionEntries_HIER) vecModelNames.push_back(pEntry->m_strModelName);
    # 	//for (auto pEntry : m_vecSectionEntries_TXDP) vecModelNames.push_back(pEntry->m_strModelName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_Lights) vecModelNames.push_back(pEntry->m_strModelName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_Particles) vecModelNames.push_back(pEntry->m_strModelName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_Unknown1) vecModelNames.push_back(pEntry->m_strModelName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_Peds) vecModelNames.push_back(pEntry->m_strModelName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_SunReflections) vecModelNames.push_back(pEntry->m_strModelName);
    # 	//for (auto pEntry : m_vecSectionEntries_PATH) vecModelNames.push_back(pEntry->m_strModelName);
    # 	//for (auto pEntry : m_vecSectionEntries_HAND) vecModelNames.push_back(pEntry->m_strModelName);
    # 	return vecModelNames;
    # vector<string>		CIDEFile::getTextureNames(void)
    # 	vector<string> vecModelNames;
    # 	for (auto pEntry : m_vecSectionEntries_OBJS) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	for (auto pEntry : m_vecSectionEntries_TOBJ) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	for (auto pEntry : m_vecSectionEntries_ANIM) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	for (auto pEntry : m_vecSectionEntries_PEDS) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	for (auto pEntry : m_vecSectionEntries_WEAP) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	for (auto pEntry : m_vecSectionEntries_CARS) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	for (auto pEntry : m_vecSectionEntries_HIER) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	for (auto pEntry : m_vecSectionEntries_TXDP) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_Lights) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_Particles) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_Unknown1) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_Peds) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	//for (auto pEntry : m_vecSectionEntries_2DFX_SunReflections) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	//for (auto pEntry : m_vecSectionEntries_PATH) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	//for (auto pEntry : m_vecSectionEntries_HAND) vecModelNames.push_back(pEntry->m_strTextureName);
    # 	return vecModelNames;