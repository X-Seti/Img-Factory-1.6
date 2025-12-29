"""
CPathUtility.py - Python conversion of CPathUtility.h and CPathUtility.cpp

Original C++ header content:
#ifndef CPathUtility_H
#define CPathUtility_H

#include <string>

class CPathUtility
{
public:
	static std::string			getFileName(std::string strPath);
	static std::string			getFileExtension(std::string strPath);
	static std::string			replaceExtension(std::string strPath, std::string strExtension);
	static std::string			removeExtension(std::string strPath);
	static std::string			addSlashToEnd(std::string strPath);
};

#endif
"""

import os
import glob
from pathlib import Path

class CPathUtility:
    # string			CPathUtility::getFileName(string strPath)
    # 	size_t uiPosition = strPath.find_last_of("/");
    # 	if (uiPosition == string::npos)
    # 		return strPath;
    # 	return strPath.substr(uiPosition + 1);
    # string			CPathUtility::getFileExtension(string strPath)
    # 	size_t uiPosition = strPath.find_last_of(".");
    # 	if (uiPosition == string::npos)
    # 		return "";
    # 	return strPath.substr(uiPosition + 1);
    # string			CPathUtility::replaceExtension(string strPath, string strExtension)
    # 	size_t uiPosition = strPath.find_last_of(".");
    # 	if (uiPosition == string::npos)
    # 		return strPath + "." + strExtension;
    # 	return strPath.substr(0, uiPosition + 1) + strExtension;
    # string			CPathUtility::removeExtension(string strPath)
    # 	size_t uiPosition = strPath.find_last_of(".");
    # 	if (uiPosition == string::npos)
    # 		return strPath;
    # 	return strPath.substr(0, uiPosition);
    # string			CPathUtility::addSlashToEnd(string strPath)
    # 	if (strPath.c_str()[strPath.length() - 1] != '\\' && strPath.c_str()[strPath.length() - 1] != '/')
    # 		strPath += "/";
    # 	return strPath;