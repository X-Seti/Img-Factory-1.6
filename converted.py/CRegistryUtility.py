"""
CRegistryUtility.py - Python conversion of CRegistryUtility.h and CRegistryUtility.cpp

Original C++ header content:
#ifndef CRegistryUtility_H
#define CRegistryUtility_H

#include <string>

class CRegistryUtility
{
public:
	static void				setRegistryValueString(std::string strKey1, std::string strKey2, std::string strValue);
	static std::string		getRegistryValueString(std::string strKey1, std::string strKey2);
	static void				setRegistryValueInt(std::string strKey1, std::string strKey2, int iValue);
	static int				getRegistryValueInt(std::string strKey1, std::string strKey2);
	
};

#endif
"""

import os
import glob
from pathlib import Path

class CRegistryUtility:
    # void			CRegistryUtility::setRegistryValueString(string strKey1, string strKey2, string strValue)
    # 	HKEY hKey;
    # 	if (RegOpenKeyEx(HKEY_CURRENT_USER, CStringUtility::convertStdStringToStdWString(strKey1).c_str(), NULL, KEY_ALL_ACCESS, &hKey) == ERROR_FILE_NOT_FOUND)
    # 	RegCloseKey(hKey);
    # string			CRegistryUtility::getRegistryValueString(string strKey1, string strKey2)
    # 	wchar_t szBuffer[MAX_PATH];
    # 	DWORD uiBufferSize = MAX_PATH;
    # 	HKEY hKey;
    # 	if (RegOpenKey(HKEY_CURRENT_USER, CStringUtility::convertStdStringToStdWString(strKey1).c_str(), &hKey) != ERROR_SUCCESS)
    # 		return "";
    # 	if (RegQueryValueEx(hKey, CStringUtility::convertStdStringToStdWString(strKey2).c_str(), NULL, NULL, (LPBYTE)szBuffer, &uiBufferSize) != ERROR_SUCCESS)
    # 		return "";
    # 	RegCloseKey(hKey);
    # 	szBuffer[uiBufferSize] = '\0';
    # 	return strData;
    # void			CRegistryUtility::setRegistryValueInt(string strKey1, string strKey2, int iValue)
    # 	HKEY hKey;
    # 	if (RegOpenKeyEx(HKEY_CURRENT_USER, CStringUtility::convertStdStringToStdWString(strKey1).c_str(), NULL, KEY_ALL_ACCESS, &hKey) == ERROR_FILE_NOT_FOUND)
    # 	RegCloseKey(hKey);
    # int				CRegistryUtility::getRegistryValueInt(string strKey1, string strKey2)
    # 	DWORD uiValue = 0;
    # 	DWORD uiBufferSize = 4;
    # 	HKEY hKey;
    # 	if (RegOpenKeyEx(HKEY_CURRENT_USER, CStringUtility::convertStdStringToStdWString(strKey1).c_str(), 0, KEY_READ, &hKey) != ERROR_SUCCESS)
    # 		return 0;
    # 	if (RegQueryValueEx(hKey, CStringUtility::convertStdStringToStdWString(strKey2).c_str(), NULL, NULL, (LPBYTE)&uiValue, &uiBufferSize) != ERROR_SUCCESS)
    # 		return 0;
    # 	RegCloseKey(hKey);
    # 	return uiValue;