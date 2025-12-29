"""
CStringUtility.py - Python conversion of CStringUtility.h and CStringUtility.cpp

Original C++ header content:
#ifndef CStringUtility_H
#define CStringUtility_H

#include <atlstr.h>
#include <string>
#include <vector>
#include <deque>

class CStringUtility
{
public:
	static std::vector<std::string>			split(std::string strString, std::string strToken);
	static std::string						join(std::vector<std::string>& vecTokens, std::string strDelimiter);
	static std::vector<std::string>			combineVectors(std::vector<std::string>& vecVector1, std::vector<std::string>& vecVector2);
	static std::deque<std::string>			convertVectorToDeque(std::vector<std::string>& vecVector);
	static std::vector<std::string>			toUpperCaseVector(std::vector<std::string>& vecVector);
	static unsigned long					findVectorKey(std::vector<std::string>& vecVector, std::string strValue);
	static std::string						replace(std::string strString, std::string strFind, std::string strReplace);
	static std::string						packULong(unsigned long uiULong, bool bBigEndian = true);
	static std::string						packUShort(unsigned short usUShort, bool bBigEndian = true);
	static std::string						packUChar(unsigned char ucUChar);
	static unsigned long					unpackULong(std::string strData, bool bBigEndian = true);
	static unsigned short					unpackUShort(std::string strData, bool bBigEndian = true);
	static unsigned char					unpackUChar(std::string strData);
	static std::string						toString(int iNumber);
	static int								toNumber(std::string strText);
	static unsigned long					toULong(std::string strString);
	static signed long						toLong(std::string strString);
	static float							toFloat(std::string strString);
	static std::string						trim(std::string strString);
	static std::string						ltrim(std::string strString);
	static std::string						rtrim(std::string strString);
	static std::string						rtrimFromLeft(std::string strString);
	static std::string						zeroPad(std::string strData, unsigned long uiPadLength);
	static std::string						toUpperCase(std::string strString);
	static std::string						convertCStringToStdString(CString str)
	{
		CT2CA pszConvertedAnsiString(str);
		return std::string(pszConvertedAnsiString);
	}
	static std::wstring						convertStdStringToStdWString(std::string str)
	{
		int iLength = MultiByteToWideChar(0, 0, str.c_str(), str.length() + 1, 0, 0);
		WCHAR *wstr = new WCHAR[iLength + 1];
		MultiByteToWideChar(0, 0, str.c_str(), str.length() + 1, wstr, iLength + 1);
		wstr[iLength] = 0;
		std::wstring wstr2(wstr);
		delete wstr;
		return wstr2;
	}
	static std::string						convertStdWStringToStdString(std::wstring str)
	{
		int len;
		int slength = (int)str.length() + 1;
		len = WideCharToMultiByte(CP_ACP, 0, str.c_str(), slength, 0, 0, 0, 0);
		char* buf = new char[len];
		WideCharToMultiByte(CP_ACP, 0, str.c_str(), slength, buf, len, 0, 0);
		std::string r(buf);
		delete[] buf;
		return r;
	}
};

#endif
"""

import os
import glob
from pathlib import Path

class CStringUtility:
    # vector<string>		CStringUtility::split(string strString, string strDelimiter)
    # 	if (strString.length() > 0 && strString.substr(strString.length() - 1, 1) == strDelimiter)
    # 		strString = strString.substr(0, strString.length() - 1);
    # 	vector<string> vecTokens;
    # 	string strToken;
    # 	unsigned long
    # 		uiSeek = 0,
    # 		uiFoundPosition = strString.find(strDelimiter, uiSeek);
    # 	while (uiFoundPosition != string::npos)
    # 		strToken = strString.substr(uiSeek, uiFoundPosition - uiSeek);
    # 		vecTokens.push_back(strToken);
    # 		uiSeek = uiFoundPosition + strDelimiter.length();
    # 		uiFoundPosition = strString.find(strDelimiter, uiSeek);
    # 	if (uiSeek != strString.length())
    # 		vecTokens.push_back(strToken);
    # 	return vecTokens;
    # string				CStringUtility::join(vector<string>& vecTokens, string strDelimiter)
    # 	if (vecTokens.size() == 0)
    # 		return "";
    # 	string strText;
    # 	for (unsigned long i = 0; i < vecTokens.size() - 1; i++)
    # 		strText += vecTokens[i] + strDelimiter;
    # 	strText += vecTokens[vecTokens.size() - 1];
    # 	return strText;
    # vector<string>		CStringUtility::combineVectors(vector<string>& vecVector1, vector<string>& vecVector2)
    # 	vector<string> vecValues;
    # 	for (auto strValue : vecVector1)
    # 		vecValues.push_back(strValue);
    # 	for (auto strValue : vecVector2)
    # 		vecValues.push_back(strValue);
    # 	return vecValues;
    # deque<string>		CStringUtility::convertVectorToDeque(vector<string>& vecVector)
    # 	deque<string> deqValues;
    # 	for (auto strValue : vecVector)
    # 		deqValues.push_back(strValue);
    # 	return deqValues;
    # vector<string>		CStringUtility::toUpperCaseVector(vector<string>& vecVector)
    # 	vector<string> vecValues;
    # 	for (auto strValue : vecVector)
    # 	return vecValues;
    # unsigned long		CStringUtility::findVectorKey(vector<string>& vecVector, string strValue)
    # 	for (unsigned long i = 0; i < vecVector.size(); i++)
    # 		if (vecVector[i] == strValue)
    # 			return i;
    # 	return -1;
    # string				CStringUtility::replace(string strString, string strFind, string strReplace)
    # 	size_t uiPos = strString.find(strFind);
    # 	while (uiPos != string::npos)
    # 		strString.replace(uiPos, strFind.length(), strReplace);
    # 		uiPos = strString.find(strFind);
    # 	return strString;
    # string				CStringUtility::packULong(unsigned long uiULong, bool bBigEndian)
    # 	char szULong[4];
    # 	if (bBigEndian)
    # 		szULong[0] = (unsigned char)floor(uiULong / 16777216);
    # 		szULong[1] = (unsigned char)floor(uiULong / 65536);
    # 		szULong[2] = (unsigned char)floor(uiULong / 256);
    # 		szULong[3] = (unsigned char)(uiULong % 256);
    # 	else
    # 		szULong[3] = (unsigned char)floor(uiULong / 16777216);
    # 		szULong[2] = (unsigned char)floor(uiULong / 65536);
    # 		szULong[1] = (unsigned char)floor(uiULong / 256);
    # 		szULong[0] = (unsigned char)(uiULong % 256);
    # 	string strULong(szULong, 4);
    # 	return strULong;
    # string				CStringUtility::packUShort(unsigned short uiUShort, bool bBigEndian)
    # 	char szUShort[2];
    # 	if (bBigEndian)
    # 		szUShort[0] = (unsigned char)floor(uiUShort / 256);
    # 		szUShort[1] = (unsigned char)(uiUShort % 256);
    # 	else
    # 		szUShort[1] = (unsigned char)floor(uiUShort / 256);
    # 		szUShort[0] = (unsigned char)(uiUShort % 256);
    # 	string strUShort(szUShort, 2);
    # 	return strUShort;
    # string				CStringUtility::packUChar(unsigned char ucUChar)
    # 	char szUChar[1];
    # 	szUChar[0] = (unsigned char)ucUChar;
    # 	string strUChar(szUChar, 1);
    # 	return strUChar;
    # unsigned long		CStringUtility::unpackULong(string strData, bool bBigEndian)
    # 	unsigned long uiInt;
    # 	if (bBigEndian)
    # 		uiInt =
    # 			(((unsigned char)strData.c_str()[0]) * 16777216) +
    # 			(((unsigned char)strData.c_str()[1]) * 65536) +
    # 			(((unsigned char)strData.c_str()[2]) * 256) +
    # 			((unsigned char)strData.c_str()[3]);
    # 	else
    # 		uiInt =
    # 			(((unsigned char)strData.c_str()[3]) * 16777216) +
    # 			(((unsigned char)strData.c_str()[2]) * 65536) +
    # 			(((unsigned char)strData.c_str()[1]) * 256) +
    # 			((unsigned char)strData.c_str()[0]);
    # 	return uiInt;
    # unsigned short		CStringUtility::unpackUShort(string strData, bool bBigEndian)
    # 	unsigned short uiInt;
    # 	if (bBigEndian)
    # 		uiInt =
    # 			(((unsigned char)strData.c_str()[0]) * 256) +
    # 			((unsigned char)strData.c_str()[1]);
    # 	else
    # 		uiInt =
    # 			(((unsigned char)strData.c_str()[1]) * 256) +
    # 			((unsigned char)strData.c_str()[0]);
    # 	return uiInt;
    # unsigned char		CStringUtility::unpackUChar(string strData)
    # 	return (unsigned char)strData.c_str()[0];
    # string				CStringUtility::toString(int iNumber)
    # 	char szString[100];
    # 	_itoa_s(iNumber, szString, 10);
    # 	return string(szString);
    # int					CStringUtility::toNumber(string strText)
    # 	return atoi(strText.c_str());
    # unsigned long		CStringUtility::toULong(string strString)
    # 	return atoi(strString.c_str());
    # signed long			CStringUtility::toLong(string strString)
    # 	return atoi(strString.c_str());
    # float				CStringUtility::toFloat(string strString)
    # 	return atof(strString.c_str());
    # string				CStringUtility::trim(string strString)
    # 	return rtrim(ltrim(strString));
    # string				CStringUtility::ltrim(string strString)
    # 	unsigned long iStart = 0;
    # 	for (unsigned long i = 0; i < strString.length(); i++)
    # 		if (strString.at(i) <= 32 || strString.at(i) >= 127)
    # 			iStart++;
    # 		else
    # 			break;
    # 	return strString.substr(iStart);
    # string				CStringUtility::rtrim(string strString)
    # 	unsigned long iEnd = strString.length() - 1;
    # 	for (signed long i = iEnd; i >= 0; i--)
    # 		if (strString.at(i) <= 32 || strString.at(i) >= 127)
    # 			iEnd--;
    # 		else
    # 			break;
    # 	return strString.substr(0, iEnd + 1);
    # string				CStringUtility::rtrimFromLeft(string strString)
    # 	for (unsigned long i = 0; i < strString.length(); i++)
    # 		if (strString.at(i) <= 32 || strString.at(i) >= 127)
    # 			return strString.substr(0, i);
    # 	return strString;
    # std::string			CStringUtility::zeroPad(string strData, unsigned long uiPadLength)
    # 	unsigned long uiOldLength = strData.size();
    # 	strData.resize(uiPadLength);
    # 	for (unsigned long i = uiOldLength; i < uiPadLength; i++)
    # 		strData[i] = '\0';
    # 	return strData;
    # string				CStringUtility::toUpperCase(string strString)
    # 	return strString;