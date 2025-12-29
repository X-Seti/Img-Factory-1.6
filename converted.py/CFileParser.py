"""
CFileParser.py - Python conversion of CFileParser.h and CFileParser.cpp

Original C++ header content:
#ifndef CFileParser_H
#define CFileParser_H

#include <string>
#include <fstream>

enum eEndian
{
	LITTLE_ENDIAN	=	0,
	BIG_ENDIAN		=	1
};

class CFileParser
{
public:
	CFileParser(void) :
		m_uiSeek(0),
		m_bReadAllAtOnce(true)
	{};

	static CFileParser*	getInstance(void);

	void				setEndian(eEndian eEndian) { m_eEndian = eEndian; }
	void				setReadAllAtOnce(bool bState) { m_bReadAllAtOnce = bState; }

	bool				open(std::string strPath, bool bBinaryMode);
	void				close(void);
	
	std::string			readString(unsigned long uiByteCount, bool bPeek = false);
	std::string			readStringUntilZero(bool bPeek = false);
	unsigned long		readULong(bool bPeek = false);
	unsigned short		readUShort(bool bPeek = false);
	unsigned char		readUChar(bool bPeek = false);
	
	std::string			readLine(void);

	void				seek(unsigned long uiSeek);
	bool				isEOF(void);

	//std::string			getFileContent(void) { return m_strFileContent; }
	std::string*		getFileContent(void) { return m_pFileContent; }
	void				setFileContent(std::string *pFileContent) { delete m_pFileContent; m_pFileContent = pFileContent; }
	
private:
	static CFileParser*	m_pInstance;
	eEndian				m_eEndian;

	bool				m_bReadAllAtOnce;

	//std::string			m_strFileContent;
	std::string*		m_pFileContent;
	unsigned long		m_uiSeek;

	std::ifstream		m_file;
	unsigned long		m_uiFileSize;
};

#endif
"""

import os
import glob
from pathlib import Path

class CFileParser:
    # CFileParser*	CFileParser::getInstance(void)
    # 	if (CFileParser::m_pInstance == nullptr)
    # bool			CFileParser::open(string strPath, bool bBinaryMode)
    # 	m_uiSeek = 0;
    # 	if (m_bReadAllAtOnce)
    # 		//return m_strFileContent != "";
    # 		//return (*m_pFileContent) != "";
    # 		return m_pFileContent->length() != 0;
    # 	else
    # 		if (m_file.is_open())
    # 			m_file.seekg(0, SEEK_END);
    # 			m_uiFileSize = m_file.tellg();
    # 			m_file.seekg(0, SEEK_SET);
    # 		return m_file.is_open();
    # void			CFileParser::close(void)
    # 	m_uiSeek = 0;
    # 	if (m_bReadAllAtOnce)
    # 		//m_strFileContent = "";
    # 		*m_pFileContent = "";
    # 		delete m_pFileContent;
    # 	else
    # 		m_file.close();
    # string		CFileParser::readString(unsigned long uiByteCount, bool bPeek)
    # 	string strData;
    # 	if (m_bReadAllAtOnce)
    # 		//string strData = m_strFileContent.substr(m_uiSeek, uiByteCount);
    # 		strData = m_pFileContent->substr(m_uiSeek, uiByteCount);
    # 	else
    # 		char *pData = new char[uiByteCount];
    # 		m_file.read(pData, uiByteCount);
    # 		strData.append(pData, uiByteCount);
    # 		delete pData;
    # 	if (!bPeek)
    # 		m_uiSeek += uiByteCount;
    # 	return strData;
    # string			CFileParser::readStringUntilZero(bool bPeek)
    # 	string strData;
    # 	unsigned long uiLength;
    # 	if (m_bReadAllAtOnce)
    # 		unsigned long uiPosition = m_pFileContent->find('\0', m_uiSeek);
    # 		uiLength = uiPosition - m_uiSeek;
    # 		strData = m_pFileContent->substr(m_uiSeek, uiLength);
    # 	else
    # 	if (!bPeek)
    # 		m_uiSeek += uiLength + 1;
    # 	return strData;
    # unsigned long	CFileParser::readULong(bool bPeek)
    # 	unsigned long uiData;
    # 	if (m_bReadAllAtOnce)
    # 	else
    # 		char cData[4];
    # 		m_file.read(cData, 4);
    # 	if (!bPeek)
    # 		m_uiSeek += 4;
    # 	return uiData;
    # unsigned short	CFileParser::readUShort(bool bPeek)
    # 	unsigned short usData;
    # 	if (m_bReadAllAtOnce)
    # 	else
    # 		char cData[2];
    # 		m_file.read(cData, 2);
    # 	if (!bPeek)
    # 		m_uiSeek += 2;
    # 	return usData;
    # unsigned char	CFileParser::readUChar(bool bPeek)
    # 	unsigned char ucData;
    # 	if (m_bReadAllAtOnce)
    # 	else
    # 		char cData[1];
    # 		m_file.read(cData, 1);
    # 	if (!bPeek)
    # 		m_uiSeek++;
    # 	return ucData;
    # string			CFileParser::readLine(void)
    # 	if (m_bReadAllAtOnce)
    # 	else
    # 		string strLine = "";
    # 		getline(m_file, strLine);
    # 		return strLine;
    # void			CFileParser::seek(unsigned long uiSeek)
    # 	if (m_bReadAllAtOnce)
    # 		m_uiSeek = uiSeek;
    # 	else
    # 		m_file.seekg(uiSeek);
    # bool			CFileParser::isEOF(void)
    # 	if (m_bReadAllAtOnce)
    # 		//return m_uiSeek >= m_strFileContent.length();
    # 		return m_uiSeek >= m_pFileContent->length();
    # 	else
    # 		unsigned long uiSeek = m_file.tellg();
    # 		return uiSeek >= m_uiFileSize;