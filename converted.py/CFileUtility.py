"""
CFileUtility.py - Python conversion of CFileUtility.h and CFileUtility.cpp

Original C++ header content:
#ifndef CFileUtility_H
#define CFileUtility_H

#include <string>

class CFileUtility
{
public:
	static std::string			getFileContent(std::string strPath, bool bBinaryMode = true);
	static std::string*			getFileContentPointer(std::string strPath, bool bBinaryMode = true);
	static std::string			getFileSubContent(std::string strPath, unsigned long uiSeek, unsigned long uiByteCount, bool bBinaryMode = true);
	static unsigned long		getFileSize(std::string& strPath);
	static void					storeFile(std::string strPath, std::string strData, bool bAppend = true, bool bBinaryMode = true);
	static void					storeFileByStringPointer(std::string strPath, std::string* pData, bool bAppend = true, bool bBinaryMode = true);
	static void					renameFile(std::string strPath, std::string strNewPath);
	static void					removeFile(std::string strPath);
	static std::string			getFileNameFromNameWithoutExtension(std::string strFolderPath, std::string strFileNameWithoutExtension);
};

#endif
"""

import os
import glob
from pathlib import Path

class CFileUtility:
    @staticmethod
    def getFileContent(strPath, bBinaryMode=True):
        """
        Get the content of a file.
        static std::string getFileContent(std::string strPath, bool bBinaryMode = true);
        """
        try:
            mode = 'rb' if bBinaryMode else 'r'
            with open(strPath, mode) as f:
                return f.read()
        except IOError:
            return ""

    @staticmethod
    def getFileContentPointer(strPath, bBinaryMode=True):
        """
        Get the content of a file as a string (Python doesn't have pointers like C++).
        static std::string* getFileContentPointer(std::string strPath, bool bBinaryMode = true);
        """
        try:
            mode = 'rb' if bBinaryMode else 'r'
            with open(strPath, mode) as f:
                return f.read()  # Return the content directly since Python doesn't have pointers
        except IOError:
            return ""

    @staticmethod
    def getFileSubContent(strPath, uiSeek, uiByteCount, bBinaryMode=True):
        """
        Get a portion of a file's content.
        static std::string getFileSubContent(std::string strPath, unsigned long uiSeek, unsigned long uiByteCount, bool bBinaryMode = true);
        """
        try:
            mode = 'rb' if bBinaryMode else 'r'
            with open(strPath, mode) as f:
                f.seek(uiSeek)
                return f.read(uiByteCount)
        except IOError:
            return ""

    @staticmethod
    def getFileSize(strPath):
        """
        Get the size of a file.
        static unsigned long getFileSize(std::string& strPath);
        """
        try:
            return os.path.getsize(strPath)
        except OSError:
            return 0

    @staticmethod
    def storeFile(strPath, strData, bAppend=True, bBinaryMode=True):
        """
        Store data to a file.
        static void storeFile(std::string strPath, std::string strData, bool bAppend = true, bool bBinaryMode = true);
        """
        try:
            # Create directories if they don't exist
            Path(strPath).parent.mkdir(parents=True, exist_ok=True)
            
            mode = 'ab' if bAppend else 'wb' if bBinaryMode else 'a' if bAppend else 'w'
            with open(strPath, mode) as f:
                if bBinaryMode and isinstance(strData, str):
                    f.write(strData.encode('utf-8'))
                else:
                    f.write(strData)
        except IOError:
            pass  # Handle error as needed

    @staticmethod
    def storeFileByStringPointer(strPath, pData, bAppend=True, bBinaryMode=True):
        """
        Store data from a string to a file (Python doesn't have pointers like C++).
        static void storeFileByStringPointer(std::string strPath, std::string* pData, bool bAppend = true, bool bBinaryMode = true);
        """
        try:
            # Create directories if they don't exist
            Path(strPath).parent.mkdir(parents=True, exist_ok=True)
            
            mode = 'ab' if bAppend else 'wb' if bBinaryMode else 'a' if bAppend else 'w'
            with open(strPath, mode) as f:
                if bBinaryMode and isinstance(pData, str):
                    f.write(pData.encode('utf-8'))
                else:
                    f.write(pData)
        except IOError:
            pass  # Handle error as needed

    @staticmethod
    def renameFile(strPath, strNewPath):
        """
        Rename a file.
        static void renameFile(std::string strPath, std::string strNewPath);
        """
        try:
            os.rename(strPath, strNewPath)
        except OSError:
            pass  # Handle error as needed

    @staticmethod
    def removeFile(strPath):
        """
        Remove/delete a file.
        static void removeFile(std::string strPath);
        """
        try:
            os.remove(strPath)
        except OSError:
            pass  # Handle error as needed

    @staticmethod
    def getFileNameFromNameWithoutExtension(strFolderPath, strFileNameWithoutExtension):
        """
        Get the full filename from a name without extension.
        static std::string getFileNameFromNameWithoutExtension(std::string strFolderPath, std::string strFileNameWithoutExtension);
        """
        try:
            # Add slash to end of folder path if not present
            if not strFolderPath.endswith('/') and not strFolderPath.endswith('\\'):
                strFolderPath += '/'
            
            # Create search pattern
            search_pattern = strFolderPath + strFileNameWithoutExtension + ".*"
            
            # Find all matching files
            matching_files = glob.glob(search_pattern)
            
            # Return first match if any found
            if matching_files:
                return matching_files[0]
                
            return ""
        except Exception:
            return ""