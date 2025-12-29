"""
files.py - Python conversion of files.h and files.cpp

Original C++ header content:
#ifndef CRYPTOPP_FILES_H
#define CRYPTOPP_FILES_H

#include "cryptlib.h"
#include "filters.h"
#include "argnames.h"

#include <iostream>
#include <fstream>

NAMESPACE_BEGIN(CryptoPP)

//! file-based implementation of Store interface
class CRYPTOPP_DLL FileStore : public Store, private FilterPutSpaceHelper, public NotCopyable
{
public:
	class Err : public Exception
	{
	public:
		Err(const std::string &s) : Exception(IO_ERROR, s) {}
	};
	class OpenErr : public Err {public: OpenErr(const std::string &filename) : Err("FileStore: error opening file for reading: " + filename) {}};
	class ReadErr : public Err {public: ReadErr() : Err("FileStore: error reading file") {}};

	FileStore() : m_stream(NULL) {}
	FileStore(std::istream &in)
		{StoreInitialize(MakeParameters(Name::InputStreamPointer(), &in));}
	FileStore(const char *filename)
		{StoreInitialize(MakeParameters(Name::InputFileName(), filename));}
#if defined(CRYPTOPP_UNIX_AVAILABLE) || _MSC_VER >= 1400
	//! specify file with Unicode name. On non-Windows OS, this function assumes that setlocale() has been called.
	FileStore(const wchar_t *filename)
		{StoreInitialize(MakeParameters(Name::InputFileNameWide(), filename));}
#endif

	std::istream* GetStream() {return m_stream;}

	lword MaxRetrievable() const;
	size_t TransferTo2(BufferedTransformation &target, lword &transferBytes, const std::string &channel=DEFAULT_CHANNEL, bool blocking=true);
	size_t CopyRangeTo2(BufferedTransformation &target, lword &begin, lword end=LWORD_MAX, const std::string &channel=DEFAULT_CHANNEL, bool blocking=true) const;
	lword Skip(lword skipMax=ULONG_MAX);

private:
	void StoreInitialize(const NameValuePairs &parameters);
	
	member_ptr<std::ifstream> m_file;
	std::istream *m_stream;
	byte *m_space;
	size_t m_len;
	bool m_waiting;
};

//! file-based implementation of Source interface
class CRYPTOPP_DLL FileSource : public SourceTemplate<FileStore>
{
public:
	typedef FileStore::Err Err;
	typedef FileStore::OpenErr OpenErr;
	typedef FileStore::ReadErr ReadErr;

	FileSource(BufferedTransformation *attachment = NULL)
		: SourceTemplate<FileStore>(attachment) {}
	FileSource(std::istream &in, bool pumpAll, BufferedTransformation *attachment = NULL)
		: SourceTemplate<FileStore>(attachment) {SourceInitialize(pumpAll, MakeParameters(Name::InputStreamPointer(), &in));}
	FileSource(const char *filename, bool pumpAll, BufferedTransformation *attachment = NULL, bool binary=true)
		: SourceTemplate<FileStore>(attachment) {SourceInitialize(pumpAll, MakeParameters(Name::InputFileName(), filename)(Name::InputBinaryMode(), binary));}
#if defined(CRYPTOPP_UNIX_AVAILABLE) || _MSC_VER >= 1400
	//! specify file with Unicode name. On non-Windows OS, this function assumes that setlocale() has been called.
	FileSource(const wchar_t *filename, bool pumpAll, BufferedTransformation *attachment = NULL, bool binary=true)
		: SourceTemplate<FileStore>(attachment) {SourceInitialize(pumpAll, MakeParameters(Name::InputFileNameWide(), filename)(Name::InputBinaryMode(), binary));}
#endif

	std::istream* GetStream() {return m_store.GetStream();}
};

//! file-based implementation of Sink interface
class CRYPTOPP_DLL FileSink : public Sink, public NotCopyable
{
public:
	class Err : public Exception
	{
	public:
		Err(const std::string &s) : Exception(IO_ERROR, s) {}
	};
	class OpenErr : public Err {public: OpenErr(const std::string &filename) : Err("FileSink: error opening file for writing: " + filename) {}};
	class WriteErr : public Err {public: WriteErr() : Err("FileSink: error writing file") {}};

	FileSink() : m_stream(NULL) {}
	FileSink(std::ostream &out)
		{IsolatedInitialize(MakeParameters(Name::OutputStreamPointer(), &out));}
	FileSink(const char *filename, bool binary=true)
		{IsolatedInitialize(MakeParameters(Name::OutputFileName(), filename)(Name::OutputBinaryMode(), binary));}
#if defined(CRYPTOPP_UNIX_AVAILABLE) || _MSC_VER >= 1400
	//! specify file with Unicode name. On non-Windows OS, this function assumes that setlocale() has been called.
	FileSink(const wchar_t *filename, bool binary=true)
		{IsolatedInitialize(MakeParameters(Name::OutputFileNameWide(), filename)(Name::OutputBinaryMode(), binary));}
#endif

	std::ostream* GetStream() {return m_stream;}

	void IsolatedInitialize(const NameValuePairs &parameters);
	size_t Put2(const byte *inString, size_t length, int messageEnd, bool blocking);
	bool IsolatedFlush(bool hardFlush, bool blocking);

private:
	member_ptr<std::ofstream> m_file;
	std::ostream *m_stream;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class FileStore:
    # // files.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # #ifndef NDEBUG
    # void Files_TestInstantiations()
    # 	FileStore f0;
    # 	FileSource f1;
    # 	FileSink f2;
    # #endif
    # void FileStore::StoreInitialize(const NameValuePairs &parameters)
    # 	m_waiting = false;
    # 	m_stream = NULL;
    # 	m_file.release();
    # 	const char *fileName = NULL;
    # #if defined(CRYPTOPP_UNIX_AVAILABLE) || _MSC_VER >= 1400
    # 	const wchar_t *fileNameWide = NULL;
    # 	if (!parameters.GetValue(Name::InputFileNameWide(), fileNameWide))
    # #endif
    # 		if (!parameters.GetValue(Name::InputFileName(), fileName))
    # 			return;
    # #ifdef CRYPTOPP_UNIX_AVAILABLE
    # 	if (fileNameWide)
    # 		fileName = (narrowed = StringNarrow(fileNameWide)).c_str();
    # #endif
    # #if _MSC_VER >= 1400
    # 	if (fileNameWide)
    # 		if (!*m_file)
    # 			throw OpenErr(StringNarrow(fileNameWide, false));
    # #endif
    # 	if (fileName)
    # 		if (!*m_file)
    # 			throw OpenErr(fileName);
    # 	m_stream = m_file.get();
    # lword FileStore::MaxRetrievable() const
    # 	if (!m_stream)
    # 		return 0;
    # 	streampos current = m_stream->tellg();
    # 	m_stream->seekg(current);
    # 	return end-current;
    # size_t FileStore::TransferTo2(BufferedTransformation &target, lword &transferBytes, const std::string &channel, bool blocking)
    # 	if (!m_stream)
    # 		transferBytes = 0;
    # 		return 0;
    # 	lword size=transferBytes;
    # 	transferBytes = 0;
    # 	if (m_waiting)
    # 		goto output;
    # 	while (size && m_stream->good())
    # 		size_t spaceSize = 1024;
    # 		m_space = HelpCreatePutSpace(target, channel, 1, UnsignedMin(size_t(0)-1, size), spaceSize);
    # 		m_stream->read((char *)m_space, (unsigned int)STDMIN(size, (lword)spaceSize));
    # 		m_len = (size_t)m_stream->gcount();
    # 		size_t blockedBytes;
    # output:
    # 		blockedBytes = target.ChannelPutModifiable2(channel, m_space, m_len, 0, blocking);
    # 		m_waiting = blockedBytes > 0;
    # 		if (m_waiting)
    # 			return blockedBytes;
    # 		size -= m_len;
    # 		transferBytes += m_len;
    # 	if (!m_stream->good() && !m_stream->eof())
    # 		throw ReadErr();
    # 	return 0;
    # size_t FileStore::CopyRangeTo2(BufferedTransformation &target, lword &begin, lword end, const std::string &channel, bool blocking) const
    # 	if (!m_stream)
    # 		return 0;
    # 	if (begin == 0 && end == 1)
    # 		int result = m_stream->peek();
    # 		if (result == char_traits<char>::eof())
    # 			return 0;
    # 		else
    # 			size_t blockedBytes = target.ChannelPut(channel, byte(result), blocking);
    # 			begin += 1-blockedBytes;
    # 			return blockedBytes;
    # 	// TODO: figure out what happens on cin
    # 	streampos current = m_stream->tellg();
    # 	streampos newPosition = current + (streamoff)begin;
    # 	if (newPosition >= endPosition)
    # 		m_stream->seekg(current);
    # 		return 0;	// don't try to seek beyond the end of file
    # 	m_stream->seekg(newPosition);
    # 	try
    # 		assert(!m_waiting);
    # 		lword copyMax = end-begin;
    # 		size_t blockedBytes = const_cast<FileStore *>(this)->TransferTo2(target, copyMax, channel, blocking);
    # 		begin += copyMax;
    # 		if (blockedBytes)
    # 			const_cast<FileStore *>(this)->m_waiting = false;
    # 			return blockedBytes;
    # 	catch(...)
    # 		m_stream->clear();
    # 		m_stream->seekg(current);
    # 		throw;
    # 	m_stream->clear();
    # 	m_stream->seekg(current);
    # 	return 0;
    # lword FileStore::Skip(lword skipMax)
    # 	if (!m_stream)
    # 		return 0;
    # 	lword oldPos = m_stream->tellg();
    # 	if (!SafeConvert(skipMax, offset))
    # 		throw InvalidArgument("FileStore: maximum seek offset exceeded");
    # 	return (lword)m_stream->tellg() - oldPos;
    # void FileSink::IsolatedInitialize(const NameValuePairs &parameters)
    # 	m_stream = NULL;
    # 	m_file.release();
    # 	const char *fileName = NULL;
    # #if defined(CRYPTOPP_UNIX_AVAILABLE) || _MSC_VER >= 1400
    # 	const wchar_t *fileNameWide = NULL;
    # 	if (!parameters.GetValue(Name::OutputFileNameWide(), fileNameWide))
    # #endif
    # 		if (!parameters.GetValue(Name::OutputFileName(), fileName))
    # 			return;
    # #ifdef CRYPTOPP_UNIX_AVAILABLE
    # 	if (fileNameWide)
    # 		fileName = (narrowed = StringNarrow(fileNameWide)).c_str();
    # #endif
    # #if _MSC_VER >= 1400
    # 	if (fileNameWide)
    # 		if (!*m_file)
    # 			throw OpenErr(StringNarrow(fileNameWide, false));
    # #endif
    # 	if (fileName)
    # 		if (!*m_file)
    # 			throw OpenErr(fileName);
    # 	m_stream = m_file.get();
    # bool FileSink::IsolatedFlush(bool hardFlush, bool blocking)
    # 	if (!m_stream)
    # 		throw Err("FileSink: output stream not opened");
    # 	m_stream->flush();
    # 	if (!m_stream->good())
    # 		throw WriteErr();
    # 	return false;
    # size_t FileSink::Put2(const byte *inString, size_t length, int messageEnd, bool blocking)
    # 	if (!m_stream)
    # 		throw Err("FileSink: output stream not opened");
    # 	while (length > 0)
    # 		if (!SafeConvert(length, size))
    # 		m_stream->write((const char *)inString, size);
    # 		inString += size;
    # 		length -= (size_t)size;
    # 	if (messageEnd)
    # 		m_stream->flush();
    # 	if (!m_stream->good())
    # 		throw WriteErr();
    # 	return 0;
    # NAMESPACE_END
    # #endif