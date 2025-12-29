"""
gzip.py - Python conversion of gzip.h and gzip.cpp

Original C++ header content:
#ifndef CRYPTOPP_GZIP_H
#define CRYPTOPP_GZIP_H

#include "zdeflate.h"
#include "zinflate.h"
#include "crc.h"

NAMESPACE_BEGIN(CryptoPP)

/// GZIP Compression (RFC 1952)
class Gzip : public Deflator
{
public:
	Gzip(BufferedTransformation *attachment=NULL, unsigned int deflateLevel=DEFAULT_DEFLATE_LEVEL, unsigned int log2WindowSize=DEFAULT_LOG2_WINDOW_SIZE, bool detectUncompressible=true)
		: Deflator(attachment, deflateLevel, log2WindowSize, detectUncompressible) {}
	Gzip(const NameValuePairs &parameters, BufferedTransformation *attachment=NULL)
		: Deflator(parameters, attachment) {}

protected:
	enum {MAGIC1=0x1f, MAGIC2=0x8b,   // flags for the header
		  DEFLATED=8, FAST=4, SLOW=2};

	void WritePrestreamHeader();
	void ProcessUncompressedData(const byte *string, size_t length);
	void WritePoststreamTail();

	word32 m_totalLen;
	CRC32 m_crc;
};

/// GZIP Decompression (RFC 1952)
class Gunzip : public Inflator
{
public:
	typedef Inflator::Err Err;
	class HeaderErr : public Err {public: HeaderErr() : Err(INVALID_DATA_FORMAT, "Gunzip: header decoding error") {}};
	class TailErr : public Err {public: TailErr() : Err(INVALID_DATA_FORMAT, "Gunzip: tail too short") {}};
	class CrcErr : public Err {public: CrcErr() : Err(DATA_INTEGRITY_CHECK_FAILED, "Gunzip: CRC check error") {}};
	class LengthErr : public Err {public: LengthErr() : Err(DATA_INTEGRITY_CHECK_FAILED, "Gunzip: length check error") {}};

	/*! \param repeat decompress multiple compressed streams in series
		\param autoSignalPropagation 0 to turn off MessageEnd signal
	*/
	Gunzip(BufferedTransformation *attachment = NULL, bool repeat = false, int autoSignalPropagation = -1);

protected:
	enum {MAGIC1=0x1f, MAGIC2=0x8b,   // flags for the header
		DEFLATED=8};

	enum FLAG_MASKS {
		CONTINUED=2, EXTRA_FIELDS=4, FILENAME=8, COMMENTS=16, ENCRYPTED=32};

	unsigned int MaxPrestreamHeaderSize() const {return 1024;}
	void ProcessPrestreamHeader();
	void ProcessDecompressedData(const byte *string, size_t length);
	unsigned int MaxPoststreamTailSize() const {return 8;}
	void ProcessPoststreamTail();

	word32 m_length;
	CRC32 m_crc;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class Gzip:
    # // gzip.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # void Gzip::WritePrestreamHeader()
    # 	m_totalLen = 0;
    # 	m_crc.Restart();
    # 	AttachedTransformation()->Put(MAGIC1);
    # 	AttachedTransformation()->Put(MAGIC2);
    # 	AttachedTransformation()->Put(DEFLATED);
    # 	AttachedTransformation()->Put(0);		// general flag
    # 	AttachedTransformation()->PutWord32(0);	// time stamp
    # 	byte extra = (GetDeflateLevel() == 1) ? FAST : ((GetDeflateLevel() == 9) ? SLOW : 0);
    # 	AttachedTransformation()->Put(extra);
    # 	AttachedTransformation()->Put(GZIP_OS_CODE);
    # void Gzip::ProcessUncompressedData(const byte *inString, size_t length)
    # 	m_crc.Update(inString, length);
    # 	m_totalLen += (word32)length;
    # void Gzip::WritePoststreamTail()
    # 	SecByteBlock crc(4);
    # 	m_crc.Final(crc);
    # 	AttachedTransformation()->Put(crc, 4);
    # 	AttachedTransformation()->PutWord32(m_totalLen, LITTLE_ENDIAN_ORDER);
    # // *************************************************************
    # Gunzip::Gunzip(BufferedTransformation *attachment, bool repeat, int propagation)
    # 	: Inflator(attachment, repeat, propagation)
    # void Gunzip::ProcessPrestreamHeader()
    # 	m_length = 0;
    # 	m_crc.Restart();
    # 	byte buf[6];
    # 	byte b, flags;
    # 	if (m_inQueue.Get(buf, 2)!=2) throw HeaderErr();
    # 	if (buf[0] != MAGIC1 || buf[1] != MAGIC2) throw HeaderErr();
    # 	if (!m_inQueue.Skip(1)) throw HeaderErr();	 // skip extra flags
    # 	if (!m_inQueue.Get(flags)) throw HeaderErr();
    # 	if (flags & (ENCRYPTED | CONTINUED)) throw HeaderErr();
    # 	if (m_inQueue.Skip(6)!=6) throw HeaderErr();    // Skip file time, extra flags and OS type
    # 	if (flags & EXTRA_FIELDS)	// skip extra fields
    # 		word16 length;
    # 		if (m_inQueue.GetWord16(length, LITTLE_ENDIAN_ORDER) != 2) throw HeaderErr();
    # 		if (m_inQueue.Skip(length)!=length) throw HeaderErr();
    # 	if (flags & FILENAME)	// skip filename
    # 		do
    # 			if(!m_inQueue.Get(b)) throw HeaderErr();
    # 		while (b);
    # 	if (flags & COMMENTS)	// skip comments
    # 		do
    # 			if(!m_inQueue.Get(b)) throw HeaderErr();
    # 		while (b);
    # void Gunzip::ProcessDecompressedData(const byte *inString, size_t length)
    # 	AttachedTransformation()->Put(inString, length);
    # 	m_crc.Update(inString, length);
    # 	m_length += (word32)length;
    # void Gunzip::ProcessPoststreamTail()
    # 	SecByteBlock crc(4);
    # 	if (m_inQueue.Get(crc, 4) != 4)
    # 		throw TailErr();
    # 	if (!m_crc.Verify(crc))
    # 		throw CrcErr();
    # 	word32 lengthCheck;
    # 	if (m_inQueue.GetWord32(lengthCheck, LITTLE_ENDIAN_ORDER) != 4)
    # 		throw TailErr();
    # 	if (lengthCheck != m_length)
    # 		throw LengthErr();
    # NAMESPACE_END