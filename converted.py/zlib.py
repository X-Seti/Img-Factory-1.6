"""
zlib.py - Python conversion of zlib.h and zlib.cpp

Original C++ header content:
#ifndef CRYPTOPP_ZLIB_H
#define CRYPTOPP_ZLIB_H

#include "adler32.h"
#include "zdeflate.h"
#include "zinflate.h"

NAMESPACE_BEGIN(CryptoPP)

/// ZLIB Compressor (RFC 1950)
class ZlibCompressor : public Deflator
{
public:
	ZlibCompressor(BufferedTransformation *attachment=NULL, unsigned int deflateLevel=DEFAULT_DEFLATE_LEVEL, unsigned int log2WindowSize=DEFAULT_LOG2_WINDOW_SIZE, bool detectUncompressible=true)
		: Deflator(attachment, deflateLevel, log2WindowSize, detectUncompressible) {}
	ZlibCompressor(const NameValuePairs &parameters, BufferedTransformation *attachment=NULL)
		: Deflator(parameters, attachment) {}

	unsigned int GetCompressionLevel() const;

protected:
	void WritePrestreamHeader();
	void ProcessUncompressedData(const byte *string, size_t length);
	void WritePoststreamTail();

	Adler32 m_adler32;
};

/// ZLIB Decompressor (RFC 1950)
class ZlibDecompressor : public Inflator
{
public:
	typedef Inflator::Err Err;
	class HeaderErr : public Err {public: HeaderErr() : Err(INVALID_DATA_FORMAT, "ZlibDecompressor: header decoding error") {}};
	class Adler32Err : public Err {public: Adler32Err() : Err(DATA_INTEGRITY_CHECK_FAILED, "ZlibDecompressor: ADLER32 check error") {}};
	class UnsupportedAlgorithm : public Err {public: UnsupportedAlgorithm() : Err(INVALID_DATA_FORMAT, "ZlibDecompressor: unsupported algorithm") {}};
	class UnsupportedPresetDictionary : public Err {public: UnsupportedPresetDictionary() : Err(INVALID_DATA_FORMAT, "ZlibDecompressor: unsupported preset dictionary") {}};

	/*! \param repeat decompress multiple compressed streams in series
		\param autoSignalPropagation 0 to turn off MessageEnd signal
	*/
	ZlibDecompressor(BufferedTransformation *attachment = NULL, bool repeat = false, int autoSignalPropagation = -1);
	unsigned int GetLog2WindowSize() const {return m_log2WindowSize;}

private:
	unsigned int MaxPrestreamHeaderSize() const {return 2;}
	void ProcessPrestreamHeader();
	void ProcessDecompressedData(const byte *string, size_t length);
	unsigned int MaxPoststreamTailSize() const {return 4;}
	void ProcessPoststreamTail();

	unsigned int m_log2WindowSize;
	Adler32 m_adler32;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class ZlibCompressor:
    # // zlib.cpp - written and placed in the public domain by Wei Dai
    # // "zlib" is the name of a well known C language compression library
    # // (http://www.zlib.org) and also the name of a compression format
    # // (RFC 1950) that the library implements. This file is part of a
    # // complete reimplementation of the zlib compression format.
    # NAMESPACE_BEGIN(CryptoPP)
    # static const byte DEFLATE_METHOD = 8;
    # static const byte FDICT_FLAG = 1 << 5;
    # // *************************************************************
    # void ZlibCompressor::WritePrestreamHeader()
    # 	m_adler32.Restart();
    # 	byte cmf = DEFLATE_METHOD | ((GetLog2WindowSize()-8) << 4);
    # 	byte flags = GetCompressionLevel() << 6;
    # 	AttachedTransformation()->PutWord16(RoundUpToMultipleOf(cmf*256+flags, 31));
    # void ZlibCompressor::ProcessUncompressedData(const byte *inString, size_t length)
    # 	m_adler32.Update(inString, length);
    # void ZlibCompressor::WritePoststreamTail()
    # 	FixedSizeSecBlock<byte, 4> adler32;
    # 	m_adler32.Final(adler32);
    # 	AttachedTransformation()->Put(adler32, 4);
    # unsigned int ZlibCompressor::GetCompressionLevel() const
    # 	static const unsigned int deflateToCompressionLevel[] = {0, 1, 1, 1, 2, 2, 2, 2, 2, 3};
    # 	return deflateToCompressionLevel[GetDeflateLevel()];
    # // *************************************************************
    # ZlibDecompressor::ZlibDecompressor(BufferedTransformation *attachment, bool repeat, int propagation)
    # 	: Inflator(attachment, repeat, propagation)
    # void ZlibDecompressor::ProcessPrestreamHeader()
    # 	m_adler32.Restart();
    # 	byte cmf;
    # 	byte flags;
    # 	if (!m_inQueue.Get(cmf) || !m_inQueue.Get(flags))
    # 		throw HeaderErr();
    # 	if ((cmf*256+flags) % 31 != 0)
    # 		throw HeaderErr();	// if you hit this exception, you're probably trying to decompress invalid data
    # 	if ((cmf & 0xf) != DEFLATE_METHOD)
    # 		throw UnsupportedAlgorithm();
    # 	if (flags & FDICT_FLAG)
    # 		throw UnsupportedPresetDictionary();
    # 	m_log2WindowSize = 8 + (cmf >> 4);
    # void ZlibDecompressor::ProcessDecompressedData(const byte *inString, size_t length)
    # 	AttachedTransformation()->Put(inString, length);
    # 	m_adler32.Update(inString, length);
    # void ZlibDecompressor::ProcessPoststreamTail()
    # 	FixedSizeSecBlock<byte, 4> adler32;
    # 	if (m_inQueue.Get(adler32, 4) != 4)
    # 		throw Adler32Err();
    # 	if (!m_adler32.Verify(adler32))
    # 		throw Adler32Err();
    # NAMESPACE_END