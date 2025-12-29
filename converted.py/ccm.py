"""
ccm.py - Python conversion of ccm.h and ccm.cpp

Original C++ header content:
#ifndef CRYPTOPP_CCM_H
#define CRYPTOPP_CCM_H

#include "authenc.h"
#include "modes.h"

NAMESPACE_BEGIN(CryptoPP)

//! .
class CRYPTOPP_DLL CRYPTOPP_NO_VTABLE CCM_Base : public AuthenticatedSymmetricCipherBase
{
public:
	CCM_Base()
		: m_digestSize(0), m_L(0) {}

	// AuthenticatedSymmetricCipher
	std::string AlgorithmName() const
		{return GetBlockCipher().AlgorithmName() + std::string("/CCM");}
	size_t MinKeyLength() const
		{return GetBlockCipher().MinKeyLength();}
	size_t MaxKeyLength() const
		{return GetBlockCipher().MaxKeyLength();}
	size_t DefaultKeyLength() const
		{return GetBlockCipher().DefaultKeyLength();}
	size_t GetValidKeyLength(size_t n) const
		{return GetBlockCipher().GetValidKeyLength(n);}
	bool IsValidKeyLength(size_t n) const
		{return GetBlockCipher().IsValidKeyLength(n);}
	unsigned int OptimalDataAlignment() const
		{return GetBlockCipher().OptimalDataAlignment();}
	IV_Requirement IVRequirement() const
		{return UNIQUE_IV;}
	unsigned int IVSize() const
		{return 8;}
	unsigned int MinIVLength() const
		{return 7;}
	unsigned int MaxIVLength() const
		{return 13;}
	unsigned int DigestSize() const
		{return m_digestSize;}
	lword MaxHeaderLength() const
		{return W64LIT(0)-1;}
	lword MaxMessageLength() const
		{return m_L<8 ? (W64LIT(1)<<(8*m_L))-1 : W64LIT(0)-1;}
	bool NeedsPrespecifiedDataLengths() const
		{return true;}
	void UncheckedSpecifyDataLengths(lword headerLength, lword messageLength, lword footerLength);

protected:
	// AuthenticatedSymmetricCipherBase
	bool AuthenticationIsOnPlaintext() const
		{return true;}
	unsigned int AuthenticationBlockSize() const
		{return GetBlockCipher().BlockSize();}
	void SetKeyWithoutResync(const byte *userKey, size_t keylength, const NameValuePairs &params);
	void Resync(const byte *iv, size_t len);
	size_t AuthenticateBlocks(const byte *data, size_t len);
	void AuthenticateLastHeaderBlock();
	void AuthenticateLastConfidentialBlock();
	void AuthenticateLastFooterBlock(byte *mac, size_t macSize);
	SymmetricCipher & AccessSymmetricCipher() {return m_ctr;}

	virtual BlockCipher & AccessBlockCipher() =0;
	virtual int DefaultDigestSize() const =0;

	const BlockCipher & GetBlockCipher() const {return const_cast<CCM_Base *>(this)->AccessBlockCipher();};
	byte *CBC_Buffer() {return m_buffer+REQUIRED_BLOCKSIZE;}

	enum {REQUIRED_BLOCKSIZE = 16};
	int m_digestSize, m_L;
	word64 m_messageLength, m_aadLength;
	CTR_Mode_ExternalCipher::Encryption m_ctr;
};

//! .
template <class T_BlockCipher, int T_DefaultDigestSize, bool T_IsEncryption>
class CCM_Final : public CCM_Base
{
public:
	static std::string StaticAlgorithmName()
		{return T_BlockCipher::StaticAlgorithmName() + std::string("/CCM");}
	bool IsForwardTransformation() const
		{return T_IsEncryption;}

private:
	BlockCipher & AccessBlockCipher() {return m_cipher;}
	int DefaultDigestSize() const {return T_DefaultDigestSize;}
	typename T_BlockCipher::Encryption m_cipher;
};

/// <a href="http://www.cryptolounge.org/wiki/CCM">CCM</a>
template <class T_BlockCipher, int T_DefaultDigestSize = 16>
struct CCM : public AuthenticatedSymmetricCipherDocumentation
{
	typedef CCM_Final<T_BlockCipher, T_DefaultDigestSize, true> Encryption;
	typedef CCM_Final<T_BlockCipher, T_DefaultDigestSize, false> Decryption;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class CCM_Base:
    # // ccm.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # void CCM_Base::SetKeyWithoutResync(const byte *userKey, size_t keylength, const NameValuePairs &params)
    # 	BlockCipher &blockCipher = AccessBlockCipher();
    # 	blockCipher.SetKey(userKey, keylength, params);
    # 	if (blockCipher.BlockSize() != REQUIRED_BLOCKSIZE)
    # 		throw InvalidArgument(AlgorithmName() + ": block size of underlying block cipher is not 16");
    # 	if (m_digestSize % 2 > 0 || m_digestSize < 4 || m_digestSize > 16)
    # 		throw InvalidArgument(AlgorithmName() + ": DigestSize must be 4, 6, 8, 10, 12, 14, or 16");
    # 	m_buffer.Grow(2*REQUIRED_BLOCKSIZE);
    # 	m_L = 8;
    # void CCM_Base::Resync(const byte *iv, size_t len)
    # 	BlockCipher &cipher = AccessBlockCipher();
    # 	m_L = REQUIRED_BLOCKSIZE-1-(int)len;
    # 	assert(m_L >= 2);
    # 	if (m_L > 8)
    # 		m_L = 8;
    # 	m_buffer[0] = byte(m_L-1);	// flag
    # 	memcpy(m_buffer+1, iv, len);
    # 	memset(m_buffer+1+len, 0, REQUIRED_BLOCKSIZE-1-len);
    # 	if (m_state >= State_IVSet)
    # 		m_ctr.Resynchronize(m_buffer, REQUIRED_BLOCKSIZE);
    # 	else
    # 		m_ctr.SetCipherWithIV(cipher, m_buffer);
    # 	m_ctr.Seek(REQUIRED_BLOCKSIZE);
    # 	m_aadLength = 0; 
    # 	m_messageLength = 0;
    # void CCM_Base::UncheckedSpecifyDataLengths(lword headerLength, lword messageLength, lword footerLength)
    # 	if (m_state != State_IVSet)
    # 		throw BadState(AlgorithmName(), "SpecifyDataLengths", "or after State_IVSet");
    # 	m_aadLength = headerLength; 
    # 	m_messageLength = messageLength;
    # 	byte *cbcBuffer = CBC_Buffer();
    # 	const BlockCipher &cipher = GetBlockCipher();
    # 	cbcBuffer[0] = byte(64*(headerLength>0) + 8*((m_digestSize-2)/2) + (m_L-1));	// flag
    # 	PutWord<word64>(true, BIG_ENDIAN_ORDER, cbcBuffer+REQUIRED_BLOCKSIZE-8, m_messageLength);
    # 	memcpy(cbcBuffer+1, m_buffer+1, REQUIRED_BLOCKSIZE-1-m_L);
    # 	cipher.ProcessBlock(cbcBuffer);
    # 	if (headerLength>0)
    # 		assert(m_bufferedDataLength == 0);
    # 		if (headerLength < ((1<<16) - (1<<8)))
    # 			PutWord<word16>(true, BIG_ENDIAN_ORDER, m_buffer, (word16)headerLength);
    # 			m_bufferedDataLength = 2;
    # 		else if (headerLength < (W64LIT(1)<<32))
    # 			m_buffer[0] = 0xff;
    # 			m_buffer[1] = 0xfe;
    # 			PutWord<word32>(false, BIG_ENDIAN_ORDER, m_buffer+2, (word32)headerLength);
    # 			m_bufferedDataLength = 6;
    # 		else
    # 			m_buffer[0] = 0xff;
    # 			m_buffer[1] = 0xff;
    # 			PutWord<word64>(false, BIG_ENDIAN_ORDER, m_buffer+2, headerLength);
    # 			m_bufferedDataLength = 10;
    # size_t CCM_Base::AuthenticateBlocks(const byte *data, size_t len)
    # 	byte *cbcBuffer = CBC_Buffer();
    # 	const BlockCipher &cipher = GetBlockCipher();
    # void CCM_Base::AuthenticateLastHeaderBlock()
    # 	byte *cbcBuffer = CBC_Buffer();
    # 	const BlockCipher &cipher = GetBlockCipher();
    # 	if (m_aadLength != m_totalHeaderLength)
    # 		throw InvalidArgument(AlgorithmName() + ": header length doesn't match that given in SpecifyDataLengths");
    # 	if (m_bufferedDataLength > 0)
    # 		xorbuf(cbcBuffer, m_buffer, m_bufferedDataLength);
    # 		cipher.ProcessBlock(cbcBuffer);
    # 		m_bufferedDataLength = 0;
    # void CCM_Base::AuthenticateLastConfidentialBlock()
    # 	byte *cbcBuffer = CBC_Buffer();
    # 	const BlockCipher &cipher = GetBlockCipher();
    # 	if (m_messageLength != m_totalMessageLength)
    # 		throw InvalidArgument(AlgorithmName() + ": message length doesn't match that given in SpecifyDataLengths");
    # 	if (m_bufferedDataLength > 0)
    # 		xorbuf(cbcBuffer, m_buffer, m_bufferedDataLength);
    # 		cipher.ProcessBlock(cbcBuffer);
    # 		m_bufferedDataLength = 0;
    # void CCM_Base::AuthenticateLastFooterBlock(byte *mac, size_t macSize)
    # 	m_ctr.Seek(0);
    # 	m_ctr.ProcessData(mac, CBC_Buffer(), macSize);
    # NAMESPACE_END
    # #endif