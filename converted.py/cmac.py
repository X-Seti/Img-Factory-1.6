"""
cmac.py - Python conversion of cmac.h and cmac.cpp

Original C++ header content:
#ifndef CRYPTOPP_CMAC_H
#define CRYPTOPP_CMAC_H

#include "seckey.h"
#include "secblock.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
class CRYPTOPP_DLL CRYPTOPP_NO_VTABLE CMAC_Base : public MessageAuthenticationCode
{
public:
	CMAC_Base() {}

	void UncheckedSetKey(const byte *key, unsigned int length, const NameValuePairs &params);
	void Update(const byte *input, size_t length);
	void TruncatedFinal(byte *mac, size_t size);
	unsigned int DigestSize() const {return GetCipher().BlockSize();}
	unsigned int OptimalBlockSize() const {return GetCipher().BlockSize();}
	unsigned int OptimalDataAlignment() const {return GetCipher().OptimalDataAlignment();}

protected:
	friend class EAX_Base;

	const BlockCipher & GetCipher() const {return const_cast<CMAC_Base*>(this)->AccessCipher();}
	virtual BlockCipher & AccessCipher() =0;

	void ProcessBuf();
	SecByteBlock m_reg;
	unsigned int m_counter;
};

/// <a href="http://www.cryptolounge.org/wiki/CMAC">CMAC</a>
/*! Template parameter T should be a class derived from BlockCipherDocumentation, for example AES, with a block size of 8, 16, or 32 */
template <class T>
class CMAC : public MessageAuthenticationCodeImpl<CMAC_Base, CMAC<T> >, public SameKeyLengthAs<T>
{
public:
	CMAC() {}
	CMAC(const byte *key, size_t length=SameKeyLengthAs<T>::DEFAULT_KEYLENGTH)
		{this->SetKey(key, length);}

	static std::string StaticAlgorithmName() {return std::string("CMAC(") + T::StaticAlgorithmName() + ")";}

private:
	BlockCipher & AccessCipher() {return m_cipher;}
	typename T::Encryption m_cipher;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class CMAC_Base:
    # // cmac.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # static void MulU(byte *k, unsigned int length)
    # 	byte carry = 0;
    # 	for (int i=length-1; i>=1; i-=2)
    # 		byte carry2 = k[i] >> 7;
    # 		k[i] += k[i] + carry;
    # 		carry = k[i-1] >> 7;
    # 		k[i-1] += k[i-1] + carry2;
    # 	if (carry)
    # 		switch (length)
    # 		case 8:
    # 			k[7] ^= 0x1b;
    # 			break;
    # 		case 16:
    # 			k[15] ^= 0x87;
    # 			break;
    # 		case 32:
    # 			k[30] ^= 4; 
    # 			k[31] ^= 0x23;
    # 			break;
    # 		default:
    # 			throw InvalidArgument("CMAC: " + IntToString(length) + " is not a supported cipher block size");
    # void CMAC_Base::UncheckedSetKey(const byte *key, unsigned int length, const NameValuePairs &params)
    # 	BlockCipher &cipher = AccessCipher();
    # 	unsigned int blockSize = cipher.BlockSize();
    # 	cipher.SetKey(key, length, params);
    # 	m_reg.CleanNew(3*blockSize);
    # 	m_counter = 0;
    # 	cipher.ProcessBlock(m_reg, m_reg+blockSize);
    # 	MulU(m_reg+blockSize, blockSize);
    # 	memcpy(m_reg+2*blockSize, m_reg+blockSize, blockSize);
    # 	MulU(m_reg+2*blockSize, blockSize);
    # void CMAC_Base::Update(const byte *input, size_t length)
    # 	if (!length)
    # 		return;
    # 	BlockCipher &cipher = AccessCipher();
    # 	unsigned int blockSize = cipher.BlockSize();
    # 	if (m_counter > 0)
    # 		unsigned int len = UnsignedMin(blockSize - m_counter, length);
    # 		xorbuf(m_reg+m_counter, input, len);
    # 		length -= len;
    # 		input += len;
    # 		m_counter += len;
    # 		if (m_counter == blockSize && length > 0)
    # 			cipher.ProcessBlock(m_reg);
    # 			m_counter = 0;
    # 	if (length > blockSize)
    # 		assert(m_counter == 0);
    # 		input += (length - leftOver);
    # 		length = leftOver;
    # 	if (length > 0)
    # 		assert(m_counter + length <= blockSize);
    # 		xorbuf(m_reg+m_counter, input, length);
    # 		m_counter += (unsigned int)length;
    # 	assert(m_counter > 0);
    # void CMAC_Base::TruncatedFinal(byte *mac, size_t size)
    # 	ThrowIfInvalidTruncatedSize(size);
    # 	BlockCipher &cipher = AccessCipher();
    # 	unsigned int blockSize = cipher.BlockSize();
    # 	if (m_counter < blockSize)
    # 		m_reg[m_counter] ^= 0x80;
    # 	else
    # 	memcpy(mac, m_reg, size);
    # 	m_counter = 0;
    # 	memset(m_reg, 0, blockSize);
    # NAMESPACE_END
    # #endif