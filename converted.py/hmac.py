"""
hmac.py - Python conversion of hmac.h and hmac.cpp

Original C++ header content:
// hmac.h - written and placed in the public domain by Wei Dai

#ifndef CRYPTOPP_HMAC_H
#define CRYPTOPP_HMAC_H

#include "seckey.h"
#include "secblock.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
class CRYPTOPP_DLL CRYPTOPP_NO_VTABLE HMAC_Base : public VariableKeyLength<16, 0, INT_MAX>, public MessageAuthenticationCode
{
public:
	HMAC_Base() : m_innerHashKeyed(false) {}
	void UncheckedSetKey(const byte *userKey, unsigned int keylength, const NameValuePairs &params);

	void Restart();
	void Update(const byte *input, size_t length);
	void TruncatedFinal(byte *mac, size_t size);
	unsigned int OptimalBlockSize() const {return const_cast<HMAC_Base*>(this)->AccessHash().OptimalBlockSize();}
	unsigned int DigestSize() const {return const_cast<HMAC_Base*>(this)->AccessHash().DigestSize();}

protected:
	virtual HashTransformation & AccessHash() =0;
	byte * AccessIpad() {return m_buf;}
	byte * AccessOpad() {return m_buf + AccessHash().BlockSize();}
	byte * AccessInnerHash() {return m_buf + 2*AccessHash().BlockSize();}

private:
	void KeyInnerHash();

	SecByteBlock m_buf;
	bool m_innerHashKeyed;
};

//! <a href="http://www.weidai.com/scan-mirror/mac.html#HMAC">HMAC</a>
/*! HMAC(K, text) = H(K XOR opad, H(K XOR ipad, text)) */
template <class T>
class HMAC : public MessageAuthenticationCodeImpl<HMAC_Base, HMAC<T> >
{
public:
	CRYPTOPP_CONSTANT(DIGESTSIZE=T::DIGESTSIZE)
	CRYPTOPP_CONSTANT(BLOCKSIZE=T::BLOCKSIZE)

	HMAC() {}
	HMAC(const byte *key, size_t length=HMAC_Base::DEFAULT_KEYLENGTH)
		{this->SetKey(key, length);}

	static std::string StaticAlgorithmName() {return std::string("HMAC(") + T::StaticAlgorithmName() + ")";}
	std::string AlgorithmName() const {return std::string("HMAC(") + m_hash.AlgorithmName() + ")";}

private:
	HashTransformation & AccessHash() {return m_hash;}

	T m_hash;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class HMAC_Base:
    # // hmac.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # void HMAC_Base::UncheckedSetKey(const byte *userKey, unsigned int keylength, const NameValuePairs &)
    # 	AssertValidKeyLength(keylength);
    # 	Restart();
    # 	HashTransformation &hash = AccessHash();
    # 	unsigned int blockSize = hash.BlockSize();
    # 	if (!blockSize)
    # 		throw InvalidArgument("HMAC: can only be used with a block-based hash function");
    # 	m_buf.resize(2*AccessHash().BlockSize() + AccessHash().DigestSize());
    # 	if (keylength <= blockSize)
    # 		memcpy(AccessIpad(), userKey, keylength);
    # 	else
    # 		AccessHash().CalculateDigest(AccessIpad(), userKey, keylength);
    # 		keylength = hash.DigestSize();
    # 	assert(keylength <= blockSize);
    # 	memset(AccessIpad()+keylength, 0, blockSize-keylength);
    # 	for (unsigned int i=0; i<blockSize; i++)
    # 		AccessOpad()[i] = AccessIpad()[i] ^ 0x5c;
    # 		AccessIpad()[i] ^= 0x36;
    # void HMAC_Base::KeyInnerHash()
    # 	assert(!m_innerHashKeyed);
    # 	HashTransformation &hash = AccessHash();
    # 	hash.Update(AccessIpad(), hash.BlockSize());
    # 	m_innerHashKeyed = true;
    # void HMAC_Base::Restart()
    # 	if (m_innerHashKeyed)
    # 		AccessHash().Restart();
    # 		m_innerHashKeyed = false;
    # void HMAC_Base::Update(const byte *input, size_t length)
    # 	if (!m_innerHashKeyed)
    # 		KeyInnerHash();
    # 	AccessHash().Update(input, length);
    # void HMAC_Base::TruncatedFinal(byte *mac, size_t size)
    # 	ThrowIfInvalidTruncatedSize(size);
    # 	HashTransformation &hash = AccessHash();
    # 	if (!m_innerHashKeyed)
    # 		KeyInnerHash();
    # 	hash.Final(AccessInnerHash());
    # 	hash.Update(AccessOpad(), hash.BlockSize());
    # 	hash.Update(AccessInnerHash(), hash.DigestSize());
    # 	hash.TruncatedFinal(mac, size);
    # 	m_innerHashKeyed = false;
    # NAMESPACE_END
    # #endif