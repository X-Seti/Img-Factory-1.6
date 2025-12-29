"""
tea.py - Python conversion of tea.h and tea.cpp

Original C++ header content:
#ifndef CRYPTOPP_TEA_H
#define CRYPTOPP_TEA_H

/** \file
*/

#include "seckey.h"
#include "secblock.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
struct TEA_Info : public FixedBlockSize<8>, public FixedKeyLength<16>, public VariableRounds<32>
{
	static const char *StaticAlgorithmName() {return "TEA";}
};

/// <a href="http://www.weidai.com/scan-mirror/cs.html#TEA">TEA</a>
class TEA : public TEA_Info, public BlockCipherDocumentation
{
	class CRYPTOPP_NO_VTABLE Base : public BlockCipherImpl<TEA_Info>
	{
	public:
		void UncheckedSetKey(const byte *userKey, unsigned int length, const NameValuePairs &params);

	protected:
		FixedSizeSecBlock<word32, 4> m_k;
		word32 m_limit;
	};

	class CRYPTOPP_NO_VTABLE Enc : public Base
	{
	public:
		void ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const;
	};

	class CRYPTOPP_NO_VTABLE Dec : public Base
	{
	public:
		void ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const;
	};

public:
	typedef BlockCipherFinal<ENCRYPTION, Enc> Encryption;
	typedef BlockCipherFinal<DECRYPTION, Dec> Decryption;
};

typedef TEA::Encryption TEAEncryption;
typedef TEA::Decryption TEADecryption;

//! _
struct XTEA_Info : public FixedBlockSize<8>, public FixedKeyLength<16>, public VariableRounds<32>
{
	static const char *StaticAlgorithmName() {return "XTEA";}
};

/// <a href="http://www.weidai.com/scan-mirror/cs.html#TEA">XTEA</a>
class XTEA : public XTEA_Info, public BlockCipherDocumentation
{
	class CRYPTOPP_NO_VTABLE Base : public BlockCipherImpl<XTEA_Info>
	{
	public:
		void UncheckedSetKey(const byte *userKey, unsigned int length, const NameValuePairs &params);

	protected:
		FixedSizeSecBlock<word32, 4> m_k;
		word32 m_limit;
	};

	class CRYPTOPP_NO_VTABLE Enc : public Base
	{
	public:
		void ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const;
	};

	class CRYPTOPP_NO_VTABLE Dec : public Base
	{
	public:
		void ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const;
	};

public:
	typedef BlockCipherFinal<ENCRYPTION, Enc> Encryption;
	typedef BlockCipherFinal<DECRYPTION, Dec> Decryption;
};

//! _
struct BTEA_Info : public FixedKeyLength<16>
{
	static const char *StaticAlgorithmName() {return "BTEA";}
};

//! <a href="http://www.weidai.com/scan-mirror/cs.html#TEA">corrected Block TEA</a> (as described in "xxtea").
/*! This class hasn't been tested yet. */
class BTEA : public BTEA_Info, public BlockCipherDocumentation
{
	class CRYPTOPP_NO_VTABLE Base : public AlgorithmImpl<SimpleKeyingInterfaceImpl<BlockCipher, BTEA_Info>, BTEA_Info>, public BTEA_Info
	{
	public:
		void UncheckedSetKey(const byte *key, unsigned int length, const NameValuePairs &params)
		{
			m_blockSize = params.GetIntValueWithDefault("BlockSize", 60*4);
			GetUserKey(BIG_ENDIAN_ORDER, m_k.begin(), 4, key, KEYLENGTH);
		}

		unsigned int BlockSize() const {return m_blockSize;}

	protected:
		FixedSizeSecBlock<word32, 4> m_k;
		unsigned int m_blockSize;
	};

	class CRYPTOPP_NO_VTABLE Enc : public Base
	{
	public:
		void ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const;
	};

	class CRYPTOPP_NO_VTABLE Dec : public Base
	{
	public:
		void ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const;
	};

public:
	typedef BlockCipherFinal<ENCRYPTION, Enc> Encryption;
	typedef BlockCipherFinal<DECRYPTION, Dec> Decryption;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class TEA:
    # // tea.cpp - modified by Wei Dai from code in the original paper
    # NAMESPACE_BEGIN(CryptoPP)
    # static const word32 DELTA = 0x9e3779b9;
    # typedef BlockGetAndPut<word32, BigEndian> Block;
    # void TEA::Base::UncheckedSetKey(const byte *userKey, unsigned int length, const NameValuePairs &params)
    # 	AssertValidKeyLength(length);
    # 	GetUserKey(BIG_ENDIAN_ORDER, m_k.begin(), 4, userKey, KEYLENGTH);
    # 	m_limit = GetRoundsAndThrowIfInvalid(params, this) * DELTA;
    # void TEA::Enc::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	word32 y, z;
    # 	word32 sum = 0;
    # 	while (sum != m_limit)
    # 		sum += DELTA;
    # 		y += (z << 4) + m_k[0] ^ z + sum ^ (z >> 5) + m_k[1];
    # 		z += (y << 4) + m_k[2] ^ y + sum ^ (y >> 5) + m_k[3];
    # void TEA::Dec::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	word32 y, z;
    # 	word32 sum = m_limit;
    # 	while (sum != 0)
    # 		z -= (y << 4) + m_k[2] ^ y + sum ^ (y >> 5) + m_k[3]; 
    # 		y -= (z << 4) + m_k[0] ^ z + sum ^ (z >> 5) + m_k[1];
    # 		sum -= DELTA;
    # void XTEA::Base::UncheckedSetKey(const byte *userKey, unsigned int length,  const NameValuePairs &params)
    # 	AssertValidKeyLength(length);
    # 	GetUserKey(BIG_ENDIAN_ORDER, m_k.begin(), 4, userKey, KEYLENGTH);
    # 	m_limit = GetRoundsAndThrowIfInvalid(params, this) * DELTA;
    # void XTEA::Enc::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	word32 y, z;
    # #ifdef __SUNPRO_CC
    # 	// workaround needed on Sun Studio 12u1 Sun C++ 5.10 SunOS_i386 128229-02 2009/09/21
    # 	size_t sum = 0;
    # 	while ((sum&0xffffffff) != m_limit)
    # #else
    # 	word32 sum = 0;
    # 	while (sum != m_limit)
    # #endif
    # 		y += (z<<4 ^ z>>5) + z ^ sum + m_k[sum&3];
    # 		sum += DELTA;
    # 		z += (y<<4 ^ y>>5) + y ^ sum + m_k[sum>>11 & 3];
    # void XTEA::Dec::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	word32 y, z;
    # #ifdef __SUNPRO_CC
    # 	// workaround needed on Sun Studio 12u1 Sun C++ 5.10 SunOS_i386 128229-02 2009/09/21
    # 	size_t sum = m_limit;
    # 	while ((sum&0xffffffff) != 0)
    # #else
    # 	word32 sum = m_limit;
    # 	while (sum != 0)
    # #endif
    # 		z -= (y<<4 ^ y>>5) + y ^ sum + m_k[sum>>11 & 3];
    # 		sum -= DELTA;
    # 		y -= (z<<4 ^ z>>5) + z ^ sum + m_k[sum&3];
    # #define MX (z>>5^y<<2)+(y>>3^z<<4)^(sum^y)+(m_k[p&3^e]^z)
    # void BTEA::Enc::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	unsigned int n = m_blockSize / 4;
    # 	word32 *v = (word32*)outBlock;
    # 	ConditionalByteReverse(BIG_ENDIAN_ORDER, v, (const word32*)inBlock, m_blockSize);
    # 	word32 y = v[0], z = v[n-1], e;
    # 	word32 p, q = 6+52/n;
    # 	word32 sum = 0;
    # 	while (q-- > 0)
    # 		sum += DELTA;
    # 		e = sum>>2 & 3;
    # 		for (p = 0; p < n-1; p++)
    # 			y = v[p+1];
    # 			z = v[p] += MX;
    # 		y = v[0];
    # 		z = v[n-1] += MX;
    # 	ConditionalByteReverse(BIG_ENDIAN_ORDER, v, v, m_blockSize);
    # void BTEA::Dec::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	unsigned int n = m_blockSize / 4;
    # 	word32 *v = (word32*)outBlock;
    # 	ConditionalByteReverse(BIG_ENDIAN_ORDER, v, (const word32*)inBlock, m_blockSize);
    # 	word32 y = v[0], z = v[n-1], e;
    # 	word32 p, q = 6+52/n;
    # 	word32 sum = q * DELTA;
    # 	while (sum != 0)
    # 		e = sum>>2 & 3;
    # 		for (p = n-1; p > 0; p--)
    # 			z = v[p-1];
    # 			y = v[p] -= MX;
    # 		z = v[n-1];
    # 		y = v[0] -= MX;
    # 		sum -= DELTA;
    # 	ConditionalByteReverse(BIG_ENDIAN_ORDER, v, v, m_blockSize);
    # NAMESPACE_END