"""
rc2.py - Python conversion of rc2.h and rc2.cpp

Original C++ header content:
#ifndef CRYPTOPP_RC2_H
#define CRYPTOPP_RC2_H

/** \file
*/

#include "seckey.h"
#include "secblock.h"
#include "algparam.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
struct RC2_Info : public FixedBlockSize<8>, public VariableKeyLength<16, 1, 128>
{
	CRYPTOPP_CONSTANT(DEFAULT_EFFECTIVE_KEYLENGTH = 1024)
	CRYPTOPP_CONSTANT(MAX_EFFECTIVE_KEYLENGTH = 1024)
	static const char *StaticAlgorithmName() {return "RC2";}
};

/// <a href="http://www.weidai.com/scan-mirror/cs.html#RC2">RC2</a>
class RC2 : public RC2_Info, public BlockCipherDocumentation
{
	class CRYPTOPP_NO_VTABLE Base : public BlockCipherImpl<RC2_Info>
	{
	public:
		void UncheckedSetKey(const byte *userKey, unsigned int length, const NameValuePairs &params);
		unsigned int OptimalDataAlignment() const {return GetAlignmentOf<word16>();}

	protected:
		FixedSizeSecBlock<word16, 64> K;  // expanded key table
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
	class Encryption : public BlockCipherFinal<ENCRYPTION, Enc>
	{
	public:
		Encryption() {}
		Encryption(const byte *key, size_t keyLen=DEFAULT_KEYLENGTH)
			{SetKey(key, keyLen);}
		Encryption(const byte *key, size_t keyLen, int effectiveKeyLen)
			{SetKey(key, keyLen, MakeParameters("EffectiveKeyLength", effectiveKeyLen));}
	};

	class Decryption : public BlockCipherFinal<DECRYPTION, Dec>
	{
	public:
		Decryption() {}
		Decryption(const byte *key, size_t keyLen=DEFAULT_KEYLENGTH)
			{SetKey(key, keyLen);}
		Decryption(const byte *key, size_t keyLen, int effectiveKeyLen)
			{SetKey(key, keyLen, MakeParameters("EffectiveKeyLength", effectiveKeyLen));}
	};
};

typedef RC2::Encryption RC2Encryption;
typedef RC2::Decryption RC2Decryption;

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class RC2:
    # // rc2.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # void RC2::Base::UncheckedSetKey(const byte *key, unsigned int keyLen, const NameValuePairs &params)
    # 	AssertValidKeyLength(keyLen);
    # 	if (effectiveLen > MAX_EFFECTIVE_KEYLENGTH)
    # 		throw InvalidArgument("RC2: effective key length parameter exceeds maximum");
    # 	static const unsigned char PITABLE[256] = {
    # 		217,120,249,196, 25,221,181,237, 40,233,253,121, 74,160,216,157,
    # 		198,126, 55,131, 43,118, 83,142, 98, 76,100,136, 68,139,251,162,
    # 		 23,154, 89,245,135,179, 79, 19, 97, 69,109,141,  9,129,125, 50,
    # 		189,143, 64,235,134,183,123, 11,240,149, 33, 34, 92,107, 78,130,
    # 		 84,214,101,147,206, 96,178, 28,115, 86,192, 20,167,140,241,220,
    # 		 18,117,202, 31, 59,190,228,209, 66, 61,212, 48,163, 60,182, 38,
    # 		111,191, 14,218, 70,105,  7, 87, 39,242, 29,155,188,148, 67,  3,
    # 		248, 17,199,246,144,239, 62,231,  6,195,213, 47,200,102, 30,215,
    # 		  8,232,234,222,128, 82,238,247,132,170,114,172, 53, 77,106, 42,
    # 		150, 26,210,113, 90, 21, 73,116, 75,159,208, 94,  4, 24,164,236,
    # 		194,224, 65,110, 15, 81,203,204, 36,145,175, 80,161,244,112, 57,
    # 		153,124, 58,133, 35,184,180,122,252,  2, 54, 91, 37, 85,151, 49,
    # 		 45, 93,250,152,227,138,146,174,  5,223, 41, 16,103,108,186,201,
    # 		211,  0,230,207,225,158,168, 44, 99, 22,  1, 63, 88,226,137,169,
    # 		 13, 56, 52, 27,171, 51,255,176,187, 72, 12, 95,185,177,205, 46,
    # 		197,243,219, 71,229,165,156,119, 10,166, 32,104,254,127,193,173};
    # 	SecByteBlock L(128);
    # 	memcpy(L, key, keyLen);
    # 	int i;
    # 	for (i=keyLen; i<128; i++)
    # 		L[i] = PITABLE[(L[i-1] + L[i-keyLen]) & 255];
    # 	unsigned int T8 = (effectiveLen+7) / 8;
    # 	byte TM = 255 >> ((8-(effectiveLen%8))%8);
    # 	L[128-T8] = PITABLE[L[128-T8] & TM];
    # 	for (i=127-T8; i>=0; i--)
    # 		L[i] = PITABLE[L[i+1] ^ L[i+T8]];
    # 	for (i=0; i<64; i++)
    # 		K[i] = L[2*i] + (L[2*i+1] << 8);
    # typedef BlockGetAndPut<word16, LittleEndian> Block;
    # void RC2::Enc::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	word16 R0, R1, R2, R3;
    # 	for (int i = 0; i < 16; i++)
    # 		R0 += (R1 & ~R3) + (R2 & R3) + K[4*i+0];
    # 		R0 = rotlFixed(R0, 1);
    # 		R1 += (R2 & ~R0) + (R3 & R0) + K[4*i+1];
    # 		R1 = rotlFixed(R1, 2);
    # 		R2 += (R3 & ~R1) + (R0 & R1) + K[4*i+2];
    # 		R2 = rotlFixed(R2, 3);
    # 		R3 += (R0 & ~R2) + (R1 & R2) + K[4*i+3];
    # 		R3 = rotlFixed(R3, 5);
    # 		if (i == 4 || i == 10)
    # 			R0 += K[R3 & 63];
    # 			R1 += K[R0 & 63];
    # 			R2 += K[R1 & 63];
    # 			R3 += K[R2 & 63];
    # void RC2::Dec::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	word16 R0, R1, R2, R3;
    # 	for (int i = 15; i >= 0; i--)
    # 		if (i == 4 || i == 10)
    # 			R3 -= K[R2 & 63];
    # 			R2 -= K[R1 & 63];
    # 			R1 -= K[R0 & 63];
    # 			R0 -= K[R3 & 63];
    # 		R3 = rotrFixed(R3, 5);
    # 		R3 -= (R0 & ~R2) + (R1 & R2) + K[4*i+3];
    # 		R2 = rotrFixed(R2, 3);
    # 		R2 -= (R3 & ~R1) + (R0 & R1) + K[4*i+2];
    # 		R1 = rotrFixed(R1, 2);
    # 		R1 -= (R2 & ~R0) + (R3 & R0) + K[4*i+1];
    # 		R0 = rotrFixed(R0, 1);
    # 		R0 -= (R1 & ~R3) + (R2 & R3) + K[4*i+0];
    # NAMESPACE_END