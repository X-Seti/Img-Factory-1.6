"""
gost.py - Python conversion of gost.h and gost.cpp

Original C++ header content:
#ifndef CRYPTOPP_GOST_H
#define CRYPTOPP_GOST_H

/** \file
*/

#include "seckey.h"
#include "secblock.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
struct GOST_Info : public FixedBlockSize<8>, public FixedKeyLength<32>
{
	static const char *StaticAlgorithmName() {return "GOST";}
};

/// <a href="http://www.weidai.com/scan-mirror/cs.html#GOST">GOST</a>
class GOST : public GOST_Info, public BlockCipherDocumentation
{
	class CRYPTOPP_NO_VTABLE Base : public BlockCipherImpl<GOST_Info>
	{
	public:
		void UncheckedSetKey(const byte *userKey, unsigned int length, const NameValuePairs &params);

	protected:
		static void PrecalculateSTable();

		static const byte sBox[8][16];
		static volatile bool sTableCalculated;
		static word32 sTable[4][256];

		FixedSizeSecBlock<word32, 8> key;
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

typedef GOST::Encryption GOSTEncryption;
typedef GOST::Decryption GOSTDecryption;

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class GOST:
    # NAMESPACE_BEGIN(CryptoPP)
    # // these are the S-boxes given in Applied Cryptography 2nd Ed., p. 333
    # /*	// these are the S-boxes given in the GOST source code listing in Applied
    # 	// Cryptography 2nd Ed., p. 644.  they appear to be from the DES S-boxes
    # */
    # void GOST::Base::UncheckedSetKey(const byte *userKey, unsigned int length, const NameValuePairs &)
    # 	AssertValidKeyLength(length);
    # 	PrecalculateSTable();
    # 	GetUserKey(LITTLE_ENDIAN_ORDER, key.begin(), 8, userKey, KEYLENGTH);
    # void GOST::Base::PrecalculateSTable()
    # 	if (!sTableCalculated)
    # 		for (unsigned i = 0; i < 4; i++)
    # 			for (unsigned j = 0; j < 256; j++) 
    # 				word32 temp = sBox[2*i][j%16] | (sBox[2*i+1][j/16] << 4);
    # 				sTable[i][j] = rotlMod(temp, 11+8*i);
    # 		sTableCalculated=true;
    # #define f(x)  ( t=x,												\
    # 				sTable[3][GETBYTE(t, 3)] ^ sTable[2][GETBYTE(t, 2)]	\
    # 			  ^ sTable[1][GETBYTE(t, 1)] ^ sTable[0][GETBYTE(t, 0)]	)
    # typedef BlockGetAndPut<word32, LittleEndian> Block;
    # void GOST::Enc::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	word32 n1, n2, t;
    # 	for (unsigned int i=0; i<3; i++)
    # 		n2 ^= f(n1+key[0]);
    # 		n1 ^= f(n2+key[1]);
    # 		n2 ^= f(n1+key[2]);
    # 		n1 ^= f(n2+key[3]);
    # 		n2 ^= f(n1+key[4]);
    # 		n1 ^= f(n2+key[5]);
    # 		n2 ^= f(n1+key[6]);
    # 		n1 ^= f(n2+key[7]);
    # 	n2 ^= f(n1+key[7]);
    # 	n1 ^= f(n2+key[6]);
    # 	n2 ^= f(n1+key[5]);
    # 	n1 ^= f(n2+key[4]);
    # 	n2 ^= f(n1+key[3]);
    # 	n1 ^= f(n2+key[2]);
    # 	n2 ^= f(n1+key[1]);
    # 	n1 ^= f(n2+key[0]);
    # void GOST::Dec::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	word32 n1, n2, t;
    # 	n2 ^= f(n1+key[0]);
    # 	n1 ^= f(n2+key[1]);
    # 	n2 ^= f(n1+key[2]);
    # 	n1 ^= f(n2+key[3]);
    # 	n2 ^= f(n1+key[4]);
    # 	n1 ^= f(n2+key[5]);
    # 	n2 ^= f(n1+key[6]);
    # 	n1 ^= f(n2+key[7]);
    # 	for (unsigned int i=0; i<3; i++)
    # 		n2 ^= f(n1+key[7]);
    # 		n1 ^= f(n2+key[6]);
    # 		n2 ^= f(n1+key[5]);
    # 		n1 ^= f(n2+key[4]);
    # 		n2 ^= f(n1+key[3]);
    # 		n1 ^= f(n2+key[2]);
    # 		n2 ^= f(n1+key[1]);
    # 		n1 ^= f(n2+key[0]);
    # NAMESPACE_END