"""
blowfish.py - Python conversion of blowfish.h and blowfish.cpp

Original C++ header content:
#ifndef CRYPTOPP_BLOWFISH_H
#define CRYPTOPP_BLOWFISH_H

/** \file */

#include "seckey.h"
#include "secblock.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
struct Blowfish_Info : public FixedBlockSize<8>, public VariableKeyLength<16, 4, 56>, public FixedRounds<16>
{
	static const char *StaticAlgorithmName() {return "Blowfish";}
};

//! <a href="http://www.weidai.com/scan-mirror/cs.html#Blowfish">Blowfish</a>
class Blowfish : public Blowfish_Info, public BlockCipherDocumentation
{
	class CRYPTOPP_NO_VTABLE Base : public BlockCipherImpl<Blowfish_Info>
	{
	public:
		void ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const;
		void UncheckedSetKey(const byte *key_string, unsigned int keylength, const NameValuePairs &params);

	private:
		void crypt_block(const word32 in[2], word32 out[2]) const;

		static const word32 p_init[ROUNDS+2];
		static const word32 s_init[4*256];

		FixedSizeSecBlock<word32, ROUNDS+2> pbox;
		FixedSizeSecBlock<word32, 4*256> sbox;
	};

public:
	typedef BlockCipherFinal<ENCRYPTION, Base> Encryption;
	typedef BlockCipherFinal<DECRYPTION, Base> Decryption;
};

typedef Blowfish::Encryption BlowfishEncryption;
typedef Blowfish::Decryption BlowfishDecryption;

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class Blowfish:
    # // blowfish.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # void Blowfish::Base::UncheckedSetKey(const byte *key_string, unsigned int keylength, const NameValuePairs &)
    # 	AssertValidKeyLength(keylength);
    # 	unsigned i, j=0, k;
    # 	word32 data, dspace[2] = {0, 0};
    # 	memcpy(pbox, p_init, sizeof(p_init));
    # 	memcpy(sbox, s_init, sizeof(s_init));
    # 	// Xor key string into encryption key vector
    # 	for (i=0 ; i<ROUNDS+2 ; ++i)
    # 		data = 0 ;
    # 		for (k=0 ; k<4 ; ++k )
    # 			data = (data << 8) | key_string[j++ % keylength];
    # 		pbox[i] ^= data;
    # 	crypt_block(dspace, pbox);
    # 	for (i=0; i<ROUNDS; i+=2)
    # 		crypt_block(pbox+i, pbox+i+2);
    # 	crypt_block(pbox+ROUNDS, sbox);
    # 	for (i=0; i<4*256-2; i+=2)
    # 		crypt_block(sbox+i, sbox+i+2);
    # 	if (!IsForwardTransformation())
    # 		for (i=0; i<(ROUNDS+2)/2; i++)
    # // this version is only used to make pbox and sbox
    # void Blowfish::Base::crypt_block(const word32 in[2], word32 out[2]) const
    # 	word32 left = in[0];
    # 	word32 right = in[1];
    # 	const word32 *const s=sbox;
    # 	const word32 *p=pbox;
    # 	left ^= p[0];
    # 	for (unsigned i=0; i<ROUNDS/2; i++)
    # 		right ^= (((s[GETBYTE(left,3)] + s[256+GETBYTE(left,2)])
    # 			  ^ s[2*256+GETBYTE(left,1)]) + s[3*256+GETBYTE(left,0)])
    # 			  ^ p[2*i+1];
    # 		left ^= (((s[GETBYTE(right,3)] + s[256+GETBYTE(right,2)])
    # 			 ^ s[2*256+GETBYTE(right,1)]) + s[3*256+GETBYTE(right,0)])
    # 			 ^ p[2*i+2];
    # 	right ^= p[ROUNDS+1];
    # 	out[0] = right;
    # 	out[1] = left;
    # void Blowfish::Base::ProcessAndXorBlock(const byte *inBlock, const byte *xorBlock, byte *outBlock) const
    # 	typedef BlockGetAndPut<word32, BigEndian> Block;
    # 	word32 left, right;
    # 	const word32 *const s=sbox;
    # 	const word32 *p=pbox;
    # 	left ^= p[0];
    # 	for (unsigned i=0; i<ROUNDS/2; i++)
    # 		right ^= (((s[GETBYTE(left,3)] + s[256+GETBYTE(left,2)])
    # 			  ^ s[2*256+GETBYTE(left,1)]) + s[3*256+GETBYTE(left,0)])
    # 			  ^ p[2*i+1];
    # 		left ^= (((s[GETBYTE(right,3)] + s[256+GETBYTE(right,2)])
    # 			 ^ s[2*256+GETBYTE(right,1)]) + s[3*256+GETBYTE(right,0)])
    # 			 ^ p[2*i+2];
    # 	right ^= p[ROUNDS+1];
    # NAMESPACE_END