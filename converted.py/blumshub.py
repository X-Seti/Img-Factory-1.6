"""
blumshub.py - Python conversion of blumshub.h and blumshub.cpp

Original C++ header content:
#ifndef CRYPTOPP_BLUMSHUB_H
#define CRYPTOPP_BLUMSHUB_H

#include "modarith.h"

NAMESPACE_BEGIN(CryptoPP)

class BlumGoldwasserPublicKey;
class BlumGoldwasserPrivateKey;

//! BlumBlumShub without factorization of the modulus
class PublicBlumBlumShub : public RandomNumberGenerator,
						   public StreamTransformation
{
public:
	PublicBlumBlumShub(const Integer &n, const Integer &seed);

	unsigned int GenerateBit();
	byte GenerateByte();
	void GenerateBlock(byte *output, size_t size);
	void ProcessData(byte *outString, const byte *inString, size_t length);

	bool IsSelfInverting() const {return true;}
	bool IsForwardTransformation() const {return true;}

protected:
	ModularArithmetic modn;
	word maxBits, bitsLeft;
	Integer current;

	friend class BlumGoldwasserPublicKey;
	friend class BlumGoldwasserPrivateKey;
};

//! BlumBlumShub with factorization of the modulus
class BlumBlumShub : public PublicBlumBlumShub
{
public:
	// Make sure p and q are both primes congruent to 3 mod 4 and at least 512 bits long,
	// seed is the secret key and should be about as big as p*q
	BlumBlumShub(const Integer &p, const Integer &q, const Integer &seed);
	
	bool IsRandomAccess() const {return true;}
	void Seek(lword index);

protected:
	const Integer p, q;
	const Integer x0;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class PublicBlumBlumShub:
    # // blumshub.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # PublicBlumBlumShub::PublicBlumBlumShub(const Integer &n, const Integer &seed)
    # 	: modn(n),
    # 	  maxBits(BitPrecision(n.BitCount())-1)
    # 	current = modn.Square(modn.Square(seed));
    # 	bitsLeft = maxBits;
    # unsigned int PublicBlumBlumShub::GenerateBit()
    # 	if (bitsLeft==0)
    # 		current = modn.Square(current);
    # 		bitsLeft = maxBits;
    # 	return current.GetBit(--bitsLeft);
    # byte PublicBlumBlumShub::GenerateByte()
    # 	byte b=0;
    # 	for (int i=0; i<8; i++)
    # 	return b;
    # void PublicBlumBlumShub::GenerateBlock(byte *output, size_t size)
    # 	while (size--)
    # void PublicBlumBlumShub::ProcessData(byte *outString, const byte *inString, size_t length)
    # 	while (length--)
    # BlumBlumShub::BlumBlumShub(const Integer &p, const Integer &q, const Integer &seed)
    # 	: PublicBlumBlumShub(p*q, seed),
    # 	  p(p), q(q),
    # 	  x0(modn.Square(seed))
    # void BlumBlumShub::Seek(lword index)
    # 	i *= 8;
    # 	Integer e = a_exp_b_mod_c (2, i / maxBits + 1, (p-1)*(q-1));
    # 	current = modn.Exponentiate(x0, e);
    # 	bitsLeft = maxBits - i % maxBits;
    # NAMESPACE_END