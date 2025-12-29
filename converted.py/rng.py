"""
rng.py - Python conversion of rng.h and rng.cpp

Original C++ header content:
// rng.h - misc RNG related classes, see also osrng.h, randpool.h

#ifndef CRYPTOPP_RNG_H
#define CRYPTOPP_RNG_H

#include "cryptlib.h"
#include "filters.h"

NAMESPACE_BEGIN(CryptoPP)

//! linear congruential generator
/*! originally by William S. England, do not use for cryptographic purposes */
class LC_RNG : public RandomNumberGenerator
{
public:
	LC_RNG(word32 init_seed)
		: seed(init_seed) {}

	void GenerateBlock(byte *output, size_t size);

	word32 GetSeed() {return seed;}

private:
	word32 seed;

	static const word32 m;
	static const word32 q;
	static const word16 a;
	static const word16 r;
};

//! RNG derived from ANSI X9.17 Appendix C

class CRYPTOPP_DLL X917RNG : public RandomNumberGenerator, public NotCopyable
{
public:
	// cipher will be deleted by destructor, deterministicTimeVector = 0 means obtain time vector from system
	X917RNG(BlockTransformation *cipher, const byte *seed, const byte *deterministicTimeVector = 0);

	void GenerateIntoBufferedTransformation(BufferedTransformation &target, const std::string &channel, lword size);

private:
	member_ptr<BlockTransformation> cipher;
	unsigned int S;			// blocksize of cipher
	SecByteBlock dtbuf; 	// buffer for enciphered timestamp
	SecByteBlock randseed, m_lastBlock, m_deterministicTimeVector;
};

/** This class implements Maurer's Universal Statistical Test for Random Bit Generators
    it is intended for measuring the randomness of *PHYSICAL* RNGs.
    For more details see his paper in Journal of Cryptology, 1992. */

class MaurerRandomnessTest : public Bufferless<Sink>
{
public:
	MaurerRandomnessTest();

	size_t Put2(const byte *inString, size_t length, int messageEnd, bool blocking);

	// BytesNeeded() returns how many more bytes of input is needed by the test
	// GetTestValue() should not be called before BytesNeeded()==0
	unsigned int BytesNeeded() const {return n >= (Q+K) ? 0 : Q+K-n;}

	// returns a number between 0.0 and 1.0, describing the quality of the
	// random numbers entered
	double GetTestValue() const;

private:
	enum {L=8, V=256, Q=2000, K=2000};
	double sum;
	unsigned int n;
	unsigned int tab[V];
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class LC_RNG:
    # // rng.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # // linear congruential generator
    # // originally by William S. England
    # // do not use for cryptographic purposes
    # /*
    # ** Original_numbers are the original published m and q in the
    # ** ACM article above.  John Burton has furnished numbers for
    # ** a reportedly better generator.  The new numbers are now
    # ** used in this program by default.
    # */
    # #ifndef LCRNG_ORIGINAL_NUMBERS
    # #else
    # #endif
    # void LC_RNG::GenerateBlock(byte *output, size_t size)
    # 	while (size--)
    # 		word32 hi = seed/q;
    # 		word32 lo = seed%q;
    # 		long test = a*lo - r*hi;
    # 		if (test > 0)
    # 			seed = test;
    # 		else
    # 			seed = test+ m;
    # 		*output++ = (GETBYTE(seed, 0) ^ GETBYTE(seed, 1) ^ GETBYTE(seed, 2) ^ GETBYTE(seed, 3));
    # // ********************************************************
    # #ifndef CRYPTOPP_IMPORTS
    # X917RNG::X917RNG(BlockTransformation *c, const byte *seed, const byte *deterministicTimeVector)
    # 	: cipher(c),
    # 	  S(cipher->BlockSize()),
    # 	  dtbuf(S),
    # 	  randseed(seed, S),
    # 	  m_lastBlock(S),
    # 	  m_deterministicTimeVector(deterministicTimeVector, deterministicTimeVector ? S : 0)
    # 	if (!deterministicTimeVector)
    # 		time_t tstamp1 = time(0);
    # 		xorbuf(dtbuf, (byte *)&tstamp1, UnsignedMin(sizeof(tstamp1), S));
    # 		cipher->ProcessBlock(dtbuf);
    # 		clock_t tstamp2 = clock();
    # 		xorbuf(dtbuf, (byte *)&tstamp2, UnsignedMin(sizeof(tstamp2), S));
    # 		cipher->ProcessBlock(dtbuf);
    # 	// for FIPS 140-2
    # 	GenerateBlock(m_lastBlock, S);
    # void X917RNG::GenerateIntoBufferedTransformation(BufferedTransformation &target, const std::string &channel, lword size)
    # 	while (size > 0)
    # 		// calculate new enciphered timestamp
    # 		if (m_deterministicTimeVector.size())
    # 			cipher->ProcessBlock(m_deterministicTimeVector, dtbuf);
    # 			IncrementCounterByOne(m_deterministicTimeVector, S);
    # 		else
    # 			clock_t c = clock();
    # 			xorbuf(dtbuf, (byte *)&c, UnsignedMin(sizeof(c), S));
    # 			time_t t = time(NULL);
    # 			xorbuf(dtbuf+S-UnsignedMin(sizeof(t), S), (byte *)&t, UnsignedMin(sizeof(t), S));
    # 			cipher->ProcessBlock(dtbuf);
    # 		// combine enciphered timestamp with seed
    # 		xorbuf(randseed, dtbuf, S);
    # 		// generate a new block of random bytes
    # 		cipher->ProcessBlock(randseed);
    # 		if (memcmp(m_lastBlock, randseed, S) == 0)
    # 			throw SelfTestFailure("X917RNG: Continuous random number generator test failed.");
    # 		// output random bytes
    # 		size_t len = UnsignedMin(S, size);
    # 		target.ChannelPut(channel, randseed, len);
    # 		size -= len;
    # 		// compute new seed vector
    # 		memcpy(m_lastBlock, randseed, S);
    # 		xorbuf(randseed, dtbuf, S);
    # 		cipher->ProcessBlock(randseed);
    # #endif
    # MaurerRandomnessTest::MaurerRandomnessTest()
    # 	: sum(0.0), n(0)
    # 	for (unsigned i=0; i<V; i++)
    # 		tab[i] = 0;
    # size_t MaurerRandomnessTest::Put2(const byte *inString, size_t length, int messageEnd, bool blocking)
    # 	while (length--)
    # 		byte inByte = *inString++;
    # 		if (n >= Q)
    # 			sum += log(double(n - tab[inByte]));
    # 		tab[inByte] = n;
    # 		n++;
    # 	return 0;
    # double MaurerRandomnessTest::GetTestValue() const
    # 	if (BytesNeeded() > 0)
    # 	double fTu = (sum/(n-Q))/log(2.0);	// this is the test value defined by Maurer
    # 	double value = fTu * 0.1392;		// arbitrarily normalize it to
    # 	return value > 1.0 ? 1.0 : value;	// a number between 0 and 1
    # NAMESPACE_END