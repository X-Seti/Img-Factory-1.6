"""
gf2_32.py - Python conversion of gf2_32.h and gf2_32.cpp

Original C++ header content:
#ifndef CRYPTOPP_GF2_32_H
#define CRYPTOPP_GF2_32_H

#include "cryptlib.h"

NAMESPACE_BEGIN(CryptoPP)

//! GF(2^32) with polynomial basis
class GF2_32
{
public:
	typedef word32 Element;
	typedef int RandomizationParameter;

	GF2_32(word32 modulus=0x0000008D) : m_modulus(modulus) {}

	Element RandomElement(RandomNumberGenerator &rng, int ignored = 0) const
		{return rng.GenerateWord32();}

	bool Equal(Element a, Element b) const
		{return a==b;}

	Element Identity() const
		{return 0;}

	Element Add(Element a, Element b) const
		{return a^b;}

	Element& Accumulate(Element &a, Element b) const
		{return a^=b;}

	Element Inverse(Element a) const
		{return a;}

	Element Subtract(Element a, Element b) const
		{return a^b;}

	Element& Reduce(Element &a, Element b) const
		{return a^=b;}

	Element Double(Element a) const
		{return 0;}

	Element MultiplicativeIdentity() const
		{return 1;}

	Element Multiply(Element a, Element b) const;

	Element Square(Element a) const
		{return Multiply(a, a);}

	bool IsUnit(Element a) const
		{return a != 0;}

	Element MultiplicativeInverse(Element a) const;

	Element Divide(Element a, Element b) const
		{return Multiply(a, MultiplicativeInverse(b));}

private:
	word32 m_modulus;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class GF2_32:
    # // gf2_32.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # GF2_32::Element GF2_32::Multiply(Element a, Element b) const
    # 	word32 table[4];
    # 	table[0] = 0;
    # 	table[1] = m_modulus;
    # 	if (a & 0x80000000)
    # 		table[2] = m_modulus ^ (a<<1);
    # 		table[3] = a<<1;
    # 	else
    # 		table[2] = a<<1;
    # 		table[3] = m_modulus ^ (a<<1);
    # #if CRYPTOPP_FAST_ROTATE(32)
    # 	b = rotrFixed(b, 30U);
    # 	word32 result = table[b&2];
    # 	for (int i=29; i>=0; --i)
    # 		b = rotlFixed(b, 1U);
    # 		result = (result<<1) ^ table[(b&2) + (result>>31)];
    # 	return (b&1) ? result ^ a : result;
    # #else
    # 	word32 result = table[(b>>30) & 2];
    # 	for (int i=29; i>=0; --i)
    # 		result = (result<<1) ^ table[((b>>i)&2) + (result>>31)];
    # 	return (b&1) ? result ^ a : result;
    # #endif
    # GF2_32::Element GF2_32::MultiplicativeInverse(Element a) const
    # 	if (a <= 1)		// 1 is a special case
    # 		return a;
    # 	// warning - don't try to adapt this algorithm for another situation
    # 	word32 g0=m_modulus, g1=a, g2=a;
    # 	word32 v0=0, v1=1, v2=1;
    # 	assert(g1);
    # 	while (!(g2 & 0x80000000))
    # 		g2 <<= 1;
    # 		v2 <<= 1;
    # 	g2 <<= 1;
    # 	v2 <<= 1;
    # 	g0 ^= g2;
    # 	v0 ^= v2;
    # 	while (g0 != 1)
    # 		if (g1 < g0 || ((g0^g1) < g0 && (g0^g1) < g1))
    # 			assert(BitPrecision(g1) <= BitPrecision(g0));
    # 			g2 = g1;
    # 			v2 = v1;
    # 		else
    # 			assert(BitPrecision(g1) > BitPrecision(g0));
    # 			g2 = g0; g0 = g1; g1 = g2;
    # 			v2 = v0; v0 = v1; v1 = v2;
    # 		while ((g0^g2) >= g2)
    # 			assert(BitPrecision(g0) > BitPrecision(g2));
    # 			g2 <<= 1;
    # 			v2 <<= 1;
    # 		assert(BitPrecision(g0) == BitPrecision(g2));
    # 		g0 ^= g2;
    # 		v0 ^= v2;
    # 	return v0;
    # NAMESPACE_END