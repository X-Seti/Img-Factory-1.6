"""
xtr.py - Python conversion of xtr.h and xtr.cpp

Original C++ header content:
#ifndef CRYPTOPP_XTR_H
#define CRYPTOPP_XTR_H

/** \file
	"The XTR public key system" by Arjen K. Lenstra and Eric R. Verheul
*/

#include "modarith.h"

NAMESPACE_BEGIN(CryptoPP)

//! an element of GF(p^2)
class GFP2Element
{
public:
	GFP2Element() {}
	GFP2Element(const Integer &c1, const Integer &c2) : c1(c1), c2(c2) {}
	GFP2Element(const byte *encodedElement, unsigned int size)
		: c1(encodedElement, size/2), c2(encodedElement+size/2, size/2) {}

	void Encode(byte *encodedElement, unsigned int size)
	{
		c1.Encode(encodedElement, size/2);
		c2.Encode(encodedElement+size/2, size/2);
	}

	bool operator==(const GFP2Element &rhs)	const {return c1 == rhs.c1 && c2 == rhs.c2;}
	bool operator!=(const GFP2Element &rhs) const {return !operator==(rhs);}

	void swap(GFP2Element &a)
	{
		c1.swap(a.c1);
		c2.swap(a.c2);
	}

	static const GFP2Element & Zero();

	Integer c1, c2;
};

//! GF(p^2), optimal normal basis
template <class F>
class GFP2_ONB : public AbstractRing<GFP2Element>
{
public:
	typedef F BaseField;

	GFP2_ONB(const Integer &p) : modp(p)
	{
		if (p%3 != 2)
			throw InvalidArgument("GFP2_ONB: modulus must be equivalent to 2 mod 3");
	}

	const Integer& GetModulus() const {return modp.GetModulus();}

	GFP2Element ConvertIn(const Integer &a) const
	{
		t = modp.Inverse(modp.ConvertIn(a));
		return GFP2Element(t, t);
	}

	GFP2Element ConvertIn(const GFP2Element &a) const
		{return GFP2Element(modp.ConvertIn(a.c1), modp.ConvertIn(a.c2));}

	GFP2Element ConvertOut(const GFP2Element &a) const
		{return GFP2Element(modp.ConvertOut(a.c1), modp.ConvertOut(a.c2));}

	bool Equal(const GFP2Element &a, const GFP2Element &b) const
	{
		return modp.Equal(a.c1, b.c1) && modp.Equal(a.c2, b.c2);
	}

	const Element& Identity() const
	{
		return GFP2Element::Zero();
	}

	const Element& Add(const Element &a, const Element &b) const
	{
		result.c1 = modp.Add(a.c1, b.c1);
		result.c2 = modp.Add(a.c2, b.c2);
		return result;
	}

	const Element& Inverse(const Element &a) const
	{
		result.c1 = modp.Inverse(a.c1);
		result.c2 = modp.Inverse(a.c2);
		return result;
	}

	const Element& Double(const Element &a) const
	{
		result.c1 = modp.Double(a.c1);
		result.c2 = modp.Double(a.c2);
		return result;
	}

	const Element& Subtract(const Element &a, const Element &b) const
	{
		result.c1 = modp.Subtract(a.c1, b.c1);
		result.c2 = modp.Subtract(a.c2, b.c2);
		return result;
	}

	Element& Accumulate(Element &a, const Element &b) const
	{
		modp.Accumulate(a.c1, b.c1);
		modp.Accumulate(a.c2, b.c2);
		return a;
	}

	Element& Reduce(Element &a, const Element &b) const
	{
		modp.Reduce(a.c1, b.c1);
		modp.Reduce(a.c2, b.c2);
		return a;
	}

	bool IsUnit(const Element &a) const
	{
		return a.c1.NotZero() || a.c2.NotZero();
	}

	const Element& MultiplicativeIdentity() const
	{
		result.c1 = result.c2 = modp.Inverse(modp.MultiplicativeIdentity());
		return result;
	}

	const Element& Multiply(const Element &a, const Element &b) const
	{
		t = modp.Add(a.c1, a.c2);
		t = modp.Multiply(t, modp.Add(b.c1, b.c2));
		result.c1 = modp.Multiply(a.c1, b.c1);
		result.c2 = modp.Multiply(a.c2, b.c2);
		result.c1.swap(result.c2);
		modp.Reduce(t, result.c1);
		modp.Reduce(t, result.c2);
		modp.Reduce(result.c1, t);
		modp.Reduce(result.c2, t);
		return result;
	}

	const Element& MultiplicativeInverse(const Element &a) const
	{
		return result = Exponentiate(a, modp.GetModulus()-2);
	}

	const Element& Square(const Element &a) const
	{
		const Integer &ac1 = (&a == &result) ? (t = a.c1) : a.c1;
		result.c1 = modp.Multiply(modp.Subtract(modp.Subtract(a.c2, a.c1), a.c1), a.c2);
		result.c2 = modp.Multiply(modp.Subtract(modp.Subtract(ac1, a.c2), a.c2), ac1);
		return result;
	}

	Element Exponentiate(const Element &a, const Integer &e) const
	{
		Integer edivp, emodp;
		Integer::Divide(emodp, edivp, e, modp.GetModulus());
		Element b = PthPower(a);
		return AbstractRing<GFP2Element>::CascadeExponentiate(a, emodp, b, edivp);
	}

	const Element & PthPower(const Element &a) const
	{
		result = a;
		result.c1.swap(result.c2);
		return result;
	}

	void RaiseToPthPower(Element &a) const
	{
		a.c1.swap(a.c2);
	}

	// a^2 - 2a^p
	const Element & SpecialOperation1(const Element &a) const
	{
		assert(&a != &result);
		result = Square(a);
		modp.Reduce(result.c1, a.c2);
		modp.Reduce(result.c1, a.c2);
		modp.Reduce(result.c2, a.c1);
		modp.Reduce(result.c2, a.c1);
		return result;
	}

	// x * z - y * z^p
	const Element & SpecialOperation2(const Element &x, const Element &y, const Element &z) const
	{
		assert(&x != &result && &y != &result && &z != &result);
		t = modp.Add(x.c2, y.c2);
		result.c1 = modp.Multiply(z.c1, modp.Subtract(y.c1, t));
		modp.Accumulate(result.c1, modp.Multiply(z.c2, modp.Subtract(t, x.c1)));
		t = modp.Add(x.c1, y.c1);
		result.c2 = modp.Multiply(z.c2, modp.Subtract(y.c2, t));
		modp.Accumulate(result.c2, modp.Multiply(z.c1, modp.Subtract(t, x.c2)));
		return result;
	}

protected:
	BaseField modp;
	mutable GFP2Element result;
	mutable Integer t;
};

void XTR_FindPrimesAndGenerator(RandomNumberGenerator &rng, Integer &p, Integer &q, GFP2Element &g, unsigned int pbits, unsigned int qbits);

GFP2Element XTR_Exponentiate(const GFP2Element &b, const Integer &e, const Integer &p);

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class GFP2Element:
    # // cryptlib.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # const GFP2Element & GFP2Element::Zero()
    # 	return Singleton<GFP2Element>().Ref();
    # void XTR_FindPrimesAndGenerator(RandomNumberGenerator &rng, Integer &p, Integer &q, GFP2Element &g, unsigned int pbits, unsigned int qbits)
    # 	assert(qbits > 9);	// no primes exist for pbits = 10, qbits = 9
    # 	assert(pbits > qbits);
    # 	Integer r1, r2;
    # 	do
    # 		assert(qFound);
    # 		bool solutionsExist = SolveModularQuadraticEquation(r1, r2, 1, -1, 1, q);
    # 		assert(solutionsExist);
    # 	assert(((p.Squared() - p + 1) % q).IsZero());
    # 	GFP2_ONB<ModularArithmetic> gfp2(p);
    # 	GFP2Element three = gfp2.ConvertIn(3), t;
    # 	while (true)
    # 		t = XTR_Exponentiate(g, p+1, p);
    # 		if (t.c1 == t.c2)
    # 			continue;
    # 		g = XTR_Exponentiate(g, (p.Squared()-p+1)/q, p);
    # 		if (g != three)
    # 			break;
    # 	assert(XTR_Exponentiate(g, q, p) == three);
    # GFP2Element XTR_Exponentiate(const GFP2Element &b, const Integer &e, const Integer &p)
    # 	unsigned int bitCount = e.BitCount();
    # 	if (bitCount == 0)
    # 		return GFP2Element(-3, -3);
    # 	// find the lowest bit of e that is 1
    # 	unsigned int lowest1bit;
    # 	for (lowest1bit=0; e.GetBit(lowest1bit) == 0; lowest1bit++) {}
    # 	GFP2_ONB<MontgomeryRepresentation> gfp2(p);
    # 	GFP2Element c = gfp2.ConvertIn(b);
    # 	GFP2Element cp = gfp2.PthPower(c);
    # 	GFP2Element S[5] = {gfp2.ConvertIn(3), c, gfp2.SpecialOperation1(c)};
    # 	// do all exponents bits except the lowest zeros starting from the top
    # 	unsigned int i;
    # 	for (i = e.BitCount() - 1; i>lowest1bit; i--)
    # 		if (e.GetBit(i))
    # 			gfp2.RaiseToPthPower(S[0]);
    # 			gfp2.Accumulate(S[0], gfp2.SpecialOperation2(S[2], c, S[1]));
    # 			S[1] = gfp2.SpecialOperation1(S[1]);
    # 			S[2] = gfp2.SpecialOperation1(S[2]);
    # 			S[0].swap(S[1]);
    # 		else
    # 			gfp2.RaiseToPthPower(S[2]);
    # 			gfp2.Accumulate(S[2], gfp2.SpecialOperation2(S[0], cp, S[1]));
    # 			S[1] = gfp2.SpecialOperation1(S[1]);
    # 			S[0] = gfp2.SpecialOperation1(S[0]);
    # 			S[2].swap(S[1]);
    # 	// now do the lowest zeros
    # 	while (i--)
    # 		S[1] = gfp2.SpecialOperation1(S[1]);
    # 	return gfp2.ConvertOut(S[1]);
    # template class AbstractRing<GFP2Element>;
    # template class AbstractGroup<GFP2Element>;
    # NAMESPACE_END