"""
rabin.py - Python conversion of rabin.h and rabin.cpp

Original C++ header content:
#ifndef CRYPTOPP_RABIN_H
#define CRYPTOPP_RABIN_H

/** \file
*/

#include "oaep.h"
#include "pssr.h"
#include "integer.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
class RabinFunction : public TrapdoorFunction, public PublicKey
{
	typedef RabinFunction ThisClass;

public:
	void Initialize(const Integer &n, const Integer &r, const Integer &s)
		{m_n = n; m_r = r; m_s = s;}

	void BERDecode(BufferedTransformation &bt);
	void DEREncode(BufferedTransformation &bt) const;

	Integer ApplyFunction(const Integer &x) const;
	Integer PreimageBound() const {return m_n;}
	Integer ImageBound() const {return m_n;}

	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);

	const Integer& GetModulus() const {return m_n;}
	const Integer& GetQuadraticResidueModPrime1() const {return m_r;}
	const Integer& GetQuadraticResidueModPrime2() const {return m_s;}

	void SetModulus(const Integer &n) {m_n = n;}
	void SetQuadraticResidueModPrime1(const Integer &r) {m_r = r;}
	void SetQuadraticResidueModPrime2(const Integer &s) {m_s = s;}

protected:
	Integer m_n, m_r, m_s;
};

//! _
class InvertibleRabinFunction : public RabinFunction, public TrapdoorFunctionInverse, public PrivateKey
{
	typedef InvertibleRabinFunction ThisClass;

public:
	void Initialize(const Integer &n, const Integer &r, const Integer &s,
							const Integer &p, const Integer &q, const Integer &u)
		{m_n = n; m_r = r; m_s = s; m_p = p; m_q = q; m_u = u;}
	void Initialize(RandomNumberGenerator &rng, unsigned int keybits)
		{GenerateRandomWithKeySize(rng, keybits);}

	void BERDecode(BufferedTransformation &bt);
	void DEREncode(BufferedTransformation &bt) const;

	Integer CalculateInverse(RandomNumberGenerator &rng, const Integer &x) const;

	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);
	/*! parameters: (ModulusSize) */
	void GenerateRandom(RandomNumberGenerator &rng, const NameValuePairs &alg);

	const Integer& GetPrime1() const {return m_p;}
	const Integer& GetPrime2() const {return m_q;}
	const Integer& GetMultiplicativeInverseOfPrime2ModPrime1() const {return m_u;}

	void SetPrime1(const Integer &p) {m_p = p;}
	void SetPrime2(const Integer &q) {m_q = q;}
	void SetMultiplicativeInverseOfPrime2ModPrime1(const Integer &u) {m_u = u;}

protected:
	Integer m_p, m_q, m_u;
};

//! Rabin
struct Rabin
{
	static std::string StaticAlgorithmName() {return "Rabin-Crypto++Variant";}
	typedef RabinFunction PublicKey;
	typedef InvertibleRabinFunction PrivateKey;
};

//! Rabin encryption
template <class STANDARD>
struct RabinES : public TF_ES<STANDARD, Rabin>
{
};

//! Rabin signature
template <class STANDARD, class H>
struct RabinSS : public TF_SS<STANDARD, H, Rabin>
{
};

// More typedefs for backwards compatibility
class SHA1;
typedef RabinES<OAEP<SHA1> >::Decryptor RabinDecryptor;
typedef RabinES<OAEP<SHA1> >::Encryptor RabinEncryptor;

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class RabinFunction:
    # // rabin.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # void RabinFunction::BERDecode(BufferedTransformation &bt)
    # 	BERSequenceDecoder seq(bt);
    # 	m_n.BERDecode(seq);
    # 	m_r.BERDecode(seq);
    # 	m_s.BERDecode(seq);
    # 	seq.MessageEnd();
    # void RabinFunction::DEREncode(BufferedTransformation &bt) const
    # 	DERSequenceEncoder seq(bt);
    # 	m_n.DEREncode(seq);
    # 	m_r.DEREncode(seq);
    # 	m_s.DEREncode(seq);
    # 	seq.MessageEnd();
    # Integer RabinFunction::ApplyFunction(const Integer &in) const
    # 	DoQuickSanityCheck();
    # 	Integer out = in.Squared()%m_n;
    # 	if (in.IsOdd())
    # 		out = out*m_r%m_n;
    # 	if (Jacobi(in, m_n)==-1)
    # 		out = out*m_s%m_n;
    # 	return out;
    # bool RabinFunction::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	bool pass = true;
    # 	if (level >= 1)
    # 		pass = pass && Jacobi(m_r, m_n) == -1 && Jacobi(m_s, m_n) == -1;
    # 	return pass;
    # bool RabinFunction::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(QuadraticResidueModPrime1)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(QuadraticResidueModPrime2)
    # 		;
    # void RabinFunction::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(QuadraticResidueModPrime1)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(QuadraticResidueModPrime2)
    # 		;
    # // *****************************************************************************
    # // private key operations:
    # // generate a random private key
    # void InvertibleRabinFunction::GenerateRandom(RandomNumberGenerator &rng, const NameValuePairs &alg)
    # 	int modulusSize = 2048;
    # 	alg.GetIntValue("ModulusSize", modulusSize) || alg.GetIntValue("KeySize", modulusSize);
    # 	if (modulusSize < 16)
    # 		throw InvalidArgument("InvertibleRabinFunction: specified modulus size is too small");
    # 	// VC70 workaround: putting these after primeParam causes overlapped stack allocation
    # 	bool rFound=false, sFound=false;
    # 	Integer t=2;
    # 	AlgorithmParameters primeParam = MakeParametersForTwoPrimesOfEqualSize(modulusSize)
    # 		("EquivalentTo", 3)("Mod", 4);
    # 	m_p.GenerateRandom(rng, primeParam);
    # 	m_q.GenerateRandom(rng, primeParam);
    # 	while (!(rFound && sFound))
    # 		int jp = Jacobi(t, m_p);
    # 		int jq = Jacobi(t, m_q);
    # 		if (!rFound && jp==1 && jq==-1)
    # 			m_r = t;
    # 			rFound = true;
    # 		if (!sFound && jp==-1 && jq==1)
    # 			m_s = t;
    # 			sFound = true;
    # 		++t;
    # 	m_n = m_p * m_q;
    # 	m_u = m_q.InverseMod(m_p);
    # void InvertibleRabinFunction::BERDecode(BufferedTransformation &bt)
    # 	BERSequenceDecoder seq(bt);
    # 	m_n.BERDecode(seq);
    # 	m_r.BERDecode(seq);
    # 	m_s.BERDecode(seq);
    # 	m_p.BERDecode(seq);
    # 	m_q.BERDecode(seq);
    # 	m_u.BERDecode(seq);
    # 	seq.MessageEnd();
    # void InvertibleRabinFunction::DEREncode(BufferedTransformation &bt) const
    # 	DERSequenceEncoder seq(bt);
    # 	m_n.DEREncode(seq);
    # 	m_r.DEREncode(seq);
    # 	m_s.DEREncode(seq);
    # 	m_p.DEREncode(seq);
    # 	m_q.DEREncode(seq);
    # 	m_u.DEREncode(seq);
    # 	seq.MessageEnd();
    # Integer InvertibleRabinFunction::CalculateInverse(RandomNumberGenerator &rng, const Integer &in) const
    # 	DoQuickSanityCheck();
    # 	ModularArithmetic modn(m_n);
    # 	r = modn.Square(r);
    # 	Integer r2 = modn.Square(r);
    # 	Integer c = modn.Multiply(in, r2);		// blind
    # 	Integer cp=c%m_p, cq=c%m_q;
    # 	int jp = Jacobi(cp, m_p);
    # 	int jq = Jacobi(cq, m_q);
    # 	if (jq==-1)
    # 		cp = cp*EuclideanMultiplicativeInverse(m_r, m_p)%m_p;
    # 		cq = cq*EuclideanMultiplicativeInverse(m_r, m_q)%m_q;
    # 	if (jp==-1)
    # 		cp = cp*EuclideanMultiplicativeInverse(m_s, m_p)%m_p;
    # 		cq = cq*EuclideanMultiplicativeInverse(m_s, m_q)%m_q;
    # 	cp = ModularSquareRoot(cp, m_p);
    # 	cq = ModularSquareRoot(cq, m_q);
    # 	if (jp==-1)
    # 		cp = m_p-cp;
    # 	Integer out = CRT(cq, m_q, cp, m_p, m_u);
    # 	out = modn.Divide(out, r);	// unblind
    # 	if ((jq==-1 && out.IsEven()) || (jq==1 && out.IsOdd()))
    # 		out = m_n-out;
    # 	return out;
    # bool InvertibleRabinFunction::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	pass = pass && m_u.IsPositive() && m_u < m_p;
    # 	if (level >= 1)
    # 		pass = pass && m_p * m_q == m_n;
    # 		pass = pass && m_u * m_q % m_p == 1;
    # 		pass = pass && Jacobi(m_r, m_p) == 1;
    # 		pass = pass && Jacobi(m_r, m_q) == -1;
    # 		pass = pass && Jacobi(m_s, m_p) == -1;
    # 		pass = pass && Jacobi(m_s, m_q) == 1;
    # 	if (level >= 2)
    # 		pass = pass && VerifyPrime(rng, m_p, level-2) && VerifyPrime(rng, m_q, level-2);
    # 	return pass;
    # bool InvertibleRabinFunction::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper<RabinFunction>(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Prime1)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Prime2)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(MultiplicativeInverseOfPrime2ModPrime1)
    # 		;
    # void InvertibleRabinFunction::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper<RabinFunction>(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Prime1)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Prime2)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(MultiplicativeInverseOfPrime2ModPrime1)
    # 		;
    # NAMESPACE_END