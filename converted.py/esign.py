"""
esign.py - Python conversion of esign.h and esign.cpp

Original C++ header content:
#ifndef CRYPTOPP_ESIGN_H
#define CRYPTOPP_ESIGN_H

/** \file
	This file contains classes that implement the
	ESIGN signature schemes as defined in IEEE P1363a.
*/

#include "pubkey.h"
#include "integer.h"
#include "asn.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
class ESIGNFunction : public TrapdoorFunction, public ASN1CryptoMaterial<PublicKey>
{
	typedef ESIGNFunction ThisClass;

public:
	void Initialize(const Integer &n, const Integer &e)
		{m_n = n; m_e = e;}

	// PublicKey
	void BERDecode(BufferedTransformation &bt);
	void DEREncode(BufferedTransformation &bt) const;

	// CryptoMaterial
	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);

	// TrapdoorFunction
	Integer ApplyFunction(const Integer &x) const;
	Integer PreimageBound() const {return m_n;}
	Integer ImageBound() const {return Integer::Power2(GetK());}

	// non-derived
	const Integer & GetModulus() const {return m_n;}
	const Integer & GetPublicExponent() const {return m_e;}

	void SetModulus(const Integer &n) {m_n = n;}
	void SetPublicExponent(const Integer &e) {m_e = e;}

protected:
	unsigned int GetK() const {return m_n.BitCount()/3-1;}

	Integer m_n, m_e;
};

//! _
class InvertibleESIGNFunction : public ESIGNFunction, public RandomizedTrapdoorFunctionInverse, public PrivateKey
{
	typedef InvertibleESIGNFunction ThisClass;

public:
	void Initialize(const Integer &n, const Integer &e, const Integer &p, const Integer &q)
		{m_n = n; m_e = e; m_p = p; m_q = q;}
	// generate a random private key
	void Initialize(RandomNumberGenerator &rng, unsigned int modulusBits)
		{GenerateRandomWithKeySize(rng, modulusBits);}

	void BERDecode(BufferedTransformation &bt);
	void DEREncode(BufferedTransformation &bt) const;

	Integer CalculateRandomizedInverse(RandomNumberGenerator &rng, const Integer &x) const;

	// GeneratibleCryptoMaterial
	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);
	/*! parameters: (ModulusSize) */
	void GenerateRandom(RandomNumberGenerator &rng, const NameValuePairs &alg);

	const Integer& GetPrime1() const {return m_p;}
	const Integer& GetPrime2() const {return m_q;}

	void SetPrime1(const Integer &p) {m_p = p;}
	void SetPrime2(const Integer &q) {m_q = q;}

protected:
	Integer m_p, m_q;
};

//! _
template <class T>
class EMSA5Pad : public PK_DeterministicSignatureMessageEncodingMethod
{
public:
	static const char *StaticAlgorithmName() {return "EMSA5";}
	
	void ComputeMessageRepresentative(RandomNumberGenerator &rng, 
		const byte *recoverableMessage, size_t recoverableMessageLength,
		HashTransformation &hash, HashIdentifier hashIdentifier, bool messageEmpty,
		byte *representative, size_t representativeBitLength) const
	{
		SecByteBlock digest(hash.DigestSize());
		hash.Final(digest);
		size_t representativeByteLength = BitsToBytes(representativeBitLength);
		T mgf;
		mgf.GenerateAndMask(hash, representative, representativeByteLength, digest, digest.size(), false);
		if (representativeBitLength % 8 != 0)
			representative[0] = (byte)Crop(representative[0], representativeBitLength % 8);
	}
};

//! EMSA5, for use with ESIGN
struct P1363_EMSA5 : public SignatureStandard
{
	typedef EMSA5Pad<P1363_MGF1> SignatureMessageEncodingMethod;
};

struct ESIGN_Keys
{
	static std::string StaticAlgorithmName() {return "ESIGN";}
	typedef ESIGNFunction PublicKey;
	typedef InvertibleESIGNFunction PrivateKey;
};

//! ESIGN, as defined in IEEE P1363a
template <class H, class STANDARD = P1363_EMSA5>
struct ESIGN : public TF_SS<STANDARD, H, ESIGN_Keys>
{
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class ESIGNFunction:
    # // esign.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # void ESIGN_TestInstantiations()
    # 	x6 = x2;
    # 	x4 = x2.GetKey();
    # void ESIGNFunction::BERDecode(BufferedTransformation &bt)
    # 	BERSequenceDecoder seq(bt);
    # 		m_n.BERDecode(seq);
    # 		m_e.BERDecode(seq);
    # 	seq.MessageEnd();
    # void ESIGNFunction::DEREncode(BufferedTransformation &bt) const
    # 	DERSequenceEncoder seq(bt);
    # 		m_n.DEREncode(seq);
    # 		m_e.DEREncode(seq);
    # 	seq.MessageEnd();
    # Integer ESIGNFunction::ApplyFunction(const Integer &x) const
    # 	DoQuickSanityCheck();
    # 	return STDMIN(a_exp_b_mod_c(x, m_e, m_n) >> (2*GetK()+2), MaxImage());
    # bool ESIGNFunction::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	bool pass = true;
    # 	pass = pass && m_e >= 8 && m_e < m_n;
    # 	return pass;
    # bool ESIGNFunction::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(PublicExponent)
    # 		;
    # void ESIGNFunction::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(PublicExponent)
    # 		;
    # // *****************************************************************************
    # void InvertibleESIGNFunction::GenerateRandom(RandomNumberGenerator &rng, const NameValuePairs &param)
    # 	int modulusSize = 1023*2;
    # 	param.GetIntValue("ModulusSize", modulusSize) || param.GetIntValue("KeySize", modulusSize);
    # 	if (modulusSize < 24)
    # 		throw InvalidArgument("InvertibleESIGNFunction: specified modulus size is too small");
    # 	if (modulusSize % 3 != 0)
    # 		throw InvalidArgument("InvertibleESIGNFunction: modulus size must be divisible by 3");
    # 	m_e = param.GetValueWithDefault("PublicExponent", Integer(32));
    # 	if (m_e < 8)
    # 		throw InvalidArgument("InvertibleESIGNFunction: public exponents less than 8 may not be secure");
    # 	// VC70 workaround: putting these after primeParam causes overlapped stack allocation
    # 	ConstByteArrayParameter seedParam;
    # 	SecByteBlock seed;
    # 	const Integer minP = Integer(204) << (modulusSize/3-8);
    # 	if (param.GetValue("Seed", seedParam))
    # 		seed.resize(seedParam.size() + 4);
    # 		memcpy(seed + 4, seedParam.begin(), seedParam.size());
    # 		PutWord(false, BIG_ENDIAN_ORDER, seed, (word32)0);
    # 		m_p.GenerateRandom(rng, CombinedNameValuePairs(primeParam, MakeParameters("Seed", ConstByteArrayParameter(seed))));
    # 		PutWord(false, BIG_ENDIAN_ORDER, seed, (word32)1);
    # 		m_q.GenerateRandom(rng, CombinedNameValuePairs(primeParam, MakeParameters("Seed", ConstByteArrayParameter(seed))));
    # 	else
    # 		m_p.GenerateRandom(rng, primeParam);
    # 		m_q.GenerateRandom(rng, primeParam);
    # 	m_n = m_p * m_p * m_q;
    # 	assert(m_n.BitCount() == modulusSize);
    # void InvertibleESIGNFunction::BERDecode(BufferedTransformation &bt)
    # 	BERSequenceDecoder privateKey(bt);
    # 		m_n.BERDecode(privateKey);
    # 		m_e.BERDecode(privateKey);
    # 		m_p.BERDecode(privateKey);
    # 		m_q.BERDecode(privateKey);
    # 	privateKey.MessageEnd();
    # void InvertibleESIGNFunction::DEREncode(BufferedTransformation &bt) const
    # 	DERSequenceEncoder privateKey(bt);
    # 		m_n.DEREncode(privateKey);
    # 		m_e.DEREncode(privateKey);
    # 		m_p.DEREncode(privateKey);
    # 		m_q.DEREncode(privateKey);
    # 	privateKey.MessageEnd();
    # Integer InvertibleESIGNFunction::CalculateRandomizedInverse(RandomNumberGenerator &rng, const Integer &x) const 
    # 	DoQuickSanityCheck();
    # 	Integer pq = m_p * m_q;
    # 	Integer p2 = m_p * m_p;
    # 	Integer r, z, re, a, w0, w1;
    # 	do
    # 		z = x << (2*GetK()+2);
    # 		re = a_exp_b_mod_c(r, m_e, m_n);
    # 		a = (z - re) % m_n;
    # 		if (w1.NotZero())
    # 			++w0;
    # 			w1 = pq - w1;
    # 	while ((w1 >> 2*GetK()+1).IsPositive());
    # 	ModularArithmetic modp(m_p);
    # 	Integer t = modp.Divide(w0 * r % m_p, m_e * re % m_p);
    # 	Integer s = r + t*pq;
    # 	assert(s < m_n);
    # /*
    # 	cout << "f = " << x << endl;
    # 	cout << "r = " << r << endl;
    # 	cout << "z = " << z << endl;
    # 	cout << "a = " << a << endl;
    # 	cout << "w0 = " << w0 << endl;
    # 	cout << "w1 = " << w1 << endl;
    # 	cout << "t = " << t << endl;
    # 	cout << "s = " << s << endl;
    # */
    # 	return s;
    # bool InvertibleESIGNFunction::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	pass = pass && m_p.BitCount() == m_q.BitCount();
    # 	if (level >= 1)
    # 		pass = pass && m_p * m_p * m_q == m_n;
    # 	if (level >= 2)
    # 		pass = pass && VerifyPrime(rng, m_p, level-2) && VerifyPrime(rng, m_q, level-2);
    # 	return pass;
    # bool InvertibleESIGNFunction::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper<ESIGNFunction>(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Prime1)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Prime2)
    # 		;
    # void InvertibleESIGNFunction::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper<ESIGNFunction>(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Prime1)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Prime2)
    # 		;
    # NAMESPACE_END