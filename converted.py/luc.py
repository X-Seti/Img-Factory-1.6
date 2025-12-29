"""
luc.py - Python conversion of luc.h and luc.cpp

Original C++ header content:
#ifndef CRYPTOPP_LUC_H
#define CRYPTOPP_LUC_H

/** \file
*/

#include "pkcspad.h"
#include "oaep.h"
#include "integer.h"
#include "dh.h"

#include <limits.h>

NAMESPACE_BEGIN(CryptoPP)

//! The LUC function.
/*! This class is here for historical and pedagogical interest. It has no
	practical advantages over other trapdoor functions and probably shouldn't
	be used in production software. The discrete log based LUC schemes
	defined later in this .h file may be of more practical interest.
*/
class LUCFunction : public TrapdoorFunction, public PublicKey
{
	typedef LUCFunction ThisClass;

public:
	void Initialize(const Integer &n, const Integer &e)
		{m_n = n; m_e = e;}

	void BERDecode(BufferedTransformation &bt);
	void DEREncode(BufferedTransformation &bt) const;

	Integer ApplyFunction(const Integer &x) const;
	Integer PreimageBound() const {return m_n;}
	Integer ImageBound() const {return m_n;}

	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);

	// non-derived interface
	const Integer & GetModulus() const {return m_n;}
	const Integer & GetPublicExponent() const {return m_e;}

	void SetModulus(const Integer &n) {m_n = n;}
	void SetPublicExponent(const Integer &e) {m_e = e;}

protected:
	Integer m_n, m_e;
};

//! _
class InvertibleLUCFunction : public LUCFunction, public TrapdoorFunctionInverse, public PrivateKey
{
	typedef InvertibleLUCFunction ThisClass;

public:
	void Initialize(RandomNumberGenerator &rng, unsigned int modulusBits, const Integer &eStart=17);
	void Initialize(const Integer &n, const Integer &e, const Integer &p, const Integer &q, const Integer &u)
		{m_n = n; m_e = e; m_p = p; m_q = q; m_u = u;}

	void BERDecode(BufferedTransformation &bt);
	void DEREncode(BufferedTransformation &bt) const;

	Integer CalculateInverse(RandomNumberGenerator &rng, const Integer &x) const;

	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);
	/*! parameters: (ModulusSize, PublicExponent (default 17)) */
	void GenerateRandom(RandomNumberGenerator &rng, const NameValuePairs &alg);

	// non-derived interface
	const Integer& GetPrime1() const {return m_p;}
	const Integer& GetPrime2() const {return m_q;}
	const Integer& GetMultiplicativeInverseOfPrime2ModPrime1() const {return m_u;}

	void SetPrime1(const Integer &p) {m_p = p;}
	void SetPrime2(const Integer &q) {m_q = q;}
	void SetMultiplicativeInverseOfPrime2ModPrime1(const Integer &u) {m_u = u;}

protected:
	Integer m_p, m_q, m_u;
};

struct LUC
{
	static std::string StaticAlgorithmName() {return "LUC";}
	typedef LUCFunction PublicKey;
	typedef InvertibleLUCFunction PrivateKey;
};

//! LUC cryptosystem
template <class STANDARD>
struct LUCES : public TF_ES<STANDARD, LUC>
{
};

//! LUC signature scheme with appendix
template <class STANDARD, class H>
struct LUCSS : public TF_SS<STANDARD, H, LUC>
{
};

// analagous to the RSA schemes defined in PKCS #1 v2.0
typedef LUCES<OAEP<SHA> >::Decryptor LUCES_OAEP_SHA_Decryptor;
typedef LUCES<OAEP<SHA> >::Encryptor LUCES_OAEP_SHA_Encryptor;

typedef LUCSS<PKCS1v15, SHA>::Signer LUCSSA_PKCS1v15_SHA_Signer;
typedef LUCSS<PKCS1v15, SHA>::Verifier LUCSSA_PKCS1v15_SHA_Verifier;

// ********************************************************

// no actual precomputation
class DL_GroupPrecomputation_LUC : public DL_GroupPrecomputation<Integer>
{
public:
	const AbstractGroup<Element> & GetGroup() const {assert(false); throw 0;}
	Element BERDecodeElement(BufferedTransformation &bt) const {return Integer(bt);}
	void DEREncodeElement(BufferedTransformation &bt, const Element &v) const {v.DEREncode(bt);}

	// non-inherited
	void SetModulus(const Integer &v) {m_p = v;}
	const Integer & GetModulus() const {return m_p;}

private:
	Integer m_p;
};

//! _
class DL_BasePrecomputation_LUC : public DL_FixedBasePrecomputation<Integer>
{
public:
	// DL_FixedBasePrecomputation
	bool IsInitialized() const {return m_g.NotZero();}
	void SetBase(const DL_GroupPrecomputation<Element> &group, const Integer &base) {m_g = base;}
	const Integer & GetBase(const DL_GroupPrecomputation<Element> &group) const {return m_g;}
	void Precompute(const DL_GroupPrecomputation<Element> &group, unsigned int maxExpBits, unsigned int storage) {}
	void Load(const DL_GroupPrecomputation<Element> &group, BufferedTransformation &storedPrecomputation) {}
	void Save(const DL_GroupPrecomputation<Element> &group, BufferedTransformation &storedPrecomputation) const {}
	Integer Exponentiate(const DL_GroupPrecomputation<Element> &group, const Integer &exponent) const;
	Integer CascadeExponentiate(const DL_GroupPrecomputation<Element> &group, const Integer &exponent, const DL_FixedBasePrecomputation<Integer> &pc2, const Integer &exponent2) const
		{throw NotImplemented("DL_BasePrecomputation_LUC: CascadeExponentiate not implemented");}	// shouldn't be called

private:
	Integer m_g;
};

//! _
class DL_GroupParameters_LUC : public DL_GroupParameters_IntegerBasedImpl<DL_GroupPrecomputation_LUC, DL_BasePrecomputation_LUC>
{
public:
	// DL_GroupParameters
	bool IsIdentity(const Integer &element) const {return element == Integer::Two();}
	void SimultaneousExponentiate(Element *results, const Element &base, const Integer *exponents, unsigned int exponentsCount) const;
	Element MultiplyElements(const Element &a, const Element &b) const
		{throw NotImplemented("LUC_GroupParameters: MultiplyElements can not be implemented");}
	Element CascadeExponentiate(const Element &element1, const Integer &exponent1, const Element &element2, const Integer &exponent2) const
		{throw NotImplemented("LUC_GroupParameters: MultiplyElements can not be implemented");}

	// NameValuePairs interface
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
	{
		return GetValueHelper<DL_GroupParameters_IntegerBased>(this, name, valueType, pValue).Assignable();
	}

private:
	int GetFieldType() const {return 2;}
};

//! _
class DL_GroupParameters_LUC_DefaultSafePrime : public DL_GroupParameters_LUC
{
public:
	typedef NoCofactorMultiplication DefaultCofactorOption;

protected:
	unsigned int GetDefaultSubgroupOrderSize(unsigned int modulusSize) const {return modulusSize-1;}
};

//! _
class DL_Algorithm_LUC_HMP : public DL_ElgamalLikeSignatureAlgorithm<Integer>
{
public:
	static const char * StaticAlgorithmName() {return "LUC-HMP";}

	void Sign(const DL_GroupParameters<Integer> &params, const Integer &x, const Integer &k, const Integer &e, Integer &r, Integer &s) const;
	bool Verify(const DL_GroupParameters<Integer> &params, const DL_PublicKey<Integer> &publicKey, const Integer &e, const Integer &r, const Integer &s) const;

	size_t RLen(const DL_GroupParameters<Integer> &params) const
		{return params.GetGroupOrder().ByteCount();}
};

//! _
struct DL_SignatureKeys_LUC
{
	typedef DL_GroupParameters_LUC GroupParameters;
	typedef DL_PublicKey_GFP<GroupParameters> PublicKey;
	typedef DL_PrivateKey_GFP<GroupParameters> PrivateKey;
};

//! LUC-HMP, based on "Digital signature schemes based on Lucas functions" by Patrick Horster, Markus Michels, Holger Petersen
template <class H>
struct LUC_HMP : public DL_SS<DL_SignatureKeys_LUC, DL_Algorithm_LUC_HMP, DL_SignatureMessageEncodingMethod_DSA, H>
{
};

//! _
struct DL_CryptoKeys_LUC
{
	typedef DL_GroupParameters_LUC_DefaultSafePrime GroupParameters;
	typedef DL_PublicKey_GFP<GroupParameters> PublicKey;
	typedef DL_PrivateKey_GFP<GroupParameters> PrivateKey;
};

//! LUC-IES
template <class COFACTOR_OPTION = NoCofactorMultiplication, bool DHAES_MODE = true>
struct LUC_IES
	: public DL_ES<
		DL_CryptoKeys_LUC,
		DL_KeyAgreementAlgorithm_DH<Integer, COFACTOR_OPTION>,
		DL_KeyDerivationAlgorithm_P1363<Integer, DHAES_MODE, P1363_KDF2<SHA1> >,
		DL_EncryptionAlgorithm_Xor<HMAC<SHA1>, DHAES_MODE>,
		LUC_IES<> >
{
	static std::string StaticAlgorithmName() {return "LUC-IES";}	// non-standard name
};

// ********************************************************

//! LUC-DH
typedef DH_Domain<DL_GroupParameters_LUC_DefaultSafePrime> LUC_DH;

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class DL_Algorithm_LUC_HMP:
    # // luc.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # void LUC_TestInstantiations()
    # 	LUCFunction t2;
    # 	InvertibleLUCFunction t3;
    # void DL_Algorithm_LUC_HMP::Sign(const DL_GroupParameters<Integer> &params, const Integer &x, const Integer &k, const Integer &e, Integer &r, Integer &s) const
    # 	const Integer &q = params.GetSubgroupOrder();
    # 	r = params.ExponentiateBase(k);
    # 	s = (k + x*(r+e)) % q;
    # bool DL_Algorithm_LUC_HMP::Verify(const DL_GroupParameters<Integer> &params, const DL_PublicKey<Integer> &publicKey, const Integer &e, const Integer &r, const Integer &s) const
    # 	Integer p = params.GetGroupOrder()-1;
    # 	const Integer &q = params.GetSubgroupOrder();
    # 	Integer Vsg = params.ExponentiateBase(s);
    # 	Integer Vry = publicKey.ExponentiatePublicElement((r+e)%q);
    # 	return (Vsg*Vsg + Vry*Vry + r*r) % p == (Vsg * Vry * r + 4) % p;
    # Integer DL_BasePrecomputation_LUC::Exponentiate(const DL_GroupPrecomputation<Element> &group, const Integer &exponent) const
    # 	return Lucas(exponent, m_g, static_cast<const DL_GroupPrecomputation_LUC &>(group).GetModulus());
    # void DL_GroupParameters_LUC::SimultaneousExponentiate(Element *results, const Element &base, const Integer *exponents, unsigned int exponentsCount) const
    # 	for (unsigned int i=0; i<exponentsCount; i++)
    # 		results[i] = Lucas(exponents[i], base, GetModulus());
    # void LUCFunction::BERDecode(BufferedTransformation &bt)
    # 	BERSequenceDecoder seq(bt);
    # 	m_n.BERDecode(seq);
    # 	m_e.BERDecode(seq);
    # 	seq.MessageEnd();
    # void LUCFunction::DEREncode(BufferedTransformation &bt) const
    # 	DERSequenceEncoder seq(bt);
    # 	m_n.DEREncode(seq);
    # 	m_e.DEREncode(seq);
    # 	seq.MessageEnd();
    # Integer LUCFunction::ApplyFunction(const Integer &x) const
    # 	DoQuickSanityCheck();
    # 	return Lucas(m_e, x, m_n);
    # bool LUCFunction::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	bool pass = true;
    # 	return pass;
    # bool LUCFunction::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(PublicExponent)
    # 		;
    # void LUCFunction::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(PublicExponent)
    # 		;
    # // *****************************************************************************
    # // private key operations:
    # class LUCPrimeSelector : public PrimeSelector
    # public:
    # 	LUCPrimeSelector(const Integer &e) : m_e(e) {}
    # 	bool IsAcceptable(const Integer &candidate) const
    # 		return RelativelyPrime(m_e, candidate+1) && RelativelyPrime(m_e, candidate-1);
    # 	Integer m_e;
    # };
    # void InvertibleLUCFunction::GenerateRandom(RandomNumberGenerator &rng, const NameValuePairs &alg)
    # 	int modulusSize = 2048;
    # 	alg.GetIntValue("ModulusSize", modulusSize) || alg.GetIntValue("KeySize", modulusSize);
    # 	if (modulusSize < 16)
    # 		throw InvalidArgument("InvertibleLUCFunction: specified modulus size is too small");
    # 	m_e = alg.GetValueWithDefault("PublicExponent", Integer(17));
    # 	if (m_e < 5 || m_e.IsEven())
    # 		throw InvalidArgument("InvertibleLUCFunction: invalid public exponent");
    # 	LUCPrimeSelector selector(m_e);
    # 	AlgorithmParameters primeParam = MakeParametersForTwoPrimesOfEqualSize(modulusSize)
    # 		("PointerToPrimeSelector", selector.GetSelectorPointer());
    # 	m_p.GenerateRandom(rng, primeParam);
    # 	m_q.GenerateRandom(rng, primeParam);
    # 	m_n = m_p * m_q;
    # 	m_u = m_q.InverseMod(m_p);
    # void InvertibleLUCFunction::Initialize(RandomNumberGenerator &rng, unsigned int keybits, const Integer &e)
    # 	GenerateRandom(rng, MakeParameters("ModulusSize", (int)keybits)("PublicExponent", e));
    # void InvertibleLUCFunction::BERDecode(BufferedTransformation &bt)
    # 	BERSequenceDecoder seq(bt);
    # 	Integer version(seq);
    # 	if (!!version)  // make sure version is 0
    # 		BERDecodeError();
    # 	m_n.BERDecode(seq);
    # 	m_e.BERDecode(seq);
    # 	m_p.BERDecode(seq);
    # 	m_q.BERDecode(seq);
    # 	m_u.BERDecode(seq);
    # 	seq.MessageEnd();
    # void InvertibleLUCFunction::DEREncode(BufferedTransformation &bt) const
    # 	DERSequenceEncoder seq(bt);
    # 	const byte version[] = {INTEGER, 1, 0};
    # 	seq.Put(version, sizeof(version));
    # 	m_n.DEREncode(seq);
    # 	m_e.DEREncode(seq);
    # 	m_p.DEREncode(seq);
    # 	m_q.DEREncode(seq);
    # 	m_u.DEREncode(seq);
    # 	seq.MessageEnd();
    # Integer InvertibleLUCFunction::CalculateInverse(RandomNumberGenerator &rng, const Integer &x) const
    # 	// not clear how to do blinding with LUC
    # 	DoQuickSanityCheck();
    # 	return InverseLucas(m_e, x, m_q, m_p, m_u);
    # bool InvertibleLUCFunction::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	pass = pass && m_u.IsPositive() && m_u < m_p;
    # 	if (level >= 1)
    # 		pass = pass && m_p * m_q == m_n;
    # 		pass = pass && RelativelyPrime(m_e, m_p+1);
    # 		pass = pass && RelativelyPrime(m_e, m_p-1);
    # 		pass = pass && RelativelyPrime(m_e, m_q+1);
    # 		pass = pass && RelativelyPrime(m_e, m_q-1);
    # 		pass = pass && m_u * m_q % m_p == 1;
    # 	if (level >= 2)
    # 		pass = pass && VerifyPrime(rng, m_p, level-2) && VerifyPrime(rng, m_q, level-2);
    # 	return pass;
    # bool InvertibleLUCFunction::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper<LUCFunction>(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Prime1)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Prime2)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(MultiplicativeInverseOfPrime2ModPrime1)
    # 		;
    # void InvertibleLUCFunction::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper<LUCFunction>(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Prime1)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Prime2)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(MultiplicativeInverseOfPrime2ModPrime1)
    # 		;
    # NAMESPACE_END