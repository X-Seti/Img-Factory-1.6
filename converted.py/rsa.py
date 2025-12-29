"""
rsa.py - Python conversion of rsa.h and rsa.cpp

Original C++ header content:
#ifndef CRYPTOPP_RSA_H
#define CRYPTOPP_RSA_H

/** \file
	This file contains classes that implement the RSA
	ciphers and signature schemes as defined in PKCS #1 v2.0.
*/

#include "pubkey.h"
#include "asn.h"
#include "pkcspad.h"
#include "oaep.h"
#include "emsa2.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
class CRYPTOPP_DLL RSAFunction : public TrapdoorFunction, public X509PublicKey
{
	typedef RSAFunction ThisClass;

public:
	void Initialize(const Integer &n, const Integer &e)
		{m_n = n; m_e = e;}

	// X509PublicKey
	OID GetAlgorithmID() const;
	void BERDecodePublicKey(BufferedTransformation &bt, bool parametersPresent, size_t size);
	void DEREncodePublicKey(BufferedTransformation &bt) const;

	// CryptoMaterial
	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);

	// TrapdoorFunction
	Integer ApplyFunction(const Integer &x) const;
	Integer PreimageBound() const {return m_n;}
	Integer ImageBound() const {return m_n;}

	// non-derived
	const Integer & GetModulus() const {return m_n;}
	const Integer & GetPublicExponent() const {return m_e;}

	void SetModulus(const Integer &n) {m_n = n;}
	void SetPublicExponent(const Integer &e) {m_e = e;}

protected:
	Integer m_n, m_e;
};

//! _
class CRYPTOPP_DLL InvertibleRSAFunction : public RSAFunction, public TrapdoorFunctionInverse, public PKCS8PrivateKey
{
	typedef InvertibleRSAFunction ThisClass;

public:
	void Initialize(RandomNumberGenerator &rng, unsigned int modulusBits, const Integer &e = 17);
	void Initialize(const Integer &n, const Integer &e, const Integer &d, const Integer &p, const Integer &q, const Integer &dp, const Integer &dq, const Integer &u)
		{m_n = n; m_e = e; m_d = d; m_p = p; m_q = q; m_dp = dp; m_dq = dq; m_u = u;}
	//! factor n given private exponent
	void Initialize(const Integer &n, const Integer &e, const Integer &d);

	// PKCS8PrivateKey
	void BERDecode(BufferedTransformation &bt)
		{PKCS8PrivateKey::BERDecode(bt);}
	void DEREncode(BufferedTransformation &bt) const
		{PKCS8PrivateKey::DEREncode(bt);}
	void Load(BufferedTransformation &bt)
		{PKCS8PrivateKey::BERDecode(bt);}
	void Save(BufferedTransformation &bt) const
		{PKCS8PrivateKey::DEREncode(bt);}
	OID GetAlgorithmID() const {return RSAFunction::GetAlgorithmID();}
	void BERDecodePrivateKey(BufferedTransformation &bt, bool parametersPresent, size_t size);
	void DEREncodePrivateKey(BufferedTransformation &bt) const;

	// TrapdoorFunctionInverse
	Integer CalculateInverse(RandomNumberGenerator &rng, const Integer &x) const;

	// GeneratableCryptoMaterial
	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	/*! parameters: (ModulusSize, PublicExponent (default 17)) */
	void GenerateRandom(RandomNumberGenerator &rng, const NameValuePairs &alg);
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);

	// non-derived interface
	const Integer& GetPrime1() const {return m_p;}
	const Integer& GetPrime2() const {return m_q;}
	const Integer& GetPrivateExponent() const {return m_d;}
	const Integer& GetModPrime1PrivateExponent() const {return m_dp;}
	const Integer& GetModPrime2PrivateExponent() const {return m_dq;}
	const Integer& GetMultiplicativeInverseOfPrime2ModPrime1() const {return m_u;}

	void SetPrime1(const Integer &p) {m_p = p;}
	void SetPrime2(const Integer &q) {m_q = q;}
	void SetPrivateExponent(const Integer &d) {m_d = d;}
	void SetModPrime1PrivateExponent(const Integer &dp) {m_dp = dp;}
	void SetModPrime2PrivateExponent(const Integer &dq) {m_dq = dq;}
	void SetMultiplicativeInverseOfPrime2ModPrime1(const Integer &u) {m_u = u;}

protected:
	Integer m_d, m_p, m_q, m_dp, m_dq, m_u;
};

class CRYPTOPP_DLL RSAFunction_ISO : public RSAFunction
{
public:
	Integer ApplyFunction(const Integer &x) const;
	Integer PreimageBound() const {return ++(m_n>>1);}
};

class CRYPTOPP_DLL InvertibleRSAFunction_ISO : public InvertibleRSAFunction
{
public:
	Integer CalculateInverse(RandomNumberGenerator &rng, const Integer &x) const;
	Integer PreimageBound() const {return ++(m_n>>1);}
};

//! RSA
struct CRYPTOPP_DLL RSA
{
	static const char * CRYPTOPP_API StaticAlgorithmName() {return "RSA";}
	typedef RSAFunction PublicKey;
	typedef InvertibleRSAFunction PrivateKey;
};

//! <a href="http://www.weidai.com/scan-mirror/ca.html#RSA">RSA cryptosystem</a>
template <class STANDARD>
struct RSAES : public TF_ES<STANDARD, RSA>
{
};

//! <a href="http://www.weidai.com/scan-mirror/sig.html#RSA">RSA signature scheme with appendix</a>
/*! See documentation of PKCS1v15 for a list of hash functions that can be used with it. */
template <class STANDARD, class H>
struct RSASS : public TF_SS<STANDARD, H, RSA>
{
};

struct CRYPTOPP_DLL RSA_ISO
{
	static const char * CRYPTOPP_API StaticAlgorithmName() {return "RSA-ISO";}
	typedef RSAFunction_ISO PublicKey;
	typedef InvertibleRSAFunction_ISO PrivateKey;
};

template <class H>
struct RSASS_ISO : public TF_SS<P1363_EMSA2, H, RSA_ISO>
{
};

// The two RSA encryption schemes defined in PKCS #1 v2.0
typedef RSAES<PKCS1v15>::Decryptor RSAES_PKCS1v15_Decryptor;
typedef RSAES<PKCS1v15>::Encryptor RSAES_PKCS1v15_Encryptor;

typedef RSAES<OAEP<SHA> >::Decryptor RSAES_OAEP_SHA_Decryptor;
typedef RSAES<OAEP<SHA> >::Encryptor RSAES_OAEP_SHA_Encryptor;

// The three RSA signature schemes defined in PKCS #1 v2.0
typedef RSASS<PKCS1v15, SHA>::Signer RSASSA_PKCS1v15_SHA_Signer;
typedef RSASS<PKCS1v15, SHA>::Verifier RSASSA_PKCS1v15_SHA_Verifier;

namespace Weak {
typedef RSASS<PKCS1v15, Weak1::MD2>::Signer RSASSA_PKCS1v15_MD2_Signer;
typedef RSASS<PKCS1v15, Weak1::MD2>::Verifier RSASSA_PKCS1v15_MD2_Verifier;

typedef RSASS<PKCS1v15, Weak1::MD5>::Signer RSASSA_PKCS1v15_MD5_Signer;
typedef RSASS<PKCS1v15, Weak1::MD5>::Verifier RSASSA_PKCS1v15_MD5_Verifier;
}

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class RSAFunction:
    # // rsa.cpp - written and placed in the public domain by Wei Dai
    # #if !defined(NDEBUG) && !defined(CRYPTOPP_IS_DLL)
    # NAMESPACE_BEGIN(CryptoPP)
    # void RSA_TestInstantiations()
    # #ifndef __MWERKS__
    # 	x3 = x2;
    # 	x6 = x2;
    # #endif
    # #ifndef __GNUC__
    # #endif
    # 	x4 = x2.GetKey();
    # NAMESPACE_END
    # #endif
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # OID RSAFunction::GetAlgorithmID() const
    # void RSAFunction::BERDecodePublicKey(BufferedTransformation &bt, bool, size_t)
    # 	BERSequenceDecoder seq(bt);
    # 		m_n.BERDecode(seq);
    # 		m_e.BERDecode(seq);
    # 	seq.MessageEnd();
    # void RSAFunction::DEREncodePublicKey(BufferedTransformation &bt) const
    # 	DERSequenceEncoder seq(bt);
    # 		m_n.DEREncode(seq);
    # 		m_e.DEREncode(seq);
    # 	seq.MessageEnd();
    # Integer RSAFunction::ApplyFunction(const Integer &x) const
    # 	DoQuickSanityCheck();
    # 	return a_exp_b_mod_c(x, m_e, m_n);
    # bool RSAFunction::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	bool pass = true;
    # 	return pass;
    # bool RSAFunction::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(PublicExponent)
    # 		;
    # void RSAFunction::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(PublicExponent)
    # 		;
    # // *****************************************************************************
    # class RSAPrimeSelector : public PrimeSelector
    # public:
    # 	RSAPrimeSelector(const Integer &e) : m_e(e) {}
    # 	Integer m_e;
    # };
    # void InvertibleRSAFunction::GenerateRandom(RandomNumberGenerator &rng, const NameValuePairs &alg)
    # 	int modulusSize = 2048;
    # 	if (modulusSize < 16)
    # 		throw InvalidArgument("InvertibleRSAFunction: specified modulus size is too small");
    # 	if (m_e < 3 || m_e.IsEven())
    # 		throw InvalidArgument("InvertibleRSAFunction: invalid public exponent");
    # 	RSAPrimeSelector selector(m_e);
    # 	AlgorithmParameters primeParam = MakeParametersForTwoPrimesOfEqualSize(modulusSize)
    # 	m_p.GenerateRandom(rng, primeParam);
    # 	m_q.GenerateRandom(rng, primeParam);
    # 	m_d = m_e.InverseMod(LCM(m_p-1, m_q-1));
    # 	assert(m_d.IsPositive());
    # 	m_dp = m_d % (m_p-1);
    # 	m_dq = m_d % (m_q-1);
    # 	m_n = m_p * m_q;
    # 	m_u = m_q.InverseMod(m_p);
    # 	if (FIPS_140_2_ComplianceEnabled())
    # 		SignaturePairwiseConsistencyTest_FIPS_140_Only(signer, verifier);
    # 		EncryptionPairwiseConsistencyTest_FIPS_140_Only(encryptor, decryptor);
    # void InvertibleRSAFunction::Initialize(RandomNumberGenerator &rng, unsigned int keybits, const Integer &e)
    # void InvertibleRSAFunction::Initialize(const Integer &n, const Integer &e, const Integer &d)
    # 	if (n.IsEven() || e.IsEven() | d.IsEven())
    # 		throw InvalidArgument("InvertibleRSAFunction: input is not a valid RSA private key");
    # 	m_n = n;
    # 	m_e = e;
    # 	m_d = d;
    # 	Integer r = --(d*e);
    # 	unsigned int s = 0;
    # 	while (r.IsEven())
    # 		r >>= 1;
    # 		s++;
    # 	ModularArithmetic modn(n);
    # 	for (Integer i = 2; ; ++i)
    # 		Integer a = modn.Exponentiate(i, r);
    # 		if (a == 1)
    # 			continue;
    # 		Integer b;
    # 		unsigned int j = 0;
    # 		while (a != n-1)
    # 			b = modn.Square(a);
    # 			if (b == 1)
    # 				m_p = GCD(a-1, n);
    # 				m_q = n/m_p;
    # 				m_dp = m_d % (m_p-1);
    # 				m_dq = m_d % (m_q-1);
    # 				m_u = m_q.InverseMod(m_p);
    # 				return;
    # 			if (++j == s)
    # 				throw InvalidArgument("InvertibleRSAFunction: input is not a valid RSA private key");
    # 			a = b;
    # void InvertibleRSAFunction::BERDecodePrivateKey(BufferedTransformation &bt, bool, size_t)
    # 	BERSequenceDecoder privateKey(bt);
    # 		word32 version;
    # 		BERDecodeUnsigned<word32>(privateKey, version, INTEGER, 0, 0);	// check version
    # 		m_n.BERDecode(privateKey);
    # 		m_e.BERDecode(privateKey);
    # 		m_d.BERDecode(privateKey);
    # 		m_p.BERDecode(privateKey);
    # 		m_q.BERDecode(privateKey);
    # 		m_dp.BERDecode(privateKey);
    # 		m_dq.BERDecode(privateKey);
    # 		m_u.BERDecode(privateKey);
    # 	privateKey.MessageEnd();
    # void InvertibleRSAFunction::DEREncodePrivateKey(BufferedTransformation &bt) const
    # 	DERSequenceEncoder privateKey(bt);
    # 		DEREncodeUnsigned<word32>(privateKey, 0);	// version
    # 		m_n.DEREncode(privateKey);
    # 		m_e.DEREncode(privateKey);
    # 		m_d.DEREncode(privateKey);
    # 		m_p.DEREncode(privateKey);
    # 		m_q.DEREncode(privateKey);
    # 		m_dp.DEREncode(privateKey);
    # 		m_dq.DEREncode(privateKey);
    # 		m_u.DEREncode(privateKey);
    # 	privateKey.MessageEnd();
    # Integer InvertibleRSAFunction::CalculateInverse(RandomNumberGenerator &rng, const Integer &x) const 
    # 	DoQuickSanityCheck();
    # 	ModularArithmetic modn(m_n);
    # 	Integer r, rInv;
    # 	do {	// do this in a loop for people using small numbers for testing
    # 		rInv = modn.MultiplicativeInverse(r);
    # 	} while (rInv.IsZero());
    # 	Integer re = modn.Exponentiate(r, m_e);
    # 	re = modn.Multiply(re, x);			// blind
    # 	// here we follow the notation of PKCS #1 and let u=q inverse mod p
    # 	// but in ModRoot, u=p inverse mod q, so we reverse the order of p and q
    # 	Integer y = ModularRoot(re, m_dq, m_dp, m_q, m_p, m_u);
    # 	y = modn.Multiply(y, rInv);				// unblind
    # 	if (modn.Exponentiate(y, m_e) != x)		// check
    # 	return y;
    # bool InvertibleRSAFunction::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	pass = pass && m_u.IsPositive() && m_u < m_p;
    # 	if (level >= 1)
    # 		pass = pass && m_p * m_q == m_n;
    # 		pass = pass && m_e*m_d % LCM(m_p-1, m_q-1) == 1;
    # 		pass = pass && m_dp == m_d%(m_p-1) && m_dq == m_d%(m_q-1);
    # 		pass = pass && m_u * m_q % m_p == 1;
    # 	if (level >= 2)
    # 		pass = pass && VerifyPrime(rng, m_p, level-2) && VerifyPrime(rng, m_q, level-2);
    # 	return pass;
    # bool InvertibleRSAFunction::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper<RSAFunction>(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Prime1)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Prime2)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(PrivateExponent)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(ModPrime1PrivateExponent)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(ModPrime2PrivateExponent)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(MultiplicativeInverseOfPrime2ModPrime1)
    # 		;
    # void InvertibleRSAFunction::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper<RSAFunction>(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Prime1)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Prime2)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(PrivateExponent)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(ModPrime1PrivateExponent)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(ModPrime2PrivateExponent)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(MultiplicativeInverseOfPrime2ModPrime1)
    # 		;
    # // *****************************************************************************
    # Integer RSAFunction_ISO::ApplyFunction(const Integer &x) const
    # 	return t % 16 == 12 ? t : m_n - t;
    # Integer InvertibleRSAFunction_ISO::CalculateInverse(RandomNumberGenerator &rng, const Integer &x) const 
    # 	return STDMIN(t, m_n-t);
    # NAMESPACE_END
    # #endif