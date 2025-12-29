"""
xtrcrypt.py - Python conversion of xtrcrypt.h and xtrcrypt.cpp

Original C++ header content:
#ifndef CRYPTOPP_XTRCRYPT_H
#define CRYPTOPP_XTRCRYPT_H

/** \file
	"The XTR public key system" by Arjen K. Lenstra and Eric R. Verheul
*/

#include "xtr.h"

NAMESPACE_BEGIN(CryptoPP)

//! XTR-DH with key validation

class XTR_DH : public SimpleKeyAgreementDomain, public CryptoParameters
{
	typedef XTR_DH ThisClass;
	
public:
	XTR_DH(const Integer &p, const Integer &q, const GFP2Element &g);
	XTR_DH(RandomNumberGenerator &rng, unsigned int pbits, unsigned int qbits);
	XTR_DH(BufferedTransformation &domainParams);

	void DEREncode(BufferedTransformation &domainParams) const;

	bool Validate(RandomNumberGenerator &rng, unsigned int level) const;
	bool GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const;
	void AssignFrom(const NameValuePairs &source);
	CryptoParameters & AccessCryptoParameters() {return *this;}
	unsigned int AgreedValueLength() const {return 2*m_p.ByteCount();}
	unsigned int PrivateKeyLength() const {return m_q.ByteCount();}
	unsigned int PublicKeyLength() const {return 2*m_p.ByteCount();}

	void GeneratePrivateKey(RandomNumberGenerator &rng, byte *privateKey) const;
	void GeneratePublicKey(RandomNumberGenerator &rng, const byte *privateKey, byte *publicKey) const;
	bool Agree(byte *agreedValue, const byte *privateKey, const byte *otherPublicKey, bool validateOtherPublicKey=true) const;

	const Integer &GetModulus() const {return m_p;}
	const Integer &GetSubgroupOrder() const {return m_q;}
	const GFP2Element &GetSubgroupGenerator() const {return m_g;}

	void SetModulus(const Integer &p) {m_p = p;}
	void SetSubgroupOrder(const Integer &q) {m_q = q;}
	void SetSubgroupGenerator(const GFP2Element &g) {m_g = g;}

private:
	unsigned int ExponentBitLength() const;

	Integer m_p, m_q;
	GFP2Element m_g;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class XTR_DH:
    # // xtrcrypt.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # XTR_DH::XTR_DH(const Integer &p, const Integer &q, const GFP2Element &g)
    # 	: m_p(p), m_q(q), m_g(g)
    # XTR_DH::XTR_DH(RandomNumberGenerator &rng, unsigned int pbits, unsigned int qbits)
    # 	XTR_FindPrimesAndGenerator(rng, m_p, m_q, m_g, pbits, qbits);
    # XTR_DH::XTR_DH(BufferedTransformation &bt)
    # 	BERSequenceDecoder seq(bt);
    # 	m_p.BERDecode(seq);
    # 	m_q.BERDecode(seq);
    # 	m_g.c1.BERDecode(seq);
    # 	m_g.c2.BERDecode(seq);
    # 	seq.MessageEnd();
    # void XTR_DH::DEREncode(BufferedTransformation &bt) const
    # 	DERSequenceEncoder seq(bt);
    # 	m_p.DEREncode(seq);
    # 	m_q.DEREncode(seq);
    # 	m_g.c1.DEREncode(seq);
    # 	m_g.c2.DEREncode(seq);
    # 	seq.MessageEnd();
    # bool XTR_DH::Validate(RandomNumberGenerator &rng, unsigned int level) const
    # 	bool pass = true;
    # 	GFP2Element three = GFP2_ONB<ModularArithmetic>(m_p).ConvertIn(3);
    # 	pass = pass && !(m_g.c1.IsNegative() || m_g.c2.IsNegative() || m_g.c1 >= m_p || m_g.c2 >= m_p || m_g == three);
    # 	if (level >= 1)
    # 		pass = pass && ((m_p.Squared()-m_p+1)%m_q).IsZero();
    # 	if (level >= 2)
    # 		pass = pass && VerifyPrime(rng, m_p, level-2) && VerifyPrime(rng, m_q, level-2);
    # 		pass = pass && XTR_Exponentiate(m_g, (m_p.Squared()-m_p+1)/m_q, m_p) != three;
    # 		pass = pass && XTR_Exponentiate(m_g, m_q, m_p) == three;
    # 	return pass;
    # bool XTR_DH::GetVoidValue(const char *name, const std::type_info &valueType, void *pValue) const
    # 	return GetValueHelper(this, name, valueType, pValue).Assignable()
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(SubgroupOrder)
    # 		CRYPTOPP_GET_FUNCTION_ENTRY(SubgroupGenerator)
    # 		;
    # void XTR_DH::AssignFrom(const NameValuePairs &source)
    # 	AssignFromHelper(this, source)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(Modulus)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(SubgroupOrder)
    # 		CRYPTOPP_SET_FUNCTION_ENTRY(SubgroupGenerator)
    # 		;
    # void XTR_DH::GeneratePrivateKey(RandomNumberGenerator &rng, byte *privateKey) const
    # 	x.Encode(privateKey, PrivateKeyLength());
    # void XTR_DH::GeneratePublicKey(RandomNumberGenerator &rng, const byte *privateKey, byte *publicKey) const
    # 	Integer x(privateKey, PrivateKeyLength());
    # 	GFP2Element y = XTR_Exponentiate(m_g, x, m_p);
    # 	y.Encode(publicKey, PublicKeyLength());
    # bool XTR_DH::Agree(byte *agreedValue, const byte *privateKey, const byte *otherPublicKey, bool validateOtherPublicKey) const
    # 	GFP2Element w(otherPublicKey, PublicKeyLength());
    # 	if (validateOtherPublicKey)
    # 		GFP2_ONB<ModularArithmetic> gfp2(m_p);
    # 		GFP2Element three = gfp2.ConvertIn(3);
    # 		if (w.c1.IsNegative() || w.c2.IsNegative() || w.c1 >= m_p || w.c2 >= m_p || w == three)
    # 			return false;
    # 		if (XTR_Exponentiate(w, m_q, m_p) != three)
    # 			return false;
    # 	Integer s(privateKey, PrivateKeyLength());
    # 	GFP2Element z = XTR_Exponentiate(w, s, m_p);
    # 	z.Encode(agreedValue, AgreedValueLength());
    # 	return true;
    # NAMESPACE_END