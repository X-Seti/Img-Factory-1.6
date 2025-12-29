"""
dh2.py - Python conversion of dh2.h and dh2.cpp

Original C++ header content:
#ifndef CRYPTOPP_DH2_H
#define CRYPTOPP_DH2_H

/** \file
*/

#include "cryptlib.h"

NAMESPACE_BEGIN(CryptoPP)

/// <a href="http://www.weidai.com/scan-mirror/ka.html#DH2">Unified Diffie-Hellman</a>
class DH2 : public AuthenticatedKeyAgreementDomain
{
public:
	DH2(SimpleKeyAgreementDomain &domain)
		: d1(domain), d2(domain) {}
	DH2(SimpleKeyAgreementDomain &staticDomain, SimpleKeyAgreementDomain &ephemeralDomain)
		: d1(staticDomain), d2(ephemeralDomain) {}

	CryptoParameters & AccessCryptoParameters() {return d1.AccessCryptoParameters();}

	unsigned int AgreedValueLength() const
		{return d1.AgreedValueLength() + d2.AgreedValueLength();}

	unsigned int StaticPrivateKeyLength() const
		{return d1.PrivateKeyLength();}
	unsigned int StaticPublicKeyLength() const
		{return d1.PublicKeyLength();}
	void GenerateStaticPrivateKey(RandomNumberGenerator &rng, byte *privateKey) const
		{d1.GeneratePrivateKey(rng, privateKey);}
	void GenerateStaticPublicKey(RandomNumberGenerator &rng, const byte *privateKey, byte *publicKey) const
		{d1.GeneratePublicKey(rng, privateKey, publicKey);}
	void GenerateStaticKeyPair(RandomNumberGenerator &rng, byte *privateKey, byte *publicKey) const
		{d1.GenerateKeyPair(rng, privateKey, publicKey);}

	unsigned int EphemeralPrivateKeyLength() const
		{return d2.PrivateKeyLength();}
	unsigned int EphemeralPublicKeyLength() const
		{return d2.PublicKeyLength();}
	void GenerateEphemeralPrivateKey(RandomNumberGenerator &rng, byte *privateKey) const
		{d2.GeneratePrivateKey(rng, privateKey);}
	void GenerateEphemeralPublicKey(RandomNumberGenerator &rng, const byte *privateKey, byte *publicKey) const
		{d2.GeneratePublicKey(rng, privateKey, publicKey);}
	void GenerateEphemeralKeyPair(RandomNumberGenerator &rng, byte *privateKey, byte *publicKey) const
		{d2.GenerateKeyPair(rng, privateKey, publicKey);}

	bool Agree(byte *agreedValue,
		const byte *staticPrivateKey, const byte *ephemeralPrivateKey, 
		const byte *staticOtherPublicKey, const byte *ephemeralOtherPublicKey,
		bool validateStaticOtherPublicKey=true) const;

protected:
	SimpleKeyAgreementDomain &d1, &d2;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class DH2:
    # // dh2.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # void DH2_TestInstantiations()
    # 	DH2 dh(*(SimpleKeyAgreementDomain*)NULL);
    # bool DH2::Agree(byte *agreedValue,
    # 		const byte *staticSecretKey, const byte *ephemeralSecretKey, 
    # 		const byte *staticOtherPublicKey, const byte *ephemeralOtherPublicKey,
    # 		bool validateStaticOtherPublicKey) const
    # 	return d1.Agree(agreedValue, staticSecretKey, staticOtherPublicKey, validateStaticOtherPublicKey)
    # 		&& d2.Agree(agreedValue+d1.AgreedValueLength(), ephemeralSecretKey, ephemeralOtherPublicKey, true);
    # NAMESPACE_END