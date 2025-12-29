"""
pkcspad.py - Python conversion of pkcspad.h and pkcspad.cpp

Original C++ header content:
#ifndef CRYPTOPP_PKCSPAD_H
#define CRYPTOPP_PKCSPAD_H

#include "cryptlib.h"
#include "pubkey.h"

#ifdef CRYPTOPP_IS_DLL
#include "sha.h"
#endif

NAMESPACE_BEGIN(CryptoPP)

//! <a href="http://www.weidai.com/scan-mirror/ca.html#cem_PKCS1-1.5">EME-PKCS1-v1_5</a>
class PKCS_EncryptionPaddingScheme : public PK_EncryptionMessageEncodingMethod
{
public:
	static const char * StaticAlgorithmName() {return "EME-PKCS1-v1_5";}

	size_t MaxUnpaddedLength(size_t paddedLength) const;
	void Pad(RandomNumberGenerator &rng, const byte *raw, size_t inputLength, byte *padded, size_t paddedLength, const NameValuePairs &parameters) const;
	DecodingResult Unpad(const byte *padded, size_t paddedLength, byte *raw, const NameValuePairs &parameters) const;
};

template <class H> class PKCS_DigestDecoration
{
public:
	static const byte decoration[];
	static const unsigned int length;
};

// PKCS_DigestDecoration can be instantiated with the following
// classes as specified in PKCS#1 v2.0 and P1363a
class SHA1;
class RIPEMD160;
class Tiger;
class SHA224;
class SHA256;
class SHA384;
class SHA512;
namespace Weak1 {
class MD2;
class MD5;
}
// end of list

#ifdef CRYPTOPP_IS_DLL
CRYPTOPP_DLL_TEMPLATE_CLASS PKCS_DigestDecoration<SHA1>;
CRYPTOPP_DLL_TEMPLATE_CLASS PKCS_DigestDecoration<SHA224>;
CRYPTOPP_DLL_TEMPLATE_CLASS PKCS_DigestDecoration<SHA256>;
CRYPTOPP_DLL_TEMPLATE_CLASS PKCS_DigestDecoration<SHA384>;
CRYPTOPP_DLL_TEMPLATE_CLASS PKCS_DigestDecoration<SHA512>;
#endif

//! <a href="http://www.weidai.com/scan-mirror/sig.html#sem_PKCS1-1.5">EMSA-PKCS1-v1_5</a>
class CRYPTOPP_DLL PKCS1v15_SignatureMessageEncodingMethod : public PK_DeterministicSignatureMessageEncodingMethod
{
public:
	static const char * CRYPTOPP_API StaticAlgorithmName() {return "EMSA-PKCS1-v1_5";}

	size_t MinRepresentativeBitLength(size_t hashIdentifierSize, size_t digestSize) const
		{return 8 * (digestSize + hashIdentifierSize + 10);}

	void ComputeMessageRepresentative(RandomNumberGenerator &rng, 
		const byte *recoverableMessage, size_t recoverableMessageLength,
		HashTransformation &hash, HashIdentifier hashIdentifier, bool messageEmpty,
		byte *representative, size_t representativeBitLength) const;

	struct HashIdentifierLookup
	{
		template <class H> struct HashIdentifierLookup2
		{
			static HashIdentifier Lookup()
			{
				return HashIdentifier(PKCS_DigestDecoration<H>::decoration, PKCS_DigestDecoration<H>::length);
			}
		};
	};
};

//! PKCS #1 version 1.5, for use with RSAES and RSASS
/*! Only the following hash functions are supported by this signature standard:
	\dontinclude pkcspad.h
	\skip can be instantiated
	\until end of list
*/
struct PKCS1v15 : public SignatureStandard, public EncryptionStandard
{
	typedef PKCS_EncryptionPaddingScheme EncryptionMessageEncodingMethod;
	typedef PKCS1v15_SignatureMessageEncodingMethod SignatureMessageEncodingMethod;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class Weak1:
    # // pkcspad.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_PKCSPAD_CPP	// SunCC workaround: compiler could cause this file to be included twice
    # #define CRYPTOPP_PKCSPAD_CPP
    # NAMESPACE_BEGIN(CryptoPP)
    # // more in dll.cpp
    # size_t PKCS_EncryptionPaddingScheme::MaxUnpaddedLength(size_t paddedLength) const
    # 	return SaturatingSubtract(paddedLength/8, 10U);
    # void PKCS_EncryptionPaddingScheme::Pad(RandomNumberGenerator &rng, const byte *input, size_t inputLen, byte *pkcsBlock, size_t pkcsBlockLen, const NameValuePairs &parameters) const
    # 	assert (inputLen <= MaxUnpaddedLength(pkcsBlockLen));	// this should be checked by caller
    # 	// convert from bit length to byte length
    # 	if (pkcsBlockLen % 8 != 0)
    # 		pkcsBlock[0] = 0;
    # 		pkcsBlock++;
    # 	pkcsBlockLen /= 8;
    # 	pkcsBlock[0] = 2;  // block type 2
    # 	// pad with non-zero random bytes
    # 	for (unsigned i = 1; i < pkcsBlockLen-inputLen-1; i++)
    # 		pkcsBlock[i] = (byte)rng.GenerateWord32(1, 0xff);
    # 	pkcsBlock[pkcsBlockLen-inputLen-1] = 0;     // separator
    # 	memcpy(pkcsBlock+pkcsBlockLen-inputLen, input, inputLen);
    # DecodingResult PKCS_EncryptionPaddingScheme::Unpad(const byte *pkcsBlock, size_t pkcsBlockLen, byte *output, const NameValuePairs &parameters) const
    # 	bool invalid = false;
    # 	size_t maxOutputLen = MaxUnpaddedLength(pkcsBlockLen);
    # 	// convert from bit length to byte length
    # 	if (pkcsBlockLen % 8 != 0)
    # 		invalid = (pkcsBlock[0] != 0) || invalid;
    # 		pkcsBlock++;
    # 	pkcsBlockLen /= 8;
    # 	// Require block type 2.
    # 	invalid = (pkcsBlock[0] != 2) || invalid;
    # 	// skip past the padding until we find the separator
    # 	size_t i=1;
    # 	while (i<pkcsBlockLen && pkcsBlock[i++]) { // null body
    # 	assert(i==pkcsBlockLen || pkcsBlock[i-1]==0);
    # 	size_t outputLen = pkcsBlockLen - i;
    # 	invalid = (outputLen > maxOutputLen) || invalid;
    # 	if (invalid)
    # 		return DecodingResult();
    # 	memcpy (output, pkcsBlock+i, outputLen);
    # 	return DecodingResult(outputLen);
    # // ********************************************************
    # #ifndef CRYPTOPP_IMPORTS
    # void PKCS1v15_SignatureMessageEncodingMethod::ComputeMessageRepresentative(RandomNumberGenerator &rng, 
    # 	const byte *recoverableMessage, size_t recoverableMessageLength,
    # 	HashTransformation &hash, HashIdentifier hashIdentifier, bool messageEmpty,
    # 	byte *representative, size_t representativeBitLength) const
    # 	assert(representativeBitLength >= MinRepresentativeBitLength(hashIdentifier.second, hash.DigestSize()));
    # 	size_t pkcsBlockLen = representativeBitLength;
    # 	// convert from bit length to byte length
    # 	if (pkcsBlockLen % 8 != 0)
    # 		representative[0] = 0;
    # 		representative++;
    # 	pkcsBlockLen /= 8;
    # 	representative[0] = 1;   // block type 1
    # 	unsigned int digestSize = hash.DigestSize();
    # 	byte *pPadding = representative + 1;
    # 	byte *pDigest = representative + pkcsBlockLen - digestSize;
    # 	byte *pHashId = pDigest - hashIdentifier.second;
    # 	byte *pSeparator = pHashId - 1;
    # 	// pad with 0xff
    # 	memset(pPadding, 0xff, pSeparator-pPadding);
    # 	*pSeparator = 0;
    # 	memcpy(pHashId, hashIdentifier.first, hashIdentifier.second);
    # 	hash.Final(pDigest);
    # #endif
    # NAMESPACE_END
    # #endif