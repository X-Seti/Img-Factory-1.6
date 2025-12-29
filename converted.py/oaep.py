"""
oaep.py - Python conversion of oaep.h and oaep.cpp

Original C++ header content:
#ifndef CRYPTOPP_OAEP_H
#define CRYPTOPP_OAEP_H

#include "pubkey.h"
#include "sha.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
class CRYPTOPP_DLL OAEP_Base : public PK_EncryptionMessageEncodingMethod
{
public:
	bool ParameterSupported(const char *name) const {return strcmp(name, Name::EncodingParameters()) == 0;}
	size_t MaxUnpaddedLength(size_t paddedLength) const;
	void Pad(RandomNumberGenerator &rng, const byte *raw, size_t inputLength, byte *padded, size_t paddedLength, const NameValuePairs &parameters) const;
	DecodingResult Unpad(const byte *padded, size_t paddedLength, byte *raw, const NameValuePairs &parameters) const;

protected:
	virtual unsigned int DigestSize() const =0;
	virtual HashTransformation * NewHash() const =0;
	virtual MaskGeneratingFunction * NewMGF() const =0;
};

//! <a href="http://www.weidai.com/scan-mirror/ca.html#cem_OAEP-MGF1">EME-OAEP</a>, for use with classes derived from TF_ES
template <class H, class MGF=P1363_MGF1>
class OAEP : public OAEP_Base, public EncryptionStandard
{
public:
	static std::string CRYPTOPP_API StaticAlgorithmName() {return std::string("OAEP-") + MGF::StaticAlgorithmName() + "(" + H::StaticAlgorithmName() + ")";}
	typedef OAEP<H, MGF> EncryptionMessageEncodingMethod;

protected:
	unsigned int DigestSize() const {return H::DIGESTSIZE;}
	HashTransformation * NewHash() const {return new H;}
	MaskGeneratingFunction * NewMGF() const {return new MGF;}
};

CRYPTOPP_DLL_TEMPLATE_CLASS OAEP<SHA>;

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class OAEP_Base:
    # // oaep.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # // ********************************************************
    # size_t OAEP_Base::MaxUnpaddedLength(size_t paddedLength) const
    # 	return SaturatingSubtract(paddedLength/8, 1+2*DigestSize());
    # void OAEP_Base::Pad(RandomNumberGenerator &rng, const byte *input, size_t inputLength, byte *oaepBlock, size_t oaepBlockLen, const NameValuePairs &parameters) const
    # 	assert (inputLength <= MaxUnpaddedLength(oaepBlockLen));
    # 	// convert from bit length to byte length
    # 	if (oaepBlockLen % 8 != 0)
    # 		oaepBlock[0] = 0;
    # 		oaepBlock++;
    # 	oaepBlockLen /= 8;
    # 	const size_t hLen = pHash->DigestSize();
    # 	const size_t seedLen = hLen, dbLen = oaepBlockLen-seedLen;
    # 	byte *const maskedSeed = oaepBlock;
    # 	byte *const maskedDB = oaepBlock+seedLen;
    # 	ConstByteArrayParameter encodingParameters;
    # 	// DB = pHash || 00 ... || 01 || M
    # 	pHash->CalculateDigest(maskedDB, encodingParameters.begin(), encodingParameters.size());
    # 	memset(maskedDB+hLen, 0, dbLen-hLen-inputLength-1);
    # 	maskedDB[dbLen-inputLength-1] = 0x01;
    # 	memcpy(maskedDB+dbLen-inputLength, input, inputLength);
    # 	rng.GenerateBlock(maskedSeed, seedLen);
    # 	pMGF->GenerateAndMask(*pHash, maskedDB, dbLen, maskedSeed, seedLen);
    # 	pMGF->GenerateAndMask(*pHash, maskedSeed, seedLen, maskedDB, dbLen);
    # DecodingResult OAEP_Base::Unpad(const byte *oaepBlock, size_t oaepBlockLen, byte *output, const NameValuePairs &parameters) const
    # 	bool invalid = false;
    # 	// convert from bit length to byte length
    # 	if (oaepBlockLen % 8 != 0)
    # 		invalid = (oaepBlock[0] != 0) || invalid;
    # 		oaepBlock++;
    # 	oaepBlockLen /= 8;
    # 	const size_t hLen = pHash->DigestSize();
    # 	const size_t seedLen = hLen, dbLen = oaepBlockLen-seedLen;
    # 	invalid = (oaepBlockLen < 2*hLen+1) || invalid;
    # 	SecByteBlock t(oaepBlock, oaepBlockLen);
    # 	byte *const maskedSeed = t;
    # 	byte *const maskedDB = t+seedLen;
    # 	pMGF->GenerateAndMask(*pHash, maskedSeed, seedLen, maskedDB, dbLen);
    # 	pMGF->GenerateAndMask(*pHash, maskedDB, dbLen, maskedSeed, seedLen);
    # 	ConstByteArrayParameter encodingParameters;
    # 	// DB = pHash' || 00 ... || 01 || M
    # 	invalid = (M == maskedDB+dbLen) || invalid;
    # 	invalid = !pHash->VerifyDigest(maskedDB, encodingParameters.begin(), encodingParameters.size()) || invalid;
    # 	if (invalid)
    # 		return DecodingResult();
    # 	M++;
    # 	memcpy(output, M, maskedDB+dbLen-M);
    # 	return DecodingResult(maskedDB+dbLen-M);
    # NAMESPACE_END
    # #endif