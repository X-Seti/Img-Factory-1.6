"""
pssr.py - Python conversion of pssr.h and pssr.cpp

Original C++ header content:
#ifndef CRYPTOPP_PSSR_H
#define CRYPTOPP_PSSR_H

#include "pubkey.h"
#include "emsa2.h"

#ifdef CRYPTOPP_IS_DLL
#include "sha.h"
#endif

NAMESPACE_BEGIN(CryptoPP)

class CRYPTOPP_DLL PSSR_MEM_Base : public PK_RecoverableSignatureMessageEncodingMethod
{
	virtual bool AllowRecovery() const =0;
	virtual size_t SaltLen(size_t hashLen) const =0;
	virtual size_t MinPadLen(size_t hashLen) const =0;
	virtual const MaskGeneratingFunction & GetMGF() const =0;

public:
	size_t MinRepresentativeBitLength(size_t hashIdentifierLength, size_t digestLength) const;
	size_t MaxRecoverableLength(size_t representativeBitLength, size_t hashIdentifierLength, size_t digestLength) const;
	bool IsProbabilistic() const;
	bool AllowNonrecoverablePart() const;
	bool RecoverablePartFirst() const;
	void ComputeMessageRepresentative(RandomNumberGenerator &rng, 
		const byte *recoverableMessage, size_t recoverableMessageLength,
		HashTransformation &hash, HashIdentifier hashIdentifier, bool messageEmpty,
		byte *representative, size_t representativeBitLength) const;
	DecodingResult RecoverMessageFromRepresentative(
		HashTransformation &hash, HashIdentifier hashIdentifier, bool messageEmpty,
		byte *representative, size_t representativeBitLength,
		byte *recoverableMessage) const;
};

template <bool USE_HASH_ID> class PSSR_MEM_BaseWithHashId;
template<> class PSSR_MEM_BaseWithHashId<true> : public EMSA2HashIdLookup<PSSR_MEM_Base> {};
template<> class PSSR_MEM_BaseWithHashId<false> : public PSSR_MEM_Base {};

template <bool ALLOW_RECOVERY, class MGF=P1363_MGF1, int SALT_LEN=-1, int MIN_PAD_LEN=0, bool USE_HASH_ID=false>
class PSSR_MEM : public PSSR_MEM_BaseWithHashId<USE_HASH_ID>
{
	virtual bool AllowRecovery() const {return ALLOW_RECOVERY;}
	virtual size_t SaltLen(size_t hashLen) const {return SALT_LEN < 0 ? hashLen : SALT_LEN;}
	virtual size_t MinPadLen(size_t hashLen) const {return MIN_PAD_LEN < 0 ? hashLen : MIN_PAD_LEN;}
	virtual const MaskGeneratingFunction & GetMGF() const {static MGF mgf; return mgf;}

public:
	static std::string CRYPTOPP_API StaticAlgorithmName() {return std::string(ALLOW_RECOVERY ? "PSSR-" : "PSS-") + MGF::StaticAlgorithmName();}
};

//! <a href="http://www.weidai.com/scan-mirror/sig.html#sem_PSSR-MGF1">PSSR-MGF1</a>
struct PSSR : public SignatureStandard
{
	typedef PSSR_MEM<true> SignatureMessageEncodingMethod;
};

//! <a href="http://www.weidai.com/scan-mirror/sig.html#sem_PSS-MGF1">PSS-MGF1</a>
struct PSS : public SignatureStandard
{
	typedef PSSR_MEM<false> SignatureMessageEncodingMethod;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class PSSR_MEM_Base:
    # // pssr.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # // more in dll.cpp
    # #ifndef CRYPTOPP_IMPORTS
    # size_t PSSR_MEM_Base::MinRepresentativeBitLength(size_t hashIdentifierLength, size_t digestLength) const
    # 	size_t saltLen = SaltLen(digestLength);
    # 	size_t minPadLen = MinPadLen(digestLength);
    # 	return 9 + 8*(minPadLen + saltLen + digestLength + hashIdentifierLength);
    # size_t PSSR_MEM_Base::MaxRecoverableLength(size_t representativeBitLength, size_t hashIdentifierLength, size_t digestLength) const
    # 	if (AllowRecovery())
    # 		return SaturatingSubtract(representativeBitLength, MinRepresentativeBitLength(hashIdentifierLength, digestLength)) / 8;
    # 	return 0;
    # bool PSSR_MEM_Base::IsProbabilistic() const 
    # 	return SaltLen(1) > 0;
    # bool PSSR_MEM_Base::AllowNonrecoverablePart() const
    # 	return true;
    # bool PSSR_MEM_Base::RecoverablePartFirst() const
    # 	return false;
    # void PSSR_MEM_Base::ComputeMessageRepresentative(RandomNumberGenerator &rng, 
    # 	const byte *recoverableMessage, size_t recoverableMessageLength,
    # 	HashTransformation &hash, HashIdentifier hashIdentifier, bool messageEmpty,
    # 	byte *representative, size_t representativeBitLength) const
    # 	assert(representativeBitLength >= MinRepresentativeBitLength(hashIdentifier.second, hash.DigestSize()));
    # 	const size_t u = hashIdentifier.second + 1;
    # 	const size_t representativeByteLength = BitsToBytes(representativeBitLength);
    # 	const size_t digestSize = hash.DigestSize();
    # 	const size_t saltSize = SaltLen(digestSize);
    # 	byte *const h = representative + representativeByteLength - u - digestSize;
    # 	SecByteBlock digest(digestSize), salt(saltSize);
    # 	hash.Final(digest);
    # 	rng.GenerateBlock(salt, saltSize);
    # 	// compute H = hash of M'
    # 	byte c[8];
    # 	PutWord(false, BIG_ENDIAN_ORDER, c, (word32)SafeRightShift<29>(recoverableMessageLength));
    # 	PutWord(false, BIG_ENDIAN_ORDER, c+4, word32(recoverableMessageLength << 3));
    # 	hash.Update(c, 8);
    # 	hash.Update(recoverableMessage, recoverableMessageLength);
    # 	hash.Update(digest, digestSize);
    # 	hash.Update(salt, saltSize);
    # 	hash.Final(h);
    # 	// compute representative
    # 	GetMGF().GenerateAndMask(hash, representative, representativeByteLength - u - digestSize, h, digestSize, false);
    # 	byte *xorStart = representative + representativeByteLength - u - digestSize - salt.size() - recoverableMessageLength - 1;
    # 	xorStart[0] ^= 1;
    # 	xorbuf(xorStart + 1, recoverableMessage, recoverableMessageLength);
    # 	xorbuf(xorStart + 1 + recoverableMessageLength, salt, salt.size());
    # 	memcpy(representative + representativeByteLength - u, hashIdentifier.first, hashIdentifier.second);
    # 	representative[representativeByteLength - 1] = hashIdentifier.second ? 0xcc : 0xbc;
    # 	if (representativeBitLength % 8 != 0)
    # 		representative[0] = (byte)Crop(representative[0], representativeBitLength % 8);
    # DecodingResult PSSR_MEM_Base::RecoverMessageFromRepresentative(
    # 	HashTransformation &hash, HashIdentifier hashIdentifier, bool messageEmpty,
    # 	byte *representative, size_t representativeBitLength,
    # 	byte *recoverableMessage) const
    # 	assert(representativeBitLength >= MinRepresentativeBitLength(hashIdentifier.second, hash.DigestSize()));
    # 	const size_t u = hashIdentifier.second + 1;
    # 	const size_t representativeByteLength = BitsToBytes(representativeBitLength);
    # 	const size_t digestSize = hash.DigestSize();
    # 	const size_t saltSize = SaltLen(digestSize);
    # 	const byte *const h = representative + representativeByteLength - u - digestSize;
    # 	SecByteBlock digest(digestSize);
    # 	hash.Final(digest);
    # 	DecodingResult result(0);
    # 	bool &valid = result.isValidCoding;
    # 	size_t &recoverableMessageLength = result.messageLength;
    # 	valid = (representative[representativeByteLength - 1] == (hashIdentifier.second ? 0xcc : 0xbc)) && valid;
    # 	valid = VerifyBufsEqual(representative + representativeByteLength - u, hashIdentifier.first, hashIdentifier.second) && valid;
    # 	GetMGF().GenerateAndMask(hash, representative, representativeByteLength - u - digestSize, h, digestSize);
    # 	if (representativeBitLength % 8 != 0)
    # 		representative[0] = (byte)Crop(representative[0], representativeBitLength % 8);
    # 	// extract salt and recoverableMessage from DB = 00 ... || 01 || M || salt
    # 	byte *salt = representative + representativeByteLength - u - digestSize - saltSize;
    # 	recoverableMessageLength = salt-M-1;
    # 	if (*M == 0x01 
    # 		&& (size_t)(M - representative - (representativeBitLength % 8 != 0)) >= MinPadLen(digestSize)
    # 		&& recoverableMessageLength <= MaxRecoverableLength(representativeBitLength, hashIdentifier.second, digestSize))
    # 		memcpy(recoverableMessage, M+1, recoverableMessageLength);
    # 	else
    # 		recoverableMessageLength = 0;
    # 		valid = false;
    # 	// verify H = hash of M'
    # 	byte c[8];
    # 	PutWord(false, BIG_ENDIAN_ORDER, c, (word32)SafeRightShift<29>(recoverableMessageLength));
    # 	PutWord(false, BIG_ENDIAN_ORDER, c+4, word32(recoverableMessageLength << 3));
    # 	hash.Update(c, 8);
    # 	hash.Update(recoverableMessage, recoverableMessageLength);
    # 	hash.Update(digest, digestSize);
    # 	hash.Update(salt, saltSize);
    # 	valid = hash.Verify(h) && valid;
    # 	if (!AllowRecovery() && valid && recoverableMessageLength != 0)
    # 	return result;
    # #endif
    # NAMESPACE_END