"""
arc4.py - Python conversion of arc4.h and arc4.cpp

Original C++ header content:
#ifndef CRYPTOPP_ARC4_H
#define CRYPTOPP_ARC4_H

#include "strciphr.h"

NAMESPACE_BEGIN(CryptoPP)

namespace Weak1 {

//! _
class CRYPTOPP_NO_VTABLE ARC4_Base : public VariableKeyLength<16, 1, 256>, public RandomNumberGenerator, public SymmetricCipher, public SymmetricCipherDocumentation
{
public:
	~ARC4_Base();

	static const char *StaticAlgorithmName() {return "ARC4";}

	void GenerateBlock(byte *output, size_t size);
	void DiscardBytes(size_t n);

    void ProcessData(byte *outString, const byte *inString, size_t length);
	
	bool IsRandomAccess() const {return false;}
	bool IsSelfInverting() const {return true;}
	bool IsForwardTransformation() const {return true;}

	typedef SymmetricCipherFinal<ARC4_Base> Encryption;
	typedef SymmetricCipherFinal<ARC4_Base> Decryption;

protected:
	void UncheckedSetKey(const byte *key, unsigned int length, const NameValuePairs &params);
	virtual unsigned int GetDefaultDiscardBytes() const {return 0;}

    FixedSizeSecBlock<byte, 256> m_state;
    byte m_x, m_y;
};

//! <a href="http://www.weidai.com/scan-mirror/cs.html#RC4">Alleged RC4</a>
DOCUMENTED_TYPEDEF(SymmetricCipherFinal<ARC4_Base>, ARC4)

//! _
class CRYPTOPP_NO_VTABLE MARC4_Base : public ARC4_Base
{
public:
	static const char *StaticAlgorithmName() {return "MARC4";}

	typedef SymmetricCipherFinal<MARC4_Base> Encryption;
	typedef SymmetricCipherFinal<MARC4_Base> Decryption;

protected:
	unsigned int GetDefaultDiscardBytes() const {return 256;}
};

//! Modified ARC4: it discards the first 256 bytes of keystream which may be weaker than the rest
DOCUMENTED_TYPEDEF(SymmetricCipherFinal<MARC4_Base>, MARC4)

}
#if CRYPTOPP_ENABLE_NAMESPACE_WEAK >= 1
namespace Weak {using namespace Weak1;}		// import Weak1 into CryptoPP::Weak
#else
using namespace Weak1;	// import Weak1 into CryptoPP with warning
#ifdef __GNUC__
#warning "You may be using a weak algorithm that has been retained for backwards compatibility. Please '#define CRYPTOPP_ENABLE_NAMESPACE_WEAK 1' before including this .h file and prepend the class name with 'Weak::' to remove this warning."
#else
#pragma message("You may be using a weak algorithm that has been retained for backwards compatibility. Please '#define CRYPTOPP_ENABLE_NAMESPACE_WEAK 1' before including this .h file and prepend the class name with 'Weak::' to remove this warning.")
#endif
#endif

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class ARC4_Base:
    # // arc4.cpp - written and placed in the public domain by Wei Dai
    # // The ARC4 algorithm was first revealed in an anonymous email to the
    # // cypherpunks mailing list. This file originally contained some
    # // code copied from this email. The code has since been rewritten in order
    # // to clarify the copyright status of this file. It should now be
    # // completely in the public domain.
    # #define CRYPTOPP_ENABLE_NAMESPACE_WEAK 1
    # NAMESPACE_BEGIN(CryptoPP)
    # namespace Weak1 {
    # void ARC4_TestInstantiations()
    # 	ARC4 x;
    # ARC4_Base::~ARC4_Base()
    # 	m_x = m_y = 0;
    # void ARC4_Base::UncheckedSetKey(const byte *key, unsigned int keyLen, const NameValuePairs &params)
    # 	AssertValidKeyLength(keyLen);
    # 	m_x = 1;
    # 	m_y = 0;
    # 	unsigned int i;
    # 	for (i=0; i<256; i++)
    # 		m_state[i] = i;
    # 	unsigned int keyIndex = 0, stateIndex = 0;
    # 	for (i=0; i<256; i++)
    # 		unsigned int a = m_state[i];
    # 		stateIndex += key[keyIndex] + a;
    # 		stateIndex &= 0xff;
    # 		m_state[i] = m_state[stateIndex];
    # 		m_state[stateIndex] = a;
    # 		if (++keyIndex >= keyLen)
    # 			keyIndex = 0;
    # 	int discardBytes = params.GetIntValueWithDefault("DiscardBytes", GetDefaultDiscardBytes());
    # 	DiscardBytes(discardBytes);
    # template <class T>
    # static inline unsigned int MakeByte(T &x, T &y, byte *s)
    # 	unsigned int a = s[x];
    # 	y = (y+a) & 0xff;
    # 	unsigned int b = s[y];
    # 	s[x] = b;
    # 	s[y] = a;
    # 	x = (x+1) & 0xff;
    # 	return s[(a+b) & 0xff];
    # void ARC4_Base::GenerateBlock(byte *output, size_t size)
    # 	while (size--)
    # 		*output++ = MakeByte(m_x, m_y, m_state);
    # void ARC4_Base::ProcessData(byte *outString, const byte *inString, size_t length)
    # 	if (length == 0)
    # 		return;
    # 	byte *const s = m_state;
    # 	unsigned int x = m_x;
    # 	unsigned int y = m_y;
    # 	if (inString == outString)
    # 		do
    # 			*outString++ ^= MakeByte(x, y, s);
    # 		} while (--length);
    # 	else
    # 		do
    # 			*outString++ = *inString++ ^ MakeByte(x, y, s);
    # 		while(--length);
    # 	m_x = x;
    # 	m_y = y;
    # void ARC4_Base::DiscardBytes(size_t length)
    # 	if (length == 0)
    # 		return;
    # 	byte *const s = m_state;
    # 	unsigned int x = m_x;
    # 	unsigned int y = m_y;
    # 	do
    # 		MakeByte(x, y, s);
    # 	while(--length);
    # 	m_x = x;
    # 	m_y = y;
    # NAMESPACE_END