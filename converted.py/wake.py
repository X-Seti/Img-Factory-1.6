"""
wake.py - Python conversion of wake.h and wake.cpp

Original C++ header content:
#ifndef CRYPTOPP_WAKE_H
#define CRYPTOPP_WAKE_H

#include "seckey.h"
#include "secblock.h"
#include "strciphr.h"

NAMESPACE_BEGIN(CryptoPP)

//! _
template <class B = BigEndian>
struct WAKE_OFB_Info : public FixedKeyLength<32>
{
	static const char *StaticAlgorithmName() {return B::ToEnum() == LITTLE_ENDIAN_ORDER ? "WAKE-OFB-LE" : "WAKE-OFB-BE";}
};

class CRYPTOPP_NO_VTABLE WAKE_Base
{
protected:
	word32 M(word32 x, word32 y);
	void GenKey(word32 k0, word32 k1, word32 k2, word32 k3);

	word32 t[257];
	word32 r3, r4, r5, r6;
};

template <class B = BigEndian>
class CRYPTOPP_NO_VTABLE WAKE_Policy : public AdditiveCipherConcretePolicy<word32, 1, 64>, protected WAKE_Base
{
protected:
	void CipherSetKey(const NameValuePairs &params, const byte *key, size_t length);
	// OFB
	void OperateKeystream(KeystreamOperation operation, byte *output, const byte *input, size_t iterationCount);
	bool CipherIsRandomAccess() const {return false;}
};

//! WAKE-OFB
template <class B = BigEndian>
struct WAKE_OFB : public WAKE_OFB_Info<B>, public SymmetricCipherDocumentation
{
	typedef SymmetricCipherFinal<ConcretePolicyHolder<WAKE_Policy<B>, AdditiveCipherTemplate<> >,  WAKE_OFB_Info<B> > Encryption;
	typedef Encryption Decryption;
};

/*
template <class B = BigEndian>
class WAKE_ROFB_Policy : public WAKE_Policy<B>
{
protected:
	void Iterate(KeystreamOperation operation, byte *output, const byte *input, unsigned int iterationCount);
};

template <class B = BigEndian>
struct WAKE_ROFB : public WAKE_Info<B>
{
	typedef SymmetricCipherTemplate<ConcretePolicyHolder<AdditiveCipherTemplate<>, WAKE_ROFB_Policy<B> > > Encryption;
	typedef Encryption Decryption;
};
*/

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class WAKE_Base:
    # // wake.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # void WAKE_TestInstantiations()
    # inline word32 WAKE_Base::M(word32 x, word32 y)
    # 	word32 w = x+y;
    # 	return (w>>8) ^ t[w & 0xff];
    # void WAKE_Base::GenKey(word32 k0, word32 k1, word32 k2, word32 k3)
    # 	// this code is mostly copied from David Wheeler's paper "A Bulk Data Encryption Algorithm"
    # 	signed int x, z, p;	
    # 	// x and z were declared as "long" in Wheeler's paper, which is a signed type. I don't know if that was intentional, but it's too late to change it now. -- Wei 7/4/2010
    # 	CRYPTOPP_COMPILE_ASSERT(sizeof(x) == 4);
    # 	static int tt[10]= {
    # 		0x726a8f3b,								 // table
    # 		0xe69a3b5c,
    # 		0xd3c71fe5,
    # 		0xab3c73d2,
    # 		0x4d3a8eb3,
    # 		0x0396d6e8,
    # 		0x3d4c2f7a,
    # 		0x9ee27cf3, } ;
    # 	t[0] = k0;
    # 	t[1] = k1;
    # 	t[2] = k2;
    # 	t[3] = k3;
    # 	for (p=4 ; p<256 ; p++)
    # 	  x=t[p-4]+t[p-1] ; 					   // fill t
    # 	  t[p]= (x>>3) ^ tt[x&7] ;
    # 	for (p=0 ; p<23 ; p++)
    # 		t[p]+=t[p+89] ; 		  // mix first entries
    # 	x=t[33] ; z=t[59] | 0x01000001 ;
    # 	z=z&0xff7fffff ;
    # 	for (p=0 ; p<256 ; p++) {		//change top byte to
    # 	  x=(x&0xff7fffff)+z ; 		 // a permutation etc
    # 	  t[p]=(t[p] & 0x00ffffff) ^ x ; }
    # 	t[256]=t[0] ;
    # 	byte y=byte(x);
    # 	for (p=0 ; p<256 ; p++) {	  // further change perm.
    # 	  t[p]=t[y=byte(t[p^y]^y)] ;  // and other digits
    # 	  t[y]=t[p+1] ;  }
    # template <class B>
    # void WAKE_Policy<B>::CipherSetKey(const NameValuePairs &params, const byte *key, size_t length)
    # 	word32 k0, k1, k2, k3;
    # 	GenKey(k0, k1, k2, k3);
    # // OFB
    # template <class B>
    # void WAKE_Policy<B>::OperateKeystream(KeystreamOperation operation, byte *output, const byte *input, size_t iterationCount)
    # #define WAKE_OUTPUT(x)\
    # 	while (iterationCount--)\
    # 		CRYPTOPP_KEYSTREAM_OUTPUT_WORD(x, B::ToEnum(), 0, r6);\
    # 		r3 = M(r3, r6);\
    # 		r4 = M(r4, r3);\
    # 		r5 = M(r5, r4);\
    # 		r6 = M(r6, r5);\
    # 		output += 4;\
    # 		if (!(x & INPUT_NULL))\
    # 			input += 4;\
    # 	typedef word32 WordType;
    # 	CRYPTOPP_KEYSTREAM_OUTPUT_SWITCH(WAKE_OUTPUT, 0);
    # /*
    # template <class B>
    # void WAKE_ROFB_Policy<B>::Iterate(KeystreamOperation operation, byte *output, const byte *input, unsigned int iterationCount)
    # 	KeystreamOutput<B> keystreamOperation(operation, output, input);
    # 	while (iterationCount--)
    # 		keystreamOperation(r6);
    # 		r3 = M(r3, r6);
    # 		r4 = M(r4, r3);
    # 		r5 = M(r5, r4);
    # 		r6 = M(r6, r5);
    # */
    # template class WAKE_Policy<BigEndian>;
    # template class WAKE_Policy<LittleEndian>;
    # //template class WAKE_ROFB_Policy<BigEndian>;
    # //template class WAKE_ROFB_Policy<LittleEndian>;
    # NAMESPACE_END