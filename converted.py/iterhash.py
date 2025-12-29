"""
iterhash.py - Python conversion of iterhash.h and iterhash.cpp

Original C++ header content:
#ifndef CRYPTOPP_ITERHASH_H
#define CRYPTOPP_ITERHASH_H

#include "cryptlib.h"
#include "secblock.h"
#include "misc.h"
#include "simple.h"

NAMESPACE_BEGIN(CryptoPP)

//! exception thrown when trying to hash more data than is allowed by a hash function
class CRYPTOPP_DLL HashInputTooLong : public InvalidDataFormat
{
public:
	explicit HashInputTooLong(const std::string &alg)
		: InvalidDataFormat("IteratedHashBase: input data exceeds maximum allowed by hash function " + alg) {}
};

//! _
template <class T, class BASE>
class CRYPTOPP_NO_VTABLE IteratedHashBase : public BASE
{
public:
	typedef T HashWordType;

	IteratedHashBase() : m_countLo(0), m_countHi(0) {}
	unsigned int OptimalBlockSize() const {return this->BlockSize();}
	unsigned int OptimalDataAlignment() const {return GetAlignmentOf<T>();}
	void Update(const byte *input, size_t length);
	byte * CreateUpdateSpace(size_t &size);
	void Restart();
	void TruncatedFinal(byte *digest, size_t size);

protected:
	inline T GetBitCountHi() const {return (m_countLo >> (8*sizeof(T)-3)) + (m_countHi << 3);}
	inline T GetBitCountLo() const {return m_countLo << 3;}

	void PadLastBlock(unsigned int lastBlockSize, byte padFirst=0x80);
	virtual void Init() =0;

	virtual ByteOrder GetByteOrder() const =0;
	virtual void HashEndianCorrectedBlock(const HashWordType *data) =0;
	virtual size_t HashMultipleBlocks(const T *input, size_t length);
	void HashBlock(const HashWordType *input) {HashMultipleBlocks(input, this->BlockSize());}

	virtual T* DataBuf() =0;
	virtual T* StateBuf() =0;

private:
	T m_countLo, m_countHi;
};

//! _
template <class T_HashWordType, class T_Endianness, unsigned int T_BlockSize, class T_Base = HashTransformation>
class CRYPTOPP_NO_VTABLE IteratedHash : public IteratedHashBase<T_HashWordType, T_Base>
{
public:
	typedef T_Endianness ByteOrderClass;
	typedef T_HashWordType HashWordType;

	CRYPTOPP_CONSTANT(BLOCKSIZE = T_BlockSize)
	// BCB2006 workaround: can't use BLOCKSIZE here
	CRYPTOPP_COMPILE_ASSERT((T_BlockSize & (T_BlockSize - 1)) == 0);	// blockSize is a power of 2
	unsigned int BlockSize() const {return T_BlockSize;}

	ByteOrder GetByteOrder() const {return T_Endianness::ToEnum();}

	inline static void CorrectEndianess(HashWordType *out, const HashWordType *in, size_t byteCount)
	{
		ConditionalByteReverse(T_Endianness::ToEnum(), out, in, byteCount);
	}

protected:
	T_HashWordType* DataBuf() {return this->m_data;}
	FixedSizeSecBlock<T_HashWordType, T_BlockSize/sizeof(T_HashWordType)> m_data;
};

//! _
template <class T_HashWordType, class T_Endianness, unsigned int T_BlockSize, unsigned int T_StateSize, class T_Transform, unsigned int T_DigestSize = 0, bool T_StateAligned = false>
class CRYPTOPP_NO_VTABLE IteratedHashWithStaticTransform
	: public ClonableImpl<T_Transform, AlgorithmImpl<IteratedHash<T_HashWordType, T_Endianness, T_BlockSize>, T_Transform> >
{
public:
	CRYPTOPP_CONSTANT(DIGESTSIZE = T_DigestSize ? T_DigestSize : T_StateSize)
	unsigned int DigestSize() const {return DIGESTSIZE;};

protected:
	IteratedHashWithStaticTransform() {this->Init();}
	void HashEndianCorrectedBlock(const T_HashWordType *data) {T_Transform::Transform(this->m_state, data);}
	void Init() {T_Transform::InitState(this->m_state);}

	T_HashWordType* StateBuf() {return this->m_state;}
	FixedSizeAlignedSecBlock<T_HashWordType, T_BlockSize/sizeof(T_HashWordType), T_StateAligned> m_state;
};

#ifndef __GNUC__
	CRYPTOPP_DLL_TEMPLATE_CLASS IteratedHashBase<word64, HashTransformation>;
	CRYPTOPP_STATIC_TEMPLATE_CLASS IteratedHashBase<word64, MessageAuthenticationCode>;

	CRYPTOPP_DLL_TEMPLATE_CLASS IteratedHashBase<word32, HashTransformation>;
	CRYPTOPP_STATIC_TEMPLATE_CLASS IteratedHashBase<word32, MessageAuthenticationCode>;
#endif

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class iterhash:
    # // iterhash.cpp - written and placed in the public domain by Wei Dai
    # #ifndef __GNUC__
    # #define CRYPTOPP_MANUALLY_INSTANTIATE_TEMPLATES
    # #endif
    # NAMESPACE_BEGIN(CryptoPP)
    # template <class T, class BASE> void IteratedHashBase<T, BASE>::Update(const byte *input, size_t len)
    # 	HashWordType oldCountLo = m_countLo, oldCountHi = m_countHi;
    # 	if ((m_countLo = oldCountLo + HashWordType(len)) < oldCountLo)
    # 		m_countHi++;             // carry from low to high
    # 	m_countHi += (HashWordType)SafeRightShift<8*sizeof(HashWordType)>(len);
    # 	if (m_countHi < oldCountHi || SafeRightShift<2*8*sizeof(HashWordType)>(len) != 0)
    # 		throw HashInputTooLong(this->AlgorithmName());
    # 	unsigned int blockSize = this->BlockSize();
    # 	unsigned int num = ModPowerOf2(oldCountLo, blockSize);
    # 	T* dataBuf = this->DataBuf();
    # 	byte* data = (byte *)dataBuf;
    # 	if (num != 0)	// process left over data
    # 		if (num+len >= blockSize)
    # 			memcpy(data+num, input, blockSize-num);
    # 			HashBlock(dataBuf);
    # 			input += (blockSize-num);
    # 			len -= (blockSize-num);
    # 			num = 0;
    # 			// drop through and do the rest
    # 		else
    # 			memcpy(data+num, input, len);
    # 			return;
    # 	// now process the input data in blocks of blockSize bytes and save the leftovers to m_data
    # 	if (len >= blockSize)
    # 		if (input == data)
    # 			assert(len == blockSize);
    # 			HashBlock(dataBuf);
    # 			return;
    # 		else if (IsAligned<T>(input))
    # 			size_t leftOver = HashMultipleBlocks((T *)input, len);
    # 			input += (len - leftOver);
    # 			len = leftOver;
    # 		else
    # 			do
    # 				memcpy(data, input, blockSize);
    # 				HashBlock(dataBuf);
    # 				input+=blockSize;
    # 				len-=blockSize;
    # 			} while (len >= blockSize);
    # 	if (len && data != input)
    # 		memcpy(data, input, len);
    # template <class T, class BASE> byte * IteratedHashBase<T, BASE>::CreateUpdateSpace(size_t &size)
    # 	unsigned int blockSize = this->BlockSize();
    # 	unsigned int num = ModPowerOf2(m_countLo, blockSize);
    # 	size = blockSize - num;
    # 	return (byte *)DataBuf() + num;
    # template <class T, class BASE> size_t IteratedHashBase<T, BASE>::HashMultipleBlocks(const T *input, size_t length)
    # 	unsigned int blockSize = this->BlockSize();
    # 	bool noReverse = NativeByteOrderIs(this->GetByteOrder());
    # 	T* dataBuf = this->DataBuf();
    # 	do
    # 		if (noReverse)
    # 			this->HashEndianCorrectedBlock(input);
    # 		else
    # 			ByteReverse(dataBuf, input, this->BlockSize());
    # 			this->HashEndianCorrectedBlock(dataBuf);
    # 		input += blockSize/sizeof(T);
    # 		length -= blockSize;
    # 	while (length >= blockSize);
    # 	return length;
    # template <class T, class BASE> void IteratedHashBase<T, BASE>::PadLastBlock(unsigned int lastBlockSize, byte padFirst)
    # 	unsigned int blockSize = this->BlockSize();
    # 	unsigned int num = ModPowerOf2(m_countLo, blockSize);
    # 	T* dataBuf = this->DataBuf();
    # 	byte* data = (byte *)dataBuf;
    # 	data[num++] = padFirst;
    # 	if (num <= lastBlockSize)
    # 		memset(data+num, 0, lastBlockSize-num);
    # 	else
    # 		memset(data+num, 0, blockSize-num);
    # 		HashBlock(dataBuf);
    # 		memset(data, 0, lastBlockSize);
    # template <class T, class BASE> void IteratedHashBase<T, BASE>::Restart()
    # 	m_countLo = m_countHi = 0;
    # 	Init();
    # template <class T, class BASE> void IteratedHashBase<T, BASE>::TruncatedFinal(byte *digest, size_t size)
    # 	this->ThrowIfInvalidTruncatedSize(size);
    # 	T* dataBuf = this->DataBuf();
    # 	T* stateBuf = this->StateBuf();
    # 	unsigned int blockSize = this->BlockSize();
    # 	ByteOrder order = this->GetByteOrder();
    # 	PadLastBlock(blockSize - 2*sizeof(HashWordType));
    # 	dataBuf[blockSize/sizeof(T)-2+order] = ConditionalByteReverse(order, this->GetBitCountLo());
    # 	dataBuf[blockSize/sizeof(T)-1-order] = ConditionalByteReverse(order, this->GetBitCountHi());
    # 	HashBlock(dataBuf);
    # 	if (IsAligned<HashWordType>(digest) && size%sizeof(HashWordType)==0)
    # 		ConditionalByteReverse<HashWordType>(order, (HashWordType *)digest, stateBuf, size);
    # 	else
    # 		ConditionalByteReverse<HashWordType>(order, stateBuf, stateBuf, this->DigestSize());
    # 		memcpy(digest, stateBuf, size);
    # 	this->Restart();		// reinit for next use
    # #ifdef __GNUC__
    # 	template class IteratedHashBase<word64, HashTransformation>;
    # 	template class IteratedHashBase<word64, MessageAuthenticationCode>;
    # 	template class IteratedHashBase<word32, HashTransformation>;
    # 	template class IteratedHashBase<word32, MessageAuthenticationCode>;
    # #endif
    # NAMESPACE_END