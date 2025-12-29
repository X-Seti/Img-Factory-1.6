"""
authenc.py - Python conversion of authenc.h and authenc.cpp

Original C++ header content:
#ifndef CRYPTOPP_AUTHENC_H
#define CRYPTOPP_AUTHENC_H

#include "cryptlib.h"
#include "secblock.h"

NAMESPACE_BEGIN(CryptoPP)

//! .
class CRYPTOPP_DLL CRYPTOPP_NO_VTABLE AuthenticatedSymmetricCipherBase : public AuthenticatedSymmetricCipher
{
public:
	AuthenticatedSymmetricCipherBase() : m_state(State_Start) {}

	bool IsRandomAccess() const {return false;}
	bool IsSelfInverting() const {return true;}
	void UncheckedSetKey(const byte *,unsigned int,const CryptoPP::NameValuePairs &) {assert(false);}

	void SetKey(const byte *userKey, size_t keylength, const NameValuePairs &params);
	void Restart() {if (m_state > State_KeySet) m_state = State_KeySet;}
	void Resynchronize(const byte *iv, int length=-1);
	void Update(const byte *input, size_t length);
	void ProcessData(byte *outString, const byte *inString, size_t length);
	void TruncatedFinal(byte *mac, size_t macSize);

protected:
	void AuthenticateData(const byte *data, size_t len);
	const SymmetricCipher & GetSymmetricCipher() const {return const_cast<AuthenticatedSymmetricCipherBase *>(this)->AccessSymmetricCipher();};

	virtual SymmetricCipher & AccessSymmetricCipher() =0;
	virtual bool AuthenticationIsOnPlaintext() const =0;
	virtual unsigned int AuthenticationBlockSize() const =0;
	virtual void SetKeyWithoutResync(const byte *userKey, size_t keylength, const NameValuePairs &params) =0;
	virtual void Resync(const byte *iv, size_t len) =0;
	virtual size_t AuthenticateBlocks(const byte *data, size_t len) =0;
	virtual void AuthenticateLastHeaderBlock() =0;
	virtual void AuthenticateLastConfidentialBlock() {}
	virtual void AuthenticateLastFooterBlock(byte *mac, size_t macSize) =0;

	enum State {State_Start, State_KeySet, State_IVSet, State_AuthUntransformed, State_AuthTransformed, State_AuthFooter};
	State m_state;
	unsigned int m_bufferedDataLength;
	lword m_totalHeaderLength, m_totalMessageLength, m_totalFooterLength;
	AlignedSecByteBlock m_buffer;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class AuthenticatedSymmetricCipherBase:
    # // authenc.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # void AuthenticatedSymmetricCipherBase::AuthenticateData(const byte *input, size_t len)
    # 	unsigned int blockSize = AuthenticationBlockSize();
    # 	unsigned int &num = m_bufferedDataLength;
    # 	byte* data = m_buffer.begin();
    # 	if (num != 0)	// process left over data
    # 		if (num+len >= blockSize)
    # 			memcpy(data+num, input, blockSize-num);
    # 			AuthenticateBlocks(data, blockSize);
    # 			input += (blockSize-num);
    # 			len -= (blockSize-num);
    # 			num = 0;
    # 			// drop through and do the rest
    # 		else
    # 			memcpy(data+num, input, len);
    # 			num += (unsigned int)len;
    # 			return;
    # 	// now process the input data in blocks of blockSize bytes and save the leftovers to m_data
    # 	if (len >= blockSize)
    # 		size_t leftOver = AuthenticateBlocks(input, len);
    # 		input += (len - leftOver);
    # 		len = leftOver;
    # 	memcpy(data, input, len);
    # 	num = (unsigned int)len;
    # void AuthenticatedSymmetricCipherBase::SetKey(const byte *userKey, size_t keylength, const NameValuePairs &params)
    # 	m_bufferedDataLength = 0;
    # 	m_state = State_Start;
    # 	SetKeyWithoutResync(userKey, keylength, params);
    # 	m_state = State_KeySet;
    # 	size_t length;
    # 	const byte *iv = GetIVAndThrowIfInvalid(params, length);
    # 	if (iv)
    # 		Resynchronize(iv, (int)length);
    # void AuthenticatedSymmetricCipherBase::Resynchronize(const byte *iv, int length)
    # 	if (m_state < State_KeySet)
    # 		throw BadState(AlgorithmName(), "Resynchronize", "key is set");
    # 	m_bufferedDataLength = 0;
    # 	m_totalHeaderLength = m_totalMessageLength = m_totalFooterLength = 0;
    # 	m_state = State_KeySet;
    # 	Resync(iv, this->ThrowIfInvalidIVLength(length));
    # 	m_state = State_IVSet;
    # void AuthenticatedSymmetricCipherBase::Update(const byte *input, size_t length)
    # 	if (length == 0)
    # 		return;
    # 	switch (m_state)
    # 	case State_Start:
    # 	case State_KeySet:
    # 		throw BadState(AlgorithmName(), "Update", "setting key and IV");
    # 	case State_IVSet:
    # 		AuthenticateData(input, length);
    # 		m_totalHeaderLength += length;
    # 		break;
    # 	case State_AuthUntransformed:
    # 	case State_AuthTransformed:
    # 		AuthenticateLastConfidentialBlock();
    # 		m_bufferedDataLength = 0;
    # 		m_state = State_AuthFooter;
    # 		// fall through
    # 	case State_AuthFooter:
    # 		AuthenticateData(input, length);
    # 		m_totalFooterLength += length;
    # 		break;
    # 	default:
    # 		assert(false);
    # void AuthenticatedSymmetricCipherBase::ProcessData(byte *outString, const byte *inString, size_t length)
    # 	m_totalMessageLength += length;
    # 	if (m_state >= State_IVSet && m_totalMessageLength > MaxMessageLength())
    # 		throw InvalidArgument(AlgorithmName() + ": message length exceeds maximum");
    # reswitch:
    # 	switch (m_state)
    # 	case State_Start:
    # 	case State_KeySet:
    # 		throw BadState(AlgorithmName(), "ProcessData", "setting key and IV");
    # 	case State_AuthFooter:
    # 		throw BadState(AlgorithmName(), "ProcessData was called after footer input has started");
    # 	case State_IVSet:
    # 		AuthenticateLastHeaderBlock();
    # 		m_bufferedDataLength = 0;
    # 		m_state = AuthenticationIsOnPlaintext()==IsForwardTransformation() ? State_AuthUntransformed : State_AuthTransformed;
    # 		goto reswitch;
    # 	case State_AuthUntransformed:
    # 		AuthenticateData(inString, length);
    # 		AccessSymmetricCipher().ProcessData(outString, inString, length);
    # 		break;
    # 	case State_AuthTransformed:
    # 		AccessSymmetricCipher().ProcessData(outString, inString, length);
    # 		AuthenticateData(outString, length);
    # 		break;
    # 	default:
    # 		assert(false);
    # void AuthenticatedSymmetricCipherBase::TruncatedFinal(byte *mac, size_t macSize)
    # 	if (m_totalHeaderLength > MaxHeaderLength())
    # 		throw InvalidArgument(AlgorithmName() + ": header length of " + IntToString(m_totalHeaderLength) + " exceeds the maximum of " + IntToString(MaxHeaderLength()));
    # 	if (m_totalFooterLength > MaxFooterLength())
    # 		if (MaxFooterLength() == 0)
    # 			throw InvalidArgument(AlgorithmName() + ": additional authenticated data (AAD) cannot be input after data to be encrypted or decrypted");
    # 		else
    # 			throw InvalidArgument(AlgorithmName() + ": footer length of " + IntToString(m_totalFooterLength) + " exceeds the maximum of " + IntToString(MaxFooterLength()));
    # 	switch (m_state)
    # 	case State_Start:
    # 	case State_KeySet:
    # 		throw BadState(AlgorithmName(), "TruncatedFinal", "setting key and IV");
    # 	case State_IVSet:
    # 		AuthenticateLastHeaderBlock();
    # 		m_bufferedDataLength = 0;
    # 		// fall through
    # 	case State_AuthUntransformed:
    # 	case State_AuthTransformed:
    # 		AuthenticateLastConfidentialBlock();
    # 		m_bufferedDataLength = 0;
    # 		// fall through
    # 	case State_AuthFooter:
    # 		AuthenticateLastFooterBlock(mac, macSize);
    # 		m_bufferedDataLength = 0;
    # 		break;
    # 	default:
    # 		assert(false);
    # 	m_state = State_KeySet;
    # NAMESPACE_END
    # #endif