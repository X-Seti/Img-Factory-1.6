"""
modes.py - Python conversion of N/A and modes.cpp

Original C++ header content:

"""

import os
import glob
from pathlib import Path

class CFB_ModePolicy:
    # // modes.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # #ifndef NDEBUG
    # #endif
    # NAMESPACE_BEGIN(CryptoPP)
    # #ifndef NDEBUG
    # void Modes_TestInstantiations()
    # #endif
    # void CFB_ModePolicy::Iterate(byte *output, const byte *input, CipherDir dir, size_t iterationCount)
    # 	assert(m_cipher->IsForwardTransformation());	// CFB mode needs the "encrypt" direction of the underlying block cipher, even to decrypt
    # 	assert(m_feedbackSize == BlockSize());
    # 	unsigned int s = BlockSize();
    # 	if (dir == ENCRYPTION)
    # 		m_cipher->ProcessAndXorBlock(m_register, input, output);
    # 		m_cipher->AdvancedProcessBlocks(output, input+s, output+s, (iterationCount-1)*s, 0);
    # 		memcpy(m_register, output+(iterationCount-1)*s, s);
    # 	else
    # 		memcpy(m_temp, input+(iterationCount-1)*s, s);	// make copy first in case of in-place decryption
    # 		m_cipher->ProcessAndXorBlock(m_register, input, output);
    # 		memcpy(m_register, m_temp, s);
    # void CFB_ModePolicy::TransformRegister()
    # 	assert(m_cipher->IsForwardTransformation());	// CFB mode needs the "encrypt" direction of the underlying block cipher, even to decrypt
    # 	m_cipher->ProcessBlock(m_register, m_temp);
    # 	unsigned int updateSize = BlockSize()-m_feedbackSize;
    # 	memmove_s(m_register, m_register.size(), m_register+m_feedbackSize, updateSize);
    # 	memcpy_s(m_register+updateSize, m_register.size()-updateSize, m_temp, m_feedbackSize);
    # void CFB_ModePolicy::CipherResynchronize(const byte *iv, size_t length)
    # 	assert(length == BlockSize());
    # 	CopyOrZero(m_register, iv, length);
    # 	TransformRegister();
    # void CFB_ModePolicy::SetFeedbackSize(unsigned int feedbackSize)
    # 	if (feedbackSize > BlockSize())
    # 		throw InvalidArgument("CFB_Mode: invalid feedback size");
    # 	m_feedbackSize = feedbackSize ? feedbackSize : BlockSize();
    # void CFB_ModePolicy::ResizeBuffers()
    # 	m_temp.New(BlockSize());
    # void OFB_ModePolicy::WriteKeystream(byte *keystreamBuffer, size_t iterationCount)
    # 	assert(m_cipher->IsForwardTransformation());	// OFB mode needs the "encrypt" direction of the underlying block cipher, even to decrypt
    # 	unsigned int s = BlockSize();
    # 	m_cipher->ProcessBlock(m_register, keystreamBuffer);
    # 	if (iterationCount > 1)
    # 		m_cipher->AdvancedProcessBlocks(keystreamBuffer, NULL, keystreamBuffer+s, s*(iterationCount-1), 0);
    # 	memcpy(m_register, keystreamBuffer+s*(iterationCount-1), s);
    # void OFB_ModePolicy::CipherResynchronize(byte *keystreamBuffer, const byte *iv, size_t length)
    # 	assert(length == BlockSize());
    # 	CopyOrZero(m_register, iv, length);
    # void CTR_ModePolicy::SeekToIteration(lword iterationCount)
    # 	int carry=0;
    # 	for (int i=BlockSize()-1; i>=0; i--)
    # 		unsigned int sum = m_register[i] + byte(iterationCount) + carry;
    # 		m_counterArray[i] = (byte) sum;
    # 		carry = sum >> 8;
    # 		iterationCount >>= 8;
    # void CTR_ModePolicy::IncrementCounterBy256()
    # 	IncrementCounterByOne(m_counterArray, BlockSize()-1);
    # void CTR_ModePolicy::OperateKeystream(KeystreamOperation operation, byte *output, const byte *input, size_t iterationCount)
    # 	assert(m_cipher->IsForwardTransformation());	// CTR mode needs the "encrypt" direction of the underlying block cipher, even to decrypt
    # 	unsigned int s = BlockSize();
    # 	unsigned int inputIncrement = input ? s : 0;
    # 	while (iterationCount)
    # 		byte lsb = m_counterArray[s-1];
    # 		size_t blocks = UnsignedMin(iterationCount, 256U-lsb);
    # 		if ((m_counterArray[s-1] = lsb + (byte)blocks) == 0)
    # 			IncrementCounterBy256();
    # 		output += blocks*s;
    # 		input += blocks*inputIncrement;
    # 		iterationCount -= blocks;
    # void CTR_ModePolicy::CipherResynchronize(byte *keystreamBuffer, const byte *iv, size_t length)
    # 	assert(length == BlockSize());
    # 	CopyOrZero(m_register, iv, length);
    # 	m_counterArray = m_register;
    # void BlockOrientedCipherModeBase::UncheckedSetKey(const byte *key, unsigned int length, const NameValuePairs &params)
    # 	m_cipher->SetKey(key, length, params);
    # 	ResizeBuffers();
    # 	if (IsResynchronizable())
    # 		size_t ivLength;
    # 		const byte *iv = GetIVAndThrowIfInvalid(params, ivLength);
    # 		Resynchronize(iv, (int)ivLength);
    # void ECB_OneWay::ProcessData(byte *outString, const byte *inString, size_t length)
    # 	assert(length%BlockSize()==0);
    # void CBC_Encryption::ProcessData(byte *outString, const byte *inString, size_t length)
    # 	if (!length)
    # 		return;
    # 	assert(length%BlockSize()==0);
    # 	unsigned int blockSize = BlockSize();
    # 	if (length > blockSize)
    # 	memcpy(m_register, outString + length - blockSize, blockSize);
    # void CBC_CTS_Encryption::ProcessLastBlock(byte *outString, const byte *inString, size_t length)
    # 	if (length <= BlockSize())
    # 		if (!m_stolenIV)
    # 			throw InvalidArgument("CBC_Encryption: message is too short for ciphertext stealing");
    # 		// steal from IV
    # 		memcpy(outString, m_register, length);
    # 		outString = m_stolenIV;
    # 	else
    # 		// steal from next to last block
    # 		xorbuf(m_register, inString, BlockSize());
    # 		m_cipher->ProcessBlock(m_register);
    # 		inString += BlockSize();
    # 		length -= BlockSize();
    # 		memcpy(outString+BlockSize(), m_register, length);
    # 	// output last full ciphertext block
    # 	xorbuf(m_register, inString, length);
    # 	m_cipher->ProcessBlock(m_register);
    # 	memcpy(outString, m_register, BlockSize());
    # void CBC_Decryption::ProcessData(byte *outString, const byte *inString, size_t length)
    # 	if (!length)
    # 		return;
    # 	assert(length%BlockSize()==0);
    # 	unsigned int blockSize = BlockSize();
    # 	memcpy(m_temp, inString+length-blockSize, blockSize);	// save copy now in case of in-place decryption
    # 	if (length > blockSize)
    # 	m_cipher->ProcessAndXorBlock(inString, m_register, outString);
    # 	m_register.swap(m_temp);
    # void CBC_CTS_Decryption::ProcessLastBlock(byte *outString, const byte *inString, size_t length)
    # 	const byte *pn, *pn1;
    # 	bool stealIV = length <= BlockSize();
    # 	if (stealIV)
    # 		pn = inString;
    # 		pn1 = m_register;
    # 	else
    # 		pn = inString + BlockSize();
    # 		pn1 = inString;
    # 		length -= BlockSize();
    # 	// decrypt last partial plaintext block
    # 	memcpy(m_temp, pn1, BlockSize());
    # 	m_cipher->ProcessBlock(m_temp);
    # 	xorbuf(m_temp, pn, length);
    # 	if (stealIV)
    # 		memcpy(outString, m_temp, length);
    # 	else
    # 		memcpy(outString+BlockSize(), m_temp, length);
    # 		// decrypt next to last plaintext block
    # 		memcpy(m_temp, pn, length);
    # 		m_cipher->ProcessBlock(m_temp);
    # 		xorbuf(outString, m_temp, m_register, BlockSize());
    # NAMESPACE_END
    # #endif