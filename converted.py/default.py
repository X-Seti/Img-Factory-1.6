"""
default.py - Python conversion of default.h and default.cpp

Original C++ header content:
#ifndef CRYPTOPP_DEFAULT_H
#define CRYPTOPP_DEFAULT_H

#include "sha.h"
#include "hmac.h"
#include "des.h"
#include "filters.h"
#include "modes.h"

NAMESPACE_BEGIN(CryptoPP)

typedef DES_EDE2 Default_BlockCipher;
typedef SHA DefaultHashModule;
typedef HMAC<DefaultHashModule> DefaultMAC;

//! Password-Based Encryptor using DES-EDE2
class DefaultEncryptor : public ProxyFilter
{
public:
	DefaultEncryptor(const char *passphrase, BufferedTransformation *attachment = NULL);
	DefaultEncryptor(const byte *passphrase, size_t passphraseLength, BufferedTransformation *attachment = NULL);

protected:
	void FirstPut(const byte *);
	void LastPut(const byte *inString, size_t length);

private:
	SecByteBlock m_passphrase;
	CBC_Mode<Default_BlockCipher>::Encryption m_cipher;
};

//! Password-Based Decryptor using DES-EDE2
class DefaultDecryptor : public ProxyFilter
{
public:
	DefaultDecryptor(const char *passphrase, BufferedTransformation *attachment = NULL, bool throwException=true);
	DefaultDecryptor(const byte *passphrase, size_t passphraseLength, BufferedTransformation *attachment = NULL, bool throwException=true);

	class Err : public Exception
	{
	public:
		Err(const std::string &s) 
			: Exception(DATA_INTEGRITY_CHECK_FAILED, s) {}
	};
	class KeyBadErr : public Err {public: KeyBadErr() : Err("DefaultDecryptor: cannot decrypt message with this passphrase") {}};

	enum State {WAITING_FOR_KEYCHECK, KEY_GOOD, KEY_BAD};
	State CurrentState() const {return m_state;}

protected:
	void FirstPut(const byte *inString);
	void LastPut(const byte *inString, size_t length);

	State m_state;

private:
	void CheckKey(const byte *salt, const byte *keyCheck);

	SecByteBlock m_passphrase;
	CBC_Mode<Default_BlockCipher>::Decryption m_cipher;
	member_ptr<FilterWithBufferedInput> m_decryptor;
	bool m_throwException;
};

//! Password-Based Encryptor using DES-EDE2 and HMAC/SHA-1
class DefaultEncryptorWithMAC : public ProxyFilter
{
public:
	DefaultEncryptorWithMAC(const char *passphrase, BufferedTransformation *attachment = NULL);
	DefaultEncryptorWithMAC(const byte *passphrase, size_t passphraseLength, BufferedTransformation *attachment = NULL);

protected:
	void FirstPut(const byte *inString) {}
	void LastPut(const byte *inString, size_t length);

private:
	member_ptr<DefaultMAC> m_mac;
};

//! Password-Based Decryptor using DES-EDE2 and HMAC/SHA-1
class DefaultDecryptorWithMAC : public ProxyFilter
{
public:
	class MACBadErr : public DefaultDecryptor::Err {public: MACBadErr() : DefaultDecryptor::Err("DefaultDecryptorWithMAC: MAC check failed") {}};

	DefaultDecryptorWithMAC(const char *passphrase, BufferedTransformation *attachment = NULL, bool throwException=true);
	DefaultDecryptorWithMAC(const byte *passphrase, size_t passphraseLength, BufferedTransformation *attachment = NULL, bool throwException=true);

	DefaultDecryptor::State CurrentState() const;
	bool CheckLastMAC() const;

protected:
	void FirstPut(const byte *inString) {}
	void LastPut(const byte *inString, size_t length);

private:
	member_ptr<DefaultMAC> m_mac;
	HashVerifier *m_hashVerifier;
	bool m_throwException;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class Default_BlockCipher:
    # // default.cpp - written and placed in the public domain by Wei Dai
    # NAMESPACE_BEGIN(CryptoPP)
    # static const unsigned int MASH_ITERATIONS = 200;
    # static const unsigned int SALTLENGTH = 8;
    # // The purpose of this function Mash() is to take an arbitrary length input
    # // string and *deterministicly* produce an arbitrary length output string such
    # // that (1) it looks random, (2) no information about the input is
    # // deducible from it, and (3) it contains as much entropy as it can hold, or
    # // the amount of entropy in the input string, whichever is smaller.
    # static void Mash(const byte *in, size_t inLen, byte *out, size_t outLen, int iterations)
    # 	if (BytePrecision(outLen) > 2)
    # 		throw InvalidArgument("Mash: output legnth too large");
    # 	byte b[2];
    # 	SecByteBlock buf(bufSize);
    # 	SecByteBlock outBuf(bufSize);
    # 	DefaultHashModule hash;
    # 	unsigned int i;
    # 	for(i=0; i<outLen; i+=DefaultHashModule::DIGESTSIZE)
    # 		b[0] = (byte) (i >> 8);
    # 		b[1] = (byte) i;
    # 		hash.Update(b, 2);
    # 		hash.Update(in, inLen);
    # 		hash.Final(outBuf+i);
    # 	while (iterations-- > 1)
    # 		memcpy(buf, outBuf, bufSize);
    # 		for (i=0; i<bufSize; i+=DefaultHashModule::DIGESTSIZE)
    # 			b[0] = (byte) (i >> 8);
    # 			b[1] = (byte) i;
    # 			hash.Update(b, 2);
    # 			hash.Update(buf, bufSize);
    # 			hash.Final(outBuf+i);
    # 	memcpy(out, outBuf, outLen);
    # static void GenerateKeyIV(const byte *passphrase, size_t passphraseLength, const byte *salt, size_t saltLength, byte *key, byte *IV)
    # 	SecByteBlock temp(passphraseLength+saltLength);
    # 	memcpy(temp, passphrase, passphraseLength);
    # 	memcpy(temp+passphraseLength, salt, saltLength);
    # 	SecByteBlock keyIV(KEYLENGTH+BLOCKSIZE);
    # 	Mash(temp, passphraseLength + saltLength, keyIV, KEYLENGTH+BLOCKSIZE, MASH_ITERATIONS);
    # 	memcpy(key, keyIV, KEYLENGTH);
    # 	memcpy(IV, keyIV+KEYLENGTH, BLOCKSIZE);
    # // ********************************************************
    # DefaultEncryptor::DefaultEncryptor(const char *passphrase, BufferedTransformation *attachment)
    # 	: ProxyFilter(NULL, 0, 0, attachment), m_passphrase((const byte *)passphrase, strlen(passphrase))
    # DefaultEncryptor::DefaultEncryptor(const byte *passphrase, size_t passphraseLength, BufferedTransformation *attachment)
    # 	: ProxyFilter(NULL, 0, 0, attachment), m_passphrase(passphrase, passphraseLength)
    # void DefaultEncryptor::FirstPut(const byte *)
    # 	// VC60 workaround: __LINE__ expansion bug
    # 	DefaultHashModule hash;
    # 	// use hash(passphrase | time | clock) as salt
    # 	hash.Update(m_passphrase, m_passphrase.size());
    # 	time_t t=time(0);
    # 	hash.Update((byte *)&t, sizeof(t));
    # 	clock_t c=clock();
    # 	hash.Update((byte *)&c, sizeof(c));
    # 	hash.Final(salt);
    # 	// use hash(passphrase | salt) as key check
    # 	hash.Update(m_passphrase, m_passphrase.size());
    # 	hash.Update(salt, SALTLENGTH);
    # 	hash.Final(keyCheck);
    # 	AttachedTransformation()->Put(salt, SALTLENGTH);
    # 	// mash passphrase and salt together into key and IV
    # 	SecByteBlock key(KEYLENGTH);
    # 	SecByteBlock IV(BLOCKSIZE);
    # 	GenerateKeyIV(m_passphrase, m_passphrase.size(), salt, SALTLENGTH, key, IV);
    # 	m_cipher.SetKeyWithIV(key, key.size(), IV);
    # 	SetFilter(new StreamTransformationFilter(m_cipher));
    # 	m_filter->Put(keyCheck, BLOCKSIZE);
    # void DefaultEncryptor::LastPut(const byte *inString, size_t length)
    # 	m_filter->MessageEnd();
    # // ********************************************************
    # DefaultDecryptor::DefaultDecryptor(const char *p, BufferedTransformation *attachment, bool throwException)
    # 	: ProxyFilter(NULL, SALTLENGTH+BLOCKSIZE, 0, attachment)
    # 	, m_state(WAITING_FOR_KEYCHECK)
    # 	, m_passphrase((const byte *)p, strlen(p))
    # 	, m_throwException(throwException)
    # DefaultDecryptor::DefaultDecryptor(const byte *passphrase, size_t passphraseLength, BufferedTransformation *attachment, bool throwException)
    # 	: ProxyFilter(NULL, SALTLENGTH+BLOCKSIZE, 0, attachment)
    # 	, m_state(WAITING_FOR_KEYCHECK)
    # 	, m_passphrase(passphrase, passphraseLength)
    # 	, m_throwException(throwException)
    # void DefaultDecryptor::FirstPut(const byte *inString)
    # 	CheckKey(inString, inString+SALTLENGTH);
    # void DefaultDecryptor::LastPut(const byte *inString, size_t length)
    # 	if (m_filter.get() == NULL)
    # 		m_state = KEY_BAD;
    # 		if (m_throwException)
    # 			throw KeyBadErr();
    # 	else
    # 		m_filter->MessageEnd();
    # 		m_state = WAITING_FOR_KEYCHECK;
    # void DefaultDecryptor::CheckKey(const byte *salt, const byte *keyCheck)
    # 	DefaultHashModule hash;
    # 	hash.Update(m_passphrase, m_passphrase.size());
    # 	hash.Update(salt, SALTLENGTH);
    # 	hash.Final(check);
    # 	SecByteBlock key(KEYLENGTH);
    # 	SecByteBlock IV(BLOCKSIZE);
    # 	GenerateKeyIV(m_passphrase, m_passphrase.size(), salt, SALTLENGTH, key, IV);
    # 	m_cipher.SetKeyWithIV(key, key.size(), IV);
    # 	decryptor->Put(keyCheck, BLOCKSIZE);
    # 	decryptor->ForceNextPut();
    # 	decryptor->Get(check+BLOCKSIZE, BLOCKSIZE);
    # 	SetFilter(decryptor.release());
    # 	if (!VerifyBufsEqual(check, check+BLOCKSIZE, BLOCKSIZE))
    # 		m_state = KEY_BAD;
    # 		if (m_throwException)
    # 			throw KeyBadErr();
    # 	else
    # 		m_state = KEY_GOOD;
    # // ********************************************************
    # static DefaultMAC * NewDefaultEncryptorMAC(const byte *passphrase, size_t passphraseLength)
    # 	SecByteBlock macKey(macKeyLength);
    # 	// since the MAC is encrypted there is no reason to mash the passphrase for many iterations
    # 	Mash(passphrase, passphraseLength, macKey, macKeyLength, 1);
    # 	return new DefaultMAC(macKey, macKeyLength);
    # DefaultEncryptorWithMAC::DefaultEncryptorWithMAC(const char *passphrase, BufferedTransformation *attachment)
    # 	: ProxyFilter(NULL, 0, 0, attachment)
    # 	, m_mac(NewDefaultEncryptorMAC((const byte *)passphrase, strlen(passphrase)))
    # 	SetFilter(new HashFilter(*m_mac, new DefaultEncryptor(passphrase), true));
    # DefaultEncryptorWithMAC::DefaultEncryptorWithMAC(const byte *passphrase, size_t passphraseLength, BufferedTransformation *attachment)
    # 	: ProxyFilter(NULL, 0, 0, attachment)
    # 	, m_mac(NewDefaultEncryptorMAC(passphrase, passphraseLength))
    # 	SetFilter(new HashFilter(*m_mac, new DefaultEncryptor(passphrase, passphraseLength), true));
    # void DefaultEncryptorWithMAC::LastPut(const byte *inString, size_t length)
    # 	m_filter->MessageEnd();
    # // ********************************************************
    # DefaultDecryptorWithMAC::DefaultDecryptorWithMAC(const char *passphrase, BufferedTransformation *attachment, bool throwException)
    # 	: ProxyFilter(NULL, 0, 0, attachment)
    # 	, m_mac(NewDefaultEncryptorMAC((const byte *)passphrase, strlen(passphrase)))
    # 	, m_throwException(throwException)
    # DefaultDecryptorWithMAC::DefaultDecryptorWithMAC(const byte *passphrase, size_t passphraseLength, BufferedTransformation *attachment, bool throwException)
    # 	: ProxyFilter(NULL, 0, 0, attachment)
    # 	, m_mac(NewDefaultEncryptorMAC(passphrase, passphraseLength))
    # 	, m_throwException(throwException)
    # DefaultDecryptor::State DefaultDecryptorWithMAC::CurrentState() const
    # 	return static_cast<const DefaultDecryptor *>(m_filter.get())->CurrentState();
    # bool DefaultDecryptorWithMAC::CheckLastMAC() const
    # 	return m_hashVerifier->GetLastResult();
    # void DefaultDecryptorWithMAC::LastPut(const byte *inString, size_t length)
    # 	m_filter->MessageEnd();
    # 	if (m_throwException && !CheckLastMAC())
    # 		throw MACBadErr();
    # NAMESPACE_END