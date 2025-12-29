"""
osrng.py - Python conversion of osrng.h and osrng.cpp

Original C++ header content:
#ifndef CRYPTOPP_OSRNG_H
#define CRYPTOPP_OSRNG_H

//! \file

#include "config.h"

#ifdef OS_RNG_AVAILABLE

#include "randpool.h"
#include "rng.h"
#include "aes.h"
#include "sha.h"
#include "fips140.h"

NAMESPACE_BEGIN(CryptoPP)

//! Exception class for Operating-System Random Number Generator.
class CRYPTOPP_DLL OS_RNG_Err : public Exception
{
public:
	OS_RNG_Err(const std::string &operation);
};

#ifdef NONBLOCKING_RNG_AVAILABLE

#ifdef CRYPTOPP_WIN32_AVAILABLE
class CRYPTOPP_DLL MicrosoftCryptoProvider
{
public:
	MicrosoftCryptoProvider();
	~MicrosoftCryptoProvider();
#if defined(_WIN64)
	typedef unsigned __int64 ProviderHandle;	// type HCRYPTPROV, avoid #include <windows.h>
#else
	typedef unsigned long ProviderHandle;
#endif
	ProviderHandle GetProviderHandle() const {return m_hProvider;}
private:
	ProviderHandle m_hProvider;
};

#pragma comment(lib, "advapi32.lib")
#endif

//! encapsulate CryptoAPI's CryptGenRandom or /dev/urandom
class CRYPTOPP_DLL NonblockingRng : public RandomNumberGenerator
{
public:
	NonblockingRng();
	~NonblockingRng();
	void GenerateBlock(byte *output, size_t size);

protected:
#ifdef CRYPTOPP_WIN32_AVAILABLE
#	ifndef WORKAROUND_MS_BUG_Q258000
		MicrosoftCryptoProvider m_Provider;
#	endif
#else
	int m_fd;
#endif
};

#endif

#ifdef BLOCKING_RNG_AVAILABLE

//! encapsulate /dev/random, or /dev/srandom on OpenBSD
class CRYPTOPP_DLL BlockingRng : public RandomNumberGenerator
{
public:
	BlockingRng();
	~BlockingRng();
	void GenerateBlock(byte *output, size_t size);

protected:
	int m_fd;
};

#endif

CRYPTOPP_DLL void CRYPTOPP_API OS_GenerateRandomBlock(bool blocking, byte *output, size_t size);

//! Automaticly Seeded Randomness Pool
/*! This class seeds itself using an operating system provided RNG. */
class CRYPTOPP_DLL AutoSeededRandomPool : public RandomPool
{
public:
	//! use blocking to choose seeding with BlockingRng or NonblockingRng. the parameter is ignored if only one of these is available
	explicit AutoSeededRandomPool(bool blocking = false, unsigned int seedSize = 32)
		{Reseed(blocking, seedSize);}
	void Reseed(bool blocking = false, unsigned int seedSize = 32);
};

//! RNG from ANSI X9.17 Appendix C, seeded using an OS provided RNG
template <class BLOCK_CIPHER>
class AutoSeededX917RNG : public RandomNumberGenerator, public NotCopyable
{
public:
	//! use blocking to choose seeding with BlockingRng or NonblockingRng. the parameter is ignored if only one of these is available
	explicit AutoSeededX917RNG(bool blocking = false, bool autoSeed = true)
		{if (autoSeed) Reseed(blocking);}
	void Reseed(bool blocking = false, const byte *additionalEntropy = NULL, size_t length = 0);
	// exposed for testing
	void Reseed(const byte *key, size_t keylength, const byte *seed, const byte *timeVector);

	bool CanIncorporateEntropy() const {return true;}
	void IncorporateEntropy(const byte *input, size_t length) {Reseed(false, input, length);}
	void GenerateIntoBufferedTransformation(BufferedTransformation &target, const std::string &channel, lword length) {m_rng->GenerateIntoBufferedTransformation(target, channel, length);}

private:
	member_ptr<RandomNumberGenerator> m_rng;
};

template <class BLOCK_CIPHER>
void AutoSeededX917RNG<BLOCK_CIPHER>::Reseed(const byte *key, size_t keylength, const byte *seed, const byte *timeVector)
{
	m_rng.reset(new X917RNG(new typename BLOCK_CIPHER::Encryption(key, keylength), seed, timeVector));
}

template <class BLOCK_CIPHER>
void AutoSeededX917RNG<BLOCK_CIPHER>::Reseed(bool blocking, const byte *input, size_t length)
{
	SecByteBlock seed(BLOCK_CIPHER::BLOCKSIZE + BLOCK_CIPHER::DEFAULT_KEYLENGTH);
	const byte *key;
	do
	{
		OS_GenerateRandomBlock(blocking, seed, seed.size());
		if (length > 0)
		{
			SHA256 hash;
			hash.Update(seed, seed.size());
			hash.Update(input, length);
			hash.TruncatedFinal(seed, UnsignedMin(hash.DigestSize(), seed.size()));
		}
		key = seed + BLOCK_CIPHER::BLOCKSIZE;
	}	// check that seed and key don't have same value
	while (memcmp(key, seed, STDMIN((unsigned int)BLOCK_CIPHER::BLOCKSIZE, (unsigned int)BLOCK_CIPHER::DEFAULT_KEYLENGTH)) == 0);

	Reseed(key, BLOCK_CIPHER::DEFAULT_KEYLENGTH, seed, NULL);
}

CRYPTOPP_DLL_TEMPLATE_CLASS AutoSeededX917RNG<AES>;

//! this is AutoSeededX917RNG\<AES\> in FIPS mode, otherwise it's AutoSeededRandomPool
#if CRYPTOPP_ENABLE_COMPLIANCE_WITH_FIPS_140_2
typedef AutoSeededX917RNG<AES> DefaultAutoSeededRNG;
#else
typedef AutoSeededRandomPool DefaultAutoSeededRNG;
#endif

NAMESPACE_END

#endif

#endif

"""

import os
import glob
from pathlib import Path

class OS_RNG_Err:
    # // osrng.cpp - written and placed in the public domain by Wei Dai
    # // Thanks to Leonard Janke for the suggestion for AutoSeededRandomPool.
    # #ifndef CRYPTOPP_IMPORTS
    # #ifdef OS_RNG_AVAILABLE
    # #ifdef CRYPTOPP_WIN32_AVAILABLE
    # #ifndef _WIN32_WINNT
    # #define _WIN32_WINNT 0x0400
    # #endif
    # #endif
    # #ifdef CRYPTOPP_UNIX_AVAILABLE
    # #endif
    # NAMESPACE_BEGIN(CryptoPP)
    # #if defined(NONBLOCKING_RNG_AVAILABLE) || defined(BLOCKING_RNG_AVAILABLE)
    # OS_RNG_Err::OS_RNG_Err(const std::string &operation)
    # 	: Exception(OTHER_ERROR, "OS_Rng: " + operation + " operation failed with error " + 
    # #ifdef CRYPTOPP_WIN32_AVAILABLE
    # 		"0x" + IntToString(GetLastError(), 16)
    # #else
    # 		IntToString(errno)
    # #endif
    # 		)
    # #endif
    # #ifdef NONBLOCKING_RNG_AVAILABLE
    # #ifdef CRYPTOPP_WIN32_AVAILABLE
    # MicrosoftCryptoProvider::MicrosoftCryptoProvider()
    # 	if(!CryptAcquireContext(&m_hProvider, 0, 0, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT))
    # 		throw OS_RNG_Err("CryptAcquireContext");
    # MicrosoftCryptoProvider::~MicrosoftCryptoProvider()
    # 	CryptReleaseContext(m_hProvider, 0);
    # #endif
    # NonblockingRng::NonblockingRng()
    # #ifndef CRYPTOPP_WIN32_AVAILABLE
    # 	m_fd = open("/dev/urandom",O_RDONLY);
    # 	if (m_fd == -1)
    # 		throw OS_RNG_Err("open /dev/urandom");
    # #endif
    # NonblockingRng::~NonblockingRng()
    # #ifndef CRYPTOPP_WIN32_AVAILABLE
    # 	close(m_fd);
    # #endif
    # void NonblockingRng::GenerateBlock(byte *output, size_t size)
    # #ifdef CRYPTOPP_WIN32_AVAILABLE
    # #	ifdef WORKAROUND_MS_BUG_Q258000
    # 		const MicrosoftCryptoProvider &m_Provider = Singleton<MicrosoftCryptoProvider>().Ref();
    # #	endif
    # 	if (!CryptGenRandom(m_Provider.GetProviderHandle(), (DWORD)size, output))
    # 		throw OS_RNG_Err("CryptGenRandom");
    # #else
    # 	while (size)
    # 		ssize_t len = read(m_fd, output, size);
    # 		if (len < 0)
    # 			// /dev/urandom reads CAN give EAGAIN errors! (maybe EINTR as well)
    # 			if (errno != EINTR && errno != EAGAIN)
    # 				throw OS_RNG_Err("read /dev/urandom");
    # 			continue;
    # 		output += len;
    # 		size -= len;
    # #endif
    # #endif
    # // *************************************************************
    # #ifdef BLOCKING_RNG_AVAILABLE
    # #ifndef CRYPTOPP_BLOCKING_RNG_FILENAME
    # #ifdef __OpenBSD__
    # #define CRYPTOPP_BLOCKING_RNG_FILENAME "/dev/srandom"
    # #else
    # #define CRYPTOPP_BLOCKING_RNG_FILENAME "/dev/random"
    # #endif
    # #endif
    # BlockingRng::BlockingRng()
    # 	m_fd = open(CRYPTOPP_BLOCKING_RNG_FILENAME,O_RDONLY);
    # 	if (m_fd == -1)
    # 		throw OS_RNG_Err("open " CRYPTOPP_BLOCKING_RNG_FILENAME);
    # BlockingRng::~BlockingRng()
    # 	close(m_fd);
    # void BlockingRng::GenerateBlock(byte *output, size_t size)
    # 	while (size)
    # 		// on some systems /dev/random will block until all bytes
    # 		// are available, on others it returns immediately
    # 		ssize_t len = read(m_fd, output, size);
    # 		if (len < 0)
    # 			// /dev/random reads CAN give EAGAIN errors! (maybe EINTR as well)
    # 			if (errno != EINTR && errno != EAGAIN)
    # 				throw OS_RNG_Err("read " CRYPTOPP_BLOCKING_RNG_FILENAME);
    # 			continue;
    # 		size -= len;
    # 		output += len;
    # 		if (size)
    # 			sleep(1);
    # #endif
    # // *************************************************************
    # void OS_GenerateRandomBlock(bool blocking, byte *output, size_t size)
    # #ifdef NONBLOCKING_RNG_AVAILABLE
    # 	if (blocking)
    # #endif
    # #ifdef BLOCKING_RNG_AVAILABLE
    # 		BlockingRng rng;
    # 		rng.GenerateBlock(output, size);
    # #endif
    # #ifdef BLOCKING_RNG_AVAILABLE
    # 	if (!blocking)
    # #endif
    # #ifdef NONBLOCKING_RNG_AVAILABLE
    # 		NonblockingRng rng;
    # 		rng.GenerateBlock(output, size);
    # #endif
    # void AutoSeededRandomPool::Reseed(bool blocking, unsigned int seedSize)
    # 	SecByteBlock seed(seedSize);
    # 	OS_GenerateRandomBlock(blocking, seed, seedSize);
    # 	IncorporateEntropy(seed, seedSize);
    # NAMESPACE_END
    # #endif
    # #endif