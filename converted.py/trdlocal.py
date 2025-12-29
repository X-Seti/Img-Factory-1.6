"""
trdlocal.py - Python conversion of trdlocal.h and trdlocal.cpp

Original C++ header content:
#ifndef CRYPTOPP_TRDLOCAL_H
#define CRYPTOPP_TRDLOCAL_H

#include "config.h"

#ifdef THREADS_AVAILABLE

#include "misc.h"

#ifdef HAS_WINTHREADS
typedef unsigned long ThreadLocalIndexType;
#else
#include <pthread.h>
typedef pthread_key_t ThreadLocalIndexType;
#endif

NAMESPACE_BEGIN(CryptoPP)

//! thread local storage
class CRYPTOPP_DLL ThreadLocalStorage : public NotCopyable
{
public:
	//! exception thrown by ThreadLocalStorage class
	class Err : public OS_Error
	{
	public:
		Err(const std::string& operation, int error);
	};

	ThreadLocalStorage();
	~ThreadLocalStorage();

	void SetValue(void *value);
	void *GetValue() const;

private:
	ThreadLocalIndexType m_index;
};

NAMESPACE_END

#endif	// #ifdef THREADS_AVAILABLE

#endif

"""

import os
import glob
from pathlib import Path

class ThreadLocalStorage:
    # // trdlocal.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # #ifdef THREADS_AVAILABLE
    # #ifdef HAS_WINTHREADS
    # #endif
    # NAMESPACE_BEGIN(CryptoPP)
    # ThreadLocalStorage::Err::Err(const std::string& operation, int error)
    # 	: OS_Error(OTHER_ERROR, "ThreadLocalStorage: " + operation + " operation failed with error 0x" + IntToString(error, 16), operation, error)
    # ThreadLocalStorage::ThreadLocalStorage()
    # #ifdef HAS_WINTHREADS
    # 	m_index = TlsAlloc();
    # 	if (m_index == TLS_OUT_OF_INDEXES)
    # 		throw Err("TlsAlloc", GetLastError());
    # #else
    # 	int error = pthread_key_create(&m_index, NULL);
    # 	if (error)
    # 		throw Err("pthread_key_create", error);
    # #endif
    # ThreadLocalStorage::~ThreadLocalStorage()
    # #ifdef HAS_WINTHREADS
    # 	if (!TlsFree(m_index))
    # 		throw Err("TlsFree", GetLastError());
    # #else
    # 	int error = pthread_key_delete(m_index);
    # 	if (error)
    # 		throw Err("pthread_key_delete", error);
    # #endif
    # void ThreadLocalStorage::SetValue(void *value)
    # #ifdef HAS_WINTHREADS
    # 	if (!TlsSetValue(m_index, value))
    # 		throw Err("TlsSetValue", GetLastError());
    # #else
    # 	int error = pthread_setspecific(m_index, value);
    # 	if (error)
    # 		throw Err("pthread_key_getspecific", error);
    # #endif
    # void *ThreadLocalStorage::GetValue() const
    # #ifdef HAS_WINTHREADS
    # 	void *result = TlsGetValue(m_index);
    # 	if (!result && GetLastError() != NO_ERROR)
    # 		throw Err("TlsGetValue", GetLastError());
    # #else
    # 	void *result = pthread_getspecific(m_index);
    # #endif
    # 	return result;
    # NAMESPACE_END
    # #endif	// #ifdef THREADS_AVAILABLE
    # #endif