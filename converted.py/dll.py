"""
dll.py - Python conversion of dll.h and dll.cpp

Original C++ header content:
#ifndef CRYPTOPP_DLL_H
#define CRYPTOPP_DLL_H

#if !defined(CRYPTOPP_IMPORTS) && !defined(CRYPTOPP_EXPORTS) && !defined(CRYPTOPP_DEFAULT_NO_DLL)
#ifdef CRYPTOPP_CONFIG_H
#error To use the DLL version of Crypto++, this file must be included before any other Crypto++ header files.
#endif
#define CRYPTOPP_IMPORTS
#endif

#include "aes.h"
#include "cbcmac.h"
#include "ccm.h"
#include "cmac.h"
#include "channels.h"
#include "des.h"
#include "dh.h"
#include "dsa.h"
#include "ec2n.h"
#include "eccrypto.h"
#include "ecp.h"
#include "files.h"
#include "fips140.h"
#include "gcm.h"
#include "hex.h"
#include "hmac.h"
#include "modes.h"
#include "mqueue.h"
#include "nbtheory.h"
#include "osrng.h"
#include "pkcspad.h"
#include "pssr.h"
#include "randpool.h"
#include "rsa.h"
#include "rw.h"
#include "sha.h"
#include "skipjack.h"
#include "trdlocal.h"

#ifdef CRYPTOPP_IMPORTS

#ifdef _DLL
// cause CRT DLL to be initialized before Crypto++ so that we can use malloc and free during DllMain()
#ifdef NDEBUG
#pragma comment(lib, "msvcrt")
#else
#pragma comment(lib, "msvcrtd")
#endif
#endif

#pragma comment(lib, "cryptopp")

#endif		// #ifdef CRYPTOPP_IMPORTS

#include <new>	// for new_handler

NAMESPACE_BEGIN(CryptoPP)

#if !(defined(_MSC_VER) && (_MSC_VER < 1300))
using std::new_handler;
#endif

typedef void * (CRYPTOPP_API * PNew)(size_t);
typedef void (CRYPTOPP_API * PDelete)(void *);
typedef void (CRYPTOPP_API * PGetNewAndDelete)(PNew &, PDelete &);
typedef new_handler (CRYPTOPP_API * PSetNewHandler)(new_handler);
typedef void (CRYPTOPP_API * PSetNewAndDelete)(PNew, PDelete, PSetNewHandler);

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class std:
    # // dll.cpp - written and placed in the public domain by Wei Dai
    # #define CRYPTOPP_MANUALLY_INSTANTIATE_TEMPLATES
    # #define CRYPTOPP_DEFAULT_NO_DLL
    # #pragma warning(default: 4660)
    # #if defined(CRYPTOPP_EXPORTS) && defined(CRYPTOPP_WIN32_AVAILABLE)
    # #endif
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # NAMESPACE_END
    # #endif
    # #ifdef CRYPTOPP_EXPORTS
    # USING_NAMESPACE(CryptoPP)
    # #if !(defined(_MSC_VER) && (_MSC_VER < 1300))
    # #endif
    # static PNew s_pNew = NULL;
    # static PDelete s_pDelete = NULL;
    # static void * New (size_t size)
    # 	void *p;
    # 	while (!(p = malloc(size)))
    # 		CallNewHandler();
    # 	return p;
    # static void SetNewAndDeleteFunctionPointers()
    # 	void *p = NULL;
    # 	HMODULE hModule = NULL;
    # 	MEMORY_BASIC_INFORMATION mbi;
    # 	while (true)
    # 		VirtualQuery(p, &mbi, sizeof(mbi));
    # 		if (p >= (char *)mbi.BaseAddress + mbi.RegionSize)
    # 			break;
    # 		p = (char *)mbi.BaseAddress + mbi.RegionSize;
    # 		if (!mbi.AllocationBase || mbi.AllocationBase == hModule)
    # 			continue;
    # 		hModule = HMODULE(mbi.AllocationBase);
    # 		PGetNewAndDelete pGetNewAndDelete = (PGetNewAndDelete)GetProcAddress(hModule, "GetNewAndDeleteForCryptoPP");
    # 		if (pGetNewAndDelete)
    # 			pGetNewAndDelete(s_pNew, s_pDelete);
    # 			return;
    # 		PSetNewAndDelete pSetNewAndDelete = (PSetNewAndDelete)GetProcAddress(hModule, "SetNewAndDeleteFromCryptoPP");
    # 		if (pSetNewAndDelete)
    # 			s_pNew = &New;
    # 			s_pDelete = &free;
    # 			pSetNewAndDelete(s_pNew, s_pDelete, &set_new_handler);
    # 			return;
    # 	// try getting these directly using mangled names of new and delete operators
    # 	hModule = GetModuleHandle("msvcrtd");
    # 	if (!hModule)
    # 		hModule = GetModuleHandle("msvcrt");
    # 	if (hModule)
    # 		// 32-bit versions
    # 		s_pNew = (PNew)GetProcAddress(hModule, "??2@YAPAXI@Z");
    # 		s_pDelete = (PDelete)GetProcAddress(hModule, "??3@YAXPAX@Z");
    # 		if (s_pNew && s_pDelete)
    # 			return;
    # 		// 64-bit versions
    # 		s_pNew = (PNew)GetProcAddress(hModule, "??2@YAPEAX_K@Z");
    # 		s_pDelete = (PDelete)GetProcAddress(hModule, "??3@YAXPEAX@Z");
    # 		if (s_pNew && s_pDelete)
    # 			return;
    # 	OutputDebugString("Crypto++ was not able to obtain new and delete function pointers.\n");
    # 	throw 0;
    # void * operator new (size_t size)
    # 	if (!s_pNew)
    # 		SetNewAndDeleteFunctionPointers();
    # 	return s_pNew(size);
    # void operator delete (void * p)
    # 	s_pDelete(p);
    # void * operator new [] (size_t size)
    # 	return operator new (size);
    # void operator delete [] (void * p)
    # 	operator delete (p);
    # #endif	// #ifdef CRYPTOPP_EXPORTS