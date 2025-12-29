"""
hrtimer.py - Python conversion of hrtimer.h and hrtimer.cpp

Original C++ header content:
#ifndef CRYPTOPP_HRTIMER_H
#define CRYPTOPP_HRTIMER_H

#include "config.h"
#ifndef HIGHRES_TIMER_AVAILABLE
#include <time.h>
#endif

NAMESPACE_BEGIN(CryptoPP)

#ifdef HIGHRES_TIMER_AVAILABLE
	typedef word64 TimerWord;
#else
	typedef clock_t TimerWord;
#endif

//! _
class CRYPTOPP_DLL CRYPTOPP_NO_VTABLE TimerBase
{
public:
	enum Unit {SECONDS = 0, MILLISECONDS, MICROSECONDS, NANOSECONDS};
	TimerBase(Unit unit, bool stuckAtZero)	: m_timerUnit(unit), m_stuckAtZero(stuckAtZero), m_started(false) {}

	virtual TimerWord GetCurrentTimerValue() =0;	// GetCurrentTime is a macro in MSVC 6.0
	virtual TimerWord TicksPerSecond() =0;	// this is not the resolution, just a conversion factor into seconds

	void StartTimer();
	double ElapsedTimeAsDouble();
	unsigned long ElapsedTime();

private:
	double ConvertTo(TimerWord t, Unit unit);

	Unit m_timerUnit;	// HPUX workaround: m_unit is a system macro on HPUX
	bool m_stuckAtZero, m_started;
	TimerWord m_start, m_last;
};

//! measure CPU time spent executing instructions of this thread (if supported by OS)
/*! /note This only works correctly on Windows NT or later. On Unix it reports process time, and others wall clock time.
*/
class ThreadUserTimer : public TimerBase
{
public:
	ThreadUserTimer(Unit unit = TimerBase::SECONDS, bool stuckAtZero = false)	: TimerBase(unit, stuckAtZero) {}
	TimerWord GetCurrentTimerValue();
	TimerWord TicksPerSecond();
};

//! high resolution timer
class CRYPTOPP_DLL Timer : public TimerBase
{
public:
	Timer(Unit unit = TimerBase::SECONDS, bool stuckAtZero = false)	: TimerBase(unit, stuckAtZero) {}
	TimerWord GetCurrentTimerValue();
	TimerWord TicksPerSecond();
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class TimerBase:
    # // hrtimer.cpp - written and placed in the public domain by Wei Dai
    # #if defined(CRYPTOPP_WIN32_AVAILABLE)
    # #elif defined(CRYPTOPP_UNIX_AVAILABLE)
    # #endif
    # NAMESPACE_BEGIN(CryptoPP)
    # #ifndef CRYPTOPP_IMPORTS
    # double TimerBase::ConvertTo(TimerWord t, Unit unit)
    # 	static unsigned long unitsPerSecondTable[] = {1, 1000, 1000*1000, 1000*1000*1000};
    # 	assert(unit < sizeof(unitsPerSecondTable) / sizeof(unitsPerSecondTable[0]));
    # 	return (double)CRYPTOPP_VC6_INT64 t * unitsPerSecondTable[unit] / CRYPTOPP_VC6_INT64 TicksPerSecond();
    # void TimerBase::StartTimer()
    # 	m_last = m_start = GetCurrentTimerValue();
    # 	m_started = true;
    # double TimerBase::ElapsedTimeAsDouble()
    # 	if (m_stuckAtZero)
    # 		return 0;
    # 	if (m_started)
    # 		TimerWord now = GetCurrentTimerValue();
    # 		if (m_last < now)	// protect against OS bugs where time goes backwards
    # 			m_last = now;
    # 		return ConvertTo(m_last - m_start, m_timerUnit);
    # 	StartTimer();
    # 	return 0;
    # unsigned long TimerBase::ElapsedTime()
    # 	double elapsed = ElapsedTimeAsDouble();
    # 	assert(elapsed <= ULONG_MAX);
    # 	return (unsigned long)elapsed;
    # TimerWord Timer::GetCurrentTimerValue()
    # #if defined(CRYPTOPP_WIN32_AVAILABLE)
    # 	LARGE_INTEGER now;
    # 	if (!QueryPerformanceCounter(&now))
    # 	return now.QuadPart;
    # #elif defined(CRYPTOPP_UNIX_AVAILABLE)
    # 	timeval now;
    # 	gettimeofday(&now, NULL);
    # 	return (TimerWord)now.tv_sec * 1000000 + now.tv_usec;
    # #else
    # 	clock_t now;
    # 	return clock();
    # #endif
    # TimerWord Timer::TicksPerSecond()
    # #if defined(CRYPTOPP_WIN32_AVAILABLE)
    # 	static LARGE_INTEGER freq = {0};
    # 	if (freq.QuadPart == 0)
    # 		if (!QueryPerformanceFrequency(&freq))
    # 	return freq.QuadPart;
    # #elif defined(CRYPTOPP_UNIX_AVAILABLE)
    # 	return 1000000;
    # #else
    # 	return CLOCKS_PER_SEC;
    # #endif
    # #endif	// #ifndef CRYPTOPP_IMPORTS
    # TimerWord ThreadUserTimer::GetCurrentTimerValue()
    # #if defined(CRYPTOPP_WIN32_AVAILABLE)
    # 	static bool getCurrentThreadImplemented = true;
    # 	if (getCurrentThreadImplemented)
    # 		FILETIME now, ignored;
    # 		if (!GetThreadTimes(GetCurrentThread(), &ignored, &ignored, &ignored, &now))
    # 			DWORD lastError = GetLastError();
    # 			if (lastError == ERROR_CALL_NOT_IMPLEMENTED)
    # 				getCurrentThreadImplemented = false;
    # 				goto GetCurrentThreadNotImplemented;
    # 		return now.dwLowDateTime + ((TimerWord)now.dwHighDateTime << 32);
    # GetCurrentThreadNotImplemented:
    # 	return (TimerWord)clock() * (10*1000*1000 / CLOCKS_PER_SEC);
    # #elif defined(CRYPTOPP_UNIX_AVAILABLE)
    # 	tms now;
    # 	times(&now);
    # 	return now.tms_utime;
    # #else
    # 	return clock();
    # #endif
    # TimerWord ThreadUserTimer::TicksPerSecond()
    # #if defined(CRYPTOPP_WIN32_AVAILABLE)
    # 	return 10*1000*1000;
    # #elif defined(CRYPTOPP_UNIX_AVAILABLE)
    # 	static const long ticksPerSecond = sysconf(_SC_CLK_TCK);
    # 	return ticksPerSecond;
    # #else
    # 	return CLOCKS_PER_SEC;
    # #endif
    # NAMESPACE_END