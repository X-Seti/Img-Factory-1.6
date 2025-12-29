"""
winpipes.py - Python conversion of winpipes.h and winpipes.cpp

Original C++ header content:
#ifndef CRYPTOPP_WINPIPES_H
#define CRYPTOPP_WINPIPES_H

#include "config.h"

#ifdef WINDOWS_PIPES_AVAILABLE

#include "network.h"
#include "queue.h"
#include <winsock2.h>

NAMESPACE_BEGIN(CryptoPP)

//! Windows Handle
class WindowsHandle
{
public:
	WindowsHandle(HANDLE h = INVALID_HANDLE_VALUE, bool own=false);
	WindowsHandle(const WindowsHandle &h) : m_h(h.m_h), m_own(false) {}
	virtual ~WindowsHandle();

	bool GetOwnership() const {return m_own;}
	void SetOwnership(bool own) {m_own = own;}

	operator HANDLE() {return m_h;}
	HANDLE GetHandle() const {return m_h;}
	bool HandleValid() const;
	void AttachHandle(HANDLE h, bool own=false);
	HANDLE DetachHandle();
	void CloseHandle();

protected:
	virtual void HandleChanged() {}

	HANDLE m_h;
	bool m_own;
};

//! Windows Pipe
class WindowsPipe
{
public:
	class Err : public OS_Error
	{
	public:
		Err(HANDLE h, const std::string& operation, int error);
		HANDLE GetHandle() const {return m_h;}

	private:
		HANDLE m_h;
	};

protected:
	virtual HANDLE GetHandle() const =0;
	virtual void HandleError(const char *operation) const;
	void CheckAndHandleError(const char *operation, BOOL result) const
		{assert(result==TRUE || result==FALSE); if (!result) HandleError(operation);}
};

//! pipe-based implementation of NetworkReceiver
class WindowsPipeReceiver : public WindowsPipe, public NetworkReceiver
{
public:
	WindowsPipeReceiver();

	bool MustWaitForResult() {return true;}
	bool Receive(byte* buf, size_t bufLen);
	unsigned int GetReceiveResult();
	bool EofReceived() const {return m_eofReceived;}

	unsigned int GetMaxWaitObjectCount() const {return 1;}
	void GetWaitObjects(WaitObjectContainer &container, CallStack const& callStack);

private:
	WindowsHandle m_event;
	OVERLAPPED m_overlapped;
	bool m_resultPending;
	DWORD m_lastResult;
	bool m_eofReceived;
};

//! pipe-based implementation of NetworkSender
class WindowsPipeSender : public WindowsPipe, public NetworkSender
{
public:
	WindowsPipeSender();

	bool MustWaitForResult() {return true;}
	void Send(const byte* buf, size_t bufLen);
	unsigned int GetSendResult();
	bool MustWaitForEof() { return false; }
	void SendEof() {}

	unsigned int GetMaxWaitObjectCount() const {return 1;}
	void GetWaitObjects(WaitObjectContainer &container, CallStack const& callStack);

private:
	WindowsHandle m_event;
	OVERLAPPED m_overlapped;
	bool m_resultPending;
	DWORD m_lastResult;
};

//! Windows Pipe Source
class WindowsPipeSource : public WindowsHandle, public NetworkSource, public WindowsPipeReceiver
{
public:
	WindowsPipeSource(HANDLE h=INVALID_HANDLE_VALUE, bool pumpAll=false, BufferedTransformation *attachment=NULL)
		: WindowsHandle(h), NetworkSource(attachment)
	{
		if (pumpAll)
			PumpAll();
	}

	NetworkSource::GetMaxWaitObjectCount;
	NetworkSource::GetWaitObjects;

private:
	HANDLE GetHandle() const {return WindowsHandle::GetHandle();}
	NetworkReceiver & AccessReceiver() {return *this;}
};

//! Windows Pipe Sink
class WindowsPipeSink : public WindowsHandle, public NetworkSink, public WindowsPipeSender
{
public:
	WindowsPipeSink(HANDLE h=INVALID_HANDLE_VALUE, unsigned int maxBufferSize=0, unsigned int autoFlushBound=16*1024)
		: WindowsHandle(h), NetworkSink(maxBufferSize, autoFlushBound) {}

	NetworkSink::GetMaxWaitObjectCount;
	NetworkSink::GetWaitObjects;

private:
	HANDLE GetHandle() const {return WindowsHandle::GetHandle();}
	NetworkSender & AccessSender() {return *this;}
};

NAMESPACE_END

#endif

#endif

"""

import os
import glob
from pathlib import Path

class WindowsHandle:
    # // winpipes.cpp - written and placed in the public domain by Wei Dai
    # #ifdef WINDOWS_PIPES_AVAILABLE
    # NAMESPACE_BEGIN(CryptoPP)
    # WindowsHandle::WindowsHandle(HANDLE h, bool own)
    # 	: m_h(h), m_own(own)
    # WindowsHandle::~WindowsHandle()
    # 	if (m_own)
    # 		try
    # 			CloseHandle();
    # 		catch (...)
    # bool WindowsHandle::HandleValid() const
    # 	return m_h && m_h != INVALID_HANDLE_VALUE;
    # void WindowsHandle::AttachHandle(HANDLE h, bool own)
    # 	if (m_own)
    # 		CloseHandle();
    # 	m_h = h;
    # 	m_own = own;
    # 	HandleChanged();
    # HANDLE WindowsHandle::DetachHandle()
    # 	HANDLE h = m_h;
    # 	m_h = INVALID_HANDLE_VALUE;
    # 	HandleChanged();
    # 	return h;
    # void WindowsHandle::CloseHandle()
    # 	if (m_h != INVALID_HANDLE_VALUE)
    # 		m_h = INVALID_HANDLE_VALUE;
    # 		HandleChanged();
    # // ********************************************************
    # void WindowsPipe::HandleError(const char *operation) const
    # 	DWORD err = GetLastError();
    # 	throw Err(GetHandle(), operation, err);
    # WindowsPipe::Err::Err(HANDLE s, const std::string& operation, int error)
    # 	: OS_Error(IO_ERROR, "WindowsPipe: " + operation + " operation failed with error 0x" + IntToString(error, 16), operation, error)
    # 	, m_h(s)
    # // *************************************************************
    # WindowsPipeReceiver::WindowsPipeReceiver()
    # 	: m_resultPending(false), m_eofReceived(false)
    # 	m_event.AttachHandle(CreateEvent(NULL, true, false, NULL), true);
    # 	CheckAndHandleError("CreateEvent", m_event.HandleValid());
    # 	memset(&m_overlapped, 0, sizeof(m_overlapped));
    # 	m_overlapped.hEvent = m_event;
    # bool WindowsPipeReceiver::Receive(byte* buf, size_t bufLen)
    # 	assert(!m_resultPending && !m_eofReceived);
    # 	HANDLE h = GetHandle();
    # 	// don't queue too much at once, or we might use up non-paged memory
    # 	if (ReadFile(h, buf, UnsignedMin((DWORD)128*1024, bufLen), &m_lastResult, &m_overlapped))
    # 		if (m_lastResult == 0)
    # 			m_eofReceived = true;
    # 	else
    # 		switch (GetLastError())
    # 		default:
    # 			CheckAndHandleError("ReadFile", false);
    # 		case ERROR_BROKEN_PIPE:
    # 		case ERROR_HANDLE_EOF:
    # 			m_lastResult = 0;
    # 			m_eofReceived = true;
    # 			break;
    # 		case ERROR_IO_PENDING:
    # 			m_resultPending = true;
    # 	return !m_resultPending;
    # void WindowsPipeReceiver::GetWaitObjects(WaitObjectContainer &container, CallStack const& callStack)
    # 	if (m_resultPending)
    # 	else if (!m_eofReceived)
    # unsigned int WindowsPipeReceiver::GetReceiveResult()
    # 	if (m_resultPending)
    # 		HANDLE h = GetHandle();
    # 		if (GetOverlappedResult(h, &m_overlapped, &m_lastResult, false))
    # 			if (m_lastResult == 0)
    # 				m_eofReceived = true;
    # 		else
    # 			switch (GetLastError())
    # 			default:
    # 				CheckAndHandleError("GetOverlappedResult", false);
    # 			case ERROR_BROKEN_PIPE:
    # 			case ERROR_HANDLE_EOF:
    # 				m_lastResult = 0;
    # 				m_eofReceived = true;
    # 		m_resultPending = false;
    # 	return m_lastResult;
    # // *************************************************************
    # WindowsPipeSender::WindowsPipeSender()
    # 	: m_resultPending(false), m_lastResult(0)
    # 	m_event.AttachHandle(CreateEvent(NULL, true, false, NULL), true);
    # 	CheckAndHandleError("CreateEvent", m_event.HandleValid());
    # 	memset(&m_overlapped, 0, sizeof(m_overlapped));
    # 	m_overlapped.hEvent = m_event;
    # void WindowsPipeSender::Send(const byte* buf, size_t bufLen)
    # 	DWORD written = 0;
    # 	HANDLE h = GetHandle();
    # 	// don't queue too much at once, or we might use up non-paged memory
    # 	if (WriteFile(h, buf, UnsignedMin((DWORD)128*1024, bufLen), &written, &m_overlapped))
    # 		m_resultPending = false;
    # 		m_lastResult = written;
    # 	else
    # 		if (GetLastError() != ERROR_IO_PENDING)
    # 			CheckAndHandleError("WriteFile", false);
    # 		m_resultPending = true;
    # void WindowsPipeSender::GetWaitObjects(WaitObjectContainer &container, CallStack const& callStack)
    # 	if (m_resultPending)
    # 	else
    # unsigned int WindowsPipeSender::GetSendResult()
    # 	if (m_resultPending)
    # 		HANDLE h = GetHandle();
    # 		BOOL result = GetOverlappedResult(h, &m_overlapped, &m_lastResult, false);
    # 		CheckAndHandleError("GetOverlappedResult", result);
    # 		m_resultPending = false;
    # 	return m_lastResult;
    # NAMESPACE_END
    # #endif