"""
mqueue.py - Python conversion of mqueue.h and mqueue.cpp

Original C++ header content:
#ifndef CRYPTOPP_MQUEUE_H
#define CRYPTOPP_MQUEUE_H

#include "queue.h"
#include "filters.h"
#include <deque>

NAMESPACE_BEGIN(CryptoPP)

//! Message Queue
class CRYPTOPP_DLL MessageQueue : public AutoSignaling<BufferedTransformation>
{
public:
	MessageQueue(unsigned int nodeSize=256);

	void IsolatedInitialize(const NameValuePairs &parameters)
		{m_queue.IsolatedInitialize(parameters); m_lengths.assign(1, 0U); m_messageCounts.assign(1, 0U);}
	size_t Put2(const byte *begin, size_t length, int messageEnd, bool blocking)
	{
		m_queue.Put(begin, length);
		m_lengths.back() += length;
		if (messageEnd)
		{
			m_lengths.push_back(0);
			m_messageCounts.back()++;
		}
		return 0;
	}
	bool IsolatedFlush(bool hardFlush, bool blocking) {return false;}
	bool IsolatedMessageSeriesEnd(bool blocking)
		{m_messageCounts.push_back(0); return false;}

	lword MaxRetrievable() const
		{return m_lengths.front();}
	bool AnyRetrievable() const
		{return m_lengths.front() > 0;}

	size_t TransferTo2(BufferedTransformation &target, lword &transferBytes, const std::string &channel=DEFAULT_CHANNEL, bool blocking=true);
	size_t CopyRangeTo2(BufferedTransformation &target, lword &begin, lword end=LWORD_MAX, const std::string &channel=DEFAULT_CHANNEL, bool blocking=true) const;

	lword TotalBytesRetrievable() const
		{return m_queue.MaxRetrievable();}
	unsigned int NumberOfMessages() const
		{return (unsigned int)m_lengths.size()-1;}
	bool GetNextMessage();

	unsigned int NumberOfMessagesInThisSeries() const
		{return m_messageCounts[0];}
	unsigned int NumberOfMessageSeries() const
		{return (unsigned int)m_messageCounts.size()-1;}

	unsigned int CopyMessagesTo(BufferedTransformation &target, unsigned int count=UINT_MAX, const std::string &channel=DEFAULT_CHANNEL) const;

	const byte * Spy(size_t &contiguousSize) const;

	void swap(MessageQueue &rhs);

private:
	ByteQueue m_queue;
	std::deque<lword> m_lengths;
	std::deque<unsigned int> m_messageCounts;
};


//! A filter that checks messages on two channels for equality
class CRYPTOPP_DLL EqualityComparisonFilter : public Unflushable<Multichannel<Filter> >
{
public:
	struct MismatchDetected : public Exception {MismatchDetected() : Exception(DATA_INTEGRITY_CHECK_FAILED, "EqualityComparisonFilter: did not receive the same data on two channels") {}};

	/*! if throwIfNotEqual is false, this filter will output a '\\0' byte when it detects a mismatch, '\\1' otherwise */
	EqualityComparisonFilter(BufferedTransformation *attachment=NULL, bool throwIfNotEqual=true, const std::string &firstChannel="0", const std::string &secondChannel="1")
		: m_throwIfNotEqual(throwIfNotEqual), m_mismatchDetected(false)
		, m_firstChannel(firstChannel), m_secondChannel(secondChannel)
		{Detach(attachment);}

	size_t ChannelPut2(const std::string &channel, const byte *begin, size_t length, int messageEnd, bool blocking);
	bool ChannelMessageSeriesEnd(const std::string &channel, int propagation=-1, bool blocking=true);

private:
	unsigned int MapChannel(const std::string &channel) const;
	bool HandleMismatchDetected(bool blocking);

	bool m_throwIfNotEqual, m_mismatchDetected;
	std::string m_firstChannel, m_secondChannel;
	MessageQueue m_q[2];
};

NAMESPACE_END

#ifndef __BORLANDC__
NAMESPACE_BEGIN(std)
template<> inline void swap(CryptoPP::MessageQueue &a, CryptoPP::MessageQueue &b)
{
	a.swap(b);
}
NAMESPACE_END
#endif

#endif

"""

import os
import glob
from pathlib import Path

class MessageQueue:
    # // mqueue.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # MessageQueue::MessageQueue(unsigned int nodeSize)
    # 	: m_queue(nodeSize), m_lengths(1, 0U), m_messageCounts(1, 0U)
    # size_t MessageQueue::CopyRangeTo2(BufferedTransformation &target, lword &begin, lword end, const std::string &channel, bool blocking) const
    # 	if (begin >= MaxRetrievable())
    # 		return 0;
    # 	return m_queue.CopyRangeTo2(target, begin, STDMIN(MaxRetrievable(), end), channel, blocking);
    # size_t MessageQueue::TransferTo2(BufferedTransformation &target, lword &transferBytes, const std::string &channel, bool blocking)
    # 	transferBytes = STDMIN(MaxRetrievable(), transferBytes);
    # 	size_t blockedBytes = m_queue.TransferTo2(target, transferBytes, channel, blocking);
    # 	m_lengths.front() -= transferBytes;
    # 	return blockedBytes;
    # bool MessageQueue::GetNextMessage()
    # 	if (NumberOfMessages() > 0 && !AnyRetrievable())
    # 		m_lengths.pop_front();
    # 		if (m_messageCounts[0] == 0 && m_messageCounts.size() > 1)
    # 			m_messageCounts.pop_front();
    # 		return true;
    # 	else
    # 		return false;
    # unsigned int MessageQueue::CopyMessagesTo(BufferedTransformation &target, unsigned int count, const std::string &channel) const
    # 	unsigned int i;
    # 	for (i=0; i<count && it != --m_lengths.end(); ++i, ++it)
    # 		walker.TransferTo(target, *it, channel);
    # 		if (GetAutoSignalPropagation())
    # 			target.ChannelMessageEnd(channel, GetAutoSignalPropagation()-1);
    # 	return i;
    # void MessageQueue::swap(MessageQueue &rhs)
    # 	m_queue.swap(rhs.m_queue);
    # 	m_lengths.swap(rhs.m_lengths);
    # const byte * MessageQueue::Spy(size_t &contiguousSize) const
    # 	const byte *result = m_queue.Spy(contiguousSize);
    # 	contiguousSize = UnsignedMin(contiguousSize, MaxRetrievable());
    # 	return result;
    # // *************************************************************
    # unsigned int EqualityComparisonFilter::MapChannel(const std::string &channel) const
    # 	if (channel == m_firstChannel)
    # 		return 0;
    # 	else if (channel == m_secondChannel)
    # 		return 1;
    # 	else
    # 		return 2;
    # size_t EqualityComparisonFilter::ChannelPut2(const std::string &channel, const byte *inString, size_t length, int messageEnd, bool blocking)
    # 	if (!blocking)
    # 		throw BlockingInputOnly("EqualityComparisonFilter");
    # 	unsigned int i = MapChannel(channel);
    # 	if (i == 2)
    # 		return Output(3, inString, length, messageEnd, blocking, channel);
    # 	else if (m_mismatchDetected)
    # 		return 0;
    # 	else
    # 		MessageQueue &q1 = m_q[i], &q2 = m_q[1-i];
    # 		if (q2.AnyMessages() && q2.MaxRetrievable() < length)
    # 			goto mismatch;
    # 		while (length > 0 && q2.AnyRetrievable())
    # 			size_t len = length;
    # 			const byte *data = q2.Spy(len);
    # 			len = STDMIN(len, length);
    # 			if (memcmp(inString, data, len) != 0)
    # 				goto mismatch;
    # 			inString += len;
    # 			length -= len;
    # 			q2.Skip(len);
    # 		q1.Put(inString, length);
    # 		if (messageEnd)
    # 			if (q2.AnyRetrievable())
    # 				goto mismatch;
    # 			else if (q2.AnyMessages())
    # 				q2.GetNextMessage();
    # 			else if (q2.NumberOfMessageSeries() > 0)
    # 				goto mismatch;
    # 			else
    # 				q1.MessageEnd();
    # 		return 0;
    # mismatch:
    # 		return HandleMismatchDetected(blocking);
    # bool EqualityComparisonFilter::ChannelMessageSeriesEnd(const std::string &channel, int propagation, bool blocking)
    # 	unsigned int i = MapChannel(channel);
    # 	if (i == 2)
    # 		OutputMessageSeriesEnd(4, propagation, blocking, channel);
    # 		return false;
    # 	else if (m_mismatchDetected)
    # 		return false;
    # 	else
    # 		MessageQueue &q1 = m_q[i], &q2 = m_q[1-i];
    # 		if (q2.AnyRetrievable() || q2.AnyMessages())
    # 			goto mismatch;
    # 		else if (q2.NumberOfMessageSeries() > 0)
    # 			return Output(2, (const byte *)"\1", 1, 0, blocking) != 0;
    # 		else
    # 			q1.MessageSeriesEnd();
    # 		return false;
    # mismatch:
    # 		return HandleMismatchDetected(blocking);
    # bool EqualityComparisonFilter::HandleMismatchDetected(bool blocking)
    # 	m_mismatchDetected = true;
    # 	if (m_throwIfNotEqual)
    # 		throw MismatchDetected();
    # 	return Output(1, (const byte *)"\0", 1, 0, blocking) != 0;
    # NAMESPACE_END
    # #endif