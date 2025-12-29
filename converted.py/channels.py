"""
channels.py - Python conversion of channels.h and channels.cpp

Original C++ header content:
#ifndef CRYPTOPP_CHANNELS_H
#define CRYPTOPP_CHANNELS_H

#include "simple.h"
#include "smartptr.h"
#include <map>
#include <list>

NAMESPACE_BEGIN(CryptoPP)

#if 0
//! Route input on default channel to different and/or multiple channels based on message sequence number
class MessageSwitch : public Sink
{
public:
	void AddDefaultRoute(BufferedTransformation &destination, const std::string &channel);
	void AddRoute(unsigned int begin, unsigned int end, BufferedTransformation &destination, const std::string &channel);

	void Put(byte inByte);
	void Put(const byte *inString, unsigned int length);

	void Flush(bool completeFlush, int propagation=-1);
	void MessageEnd(int propagation=-1);
	void PutMessageEnd(const byte *inString, unsigned int length, int propagation=-1);
	void MessageSeriesEnd(int propagation=-1);

private:
	typedef std::pair<BufferedTransformation *, std::string> Route;
	struct RangeRoute
	{
		RangeRoute(unsigned int begin, unsigned int end, const Route &route)
			: begin(begin), end(end), route(route) {}
		bool operator<(const RangeRoute &rhs) const {return begin < rhs.begin;}
		unsigned int begin, end;
		Route route;
	};

	typedef std::list<RangeRoute> RouteList;
	typedef std::list<Route> DefaultRouteList;

	RouteList m_routes;
	DefaultRouteList m_defaultRoutes;
	unsigned int m_nCurrentMessage;
};
#endif

class ChannelSwitchTypedefs
{
public:
	typedef std::pair<BufferedTransformation *, std::string> Route;
	typedef std::multimap<std::string, Route> RouteMap;

	typedef std::pair<BufferedTransformation *, value_ptr<std::string> > DefaultRoute;
	typedef std::list<DefaultRoute> DefaultRouteList;

	// SunCC workaround: can't use const_iterator here
	typedef RouteMap::iterator MapIterator;
	typedef DefaultRouteList::iterator ListIterator;
};

class ChannelSwitch;

class ChannelRouteIterator : public ChannelSwitchTypedefs
{
public:
	ChannelSwitch& m_cs;
	std::string m_channel;
	bool m_useDefault;
	MapIterator m_itMapCurrent, m_itMapEnd;
	ListIterator m_itListCurrent, m_itListEnd;

	ChannelRouteIterator(ChannelSwitch &cs) : m_cs(cs) {}
	void Reset(const std::string &channel);
	bool End() const;
	void Next();
	BufferedTransformation & Destination();
	const std::string & Channel();
};

//! Route input to different and/or multiple channels based on channel ID
class CRYPTOPP_DLL ChannelSwitch : public Multichannel<Sink>, public ChannelSwitchTypedefs
{
public:
	ChannelSwitch() : m_it(*this), m_blocked(false) {}
	ChannelSwitch(BufferedTransformation &destination) : m_it(*this), m_blocked(false)
	{
		AddDefaultRoute(destination);
	}
	ChannelSwitch(BufferedTransformation &destination, const std::string &outChannel) : m_it(*this), m_blocked(false)
	{
		AddDefaultRoute(destination, outChannel);
	}

	void IsolatedInitialize(const NameValuePairs &parameters=g_nullNameValuePairs);

	size_t ChannelPut2(const std::string &channel, const byte *begin, size_t length, int messageEnd, bool blocking);
	size_t ChannelPutModifiable2(const std::string &channel, byte *begin, size_t length, int messageEnd, bool blocking);

	bool ChannelFlush(const std::string &channel, bool completeFlush, int propagation=-1, bool blocking=true);
	bool ChannelMessageSeriesEnd(const std::string &channel, int propagation=-1, bool blocking=true);

	byte * ChannelCreatePutSpace(const std::string &channel, size_t &size);
	
	void AddDefaultRoute(BufferedTransformation &destination);
	void RemoveDefaultRoute(BufferedTransformation &destination);
	void AddDefaultRoute(BufferedTransformation &destination, const std::string &outChannel);
	void RemoveDefaultRoute(BufferedTransformation &destination, const std::string &outChannel);
	void AddRoute(const std::string &inChannel, BufferedTransformation &destination, const std::string &outChannel);
	void RemoveRoute(const std::string &inChannel, BufferedTransformation &destination, const std::string &outChannel);

private:
	RouteMap m_routeMap;
	DefaultRouteList m_defaultRoutes;

	ChannelRouteIterator m_it;
	bool m_blocked;

	friend class ChannelRouteIterator;
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class MessageSwitch:
    # // channels.cpp - written and placed in the public domain by Wei Dai
    # #ifndef CRYPTOPP_IMPORTS
    # NAMESPACE_BEGIN(CryptoPP)
    # USING_NAMESPACE(std)
    # #if 0
    # void MessageSwitch::AddDefaultRoute(BufferedTransformation &destination, const std::string &channel)
    # 	m_defaultRoutes.push_back(Route(&destination, channel));
    # void MessageSwitch::AddRoute(unsigned int begin, unsigned int end, BufferedTransformation &destination, const std::string &channel)
    # 	RangeRoute route(begin, end, Route(&destination, channel));
    # 	m_routes.insert(it, route);
    # /*
    # class MessageRouteIterator
    # public:
    # 	bool m_useDefault;
    # 	RouteIterator m_itRouteCurrent, m_itRouteEnd;
    # 	DefaultIterator m_itDefaultCurrent, m_itDefaultEnd;
    # 	MessageRouteIterator(MessageSwitch &ms, const std::string &channel)
    # 		: m_channel(channel)
    # 		pair<MapIterator, MapIterator> range = cs.m_routeMap.equal_range(channel);
    # 		if (range.first == range.second)
    # 			m_useDefault = true;
    # 			m_itListCurrent = cs.m_defaultRoutes.begin();
    # 			m_itListEnd = cs.m_defaultRoutes.end();
    # 		else
    # 			m_useDefault = false;
    # 			m_itMapCurrent = range.first;
    # 			m_itMapEnd = range.second;
    # 	bool End() const
    # 		return m_useDefault ? m_itListCurrent == m_itListEnd : m_itMapCurrent == m_itMapEnd;
    # 	void Next()
    # 		if (m_useDefault)
    # 			++m_itListCurrent;
    # 		else
    # 			++m_itMapCurrent;
    # 	BufferedTransformation & Destination()
    # 		return m_useDefault ? *m_itListCurrent->first : *m_itMapCurrent->second.first;
    # 	const std::string & Message()
    # 		if (m_useDefault)
    # 			return m_itListCurrent->second.get() ? *m_itListCurrent->second.get() : m_channel;
    # 		else
    # 			return m_itMapCurrent->second.second;
    # };
    # */
    # #endif
    # //
    # // ChannelRouteIterator
    # //////////////////////////
    # void ChannelRouteIterator::Reset(const std::string &channel)
    # 	m_channel = channel;
    # 	pair<MapIterator, MapIterator> range = m_cs.m_routeMap.equal_range(channel);
    # 	if (range.first == range.second)
    # 		m_useDefault = true;
    # 		m_itListCurrent = m_cs.m_defaultRoutes.begin();
    # 		m_itListEnd = m_cs.m_defaultRoutes.end();
    # 	else
    # 		m_useDefault = false;
    # 		m_itMapCurrent = range.first;
    # 		m_itMapEnd = range.second;
    # bool ChannelRouteIterator::End() const
    # 	return m_useDefault ? m_itListCurrent == m_itListEnd : m_itMapCurrent == m_itMapEnd;
    # void ChannelRouteIterator::Next()
    # 	if (m_useDefault)
    # 		++m_itListCurrent;
    # 	else
    # 		++m_itMapCurrent;
    # BufferedTransformation & ChannelRouteIterator::Destination()
    # 	return m_useDefault ? *m_itListCurrent->first : *m_itMapCurrent->second.first;
    # const std::string & ChannelRouteIterator::Channel()
    # 	if (m_useDefault)
    # 		return m_itListCurrent->second.get() ? *m_itListCurrent->second.get() : m_channel;
    # 	else
    # 		return m_itMapCurrent->second.second;
    # //
    # // ChannelSwitch
    # ///////////////////
    # size_t ChannelSwitch::ChannelPut2(const std::string &channel, const byte *begin, size_t length, int messageEnd, bool blocking)
    # 	if (m_blocked)
    # 		m_blocked = false;
    # 		goto WasBlocked;
    # 	m_it.Reset(channel);
    # 	while (!m_it.End())
    # WasBlocked:
    # 		if (m_it.Destination().ChannelPut2(m_it.Channel(), begin, length, messageEnd, blocking))
    # 			m_blocked = true;
    # 			return 1;
    # 		m_it.Next();
    # 	return 0;
    # void ChannelSwitch::IsolatedInitialize(const NameValuePairs &parameters/* =g_nullNameValuePairs */)
    # 	m_routeMap.clear();
    # 	m_defaultRoutes.clear();
    # 	m_blocked = false;
    # bool ChannelSwitch::ChannelFlush(const std::string &channel, bool completeFlush, int propagation, bool blocking)
    # 	if (m_blocked)
    # 		m_blocked = false;
    # 		goto WasBlocked;
    # 	m_it.Reset(channel);
    # 	while (!m_it.End())
    # 	  WasBlocked:
    # 		if (m_it.Destination().ChannelFlush(m_it.Channel(), completeFlush, propagation, blocking))
    # 			m_blocked = true;
    # 			return true;
    # 		m_it.Next();
    # 	return false;
    # bool ChannelSwitch::ChannelMessageSeriesEnd(const std::string &channel, int propagation, bool blocking)
    # 	if (m_blocked)
    # 		m_blocked = false;
    # 		goto WasBlocked;
    # 	m_it.Reset(channel);
    # 	while (!m_it.End())
    # 	  WasBlocked:
    # 		if (m_it.Destination().ChannelMessageSeriesEnd(m_it.Channel(), propagation))
    # 			m_blocked = true;
    # 			return true;
    # 		m_it.Next();
    # 	return false;
    # byte * ChannelSwitch::ChannelCreatePutSpace(const std::string &channel, size_t &size)
    # 	m_it.Reset(channel);
    # 	if (!m_it.End())
    # 		BufferedTransformation &target = m_it.Destination();
    # 		m_it.Next();
    # 		if (m_it.End())	// there is only one target channel
    # 			return target.ChannelCreatePutSpace(channel, size);
    # 	size = 0;
    # 	return NULL;
    # size_t ChannelSwitch::ChannelPutModifiable2(const std::string &channel, byte *inString, size_t length, int messageEnd, bool blocking)
    # 	ChannelRouteIterator it(*this);
    # 	it.Reset(channel);
    # 	if (!it.End())
    # 		BufferedTransformation &target = it.Destination();
    # 		it.Next();
    # 		if (it.End())	// there is only one target channel
    # 			return target.ChannelPutModifiable2(targetChannel, inString, length, messageEnd, blocking);
    # 	return ChannelPut2(channel, inString, length, messageEnd, blocking);
    # void ChannelSwitch::AddDefaultRoute(BufferedTransformation &destination)
    # void ChannelSwitch::RemoveDefaultRoute(BufferedTransformation &destination)
    # 	for (DefaultRouteList::iterator it = m_defaultRoutes.begin(); it != m_defaultRoutes.end(); ++it)
    # 		if (it->first == &destination && !it->second.get())
    # 			m_defaultRoutes.erase(it);
    # 			break;
    # void ChannelSwitch::AddDefaultRoute(BufferedTransformation &destination, const std::string &outChannel)
    # 	m_defaultRoutes.push_back(DefaultRoute(&destination, outChannel));
    # void ChannelSwitch::RemoveDefaultRoute(BufferedTransformation &destination, const std::string &outChannel)
    # 	for (DefaultRouteList::iterator it = m_defaultRoutes.begin(); it != m_defaultRoutes.end(); ++it)
    # 		if (it->first == &destination && (it->second.get() && *it->second == outChannel))
    # 			m_defaultRoutes.erase(it);
    # 			break;
    # void ChannelSwitch::AddRoute(const std::string &inChannel, BufferedTransformation &destination, const std::string &outChannel)
    # void ChannelSwitch::RemoveRoute(const std::string &inChannel, BufferedTransformation &destination, const std::string &outChannel)
    # 	pair<MapIterator, MapIterator> range = m_routeMap.equal_range(inChannel);
    # 	for (MapIterator it = range.first; it != range.second; ++it)
    # 		if (it->second.first == &destination && it->second.second == outChannel)
    # 			m_routeMap.erase(it);
    # 			break;
    # NAMESPACE_END
    # #endif