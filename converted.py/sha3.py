"""
sha3.py - Python conversion of sha3.h and sha3.cpp

Original C++ header content:
// sha3.h - written and placed in the public domain by Wei Dai

#ifndef CRYPTOPP_SHA3_H
#define CRYPTOPP_SHA3_H

#include "cryptlib.h"
#include "secblock.h"

NAMESPACE_BEGIN(CryptoPP)

/// <a href="http://en.wikipedia.org/wiki/SHA-3">SHA-3</a>
class SHA3 : public HashTransformation
{
public:
	SHA3(unsigned int digestSize) : m_digestSize(digestSize) {Restart();}
	unsigned int DigestSize() const {return m_digestSize;}
	std::string AlgorithmName() const {return "SHA-3-" + IntToString(m_digestSize*8);}
	unsigned int OptimalDataAlignment() const {return GetAlignmentOf<word64>();}

	void Update(const byte *input, size_t length);
	void Restart();
	void TruncatedFinal(byte *hash, size_t size);

protected:
	inline unsigned int r() const {return 200 - 2 * m_digestSize;}

	FixedSizeSecBlock<word64, 25> m_state;
	unsigned int m_digestSize, m_counter;
};

class SHA3_224 : public SHA3
{
public:
	CRYPTOPP_CONSTANT(DIGESTSIZE = 28)
	SHA3_224() : SHA3(DIGESTSIZE) {}
	static const char * StaticAlgorithmName() {return "SHA-3-224";}
};

class SHA3_256 : public SHA3
{
public:
	CRYPTOPP_CONSTANT(DIGESTSIZE = 32)
	SHA3_256() : SHA3(DIGESTSIZE) {}
	static const char * StaticAlgorithmName() {return "SHA-3-256";}
};

class SHA3_384 : public SHA3
{
public:
	CRYPTOPP_CONSTANT(DIGESTSIZE = 48)
	SHA3_384() : SHA3(DIGESTSIZE) {}
	static const char * StaticAlgorithmName() {return "SHA-3-384";}
};

class SHA3_512 : public SHA3
{
public:
	CRYPTOPP_CONSTANT(DIGESTSIZE = 64)
	SHA3_512() : SHA3(DIGESTSIZE) {}
	static const char * StaticAlgorithmName() {return "SHA-3-512";}
};

NAMESPACE_END

#endif

"""

import os
import glob
from pathlib import Path

class Block:
    # // sha3.cpp - modified by Wei Dai from Ronny Van Keer's public domain Keccak-simple.c
    # // all modifications here are placed in the public domain by Wei Dai
    # /*
    # The Keccak sponge function, designed by Guido Bertoni, Joan Daemen,
    # Michael Peeters and Gilles Van Assche. For more information, feedback or
    # questions, please refer to our website: http://keccak.noekeon.org/
    # Implementation by Ronny Van Keer,
    # hereby denoted as "the implementer".
    # To the extent possible under law, the implementer has waived all copyright
    # and related or neighboring rights to the source code in this file.
    # http://creativecommons.org/publicdomain/zero/1.0/
    # */
    # NAMESPACE_BEGIN(CryptoPP)
    # static const word64 KeccakF_RoundConstants[24] = 
    #     W64LIT(0x0000000000000001), W64LIT(0x0000000000008082), W64LIT(0x800000000000808a),
    #     W64LIT(0x8000000080008000), W64LIT(0x000000000000808b), W64LIT(0x0000000080000001),
    #     W64LIT(0x8000000080008081), W64LIT(0x8000000000008009), W64LIT(0x000000000000008a),
    #     W64LIT(0x0000000000000088), W64LIT(0x0000000080008009), W64LIT(0x000000008000000a),
    #     W64LIT(0x000000008000808b), W64LIT(0x800000000000008b), W64LIT(0x8000000000008089),
    #     W64LIT(0x8000000000008003), W64LIT(0x8000000000008002), W64LIT(0x8000000000000080), 
    #     W64LIT(0x000000000000800a), W64LIT(0x800000008000000a), W64LIT(0x8000000080008081),
    #     W64LIT(0x8000000000008080), W64LIT(0x0000000080000001), W64LIT(0x8000000080008008)
    # };
    # static void KeccakF1600(word64 *state)
    #         word64 Aba, Abe, Abi, Abo, Abu;
    #         word64 Aga, Age, Agi, Ago, Agu;
    #         word64 Aka, Ake, Aki, Ako, Aku;
    #         word64 Ama, Ame, Ami, Amo, Amu;
    #         word64 Asa, Ase, Asi, Aso, Asu;
    #         word64 BCa, BCe, BCi, BCo, BCu;
    #         word64 Da, De, Di, Do, Du;
    #         word64 Eba, Ebe, Ebi, Ebo, Ebu;
    #         word64 Ega, Ege, Egi, Ego, Egu;
    #         word64 Eka, Eke, Eki, Eko, Eku;
    #         word64 Ema, Eme, Emi, Emo, Emu;
    #         word64 Esa, Ese, Esi, Eso, Esu;
    #         //copyFromState(A, state)
    # 		typedef BlockGetAndPut<word64, LittleEndian, true, true> Block;
    @staticmethod
    def Get(self):  # TODO: Add proper parameters and implementation
        # Converted from C++: Block::Get(state)(Aba)(Abe)(Abi)(Abo)(Abu)(Aga)(Age)(Agi)(Ago)(Agu)(Aka)(Ake)(Aki)(Ako)(Aku)(Ama)(Ame)(Ami)(Amo)(Amu)(Asa)(Ase)(Asi)(Aso)(Asu);
        pass
    #         for( unsigned int round = 0; round < 24; round += 2 )
    #             //    prepareTheta
    #             BCa = Aba^Aga^Aka^Ama^Asa;
    #             BCe = Abe^Age^Ake^Ame^Ase;
    #             BCi = Abi^Agi^Aki^Ami^Asi;
    #             BCo = Abo^Ago^Ako^Amo^Aso;
    #             BCu = Abu^Agu^Aku^Amu^Asu;
    #             //thetaRhoPiChiIotaPrepareTheta(round  , A, E)
    #             Da = BCu^rotlFixed(BCe, 1);
    #             De = BCa^rotlFixed(BCi, 1);
    #             Di = BCe^rotlFixed(BCo, 1);
    #             Do = BCi^rotlFixed(BCu, 1);
    #             Du = BCo^rotlFixed(BCa, 1);
    #             Aba ^= Da;
    #             BCa = Aba;
    #             Age ^= De;
    #             BCe = rotlFixed(Age, 44);
    #             Aki ^= Di;
    #             BCi = rotlFixed(Aki, 43);
    #             Amo ^= Do;
    #             BCo = rotlFixed(Amo, 21);
    #             Asu ^= Du;
    #             BCu = rotlFixed(Asu, 14);
    #             Eba =   BCa ^((~BCe)&  BCi );
    #             Eba ^= (word64)KeccakF_RoundConstants[round];
    #             Ebe =   BCe ^((~BCi)&  BCo );
    #             Ebi =   BCi ^((~BCo)&  BCu );
    #             Ebo =   BCo ^((~BCu)&  BCa );
    #             Ebu =   BCu ^((~BCa)&  BCe );
    #             Abo ^= Do;
    #             BCa = rotlFixed(Abo, 28);
    #             Agu ^= Du;
    #             BCe = rotlFixed(Agu, 20);
    #             Aka ^= Da;
    #             BCi = rotlFixed(Aka,  3);
    #             Ame ^= De;
    #             BCo = rotlFixed(Ame, 45);
    #             Asi ^= Di;
    #             BCu = rotlFixed(Asi, 61);
    #             Ega =   BCa ^((~BCe)&  BCi );
    #             Ege =   BCe ^((~BCi)&  BCo );
    #             Egi =   BCi ^((~BCo)&  BCu );
    #             Ego =   BCo ^((~BCu)&  BCa );
    #             Egu =   BCu ^((~BCa)&  BCe );
    #             Abe ^= De;
    #             BCa = rotlFixed(Abe,  1);
    #             Agi ^= Di;
    #             BCe = rotlFixed(Agi,  6);
    #             Ako ^= Do;
    #             BCi = rotlFixed(Ako, 25);
    #             Amu ^= Du;
    #             BCo = rotlFixed(Amu,  8);
    #             Asa ^= Da;
    #             BCu = rotlFixed(Asa, 18);
    #             Eka =   BCa ^((~BCe)&  BCi );
    #             Eke =   BCe ^((~BCi)&  BCo );
    #             Eki =   BCi ^((~BCo)&  BCu );
    #             Eko =   BCo ^((~BCu)&  BCa );
    #             Eku =   BCu ^((~BCa)&  BCe );
    #             Abu ^= Du;
    #             BCa = rotlFixed(Abu, 27);
    #             Aga ^= Da;
    #             BCe = rotlFixed(Aga, 36);
    #             Ake ^= De;
    #             BCi = rotlFixed(Ake, 10);
    #             Ami ^= Di;
    #             BCo = rotlFixed(Ami, 15);
    #             Aso ^= Do;
    #             BCu = rotlFixed(Aso, 56);
    #             Ema =   BCa ^((~BCe)&  BCi );
    #             Eme =   BCe ^((~BCi)&  BCo );
    #             Emi =   BCi ^((~BCo)&  BCu );
    #             Emo =   BCo ^((~BCu)&  BCa );
    #             Emu =   BCu ^((~BCa)&  BCe );
    #             Abi ^= Di;
    #             BCa = rotlFixed(Abi, 62);
    #             Ago ^= Do;
    #             BCe = rotlFixed(Ago, 55);
    #             Aku ^= Du;
    #             BCi = rotlFixed(Aku, 39);
    #             Ama ^= Da;
    #             BCo = rotlFixed(Ama, 41);
    #             Ase ^= De;
    #             BCu = rotlFixed(Ase,  2);
    #             Esa =   BCa ^((~BCe)&  BCi );
    #             Ese =   BCe ^((~BCi)&  BCo );
    #             Esi =   BCi ^((~BCo)&  BCu );
    #             Eso =   BCo ^((~BCu)&  BCa );
    #             Esu =   BCu ^((~BCa)&  BCe );
    #             //    prepareTheta
    #             BCa = Eba^Ega^Eka^Ema^Esa;
    #             BCe = Ebe^Ege^Eke^Eme^Ese;
    #             BCi = Ebi^Egi^Eki^Emi^Esi;
    #             BCo = Ebo^Ego^Eko^Emo^Eso;
    #             BCu = Ebu^Egu^Eku^Emu^Esu;
    #             //thetaRhoPiChiIotaPrepareTheta(round+1, E, A)
    #             Da = BCu^rotlFixed(BCe, 1);
    #             De = BCa^rotlFixed(BCi, 1);
    #             Di = BCe^rotlFixed(BCo, 1);
    #             Do = BCi^rotlFixed(BCu, 1);
    #             Du = BCo^rotlFixed(BCa, 1);
    #             Eba ^= Da;
    #             BCa = Eba;
    #             Ege ^= De;
    #             BCe = rotlFixed(Ege, 44);
    #             Eki ^= Di;
    #             BCi = rotlFixed(Eki, 43);
    #             Emo ^= Do;
    #             BCo = rotlFixed(Emo, 21);
    #             Esu ^= Du;
    #             BCu = rotlFixed(Esu, 14);
    #             Aba =   BCa ^((~BCe)&  BCi );
    #             Aba ^= (word64)KeccakF_RoundConstants[round+1];
    #             Abe =   BCe ^((~BCi)&  BCo );
    #             Abi =   BCi ^((~BCo)&  BCu );
    #             Abo =   BCo ^((~BCu)&  BCa );
    #             Abu =   BCu ^((~BCa)&  BCe );
    #             Ebo ^= Do;
    #             BCa = rotlFixed(Ebo, 28);
    #             Egu ^= Du;
    #             BCe = rotlFixed(Egu, 20);
    #             Eka ^= Da;
    #             BCi = rotlFixed(Eka, 3);
    #             Eme ^= De;
    #             BCo = rotlFixed(Eme, 45);
    #             Esi ^= Di;
    #             BCu = rotlFixed(Esi, 61);
    #             Aga =   BCa ^((~BCe)&  BCi );
    #             Age =   BCe ^((~BCi)&  BCo );
    #             Agi =   BCi ^((~BCo)&  BCu );
    #             Ago =   BCo ^((~BCu)&  BCa );
    #             Agu =   BCu ^((~BCa)&  BCe );
    #             Ebe ^= De;
    #             BCa = rotlFixed(Ebe, 1);
    #             Egi ^= Di;
    #             BCe = rotlFixed(Egi, 6);
    #             Eko ^= Do;
    #             BCi = rotlFixed(Eko, 25);
    #             Emu ^= Du;
    #             BCo = rotlFixed(Emu, 8);
    #             Esa ^= Da;
    #             BCu = rotlFixed(Esa, 18);
    #             Aka =   BCa ^((~BCe)&  BCi );
    #             Ake =   BCe ^((~BCi)&  BCo );
    #             Aki =   BCi ^((~BCo)&  BCu );
    #             Ako =   BCo ^((~BCu)&  BCa );
    #             Aku =   BCu ^((~BCa)&  BCe );
    #             Ebu ^= Du;
    #             BCa = rotlFixed(Ebu, 27);
    #             Ega ^= Da;
    #             BCe = rotlFixed(Ega, 36);
    #             Eke ^= De;
    #             BCi = rotlFixed(Eke, 10);
    #             Emi ^= Di;
    #             BCo = rotlFixed(Emi, 15);
    #             Eso ^= Do;
    #             BCu = rotlFixed(Eso, 56);
    #             Ama =   BCa ^((~BCe)&  BCi );
    #             Ame =   BCe ^((~BCi)&  BCo );
    #             Ami =   BCi ^((~BCo)&  BCu );
    #             Amo =   BCo ^((~BCu)&  BCa );
    #             Amu =   BCu ^((~BCa)&  BCe );
    #             Ebi ^= Di;
    #             BCa = rotlFixed(Ebi, 62);
    #             Ego ^= Do;
    #             BCe = rotlFixed(Ego, 55);
    #             Eku ^= Du;
    #             BCi = rotlFixed(Eku, 39);
    #             Ema ^= Da;
    #             BCo = rotlFixed(Ema, 41);
    #             Ese ^= De;
    #             BCu = rotlFixed(Ese, 2);
    #             Asa =   BCa ^((~BCe)&  BCi );
    #             Ase =   BCe ^((~BCi)&  BCo );
    #             Asi =   BCi ^((~BCo)&  BCu );
    #             Aso =   BCo ^((~BCu)&  BCa );
    #             Asu =   BCu ^((~BCa)&  BCe );
    #         //copyToState(state, A)
    @staticmethod
    def Put(self):  # TODO: Add proper parameters and implementation
        # Converted from C++: Block::Put(NULL, state)(Aba)(Abe)(Abi)(Abo)(Abu)(Aga)(Age)(Agi)(Ago)(Agu)(Aka)(Ake)(Aki)(Ako)(Aku)(Ama)(Ame)(Ami)(Amo)(Amu)(Asa)(Ase)(Asi)(Aso)(Asu);
        pass
    # void SHA3::Update(const byte *input, size_t length)
    # 	size_t spaceLeft;
    # 	while (length >= (spaceLeft = r() - m_counter))
    # 		xorbuf(m_state.BytePtr() + m_counter, input, spaceLeft);
    # 		KeccakF1600(m_state);
    # 		input += spaceLeft;
    # 		length -= spaceLeft;
    # 		m_counter = 0;
    # 	xorbuf(m_state.BytePtr() + m_counter, input, length);
    # 	m_counter += (unsigned int)length;
    # void SHA3::Restart()
    # 	memset(m_state, 0, m_state.SizeInBytes());
    # 	m_counter = 0;
    # void SHA3::TruncatedFinal(byte *hash, size_t size)
    # 	ThrowIfInvalidTruncatedSize(size);
    # 	m_state.BytePtr()[m_counter] ^= 1;
    # 	m_state.BytePtr()[r()-1] ^= 0x80;
    # 	KeccakF1600(m_state);
    # 	memcpy(hash, m_state, size);
    # 	Restart();
    # NAMESPACE_END