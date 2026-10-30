#!/usr/bin/env python3
#this belongs in apps/methods/pvrtc_decode.py - Version: 3
# X-Seti - October05 2026 - IMG Factory 1.6 - PVRTC decode entry point

"""
PVRTC 2bpp/4bpp decode wrappers around the txd_lc_android codec.
"""

# Codec: Morton block order, bilinear colours, checkerboard 2bpp modes.


##Methods list -
# decode_pvrtc
# decode_pvrtc2


def decode_pvrtc(data, width, height, bpp): #vers 1
    """Decode one PVRTC level to RGBA bytes."""
    from apps.methods.txd_lc_android import _decode_pvrtc
    return _decode_pvrtc(bytes(data), width, height, bpp).tobytes()


def decode_pvrtc2(data, width, height): #vers 3
    """Decode one PVRTC 2bpp level to RGBA bytes."""
    return decode_pvrtc(data, width, height, 2)


__all__ = ['decode_pvrtc', 'decode_pvrtc2']
