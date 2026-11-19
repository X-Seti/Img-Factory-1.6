#this belongs in apps/methods/hdr_tone_mapping.py - Version: 1
# X-Seti - November 19 2026 - IMG Factory 1.6 - Hdr tone mapping (from nebi23)

"""Convert retained HDR pixels to normalized LDR values for later encoding.

The source array is never modified. Output RGB/luminance values are float32
in the [0, 1] range and remain linear; display transfer functions and TXD
encoding are intentionally handled by later export stages.
"""

##Methods list -
# tone_map


import numpy as np


SUPPORTED_OPERATORS = ("reinhard", "aces")
_MAX_TONE_MAP_INPUT = 1.0e20


def tone_map(hdr_pixels, operator="reinhard", exposure=0.0, channel_names=None): #vers 1
    """Tone-map HxWxC HDR pixels into a new float32 LDR array.

    Args:
        hdr_pixels: HxWxC floating-point NumPy-compatible pixel data.
        operator: "reinhard" or "aces".
        exposure: Exposure adjustment in stops, applied before tone mapping.
        channel_names: Optional channel names. If omitted, 2-channel data is
            treated as luminance+alpha and 4-channel data as RGBA.

    Returns:
        A new HxWxC float32 array. Color channels are normalized to [0, 1].
        Alpha, when present, is sanitized and clipped to [0, 1] without
        exposure or tone mapping.
    """
    if operator not in SUPPORTED_OPERATORS:
        raise ValueError(
            f"Unsupported tone-mapping operator: {operator}. "
            f"Choose one of: {', '.join(SUPPORTED_OPERATORS)}."
        )
    try:
        exposure = float(exposure)
    except (TypeError, ValueError) as exc:
        raise ValueError("Exposure must be a finite number of stops.") from exc
    if not np.isfinite(exposure) or not -32.0 <= exposure <= 32.0:
        raise ValueError("Exposure must be finite and between -32 and +32 stops.")

    pixels = np.asarray(hdr_pixels)
    if pixels.ndim != 3:
        raise ValueError("HDR pixels must have HxWxC dimensions.")
    if pixels.dtype.kind != "f":
        raise ValueError("HDR pixels must use a floating-point dtype.")

    channel_count = pixels.shape[2]
    if channel_names is None:
        if channel_count == 1:
            channel_names = ["Y"]
        elif channel_count == 2:
            channel_names = ["Y", "A"]
        elif channel_count == 3:
            channel_names = ["R", "G", "B"]
        elif channel_count == 4:
            channel_names = ["R", "G", "B", "A"]
        else:
            raise ValueError("HDR pixels must have 1, 2, 3, or 4 channels.")
    else:
        channel_names = list(channel_names)
        if len(channel_names) != channel_count:
            raise ValueError("Channel names must match the pixel channel count.")
        if channel_names not in (
            ["Y"], ["Y", "A"], ["R", "G", "B"], ["R", "G", "B", "A"]
        ):
            raise ValueError("Supported channels are Y, YA, RGB, and RGBA.")

    has_alpha = "A" in channel_names
    color_count = channel_count - int(has_alpha)

    # Float64 plus a conservative upper bound avoids overflow even for
    # float64 input, +Inf, and the maximum supported exposure (+32 stops).
    colors = np.nan_to_num(
        pixels[:, :, :color_count].astype(np.float64, copy=True),
        nan=0.0, posinf=_MAX_TONE_MAP_INPUT, neginf=0.0,
    )
    colors = np.clip(colors, 0.0, _MAX_TONE_MAP_INPUT)
    colors *= 2.0 ** exposure

    if operator == "reinhard":
        mapped = colors / (1.0 + colors)
    else:
        # ACES fitted approximation (Narkowicz), evaluated in float64.
        a, b, c, d, e = (2.51, 0.03, 2.43, 0.59, 0.14)
        mapped = (colors * (a * colors + b)) / (
            colors * (c * colors + d) + e
        )

    mapped = np.clip(mapped, 0.0, 1.0).astype(np.float32)
    if has_alpha:
        alpha = np.nan_to_num(
            pixels[:, :, color_count:color_count + 1].astype(np.float32, copy=True),
            nan=0.0, posinf=1.0, neginf=0.0,
        )
        alpha = np.clip(alpha, 0.0, 1.0)
        mapped = np.concatenate((mapped, alpha), axis=2)

    return mapped.astype(np.float32, copy=False)
