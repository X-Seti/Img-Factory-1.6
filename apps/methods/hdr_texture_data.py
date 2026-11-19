#this belongs in apps/methods/hdr_texture_data.py - Version: 1
# X-Seti - November 19 2026 - IMG Factory 1.6 - Hdr texture data (from nebi23)

##Methods list -
# copy_hdr_fields
# make_hdr_preview_rgba
# mark_hdr_preview_modified
# prepare_hdr_export_texture

from copy import deepcopy


def copy_hdr_fields(source, target): #vers 1
    """Copy auxiliary HDR fields without sharing mutable data."""
    if "hdr_data" in source:
        data = source["hdr_data"]
        target["hdr_data"] = data.copy() if hasattr(data, "copy") else deepcopy(data)
    for key in ("hdr_metadata", "hdr_source", "hdr_preview_stale",
                "hdr_tone_operator", "hdr_exposure"):
        if key in source:
            target[key] = deepcopy(source[key])
    return target


def make_hdr_preview_rgba(hdr_result): #vers 1
    """Create a clipped 8-bit UI preview without modifying the HDR source.

    This is deliberately not a tone mapper. Values outside [0, 1] are clipped
    only in this disposable preview; hdr_result['pixel_data'] stays untouched.
    """
    import numpy as np

    pixels = np.asarray(hdr_result["pixel_data"])
    names = hdr_result["channel_names"]
    if pixels.ndim != 3:
        raise ValueError("HDR preview expects HxWxC pixel data.")

    if names in (["Y"], ["Y", "A"]):
        rgb = np.repeat(pixels[:, :, :1], 3, axis=2)
    else:
        rgb = pixels[:, :, :3]

    rgb = np.nan_to_num(rgb, nan=0.0, posinf=1.0, neginf=0.0)
    rgb8 = np.rint(np.clip(rgb, 0.0, 1.0) * 255.0).astype(np.uint8)

    if "A" in names:
        alpha = pixels[:, :, names.index("A"):names.index("A") + 1]
        alpha8 = np.rint(
            np.clip(np.nan_to_num(alpha, nan=0.0, posinf=1.0, neginf=0.0), 0.0, 1.0)
            * 255.0
        ).astype(np.uint8)
    else:
        alpha8 = np.full((*pixels.shape[:2], 1), 255, dtype=np.uint8)

    return np.concatenate((rgb8, alpha8), axis=2).tobytes()


def mark_hdr_preview_modified(texture): #vers 1
    """Mark the RGBA8 preview as diverged from its retained HDR source."""
    if "hdr_data" in texture:
        texture["hdr_preview_stale"] = True
    return texture


def prepare_hdr_export_texture(texture): #vers 1
    """Return a TXD-ready copy, tone-mapping retained HDR data when still current.

    The original texture and HDR array are never modified. A stale HDR preview
    is intentionally ignored so user edits to RGBA8 are not overwritten.
    Existing mipmap depth is retained, but every level is regenerated from the
    newly tone-mapped base image so old LDR mip pixels cannot leak into export.
    """
    import numpy as np
    from copy import copy
    from PIL import Image
    from apps.methods.hdr_tone_mapping import tone_map

    if "hdr_data" not in texture or texture.get("hdr_preview_stale", False):
        return texture

    pixels = np.asarray(texture["hdr_data"])
    if pixels.ndim != 3 or pixels.shape[2] not in (1, 2, 3, 4):
        raise ValueError("HDR export data must have HxWxC dimensions and 1-4 channels.")

    metadata = texture.get("hdr_metadata") or {}
    channel_names = metadata.get("channels")
    if channel_names is None:
        channel_names = {
            1: ["Y"], 2: ["Y", "A"], 3: ["R", "G", "B"], 4: ["R", "G", "B", "A"]
        }[pixels.shape[2]]

    target_width = int(texture.get("width") or 0)
    target_height = int(texture.get("height") or 0)
    if target_width <= 0 or target_height <= 0:
        raise ValueError("HDR export target dimensions must be positive.")

    if pixels.shape[:2] != (target_height, target_width):
        resized = []
        for channel in range(pixels.shape[2]):
            plane = Image.fromarray(pixels[:, :, channel].astype(np.float32), mode="F")
            plane = plane.resize((target_width, target_height), Image.Resampling.BICUBIC)
            resized.append(np.asarray(plane, dtype=np.float32))
        pixels = np.stack(resized, axis=2)

    operator = texture.get("hdr_tone_operator", "reinhard")
    exposure = texture.get("hdr_exposure", 0.0)
    mapped = tone_map(pixels, operator=operator, exposure=exposure,
                      channel_names=channel_names)

    has_alpha = "A" in channel_names
    color_count = mapped.shape[2] - int(has_alpha)
    linear = np.clip(mapped[:, :, :color_count], 0.0, 1.0)
    if channel_names in (["Y"], ["Y", "A"]):
        linear = np.repeat(linear, 3, axis=2)
    srgb = np.where(
        linear <= 0.0031308,
        linear * 12.92,
        1.055 * np.power(linear, 1.0 / 2.4) - 0.055,
    )
    rgb8 = np.rint(np.clip(srgb, 0.0, 1.0) * 255.0).astype(np.uint8)

    if has_alpha:
        alpha8 = np.rint(
            np.clip(mapped[:, :, color_count], 0.0, 1.0) * 255.0
        ).astype(np.uint8)[:, :, None]
    else:
        alpha8 = np.full((target_height, target_width, 1), 255, dtype=np.uint8)

    prepared = copy(texture)
    prepared["rgba_data"] = np.concatenate((rgb8, alpha8), axis=2).tobytes()

    # Keep the requested mipmap count, rebuilding every level from the
    # tone-mapped base image instead of retaining levels from an older preview.
    source_levels = texture.get("mipmap_levels") or []
    level_count = max(1, len(source_levels))
    if level_count > 1:
        base = Image.frombytes(
            "RGBA", (target_width, target_height), prepared["rgba_data"]
        )
        levels = []
        width, height = target_width, target_height
        for level in range(level_count):
            image = base if level == 0 else base.resize(
                (width, height), Image.Resampling.BOX
            )
            rgba = image.tobytes()
            levels.append({
                "level": level,
                "width": width,
                "height": height,
                "rgba_data": rgba,
                "compressed_data": None,
                "compressed_size": len(rgba),
            })
            if width == 1 and height == 1:
                break
            width, height = max(1, width // 2), max(1, height // 2)
        prepared["mipmap_levels"] = levels
        prepared["mipmaps"] = len(levels)
    else:
        prepared["mipmap_levels"] = []
        prepared["mipmaps"] = 1

    return prepared
