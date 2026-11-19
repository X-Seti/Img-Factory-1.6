#this belongs in apps/methods/float_image_loader.py - Version: 1
# X-Seti - November 19 2026 - IMG Factory 1.6 - Float image loader (from nebi23)

"""High-precision image loading for HDR workflows.

This module deliberately does not tone-map, normalize, clip, gamma-correct,
or convert floating-point pixel values to an 8-bit representation.
"""

##Methods list -
# _build_result
# _exr_color_space
# _load_exr
# _load_tiff
# _safe_metadata
# _tiff_channel_names
# _tiff_color_space
# _tiff_extra_sample_name
# load_float_image
#
##class FloatImageLoadError: -


from pathlib import Path

import numpy as np


class FloatImageLoadError(ValueError):
    """Raised when an image cannot be loaded as floating-point pixel data."""


def load_float_image(file_path): #vers 1
    """Load a floating-point EXR or TIFF image without changing its values.

    Returns a dictionary containing an HxWxC NumPy array in pixel_data.
    The array retains the source floating-point dtype where the source format
    exposes one consistently (EXR HALF remains float16; TIFF FLOAT32 remains
    float32). No tone mapping or display conversion is performed.
    """
    path = Path(file_path)
    if not path.is_file():
        raise FloatImageLoadError(f"Image file not found: {path}")

    extension = path.suffix.lower()
    if extension == ".exr":
        return _load_exr(path)
    if extension in (".tif", ".tiff"):
        return _load_tiff(path)

    raise FloatImageLoadError(
        f"Unsupported floating-point image format: {extension or '(no extension)'}"
    )


def _load_exr(path): #vers 1
    try:
        import OpenEXR
    except ImportError as exc:
        raise FloatImageLoadError(
            "EXR support requires the OpenEXR Python package. "
            "Install it with: pip install OpenEXR"
        ) from exc

    try:
        exr_file = OpenEXR.File(str(path), separate_channels=True)
        if len(exr_file.parts) != 1:
            raise FloatImageLoadError(
                "Multi-part EXR files are not supported; export a single-part EXR."
            )
        header = exr_file.header()
        source_channels = exr_file.channels()
        channel_groups = {}

        for name, channel in source_channels.items():
            if "." in name:
                layer, channel_name = name.rsplit(".", 1)
            else:
                layer, channel_name = "", name
            if channel_name in ("R", "G", "B", "A", "Y"):
                channel_groups.setdefault(layer, {})[channel_name] = channel.pixels

        def supported_channels(group): #vers 1
            if all(name in group for name in ("R", "G", "B")):
                names = ["R", "G", "B"]
            elif "Y" in group:
                names = ["Y"]
            else:
                return None
            if "A" in group:
                names.append("A")
            return names

        candidates = [
            (layer, group, supported_channels(group))
            for layer, group in channel_groups.items()
            if supported_channels(group) is not None
        ]
        if not candidates:
            raise FloatImageLoadError(
                "EXR must contain RGB channels or a luminance Y channel."
            )

        # Prefer the conventional unlayered channels. Otherwise, only accept
        # a single usable layer rather than silently mixing or guessing.
        unlayered = [item for item in candidates if item[0] == ""]
        if unlayered:
            selected_layer, channel_map, channel_names = unlayered[0]
        elif len(candidates) == 1:
            selected_layer, channel_map, channel_names = candidates[0]
        else:
            layers = ", ".join(item[0] for item in candidates)
            raise FloatImageLoadError(
                f"EXR contains multiple usable channel layers ({layers}); "
                "select or flatten one layer before importing."
            )

        arrays = [np.asarray(channel_map[name]) for name in channel_names]
        if any(array.ndim != 2 for array in arrays):
            raise FloatImageLoadError("Multipart or deep EXR channel data is not supported.")

        shape = arrays[0].shape
        if any(array.shape != shape for array in arrays):
            raise FloatImageLoadError("EXR channels do not share the same dimensions.")

        dtypes = {array.dtype for array in arrays}
        if len(dtypes) != 1:
            dtype_names = ", ".join(sorted(str(dtype) for dtype in dtypes))
            raise FloatImageLoadError(
                "EXR channels use mixed pixel data types "
                f"({dtype_names}); convert them to one floating-point type first."
            )

        pixel_data = np.stack(arrays, axis=-1)
        if pixel_data.dtype.kind != "f":
            raise FloatImageLoadError(
                f"EXR channels are not floating-point data (dtype: {pixel_data.dtype})."
            )

        color_space = _exr_color_space(header)
        ignored_channels = [
            name for name in source_channels
            if name not in {
                f"{selected_layer}.{channel_name}" if selected_layer else channel_name
                for channel_name in channel_names
            }
        ]
        metadata = {
            "header": _safe_metadata(header),
            "channel_names": channel_names,
            "selected_layer": selected_layer or None,
            "ignored_channels": ignored_channels,
        }
        return _build_result(
            pixel_data, "EXR", channel_names, color_space, metadata
        )
    except FloatImageLoadError:
        raise
    except Exception as exc:
        raise FloatImageLoadError(f"Could not read EXR image '{path.name}': {exc}") from exc


def _load_tiff(path): #vers 1
    try:
        import tifffile
    except ImportError as exc:
        raise FloatImageLoadError(
            "TIFF support requires the tifffile package. "
            "Install it with: pip install tifffile"
        ) from exc

    try:
        with tifffile.TiffFile(path) as tif:
            if not tif.series:
                raise FloatImageLoadError("TIFF file contains no image series.")
            series = tif.series[0]
            if len(tif.series) != 1 or len(series.pages) != 1:
                raise FloatImageLoadError(
                    "Multi-page or multi-series TIFF files are not supported."
                )

            pixel_data = series.asarray()
            axes = series.axes
            page = series.pages[0]
            metadata = {
                "axes": axes,
                "photometric": str(page.photometric),
                "samples_per_pixel": int(page.samplesperpixel),
                "planar_configuration": (
                    int(page.planarconfig) if page.planarconfig is not None else None
                ),
                "bits_per_sample": page.bitspersample,
                "compression": str(page.compression),
                "extra_samples": [
                    _tiff_extra_sample_name(sample)
                    for sample in (page.extrasamples or ())
                ],
            }

            if pixel_data.dtype.kind != "f":
                raise FloatImageLoadError(
                    "TIFF must contain floating-point samples; "
                    f"found {pixel_data.dtype}."
                )

            if pixel_data.ndim == 2 and axes.endswith("YX"):
                if page.samplesperpixel != 1 or page.photometric not in (0, 1):
                    raise FloatImageLoadError(
                        "Unsupported single-channel TIFF photometric layout: "
                        f"photometric={page.photometric}, "
                        f"samples_per_pixel={page.samplesperpixel}."
                    )
                channel_names = _tiff_channel_names(
                    1, page.extrasamples, page.photometric
                )
                pixel_data = pixel_data[..., np.newaxis]
            elif pixel_data.ndim == 3 and axes.endswith("YXS"):
                channel_count = pixel_data.shape[-1]
                if page.planarconfig != 1:
                    raise FloatImageLoadError(
                        "TIFF YXS layout requires contiguous planar configuration "
                        f"(found {page.planarconfig})."
                    )
                if page.samplesperpixel != channel_count:
                    raise FloatImageLoadError(
                        "TIFF sample count does not match its pixel layout: "
                        f"metadata={page.samplesperpixel}, data={channel_count}."
                    )
                channel_names = _tiff_channel_names(
                    channel_count, page.extrasamples, page.photometric
                )
            elif pixel_data.ndim == 3 and axes.endswith("SYX"):
                if page.planarconfig != 2:
                    raise FloatImageLoadError(
                        "TIFF SYX layout requires separate planar configuration "
                        f"(found {page.planarconfig})."
                    )
                pixel_data = np.moveaxis(pixel_data, 0, -1)
                channel_count = pixel_data.shape[-1]
                if page.samplesperpixel != channel_count:
                    raise FloatImageLoadError(
                        "TIFF sample count does not match its pixel layout: "
                        f"metadata={page.samplesperpixel}, data={channel_count}."
                    )
                channel_names = _tiff_channel_names(
                    channel_count, page.extrasamples, page.photometric
                )
            else:
                raise FloatImageLoadError(
                    f"Unsupported TIFF image layout: axes={axes}, "
                    f"shape={pixel_data.shape}."
                )

            if not pixel_data.flags.c_contiguous:
                pixel_data = np.ascontiguousarray(pixel_data)

            color_space = _tiff_color_space(page)
            return _build_result(
                pixel_data, "TIFF", channel_names, color_space, metadata
            )
    except FloatImageLoadError:
        raise
    except Exception as exc:
        raise FloatImageLoadError(f"Could not read TIFF image '{path.name}': {exc}") from exc


def _tiff_extra_sample_name(sample): #vers 1
    return getattr(sample, "name", str(sample)).upper()


def _tiff_channel_names(channel_count, extra_samples=(), photometric=None): #vers 1
    extra_names = [
        _tiff_extra_sample_name(sample) for sample in (extra_samples or ())
    ]
    has_alpha = (
        len(extra_names) == 1
        and extra_names[0] in {"ASSOCALPHA", "UNASSALPHA"}
    )

    if channel_count == 1 and photometric in (0, 1) and not extra_names:
        return ["Y"]
    if channel_count == 2 and photometric == 1 and has_alpha:
        return ["Y", "A"]
    if channel_count == 3 and photometric == 2 and not extra_names:
        return ["R", "G", "B"]
    if channel_count == 4 and photometric == 2 and has_alpha:
        return ["R", "G", "B", "A"]

    raise FloatImageLoadError(
        "Unsupported TIFF channel layout: "
        f"{channel_count} samples, photometric={photometric}, "
        f"extra_samples={extra_names}. "
        "Expected grayscale, grayscale+alpha, RGB, or RGBA."
    )


def _exr_color_space(header): #vers 1
    chromaticities = header.get("chromaticities")
    if chromaticities is not None:
        return "chromaticities"
    return None


def _tiff_color_space(page): #vers 1
    if "InterColorProfile" in page.tags:
        return "ICC profile"
    return None


def _safe_metadata(header): #vers 1
    metadata = {}
    for key, value in header.items():
        try:
            metadata[str(key)] = str(value)
        except Exception:
            metadata[str(key)] = repr(value)
    return metadata


def _build_result(pixel_data, source_format, channel_names, color_space, metadata): #vers 1
    height, width, channels = pixel_data.shape
    finite_values = pixel_data[np.isfinite(pixel_data)]
    value_range = (
        (float(finite_values.min()), float(finite_values.max()))
        if finite_values.size
        else (float("nan"), float("nan"))
    )
    return {
        "width": int(width),
        "height": int(height),
        "channels": int(channels),
        "dtype": str(pixel_data.dtype),
        "pixel_data": pixel_data,
        "has_alpha": "A" in channel_names,
        "channel_names": list(channel_names),
        "color_space": color_space,
        "source_format": source_format,
        "value_range": value_range,
        "metadata": metadata,
    }
