import struct
import unittest

import numpy as np

from apps.methods.txd_splice import _SA_VER, _native_parts, build_native_chunk


def _encoded_argb8888_pixel(texture):
    chunk = build_native_chunk(texture, _SA_VER, platform=9)
    _version, children = _native_parts(chunk)
    native_struct = next(payload for kind, payload in children if kind == 1)
    pixel_offset = 4 + 4 + 32 + 32 + struct.calcsize("<IIHHBBBB") + 4
    return native_struct[pixel_offset:pixel_offset + 4]


class TxdHdrExportIntegrationTests(unittest.TestCase):
    def _base_texture(self):
        return {
            "name": "hdr_test",
            "width": 1,
            "height": 1,
            "depth": 32,
            "format": "ARGB8888",
            "rgba_data": bytes([10, 20, 30, 40]),
            "has_alpha": True,
        }

    def test_fresh_hdr_data_is_used_by_txd_writer(self):
        texture = self._base_texture()
        texture.update({
            "hdr_data": np.array([[[1.0, 0.0, 0.0]]], dtype=np.float32),
            "hdr_metadata": {"channels": ["R", "G", "B"]},
            "hdr_preview_stale": False,
        })

        encoded = _encoded_argb8888_pixel(texture)

        # Reinhard maps linear red 1.0 to 0.5; sRGB encoding is about 188.
        self.assertGreater(encoded[2], 180)
        self.assertLess(encoded[0], 5)
        self.assertEqual(encoded[3], 255)
        self.assertEqual(texture["rgba_data"], bytes([10, 20, 30, 40]))

    def test_stale_hdr_data_does_not_overwrite_edited_rgba(self):
        texture = self._base_texture()
        texture.update({
            "hdr_data": np.array([[[1.0, 0.0, 0.0]]], dtype=np.float32),
            "hdr_metadata": {"channels": ["R", "G", "B"]},
            "hdr_preview_stale": True,
        })

        self.assertEqual(_encoded_argb8888_pixel(texture), bytes([30, 20, 10, 40]))

    def test_ordinary_ldr_texture_keeps_existing_export_path(self):
        texture = self._base_texture()

        self.assertEqual(_encoded_argb8888_pixel(texture), bytes([30, 20, 10, 40]))


if __name__ == "__main__":
    unittest.main()
