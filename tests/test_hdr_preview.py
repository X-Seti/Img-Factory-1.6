import unittest

import numpy as np

from apps.methods.hdr_texture_data import make_hdr_preview_rgba


class HdrPreviewTests(unittest.TestCase):
    def test_rgb_preview_does_not_modify_hdr_pixels(self):
        pixels = np.array([[[1.5, 0.5, -0.25]]], dtype=np.float32)
        result = {
            "pixel_data": pixels,
            "channel_names": ["R", "G", "B"],
        }
        preview = make_hdr_preview_rgba(result)
        self.assertEqual(list(preview), [255, 128, 0, 255])
        self.assertEqual(float(pixels[0, 0, 0]), 1.5)
        self.assertEqual(float(pixels[0, 0, 2]), -0.25)

    def test_grayscale_preview_is_replicated(self):
        result = {
            "pixel_data": np.array([[[0.25]]], dtype=np.float32),
            "channel_names": ["Y"],
        }
        self.assertEqual(
            list(make_hdr_preview_rgba(result)),
            [64, 64, 64, 255],
        )

    def test_alpha_is_preserved_in_preview(self):
        result = {
            "pixel_data": np.array([[[0.2, 0.4, 0.6, 0.5]]], dtype=np.float32),
            "channel_names": ["R", "G", "B", "A"],
        }
        self.assertEqual(
            list(make_hdr_preview_rgba(result)),
            [51, 102, 153, 128],
        )


if __name__ == "__main__":
    unittest.main()
