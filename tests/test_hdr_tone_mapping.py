import unittest

import numpy as np

from apps.methods.hdr_tone_mapping import tone_map


class HdrToneMappingTests(unittest.TestCase):
    def test_reinhard_maps_hdr_values_to_unit_range(self):
        pixels = np.array(
            [[[0.0, 1.0, 3.0], [10.0, 0.5, 0.25]]],
            dtype=np.float32,
        )
        original = pixels.copy()

        result = tone_map(pixels, operator="reinhard")

        self.assertEqual(result.dtype, np.float32)
        self.assertTrue(np.all(result >= 0.0))
        self.assertTrue(np.all(result <= 1.0))
        np.testing.assert_allclose(
            result[0, 0], [0.0, 0.5, 0.75], rtol=1e-6
        )
        np.testing.assert_array_equal(pixels, original)

    def test_aces_maps_highlights_and_preserves_shape(self):
        pixels = np.array([[[0.18, 1.0, 4.0]]], dtype=np.float32)
        result = tone_map(pixels, operator="aces")
        self.assertEqual(result.shape, pixels.shape)
        self.assertEqual(result.dtype, np.float32)
        self.assertTrue(np.all(result >= 0.0))
        self.assertTrue(np.all(result <= 1.0))
        self.assertGreater(result[0, 0, 2], result[0, 0, 1])

    def test_exposure_is_applied_before_tone_mapping(self):
        pixels = np.array([[[1.0]]], dtype=np.float32)
        normal = tone_map(pixels, exposure=0.0)
        brighter = tone_map(pixels, exposure=1.0)
        self.assertGreater(brighter[0, 0, 0], normal[0, 0, 0])

    def test_alpha_is_clamped_but_not_tone_mapped(self):
        pixels = np.array(
            [[[2.0, 1.0, 0.5, 1.5], [1.0, 1.0, 1.0, -0.5]]],
            dtype=np.float32,
        )
        result = tone_map(pixels, operator="reinhard")
        np.testing.assert_allclose(result[0, 0], [2/3, 0.5, 1/3, 1.0])
        self.assertEqual(result[0, 1, 3], 0.0)

    def test_grayscale_and_grayscale_alpha_are_supported(self):
        gray = np.array([[[2.0]]], dtype=np.float32)
        gray_alpha = np.array([[[2.0, 0.25]]], dtype=np.float32)
        np.testing.assert_allclose(tone_map(gray), [[[2/3]]])
        np.testing.assert_allclose(tone_map(gray_alpha), [[[2/3, 0.25]]])

    def test_non_finite_and_negative_values_are_sanitized(self):
        pixels = np.array(
            [[[np.nan, np.inf, -np.inf], [-2.0, 0.0, 1.0]]],
            dtype=np.float32,
        )
        result = tone_map(pixels)
        self.assertTrue(np.all(np.isfinite(result)))
        np.testing.assert_allclose(result[0, 0], [0.0, 1.0, 0.0])
        np.testing.assert_allclose(result[0, 1], [0.0, 0.0, 0.5])

    def test_extreme_values_remain_finite_at_maximum_exposure(self):
        pixels = np.array(
            [[[np.inf], [np.finfo(np.float64).max], [1.0e30]]],
            dtype=np.float64,
        )
        for operator in ("reinhard", "aces"):
            with self.subTest(operator=operator):
                result = tone_map(pixels, operator=operator, exposure=32.0)
                self.assertTrue(np.all(np.isfinite(result)))
                self.assertTrue(np.all(result >= 0.0))
                self.assertTrue(np.all(result <= 1.0))
                self.assertTrue(np.all(result > 0.99))

    def test_invalid_operator_and_input_are_rejected(self):
        with self.assertRaises(ValueError):
            tone_map(np.ones((1, 1, 3), dtype=np.float32), operator="unknown")
        with self.assertRaises(ValueError):
            tone_map(np.ones((1, 3), dtype=np.float32))
        with self.assertRaises(ValueError):
            tone_map(np.ones((1, 1, 3), dtype=np.uint8))
        with self.assertRaises(ValueError):
            tone_map(np.ones((1, 1, 3), dtype=np.float32), exposure=100)


if __name__ == "__main__":
    unittest.main()
