import unittest

import numpy as np

from apps.methods.hdr_texture_data import (
    copy_hdr_fields, make_hdr_preview_rgba, mark_hdr_preview_modified,
    prepare_hdr_export_texture,
)


class HdrTextureDataCopyTests(unittest.TestCase):

    def test_grayscale_preview_is_replicated_to_rgb(self):
        pixels = np.array([[[0.25]]], dtype=np.float32)
        rgba = np.frombuffer(make_hdr_preview_rgba({
            "pixel_data": pixels, "channel_names": ["Y"],
        }), dtype=np.uint8)
        self.assertEqual(rgba.tolist(), [64, 64, 64, 255])

    def test_grayscale_alpha_preview_keeps_luminance_and_alpha_separate(self):
        pixels = np.array([[[0.25, 0.5]]], dtype=np.float32)
        rgba = np.frombuffer(make_hdr_preview_rgba({
            "pixel_data": pixels, "channel_names": ["Y", "A"],
        }), dtype=np.uint8)
        self.assertEqual(rgba.tolist(), [64, 64, 64, 128])

    def test_pixel_array_is_copied_independently(self):
        source = {
            "hdr_data": np.array([[[1.5, -0.25, 2.0]]], dtype=np.float32),
            "hdr_metadata": {"channels": ["R", "G", "B"]},
            "hdr_source": {"format": "EXR"},
        }
        target = copy_hdr_fields(source, {})

        self.assertIsNot(target["hdr_data"], source["hdr_data"])
        np.testing.assert_array_equal(target["hdr_data"], source["hdr_data"])
        target["hdr_data"][0, 0, 0] = 9.0
        self.assertEqual(source["hdr_data"][0, 0, 0], 1.5)

    def test_metadata_is_copied_independently(self):
        source = {"hdr_metadata": {"channels": ["R", "G", "B"]}}
        target = copy_hdr_fields(source, {})
        target["hdr_metadata"]["channels"].append("A")
        self.assertEqual(source["hdr_metadata"]["channels"], ["R", "G", "B"])

    def test_tone_mapping_settings_are_copied(self):
        source = {
            "hdr_tone_operator": "aces",
            "hdr_exposure": 1.25,
        }
        target = copy_hdr_fields(source, {})
        self.assertEqual(target["hdr_tone_operator"], "aces")
        self.assertEqual(target["hdr_exposure"], 1.25)

    def test_preview_stale_flag_is_copied(self):
        source = {"hdr_data": np.ones((1, 1, 3)), "hdr_preview_stale": True}
        target = copy_hdr_fields(source, {})
        self.assertTrue(target["hdr_preview_stale"])

    def test_edit_marks_existing_hdr_source_stale(self):
        texture = {"hdr_data": np.ones((1, 1, 3)), "hdr_preview_stale": False}
        self.assertIs(mark_hdr_preview_modified(texture), texture)
        self.assertTrue(texture["hdr_preview_stale"])

    def test_edit_without_hdr_source_does_not_add_flag(self):
        texture = {"rgba_data": b"\\x00"}
        mark_hdr_preview_modified(texture)
        self.assertNotIn("hdr_preview_stale", texture)

    def test_missing_hdr_fields_are_ignored(self):
        target = copy_hdr_fields({"name": "ordinary"}, {"name": "copy"})
        self.assertEqual(target, {"name": "copy"})

    def test_fresh_hdr_is_tone_mapped_and_converted_to_srgb(self):
        pixels = np.array([[[1.0, 0.5, 0.0, 0.5]]], dtype=np.float32)
        texture = {
            "width": 1, "height": 1, "rgba_data": bytes([1, 2, 3, 4]),
            "hdr_data": pixels.copy(),
            "hdr_metadata": {"channels": ["R", "G", "B", "A"]},
            "hdr_preview_stale": False,
            "mipmap_levels": [{"level": 0, "rgba_data": b"old"}],
        }
        prepared = prepare_hdr_export_texture(texture)
        self.assertIsNot(prepared, texture)
        self.assertEqual(len(prepared["rgba_data"]), 4)
        self.assertGreater(prepared["rgba_data"][0], 180)
        self.assertEqual(prepared["rgba_data"][3], 128)
        self.assertEqual(prepared["mipmap_levels"], [])
        self.assertEqual(texture["rgba_data"], bytes([1, 2, 3, 4]))
        np.testing.assert_array_equal(texture["hdr_data"], pixels)
        self.assertEqual(len(texture["mipmap_levels"]), 1)

    def test_export_uses_saved_tone_mapping_settings(self):
        pixels = np.array([[[0.18, 0.5, 1.0]]], dtype=np.float32)
        base = {
            "width": 1, "height": 1, "rgba_data": bytes(4),
            "hdr_data": pixels.copy(),
            "hdr_metadata": {"channels": ["R", "G", "B"]},
            "hdr_preview_stale": False,
        }
        reinhard = prepare_hdr_export_texture(dict(base))
        aces_texture = dict(base, hdr_tone_operator="aces", hdr_exposure=1.0)
        aces = prepare_hdr_export_texture(aces_texture)
        self.assertNotEqual(reinhard["rgba_data"], aces["rgba_data"])
        np.testing.assert_array_equal(base["hdr_data"], pixels)

    def test_stale_preview_is_exported_without_hdr_conversion(self):
        texture = {
            "width": 1, "height": 1, "rgba_data": bytes([10, 20, 30, 40]),
            "hdr_data": np.ones((1, 1, 3), dtype=np.float32),
            "hdr_preview_stale": True,
        }
        self.assertIs(prepare_hdr_export_texture(texture), texture)

    def test_hdr_dimensions_are_resized_for_export(self):
        texture = {
            "width": 2, "height": 1, "rgba_data": bytes(8),
            "hdr_data": np.ones((1, 1, 3), dtype=np.float32),
            "hdr_metadata": {"channels": ["R", "G", "B"]},
            "hdr_preview_stale": False,
        }
        prepared = prepare_hdr_export_texture(texture)
        self.assertEqual(len(prepared["rgba_data"]), 8)
        self.assertEqual(texture["hdr_data"].shape, (1, 1, 3))

    def test_grayscale_hdr_exports_as_rgb_with_opaque_alpha(self):
        texture = {
            "width": 1, "height": 1, "rgba_data": bytes(4),
            "hdr_data": np.array([[[0.5]]], dtype=np.float32),
            "hdr_metadata": {"channels": ["Y"]},
            "hdr_preview_stale": False,
        }
        prepared = prepare_hdr_export_texture(texture)
        rgba = np.frombuffer(prepared["rgba_data"], dtype=np.uint8)
        self.assertEqual(rgba[0], rgba[1])
        self.assertEqual(rgba[1], rgba[2])
        self.assertEqual(rgba[3], 255)


if __name__ == "__main__":
    unittest.main()
