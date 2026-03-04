#this belongs in core/img_encryption.py - Version: 1
# X-Seti - March04 2026 - IMG Factory 1.6 - IMG Encryption Functions
"""
IMG Encryption - Fastman92 IMG3 encrypt/decrypt support.
Based on CIMGFile eIMGVersion: IMG_3_ENCRYPTED / IMG_3_UNENCRYPTED
"""

import os
import struct
from typing import Optional, Callable

##Methods list -
# detect_encryption
# encrypt_img_file
# decrypt_img_file
# _fastman92_crypt
# integrate_encryption_functions

# Fastman92 IMG3 encryption key (XOR-based, 32-byte repeating key)
FASTMAN92_KEY = bytes([
    0x34, 0x12, 0x78, 0x56, 0xBC, 0x9A, 0xF0, 0xDE,
    0x34, 0x12, 0x78, 0x56, 0xBC, 0x9A, 0xF0, 0xDE,
    0x34, 0x12, 0x78, 0x56, 0xBC, 0x9A, 0xF0, 0xDE,
    0x34, 0x12, 0x78, 0x56, 0xBC, 0x9A, 0xF0, 0xDE,
])

# IMG3 encrypted magic - replaces "VER3" header
FASTMAN92_MAGIC     = b'\x00\x00\x00\x00'
IMG3_MAGIC          = b'VER3'
FASTMAN92_ENC_MAGIC = b'\xA4\x15\x06\x00'  # Fastman92 encrypted header marker


def detect_encryption(img_path: str) -> bool: #vers 1
    """Return True if IMG file appears to be Fastman92 encrypted."""
    try:
        with open(img_path, 'rb') as f:
            header = f.read(4)
        return header == FASTMAN92_ENC_MAGIC or (header != IMG3_MAGIC and header != b'VER2')
    except Exception:
        return False


def _fastman92_crypt(data: bytes) -> bytes: #vers 1
    """XOR data with repeating Fastman92 key (encrypt and decrypt are identical)."""
    key_len = len(FASTMAN92_KEY)
    result = bytearray(len(data))
    for i, byte in enumerate(data):
        result[i] = byte ^ FASTMAN92_KEY[i % key_len]
    return bytes(result)


def encrypt_img_file(img_path: str,
                     output_path: Optional[str] = None,
                     progress_cb: Optional[Callable] = None) -> bool: #vers 1
    """
    Encrypt an IMG3 file using Fastman92 XOR encryption.
    Writes to output_path (or overwrites img_path if None).
    """
    try:
        if detect_encryption(img_path):
            return False  # Already encrypted

        with open(img_path, 'rb') as f:
            data = f.read()

        if data[:4] != IMG3_MAGIC:
            return False  # Not a V3 IMG

        if progress_cb:
            progress_cb(0, 100, "Encrypting IMG...")

        # Encrypt everything after the 4-byte magic header
        encrypted_body = _fastman92_crypt(data[4:])
        out_data = FASTMAN92_ENC_MAGIC + encrypted_body

        dst = output_path or img_path
        with open(dst, 'wb') as f:
            f.write(out_data)

        if progress_cb:
            progress_cb(100, 100, "Encryption complete")
        return True

    except Exception as e:
        if progress_cb:
            progress_cb(0, 100, f"Encryption error: {e}")
        return False


def decrypt_img_file(img_path: str,
                     output_path: Optional[str] = None,
                     progress_cb: Optional[Callable] = None) -> bool: #vers 1
    """
    Decrypt a Fastman92-encrypted IMG3 file.
    Writes to output_path (or overwrites img_path if None).
    """
    try:
        if not detect_encryption(img_path):
            return False  # Not encrypted

        with open(img_path, 'rb') as f:
            data = f.read()

        if progress_cb:
            progress_cb(0, 100, "Decrypting IMG...")

        # Decrypt body, restore VER3 magic
        decrypted_body = _fastman92_crypt(data[4:])
        out_data = IMG3_MAGIC + decrypted_body

        dst = output_path or img_path
        with open(dst, 'wb') as f:
            f.write(out_data)

        if progress_cb:
            progress_cb(100, 100, "Decryption complete")
        return True

    except Exception as e:
        if progress_cb:
            progress_cb(0, 100, f"Decryption error: {e}")
        return False


def encrypt_img_action(main_window): #vers 1
    """
    GUI action: toggle encrypt/decrypt on the currently loaded IMG.
    Called from the Encrypt button in the right panel.
    """
    from PyQt6.QtWidgets import QMessageBox, QFileDialog
    try:
        # Get active IMG path
        tab_widget = getattr(main_window, 'main_tab_widget', None)
        if not tab_widget:
            main_window.log_message("No tab widget found")
            return

        current = tab_widget.currentWidget()
        img_obj = getattr(current, 'file_object', None)
        img_path = getattr(current, 'file_path', None)

        if not img_path or not os.path.exists(img_path):
            QMessageBox.warning(main_window, "Encrypt IMG",
                "No IMG file is currently loaded.")
            return

        # Check if V3
        with open(img_path, 'rb') as f:
            magic = f.read(4)

        already_enc = detect_encryption(img_path)
        is_v3 = (magic == IMG3_MAGIC or already_enc)

        if not is_v3:
            QMessageBox.warning(main_window, "Encrypt IMG",
                "Fastman92 encryption only applies to IMG Version 3 files.")
            return

        action = "Decrypt" if already_enc else "Encrypt"
        reply = QMessageBox.question(
            main_window, f"{action} IMG",
            f"{'Decrypt' if already_enc else 'Encrypt'} this IMG file?\n\n{img_path}\n\n"
            f"A backup will be created first.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return

        # Backup
        backup_path = img_path + '.bak'
        import shutil
        shutil.copy2(img_path, backup_path)
        main_window.log_message(f"Backup: {backup_path}")

        def _progress(val, total, msg):
            main_window.log_message(msg)

        if already_enc:
            ok = decrypt_img_file(img_path, progress_cb=_progress)
        else:
            ok = encrypt_img_file(img_path, progress_cb=_progress)

        if ok:
            main_window.log_message(f"{'Decrypted' if already_enc else 'Encrypted'}: {img_path}")
            QMessageBox.information(main_window, f"{action} IMG",
                f"{'Decryption' if already_enc else 'Encryption'} complete.\nBackup saved to:\n{backup_path}")
        else:
            main_window.log_message(f"{action} failed")
            QMessageBox.warning(main_window, f"{action} IMG", f"{action} failed.")

    except Exception as e:
        main_window.log_message(f"Encrypt action error: {e}")


def integrate_encryption_functions(main_window): #vers 1
    """Attach encrypt_img to main_window."""
    main_window.encrypt_img = lambda: encrypt_img_action(main_window)
