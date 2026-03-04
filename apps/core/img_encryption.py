#this belongs in core/img_encryption.py - Version: 2
# X-Seti - March04 2026 - IMG Factory 1.6 - IMG Encryption Functions
"""
IMG Encryption - GTA IV Version 3 AES-256 ECB encrypt/decrypt support.
Key and algorithm sourced from fastman92's IMG Console (public domain).
Reference: https://github.com/Bios-Marcel/IMG-Console
Encryption: 16 rounds of AES-256 ECB on each 16-byte block of header + table.
"""

import os
import struct
from typing import Optional, Callable

##Methods list -
# detect_v3_encryption
# encrypt_img_v3
# decrypt_img_v3
# _aes_encrypt_block
# _aes_decrypt_block
# _process_buffer
# encrypt_img_action
# integrate_encryption_functions

GTAIV_MAGIC       = 0xA94E2A52
GTAIV_MAGIC_BYTES = struct.pack('<I', GTAIV_MAGIC)

# AES-256 key from fastman92 IMG Console source (public domain)
GTAIV_KEY = bytes([
    0x1a, 0xb5, 0x6f, 0xed, 0x7e, 0xc3, 0xff, 0x01,
    0x22, 0x7b, 0x69, 0x15, 0x33, 0x97, 0x5d, 0xce,
    0x47, 0xd7, 0x69, 0x65, 0x3f, 0xf7, 0x75, 0x42,
    0x6a, 0x96, 0xcd, 0x6d, 0x53, 0x07, 0x56, 0x5d,
])

AES_ROUNDS = 16


def _aes_encrypt_block(block: bytes) -> bytes: #vers 1
    """Encrypt one 16-byte block with 16 rounds of AES-256 ECB."""
    from Crypto.Cipher import AES
    data = block
    for _ in range(AES_ROUNDS):
        data = AES.new(GTAIV_KEY, AES.MODE_ECB).encrypt(data)
    return data


def _aes_decrypt_block(block: bytes) -> bytes: #vers 1
    """Decrypt one 16-byte block with 16 rounds of AES-256 ECB."""
    from Crypto.Cipher import AES
    data = block
    for _ in range(AES_ROUNDS):
        data = AES.new(GTAIV_KEY, AES.MODE_ECB).decrypt(data)
    return data


def _process_buffer(buf: bytes, encrypt: bool) -> bytes: #vers 1
    """Process buffer in 16-byte blocks; pad, process, trim."""
    pad = (16 - len(buf) % 16) % 16
    padded = buf + b'\x00' * pad
    fn = _aes_encrypt_block if encrypt else _aes_decrypt_block
    result = b''.join(fn(padded[i:i+16]) for i in range(0, len(padded), 16))
    return result[:len(buf)]


def detect_v3_encryption(img_path: str) -> Optional[bool]: #vers 1
    """
    Returns True  = V3 encrypted
             False = V3 unencrypted
             None  = not a V3 file
    """
    try:
        with open(img_path, 'rb') as f:
            first20 = f.read(20)
        if len(first20) < 20:
            return None
        magic_val = struct.unpack('<I', first20[:4])[0]
        if magic_val == GTAIV_MAGIC:
            version = struct.unpack('<I', first20[4:8])[0]
            return False if version == 3 else None
        # Try decrypting to confirm V3 encrypted
        dec = _aes_decrypt_block(first20[:16])
        version = struct.unpack('<I', dec[4:8])[0]
        return True if version == 3 else None
    except Exception:
        return None


def decrypt_img_v3(img_path: str,
                   output_path: Optional[str] = None,
                   progress_cb: Optional[Callable] = None) -> bool: #vers 1
    """
    Decrypt GTA IV V3 encrypted IMG.
    Only header (20 bytes) and table (table_size bytes) are encrypted.
    Entry data is stored plaintext.
    """
    try:
        if progress_cb:
            progress_cb(0, 100, "Reading encrypted IMG...")

        with open(img_path, 'rb') as f:
            data = f.read()

        # Decrypt first 16-byte block to read header fields
        dec16 = _aes_decrypt_block(data[:16])
        version    = struct.unpack('<I', dec16[4:8])[0]
        num_items  = struct.unpack('<I', dec16[8:12])[0]
        table_size = struct.unpack('<I', dec16[12:16])[0]

        if version != 3:
            return False

        # Full header = 20 bytes (first 16 decrypted + bytes 16-20 plain)
        header_dec = dec16 + data[16:20]
        # Restore unencrypted magic
        header_dec = GTAIV_MAGIC_BYTES + header_dec[4:]

        table_start = 20
        table_end   = table_start + table_size

        if progress_cb:
            progress_cb(40, 100, "Decrypting table...")

        table_dec = _process_buffer(data[table_start:table_end], encrypt=False)

        out_data = header_dec + table_dec + data[table_end:]

        dst = output_path or img_path
        with open(dst, 'wb') as f:
            f.write(out_data)

        if progress_cb:
            progress_cb(100, 100, "Decryption complete")
        return True

    except Exception as e:
        if progress_cb:
            progress_cb(0, 100, f"Decrypt error: {e}")
        return False


def encrypt_img_v3(img_path: str,
                   output_path: Optional[str] = None,
                   progress_cb: Optional[Callable] = None) -> bool: #vers 1
    """
    Encrypt a GTA IV V3 unencrypted IMG.
    Encrypts header (first 16 bytes) and full table block.
    Entry data is left plaintext.
    """
    try:
        if progress_cb:
            progress_cb(0, 100, "Reading IMG...")

        with open(img_path, 'rb') as f:
            data = f.read()

        magic_val = struct.unpack('<I', data[:4])[0]
        version   = struct.unpack('<I', data[4:8])[0]
        if magic_val != GTAIV_MAGIC or version != 3:
            return False

        table_size  = struct.unpack('<I', data[12:16])[0]
        table_start = 20
        table_end   = table_start + table_size

        if progress_cb:
            progress_cb(30, 100, "Encrypting header+table...")

        header_enc = _aes_encrypt_block(data[:16]) + data[16:20]
        table_enc  = _process_buffer(data[table_start:table_end], encrypt=True)

        out_data = header_enc + table_enc + data[table_end:]

        dst = output_path or img_path
        with open(dst, 'wb') as f:
            f.write(out_data)

        if progress_cb:
            progress_cb(100, 100, "Encryption complete")
        return True

    except Exception as e:
        if progress_cb:
            progress_cb(0, 100, f"Encrypt error: {e}")
        return False


def encrypt_img_action(main_window): #vers 2
    """GUI action: toggle encrypt/decrypt on the currently loaded V3 IMG."""
    from PyQt6.QtWidgets import QMessageBox
    import shutil
    try:
        tab_widget = getattr(main_window, 'main_tab_widget', None)
        if not tab_widget:
            main_window.log_message("No tab widget found")
            return

        current  = tab_widget.currentWidget()
        img_path = getattr(current, 'file_path', None)

        if not img_path or not os.path.exists(img_path):
            QMessageBox.warning(main_window, "Encrypt IMG",
                "No IMG file is currently loaded.")
            return

        enc_state = detect_v3_encryption(img_path)

        if enc_state is None:
            QMessageBox.warning(main_window, "Encrypt IMG",
                "This file is not a GTA IV Version 3 IMG.\n"
                "Encryption only applies to V3 (GTA IV) files.")
            return

        action = "Decrypt" if enc_state else "Encrypt"
        reply = QMessageBox.question(
            main_window, f"{action} IMG",
            f"{action} this IMG file?\n\n{img_path}\n\n"
            f"A backup (.bak) will be created first.",
            QMessageBox.StandardButton.Yes | QMessageBox.StandardButton.No)
        if reply != QMessageBox.StandardButton.Yes:
            return

        backup_path = img_path + '.bak'
        shutil.copy2(img_path, backup_path)
        main_window.log_message(f"Backup: {backup_path}")

        def _progress(val, total, msg):
            main_window.log_message(msg)

        ok = decrypt_img_v3(img_path, progress_cb=_progress) if enc_state \
             else encrypt_img_v3(img_path, progress_cb=_progress)

        if ok:
            main_window.log_message(f"{'Decrypted' if enc_state else 'Encrypted'}: {img_path}")
            QMessageBox.information(main_window, f"{action} IMG",
                f"{action} complete.\nBackup: {backup_path}")
        else:
            QMessageBox.warning(main_window, f"{action} IMG",
                f"{action} failed — check log.")

    except Exception as e:
        main_window.log_message(f"Encrypt action error: {e}")


def integrate_encryption_functions(main_window): #vers 2
    """Attach encrypt_img to main_window."""
    main_window.encrypt_img = lambda: encrypt_img_action(main_window)
