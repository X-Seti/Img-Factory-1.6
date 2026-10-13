# X-Seti - belongs in apps/methods/imgcol-exist.py - file context system - version 3

file_object = None
file_type = None

# Central registry of supported types
SUPPORTED_TYPES = {
    "IMG": "Image Archive",
    "COL": "Collision File",
    # future:
    # "TXD": "Texture Dictionary",
    # "DFF": "RenderWare Model",
    # "DIR": "IMG Directory",
    # "AUD": "Audio File",
}


def set_context(main_window):  # vers 3
    """Assign file_object and file_type from the main window."""
    global file_object, file_type
    file_object = getattr(main_window, "file_object", None)
    file_type = getattr(main_window, "file_type", None)


def has_valid_file():  # vers 3
    """
    Generic check for any loaded file.
    Returns (boolean, message)
    """
    if file_object is None:
        return False, "No file is currently loaded"

    if file_type not in SUPPORTED_TYPES:
        return False, f"Unsupported file type: {file_type}"

    return True, None


def has_file_type(*types):  # vers 3
    """
    Check if the current file_type matches one of the accepted types.

    Returns (boolean, message)
    """
    if file_object is None:
        return False, "No file is currently loaded"

    if file_type not in types:
        allowed = ", ".join(types)
        return False, f"This operation requires: {allowed}, but current is: {file_type}"

    return True, None

__all__ = [
    'set_context'
]
