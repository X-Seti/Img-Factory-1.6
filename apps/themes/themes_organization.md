# 🎨 Themes Folder Organization

## New Theme System Structure:

```
themes/
├── img_factory.json          # Professional IMG Factory theme
├── img_factory_dark.json     # Dark mode version
├── lcars.json                # Star Trek LCARS theme
├── amiga_workbench.json      # Classic Amiga 3.1
├── amiga_mui.json            # Amiga MUI theme
├── deep_purple.json          # Purple space theme
├── tea_and_toast.json        # Cozy morning theme
├── matrix.json               # Matrix green theme
├── knight_rider.json         # KITT dashboard theme
├── classic_dark.json         # Professional dark
├── light_professional.json   # Clean light theme
└── exported_themes/          # User-exported themes
    ├── custom_theme1.json
    └── my_theme.json
```

## Theme File Format:

Each theme file follows this structure:

```json
{
    "name": "Theme Display Name",
    "description": "Theme description with emoji 🎨",
    "category": "Professional", 
    "author": "X-Seti",
    "version": "1.0",
    "colors": {
        "bg_primary": "#ffffff",
        "bg_secondary": "#f8f9fa",
        "bg_tertiary": "#e9ecef",
        "panel_bg": "#f1f3f4",
        "accent_primary": "#1976d2",
        "accent_secondary": "#1565c0",
        "text_primary": "#000000",      // Pure black text
        "text_secondary": "#2d2d2d",    // Very dark gray
        "text_accent": "#15803d",       // Keep accent darker
        "button_normal": "#e3f2fd",
        "button_hover": "#bbdefb", 
        "button_pressed": "#90caf9",
        "border": "#dee2e6",
        "success": "#4caf50",
        "warning": "#ff9800",
        "error": "#f44336",
        "grid": "#f0f0f0",
        "pin_default": "#757575",
        "pin_highlight": "#2196f3",
        "action_import": "#2196f3",
        "action_export": "#4caf50",
        "action_remove": "#f44336", 
        "action_update": "#ff9800",
        "action_convert": "#9c27b0"
    }
}
```

## Benefits of File-Based Themes:

✅ **Organization**: Clean separation of themes from code
✅ **Extensibility**: Easy to add new themes without code changes  
✅ **User Themes**: Users can create and share custom themes
✅ **Maintenance**: Individual theme files are easier to edit
✅ **Backup**: Themes can be backed up separately
✅ **Sharing**: Easy to share themes between installations

## Theme Categories:

### 🏢 **Professional Themes**
- IMG Factory (Light & Dark)
- Classic Dark
- Light Professional

### 🖥️ **Retro Computing**
- Amiga Workbench 3.1
- Amiga MUI
- Classic terminal themes

### 🎬 **Pop Culture**
- LCARS (Star Trek)
- Matrix Green
- Knight Rider (KITT)

### 🎨 **Creative/Fun**
- Deep Purple Space
- Tea 'n' Toast Morning
- Custom user themes

## Usage in Code:

The `AppSettings` class now automatically:

1. **Scans** the `themes/` folder for `.json` files
2. **Loads** each theme into memory
3. **Falls back** to built-in themes if folder doesn't exist
4. **Allows** saving new themes to the folder
5. **Refreshes** themes dynamically

## Migration Notes:

- Existing `themes.json` files are still supported
- Built-in themes serve as fallback
- Theme format is backwards compatible
- No breaking changes to existing code
