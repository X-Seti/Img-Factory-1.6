# Tmp_Template — Workshop Template

## DO NOT IMPORT FROM THIS FOLDER

Every workshop must be **standalone and self-contained**.

## Standard bootstrap (copy into every workshop)

```python
import sys
from pathlib import Path
# Find project root (the folder containing 'apps/') and add to sys.path
# so all apps.* imports resolve when running the workshop standalone.
_root = Path(__file__).resolve().parents[3]  # apps/components/X/file.py
if str(_root) not in sys.path: sys.path.insert(0, str(_root))
```

`parents` index by depth:
- `apps/components/X/workshop.py` → `parents[3]`
- `apps/components/X/depends/file.py` → `parents[4]`



```bash
cp -r apps/components/Tmp_Template apps/components/My_Workshop
```

Then inside `My_Workshop/`:
1. Rename `temp_workshop.py` → `my_workshop.py`
2. Edit `my_workshop.py` — change `App_name`, `config_key`, fill in stubs
3. Edit `gui_workshop.py` **in your copy** if you need to change the base layout
4. Never `import` from `Tmp_Template`

## Why standalone?

- Workshops run independently (`python3 launch_my_workshop.py`)
- No import chain breaking when the template changes
- No `setup_ui()` timing issues from shared base classes
- Easy to find — all code for a workshop is in one folder

## The setup_ui timing problem

If your workshop needs state set up **before** `setup_ui()` runs:

```python
def __init__(self, parent=None, main_window=None):
    self._defer_setup_ui = True   # stops auto-call
    super().__init__(parent, main_window)
    # set up your state here
    self._my_data = []
    self.setup_ui()               # call manually when ready
```

## What's in this folder

| File | Purpose |
|------|---------|
| `gui_workshop.py` | Base class — copy into your workshop |
| `temp_workshop.py` | Example workshop — copy and rename |
| `README.md` | This file |
