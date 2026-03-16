#this belongs in apps/core/lcs_vcs_disc_layout.py - Version: 1
# X-Seti - March 2026 - IMG Factory 1.6
# LCS and VCS PS2 disc layout reference
# Used to locate GAME.DTZ, streaming areas, cutscene dirs etc.

import os

DISC_LAYOUTS = {
    "VCS": {
        "game": "GTA Vice City Stories",
        "platform": "PS2",
        "dtz_path": "GAME.DTZ",
        "main_img": "GTA3PS2.IMG",
        "streaming_areas": [
            "BEACH",
            "MAINLA",
            "MALL"
        ],
        "cutscene_dir": "PS2/MOCAPPS2.DIR",
        "cutscene_img": "PS2/MOCAPPS2.IMG",
        "texture_ext": ".XTX",
        "streaming_dir": "",
        "modules_dir": "MODULES",
        "splash_dir": "SPLASH"
    },
    "LCS": {
        "game": "GTA Liberty City Stories",
        "platform": "PS2",
        "dtz_path": "CHK/PS2/GAME.DTZ",
        "main_img": "MODELS/GTA3PS2.IMG",
        "streaming_areas": [
            "COMMER",
            "INDUST",
            "SUBURB",
            "UNDERG"
        ],
        "cutscene_dir": "ANIM/CUTS.DIR",
        "cutscene_img": "ANIM/CUTS.IMG",
        "texture_ext": ".CHK",
        "streaming_dir": "MODELS",
        "modules_dir": "MODULES",
        "splash_dir": "SPLASH"
    }
}


def find_disc_root(path: str) -> tuple[str, str]:
    """Given any file path on an LCS or VCS disc, find the disc root and game.
    
    Returns (disc_root, game) where game is 'LCS', 'VCS', or ''.
    """
    path = os.path.abspath(path)
    # Walk up from the given path looking for known disc markers
    check = os.path.dirname(path)
    for _ in range(8):  # max 8 levels up
        # VCS: GAME.DTZ at root
        if os.path.exists(os.path.join(check, 'GAME.DTZ')) or \
           os.path.exists(os.path.join(check, 'game.dtz')):
            return check, 'VCS'
        # LCS: CHK/PS2/GAME.DTZ
        lcs_dtz = os.path.join(check, 'CHK', 'PS2', 'GAME.DTZ')
        if os.path.exists(lcs_dtz):
            return check, 'LCS'
        lcs_dtz2 = os.path.join(check, 'chk', 'ps2', 'game.dtz')
        if os.path.exists(lcs_dtz2):
            return check, 'LCS'
        parent = os.path.dirname(check)
        if parent == check:
            break
        check = parent
    return '', ''


def find_game_dtz(disc_root: str, game: str) -> str:
    """Find GAME.DTZ given disc root and game type."""
    layout = DISC_LAYOUTS.get(game, {})
    rel = layout.get('dtz_path', '')
    if not rel:
        return ''
    for variant in [rel, rel.lower(), rel.upper()]:
        p = os.path.join(disc_root, variant.replace('/', os.sep))
        if os.path.exists(p):
            return p
    return ''


def find_cutscene_img(disc_root: str, game: str) -> tuple[str, str]:
    """Find cutscene DIR and IMG files given disc root and game."""
    layout = DISC_LAYOUTS.get(game, {})
    dir_rel = layout.get('cutscene_dir', '')
    img_rel = layout.get('cutscene_img', '')
    dir_path = ''
    img_path = ''
    for rel, attr in [(dir_rel, 'dir_path'), (img_rel, 'img_path')]:
        for variant in [rel, rel.lower()]:
            p = os.path.join(disc_root, variant.replace('/', os.sep))
            if os.path.exists(p):
                if attr == 'dir_path':
                    dir_path = p
                else:
                    img_path = p
                break
    return dir_path, img_path
