#!/usr/bin/env python3
#this belongs in root /apps/app_info.py - Version: 4
# X-Seti - Feb 24 2026 - IMG Factory 1.6 - App constants
"""
App-wide constants - import from here to avoid circular imports.

Build numbering:  Build <release>.<imgfactory_file_version>
  release               — incremented manually each session
  imgfactory_file_version — the Version: N comment in imgfactory.py;
                            increment it on every change to that file.
"""

App_name  = "Img Factory 1.6"
App_build = "May 06 2026"
App_build_num = "Build 370"          # increment manually with each release
App_imgfactory_version = 80          # mirrors Version: N in imgfactory.py — increment on every change
App_auth  = "X-Seti"

def get_full_build() -> str:
    """Return combined build string e.g. 'Build 369.77'"""
    return f"{App_build_num}.{App_imgfactory_version}"
