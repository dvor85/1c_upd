# -*- mode: python ; coding: utf-8 -*-
from pathlib import Path
block_cipher = None


a = Analysis(['upd_1c.py'],
             pathex=[Path().cwd()],
             binaries=[],
             datas=[('forms/mainform.glade', 'forms'), ('forms/testcheck.glade', 'forms')],
             hiddenimports=[],
             hookspath=[],
             runtime_hooks=[],
             excludes=[],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher,
             noarchive=False)
pyz = PYZ(a.pure, a.zipped_data,
          cipher=block_cipher)

a.datas = TOC([x for x in a.datas if not ((  # x[0].startswith("share/icons") or
    # x[0].startswith("share/themes") or
    x[0].startswith("share/themes")))])
exe = EXE(pyz,
          a.scripts,
          a.binaries,
          a.zipfiles,
          a.datas,
          [],
          name='upd_1c',
          debug=False,
          bootloader_ignore_signals=False,
          strip=False,
          upx=True,
          upx_exclude=[],
          runtime_tmpdir=None,
          console=False)
