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

# Добавить этот текст для удаления лишнего из конечного файла
a.datas = TOC([x for x in a.datas if
               not ((x[0].startswith("share/icons") or
                     x[0].startswith("share/themes")or
                     x[0].startswith("share/locale"))) or
               x[0].startswith("share/icons/Adwaita/16x16") or
               x[0].startswith("share/icons/Adwaita/32x32") or
               x[0].startswith("share/icons/Adwaita/index.theme") or
               x[0].startswith("share/icons/hicolor/16x16") or
               x[0].startswith("share/icons/hicolor/32x32") or
               x[0].startswith("share/icons/hicolor/index.theme") or
               x[0].startswith("share/locale/ru")])
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
