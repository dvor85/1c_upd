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
               x[0].startswith("share/icons/Adwaita") or
               x[0].startswith("share/icons/hicolor") or
               x[0].startswith("share/locale/ru")])
a.datas = TOC([x for x in a.datas if
               not ((x[0].startswith("share/icons/Adwaita/512x512") or
                     x[0].startswith("share/icons/Adwaita/256x256") or
                     x[0].startswith("share/icons/Adwaita/scalable") or
                     x[0].startswith("share/icons/Adwaita/cursors") or
                     x[0].startswith("share/icons/hicolor/512x512") or
                     x[0].startswith("share/icons/hicolor/256x256") or
                     x[0].startswith("share/icons/hicolor/scalable")))])

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
