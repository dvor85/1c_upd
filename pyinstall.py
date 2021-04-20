#!/usr/bin/env python

from pathlib import Path
import subprocess
import sys

IS_WIN = sys.platform.startswith("win")

name = "upd_1c"
file = "{}.spec".format(name)
if not Path(file).exists():
    file = "{}.py".format(name)


if IS_WIN:
    subprocess.check_output(["pyinstaller",
                             "-y",
                             "--clean",
                             "--add-data", "forms/mainform.glade;forms",
                             "--add-data", "forms/testcheck.glade;forms",
                             "--name", name,
                             "--noconsole",
                             "--onefile",
                             file])
else:
    subprocess.check_output([Path().home() / ".local/share/virtualenvs/upd_1c-YNNi-LTU/bin/pyinstaller",
                             "-y",
                             "--clean",
                             "--add-data", "forms/mainform.glade:forms",
                             "--add-data", "forms/testcheck.glade:forms",
                             "--name", name,
                             "--noconsole",
                             "--onefile",
                             "--log-level", "DEBUG",
                             file])
# Добавить этот текст для удаления лишнего из конечного файла
# a.datas = TOC([x for x in a.datas if not ((x[0].startswith("share/icons") or
#                                           x[0].startswith("share/themes") or
#                                           x[0].startswith("share/themes")))])
