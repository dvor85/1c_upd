#!/usr/bin/env python

from pathlib import Path
import subprocess

name = "upd_1c"
PYINSTALLER = Path().home() + ".local/share/virtualenvs/upd_1c-YNNi-LTU/bin/pyinstaller"
PYTHON = Path().home() + ".local/share/virtualenvs/upd_1c-YNNi-LTU/bin/python"
PYTHONOPTIMIZE = 2

file = "{}.spec".format(name) if Path(name).exists() else "{}.py".format(name)
subprocess.check_output([PYINSTALLER,
                         "-y",
                         "--clean",
                         "--add-data", "forms/mainform.glade:forms",
                         "--add-data", "forms/testcheck.glade:forms",
                         "--name", name,
                         "--noconsole",
                         "--onedir",
                         file])
