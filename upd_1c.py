'''
Created on 26 мар. 2021 г.

@author: demon
'''

import logging
import logger
import config
from pathlib import Path
import os
import base64
import threading
import subprocess
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk  # @IgnorePep8

global log

BaseActions = dict(ba_update=0, ba_dumpib=1, ba_restoreib=2, ba_dumpcfg=3,
                   ba_loadcfg=4, ba_check=5, ba_enterprise=6, ba_config=7, ba_cache=8, ba_journal=9, ba_integrity=10,
                   ba_physical=11, ba_macro=12, ba_convert=13)


class MyThread(threading.Thread):
    def __init__(self, group=None, target=None, name=None,
                 args=(), kwargs=None, *, daemon=None):
        threading.Thread.__init__(self, group=group, target=target, name=name, args=args, kwargs=kwargs)
        self.daemon = True

    def run(self):
        try:
            threading.Thread.run(self)
        except Exception as e:
            log.info(e)


class Mainform():
    def __init__(self):
        builder = Gtk.Builder()
        builder.add_from_file("forms/mainform.glade")

        builder.connect_signals(self)
        self.wmain = builder.get_object("mainform")
        self.UserE = builder.get_object('UserE')
        self.PassE = builder.get_object('PassE')
        self.exe_path_edit = builder.get_object('exe_path_edit')
        self.base_path_edit = builder.get_object('base_path_edit')
        self.bak_path_edit = builder.get_object('bak_path_edit')
        self.status_bar = builder.get_object('status_bar')
        self.bak_count_edit = builder.get_object('bak_count_edit')
        self.page_size_edit = builder.get_object('page_size_edit')
        self.macros_list = builder.get_object('macros_list')
        self.macros_liststore = builder.get_object('macros_liststore')
        self.updates_list = builder.get_object('updates_list')
        self.updates_liststore = builder.get_object('updates_liststore')
        self.logs_textbuffer = builder.get_object('logs_textbuffer')
        self.create()
        self.logfile = self.ini[config.SectionMain].get(
            config.KeyLogFile, Path(os.path.dirname(__file__)) / 'logs' / 'upd_1c.log')
        global log
        log = logger.get_logger(__name__, self.logfile, level=logging.DEBUG, callback=self.logs_callback)
        log.info('Started')
        self.main_thread = None

    def logs_callback(self, msg):
        self.logs_textbuffer.insert(self.logs_textbuffer.get_end_iter(), msg)

    def create(self):
        self.wmain.set_title('Upd_1c')
        ini_file = Path(os.path.dirname(__file__)) / 'upd_1c.ini'
        self.status_bar.push(1, str(ini_file))
        self.ini = config.Config(ini_file)
        self.UserE.set_text(self.ini[config.SectionBase].get(config.KeyUser, ''))
        self.bak_path_edit.set_text(self.ini[config.SectionBase].get(config.KeyBakcupPath, ''))
        self.exe_path_edit.set_text(self.ini[config.SectionMain].get(config.KeyExecutable, ''))
        self.base_path_edit.set_text(self.ini[config.SectionBase].get(config.KeyPath, ''))
        self.PassE.set_text(base64.b64decode(self.ini[config.SectionBase].get(config.KeyPass, '')).decode('UTF-8'))
        self.bak_count_edit.set_value(int(self.ini[config.SectionBase].get(config.KeyBackupCount, '3')))
        self.page_size_edit.set_value(int(self.ini[config.SectionBase].get(config.KeyPageSize, '8')))

        for i in self.ini[config.SectionMacro].items():
            self.macros_liststore.append(list(i))
        renderer = Gtk.CellRendererText()
        column = Gtk.TreeViewColumn("Title", renderer, text=1)
        self.macros_list.append_column(column)

        for i in self.ini[config.SectionUpdates].items():
            self.updates_liststore.append(list(i))
        renderer = Gtk.CellRendererText()
        column = Gtk.TreeViewColumn("Title", renderer, text=1)
        self.updates_list.append_column(column)

    def loop(self):
        self.wmain.show_all()

    def onDestroy(self, *args):
        Gtk.main_quit()

    def add_filters(self, dialog):
        filter_any = Gtk.FileFilter()
        filter_any.set_name("Any files")
        filter_any.add_pattern("*")
        dialog.add_filter(filter_any)

    def runEnterprise(self, widget):
        log.info('{n}'.format(n=widget.get_label()))
        proc = [self.exe_path_edit.get_text()]
        proc.append('ENTERPRISE')
        proc.append('/DisableStartupMessages')
        proc.append('/DisableSplash')
        proc.append('/F"' + self.base_path_edit.get_text() + '"')
        proc.append('/N"' + self.UserE.get_text() + '"')
        proc.append('/P"' + self.PassE.get_text() + '"')
        proc.append('/Out "' + str(self.logfile) + '" -NoTruncate')
        try:
            subprocess.check_call(proc)
        except Exception as e:
            log.error('Команда "{n}" завершена с ошибкой'.format(n=widget.get_label()))
            raise e

    def on_runEnterprise(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            self.main_thread = MyThread(target=self.runEnterprise, name=__name__, args=(widget,))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_exe_path_btn_clicked(self, widget):
        dialog = Gtk.FileChooserDialog(
            title="Please choose a file", parent=None, action=Gtk.FileChooserAction.OPEN
        )
        dialog.add_buttons(
            Gtk.STOCK_CANCEL,
            Gtk.ResponseType.CANCEL,
            Gtk.STOCK_OPEN,
            Gtk.ResponseType.OK,
        )
        dialog.set_filename(self.exe_path_edit.get_text())

        self.add_filters(dialog)

        response = dialog.run()
        if response == Gtk.ResponseType.OK:
            self.exe_path_edit.set_text(dialog.get_filename())
        elif response == Gtk.ResponseType.CANCEL:
            print("Cancel clicked")

        dialog.destroy()

    def on_base_path_btn_clicked(self, widget):
        dialog = Gtk.FileChooserDialog(
            title="Please choose a file", parent=None, action=Gtk.FileChooserAction.SELECT_FOLDER
        )
        dialog.add_buttons(
            Gtk.STOCK_CANCEL,
            Gtk.ResponseType.CANCEL,
            Gtk.STOCK_OPEN,
            Gtk.ResponseType.OK,
        )
        dialog.set_filename(self.base_path_edit.get_text())
        response = dialog.run()
        if response == Gtk.ResponseType.OK:
            self.base_path_edit.set_text(dialog.get_filename())
        elif response == Gtk.ResponseType.CANCEL:
            print("Cancel clicked")

        dialog.destroy()

    def on_bak_path_btn_clicked(self, widget):
        dialog = Gtk.FileChooserDialog(
            title="Please choose a file", parent=None, action=Gtk.FileChooserAction.SELECT_FOLDER
        )

        dialog.add_buttons(
            Gtk.STOCK_CANCEL,
            Gtk.ResponseType.CANCEL,
            Gtk.STOCK_OPEN,
            Gtk.ResponseType.OK,
        )
        dialog.set_filename(self.bak_path_edit.get_text())
        response = dialog.run()
        if response == Gtk.ResponseType.OK:
            self.bak_path_edit.set_text(dialog.get_filename())
        elif response == Gtk.ResponseType.CANCEL:
            print("Cancel clicked")

        dialog.destroy()


Mainform().loop()
Gtk.main()
