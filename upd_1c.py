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
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk  # @IgnorePep8

global log


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
        log = logger.get_logger(__name__, self.logfile, logging.INFO)
        log.info('started!', callback=self.logs_callback)

    def logs_callback(self, msg):
        self.logs_textbuffer.insert(self.logs_textbuffer.get_end_iter(), msg)

    def create(self):
        log = logger.get_logger('upd_1c', level=logging.DEBUG)
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
