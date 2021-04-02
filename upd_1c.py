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
import datetime
import gi
import re
from pickle import NONE
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk  # @IgnorePep8

global log

BaseActions = dict(ba_update=0, ba_dumpib=1, ba_restoreib=2, ba_dumpcfg=3,
                   ba_loadcfg=4, ba_check=5, ba_enterprise=6, ba_config=7,
                   ba_cache=8, ba_journal=9, ba_integrity=10, ba_physical=11,
                   ba_macro=12, ba_convert=13, ba_updatecfg=14)


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

    def loop(self):
        self.wmain.show_all()

    def onDestroy(self, *args):
        Gtk.main_quit()

    def macrosAction(self, action, param=None, *args, **kwargs):

        try:
            if action == BaseActions['ba_macro']:
                title = 'Макрос запущен'
                for (key, val) in self.macros_liststore:
                    # key = row[0]
                    # val = row[1]
                    act = key.split('_')[1]
                    v_p = re.split('\(|\)', val)
                    if len(v_p) > 1:
                        par = v_p[1]
                    log.info('{} "{}"'.format(v_p[0], par))
                    # self.macrosAction(act, par)

            elif action == BaseActions['ba_update']:
                if param is not None:
                    title = 'Установка обновления'
                    log.info('{} "{}"'.format(title, param))
                    self.run_1c('DESIGNER', ['/UpdateCfg', param])
                    log.info('{} завершено!'.format(title))
                    # for row in self.macros_liststore:
                    # for k, v in row:
                    # # act = k.split('_')[1]
                    # param = re.split('(|)', v)[1]
                    # log.info('{} "{}"'.format(title, param))
              # case baseAction of
                # ba_update:
                # begin
                  # for i := 0 to ListBox1.Count - 1 do
                  # begin
                    # ListBox1.ClearSelection;
                    # ListBox1.Selected[i] := True;
                    # Caption := 'Установка обновления';
                    # AddLog(LogFile, Format('%s: "%s"', [Caption, ListBox1.Items[i]]));
                    # if not updateBase(ListBox1.Items[i], (i = ListBox1.Count - 1)) then
                    # raise Exception.Create('Ошибка обновления!');
                    # Inc(currentTask);
                  # end;
                  # ListBox1.ClearSelection;
                # end;
                pass

            elif action == BaseActions['ba_updatecfg']:
                title = 'Обновление конфигурации'
                log.info('{}'.format(title))
                if param is not None:
                    self.run_1c('DESIGNER', ['/UpdateDBCfg'])
                log.info('{} завершена!'.format(title))
            elif action == BaseActions['ba_dumpib']:
                title = 'Выгрузка информационной базы'
                log.info(title)
                log.debug('файл: {}'.format(param))
                self.run_1c('DESIGNER', ['/DumpIB', param])

                log.info('{} завершена!'.format(title))

                # DeleteOld(IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)), '*.dt', SpinEdit1.Value);
            elif action == BaseActions['ba_restoreib']:
                title = 'Загрузка информационной базы'
                log.info('{} "{}"'.format(title, param))
                if param is not None:
                    self.run_1c('DESIGNER', ['/RestoreIB', param])
                log.info('{} завершена!'.format(title))
            elif action == BaseActions['ba_dumpcfg']:
                title = 'Выгрузка конфигурации'
                log.info('{} "{}"'.format(title, param))
                if param is not None:
                    self.run_1c('DESIGNER', ['/DumpCFG', param])
                log.info('{} завершена!'.format(title))
                # DeleteOld(IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)), '*.cf', SpinEdit1.Value);
            elif action == BaseActions['ba_loadcfg']:
                title = 'Загрузка конфигурации'
                if param is not None:
                    ext_params = []
                    if param.endwith(".cfe"):
                        title = 'Загрузка расширения'
                        ext_params.append("-Extension")
                        ext_params.append(param.replace(".cfe", ""))
                    log.info('{} "{}"'.format(title, param))
                    self.run_1c('DESIGNER', ['/LoadCfg', param] + ext_params)
                    log.info('{} завершена!'.format(title))
            elif action == BaseActions['ba_check']:
                # Caption := 'Тестирование и исправление базы';
                # AddLog(LogFile, Caption);
                # if not CheckAndRepair(param) then
                    # raise Exception.Create(Caption + ' завершено с ошибкой!');
                # Caption := Caption + ' успешно завершено!';
                # AddLog(LogFile, Caption);
                # end;
                pass
            elif action == BaseActions['ba_enterprise']:

                title = 'Запуск в режиме ENTERPRISE'
                log.info(title)
                self.run_1c('ENTERPRISE')
                log.info('{} успешно завершен!'.format(title))
            elif action == BaseActions['ba_config']:
                title = 'Запуск в режиме конфигуратора'
                log.info(title)
                self.run_1c('DESIGNER')
                log.info('{} успешно завершен!'.format(title))

            elif action == BaseActions['ba_integrity']:
                title = 'Восстановление структуры информационной базы'
                log.info(title)
                self.run_1c('DESIGNER', ['/IBRestoreIntegrity'])
                log.info('{} завершено!'.format(title))
            elif action == BaseActions['ba_physical']:
                # begin
                # Caption := 'Восстановление физической целостности';
                # AddLog(LogFile, Caption);
                # if not CheckPhysicalIntegrity() then
                    # raise Exception.Create(Caption + ' завершено с ошибкой!');
                # Caption := Caption + ' успешно завершено!';
                # AddLog(LogFile, Caption);
                # end;
                pass
            elif action == BaseActions['ba_cache']:
                # ba_cache:
                # begin
                # Caption := 'Очистка кэша';
                # AddLog(LogFile, Caption);
                # if not ClearCache() then
                    # raise Exception.Create(Caption + ' завершена с ошибкой!');
                # Caption := Caption + ' успешно завершена!';
                # AddLog(LogFile, Caption);
                # end;
                pass
            elif action == BaseActions['ba_convert']:
                # begin
                # Caption := 'Конвертация файловой ИБ в новый формат';
                # AddLog(LogFile, Caption);
                # if not ConvertFileBase() then
                    # raise Exception.Create(Caption + ' завершена с ошибкой!');
                # Caption := Caption + ' успешно завершена!';
                # AddLog(LogFile, Caption);
                # end;
                pass
            elif action == BaseActions['ba_journal']:
                # ba_journal:
                # begin
                # Caption := 'Сокращение журнала регистрации';
                # if not param.IsEmpty then
                # begin
                    # AddLog(LogFile, Format('%s "%s"', [Caption, param]));
                    # if not ReduceEventLogSize(param) then
                    # raise Exception.Create(Caption + ' завершено с ошибкой!');
                    # Caption := Caption + ' успешно завершено!';
                    # AddLog(LogFile, Caption);
                    # DeleteOld(IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)), '*.lgd', SpinEdit1.Value);
                # end;
                # end;
                pass
        except Exception as e:
            log.info('"{}" завершена с ошибкой!'.format(title))
            log.debug(e)

    def run_1c(self, mode, params=None):
        proc = [self.exe_path_edit.get_text()]
        proc.append(mode)
        proc.append('/DisableStartupMessages')
        proc.append('/DisableStartupDialogs')
        proc.append('/DisableSplash')
        if params is not None:
            proc.extend(params)
        proc.append('/F"' + self.base_path_edit.get_text() + '"')
        proc.append('/N"' + self.UserE.get_text() + '"')
        proc.append('/P"' + self.PassE.get_text() + '"')
        proc.append('/Out "' + str(self.logfile) + '" -NoTruncate')
        subprocess.check_call(proc)

    def on_runMacro(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_macro'], ))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_updateIB(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(), pattern="*.cf|*.cfu")
            if fn:
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_update'], fn))
                self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_updateCFG(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_updatecfg'],))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_runEnterprise(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_enterprise'],))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_runDesigner(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_config'],))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_dumpIB(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            fn = Path(self.bak_path_edit.get_text()) / datetime.datetime.today().strftime('%d.%m.%Y_%H.%M.%S.dt')
            Path(self.bak_path_edit.get_text()).mkdir(parents=True, exist_ok=True)
            self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_dumpib'], str(fn)))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_restoreIB(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(), pattern="*.dt")
            if fn:
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_restoreib'], fn))
                self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_dumpCFG(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            fn = Path(self.bak_path_edit.get_text()) / datetime.datetime.today().strftime('%d.%m.%Y_%H.%M.%S.cf')
            Path(self.bak_path_edit.get_text()).mkdir(parents=True, exist_ok=True)
            self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_dumbcfg'], str(fn)))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_loadCFG(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(), pattern="*.cf|*.cfe")
            if fn:
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_restoreib'], fn))
                self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_IBRestoreIntegrity(self, widget):
        if self.main_thread is None or not self.main_thread.is_alive():
            self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_integrity'],))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def add_filters(self, dialog, pattern=None):
        _filter = Gtk.FileFilter()
        _filter.set_name("{} files".format(pattern))
        _filter.add_pattern(pattern)
        dialog.add_filter(_filter)

    def ChooserDialog(self, widget, action=Gtk.FileChooserAction.OPEN, filename=None, pattern=None):
        dialog = Gtk.FileChooserDialog(
            title="Please choose a file", parent=None, action=action
        )
        try:
            dialog.add_buttons(
                Gtk.STOCK_CANCEL,
                Gtk.ResponseType.CANCEL,
                Gtk.STOCK_OPEN,
                Gtk.ResponseType.OK,
            )
            if filename:
                dialog.set_filename(filename)

            if pattern is not None:
                for p in pattern.split("|"):
                    self.add_filters(dialog, p)

            response = dialog.run()
            if response == Gtk.ResponseType.OK:
                return dialog.get_filename()
        finally:
            dialog.destroy()

    def on_exe_path_btn_clicked(self, widget):
        fn = self.ChooserDialog(widget, filename=self.exe_path_edit.get_text(), pattern="*")
        if fn:
            self.exe_path_edit.set_text(fn)

    def on_base_path_btn_clicked(self, widget):
        fn = self.ChooserDialog(widget, action=Gtk.FileChooserAction.SELECT_FOLDER, filename=self.base_path_edit.get_text())
        if fn:
            self.base_path_edit.set_text(fn)

    def on_bak_path_btn_clicked(self, widget):
        fn = self.ChooserDialog(widget, action=Gtk.FileChooserAction.SELECT_FOLDER, filename=self.bak_path_edit.get_text())
        if fn:
            self.bak_path_edit.set_text(fn)


Mainform().loop()
Gtk.main()
