#!/usr/bin/env python3

'''
Created on 26 мар. 2021 г.

@author: demon
'''

import logging
import logger
import config
from pathlib import Path
import base64
import threading
import subprocess
import datetime
import gi
import re
import argparse
import sys
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk  # @IgnorePep8

global log

IS_WIN = sys.platform.startswith("win")
if getattr(sys, 'frozen', False):
    # we are running in a bundle
    self_dir = sys._MEIPASS  # @UndefinedVariable
    executable = sys.executable
else:
    # we are running in a normal Python environment
    self_dir = Path(__file__).parent.as_posix()
    executable = __file__


class BaseActions:
    ba_update = 0
    ba_dumpib = 1
    ba_restoreib = 2
    ba_dumpcfg = 3
    ba_loadcfg = 4
    ba_check = 5
    ba_enterprise = 6
    ba_config = 7
    ba_cache = 8
    ba_journal = 9
    ba_integrity = 10
    ba_physical = 11
    ba_macro = 12
    ba_convert = 13
    ba_updatecfg = 14


class Options():
    _instance = None
    _lock = threading.Lock()

    @staticmethod
    def get_instance():
        if Options._instance is None:
            with Options._lock:
                Options._instance = Options()
        return Options._instance

    def __init__(self):
        parser = argparse.ArgumentParser(prog=Path(executable).name, add_help=True)
        parser.add_argument('ini_file', nargs="?", default=str(Path(executable).with_name('upd_1c.ini')),
                            help='Path to ini file')
        parser.add_argument('--not_interactive', '-n', action='store_true', default=False,
                            help='Run in non interactive mode')

        self.options = parser.parse_args()

    def __call__(self):
        return self.options


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


class TestCheck():
    def __init__(self, params=[4]):
        self.builder = Gtk.Builder()
        self.builder.add_from_file(str(Path(self_dir) / "forms/testcheck.glade"))
        self.testcheck_dlg = self.builder.get_object('testcheckform')
        self.testcheck_dlg.add_buttons(
            Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL, Gtk.STOCK_OK, Gtk.ResponseType.OK
        )
        for c in range(0, 5):
            try:
                chk = self.builder.get_object('chk' + c)
                chk.set_active(c in params)
            except Exception:
                break

    def run(self):
        try:
            return self.testcheck_dlg.run()
        finally:
            self.testcheck_dlg.destroy()

    def get_param(self):
        chk_params = []
        for c in range(0, 5):
            try:
                chk = self.builder.get_object('chk{}'.format(c))
                if chk.get_active():
                    chk_params.append(c)
            except Exception as e:
                break
        return ";".join(map(str, chk_params))


class Mainform():
    def __init__(self):
        global log
        self.options = Options.get_instance()()
        self.logfile = Path(self.options.ini_file).parent / 'logs' / Path(self.options.ini_file).with_suffix(".log").name
        log = logger.get_logger(__name__, self.logfile, level=logging.DEBUG, callback=self.logs_callback)
        builder = Gtk.Builder()
        builder.add_from_file(str(Path(self_dir) / "forms/mainform.glade"))
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
        self.macros_list.append_column(Gtk.TreeViewColumn("commands", Gtk.CellRendererText(), text=1))
        self.macros_liststore = builder.get_object('macros_liststore')
        self.logs_textbuffer = builder.get_object('logs_textbuffer')
        self.logview = builder.get_object('logview')
        self.testcheck_dlg = builder.get_object('testcheckform')
        self.commands_menu = builder.get_object('commands_menu')

        log.info('Started')
        self.populate_liststore_menu()
        self.configure()
        self.main_thread = None
        if self.options.not_interactive:
            self.macrosAction(BaseActions.ba_macro)
        else:
            self.wmain.show_all()
            Gtk.main()

    def configure(self, widget=None):
        self.wmain.set_title('Upd_1c')
        self.status_bar.push(1, str(self.options.ini_file))
        self.ini = config.Config(self.options.ini_file)
        if self.ini.has_section(config.SectionBase):
            self.UserE.set_text(self.ini[config.SectionBase].get(config.KeyUser, ''))
            self.bak_path_edit.set_text(self.ini[config.SectionBase].get(config.KeyBakcupPath, ''))
            self.base_path_edit.set_text(self.ini[config.SectionBase].get(config.KeyPath, ''))
            self.PassE.set_text(base64.b64decode(self.ini[config.SectionBase].get(config.KeyPass, '')).decode('UTF-8'))
            self.bak_count_edit.set_value(int(self.ini[config.SectionBase].get(config.KeyBackupCount, '3')))
            self.page_size_edit.set_value(int(self.ini[config.SectionBase].get(config.KeyPageSize, '8')))
        if self.ini.has_section(config.SectionMain):
            self.exe_path_edit.set_text(self.ini[config.SectionMain].get(config.KeyExecutable, ''))

        self.macros_liststore.clear()
        if self.ini.has_section(config.SectionMacro):
            for key, val in self.ini.items(config.SectionMacro):
                _key = key.split("_")[1]
                self.macros_liststore.append([_key, val])
            self.ini[config.SectionMacro].clear()

        log.info("Настройки загружены")

    def on_save_config(self, widget):
        if not self.ini.has_section(config.SectionMain):
            self.ini.add_section(config.SectionMain)
        if not self.ini.has_section(config.SectionBase):
            self.ini.add_section(config.SectionBase)
        if not self.ini.has_section(config.SectionMacro):
            self.ini.add_section(config.SectionMacro)
        self.ini.set(config.SectionMain, config.KeyExecutable, self.exe_path_edit.get_text())
        self.ini.set(config.SectionBase, config.KeyUser, self.UserE.get_text())
        self.ini.set(config.SectionBase, config.KeyPass, base64.b64encode(self.PassE.get_text().encode('UTF-8')).decode('UTF-8'))
        self.ini.set(config.SectionBase, config.KeyBakcupPath, self.bak_path_edit.get_text())
        self.ini.set(config.SectionBase, config.KeyPath, self.base_path_edit.get_text())
        self.ini.set(config.SectionBase, config.KeyBackupCount, self.bak_count_edit.get_text())
        self.ini.set(config.SectionBase, config.KeyPageSize, self.page_size_edit.get_text())

        for i, cmd in enumerate(self.macros_liststore):
            key = "{}_{}".format(i, cmd[0])
            self.ini.set(config.SectionMacro, key, cmd[1])
        self.ini.write()
        log.info("Настройки сохранены")

    def populate_liststore_menu(self):
        self.macros_menu = Gtk.Menu()
        menu_add = Gtk.MenuItem(label="Добавить")
        menu_add.set_submenu(self.commands_menu)
        self.macros_menu.append(menu_add)
        menu_del = Gtk.MenuItem(label="Удалить")
        menu_del.connect("activate", self.on_menu_del_click)
        self.macros_menu.append(menu_del)
        menu_del_all = Gtk.MenuItem(label="Удалить все")
        menu_del_all.connect("activate", self.on_menu_del_all_click)
        self.macros_menu.append(menu_del_all)
        # self.macros_menu.attach_to_widget(self.macros_list)
        self.macros_menu.show_all()

    def on_menu_del_click(self, widget):
        sel = self.macros_list.get_selection().get_selected()
        if sel[1] is not None:
            self.macros_liststore.remove(sel[1])

    def on_menu_del_all_click(self, widget):
        self.macros_liststore.clear()

    def on_macros_list_popup_menu(self, widget, event=None):
        if (event is not None and event.button == 3) or event is None:
            # if not self.commands_menu.get_attach_widget() == widget:
                # self.commands_menu.detach()
                # self.commands_menu.attach_to_widget(widget)
            # self.macros_menu.get_children()[0].add_child(self.commands_menu, None, None)
            self.commands_menu.set_name("macros")
            # self.macros_menu.popup(None, None, None, None, event.button, event.time)
            self.macros_menu.popup_at_pointer(None)

    def on_commands_menu_select(self, widget):
        self.commands_menu.set_name("menu")
        # if not self.commands_menu.get_attach_widget() == widget:
        # self.commands_menu.detach()
        # self.commands_menu.attach_to_widget(widget)
        # self.commands_menu.popup_at_widget(widget, Gdk.Gravity(1), Gdk.Gravity(1), None)

    def logs_callback(self, msg):
        self.logs_textbuffer.insert(self.logs_textbuffer.get_end_iter(), msg)
        self.logview.scroll_to_iter(self.logs_textbuffer.get_end_iter(), 0, False, 0, 0)

    def onDestroy(self, *args):
        Gtk.main_quit()

    def on_click_about(self, widget):
        dlg = Gtk.MessageDialog(parent=self.wmain,
                                title="upd_1c",
                                text="""Программа для пакетного обновления типовых конфигураций!
Version: {ver}
Автор: Дмитрий Воротилин, dvor85@gmail.com""".format(ver=config.Version))
        dlg.add_buttons(Gtk.STOCK_OK,
                        Gtk.ResponseType.OK)
        dlg.run()
        dlg.destroy()

    def macrosAction(self, action, param=None, *args, **kwargs):
        try:
            if action == BaseActions.ba_macro:
                title = 'Макрос'
                log.info("{} запущен".format(title))
                for (key, val) in self.macros_liststore:
                    _action = int(key)
                    _param = None
                    v_p = re.split('\(|\)', val)
                    if len(v_p) > 1:
                        _param = v_p[1]
                    self.macrosAction(_action, _param)
                log.info("{} завершен".format(title))

            elif action == BaseActions.ba_update:
                if param is not None:
                    title = 'Установка обновления'
                    log.info('{} "{}"'.format(title, param))
                    self.run_1c('DESIGNER', ['/UpdateCfg', param])
                    log.info('{} завершено!'.format(title))

            elif action == BaseActions.ba_updatecfg:
                title = 'Обновление конфигурации'
                log.info('{}'.format(title))
                self.run_1c('DESIGNER', ['/UpdateDBCfg'])
                log.info('{} завершена!'.format(title))

            elif action == BaseActions.ba_dumpib:
                title = 'Выгрузка информационной базы'
                fn = Path(self.bak_path_edit.get_text()) / datetime.datetime.today().strftime('%d.%m.%Y_%H.%M.%S.dt')
                fn.parent.mkdir(parents=True, exist_ok=True)
                log.info('{} в файл "{}"'.format(title, fn.as_posix()))
                self.run_1c('DESIGNER', ['/DumpIB', fn.as_posix()])
                self.delete_old(self.bak_path_edit.get_text(), "*.dt", int(self.bak_count_edit.get_text()))
                log.info('{} завершена!'.format(title))

            elif action == BaseActions.ba_restoreib:
                title = 'Загрузка информационной базы'
                log.info('{} "{}"'.format(title, param))
                if param is not None:
                    self.run_1c('DESIGNER', ['/RestoreIB', param])
                log.info('{} завершена!'.format(title))

            elif action == BaseActions.ba_dumpcfg:
                title = 'Выгрузка конфигурации'
                fn = Path(self.bak_path_edit.get_text()) / datetime.datetime.today().strftime('%d.%m.%Y_%H.%M.%S.cf')
                fn.parent.mkdir(parents=True, exist_ok=True)
                log.info('{} "{}"'.format(title, fn.as_posix()))
                self.run_1c('DESIGNER', ['/DumpCFG', fn.as_posix()])
                self.delete_old(self.bak_path_edit.get_text(), "*.cf", int(self.bak_count_edit.get_text()))
                log.info('{} завершена!'.format(title))

            elif action == BaseActions.ba_loadcfg:
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

            elif action == BaseActions.ba_enterprise:
                title = 'Запуск в режиме ENTERPRISE'
                log.info(title)
                self.run_1c('ENTERPRISE')
                log.info('{} успешно завершен!'.format(title))

            elif action == BaseActions.ba_config:
                title = 'Запуск в режиме конфигуратора'
                log.info(title)
                self.run_1c('DESIGNER')
                log.info('{} успешно завершен!'.format(title))

            elif action == BaseActions.ba_integrity:
                title = 'Восстановление структуры информационной базы'
                log.info(title)
                self.run_1c('DESIGNER', ['/IBRestoreIntegrity'])
                log.info('{} завершено!'.format(title))

            elif action == BaseActions.ba_physical:
                title = 'Восстановление физической целостности'
                log.info(title)
                suf = ".exe" if IS_WIN else ""
                subprocess.check_call([Path(self.exe_path_edit.get_text()).parent / 'chdbfl{}'.format(suf)])
                log.info('{} завершено!'.format(title))

            elif action == BaseActions.ba_check:
                title = 'Тестирование и  исправление'
                log.info(title)
                if param is not None:
                    chk_params = []
                    params_map = ['-ReIndex', '-LogIntegrity', '-LogAndRefsIntegrity',
                                  '-RecalcTotals', '-IBCompression', '-Rebuild']
                    for p in param.split(';'):
                        chk_params.append(params_map[int(p)])
                    self.run_1c('DESIGNER', ['/IBCheckAndRepair'] + chk_params)
                log.info('{} завершено!'.format(title))

            elif action == BaseActions.ba_cache:
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

            elif action == BaseActions.ba_convert:
                title = 'Конвертация файловой ИБ в новый формат'
                log.info('{} с размером страницы {}k'.format(title, param))
                if param is not None:
                    suf = ".exe" if IS_WIN else ""
                    proc = [Path(self.exe_path_edit.get_text()).parent / 'cnvdbfl{}'.format(suf)]
                    proc.append('--convert')
                    proc.append('--format=8.3.8')
                    proc.append('--page={}k'.format(param))
                    proc.append(str(Path(self.base_path_edit.get_text()) / '1Cv8.1CD'))
                    log.info(subprocess.check_output(proc).decode())
                log.info('{} завершено!'.format(title))

            elif action == BaseActions.ba_journal:
                title = 'Сокращение журнала регистрации'
                self.delete_old(self.bak_path_edit.get_text(), "*.lgd", int(self.bak_count_edit.get_text()))

        except Exception as e:
            log.info('"{}" завершена с ошибкой!'.format(title))
            log.debug(e)

    def delete_old(self, folder, pattern, need_count):
        files = sorted(Path(folder).glob(pattern), key=lambda f: f.stat().st_ctime, reverse=True)
        try:
            for f in files[need_count:]:
                log.info('Удаление устаревшего файла "{}"'.format(str(f)))
                f.unlink()
        except Exception as e:
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
        action = BaseActions.ba_macro
        self.run_in_thread(widget, action)

    def on_updateIB(self, widget):
        action = BaseActions.ba_update
        fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(),
                                pattern="*.cf|*.cfu", title="Выберите файл конфигурации")
        if not fn:
            return
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action, fn)
        else:
            self.add_macros_command(widget, action, fn)

    def on_updateCFG(self, widget):
        action = BaseActions.ba_updatecfg
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action)
        else:
            self.add_macros_command(widget, action)

    def on_runEnterprise(self, widget):
        action = BaseActions.ba_enterprise
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action)
        else:
            self.add_macros_command(widget, action)

    def on_runDesigner(self, widget):
        action = BaseActions.ba_config
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action)
        else:
            self.add_macros_command(widget, action)

    def on_dumpIB(self, widget):
        action = BaseActions.ba_dumpib
        if self.commands_menu.get_name() == "menu":
            fn = Path(self.bak_path_edit.get_text()) / datetime.datetime.today().strftime('%d.%m.%Y_%H.%M.%S.dt')
            fn.parent.mkdir(parents=True, exist_ok=True)
            self.run_in_thread(widget, action, fn.as_posix())
        else:
            self.add_macros_command(widget, action)

    def on_restoreIB(self, widget):
        action = BaseActions.ba_restoreib
        fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(), pattern="*.dt", title="Выберите файл выгрузки")
        if not fn:
            return
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action, fn)
        else:
            self.add_macros_command(widget, action, fn)

    def on_dumpCFG(self, widget):
        action = BaseActions.ba_dumpcfg
        if self.commands_menu.get_name() == "menu":
            fn = Path(self.bak_path_edit.get_text()) / datetime.datetime.today().strftime('%d.%m.%Y_%H.%M.%S.cf')
            fn.parent.mkdir(parents=True, exist_ok=True)
            self.run_in_thread(widget, action, fn.as_posix())
        else:
            self.add_macros_command(widget, action)

    def on_loadCFG(self, widget):
        action = BaseActions.ba_loadcfg
        fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(), pattern="*.cf|*.cfe",
                                title="Выберите файл конфигурации или расширения")
        if not fn:
            return
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action, fn)
        else:
            self.add_macros_command(widget, action, fn)

    def on_testcheck(self, widget):
        action = BaseActions.ba_check
        testcheck_dlg = TestCheck()
        res = testcheck_dlg.run()
        if res == Gtk.ResponseType.OK:
            param = testcheck_dlg.get_param()
            if self.commands_menu.get_name() == "menu":
                self.run_in_thread(widget, action, param)
            else:
                self.add_macros_command(widget, action, param)

    def on_IBRestoreIntegrity(self, widget):
        action = BaseActions.ba_integrity
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action)
        else:
            self.add_macros_command(widget, action)

    def on_physical_restore(self, widget):
        action = BaseActions.ba_physical
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action)
        else:
            self.add_macros_command(widget, action)

    def on_convert_fib(self, widget):
        action = BaseActions.ba_convert
        if self.commands_menu.get_name() == "menu":
            self.run_in_thread(widget, action, self.page_size_edit.get_text())
        else:
            self.add_macros_command(widget, action, self.page_size_edit.get_text())

    def run_in_thread(self, widget, action, param=None):
        if self.main_thread is None or not self.main_thread.is_alive():
            self.main_thread = MyThread(target=self.macrosAction, name=widget.get_label(), args=(action, param))
            self.main_thread.start()
        else:
            log.info('Дождитесь завершения предыдущей команды!')

    def add_macros_command(self, widget, action, param=None):
        sel = self.macros_list.get_selection().get_selected()
        pos = -1
        if sel[1] is not None:
            pos = int(self.macros_liststore.get_string_from_iter(sel[1]))

        if param is not None:
            self.macros_liststore.insert(pos, [str(action), "{cmd}({param})".format(cmd=widget.get_label(), param=param)])
        else:
            self.macros_liststore.insert(pos, [str(action), widget.get_label()])

    def add_filters(self, dialog, pattern=None):
        _filter = Gtk.FileFilter()
        _filter.set_name("{} files".format(pattern))
        _filter.add_pattern(pattern)
        dialog.add_filter(_filter)

    def ChooserDialog(self, widget, action=Gtk.FileChooserAction.OPEN, filename=None, pattern=None, title=None):
        dialog = Gtk.FileChooserDialog(
            title=title, parent=self.wmain, action=action
        )
        dialog.set_local_only(False)
        dialog
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
        suf = ".exe" if IS_WIN else ""
        fn = self.ChooserDialog(widget, filename=self.exe_path_edit.get_text(),
                                pattern="*" + suf, title="Выберите исполняемый файл 1с")
        if fn:
            self.exe_path_edit.set_text(fn)

    def on_base_path_btn_clicked(self, widget):
        fn = self.ChooserDialog(widget, action=Gtk.FileChooserAction.SELECT_FOLDER,
                                filename=self.base_path_edit.get_text(), title="Выберите каталог с базой 1с")
        if fn:
            self.base_path_edit.set_text(fn)

    def on_bak_path_btn_clicked(self, widget):
        fn = self.ChooserDialog(widget, action=Gtk.FileChooserAction.SELECT_FOLDER,
                                filename=self.bak_path_edit.get_text(), title="Выберитите каталог для бэкапа")
        if fn:
            self.bak_path_edit.set_text(fn)


if __name__ == "__main__":
    Mainform()
