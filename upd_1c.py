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
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk, Gdk  # @IgnorePep8

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


class TestCheck():
    def __init__(self, params=[4]):
        self.builder = Gtk.Builder()
        self.builder.add_from_file("forms/testcheck.glade")
        self.testcheck_dlg = self.builder.get_object('testcheckform')
        self.testcheck_dlg.add_buttons(
            Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL, Gtk.STOCK_OK, Gtk.ResponseType.OK
        )
        self.chk_params = params
        for c in range(0, 5):
            try:
                chk = self.builder.get_object('chk' + c)
                chk.set_active(c in self.chk_params)
            except Exception:
                break

    def run(self):
        try:
            return self.testcheck_dlg.run()
        finally:
            self.testcheck_dlg.destroy()

    def get_param(self):
        for c in range(0, 5):
            try:
                chk = self.builder.get_object('chk{}'.format(c))
                if chk.get_active():
                    self.chk_params.append(c)
            except Exception as e:
                break
        return ";".join(map(str, self.chk_params))


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
        self.testcheck_dlg = builder.get_object('testcheckform')
        self.commands_menu = builder.get_object('commands_menu')
        self.populate_liststore_menu()

        self.configure()

        log.info('Started')
        self.main_thread = None
        self.wmain.show_all()
        Gtk.main()

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

    def on_macros_list_popup_menu(self, widget, event):
        if event.button == 3:
            # if not self.commands_menu.get_attach_widget() == widget:
                # self.commands_menu.detach()
                # self.commands_menu.attach_to_widget(widget)
            # self.macros_menu.get_children()[0].add_child(self.commands_menu, None, None)
            self.commands_menu.set_name("macros")
            self.macros_menu.popup_at_pointer(None)

    def on_commands_menu_select(self, widget):
        self.commands_menu.set_name("menu")
        # if not self.commands_menu.get_attach_widget() == widget:
        # self.commands_menu.detach()
        # self.commands_menu.attach_to_widget(widget)
        # self.commands_menu.popup_at_widget(widget, Gdk.Gravity(1), Gdk.Gravity(1), None)

    def logs_callback(self, msg):
        self.logs_textbuffer.insert(self.logs_textbuffer.get_end_iter(), msg)

    def configure(self):
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

        self.logfile = self.ini[config.SectionMain].get(
            config.KeyLogFile, Path(os.path.dirname(__file__)) / 'logs' / 'upd_1c.log')
        global log
        log = logger.get_logger(__name__, self.logfile, level=logging.DEBUG, callback=self.logs_callback)

        for key, val in self.ini[config.SectionMacro].items():
            _key = key.split("_")[1]
            self.macros_liststore.append([_key, val])
        renderer = Gtk.CellRendererText()
        column = Gtk.TreeViewColumn("commands", renderer, text=1)
        self.macros_list.append_column(column)
        self.ini[config.SectionMacro].clear()

    def on_save_config(self, widget):
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

    def onDestroy(self, *args):
        Gtk.main_quit()

    def macrosAction(self, action, param=None, *args, **kwargs):
        try:
            if action == BaseActions['ba_macro']:
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

            elif action == BaseActions['ba_update']:
                if param is not None:
                    title = 'Установка обновления'
                    log.info('{} "{}"'.format(title, param))
                    self.run_1c('DESIGNER', ['/UpdateCfg', param])
                    log.info('{} завершено!'.format(title))

            elif action == BaseActions['ba_updatecfg']:
                title = 'Обновление конфигурации'
                log.info('{}'.format(title))
                if param is not None:
                    self.run_1c('DESIGNER', ['/UpdateDBCfg'])
                log.info('{} завершена!'.format(title))
            elif action == BaseActions['ba_dumpib']:
                title = 'Выгрузка информационной базы'
                log.info('{} в файл "{}"'.format(title, param))
                if param is not None:
                    self.run_1c('DESIGNER', ['/DumpIB', param])
                    self.delete_old(self.bak_path_edit.get_text(), "*.dt", int(self.bak_count_edit.get_text()))
                log.info('{} завершена!'.format(title))

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
                    self.delete_old(self.bak_path_edit.get_text(), "*.cf", int(self.bak_count_edit.get_text()))
                log.info('{} завершена!'.format(title))

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
                title = 'Восстановление физической целостности'
                log.info(title)
                chdbfl = Path(self.exe_path_edit.get_text()).parent / 'chdbfl'
                subprocess.check_call([str(chdbfl)])
                log.info('{} завершено!'.format(title))

            elif action == BaseActions['ba_check']:
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
                title = 'Конвертация файловой ИБ в новый формат'
                log.info('{} с размером страницы {}k'.format(title, param))
                if param is not None:
                    proc = [Path(self.exe_path_edit.get_text()).parent / 'cnvdbfl']
                    proc.append('--convert')
                    proc.append('--format=8.3.8')
                    proc.append('--page={}k'.format(param))
                    proc.append(str(Path(self.base_path_edit.get_text()) / '1Cv8.1CD'))
                    log.info(subprocess.check_output(proc).decode())
                log.info('{} завершено!'.format(title))

            elif action == BaseActions['ba_journal']:
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
        if self.main_thread is None or not self.main_thread.is_alive():
            self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(BaseActions['ba_macro'], ))
            self.main_thread.start()
        else:
            log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))

    def on_updateIB(self, widget):
        action = BaseActions['ba_update']
        fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(), pattern="*.cf|*.cfu")
        if not fn:
            return
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action, fn))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action, fn)

    def on_updateCFG(self, widget):
        action = BaseActions['ba_updatecfg']
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action,))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action)

    def on_runEnterprise(self, widget):
        action = BaseActions['ba_enterprise']
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action,))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action)

    def on_runDesigner(self, widget):
        action = BaseActions['ba_config']
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action,))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action)

    def on_dumpIB(self, widget):
        action = BaseActions['ba_dumpib']
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                fn = Path(self.bak_path_edit.get_text()) / datetime.datetime.today().strftime('%d.%m.%Y_%H.%M.%S.dt')
                Path(self.bak_path_edit.get_text()).mkdir(parents=True, exist_ok=True)
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action, str(fn)))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action)

    def on_restoreIB(self, widget):
        action = BaseActions['ba_restoreib']
        fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(), pattern="*.dt")
        if not fn:
            return
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action, fn))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action, fn)

    def on_dumpCFG(self, widget):
        action = BaseActions['ba_dumpcfg']
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                fn = Path(self.bak_path_edit.get_text()) / datetime.datetime.today().strftime('%d.%m.%Y_%H.%M.%S.cf')
                Path(self.bak_path_edit.get_text()).mkdir(parents=True, exist_ok=True)
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action, str(fn)))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action)

    def on_loadCFG(self, widget):
        action = BaseActions['ba_loadcfg']
        fn = self.ChooserDialog(widget, filename=self.bak_path_edit.get_text(), pattern="*.cf|*.cfe")
        if not fn:
            return
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action, fn))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action, fn)

    def on_testcheck(self, widget):
        action = BaseActions['ba_check']
        testcheck_dlg = TestCheck()
        res = testcheck_dlg.run()
        if res == Gtk.ResponseType.OK:
            chk_params = testcheck_dlg.get_param()
            if self.commands_menu.get_name() == "menu":
                if self.main_thread is None or not self.main_thread.is_alive():
                    self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action, chk_params))
                    self.main_thread.start()
                else:
                    log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
            else:
                self.add_macros_command(widget, action, chk_params)

    def on_IBRestoreIntegrity(self, widget):
        action = BaseActions['ba_integrity']
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action,))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action)

    def on_physical_restore(self, widget):
        action = BaseActions['ba_physical']
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(action,))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action)

    def on_convert_fib(self, widget):
        action = BaseActions['ba_convert']
        if self.commands_menu.get_name() == "menu":
            if self.main_thread is None or not self.main_thread.is_alive():
                self.main_thread = MyThread(target=self.macrosAction, name=__name__, args=(
                    action, self.page_size_edit.get_text()))
                self.main_thread.start()
            else:
                log.info('"{n}" уже запущено. Дождитесь завершения!'.format(n=widget.get_label()))
        else:
            self.add_macros_command(widget, action, self.page_size_edit.get_text())

    def add_macros_command(self, widget, action, param=None):
        sel = self.macros_list.get_selection().get_selected()
        if sel[1] is not None:
            pos = int(self.macros_liststore.get_string_from_iter(sel[1]))
            if param is not None:
                self.macros_liststore.insert(pos, [str(action), "{cmd}({param})".format(cmd=widget.get_label(), param=param)])
            else:
                self.macros_liststore.insert(pos, [str(action), widget.get_label()])
        else:
            if param is not None:
                self.macros_liststore.append([str(action),
                                              "{cmd}({param})".format(cmd=widget.get_label(), param=param)])
            else:
                self.macros_liststore.append([str(action), widget.get_label()])

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


if __name__ == "__main__":
    Mainform()
