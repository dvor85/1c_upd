unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Menus, ComCtrls, ActnList, Spin, ExtDlgs, FileUtil, IniFiles,
  lazutf8, ShortPathEdit, LCLIntf, TypInfo, CheckAndRepairForm;

const
  Version: string = '2.2.5';
  SectionMain: string = 'Main';
  KeyExecutable: string = 'Executable';
  SectionBase: string = 'Base';
  KeyPath: string = 'Path';
  KeyBakcupPath: string = 'BakcupPath';
  KeyBackupCount: string = 'BakcupCount';
  KeyUser: string = 'User';
  KeyPass: string = 'Password';
  KeyPageSize: string = 'PageSize';
  KeyLogFile: string = 'LogFile';
  SectionUpdates: string = 'Updates';
  SectionMacro: string = 'Macro';

type

  TBaseAction = (ba_update, ba_dumpib, ba_restoreib, ba_dumpcfg,
    ba_loadcfg, ba_check, ba_enterprise, ba_config, ba_cache, ba_journal, ba_integrity, ba_physical,
    ba_macro, ba_convert);

  TMyThread = class(TThread)
    FBaseAction: TBaseAction;
    FParam: string;
    constructor Create(BaseAction: TBaseAction; Param: string = ''); overload;
  protected
    procedure Execute; override;
  public
    procedure OnMyTerminate(Sender: TObject);
  end;

  { TForm1 }

  TForm1 = class(TForm)

    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CalendarDialog1: TCalendarDialog;
    commands_menu: TMenuItem;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    bakpath_edit: TLabeledEdit;
    basepath_edit: TLabeledEdit;
    executable_edit: TLabeledEdit;
    user_edit: TLabeledEdit;
    pass_edit: TLabeledEdit;
    macro_list: TListBox;
    MainMenu1: TMainMenu;
    MenuItem26: TMenuItem;
    mi_runmacro: TMenuItem;
    mi_del_from_macro: TMenuItem;
    mi_del_all: TMenuItem;
    mi_delcache: TMenuItem;
    mi_reduce_journal: TMenuItem;
    mi_repairib: TMenuItem;
    mi_repair_physical: TMenuItem;
    mi_convert: TMenuItem;
    mi_save_config: TMenuItem;
    mi_reload_config: TMenuItem;
    mi_add_to_macro: TMenuItem;
    PopupMenu2: TPopupMenu;
    logs_memo: TMemo;
    mi_dumpib: TMenuItem;
    mi_loadib: TMenuItem;
    MenuItem12: TMenuItem;
    mi_loadcfg: TMenuItem;
    mi_dumpcfg: TMenuItem;
    mi_updatecfg: TMenuItem;
    MenuItem16: TMenuItem;
    mi_testcheck: TMenuItem;
    MenuItem4: TMenuItem;
    mi_run_enterprise: TMenuItem;
    mi_run_designer: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;

    OpenDialog1: TOpenDialog;
    Process1: TProcess;
    SomeProc: TProcess;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    bakcount_edit: TSpinEdit;
    pagesize_edit: TSpinEdit;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;

    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure executable_editChange(Sender: TObject);
    procedure mi_dumpibClick(Sender: TObject);
    procedure mi_loadibClick(Sender: TObject);
    procedure mi_loadcfgClick(Sender: TObject);
    procedure mi_dumpcfgClick(Sender: TObject);
    procedure mi_updatecfgClick(Sender: TObject);

    procedure mi_runmacroClick(Sender: TObject);
    procedure mi_del_from_macroClick(Sender: TObject);
    procedure mi_del_allClick(Sender: TObject);

    procedure mi_delcacheClick(Sender: TObject);
    procedure mi_reduce_journalClick(Sender: TObject);
    procedure mi_repairibClick(Sender: TObject);
    procedure mi_repair_physicalClick(Sender: TObject);
    procedure mi_convertClick(Sender: TObject);
    procedure mi_testcheckClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure mi_run_enterpriseClick(Sender: TObject);
    procedure mi_run_designerClick(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);

  private
    { private declarations }
    MyThread: TMyThread;
    LogFile: string;
    iniPath: string;
    ini: TIniFile;
    currentTask, progress: integer;
    function GetIBCheckParams(): string;
    procedure ExecuteBaseAction(baseAction: TBaseAction; param: string);
    procedure SaveSettings();
    procedure SetComponentsEnabled(State: boolean);
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    function runEnterprise(): boolean;
    function runConfig(): boolean;
    function dumpIB(): boolean;
    function restoreIB(filename: string): boolean;
    function dumpCFG(): boolean;
    function loadCFG(filename: string; updateCfg: boolean = True): boolean;
    function CheckAndRepair(param: string): boolean;
    function updateBase(filename: string; updateCfg: boolean = False): boolean;
    function IBRestoreIntegrity(): boolean;
    function ClearCache(): boolean;
    function ReduceEventLogSize(date: string): boolean;
    function CheckPhysicalIntegrity(): boolean;
    function ConvertFileBase(): boolean;
    function AddMacrosCommand(Sender: TObject; baseAction: TBaseAction; param: string = ''): boolean;
  public
    { public declarations }
  end;


var
  Form1: TForm1;
  run_non_interactive: boolean = False;


implementation

uses base64;


{$R *.lfm}

procedure AddLog(LogFileName: string; LogString: string);
var
  F: TFileStream;
  PStr: PChar;
  Str: string;
  LengthLogString: cardinal;
begin
  Str := DateTimeToStr(Now()) + ': ' + LogString + #13#10;
  LengthLogString := Length(Str);
  try
    if FileExists(LogFileName) then
      F := TFileStream.Create(LogFileName, fmOpenWrite)
    else
    begin
      ForceDirectories(ExtractFileDir(LogFileName));
      F := TFileStream.Create(LogFileName, fmCreate);
    end;
  except
    MessageDlg(Form1.Caption, LogString, mtError, [mbYes], 0);
    Exit;
  end;
  PStr := StrAlloc(LengthLogString + 1);
  try
    try
      StrPCopy(PStr, Str);
      F.Position := F.Size;
      F.Write(PStr^, LengthLogString);
      Form1.logs_memo.Lines.AddText(Str);
      //SendMessage(Form1.Memo1.Handle, EM_LINESCROLL, 0, Form1.Memo1.Lines.Count);
    except
      MessageDlg(Form1.Caption, LogString, mtError, [mbYes], 0);
      Exit;
    end;
  finally
    StrDispose(PStr);
    F.Free;
  end;
end;

function SortByCTime(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := FileAge(List[index2]) - FileAge(List[index1]);
end;

procedure DeleteOld(dir, mask: string; needed: integer);
//Удалить старые архивы
var
  flist: TStringList;
  i: integer;
begin
  flist := TStringList.Create;
  try
    FindAllFiles(flist, dir, mask, False);
    flist.CustomSort(@SortByCTime);
    for i := needed to flist.Count - 1 do
    begin
      if DeleteFile(PChar(flist[i])) then
        AddLog(Form1.LogFile, 'Удаление устаревшего: "' + flist[i] + '"');
    end;
  finally
    flist.Free;
  end;
end;


{ TForm1 }


constructor TMyThread.Create(BaseAction: TBaseAction; Param: string = '');
begin
  inherited Create(False);
  FBaseAction := BaseAction;
  FreeOnTerminate := True;
  OnTerminate := @OnMyTerminate;
  FParam := Param;
end;

procedure TMyThread.OnMyTerminate(Sender: TObject);
begin
  if run_non_interactive then
    Form1.Close();
end;


procedure TMyThread.Execute;
var
  i, totalTasks: integer;
  msg, param: string;
  params: TStringArray;
begin
  Form1.SetComponentsEnabled(False);
  Form1.progress := 0;
  try
    try
      with Form1 do
      begin
        if Process1.Running then
          raise Exception.Create('Процесс уже запущен, дождитесь окончания!');

        if (Process1.Executable = '') or (basepath_edit.Text = '') then
          raise Exception.Create('Не заполнены необходимые поля!');
        if (FBaseAction = ba_macro) then
        begin
          currentTask := 0;
          totalTasks := macro_list.Count + macro_list.Count;
          if (macro_list.Count < 1) then
            raise Exception.Create('Действия для макроса не заполнены!');

          for i := 0 to macro_list.Count - 1 do
          begin
            params := macro_list.Items[i].Split(['(', ')']);
            param := '';
            if length(params) > 1 then
              param := params[1];
            macro_list.ClearSelection;
            macro_list.Selected[i] := True;
            ExecuteBaseAction(TbaseAction(PtrInt(macro_list.Items.Objects[i])), param);
            Inc(currentTask);
            progress := currentTask * 100 div totalTasks;
          end;
          macro_list.ClearSelection;
          Caption := 'Выполнение макроса успешно завершено!';
          AddLog(LogFile, Caption);
        end
        else
          ExecuteBaseAction(FBaseAction, FParam);
      end;
      //MessageDlg(Form1.Caption, mtInformation, [mbOK], 0);
    except
      on e: Exception do
      begin
        msg := e.message;
        Form1.Caption := 'Операция завершена с ошибкой: "' + msg +
          '". Проверьте ' + ExtractFilePath(ParamStr(0)) + Form1.LogFile;
        AddLog(Form1.LogFile, Form1.Caption);
        MessageDlg(Form1.Caption, mtError, [mbOK], 0);
      end;
    end;
  finally
    Form1.SetComponentsEnabled(True);
    self.Terminate;
  end;
end;


procedure TForm1.ExecuteBaseAction(baseAction: TBaseAction; param: string);
var
  i: integer;
begin
  case baseAction of
    ba_update:
    begin
      for i := 0 to macro_list.Count - 1 do
      begin
        macro_list.ClearSelection;
        macro_list.Selected[i] := True;
        Caption := 'Установка обновления';
        AddLog(LogFile, Format('%s: "%s"', [Caption, macro_list.Items[i]]));
        if not updateBase(macro_list.Items[i], (i = macro_list.Count - 1)) then
          raise Exception.Create('Ошибка обновления!');
        Inc(currentTask);
      end;
      macro_list.ClearSelection;
    end;

    ba_dumpib:
    begin
      Caption := 'Выгрузка информационной базы';
      AddLog(LogFile, Caption);
      if not dumpIB() then
        raise Exception.Create(Caption + ' не выполнена!');
      Caption := Caption + ' завершена!';
      AddLog(LogFile, Caption);
      DeleteOld(IncludeTrailingBackslash(ExpandFileName(bakpath_edit.Text)), '*.dt', bakcount_edit.Value);
    end;

    ba_restoreib:
    begin
      Caption := 'Загрузка информационной базы';
      AddLog(LogFile, Format('%s "%s"', [Caption, param]));
      if (param = '') or (not restoreIB(param)) then
        raise Exception.Create(Caption + ' завершена с ошибкой!');
      Caption := Caption + ' успешно завершено!';
      AddLog(LogFile, Caption);
    end;

    ba_dumpcfg:
    begin
      Caption := 'Выгрузка конфигурации';
      AddLog(LogFile, Caption);
      if not dumpCFG() then
        raise Exception.Create('Кофигурация не выгружена!');
      Caption := 'Кофигурация успешно выгружена!';
      AddLog(LogFile, Caption);
      DeleteOld(IncludeTrailingBackslash(ExpandFileName(bakpath_edit.Text)), '*.cf', bakcount_edit.Value);
    end;

    ba_loadcfg:
    begin
      Caption := 'Загрузка конфигурации';
      AddLog(LogFile, Format('%s "%s"', [Caption, param]));
      if ExtractFileExt(param) = '.cfe' then
        Caption := 'Загрузка расширения';
      AddLog(LogFile, Format('%s "%s"', [Caption, param]));
      if (param = '') or (not loadCFG(param)) then
        raise Exception.Create('Ошибка ' + Caption + '!');
      Caption := Caption + ' успешно завершено!';
      AddLog(LogFile, Caption);
    end;

    ba_check:
    begin
      Caption := 'Тестирование и исправление базы';
      AddLog(LogFile, Caption);
      if not CheckAndRepair(param) then
        raise Exception.Create(Caption + ' завершено с ошибкой!');
      Caption := Caption + ' успешно завершено!';
      AddLog(LogFile, Caption);
    end;

    ba_enterprise:
    begin
      Caption := 'Запуск в режиме ENTERPRISE';
      AddLog(LogFile, Caption);
      if not runEnterprise() then
        raise Exception.Create(Caption + ' завершен с ошибкой!');
      Caption := Caption + ' успешно завершен!';
      AddLog(LogFile, Caption);
    end;

    ba_config:
    begin
      Caption := 'Запуск в режиме конфигуратора';
      AddLog(LogFile, Caption);
      if not runConfig() then
        raise Exception.Create(Caption + ' завершен с ошибкой!');
      Caption := Caption + ' успешно завершен!';
      AddLog(LogFile, Caption);
    end;
    ba_integrity:
    begin
      Caption := 'Восстановление структуры информационной базы';
      AddLog(LogFile, Caption);
      if not IBRestoreIntegrity() then
        raise Exception.Create(Caption + ' завершено с ошибкой!');
      Caption := Caption + ' успешно завершено!';
      AddLog(LogFile, Caption);
    end;
    ba_physical:
    begin
      Caption := 'Восстановление физической целостности';
      AddLog(LogFile, Caption);
      if not CheckPhysicalIntegrity() then
        raise Exception.Create(Caption + ' завершено с ошибкой!');
      Caption := Caption + ' успешно завершено!';
      AddLog(LogFile, Caption);
    end;
    ba_cache:
    begin
      Caption := 'Очистка кэша';
      AddLog(LogFile, Caption);
      if not ClearCache() then
        raise Exception.Create(Caption + ' завершена с ошибкой!');
      Caption := Caption + ' успешно завершена!';
      AddLog(LogFile, Caption);
    end;
    ba_convert:
    begin
      Caption := 'Конвертация файловой ИБ в новый формат';
      AddLog(LogFile, Caption);
      if not ConvertFileBase() then
        raise Exception.Create(Caption + ' завершена с ошибкой!');
      Caption := Caption + ' успешно завершена!';
      AddLog(LogFile, Caption);
    end;
    ba_journal:
    begin
      Caption := 'Сокращение журнала регистрации';
      if not param.IsEmpty then
      begin
        AddLog(LogFile, Format('%s "%s"', [Caption, param]));
        if not ReduceEventLogSize(param) then
          raise Exception.Create(Caption + ' завершено с ошибкой!');
        Caption := Caption + ' успешно завершено!';
        AddLog(LogFile, Caption);
        DeleteOld(IncludeTrailingBackslash(ExpandFileName(bakpath_edit.Text)), '*.lgd', bakcount_edit.Value);
      end;
    end;

  end;
end;

function TForm1.GetIBCheckParams(): string;
var
  i: integer;
  params: string;
begin
  Result := '';
  CheckAndRepairF.ShowModal;
  if CheckAndRepairF.ModalResult = mrOk then
  begin
    params := '';
    for i := 0 to CheckAndRepairF.CheckGroup1.Items.Count - 1 do
      if CheckAndRepairF.CheckGroup1.Checked[i] then
        params := params + IntToStr(i) + ',';
    Delete(params, length(params), 1);
    Result := params;
  end;
end;


procedure TForm1.SetComponentsEnabled(State: boolean);
begin
  bakpath_edit.Enabled := State;
  basepath_edit.Enabled := State;
  executable_edit.Enabled := State;
  user_edit.Enabled := State;
  pass_edit.Enabled := State;
  BitBtn1.Enabled := State;
  BitBtn2.Enabled := State;
  BitBtn3.Enabled := State;
  macro_list.Enabled := State;
  bakcount_edit.Enabled := State;
  pagesize_edit.Enabled := State;
end;

function TForm1.runEnterprise(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('ENTERPRISE');
    Add('/DisableStartupMessages');
    Add('/DisableSplash');
    Add('/F"' + UTF8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + UTF8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + UTF8ToWinCP(pass_edit.Text) + '"');
    Add('/Out "' + UTF8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.runConfig(): boolean;
begin

  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;

end;

function TForm1.IBRestoreIntegrity(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/IBRestoreIntegrity');
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.restoreIB(filename: string): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/RestoreIB"' + Utf8ToWinCP(filename) + '"');
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;


function TForm1.dumpIB(): boolean;
var
  fn: string;

begin
  fn := IncludeTrailingBackslash(ExpandFileName(bakpath_edit.Text)) + FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.dt';
  ForceDirectories(ExtractFileDir(fn));
  AddLog(LogFile, fn);

  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/DumpIB"' + Utf8ToWinCP(fn) + '"');
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.loadCFG(filename: string; updateCfg: boolean = True): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/LoadCfg"' + Utf8ToWinCP(filename) + '"');
    if ExtractFileExt(filename) = '.cfe' then
      Add('-Extension"' + Utf8ToWinCP(StringReplace(ExtractFileName(filename), '.cfe', '', [rfReplaceAll, rfIgnoreCase])) + '"');
    if updateCfg then
      Add('/UpdateDBCfg');
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.dumpCFG(): boolean;
var
  fn: string;
begin
  fn := IncludeTrailingBackslash(ExpandFileName(bakpath_edit.Text)) + FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.cf';
  AddLog(LogFile, fn);
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/DumpCfg"' + Utf8ToWinCP(fn) + '"');
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.ReduceEventLogSize(date: string): boolean;
var
  fn: string;
begin
  fn := IncludeTrailingBackslash(ExpandFileName(bakpath_edit.Text)) + date + '.lgd';
  AddLog(LogFile, 'Backup: "' + fn + '"');
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/ReduceEventLogSize ' + date);
    Add('-saveAs"' + Utf8ToWinCP(fn) + '"');
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;

  SomeProc.CurrentDirectory := ExtractFilePath(ParamStr(0));
  SomeProc.Executable := IncludeTrailingBackslash(SomeProc.CurrentDirectory) + 'sqlite3.exe';
  if FileExists(SomeProc.Executable) then
  begin
    SomeProc.Options := [poWaitOnExit];
    SomeProc.ShowWindow := swoHIDE;
    with SomeProc.Parameters do
    begin
      Clear();
      Add(Utf8ToWinCP(IncludeTrailingBackslash(basepath_edit.Text) + '1Cv8Log\1Cv8.lgd'));
      Add('vacuum');
    end;
    SomeProc.Execute;
  end
  else
    AddLog(LogFile, 'Отсутствует: "' + SomeProc.Executable + '"');

end;


function TForm1.CheckPhysicalIntegrity(): boolean;
begin
  SomeProc.CurrentDirectory := ExtractFilePath(executable_edit.Text);
  SomeProc.Executable := IncludeTrailingBackslash(SomeProc.CurrentDirectory) + 'chdbfl.exe';
  if FileExists(SomeProc.Executable) then
  begin
    SomeProc.Options := [poWaitOnExit];
    SomeProc.ShowWindow := swoShowNormal;
    SomeProc.Execute;
  end
  else
    AddLog(LogFile, 'Отсутствует: "' + SomeProc.Executable + '"');
  Result := SomeProc.ExitStatus = 0;
end;

function TForm1.ConvertFileBase(): boolean;
var
  base_file: string;
begin
  base_file := IncludeTrailingBackslash(basepath_edit.Text) + '1Cv8.1CD';
  SomeProc.CurrentDirectory := ExtractFilePath(executable_edit.Text);
  SomeProc.Executable := IncludeTrailingBackslash(SomeProc.CurrentDirectory) + 'cnvdbfl.exe';
  if FileExists(SomeProc.Executable) then
  begin
    SomeProc.Options := [poWaitOnExit];
    SomeProc.ShowWindow := swoHIDE;
    with SomeProc.Parameters do
    begin
      Clear();
      Add('--convert');
      Add('--format=8.3.8');
      Add('--page=' + IntToStr(pagesize_edit.Value) + 'k');
      Add(Utf8ToWinCP(base_file));
    end;
    SomeProc.Execute;
  end
  else
    AddLog(LogFile, 'Отсутствует: "' + SomeProc.Executable + '"');
  Result := SomeProc.ExitStatus = 0;
end;

function TForm1.CheckAndRepair(param: string): boolean;
var
  i: integer;
  params: TStringArray;
begin
  params := param.Split(',');
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/IBCheckAndRepair');
    for i := 0 to length(params) - 1 do
      case params[i] of
        '0': Add('-ReIndex');
        '1': Add('-LogIntegrity');
        '2': Add('-LogAndRefsIntegrity');
        '3': Add('-RecalcTotals');
        '4': Add('-IBCompression');
        '5': Add('-Rebuild');
      end;
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.updateBase(filename: string; updateCfg: boolean = False): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(basepath_edit.Text) + '"');
    Add('/N"' + Utf8ToWinCP(user_edit.Text) + '"');
    Add('/P"' + Utf8ToWinCP(pass_edit.Text) + '"');
    Add('/UpdateCfg"' + Utf8ToWinCP(filename) + '"');
    if updateCfg then
      Add('/UpdateDBCfg');
    Add('/Out "' + Utf8ToWinCP(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;


function TForm1.ClearCache(): boolean;
var
  paths, envs: tstringarray;
  i, j: integer;
  p: string;
begin
  Result := True;
  envs := TStringArray.Create(GetEnvironmentVariableUTF8('LocalAppData'), GetEnvironmentVariableUTF8('AppData'));
  paths := TStringArray.Create('1C\1cv8', '1C\1cv82');

  for i := 0 to length(envs) - 1 do
    for j := 0 to length(paths) - 1 do
    begin
      p := IncludeTrailingBackslash(envs[i]) + paths[j];
      AddLog(LogFile, Format('Удаление %s', [p]));
      if DirectoryExists(p) then
        DeleteDirectory(p, True);
    end;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := bakpath_edit.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    bakpath_edit.Text := SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm1.SaveSettings();
var
  i: integer;
begin
  ini.WriteString(SectionMain, KeyExecutable, executable_edit.Text);
  ini.WriteString(SectionBase, KeyBakcupPath, bakpath_edit.Text);
  ini.WriteString(SectionBase, KeyPath, basepath_edit.Text);
  ini.WriteString(SectionBase, KeyUser, user_edit.Text);
  ini.WriteString(SectionBase, KeyPass, EncodeStringBase64(pass_edit.Text));
  ini.WriteInteger(SectionBase, KeyBackupCount, bakcount_edit.Value);
  ini.WriteInteger(SectionBase, KeyPageSize, pagesize_edit.Value);
  ini.EraseSection(SectionUpdates);
  ini.EraseSection(SectionMacro);
  for i := 0 to macro_list.Count - 1 do
    ini.WriteString(SectionMacro, Format('%d_%d', [i, Ord(TBaseAction(PtrInt(macro_list.Items.Objects[i])))]),
      macro_list.Items[i]);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not run_non_interactive and (MessageDlg('Сохранить настройки?', mtConfirmation, mbYesNo, 0) = mrYes) then
  begin
    SaveSettings();
  end;
  ini.Free;
end;


procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Process1.Running then
    CanClose := run_non_interactive or (MessageDlg(
      'Идет работа с базой данных. Вы действительно хотите закрыть программу?',
      mtWarning, mbYesNo, 0) = mrYes)
  else
    CanClose := True;
end;

procedure TForm1.CustomExceptionHandler(Sender: TObject; E: Exception);
var
  msg: string;
begin
  msg := e.message;
  AddLog(LogFile, msg);
  MessageDlg('Непредвиденная ошибка: "' + msg + '" .Проверьте ' +
    ExtractFilePath(ParamStr(0)) + LogFile,
    mtError, [mbOK], 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  list: TStrings;
  i, k: integer;
  mi: TMenuItem;
begin
  Application.OnException := @CustomExceptionHandler;

  Process1.CurrentDirectory := ExtractFilePath(ParamStr(0));
  Process1.Options := [poWaitOnExit];
  iniPath := ChangeFileExt(ParamStr(0), '.ini');
  for i := 1 to Paramcount do
  begin
    if FileExists(ParamStr(i)) then
    begin
      iniPath := ParamStr(i);
      continue;
    end;
    run_non_interactive := run_non_interactive or (ParamStr(i) = '-r');
  end;
  StatusBar1.SimpleText := AnsiToUtf8(iniPath);
  ini := TIniFile.Create(iniPath);
  executable_edit.Text := ini.ReadString(SectionMain, KeyExecutable, '');
  LogFile := ini.ReadString(SectionMain, KeyLogFile, IncludeTrailingPathDelimiter('logs') +
    ChangeFileExt(ExtractFileName(iniPath), '.log'));
  bakpath_edit.Text := ini.ReadString(SectionBase, KeyBakcupPath, '');
  bakcount_edit.Value := ini.ReadInteger(SectionBase, KeyBackupCount, 3);
  pagesize_edit.Value := ini.ReadInteger(SectionBase, KeyPageSize, 8);
  basepath_edit.Text := ini.ReadString(SectionBase, KeyPath, '');
  user_edit.Text := ini.ReadString(SectionBase, KeyUser, '');
  try
    pass_edit.Text := DecodeStringBase64(ini.ReadString(SectionBase, KeyPass, ''));
  except
  end;
  list := TStringList.Create;

  try
    ini.ReadSectionValues(SectionMacro, list);
    for i := 0 to list.Count - 1 do
      macro_list.AddItem(list.ValueFromIndex[i], TObject(PtrInt(TBaseAction(StrToInt(list.Names[i].Split('_')[1])))));
  finally
    list.Free;
  end;
  for i := 0 to commands_menu.Count - 1 do
  begin
    mi := TMenuItem.Create(nil);
    mi.Caption := commands_menu.Items[i].Caption;
    mi.OnClick := commands_menu.Items[i].OnClick;
    mi.Tag := 1;
    PopupMenu2.Items[0].Add(mi);
    //for k:=0 to  commands_menu.Items[i].Count do
    //begin
    //  mi.Add();
    //mi.Caption:=commands_menu.Items[i].Caption;
    //mi.OnClick:=commands_menu.Items[i].OnClick;
    //PopupMenu2.Items[0].Add(mi);
    //end;
  end;
  AddLog(LogFile, ExtractFileName(ParamStr(0)) + ' started!');
  if run_non_interactive then
  begin
    Application.ShowMainForm := False;
    MyThread := TMyThread.Create(ba_macro);
  end;
end;

procedure TForm1.executable_editChange(Sender: TObject);
begin
  if not Process1.Running then
    Process1.Executable := executable_edit.Text;
end;



procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := basepath_edit.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    basepath_edit.Text := SelectDirectoryDialog1.FileName;
  end;
end;


procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(executable_edit.Text);
  OpenDialog1.FilterIndex := 2;
  if OpenDialog1.Execute then
  begin
    executable_edit.Text := OpenDialog1.FileName;
  end;
end;


procedure TForm1.mi_runmacroClick(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_macro);
end;

procedure TForm1.mi_del_from_macroClick(Sender: TObject);
var
  k: integer;
begin
  k := macro_list.ItemIndex;
  if k < 0 then
    exit;
  macro_list.Items.Delete(k);
end;

procedure TForm1.mi_del_allClick(Sender: TObject);
begin
  macro_list.Items.Clear;
end;




procedure TForm1.mi_delcacheClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_cache)
  else
    MyThread := TMyThread.Create(ba_cache);
end;

procedure TForm1.mi_reduce_journalClick(Sender: TObject);
begin
  CalendarDialog1.Date := Now();
  if CalendarDialog1.Execute then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_journal, FormatDateTime('yyyy-mm-dd', CalendarDialog1.Date))
    else
      MyThread := TMyThread.Create(ba_journal, FormatDateTime('yyyy-mm-dd', CalendarDialog1.Date));
end;



procedure TForm1.mi_repairibClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_integrity)
  else
    MyThread := TMyThread.Create(ba_integrity);
end;



procedure TForm1.mi_repair_physicalClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_physical)
  else
    MyThread := TMyThread.Create(ba_physical);
end;




procedure TForm1.mi_convertClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_convert)
  else
    MyThread := TMyThread.Create(ba_convert);
end;




procedure TForm1.mi_run_enterpriseClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_enterprise)
  else
    MyThread := TMyThread.Create(ba_enterprise);
end;

procedure TForm1.mi_run_designerClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_config)
  else
    MyThread := TMyThread.Create(ba_config);
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  SaveSettings();
end;

procedure TForm1.mi_updatecfgClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_update)
  else
    MyThread := TMyThread.Create(ba_update);
end;




procedure TForm1.mi_dumpibClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_dumpib)
  else
    MyThread := TMyThread.Create(ba_dumpib);
end;

procedure TForm1.mi_loadibClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := bakpath_edit.Text;
  OpenDialog1.FilterIndex := 4;
  if OpenDialog1.Execute then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_restoreib, OpenDialog1.FileName)
    else
      MyThread := TMyThread.Create(ba_restoreib, OpenDialog1.FileName);
end;

procedure TForm1.mi_dumpcfgClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_dumpcfg)
  else
    MyThread := TMyThread.Create(ba_dumpcfg);
end;

procedure TForm1.mi_loadcfgClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := bakpath_edit.Text;
  OpenDialog1.FilterIndex := 3;
  if OpenDialog1.Execute then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_loadcfg, OpenDialog1.FileName)
    else
      MyThread := TMyThread.Create(ba_loadcfg, OpenDialog1.FileName);
end;

procedure TForm1.mi_testcheckClick(Sender: TObject);
var
  params: string;
begin
  params := GetIBCheckParams();
  if not params.IsEmpty then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_check, params)
    else
      MyThread := TMyThread.Create(ba_check, params);
end;

function TForm1.AddMacrosCommand(Sender: TObject; baseAction: TBaseAction; param: string = ''): boolean;
var
  k: integer;
begin
  k := macro_list.ItemIndex;
  if param.IsEmpty() then
  begin
    if (k > -1) then
      macro_list.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(PtrUint(baseAction)))
    else
      macro_list.AddItem(TMenuItem(Sender).Caption, TObject(PtrUint(baseAction)));
  end
  else
  begin
    if (k > -1) then
      macro_list.Items.InsertObject(k, Format('%s(%s)', [TMenuItem(Sender).Caption, param]), TObject(PtrUint(baseAction)))
    else
      macro_list.AddItem(Format('%s(%s)', [TMenuItem(Sender).Caption, param]), TObject(PtrUint(baseAction)));
  end;
  Result := True;
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  MessageDlg('Программа для пакетного обновления типовых конфигураций!' +
    #10#13 + 'Version: ' + Version + #10#13 + 'Автор: Дмитрий Воротилин, dvor85@gmail.com',
    mtInformation, [mbOK], 0);
end;


























end.
