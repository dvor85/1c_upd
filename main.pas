unit main;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Menus, ComCtrls, ActnList, Spin, ExtDlgs, FileUtil, IniFiles,
  lazutf8, process, UTF8Process, lclintf, TypInfo, CheckAndRepairForm;

const
  Version: string = '2.3.7';
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

  TBaseAction = (ba_update, ba_updatecfg, ba_dumpib, ba_restoreib, ba_dumpcfg,
    ba_loadcfg, ba_check, ba_enterprise, ba_config, ba_cache, ba_journal,
    ba_integrity, ba_physical,
    ba_macro, ba_convert, ba_createcfg);

  TShowStatusEvent = procedure(Status: string) of object;

  TMyThread = class(TThread)
  private
    FOnShowStatus: TShowStatusEvent;
    FParam: string;
    FBaseAction: TBaseAction;
    FState: boolean;
    FMsg: string;
    procedure ShowStatus;
    procedure SetComponentsEnabled;
  protected
    procedure Execute; override;
  public
    constructor Create(BaseAction: TBaseAction; Param: string = ''); overload;
    property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
    procedure OnMyTerminate(Sender: TObject);
    procedure ExecuteBaseAction(baseAction: TBaseAction; param: string);
    function run_1c(mode: string; params: array of string): boolean;
    function create_1c(params: array of string): boolean;
    function ReduceEventLogSize(date: string): boolean;
    function CheckPhysicalIntegrity(): boolean;
    function ConvertFileBase(): boolean;
    function ClearCache(): boolean;
    procedure DeleteOld(dir, mask: string; needed: integer);
  end;

  { TForm1 }

  TForm1 = class(TForm)

    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
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
    MenuItem1: TMenuItem;
    mi_createcfg: TMenuItem;
    mi_update_cfg: TMenuItem;
    Process1: TProcessUTF8;
    SomeProc: TProcessUTF8;
    user_edit: TLabeledEdit;
    pass_edit: TLabeledEdit;
    macros_list: TListBox;
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
    macros_menu: TPopupMenu;
    logs_memo: TMemo;
    mi_dumpib: TMenuItem;
    mi_loadib: TMenuItem;
    MenuItem12: TMenuItem;
    mi_loadcfg: TMenuItem;
    mi_dumpcfg: TMenuItem;
    mi_update: TMenuItem;
    MenuItem16: TMenuItem;
    mi_testcheck: TMenuItem;
    MenuItem4: TMenuItem;
    mi_run_enterprise: TMenuItem;
    mi_run_designer: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;

    OpenDialog1: TOpenDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    bakcount_edit: TSpinEdit;
    pagesize_edit: TSpinEdit;
    Splitter1: TSplitter;

    procedure basepath_editChange(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure executable_editChange(Sender: TObject);
    procedure macros_listDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure macros_listDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
    procedure mi_createcfgClick(Sender: TObject);
    procedure mi_dumpibClick(Sender: TObject);
    procedure mi_loadibClick(Sender: TObject);
    procedure mi_loadcfgClick(Sender: TObject);
    procedure mi_dumpcfgClick(Sender: TObject);
    procedure mi_reload_configClick(Sender: TObject);
    procedure mi_updateClick(Sender: TObject);

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
    procedure mi_save_configClick(Sender: TObject);
    procedure mi_update_cfgClick(Sender: TObject);

  private
    { private declarations }
    MyThread: TMyThread;
    LogFile: string;
    iniPath: string;
    ini: TIniFile;
    progress: integer;
    procedure populateMacrosMenu(src, dst: TMenuItem);
    function GetIBCheckParams(): string;
    function GetBasePageSize(): integer;
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);


    function GetInitialDir(): string;


    function AddMacrosCommand(Sender: TObject; baseAction: TBaseAction; param: string = ''): boolean;
    procedure AddLog(msg: string);

  public
    { public declarations }
  end;


var
  Form1: TForm1;
  run_non_interactive: boolean = False;


implementation

uses base64;


{$R *.lfm}



function SortByCTime(List: TStringList; Index1, Index2: integer): integer;
begin
  Result := FileAge(List[index2]) - FileAge(List[index1]);
end;

procedure TMyThread.DeleteOld(dir, mask: string; needed: integer);
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
      begin
        FMsg := 'Удаление устаревшего: "' + flist[i] + '"';
        Synchronize(@ShowStatus);
      end;
    end;
  finally
    flist.Free;
  end;
end;


constructor TMyThread.Create(BaseAction: TBaseAction; Param: string = '');
begin
  inherited Create(False);
  FreeOnTerminate := True;
  OnTerminate := @OnMyTerminate;
  FBaseAction := BaseAction;
  FParam := Param;
end;

procedure TMyThread.SetComponentsEnabled;
begin
  with Form1 do
  begin
    bakpath_edit.Enabled := FState;
    basepath_edit.Enabled := FState;
    executable_edit.Enabled := FState;
    user_edit.Enabled := FState;
    pass_edit.Enabled := FState;
    BitBtn1.Enabled := FState;
    BitBtn2.Enabled := FState;
    BitBtn3.Enabled := FState;
    BitBtn4.Enabled := FState;
    macros_list.Enabled := FState;
    bakcount_edit.Enabled := FState;
    pagesize_edit.Enabled := FState;
  end;
end;



procedure TMyThread.ExecuteBaseAction(baseAction: TBaseAction; param: string);
var
  i, _action: integer;
  _param, fn, msg: string;
  _params: TStringArray;
begin
  if not Terminated then
  begin
    with Form1 do
    begin
      case baseAction of
        ba_macro:
        begin
          for i := 0 to macros_list.Count - 1 do
          begin
            macros_list.ClearSelection;
            macros_list.Selected[i] := True;
            _params := macros_list.Items[i].Split(['(', ')']);
            _param := '';
            if length(_params) > 1 then
              _param := _params[1];
            _action := PtrInt(macros_list.Items.Objects[i]);
            ExecuteBaseAction(TbaseAction(_action), _param);
          end;
          fmsg := 'Выполнение макроса успешно завершено!';
          Synchronize(@showstatus);
        end;

        ba_update:
        begin
          msg := 'Установка обновления';
          fmsg := Format('%s: "%s"', [msg, param]);
          Synchronize(@showstatus);
          if (param = '') or (not run_1c('DESIGNER', ['/UpdateCfg', param])) then
            raise Exception.Create('Ошибка обновления!');
          macros_list.ClearSelection;
        end;

        ba_updatecfg:
        begin
          fmsg := 'Обновление конфигурации';
          Synchronize(@showstatus);
          if not run_1c('DESIGNER', ['/UpdateDBCfg']) then
            raise Exception.Create(fmsg + ' не выполнена!');
          fmsg := fmsg + ' успешно завершено!';
          Synchronize(@showstatus);
        end;

        ba_dumpib:
        begin
          msg := 'Выгрузка информационной базы';
          fn := IncludeTrailingPathDelimiter(ExpandFileName(bakpath_edit.Text)) +
            FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.dt';
          fmsg := Format('%s: "%s"', [msg, fn]);
          Synchronize(@showstatus);
          ForceDirectories(ExtractFileDir(fn));
          if not run_1c('DESIGNER', ['/DumpIB', fn]) then
            raise Exception.Create(msg + ' не выполнена!');
          fmsg := msg + ' завершена!';
          Synchronize(@showstatus);
          DeleteOld(IncludeTrailingPathDelimiter(ExpandFileName(bakpath_edit.Text)),
            '*.dt', bakcount_edit.Value);
        end;

        ba_restoreib:
        begin
          msg := 'Загрузка информационной базы';
          fmsg := Format('%s "%s"', [msg, param]);
          Synchronize(@showstatus);
          if (param = '') or (not run_1c('DESIGNER', ['/RestoreIB', param])) then
            raise Exception.Create(msg + ' завершена с ошибкой!');
          fmsg := msg + ' успешно завершено!';
          Synchronize(@showstatus);
        end;

        ba_dumpcfg:
        begin
          msg := 'Выгрузка конфигурации';
          fn := IncludeTrailingPathDelimiter(ExpandFileName(bakpath_edit.Text)) +
            FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.cf';
          fmsg := Format('%s: "%s"', [msg, fn]);
          Synchronize(@showstatus);
          ForceDirectories(ExtractFileDir(fn));
          if not run_1c('DESIGNER', ['/DumpCFG', fn]) then
            raise Exception.Create('Кофигурация не выгружена!');
          fmsg := 'Кофигурация успешно выгружена!';
          Synchronize(@showstatus);
          DeleteOld(IncludeTrailingPathDelimiter(ExpandFileName(bakpath_edit.Text)),
            '*.cf', bakcount_edit.Value);
        end;

        ba_loadcfg:
        begin
          msg := 'Загрузка конфигурации';
          fmsg := Format('%s "%s"', [msg, param]);
          Synchronize(@showstatus);
          setlength(_params, 0);
          SetLength(_params, 3);
          _params[0] := '/LoadCFG';
          _params[1] := param;
          if ExtractFileExt(param) = '.cfe' then
          begin
            msg := 'Загрузка расширения';
            _params[2] := '-Extension"' + StringReplace(ExtractFileName(param), '.cfe',
              '', [rfReplaceAll, rfIgnoreCase]) + '"';
          end;
          if (param = '') or (not run_1c('DESIGNER', _params)) then
            raise Exception.Create('Ошибка ' + msg + '!');
          fmsg := msg + ' успешно завершено!';
          Synchronize(@showstatus);
        end;

        ba_createcfg:
        begin
          msg := 'Создание конфигурации';
          fmsg := Format('%s из "%s"', [msg, param]);
          Synchronize(@showstatus);
          if not create_1c(['/UseTemplate', param]) then
            raise Exception.Create('Ошибка ' + msg + '!');
          fmsg := msg + ' успешно завершено!';
          Synchronize(@showstatus);
        end;

        ba_check:
        begin
          fmsg := 'Тестирование и исправление базы';
          Synchronize(@showstatus);
          _params := param.Split([',']);
          for i := 0 to length(_params) - 1 do
            case _params[i] of
              '0': _params[i] := '-ReIndex';
              '1': _params[i] := '-LogIntegrity';
              '2': _params[i] := '-LogAndRefsIntegrity';
              '3': _params[i] := '-RecalcTotals';
              '4': _params[i] := '-IBCompression';
              '5': _params[i] := '-Rebuild';
            end;
          setlength(_params, length(_params) + 1);
          _params[length(_params) - 1] := _params[0];
          _params[0] := '/IBCheckAndRepair';

          if not run_1c('DESIGNER', _params) then
            raise Exception.Create(fmsg + ' завершено с ошибкой!');
          fmsg := fmsg + ' успешно завершено!';
          Synchronize(@showstatus);
        end;

        ba_enterprise:
        begin
          fmsg := 'Запуск в режиме ENTERPRISE';
          Synchronize(@showstatus);
          if not run_1c('ENTERPRISE', []) then
            raise Exception.Create(fmsg + ' завершен с ошибкой!');
          fmsg := fmsg + ' успешно завершен!';
          Synchronize(@showstatus);
        end;

        ba_config:
        begin
          fmsg := 'Запуск в режиме конфигуратора';
          Synchronize(@showstatus);
          if not run_1c('DESIGNER', []) then
            raise Exception.Create(fmsg + ' завершен с ошибкой!');
          fmsg := fmsg + ' успешно завершен!';
          Synchronize(@showstatus);
        end;

        ba_integrity:
        begin
          fmsg :=
            'Восстановление структуры информационной базы';
          Synchronize(@showstatus);
          if not run_1c('DESIGNER', ['/IBRestoreIntegrity']) then
            raise Exception.Create(fmsg + ' завершено с ошибкой!');
          fmsg := fmsg + ' успешно завершено!';
          Synchronize(@showstatus);
        end;

        ba_physical:
        begin
          fmsg := 'Восстановление физической целостности';
          Synchronize(@showstatus);
          if not CheckPhysicalIntegrity() then
            raise Exception.Create(fmsg + ' завершено с ошибкой!');
          fmsg := fmsg + ' успешно завершено!';
          Synchronize(@showstatus);
        end;

        ba_cache:
        begin
          fmsg := 'Очистка кэша';
          Synchronize(@showstatus);
          if not ClearCache() then
            raise Exception.Create(fmsg + ' завершена с ошибкой!');
          fmsg := fmsg + ' успешно завершена!';
          Synchronize(@showstatus);
        end;

        ba_convert:
        begin
          fmsg := 'Конвертация файловой ИБ в новый формат';
          Synchronize(@showstatus);
          if not ConvertFileBase() then
            raise Exception.Create(fmsg + ' завершена с ошибкой!');
          fmsg := fmsg + ' успешно завершена!';
          Synchronize(@showstatus);
        end;

        ba_journal:
        begin
          msg := 'Сокращение журнала регистрации';
          if not param.IsEmpty then
          begin
            fmsg := Format('%s "%s"', [msg, param]);
            Synchronize(@showstatus);
            if not ReduceEventLogSize(param) then
              raise Exception.Create(msg + ' завершено с ошибкой!');
            fmsg := msg + ' успешно завершено!';
            Synchronize(@showstatus);
            DeleteOld(IncludeTrailingPathDelimiter(ExpandFileName(bakpath_edit.Text)),
              '*.lgd', bakcount_edit.Value);
          end;
        end;
      end;
    end;
  end;
end;

procedure TMyThread.OnMyTerminate(Sender: TObject);
begin
  if run_non_interactive then
    Form1.Close();
end;

procedure TMyThread.ShowStatus;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  if Assigned(FOnShowStatus) then
  begin
    FOnShowStatus(fmsg);
  end;
end;


procedure TMyThread.Execute;
begin
  FState := False;
  Synchronize(@SetComponentsEnabled);
  Form1.progress := 0;
  try
    try
      with Form1 do
      begin
        if Process1.Running then
        begin
          fmsg :=
            'Процесс уже запущен, дождитесь окончания!';
          Synchronize(@showstatus);
          exit;
        end;

        if (Process1.Executable = '') or (basepath_edit.Text = '') then
          raise Exception.Create('Не заполнены необходимые поля!');

        ExecuteBaseAction(FBaseAction, FParam);
        FState := True;
      end;
    except
      on e: Exception do
      begin
        FState := True;
        Fmsg := 'Операция завершена с ошибкой: "' + e.message +
          '". Проверьте ' + Form1.LogFile;
        Synchronize(@showstatus);
      end;
    end;
  finally
    Synchronize(@SetComponentsEnabled);
    self.Terminate;
  end;
end;


{ TForm1 }

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


procedure TForm1.AddLog(msg: string);
var
  F: TFileStream;
  PStr: PChar;
  Str: string;
  LengthLogString: cardinal;
begin
  Str := DateTimeToStr(Now()) + ': ' + msg + #13#10;
  LengthLogString := Length(Str);
  try
    if FileExists(LogFile) then
      F := TFileStream.Create(LogFile, fmOpenWrite)
    else
    begin
      ForceDirectories(ExtractFileDir(LogFile));
      F := TFileStream.Create(LogFile, fmCreate);
    end;
  except
    MessageDlg(Caption, msg, mtError, [mbYes], 0);
    Exit;
  end;
  PStr := StrAlloc(LengthLogString + 1);
  try
    try
      StrPCopy(PStr, Str);
      F.Position := F.Size;
      F.Write(PStr^, LengthLogString);
      Form1.logs_memo.Lines.AddText(Str);
      Form1.logs_memo.SelStart := MaxInt;
    except
      MessageDlg(Form1.Caption, msg, mtError, [mbYes], 0);
      Exit;
    end;
  finally
    StrDispose(PStr);
    F.Free;
  end;
end;




function TMyThread.create_1c(params: array of string): boolean;
var
  fn: string;
begin
  fn := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'logs' + PathDelim + 'test.out';
  with Form1 do
  begin
    with Process1.Parameters do
    begin
      Clear();
      Add('CREATEINFOBASE');
      Add(Format('File="%s";DBFormat=%s;DBPageSize=%s', [basepath_edit.Text, '8.3.8',
        IntToStr(pagesize_edit.Value) + 'k']));
      Add('/DumpResult');
      Add(fn);
      AddStrings(params);
    end;

    Process1.Execute;

  end;
  Result := pos('0', ReadFileToString(fn)) > -1;
  DeleteFile(fn);
end;

function TMyThread.run_1c(mode: string; params: array of string): boolean;
begin
  with Form1 do
  begin
    with Process1.Parameters do
    begin
      Clear();
      Add(mode);
      Add('/DisableStartupMessages');
      Add('/DisableStartupDialogs');
      Add('/DisableSplash');
      AddStrings(params);
      if string(basepath_edit.Text).StartsWith('http', True) then
        Add('/WS"' + basepath_edit.Text + '"')
      else
        Add('/F"' + basepath_edit.Text + '"');
      Add('/N"' + user_edit.Text + '"');
      Add('/P"' + pass_edit.Text + '"');
      Add('/Out"' + LogFile + '" -NoTruncate');
    end;
    Process1.Execute;
    Result := Process1.ExitStatus = 0;

  end;
end;


function TMyThread.ReduceEventLogSize(date: string): boolean;
var
  fn: string;
begin
  with Form1 do
  begin
    Result := True;
    fn := IncludeTrailingPathDelimiter(ExpandFileName(bakpath_edit.Text)) + date + '.lgd';
    AddLog('Backup: "' + fn + '"');
    run_1c('DESIGNER', ['/ReduceEventLogSize ' + date, '-saveAs"' + fn + '"']);

    SomeProc.CurrentDirectory := ExtractFilePath(ParamStr(0));
  {$IFDEF MSWINDOWS}
    SomeProc.Executable := FindFilenameOfCmd('sqlite3.exe');
  {$ENDIF}
  {$IFDEF UNIX}
    SomeProc.Executable := FindFilenameOfCmd('sqlite3');
  {$ENDIF}
    if FileExists(SomeProc.Executable) then
    begin
      SomeProc.Options := [poWaitOnExit];
      SomeProc.ShowWindow := swoHIDE;
      with SomeProc.Parameters do
      begin
        Clear();
        Add(IncludeTrailingPathDelimiter(basepath_edit.Text) + '1Cv8Log' + PathDelim + '1Cv8.lgd');
        Add('vacuum');
      end;
      SomeProc.Execute;
    end
    else
    begin
      fmsg := 'Отсутствует: "' + SomeProc.Executable + '"';
      Synchronize(@showstatus);

    end;

  end;
end;


function TMyThread.CheckPhysicalIntegrity(): boolean;
begin
  with Form1 do
  begin
    SomeProc.CurrentDirectory := ExtractFilePath(executable_edit.Text);
  {$IFDEF MSWINDOWS}
    SomeProc.Executable := IncludeTrailingPathDelimiter(SomeProc.CurrentDirectory) + 'chdbfl.exe';
  {$ENDIF}
  {$IFDEF UNIX}
    SomeProc.Executable := IncludeTrailingPathDelimiter(SomeProc.CurrentDirectory) + 'chdbfl';
  {$ENDIF}
    if FileExists(SomeProc.Executable) then
    begin
      SomeProc.Options := [poWaitOnExit];
      SomeProc.ShowWindow := swoShowNormal;
      SomeProc.Execute;
    end
    else
    begin
      fmsg := 'Отсутствует: "' + SomeProc.Executable + '"';
      Synchronize(@showstatus);
    end;
    Result := SomeProc.ExitStatus = 0;

  end;
end;

function TMyThread.ConvertFileBase(): boolean;
var
  base_file: string;
begin
  with Form1 do
  begin
    base_file := IncludeTrailingPathDelimiter(basepath_edit.Text) + '1Cv8.1CD';
    SomeProc.CurrentDirectory := ExtractFilePath(executable_edit.Text);
  {$IFDEF MSWINDOWS}
    SomeProc.Executable := IncludeTrailingPathDelimiter(SomeProc.CurrentDirectory) + 'cnvdbfl.exe';
  {$ENDIF}
  {$IFDEF UNIX}
    SomeProc.Executable := IncludeTrailingPathDelimiter(SomeProc.CurrentDirectory) + 'cnvdbfl';
  {$ENDIF}

    if FileExists(SomeProc.Executable) and FileExists(base_file) then
    begin
      SomeProc.Options := [poWaitOnExit];
      SomeProc.ShowWindow := swoHIDE;
      with SomeProc.Parameters do
      begin
        Clear();
        Add('--convert');
        Add('--format=8.3.8');
        Add('--page=' + IntToStr(pagesize_edit.Value) + 'k');
        Add(base_file);
      end;
      SomeProc.Execute;
    end
    else
    begin
      fmsg := 'Отсутствует: "' + SomeProc.Executable + '"';
      Synchronize(@showstatus);
    end;
    Result := SomeProc.ExitStatus = 0;

  end;
end;


function TMyThread.ClearCache(): boolean;
var
  paths, envs: tstringarray;
  i, j: integer;
  p: string;
begin
  Result := True;
  {$IFDEF MSWINDOWS}
  envs := TStringArray.Create(GetEnvironmentVariableUTF8('LocalAppData'), GetEnvironmentVariableUTF8('AppData'));
  paths := TStringArray.Create('1C' + PathDelim + '1cv8', '1C' + PathDelim + '1cv82');
  {$ENDIF}
  {$IFDEF UNIX}
  envs := TStringArray.Create(GetEnvironmentVariableUTF8('home'));
  paths := TStringArray.Create('.1cv8' + PathDelim + '1C', '.1cv82' + PathDelim + '1C');
  {$ENDIF}

  for i := 0 to length(envs) - 1 do
    for j := 0 to length(paths) - 1 do
    begin
      p := IncludeTrailingPathDelimiter(envs[i]) + paths[j];
      fmsg := Format('Удаление %s', [p]);
      Synchronize(@showstatus);
      if DirectoryExists(p) then
        DeleteDirectory(p, True);
    end;
end;

function TForm1.GetBasePageSize(): integer;
var
  base_file:string;
  aout: tstringlist;
begin
  result:=8;
  base_file := IncludeTrailingPathDelimiter(basepath_edit.Text) + '1Cv8.1CD';
  SomeProc.CurrentDirectory := ExtractFilePath(executable_edit.Text);
  {$IFDEF MSWINDOWS}
  SomeProc.Executable := IncludeTrailingPathDelimiter(SomeProc.CurrentDirectory) + 'cnvdbfl.exe';
  {$ENDIF}
  {$IFDEF UNIX}
  SomeProc.Executable := IncludeTrailingPathDelimiter(SomeProc.CurrentDirectory) + 'cnvdbfl';
  {$ENDIF}
  if FileExists(SomeProc.Executable) and FileExists(base_file) then
  begin
    SomeProc.Options := [poWaitOnExit, poUsePipes,  poStdErrToOutPut];
    SomeProc.ShowWindow := swoHIDE;
    with SomeProc.Parameters do
    begin
      Clear();
      Add('--info');
      Add(base_file);
    end;
    SomeProc.Execute;
    aout := tstringlist.Create;
    try
       aout.LoadFromStream(SomeProc.Output);
       result:=aout[1].Remove(0, aout[1].IndexOf(':')+2).ToInteger div 1024;
    finally
       aout.free();
    end;
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

procedure TForm1.BitBtn4Click(Sender: TObject);
var
  paths, envs: tstringarray;
  flist: TStringList;
  i, j, k: integer;
  max_ver, max_path, dir_name, p: string;
  base_file :string;
  aout:tstringlist;
begin
  {$IFDEF MSWINDOWS}
  envs := TStringArray.Create(IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('LocalAppData')) +
    'Programs' + PathDelim, IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('PROGRAMFILES')),
    IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('PROGRAMFILES(x86)')),
    IncludeTrailingPathDelimiter(GetEnvironmentVariableUTF8('PROGRAMW6432')));

  paths := TStringArray.Create('1cv8' + PathDelim, '1cv8_x86' + pathDelim, '1cv8_x64' + PathDelim);
  {$ENDIF}
  {$IFDEF UNIX}
  envs := TStringArray.Create('/opt/1cv8/');
  paths := TStringArray.Create('x86_64/');
  {$ENDIF}
  max_ver := '';
  for i := 0 to length(envs) - 1 do
    for j := 0 to length(paths) - 1 do
    begin
      p := envs[i] + paths[j];
      logs_memo.Append(Format('Поиск в: %s', [p]));
      flist := FindAllDirectories(p, False);
      try
        for k := 0 to flist.Count - 1 do
        begin
          dir_name := ExtractFileName(flist[k]);
          if dir_name.StartsWith('8') and (dir_name > max_ver) then
          begin
            logs_memo.Append(Format('Найдена версия: %s', [dir_name]));
            max_ver := dir_name;
            max_path := IncludeTrailingPathDelimiter(flist[k]);
          end;
        end;
      finally
        flist.Free;
      end;
    end;
  if not max_ver.IsEmpty then
  begin
    {$IFDEF MSWINDOWS}
    executable_edit.Text := max_path + 'bin' + PathDelim + '1cv8.exe';
    {$ENDIF}
    {$IFDEF UNIX}
    executable_edit.Text := max_path + '1cv8';
    {$ENDIF}
  end;
end;



procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //if not run_non_interactive and (MessageDlg('Сохранить настройки?', mtConfirmation, mbYesNo, 0) = mrYes) then
  //begin
  //  SaveSettings();
  //end;
  //ini.Free;
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
  AddLog(msg);
  MessageDlg('Непредвиденная ошибка: "' + msg + '" .Проверьте ' +
    ExtractFilePath(ParamStr(0)) + LogFile,
    mtError, [mbOK], 0);
end;


procedure TForm1.populateMacrosMenu(src, dst: TMenuItem);
var
  _mi, mi: TMenuItem;
begin
  for mi in src do
  begin
    _mi := TMenuItem.Create(nil);
    _mi.Caption := mi.Caption;
    _mi.OnClick := mi.OnClick;
    _mi.Tag := 1;
    if mi.Count > 0 then
      populateMacrosMenu(mi, _mi);
    dst.add(_mi);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;

begin

  Application.OnException := @CustomExceptionHandler;


  Process1.CurrentDirectory := ExtractFilePath(ParamStr(0));
  Process1.Options := [poWaitOnExit];
  iniPath := ChangeFileExt(ParamStr(0), '.ini');
  for i := 1 to Paramcount do
  begin
    if FileExists(ParamStr(i)) or (ExtractFileExt(ParamStr(i)) = '.ini') then
    begin
      iniPath := ParamStr(i);
      continue;
    end;
    run_non_interactive := run_non_interactive or (ParamStr(i) = '-r');
  end;
  Caption := Format('%s: "%s"', ['upd_1c', iniPath]);
  ini := TIniFile.Create(iniPath);
  LogFile := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'logs' + PathDelim +
    ChangeFileExt(ExtractFileName(iniPath), '.log');
  mi_reload_config.Click();
  populateMacrosMenu(commands_menu, macros_menu.Items[0]);
  AddLog(ExtractFileName(ParamStr(0)) + ' started!');

  if run_non_interactive then
  begin
    Application.ShowMainForm := False;
    MyThread := TMyThread.Create(ba_macro);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;

  end;
end;

procedure TForm1.executable_editChange(Sender: TObject);
begin
  if not Process1.Running then
    Process1.Executable := executable_edit.Text;
  pagesize_edit.value:=GetBasePageSize();
end;

procedure TForm1.macros_listDragDrop(Sender, Source: TObject; X, Y: integer);
var
  k: integer;
begin
  with (Sender as TListBox) do
  begin
    k := ItemAtPos(Point(x, y), False);
    if k < 0 then
      k := Count - 1;
    Items.Move(ItemIndex, k);
  end;
end;

procedure TForm1.macros_listDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
begin
  Accept := (Sender = Source);
end;

procedure TForm1.mi_createcfgClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := bakpath_edit.Text;
  OpenDialog1.FilterIndex := 6;
  if OpenDialog1.Execute then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_createcfg, OpenDialog1.FileName)
    else
    begin
      MyThread := TMyThread.Create(ba_createcfg, OpenDialog1.FileName);
      MyThread.OnShowStatus := @AddLog;
      MyThread.Start;
    end;

end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := basepath_edit.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    basepath_edit.Text := SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm1.basepath_editChange(Sender: TObject);
begin
  pagesize_edit.Value:=GetBasePageSize();
end;


procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(executable_edit.Text);
  OpenDialog1.FilterIndex := 2;
  {$IFDEF UNIX}
  OpenDialog1.FilterIndex := 5;
  {$ENDIF}
  if OpenDialog1.Execute then
  begin
    executable_edit.Text := OpenDialog1.FileName;
  end;
end;


procedure TForm1.mi_runmacroClick(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_macro);
  MyThread.OnShowStatus := @AddLog;
  MyThread.Start;
end;

procedure TForm1.mi_del_from_macroClick(Sender: TObject);
var
  k: integer;
begin
  k := macros_list.ItemIndex;
  if k < 0 then
    exit;
  macros_list.Items.Delete(k);
end;

procedure TForm1.mi_del_allClick(Sender: TObject);
begin
  macros_list.Items.Clear;
end;




procedure TForm1.mi_delcacheClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_cache)
  else
  begin
    MyThread := TMyThread.Create(ba_cache);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;

end;

procedure TForm1.mi_reduce_journalClick(Sender: TObject);
begin
  CalendarDialog1.Date := Now();
  if CalendarDialog1.Execute then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_journal, FormatDateTime('yyyy-mm-dd', CalendarDialog1.Date))
    else
    begin
      MyThread := TMyThread.Create(ba_journal, FormatDateTime('yyyy-mm-dd', CalendarDialog1.Date));
      MyThread.OnShowStatus := @AddLog;
      MyThread.Start;
    end;
end;



procedure TForm1.mi_repairibClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_integrity)
  else
  begin
    MyThread := TMyThread.Create(ba_integrity);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;
end;



procedure TForm1.mi_repair_physicalClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_physical)
  else
  begin
    MyThread := TMyThread.Create(ba_physical);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;
end;




procedure TForm1.mi_convertClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_convert)
  else
  begin
    MyThread := TMyThread.Create(ba_convert);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;
end;




procedure TForm1.mi_run_enterpriseClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_enterprise)
  else
  begin
    MyThread := TMyThread.Create(ba_enterprise);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;
end;

procedure TForm1.mi_run_designerClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_config)
  else
  begin
    MyThread := TMyThread.Create(ba_config);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;
end;

function TForm1.GetInitialDir(): string;
var
  i, _action: integer;
  _params: TStringArray;
begin
  Result := '';
  for i := macros_list.Count - 1 downto 0 do
  begin
    _action := PtrInt(macros_list.Items.Objects[i]);
    if TbaseAction(_action) = ba_update then
    begin
      _params := macros_list.Items[i].Split(['(', ')']);
      if length(_params) > 1 then
      begin
        Result := ExtractFileDir(_params[1]);
        Exit;
      end;
    end;
  end;
end;

procedure TForm1.mi_updateClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := GetInitialDir();
  OpenDialog1.FilterIndex := 1;
  if OpenDialog1.Execute then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_update, OpenDialog1.FileName)
    else
    begin
      MyThread := TMyThread.Create(ba_update, OpenDialog1.FileName);
      MyThread.OnShowStatus := @AddLog;
      MyThread.Start;
    end;
end;

procedure TForm1.mi_update_cfgClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_updatecfg)
  else
  begin
    MyThread := TMyThread.Create(ba_updatecfg);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;
end;




procedure TForm1.mi_dumpibClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_dumpib)
  else
  begin
    MyThread := TMyThread.Create(ba_dumpib);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;

end;

procedure TForm1.mi_loadibClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := bakpath_edit.Text;
  OpenDialog1.FilterIndex := 4;
  if OpenDialog1.Execute then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_restoreib, OpenDialog1.FileName)
    else
    begin
      MyThread := TMyThread.Create(ba_restoreib, OpenDialog1.FileName);
      MyThread.OnShowStatus := @AddLog;
      MyThread.Start;
    end;
end;

procedure TForm1.mi_dumpcfgClick(Sender: TObject);
begin
  if (Sender as TMenuItem).Tag = 1 then
    AddMacrosCommand(Sender, ba_dumpcfg)
  else
  begin
    MyThread := TMyThread.Create(ba_dumpcfg);
    MyThread.OnShowStatus := @AddLog;
    MyThread.Start;
  end;
end;

procedure TForm1.mi_reload_configClick(Sender: TObject);
var
  i: integer;
  list: TStrings;
begin
  executable_edit.Text := ini.ReadString(SectionMain, KeyExecutable, '');
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
    macros_list.Clear;
    for i := 0 to list.Count - 1 do
      macros_list.AddItem(list.ValueFromIndex[i],
        TObject(PtrInt(TBaseAction(StrToInt(list.Names[i].Split('_')[1])))));
  finally
    list.Free;
  end;
  AddLog('Настройки загружены');
end;

procedure TForm1.mi_save_configClick(Sender: TObject);
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
  for i := 0 to macros_list.Count - 1 do
    ini.WriteString(SectionMacro, Format('%d_%d', [i, Ord(TBaseAction(PtrInt(macros_list.Items.Objects[i])))]),
      macros_list.Items[i]);
  AddLog('Настройки сохранены');
end;



procedure TForm1.mi_loadcfgClick(Sender: TObject);
begin
  OpenDialog1.InitialDir := bakpath_edit.Text;
  OpenDialog1.FilterIndex := 3;
  if OpenDialog1.Execute then
    if (Sender as TMenuItem).Tag = 1 then
      AddMacrosCommand(Sender, ba_loadcfg, OpenDialog1.FileName)
    else
    begin
      MyThread := TMyThread.Create(ba_loadcfg, OpenDialog1.FileName);
      MyThread.OnShowStatus := @AddLog;
      MyThread.Start;
    end;
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
    begin
      MyThread := TMyThread.Create(ba_check, params);
      MyThread.OnShowStatus := @AddLog;
      MyThread.Start;
    end;
end;

function TForm1.AddMacrosCommand(Sender: TObject; baseAction: TBaseAction; param: string = ''): boolean;
var
  k: integer;
begin
  k := macros_list.ItemIndex;
  if param.IsEmpty() then
  begin
    if (k > -1) then
      macros_list.Items.InsertObject(k, TMenuItem(Sender).Caption,
        TObject(PtrInt(baseAction)))
    else
      macros_list.AddItem(TMenuItem(Sender).Caption, TObject(PtrInt(baseAction)));
  end
  else
  begin
    if (k > -1) then
      macros_list.Items.InsertObject(k, Format('%s(%s)', [TMenuItem(Sender).Caption, param]), TObject(PtrInt(baseAction)))
    else
      macros_list.AddItem(Format('%s(%s)', [TMenuItem(Sender).Caption, param]),
        TObject(PtrInt(baseAction)));
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
