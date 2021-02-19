unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Menus, ComCtrls, ActnList, Spin, ExtDlgs, FileUtil, IniFiles,
  Windows, lazutf8, TypInfo, CheckAndRepairForm;

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
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    ListBox2: TListBox;
    MainMenu1: TMainMenu;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    PopupMenu2: TPopupMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Process1: TProcess;
    SomeProc: TProcess;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StatusBar1: TStatusBar;

    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabeledEdit3Change(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem21Click(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MenuItem25Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem28Click(Sender: TObject);
    procedure MenuItem29Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem30Click(Sender: TObject);
    procedure MenuItem31Click(Sender: TObject);
    procedure MenuItem32Click(Sender: TObject);
    procedure MenuItem33Click(Sender: TObject);
    procedure MenuItem34Click(Sender: TObject);
    procedure MenuItem35Click(Sender: TObject);
    procedure MenuItem36Click(Sender: TObject);
    procedure MenuItem37Click(Sender: TObject);
    procedure MenuItem38Click(Sender: TObject);
    procedure MenuItem39Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
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
      Form1.Memo1.Lines.AddText(Str);
      SendMessage(Form1.Memo1.Handle, EM_LINESCROLL, 0, Form1.Memo1.Lines.Count);
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

        if (Process1.Executable = '') or (LabeledEdit2.Text = '') then
          raise Exception.Create('Не заполнены необходимые поля!');
        if (FBaseAction = ba_macro) then
        begin
          currentTask := 0;
          totalTasks := ListBox1.Count + ListBox2.Count;
          if (ListBox2.Count < 1) then
            raise Exception.Create('Действия для макроса не заполнены!');

          for i := 0 to ListBox2.Count - 1 do
          begin
            params := ListBox2.Items[i].Split(['(', ')']);
            param := '';
            if length(params) > 1 then
              param := params[1];
            listbox2.ClearSelection;
            ListBox2.Selected[i] := True;
            ExecuteBaseAction(TbaseAction(ListBox2.Items.Objects[i]), param);
            Inc(currentTask);
            progress := currentTask * 100 div totalTasks;
          end;
          listbox2.ClearSelection;
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
      for i := 0 to ListBox1.Count - 1 do
      begin
        ListBox1.ClearSelection;
        ListBox1.Selected[i] := True;
        Caption := 'Установка обновления';
        AddLog(LogFile, Format('%s: "%s"', [Caption, ListBox1.Items[i]]));
        if not updateBase(ListBox1.Items[i], (i = ListBox1.Count - 1)) then
          raise Exception.Create('Ошибка обновления!');
        Inc(currentTask);
      end;
      ListBox1.ClearSelection;
    end;

    ba_dumpib:
    begin
      Caption := 'Выгрузка информационной базы';
      AddLog(LogFile, Caption);
      if not dumpIB() then
        raise Exception.Create(Caption + ' не выполнена!');
      Caption := Caption + ' завершена!';
      AddLog(LogFile, Caption);
      DeleteOld(IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)), '*.dt', SpinEdit1.Value);
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
      DeleteOld(IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)), '*.cf', SpinEdit1.Value);
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
        DeleteOld(IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)), '*.lgd', SpinEdit1.Value);
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
  LabeledEdit1.Enabled := State;
  LabeledEdit2.Enabled := State;
  LabeledEdit3.Enabled := State;
  LabeledEdit4.Enabled := State;
  LabeledEdit5.Enabled := State;
  BitBtn1.Enabled := State;
  BitBtn2.Enabled := State;
  BitBtn3.Enabled := State;
  ListBox1.Enabled := State;
  ListBox2.Enabled := State;
  SpinEdit1.Enabled := State;
  SpinEdit2.Enabled := State;
end;

function TForm1.runEnterprise(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('ENTERPRISE');
    Add('/DisableStartupMessages');
    Add('/DisableSplash');
    Add('/F"' + UTF8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + UTF8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + UTF8ToWinCP(LabeledEdit5.Text) + '"');
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
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
  fn := IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)) + FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.dt';
  ForceDirectories(ExtractFileDir(fn));
  AddLog(LogFile, fn);

  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
  fn := IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)) + FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.cf';
  AddLog(LogFile, fn);
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
  fn := IncludeTrailingBackslash(ExpandFileName(LabeledEdit1.Text)) + date + '.lgd';
  AddLog(LogFile, 'Backup: "' + fn + '"');
  with Process1.Parameters do
  begin
    Clear();
    Add('DESIGNER');
    Add('/DisableStartupMessages');
    Add('/DisableStartupDialogs');
    Add('/DisableSplash');
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
      Add(Utf8ToWinCP(IncludeTrailingBackslash(LabeledEdit2.Text) + '1Cv8Log\1Cv8.lgd'));
      Add('vacuum');
    end;
    SomeProc.Execute;
  end
  else
    AddLog(LogFile, 'Отсутствует: "' + SomeProc.Executable + '"');

end;


function TForm1.CheckPhysicalIntegrity(): boolean;
begin
  SomeProc.CurrentDirectory := ExtractFilePath(LabeledEdit3.Text);
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
  base_file := IncludeTrailingBackslash(LabeledEdit2.Text) + '1Cv8.1CD';
  SomeProc.CurrentDirectory := ExtractFilePath(LabeledEdit3.Text);
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
      Add('--page=' + IntToStr(SpinEdit2.Value) + 'k');
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
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
    Add('/F"' + Utf8ToWinCP(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToWinCP(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToWinCP(LabeledEdit5.Text) + '"');
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
  SelectDirectoryDialog1.InitialDir := LabeledEdit1.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    LabeledEdit1.Text := SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm1.SaveSettings();
var
  i: integer;
begin
  ini.WriteString(SectionMain, KeyExecutable, LabeledEdit3.Text);
  ini.WriteString(SectionBase, KeyBakcupPath, LabeledEdit1.Text);
  ini.WriteString(SectionBase, KeyPath, LabeledEdit2.Text);
  ini.WriteString(SectionBase, KeyUser, LabeledEdit4.Text);
  ini.WriteString(SectionBase, KeyPass, EncodeStringBase64(LabeledEdit5.Text));
  ini.WriteInteger(SectionBase, KeyBackupCount, SpinEdit1.Value);
  ini.WriteInteger(SectionBase, KeyPageSize, SpinEdit2.Value);
  ini.EraseSection(SectionUpdates);
  for i := 0 to ListBox1.Count - 1 do
    ini.WriteString(SectionUpdates, IntToStr(i), ListBox1.Items[i]);
  ini.EraseSection(SectionMacro);
  for i := 0 to ListBox2.Count - 1 do
    ini.WriteString(SectionMacro, Format('%d_%d', [i, Ord(TBaseAction(ListBox2.Items.Objects[i]))]),
      ListBox2.Items[i]);
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
  i: integer;
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
  LabeledEdit3.Text := ini.ReadString(SectionMain, KeyExecutable, '');
  LogFile := ini.ReadString(SectionMain, KeyLogFile, 'logs\' + ChangeFileExt(ExtractFileName(iniPath), '.log'));
  LabeledEdit1.Text := ini.ReadString(SectionBase, KeyBakcupPath, '');
  SpinEdit1.Value := ini.ReadInteger(SectionBase, KeyBackupCount, 3);
  SpinEdit2.Value := ini.ReadInteger(SectionBase, KeyPageSize, 8);
  LabeledEdit2.Text := ini.ReadString(SectionBase, KeyPath, '');
  LabeledEdit4.Text := ini.ReadString(SectionBase, KeyUser, '');
  try
    LabeledEdit5.Text := DecodeStringBase64(ini.ReadString(SectionBase, KeyPass, ''));
  except
  end;
  list := TStringList.Create;
  try
    ini.ReadSectionValues(SectionUpdates, list);
    for i := 0 to list.Count - 1 do
      ListBox1.Items.Add(list.ValueFromIndex[i]);
  finally
    list.Clear;
  end;
  try
    ini.ReadSectionValues(SectionMacro, list);
    for i := 0 to list.Count - 1 do
      ListBox2.AddItem(list.ValueFromIndex[i], TObject(TBaseAction(StrToInt(list.Names[i].Split('_')[1]))));
  finally
    list.Free;
  end;
  AddLog(LogFile, ExtractFileName(ParamStr(0)) + ' started!');
  if run_non_interactive then
  begin
    Application.ShowMainForm := False;
    MyThread := TMyThread.Create(ba_macro);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
end;

procedure TForm1.LabeledEdit3Change(Sender: TObject);
begin
  if not Process1.Running then
    Process1.Executable := LabeledEdit3.Text;
end;


procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := LabeledEdit2.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    LabeledEdit2.Text := SelectDirectoryDialog1.FileName;
  end;
end;


procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := ExtractFilePath(LabeledEdit3.Text);
  OpenDialog1.FilterIndex := 2;
  if OpenDialog1.Execute then
  begin
    LabeledEdit3.Text := OpenDialog1.FileName;
  end;
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
var
  k: integer;
begin
  if ListBox1.Count > 0 then
    OpenDialog1.InitialDir := ExtractFilePath(ListBox1.Items[ListBox1.Count - 1]);
  OpenDialog1.FilterIndex := 1;
  if (OpenDialog1.Execute) then
  begin
    k := ListBox1.ItemIndex;
    if (k > -1) then
      ListBox1.Items.Insert(k, OpenDialog1.FileName)
    else
      ListBox1.Items.Add(OpenDialog1.FileName);
  end;
end;

procedure TForm1.MenuItem20Click(Sender: TObject);
var
  k: integer;
begin
  OpenDialog1.InitialDir := LabeledEdit1.Text;
  OpenDialog1.FilterIndex := 4;
  if OpenDialog1.Execute then
  begin
    k := ListBox2.ItemIndex;
    if (k > -1) then
      ListBox2.Items.InsertObject(k, Format('%s(%s)', [TMenuItem(Sender).Caption, OpenDialog1.FileName]), TObject(ba_restoreib))
    else
      ListBox2.AddItem(Format('%s(%s)', [TMenuItem(Sender).Caption, OpenDialog1.FileName]), TObject(ba_restoreib));
  end;
end;

procedure TForm1.MenuItem21Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_update))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_update));
end;

procedure TForm1.MenuItem22Click(Sender: TObject);
var
  k: integer;
  params: string;
begin
  params := GetIBCheckParams();
  if not params.IsEmpty then
  begin
    k := ListBox2.ItemIndex;
    if (k > -1) then
      ListBox2.Items.InsertObject(k, Format('%s(%s)', [TMenuItem(Sender).Caption, params]), TObject(ba_check))
    else
      ListBox2.AddItem(Format('%s(%s)', [TMenuItem(Sender).Caption, params]), TObject(ba_check));
  end;
end;

procedure TForm1.MenuItem24Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_dumpcfg))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_dumpcfg));
end;

procedure TForm1.MenuItem25Click(Sender: TObject);
var
  k: integer;
begin
  OpenDialog1.InitialDir := LabeledEdit1.Text;
  OpenDialog1.FilterIndex := 3;
  if OpenDialog1.Execute then
  begin
    k := ListBox2.ItemIndex;
    if (k > -1) then
      ListBox2.Items.InsertObject(k, Format('%s(%s)', [TMenuItem(Sender).Caption, OpenDialog1.FileName]), TObject(ba_loadcfg))
    else
      ListBox2.AddItem(Format('%s(%s)', [TMenuItem(Sender).Caption, OpenDialog1.FileName]), TObject(ba_loadcfg));
  end;
end;

procedure TForm1.MenuItem27Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_macro);
end;

procedure TForm1.MenuItem28Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if k < 0 then
    exit;
  ListBox2.Items.Delete(k);
end;

procedure TForm1.MenuItem29Click(Sender: TObject);
begin
  ListBox2.Items.Clear;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox1.ItemIndex;
  if k < 0 then
    exit;
  ListBox1.Items.Delete(k);
end;

procedure TForm1.MenuItem30Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_cache))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_cache));
end;

procedure TForm1.MenuItem31Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_cache);
end;

procedure TForm1.MenuItem32Click(Sender: TObject);
begin
  CalendarDialog1.Date := Now();
  if CalendarDialog1.Execute then
    MyThread := TMyThread.Create(ba_journal, FormatDateTime('yyyy-mm-dd', CalendarDialog1.Date));
end;

procedure TForm1.MenuItem33Click(Sender: TObject);
var
  k: integer;
begin
  CalendarDialog1.Date := Now();
  if CalendarDialog1.Execute then
  begin
    k := ListBox2.ItemIndex;
    if (k > -1) then
      ListBox2.Items.InsertObject(k, Format('%s(%s)', [TMenuItem(Sender).Caption,
        FormatDateTime('yyyy-mm-dd', CalendarDialog1.Date)]), TObject(ba_journal))
    else
      ListBox2.AddItem(Format('%s(%s)', [TMenuItem(Sender).Caption, FormatDateTime('yyyy-mm-dd', CalendarDialog1.Date)]),
        TObject(ba_journal));
  end;
end;

procedure TForm1.MenuItem34Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_integrity);
end;

procedure TForm1.MenuItem35Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_integrity))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_integrity));
end;

procedure TForm1.MenuItem36Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_physical);
end;

procedure TForm1.MenuItem37Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_physical))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_physical));
end;

procedure TForm1.MenuItem38Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_convert))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_convert));
end;

procedure TForm1.MenuItem39Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_convert);
end;


procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  ListBox1.Items.Clear;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_enterprise);
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_config);
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  SaveSettings();
end;

procedure TForm1.MenuItem15Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_update);
end;


procedure TForm1.MenuItem17Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_enterprise))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_enterprise));
end;

procedure TForm1.MenuItem18Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_config))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_config));
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox2.ItemIndex;
  if (k > -1) then
    ListBox2.Items.InsertObject(k, TMenuItem(Sender).Caption, TObject(ba_dumpib))
  else
    ListBox2.AddItem(TMenuItem(Sender).Caption, TObject(ba_dumpib));
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_dumpib);
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := LabeledEdit1.Text;
  OpenDialog1.FilterIndex := 4;
  if OpenDialog1.Execute then
  begin
    MyThread := TMyThread.Create(ba_restoreib, OpenDialog1.FileName);
  end;
end;

procedure TForm1.MenuItem14Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_dumpcfg);
end;

procedure TForm1.MenuItem13Click(Sender: TObject);
begin
  OpenDialog1.InitialDir := LabeledEdit1.Text;
  OpenDialog1.FilterIndex := 3;
  if OpenDialog1.Execute then
  begin
    MyThread := TMyThread.Create(ba_loadcfg, OpenDialog1.FileName);
  end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
var
  params: string;
begin
  params := GetIBCheckParams();
  if not params.IsEmpty then
    MyThread := TMyThread.Create(ba_check, params);
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  MessageDlg('Программа для пакетного обновления типовых конфигураций!' +
    #10#13 + 'Version: ' + Version + #10#13 + 'Автор: Дмитрий Воротилин, dvor85@gmail.com',
    mtInformation, [mbOK], 0);
end;
























end.
