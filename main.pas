unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, CheckLst, ComCtrls,
  IniFiles;

const
  SectionMain: string = 'Main';
  KeyExecutable: string = 'Executable';
  SectionBase: string = 'Base';
  KeyPath: string = 'Path';
  KeyBakcupPath: string = 'BakcupPath';
  KeyUser: string = 'User';
  KeyPass: string = 'Password';
  KeyLogFile: string = 'LogFile';
  SectionUpdates: string = 'Updates';

type

  TBaseAction = (ba_update, ba_dumpib, ba_restoreib, ba_dumpcfg, ba_loadcfg, ba_check, ba_enterprise, ba_config);

  TMyThread = class(TThread)
    FBaseAction: TBaseAction;
    constructor Create(BaseAction: TBaseAction); overload;
  protected
    procedure Execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)

    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    ListBox1: TListBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Process1: TProcess;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure LabeledEdit3Change(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);

  private
    { private declarations }
    MyThread: TMyThread;
    LogFile: string;
    iniPath: string;
    ini: TIniFile;
    procedure SetComponentsEnabled(State: boolean);
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    function runEnterprise(): boolean;
    function runConfig(): boolean;
    function dumpIB(): boolean;
    function restoreIB(filename: string): boolean;
    function dumpCFG(): boolean;
    function loadCFG(filename: string; updateCfg: boolean = True): boolean;
    function CheckAndRepair(): boolean;
    function updateBase(filename: string; updateCfg: boolean = False): boolean;
    function updateCFG(): boolean;
  public
    { public declarations }
  end;




var
  Form1: TForm1;


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
    except
      MessageDlg(Form1.Caption, LogString, mtError, [mbYes], 0);
      Exit;
    end;
  finally
    StrDispose(PStr);
    F.Free;
  end;
end;

{ TForm1 }


constructor TMyThread.Create(BaseAction: TBaseAction);
begin
  inherited Create(False);
  FBaseAction := BaseAction;
  FreeOnTerminate := True;
end;

procedure TMyThread.Execute;
var
  i, progress, totalTasks, currentTask: integer;
begin
  Form1.SetComponentsEnabled(False);
  try
    try
      with Form1 do
      begin
        if Process1.Running then
          raise Exception.Create('Процесс уже запущен, дождитесь окончания!');

        if (Process1.Executable = '') or (LabeledEdit2.Text = '') then
          raise Exception.Create('Не заполнены необходимые поля!');

        case FBaseAction of
          ba_update:
          begin
            currentTask := 0;
            totalTasks := 4 + ListBox1.Count;
            if not dumpIB() then
              raise Exception.Create('Выгрузка информационной базы не выполнена!');
            Inc(currentTask);
            progress := currentTask * 100 div totalTasks;
            Form1.Caption := 'Идет обновление: ' + IntToStr(progress) + '%...';

            if not CheckAndRepair() then
              raise Exception.Create('Тестирование и исправление базы завершено с ошибкой!');
            Inc(currentTask);
            progress := currentTask * 100 div totalTasks;
            Form1.Caption := 'Идет обновление: ' + IntToStr(progress) + '%...';

            for i := 0 to ListBox1.Count - 1 do
            begin
              AddLog(LogFile, 'Установка обновления: "' + ListBox1.Items[i] + '"');
              if not updateBase(ListBox1.Items[i], (i = ListBox1.Count - 1)) then
                raise Exception.Create('Ошибка обновления!');
              Inc(currentTask);
              progress := currentTask * 100 div totalTasks;
              Form1.Caption := 'Идет обновление: ' + IntToStr(progress) + '%...';
            end;

            if not runEnterprise() then
              raise Exception.Create('Запуск в режиме ENTERPRISE завершен с ошибкой!');
            Inc(currentTask);
            progress := currentTask * 100 div totalTasks;
            Form1.Caption := 'Идет обновление: ' + IntToStr(progress) + '%...';

            if not CheckAndRepair() then
              raise Exception.Create('Тестирование и исправление базы завершено с ошибкой!');

            Form1.Caption := 'Обновление успешно завершено';
          end;

          ba_dumpib:
          begin
            AddLog(LogFile, 'Выгрузка информационной базы');
            Form1.Caption := 'Идет выгрузка информационной базы...';
            if not dumpIB() then
              raise Exception.Create('Выгрузка информационной базы не выполнена!');
            Form1.Caption := 'Выгрузка информационной базы завершена!';
          end;

          ba_restoreib:
          begin
            AddLog(LogFile, 'Загрузка информационной базы');
            Form1.Caption := 'Идет загрузка информационной базы...';
            if not restoreIB(OpenDialog1.FileName) then
              raise Exception.Create('Ошибка загрузки информационной базы!');
            Form1.Caption := 'Загрузка информационной базы успешно завершено!';
          end;

          ba_dumpcfg:
          begin
            AddLog(LogFile, 'Выгрузка конфигурации');
            Form1.Caption := 'Идет выгрузка конфигурации...';
            if not dumpCFG() then
              raise Exception.Create('Кофигурация не выгружена!');
            Form1.Caption := 'Кофигурация успешно выгружена!';
          end;

          ba_loadcfg:
          begin
            AddLog(LogFile, 'Загрузка конфигурации');
            Form1.Caption := 'Идет загрузка конфигурации...';
            if not loadCFG(OpenDialog1.FileName) then
              raise Exception.Create('Ошибка загрузки конфигурации!');
            Form1.Caption := 'Загрузка конфигурации успешно завершено!';
          end;

          ba_check:
          begin
            AddLog(LogFile, 'Тестирование и исправление базы');
            Form1.Caption := 'Идет тестирование и исправление базы...';
            if not CheckAndRepair() then
              raise Exception.Create('Тестирование и исправление базы завершено с ошибкой!');
            Form1.Caption := 'Тестирование и исправление базы успешно завершено!';
          end;

          ba_enterprise:
          begin
            AddLog(LogFile, 'Запуск в режиме ENTERPRISE');
            Form1.Caption := 'Запуск в режиме ENTERPRISE...';
            if not runEnterprise() then
              raise Exception.Create('Запуск в режиме ENTERPRISE завершен с ошибкой!');
            Form1.Caption := 'Запуск в режиме ENTERPRISE успешно завершен!';
          end;

          ba_config:
          begin
            AddLog(LogFile, 'Запуск в режиме конфигуратора');
            Form1.Caption := 'Запуск в режиме конфигуратора...';
            if not runConfig() then
              raise Exception.Create('Запуск в режиме конфигуратора завершен с ошибкой!');
            Form1.Caption := 'Запуск в режиме конфигуратора успешно завершен!';
          end;

        end;
      end;
      MessageDlg(Form1.Caption, mtInformation, [mbOK], 0);
    except
      on e: Exception do
      begin
        AddLog(Form1.LogFile, e.message);
        Form1.Caption := 'Операция завершена с ошибкой: "' + e.message + '". Проверьте ' +
          ExtractFilePath(ParamStr(0)) + Form1.LogFile;
        MessageDlg(Form1.Caption, mtError, [mbOK], 0);
      end;
    end;
  finally
    Form1.SetComponentsEnabled(True);
    self.Terminate;
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
end;

function TForm1.runEnterprise(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('ENTERPRISE');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.runConfig(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.restoreIB(filename: string): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/RestoreIB"' + Utf8ToAnsi(filename) + '"');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.dumpIB(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/DumpIB"' + IncludeTrailingBackslash(Utf8ToAnsi(LabeledEdit1.Text)) +
      FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.dt"');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.loadCFG(filename: string; updateCfg: boolean = True): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/LoadCfg"' + Utf8ToAnsi(filename) + '"');
    if updateCfg then
      Add('/UpdateDBCfg');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.dumpCFG(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/DumpCfg"' + IncludeTrailingBackslash(Utf8ToAnsi(LabeledEdit1.Text)) +
      FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.cf"');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.CheckAndRepair(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/IBCheckAndRepair -Rebuild -ReIndex -LogAndRefsIntegrity -RecalcTotals -IBCompression');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.updateBase(filename: string; updateCfg: boolean = False): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/UpdateCfg"' + Utf8ToAnsi(filename) + '"');
    if updateCfg then
      Add('/UpdateDBCfg');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.updateCFG(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/DisableStartupMessages');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/UpdateDBCfg');
    Add('/Out "' + Utf8ToAnsi(LogFile) + '" -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  SelectDirectoryDialog1.InitialDir := LabeledEdit1.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    LabeledEdit1.Text := SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: integer;
begin
  if MessageDlg('Сохранить настройки?', mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    ini.WriteString(SectionMain, KeyExecutable, LabeledEdit3.Text);
    ini.WriteString(SectionBase, KeyBakcupPath, LabeledEdit1.Text);
    ini.WriteString(SectionBase, KeyPath, LabeledEdit2.Text);
    ini.WriteString(SectionBase, KeyUser, LabeledEdit4.Text);
    ini.WriteString(SectionBase, KeyPass, EncodeStringBase64(LabeledEdit5.Text));
    ini.EraseSection(SectionUpdates);
    for i := 0 to ListBox1.Count - 1 do
      ini.WriteString(SectionUpdates, IntToStr(i), ListBox1.Items[i]);
    ini.Free;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Process1.Running then
    CanClose := (MessageDlg('Идет работа с базой данных. Вы действительно хотите закрыть программу?',
      mtWarning, mbYesNo, 0) = mrYes)
  else
    CanClose := True;
end;

procedure TForm1.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
  AddLog(LogFile, E.Message);
  MessageDlg('Непредвиденная ошибка: "' + E.Message + '" .Проверьте ' +
    ExtractFilePath(ParamStr(0)) + LogFile, mtError, [mbOK], 0);
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
  if Paramcount > 0 then
    iniPath := ParamStr(1);
  StatusBar1.SimpleText := AnsiToUtf8(iniPath);
  ini := TIniFile.Create(iniPath);
  LabeledEdit3.Text := ini.ReadString(SectionMain, KeyExecutable, '');
  LogFile := ini.ReadString(SectionMain, KeyLogFile, 'logs\out.log');
  LabeledEdit1.Text := ini.ReadString(SectionBase, KeyBakcupPath, '');
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
    list.Free;
  end;

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

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  k: integer;
begin
  k := ListBox1.ItemIndex;
  if k < 0 then
    exit;
  ListBox1.Items.Delete(k);
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

procedure TForm1.MenuItem15Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_update);
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
    MyThread := TMyThread.Create(ba_restoreib);
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
    MyThread := TMyThread.Create(ba_loadcfg);
  end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(ba_check);
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  MessageDlg('Программа для пакетного обновления типовых конфигураций!' + #10#13 +
    'Автор: Дмитрий Воротилин, dvor85@gmail.com',
    mtInformation, [mbOK], 0);
end;





end.
