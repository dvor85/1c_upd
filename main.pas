unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Forms, Controls, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, CheckLst,
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
  TMyThread = class(TThread)
    FAction: integer;
    constructor Create(CreateSuspended: boolean; Action: integer); overload;
  protected
    procedure Execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)

    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CheckListBox1: TCheckListBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    LabeledEdit5: TLabeledEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Process1: TProcess;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure LabeledEdit3Change(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    { private declarations }
    MyThread: TMyThread;
    LogFile: string;
    ini: TIniFile;
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    function dumpIB(): boolean;
    function dumpCFG(): boolean;
    function CheckAndRepair(): boolean;
    function updateBase(upd: string): boolean;
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


constructor TMyThread.Create(CreateSuspended: boolean; Action: integer);
begin
  inherited Create(CreateSuspended);
  FAction := Action;
  FreeOnTerminate := True;
end;

procedure TMyThread.Execute;
var
  i, progress: integer;
begin
  try
    with Form1 do
    begin
      if Process1.Running then
        raise Exception.Create('Процесс уже запущен, дождитесь окончания!');

      if (Process1.Executable = '') or (LabeledEdit2.Text = '') then
        raise Exception.Create('Не заполнены необходимые поля!');
      case FAction of
        0:
        begin
          Form1.Caption := 'Идет обновление: 0%...';
          for i := 0 to CheckListBox1.Count - 1 do
          begin
            AddLog(LogFile, CheckListBox1.Items[i]);
            if CheckListBox1.Checked[i] then
              if not dumpIB() then
                raise Exception.Create('Выгрузка информационной базы не выполнена!');

            if not updateBase(CheckListBox1.Items[i]) then
              raise Exception.Create('Ошибка обновления!');
            progress := (i + 1) * 100 div (CheckListBox1.Count + 1);
            Form1.Caption := 'Идет обновление: ' + IntToStr(progress) + '%...';
          end;

          if not CheckAndRepair() then
            raise Exception.Create('Тестирование и исправление базы завершено с ошибкой!');

          Form1.Caption := 'Обновление успешно завершено';
        end;

        1:
        begin
          Form1.Caption := 'Идет выгрузка информационной базы...';
          if not dumpIB() then
            raise Exception.Create('Выгрузка информационной базы не выполнена!');
          Form1.Caption := 'Выгрузка информационной базы завершена!';
        end;

        2:
        begin
          Form1.Caption := 'Идет выгрузка конфигурации...';
          if not dumpCFG() then
            raise Exception.Create('Кофигурация не выгружена!');
          Form1.Caption := 'Кофигурация успешно выгружена!';
        end;

        3:
        begin
          Form1.Caption := 'Идет тестирование и исправление базы...';
          if not CheckAndRepair() then
            raise Exception.Create('Тестирование и исправление базы завершено с ошибкой!');
          Form1.Caption := 'Тестирование и исправление базы успешно завершено!';
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
  self.Terminate;
end;



function TForm1.dumpIB(): boolean;
begin
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/DumpIB"' + IncludeTrailingBackslash(Utf8ToAnsi(LabeledEdit1.Text)) +
      FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.dt"');
    Add('/Out ' + LogFile + ' -NoTruncate');
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
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/DumpCfg"' + IncludeTrailingBackslash(Utf8ToAnsi(LabeledEdit1.Text)) +
      FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.cf"');
    Add('/Out ' + LogFile + ' -NoTruncate');
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
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/IBCheckAndRepair -Rebuild -ReIndex -LogIntergrity -RecalcTotals -IBCompression');
    Add('/Out ' + LogFile + ' -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

function TForm1.updateBase(upd: string): boolean;
begin
  if (Process1.Executable = '') or (LabeledEdit2.Text = '') then
    raise Exception.Create('Не заполнены необходимые поля!');
  with Process1.Parameters do
  begin
    Clear();
    Add('CONFIG');
    Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
    Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
    Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
    Add('/UpdateCfg"' + Utf8ToAnsi(upd) + '"');
    Add('/UpdateDBCfg');
    Add('/Out ' + LogFile + ' -NoTruncate');
  end;
  Process1.Execute;
  Result := Process1.ExitStatus = 0;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
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
    for i := 0 to CheckListBox1.Count - 1 do
      ini.WriteString(SectionUpdates, IntToStr(i), CheckListBox1.Items[i]);
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

  ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  LabeledEdit3.Text := ini.ReadString(SectionMain, KeyExecutable, '');
  LogFile := ini.ReadString(SectionMain, KeyLogFile, 'logs\out.log');
  LabeledEdit1.Text := ini.ReadString(SectionBase, KeyBakcupPath, '');
  LabeledEdit2.Text := ini.ReadString(SectionBase, KeyPath, '');
  LabeledEdit4.Text := ini.ReadString(SectionBase, KeyUser, '');
  LabeledEdit5.Text := DecodeStringBase64(ini.ReadString(SectionBase, KeyPass, ''));
  list := TStringList.Create;
  try
    ini.ReadSectionValues(SectionUpdates, list);
    for i := 0 to list.Count - 1 do
      CheckListBox1.Items.Add(list.ValueFromIndex[i]);
  finally
    list.Free;
  end;
  Process1.CurrentDirectory := ExtractFilePath(ParamStr(0));
  Process1.Options := [poWaitOnExit];
end;

procedure TForm1.LabeledEdit3Change(Sender: TObject);
begin
  Process1.Executable := LabeledEdit3.Text;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    LabeledEdit2.Text := SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
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
  OpenDialog1.FilterIndex := 1;
  if (OpenDialog1.Execute) then
  begin
    k := CheckListBox1.ItemIndex;
    if (k > -1) then
      CheckListBox1.Items.Insert(k, OpenDialog1.FileName)
    else
      CheckListBox1.Items.Add(OpenDialog1.FileName);
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  k: integer;
begin
  k := CheckListBox1.ItemIndex;
  if k < 0 then
    exit;
  CheckListBox1.Items.Delete(k);
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(False, 0);
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  CheckListBox1.Items.Clear;
end;

procedure TForm1.MenuItem6Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(False, 1);
end;

procedure TForm1.MenuItem7Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(False, 2);
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  MyThread := TMyThread.Create(False, 3);
end;




end.
