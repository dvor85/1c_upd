unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, CheckLst, IniPropStorage, IniFiles;

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
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
  private
    { private declarations }
    MyThread: TMyThread;
    LogFile: string;
    ini: TIniFile;
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
    function dumpBase(): boolean;
    function testBase(): boolean;
    function updateBase(upd: string): boolean;
  public
    { public declarations }
  end;




var
  Form1: TForm1;


implementation



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




procedure TMyThread.Execute;
var
  i, progress: integer;
begin
  try
    with Form1 do
    begin
      if Process1.Running then
        raise Exception.Create('Процесс обновления уже запущен, дождитесь окончания!');
      if (LabeledEdit3.Text = '') or (LabeledEdit2.Text = '') then
        raise Exception.Create('Не заполнены необходимые поля!');

      Process1.Executable := LabeledEdit3.Text;

      Process1.CurrentDirectory := ExtractFilePath(ParamStr(0));
      Form1.Caption := 'Обновление: 0%';
      for i := 0 to CheckListBox1.Count - 1 do
      begin
        AddLog(LogFile, CheckListBox1.Items[i]);
        if CheckListBox1.Checked[i] then
          if not dumpBase() then
            raise Exception.Create('Резервная копия базы не создана!');

        if not updateBase(CheckListBox1.Items[i]) then
          raise Exception.Create('Ошибка обновления!');
        progress := (i + 1) * 100 div (CheckListBox1.Count + 1);
        Form1.Caption := 'Обновление: ' + IntToStr(progress) + '%';
      end;

      if not testBase() then
        raise Exception.Create('Тестирование и исправление базы завершено с ошибкой!');
      progress := 100;
      Form1.Caption := 'Обновление: ' + IntToStr(progress) + '% ' + ' Проверьте ' +
        ExtractFilePath(ParamStr(0)) + LogFile;
      MessageDlg('Обновление успешно завершено!', mtInformation, [mbOK], 0);
    end;

  except
    on e: Exception do
    begin
      AddLog(Form1.LogFile, e.message);
      Form1.Caption := 'Обновление завершено с ошибкой: "' + e.message + '" Проверьте ' +
        ExtractFilePath(ParamStr(0)) + Form1.LogFile;
      MessageDlg(Form1.Caption, mtError, [mbOK], 0);
    end;
  end;
  self.Terminate;
end;



function TForm1.dumpBase(): boolean;
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

function TForm1.testBase(): boolean;
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
    ini.WriteString(SectionBase, KeyPass, LabeledEdit5.Text);
    ini.EraseSection(SectionUpdates);
    for i := 0 to CheckListBox1.Count - 1 do
      ini.WriteString(SectionUpdates, IntToStr(i), CheckListBox1.Items[i]);
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Process1.Running then
    CanClose := (MessageDlg('Идет процесс обновления. Вы действительно хотите его прервать?',
      mtWarning, mbYesNo, 0) = mrYes)
  else
    CanClose := True;
end;

procedure TForm1.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
  AddLog(LogFile, E.Message);
  MessageDlg('Непредвиденная ошибка: ' + E.Message + '.Проверьте ' + ExtractFilePath(ParamStr(0)) +
    LogFile, mtError, [mbOK], 0);
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
  LabeledEdit5.Text := ini.ReadString(SectionBase, KeyPass, '');
  list := TStringList.Create;
  try
    ini.ReadSectionValues(SectionUpdates, list);
    for i := 0 to list.Count - 1 do
      CheckListBox1.Items.Add(list.ValueFromIndex[i]);
  finally
    list.Free;
  end;
  Process1.Options := [poWaitOnExit];
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
  MyThread := TMyThread.Create(False);
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  CheckListBox1.Items.Clear;
end;




end.
