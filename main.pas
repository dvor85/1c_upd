unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, Menus, CheckLst;

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
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    Process1: TProcess;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
  private
    { private declarations }
    MyThread: TMyThread;
    LogFile: string;
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
  i: integer;
begin
  with Form1 do
  begin
    if (LabeledEdit3.Text = '') or (LabeledEdit2.Text = '') then
    begin
      AddLog(LogFile, 'Не заполнены необходимые поля!');
      exit;
    end;
    Process1.Executable := LabeledEdit3.Text;
    Process1.Options := [poWaitOnExit, poUsePipes, poStderrToOutPut];
    Process1.CurrentDirectory := ExtractFilePath(ParamStr(0));
    Form1.Caption := 'Обновление: 0%';
    for i := 0 to CheckListBox1.Count - 1 do
    begin
      AddLog(LogFile, CheckListBox1.Items[i]);
      Process1.Active := False;
      with Process1.Parameters do
      begin
        //Выгрузка информационной базы
        if (CheckListBox1.Checked[i]) then
        begin
          Clear();
          Add('CONFIG');
          Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
          Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
          Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
          Add('/DumpIB"' + IncludeTrailingBackslash(Utf8ToAnsi(LabeledEdit1.Text)) +
            FormatDateTime('dd.mm.yyyy_hh.nn.ss', Now()) + '.dt"');
          Add('/Out ' + LogFile + ' -NoTruncate');
          Process1.Execute;
        end;

        //Обновление информационной базы
        Clear();
        Add('CONFIG');
        Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
        Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
        Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
        Add('/UpdateCfg"' + Utf8ToAnsi(CheckListBox1.Items[i]) + '"');
        Add('/UpdateDBCfg');
        Add('/Out ' + LogFile + ' -NoTruncate');
        Process1.Execute;
      end;
      Form1.Caption := 'Обновление: ' + IntToStr((i + 1) * 100 div (CheckListBox1.Count + 1)) + '%';
    end;
    with Process1.Parameters do
    begin
      Clear();
      Add('CONFIG');
      Add('/F"' + Utf8ToAnsi(LabeledEdit2.Text) + '"');
      Add('/N"' + Utf8ToAnsi(LabeledEdit4.Text) + '"');
      Add('/P"' + Utf8ToAnsi(LabeledEdit5.Text) + '"');
      Add('/IBCheckAndRepair -ReIndex -LogIntergrity -RecalcTotals -IBCompression');
      Add('/Out ' + LogFile + ' -NoTruncate');
      Process1.Execute;
    end;
    Form1.Caption := 'Обновление: ' + '100% ' + 'Проверьте ' + ExtractFilePath(ParamStr(0)) + LogFile;
  end;
  self.Terminate;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
  begin
    LabeledEdit1.Text := SelectDirectoryDialog1.FileName;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LogFile := 'logs\out.log';
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
begin
  OpenDialog1.FilterIndex := 1;
  if (OpenDialog1.Execute) then
  begin
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
  MyThread := TMyThread.Create(false);
end;


end.
