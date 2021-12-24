unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  IniFiles, LCLType, Buttons, Spin;

type
  
  { TMainForm }

  TMainForm = class(TForm)
    btnGetExchangeRates: TButton;
    btConvert: TButton;
    cbSource: TComboBox;
    cbDest: TComboBox;
    edSearchCountry: TEdit;
    GroupBox1: TGroupBox;
    seSource: TFloatSpinEdit;
    Grid: TStringGrid;
    gbCalculator: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    btnClearSearch: TSpeedButton;
    Label2: TLabel;
    edDest: TEdit;
    SpeedButton1: TSpeedButton;
    procedure btnGetExchangeRatesClick(Sender: TObject);
    procedure btConvertClick(Sender: TObject);
    procedure SymbolDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SymbolDragOver(Sender, Source: TObject; X, Y: Integer; 
      State: TDragState; var Accept: Boolean);
    procedure edSearchCountryUTF8KeyPress(Sender: TObject; 
      var UTF8Key: TUTF8Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure GridCompareCells(Sender: TObject; ACol, ARow, BCol, 
      BRow: Integer; var Result: integer);
    procedure SpeedButton1Click(Sender: TObject);
    
  private
    FCurrenciesToIni: Boolean;
    function CreateIniFile: TCustomIniFile;
    procedure LoadFromIni;
    procedure PrepareGrid;
    procedure SaveToIni;
    
  protected
    function BuildURL: String;
    procedure ExtractExchangeRates(AStream: TStream); 

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  {$IF FPC_FullVersion >= 30200}
  opensslsockets,
  {$IFEND}
  fphttpclient,
  fpjson, jsonparser, math;

const 
  BASE_URL = 'https://openexchangerates.org/api/';
  
var
  APP_ID: String = ''; 

// Get file from the internet
function Download(URL: String; AStream: TStream; out AErrMsg: String): Boolean;
begin
  AErrMsg := '';
  with TFpHttpClient.Create(nil) do
    try
      try
        AllowRedirect := true;
        Get(URL, AStream);
        AStream.Position := 0;
        Result := true;
      except
        on E:EHTTPClient do begin
          AErrMsg := E.Message;
          Result := false;
        end;
      end;
    finally
      Free;
    end;
end;


{ TMainForm }

procedure TMainForm.btnClearSearchClick(Sender: TObject);
begin
  edSearchCountry.Clear;
end;

procedure TMainForm.GridCompareCells(Sender: TObject; ACol, ARow, BCol, 
  BRow: Integer; var Result: integer);
const
  HUGE = 1E10;
var
  val1, val2: Double;
  s1, s2: String;
begin
  s1 := Grid.Cells[ACol, ARow];
  s2 := Grid.Cells[BCol, BRow];
  if Grid.SortColumn = 1 then
  begin
    if not TryStrToFloat(s1, val1) then
      val1 := HUGE;
    if not TryStrToFloat(s2, val2) then
      val2 := HUGE;
    Result := Math.CompareValue(val1, val2);
  end else
    Result := CompareText(s1, s2);
  if Grid.SortOrder = soDescending then 
    Result := -Result;
end;

procedure TMainForm.SpeedButton1Click(Sender: TObject);
var
  s: String;
begin
  s := cbSource.Text;
  cbSource.Text := cbDest.Text;
  cbDest.Text := s;
  edDest.Clear;
end;

procedure TMainForm.btnGetExchangeRatesClick(Sender: TObject);
var
  url: String;
  stream: TMemoryStream;
  err: String = '';
  res: TModalResult;
  ini: TCustomIniFile;
begin
  if App_ID = '' then begin
    res := MessageDlg('App_ID not specified.' + LineEnding + LineEnding +
      'Please register at https://openexchangerates.org (free account)' +
      'and enter the received App_ID in the next dialog.', mtError, mbYesNoCancel, 0);
    if res = mrYes then
    begin
      App_ID := InputBox('Access to OpenExchangeRates.org', 'Enter App_ID', '');
      if App_ID = '' then
        exit;
      ini := CreateIniFile;
      try
        ini.WriteString('Settings', 'App_ID', App_ID);
      finally
        ini.Free;
      end;
    end else
      exit;
  end;
      
  Screen.Cursor := crHourGlass;
  url := BuildURL;
  stream := TMemoryStream.Create;
  try
    if Download(url, stream, err) then
    begin
      stream.Position := 0;
      ExtractExchangeRates(stream);
      FCurrenciesToIni := true;
    end else 
      ShowMessage(err);
  finally
    stream.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.btConvertClick(Sender: TObject);
var
  srcValue, destValue: Double;
  srcCurr, destCurr: String;
  srcRate, destRate: Double;
  idx: Integer;
begin
  srcCurr := cbSource.Text;
  if srcCurr = '' then
  begin
    MessageDlg('No source currency selected.', mtError, [mbOK], 0);
    cbSource.SetFocus;
    exit;
  end;
  
  idx := Grid.Cols[3].IndexOf(srcCurr);
  if idx = -1 then
  begin
    MessageDlg('Source currency symbol not found.', mtError, [mbOk], 0);
    cbSource.SetFocus;
    exit;
  end;
  srcRate := StrToFloat(Grid.Cells[1, idx]);
  
  destCurr := cbDest.Text;
  if destCurr = '' then
  begin
    MessageDlg('No result currency selected.', mtError, [mbOk], 0);
    cbDest.SetFocus;
    exit;
  end;
  
  idx := Grid.Cols[3].IndexOf(destCurr);
  if idx = -1 then
  begin
    MessageDlg('Result currency symbol not found.', mtError, [mbOK], 0);
    cbDest.SetFocus;
    exit;
  end;
  destRate := StrToFloat(Grid.Cells[1, idx]);

  if not TryStrToFloat(seSource.Text, srcValue) then
  begin
    MessageDlg('No valid number entered.', mtError, [mbOK], 0);
    seSource.SetFocus;
    exit;
  end;
  destValue := srcValue * srcRate / destRate;
  
  edDest.Text := FormatFloat('0.00', destValue);
end;

function TMainForm.BuildURL: String;
begin
  Result := Format('%s%s?app_id=%s', [BASE_URL, 'latest.json', APP_ID]);
  // Thid uses base currency USD; others are not allowed by the free version.
end;

procedure TMainForm.edSearchCountryUTF8KeyPress(Sender: TObject; 
  var UTF8Key: TUTF8Char);
var
  s: String;
  i: Integer;
begin
  s := Uppercase(edSearchCountry.Text + UTF8Key);
  for i := 1 to Grid.RowCount-1 do
    if pos(s, Uppercase(Grid.Cells[0, i])) = 1 then
      Grid.Row := i;
end;

function TMainForm.CreateIniFile: TCustomIniFile;
begin
  Result := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    SaveToIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadFromIni;
  
  if Grid.RowCount = 1 then
  begin
    PrepareGrid;
    FCurrenciesToIni := true;

    cbSource.Items.Assign(Grid.Cols[3]);
    cbSource.Items.Delete(0);
    cbSource.ItemIndex := -1;
    
    cbDest.Items.Assign(Grid.Cols[3]);
    cbDest.Items.Delete(0);
    cbDest.ItemIndex := -1;
  end else
    FCurrenciesToIni := false;
  
  Grid.RowHeights[0] := 2*Grid.DefaultRowHeight - 2*varCellPadding;
end;

procedure TMainForm.ExtractExchangeRates(AStream: TStream);
var
  jParser: TJsonParser;
  json: TJsonData = nil;
  jRates: TJsonObject;
  i, j: Integer;
  exchRate: TJSONFloat;
  currCode: String;
begin
  jParser := TJsonParser.Create(AStream, []);
  try
    json := jParser.Parse;
    jRates := TJsonObject(json.FindPath('rates'));
    for i := 0 to jRates.Count-1 do
    begin
      currCode := jRates.Names[i];
      exchRate := jRates.Items[i].AsFloat;
      for j := 1 to Grid.RowCount-1 do 
        if Grid.Cells[3, j] = currCode then
          Grid.Cells[1, j] := FormatFloat('0.0000', exchRate);
    end;
  finally
    json.Free;
    jParser.Free;
  end;
end;  

procedure TMainForm.LoadFromIni;
var
  ini: TCustomIniFile;
  L: TStringList;
  i, r: Integer;
  s: String;
  section: String;
  sa: TStringArray;
begin
  ini := CreateIniFile;
  try
    App_ID := ini.ReadString('Settings', 'App_ID', '');
  
    section := 'Exchange rates';
    L := TStringList.Create;
    Grid.BeginUpdate;
    try
      ini.ReadSection(section, L);
      Grid.Clear;
      Grid.RowCount := L.Count + 1;
      r := Grid.RowCount;
      r := Grid.FixedRows;
      for i := 0 to L.Count-1 do begin
        Grid.Cells[0, r] := L[i];
        s := ini.ReadString(section, L[i], '');
        if s <> '' then
        begin
          sa := s.Split(';');
          Grid.Cells[1, r] := sa[0];
          Grid.Cells[2, r] := sa[1];
          Grid.Cells[3, r] := sa[2];
        end;
        inc(r);
      end;
    finally
      Grid.EndUpdate;
      L.Free;
    end;

    L := TStringList.Create;
    try
      L.Duplicates := dupIgnore;
      L.Sorted := true;
      L.Assign(Grid.Cols[3]);
      cbSource.Items.Assign(L);
      cbDest.Items.Assign(L);
    finally
      L.Free;
    end;

    section := 'Calculator';
    seSource.Value := ini.ReadFloat(section, 'Amount', 100.0);
    cbSource.Text := ini.ReadString(section, 'SourceCurrency', '');
    cbDest.Text := ini.ReadString(section, 'DestinationCurrency', '');

  finally
    ini.Free;
  end;
end;

procedure TMainForm.PrepareGrid;
var
  L: TStrings;
  stream: TResourceStream;
  sa: TStringArray;
  i: Integer;
begin
  L := TStringList.Create;
  try
    // Country names and currency names are stored in a resource file which is
    // based on the xls file "list_one.xls" provided by 
    // https://www.six-group.com/en/products-services/financial-information/data-standards.html
    // It has country, currency name, currency symbol in 
    // columns 0, 1 and 2, respectively.
    // Unnecessary lines were removed from the file.
    stream := TResourceStream.Create(HINSTANCE, 'LIST_ONE', RT_RCDATA);
    try
      L.LoadFromStream(stream);
    finally
      stream.Free;
    end;
    
    Grid.BeginUpdate;
    try
      Grid.RowCount := L.Count;
      for i := 1 to L.Count-1 do begin  
        sa := L[i].Split(';');
        Grid.Cells[0, i] := sa[0];  // Country
        Grid.Cells[2, i] := sa[1];  // Full currency name
        Grid.Cells[3, i] := sa[2];  // Currency symbol
      end;
      Grid.Row := 1;
      Grid.Cells[1, 0] := 'Exchange rate' + LineEnding + '(1 USD = ...)';
    finally
      Grid.EndUpdate;
    end;
  finally
    L.Free;
  end;
end;

procedure TMainForm.SaveToIni;
var
  ini: TCustomIniFile;
  section: String;
  i: Integer;
begin
  ini := CreateIniFile;
  try
    ini.WriteString('Settings', 'App_ID', App_ID);

    section := 'Calculator';
    ini.WriteFloat(section, 'Amount', seSource.Value);
    ini.WriteString(section, 'SourceCurrency', cbSource.Text);
    ini.WriteString(section, 'DestinationCurrency', cbDest.Text);

    if FCurrenciesToIni then
    begin
      section := 'Exchange rates';
      for i := 1 to Grid.RowCount-1 do
        ini.WriteString(section, 
          Grid.Cells[0, i], 
          Grid.Cells[1, i] + ';' + Grid.Cells[2, i] + ';' + Grid.Cells[3, i]);
      FCurrenciesToIni := false;
    end;
  finally
    ini.Free;
  end;
end;  

procedure TMainForm.SymbolDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  symbol: String;
begin
  if (Source = Grid) then
  begin
    symbol := Grid.Cells[3, Grid.Row];
    if Sender = cbSource then
      cbSource.Text := symbol;
    if Sender = cbDest then
      cbDest.Text := symbol;
  end;
end;

procedure TMainForm.SymbolDragOver(Sender, Source: TObject; X, Y: Integer; 
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Grid) and ((Sender = cbSource) or (Sender = cbDest));
end;


end.

