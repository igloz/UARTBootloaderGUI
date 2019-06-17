unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, windows, SysUtils, registry, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ComPort, HexFileParser, UARTProgrammer;

type
  { TRegistryChangeWatcher }

  TRegistryChangeWatcher = class(TThread)
  public type
    TRegistryChangeCallback = procedure(hKey: HKEY) of object;
  private
    FProtect: TCriticalSection;
    FEvent: HANDLE;
    FKey: HKEY;
    FWatchSubtree: WINBOOL;
    FNotifyFilter: DWORD;
    FCallback: TRegistryChangeCallback;
    procedure ExecuteCallback;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    { BeginWatch takes ownership of hKey. Subsequent call to EndWatch will close it. }
    procedure BeginWatch(
      hKey: HKEY;
      bWatchSubtree: WINBOOL;
      dwNotifyFilter: DWORD;
      Callback: TRegistryChangeCallback);
    procedure EndWatch;
  end;

  { TAsyncProgrammer }

  TAsyncProgrammerState = (
    apsNotConnected,
    apsConnecting,
    apsConnected,
    apsProgramming,
    apsDisconnecting);

  TLogMessageOption = (
    lmoStartNewLine,
    lmoAppendToLastLine,
    lmoReplaceLastLine);

  TAsyncProgrammer = class(TThread)
  public type
    TStateChangedCallback = procedure of object;
    TProgressCallback = procedure(ProgressValue, MaxProgressValue: Cardinal) of object;
    TLogMessageCallback = procedure(const AMessage: String; AOption: TLogMessageOption) of object;
  private type
    TOperation = (opNone, opConnect, opDisconnect, opProgramFlash);
  private
    FParser: THexFileParser;
    FProgrammer: TUARTProgrammer;
    FState: TAsyncProgrammerState;
    FStateChangedCallback: TStateChangedCallback;
    FProgressCallback: TProgressCallback;
    FLogMessageCallback: TLogMessageCallback;
    FAProgressValue: Cardinal;
    FAMaxProgressValue: Cardinal;
    FAMessage: String;
    FAOption: TLogMessageOption;
    FEvent: HANDLE;
    FOperation: TOperation;
    FWorking: Boolean;
    FCanceling: Boolean;
    FHexFileName: String;
    function GetComPort: TComPort;
    function GetComPortDescription: String;
    procedure ExecuteStateChangedCallback;
    procedure ExecuteProgressCallback;
    procedure ExecuteLogMessageCallback;
    procedure SetState(AState: TAsyncProgrammerState);
    procedure Progress(ProgressValue, MaxProgressValue: Cardinal);
    procedure LogMessage(const AMessage: String; AOption: TLogMessageOption = lmoStartNewLine);
    procedure ProgrammerProgressCallback(ProgressValue: Cardinal; var Cancel: Boolean);
    procedure ConnectToBootloader;
    procedure DisconnectFromBootloader;
    procedure ProgramFlashMemory(const HexFileName: String);
    procedure BeginAsyncOperation(AOperation: TOperation);
    procedure ExecuteOperation;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ConnectToBootloaderAsync;
    procedure DisconnectFromBootloaderAsync;
    procedure ProgramFlashMemoryAsync(const HexFileName: String);
    procedure CancelAsyncOperation;
    property ComPort: TComPort read GetComPort;
    property Programmer: TUARTProgrammer read FProgrammer;
    property State: TAsyncProgrammerState read FState;
    property Canceling: Boolean read FCanceling;
    property StateChangedCallback: TStateChangedCallback write FStateChangedCallback;
    property ProgressCallback: TProgressCallback write FProgressCallback;
    property LogMessageCallback: TLogMessageCallback write FLogMessageCallback;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    DisconnectButton: TButton;
    ConnectButton: TButton;
    CommunicationLogMemo: TMemo;
    CommunicationLogLabel: TLabel;
    StopConnectingButton: TButton;
    ProgramFlashButton: TButton;
    CancelProgrammingButton: TButton;
    ProgressBar: TProgressBar;
    SendBeforeConnectEdit: TEdit;
    MsecLabel: TLabel;
    WaitAfterSendBeforeConnectEdit: TEdit;
    SendBeforeConnectCheckBox: TCheckBox;
    SelectHexFileButton: TButton;
    ComPortComboBox: TComboBox;
    BaudRateComboBox: TComboBox;
    HexFileEdit: TEdit;
    HexFileLabel: TLabel;
    SelectHexFileDialog: TOpenDialog;
    ParityComboBox: TComboBox;
    StopBitsComboBox: TComboBox;
    ComPortLabel: TLabel;
    BaudRateLabel: TLabel;
    ParityLabel: TLabel;
    StopBitsLabel: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BaudRateComboBoxEditingDone(Sender: TObject);
    procedure ComPortComboBoxSelect(Sender: TObject);
    procedure ParityComboBoxSelect(Sender: TObject);
    procedure StopBitsComboBoxSelect(Sender: TObject);
    procedure SendBeforeConnectCheckBoxClick(Sender: TObject);
    procedure SendBeforeConnectEditEditingDone(Sender: TObject);
    procedure WaitAfterSendBeforeConnectEditEditingDone(Sender: TObject);
    procedure HexFileEditEditingDone(Sender: TObject);
    procedure SelectHexFileButtonClick(Sender: TObject);
    procedure ConnectButtonClick(Sender: TObject);
    procedure StopConnectingButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure ProgramFlashButtonClick(Sender: TObject);
    procedure CancelProgrammingButtonClick(Sender: TObject);
  private
    FRegistryChangeWatcher: TRegistryChangeWatcher;
    FProgrammer: TAsyncProgrammer;
    procedure LoadConfiguration;
    procedure SaveConfiguration;
    procedure BeginWatchComPorts;
    procedure RefreshComPortList(Key: HKEY);
    procedure ReconfigureControls;
    procedure ProgrammerStateChanged;
    procedure ProgrammerProgress(ProgressValue, MaxProgressValue: Cardinal);
    procedure ProgrammerLogMessage(const AMessage: String; AOption: TLogMessageOption);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

const
  MIN_BAUDRATE = 600;
  MAX_BAUDRATE = 500000;

  MAX_PROGRESS = 1000;

  ConfigurationRegistryPath = '\Software\igloz\UARTBootloaderGUI\Configuration';
  RegistryMainFormTop = 'MainFormTop';
  RegistryMainFormLeft = 'MainFormLeft';
  RegistryPortNumber = 'PortNumber';
  RegistryBaudRate = 'BaudRate';
  RegistryParity = 'Parity';
  RegistryStopBits = 'StopBits';
  RegistryHexFile = 'HexFile';
  RegistrySendBeforeConnect = 'SendBeforeConnect';
  RegistrySendBeforeConnectString = 'SendBeforeConnectString';
  RegistryWaitAfterSendBeforeConnect = 'WaitAfterSendBeforeConnect';

procedure ErrorMessageBox(const AMessage: String); overload;
begin
  Application.MessageBox(PChar(AMessage), 'Error', MB_ICONERROR);
end;

procedure ErrorMessageBox(const AMessage: String; Args: Array of Const); overload;
begin
  ErrorMessageBox(Format(AMessage, Args));
end;

procedure MakeComboBoxNumbersOnly(ComboBox: TComboBox);
var
  Info: TComboBoxInfo;
begin
  Info.cbSize := SizeOf(Info);
  GetComboBoxInfo(ComboBox.Handle, @Info);
  SetWindowLong(Info.hwndItem, GWL_STYLE, GetWindowLong(Info.hwndItem, GWL_STYLE) or ES_NUMBER);
end;

{ TRegistryChangeWatcher }

procedure TRegistryChangeWatcher.ExecuteCallback;
begin
  if (FKey <> 0) and Assigned(FCallback) then
    FCallback(FKey);
end;

procedure TRegistryChangeWatcher.Execute;
var
  Event: HANDLE;
  Key: HKEY;
  WatchSubtree: WINBOOL;
  NotifyFilter: DWORD;
begin
  while FEvent <> 0 do
  begin
    if (FKey <> 0) and Assigned(FCallback) then
      Synchronize(@ExecuteCallback);

    EnterCriticalSection(FProtect);
    Key := FKey;
    WatchSubtree := FWatchSubtree;
    NotifyFilter := FNotifyFilter;
    LeaveCriticalSection(FProtect);

    Event := FEvent;
    if (Key <> 0) and (Event <> 0) then
      RegNotifyChangeKeyValue(Key, WatchSubtree, NotifyFilter, Event, True);

    Event := FEvent;
    if (Event <> 0) then
      WaitForSingleObject(Event, INFINITE);
  end;
end;

constructor TRegistryChangeWatcher.Create;
begin
  InitializeCriticalSection(FProtect);
  FEvent := CreateEvent(nil, False, False, nil);
  FKey := 0;
  FCallback := nil;
  inherited Create(False);
end;

destructor TRegistryChangeWatcher.Destroy;
var
  Event: HANDLE;
begin
  EndWatch;
  Event := FEvent;
  if Event <> 0 then
  begin
    FEvent := 0;
    SetEvent(Event);
    WaitFor;
    CloseHandle(Event);
  end;
  DeleteCriticalSection(FProtect);
  inherited Destroy;
end;

procedure TRegistryChangeWatcher.BeginWatch(
  hKey: HKEY;
  bWatchSubtree: WINBOOL;
  dwNotifyFilter: DWORD;
  Callback: TRegistryChangeCallback);
begin
  EndWatch;
  if FEvent <> 0 then
  begin
    EnterCriticalSection(FProtect);
    FKey := hKey;
    FWatchSubtree := bWatchSubtree;
    FNotifyFilter := dwNotifyFilter;
    FCallback := Callback;
    LeaveCriticalSection(FProtect);
    SetEvent(FEvent);
  end;
end;

procedure TRegistryChangeWatcher.EndWatch;
var
  Key: HKEY;
begin
  if FEvent <> 0 then
  begin
    EnterCriticalSection(FProtect);
    Key := FKey;
    FKey := 0;
    FCallback := nil;
    LeaveCriticalSection(FProtect);
    if Key <> 0 then
      RegCloseKey(Key);
  end;
end;

{ TAsyncProgrammer }

function TAsyncProgrammer.GetComPort: TComPort;
begin
  Result := FProgrammer.ComPort;
end;

function TAsyncProgrammer.GetComPortDescription: String;
const
  ParityStrings: Array[TParity] of String = ('N', 'O', 'E', 'M', 'S');
  StopBitsStrings: Array[TStopBits] of String = ('1', '1.5', '2');
begin
  with ComPort do
    Result := Format('COM%d with %u baud %u%s%s', [PortNumber, BaudRate, ByteSize, ParityStrings[Parity], StopBitsStrings[StopBits]]);
end;

procedure TAsyncProgrammer.ExecuteStateChangedCallback;
begin
  if Assigned(FStateChangedCallback) then
    FStateChangedCallback;
end;

procedure TAsyncProgrammer.ExecuteProgressCallback;
begin
  if Assigned(FProgressCallback) then
    FProgressCallback(FAProgressValue, FAMaxProgressValue);
end;

procedure TAsyncProgrammer.ExecuteLogMessageCallback;
begin
  if Assigned(FLogMessageCallback) then
    FLogMessageCallback(FAMessage, FAOption);
end;

procedure TAsyncProgrammer.SetState(AState: TAsyncProgrammerState);
begin
  FState := AState;
  FCanceling := False;
  if Assigned(FStateChangedCallback) then
    Synchronize(@ExecuteStateChangedCallback);
end;

procedure TAsyncProgrammer.Progress(ProgressValue, MaxProgressValue: Cardinal);
begin
  if Assigned(FProgressCallback) then
  begin
    FAProgressValue := ProgressValue;
    FAMaxProgressValue := MaxProgressValue;
    Synchronize(@ExecuteProgressCallback);
  end;
end;

procedure TAsyncProgrammer.LogMessage(const AMessage: String; AOption: TLogMessageOption);
begin
  if Assigned(FLogMessageCallback) then
  begin
    FAMessage := AMessage;
    FAOption := AOption;
    Synchronize(@ExecuteLogMessageCallback);
  end;
end;

procedure TAsyncProgrammer.ProgrammerProgressCallback(ProgressValue: Cardinal; var Cancel: Boolean);
begin
  Progress(ProgressValue, MAX_PROGRESS);
  Cancel := FCanceling;
end;

procedure TAsyncProgrammer.ConnectToBootloader;
begin
  SetState(apsConnecting);
  LogMessage(Format('Opening port %s...', [GetComPortDescription]));
  with ComPort do
  begin
    Open;
    FlushBuffers;
    LogMessage(' Port opened.', lmoAppendToLastLine);
  end;
  LogMessage(Format('Connecting to bootloader at %s...', [GetComPortDescription]));
  with Programmer do
  begin
    ConnectToBootloader;
    if Connected then
    begin
      LogMessage(' Connected.', lmoAppendToLastLine);
      LogMessage(Format('Hardware id: %s', [HardwareId]));
      LogMessage(Format('Page size: %u bytes', [PageSize]));
      LogMessage(Format('Number of pages available for programming: %u', [PageCount]));
    end;
  end;
end;

procedure TAsyncProgrammer.DisconnectFromBootloader;
begin
  SetState(apsDisconnecting);
  LogMessage('Disconnecting from bootloader...');
  with Programmer do
  begin
    DisconnectFromBootloader;
    if not Connected then
      LogMessage(' Disconnected.', lmoAppendToLastLine);
  end;
end;

procedure TAsyncProgrammer.ProgramFlashMemory(const HexFileName: String);
var
  MemoryContent: TMemoryContent;
begin
  SetState(apsProgramming);
  LogMessage(Format('Parsing hexfile %s', [HexFileName]));
  MemoryContent := FParser.Parse(HexFileName);
  LogMessage('Programming flash memory...');
  Programmer.ProgramFlashMemory(MemoryContent);
  LogMessage(' Success!', lmoAppendToLastLine);
end;

procedure TAsyncProgrammer.BeginAsyncOperation(AOperation: TOperation);
var
  Event: HANDLE;
begin
  Event := FEvent;
  if Event <> 0 then
  begin
    FOperation := AOperation;
    SetEvent(Event);
  end;
end;

procedure TAsyncProgrammer.ExecuteOperation;
var
  Operation: TOperation;
begin
  Operation := FOperation;
  FOperation := opNone;

  try
    case Operation of
    opConnect:
      ConnectToBootloader;
    opDisconnect:
      DisconnectFromBootloader;
    opProgramFlash:
      ProgramFlashMemory(FHexFileName);
    end;
  except
    on E: Exception do
    begin
      LogMessage(E.Message);
      Progress(0, MAX_PROGRESS); { Reset progress bar on error }
    end;
  end;

  if Programmer.Connected then
    SetState(apsConnected)
  else
  begin
    ComPort.Close;
    SetState(apsNotConnected);
  end;
end;

procedure TAsyncProgrammer.Execute;
var
  Event: HANDLE;
begin
  while FEvent <> 0 do
  begin
    FWorking := True;

    try
      ExecuteOperation;
    except
      LogMessage('Unhandled exception');
    end;

    FWorking := False;
    Event := FEvent;
    if (Event <> 0) then
      WaitForSingleObject(Event, INFINITE);
  end;
end;

constructor TAsyncProgrammer.Create;
begin
  FParser := THexFileParser.Create;
  FProgrammer := TUARTProgrammer.Create;
  FProgrammer.SetProgressCallback(@ProgrammerProgressCallback, MAX_PROGRESS);
  FState := apsNotConnected;
  FStateChangedCallback := nil;
  FProgressCallback := nil;
  FLogMessageCallback := nil;
  FEvent := CreateEvent(nil, False, False, nil);
  FOperation := opNone;
  FWorking := False;
  FCanceling := False;
  inherited Create(False);
end;

destructor TAsyncProgrammer.Destroy;
var
  Event: HANDLE;
begin
  Event := FEvent;
  if Event <> 0 then
  begin
    FEvent := 0;
    FCanceling := True;
    SetEvent(Event);
    WaitFor;
    CloseHandle(Event);
  end;
  FreeAndNil(FProgrammer);
  FreeAndNil(FParser);
  inherited Destroy;
end;

procedure TAsyncProgrammer.ConnectToBootloaderAsync;
begin
  if not FWorking then
    BeginAsyncOperation(opConnect);
end;

procedure TAsyncProgrammer.DisconnectFromBootloaderAsync;
begin
  if not FWorking then
    BeginAsyncOperation(opDisconnect);
end;

procedure TAsyncProgrammer.ProgramFlashMemoryAsync(const HexFileName: String);
begin
  if not FWorking then
  begin
    FHexFileName := HexFileName;
    BeginAsyncOperation(opProgramFlash);
  end;
end;

procedure TAsyncProgrammer.CancelAsyncOperation;
begin
  if FWorking then
  begin
    FCanceling := True;
    ExecuteStateChangedCallback;
  end;
end;

{ TMainForm }

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := CloseAction;
  FRegistryChangeWatcher.EndWatch;
  SaveConfiguration;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FRegistryChangeWatcher := TRegistryChangeWatcher.Create;
  FProgrammer := TAsyncProgrammer.Create;
  with FProgrammer do
  begin
    StateChangedCallback := @ProgrammerStateChanged;
    ProgressCallback := @ProgrammerProgress;
    LogMessageCallback := @ProgrammerLogMessage;
  end;
  with FProgrammer.ComPort do
  begin
    PortNumber := -1;
    BaudRate := 9600;
    ByteSize := 8;
    Parity := NOPARITY;
    StopBits := ONESTOPBIT;
    ReadIntervalTimeout := 50;
    ReadTotalTimeoutMultiplier := 0;
    ReadTotalTimeoutConstant := 500;
    WriteTotalTimeoutMultiplier := 0;
    WriteTotalTimeoutConstant := 0;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProgrammer);
  FreeAndNil(FRegistryChangeWatcher);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LoadConfiguration;
  BeginWatchComPorts;
  MakeComboBoxNumbersOnly(BaudRateComboBox);
  with FProgrammer.ComPort do
  begin
    BaudRateComboBox.Text := IntToStr(BaudRate);
    with ParityComboBox.Items do
    begin
      Clear;
      AddObject('None', TObject(NOPARITY));
      AddObject('Odd', TObject(ODDPARITY));
      AddObject('Even', TObject(EVENPARITY));
      ParityComboBox.ItemIndex := IndexOfObject(TObject(PtrInt(Parity)));
    end;
    with StopBitsComboBox.Items do
    begin
      Clear;
      AddObject('1', TObject(ONESTOPBIT));
      AddObject('2', TObject(TWOSTOPBITS));
      StopBitsComboBox.ItemIndex := IndexOfObject(TObject(PtrInt(StopBits)));
    end;
  end;
  with FProgrammer.Programmer do
  begin
    SendBeforeConnectCheckBox.Checked := SendBeforeConnect;
    WaitAfterSendBeforeConnectEdit.Text := IntToStr(WaitAfterSendBeforeConnect);
    SendBeforeConnectEdit.Text := SendBeforeConnectString;
  end;
  ReconfigureControls;
end;

procedure TMainForm.BaudRateComboBoxEditingDone(Sender: TObject);
var
  BaudRate: Cardinal;
begin
  BaudRate := StrToIntDef(BaudRateComboBox.Text, 0);
  if (BaudRate < MIN_BAUDRATE) or (BaudRate > MAX_BAUDRATE) then
  begin
    ErrorMessageBox('Invalid Baudrate. Should be %d..%d', [MIN_BAUDRATE, MAX_BAUDRATE]);
    ActiveControl := BaudRateComboBox;
  end
  else
  begin
    FProgrammer.ComPort.BaudRate := BaudRate;
    ReconfigureControls;
  end;
end;

procedure TMainForm.ComPortComboBoxSelect(Sender: TObject);
begin
  with ComPortComboBox do
    FProgrammer.ComPort.PortNumber := PtrInt(Items.Objects[ItemIndex]);
  ReconfigureControls;
end;

procedure TMainForm.ParityComboBoxSelect(Sender: TObject);
begin
  with ParityComboBox do
    FProgrammer.ComPort.Parity := PtrInt(Items.Objects[ItemIndex]);
  ReconfigureControls;
end;

procedure TMainForm.StopBitsComboBoxSelect(Sender: TObject);
begin
  with StopBitsComboBox do
    FProgrammer.ComPort.StopBits := PtrInt(Items.Objects[ItemIndex]);
  ReconfigureControls;
end;

procedure TMainForm.SendBeforeConnectCheckBoxClick(Sender: TObject);
begin
  FProgrammer.Programmer.SendBeforeConnect := SendBeforeConnectCheckBox.Checked;
  ReconfigureControls;
end;

procedure TMainForm.SendBeforeConnectEditEditingDone(Sender: TObject);
begin
  FProgrammer.Programmer.SendBeforeConnectString := SendBeforeConnectEdit.Text;
end;

procedure TMainForm.WaitAfterSendBeforeConnectEditEditingDone(Sender: TObject);
var
  WaitInterval: Integer;
begin
  WaitInterval := StrToIntDef(WaitAfterSendBeforeConnectEdit.Text, -1);
  if WaitInterval < 0 then
  begin
    ErrorMessageBox('Invalid wait interval');
    ActiveControl := WaitAfterSendBeforeConnectEdit;
  end
  else
  begin
    FProgrammer.Programmer.WaitAfterSendBeforeConnect := WaitInterval;
    ReconfigureControls;
  end;
end;

procedure TMainForm.HexFileEditEditingDone(Sender: TObject);
begin
  ReconfigureControls;
end;

procedure TMainForm.SelectHexFileButtonClick(Sender: TObject);
begin
  with SelectHexFileDialog do
  begin
    FileName := HexFileEdit.Text;
    if Execute then
    begin
      HexFileEdit.Text := FileName;
      ReconfigureControls;
    end;
  end;
end;

procedure TMainForm.ConnectButtonClick(Sender: TObject);
begin
  FProgrammer.ConnectToBootloaderAsync;
end;

procedure TMainForm.StopConnectingButtonClick(Sender: TObject);
begin
  FProgrammer.CancelAsyncOperation;
end;

procedure TMainForm.DisconnectButtonClick(Sender: TObject);
begin
  FProgrammer.DisconnectFromBootloaderAsync;
end;

procedure TMainForm.ProgramFlashButtonClick(Sender: TObject);
begin
  FProgrammer.ProgramFlashMemoryAsync(HexFileEdit.Text);
end;

procedure TMainForm.CancelProgrammingButtonClick(Sender: TObject);
begin
  FProgrammer.CancelAsyncOperation;
end;

procedure TMainForm.LoadConfiguration;
  function AdjustCoordinate(ACoordinate, ASize, ScreenSize: Integer): Integer;
  begin
    Dec(ScreenSize, ASize);
    if ACoordinate > ScreenSize then
      ACoordinate := ScreenSize;
    if ACoordinate < 0 then
      ACoordinate := 0;
    Result := ACoordinate;
  end;

  function AdjustPortNumber(APortNumber: Integer): Integer;
  begin
    if (APortNumber > 0) and (APortNumber < 256) then
      Result := APortNumber
    else
      Result := -1;
  end;

  function AdjustBaudRate(ABaudRate: Cardinal): Cardinal;
  begin
    if (ABaudRate >= MIN_BAUDRATE) and (ABaudRate <= MAX_BAUDRATE) then
      Result := ABaudRate
    else
      Result := 9600;
  end;

begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(ConfigurationRegistryPath, False) then
      begin
        if ValueExists(RegistryMainFormTop) then
          Top := AdjustCoordinate(ReadInteger(RegistryMainFormTop), Height, Screen.Height);
        if ValueExists(RegistryMainFormLeft) then
          Left := AdjustCoordinate(ReadInteger(RegistryMainFormLeft), Width, Screen.Width);
        with FProgrammer.ComPort do
        begin
          if ValueExists(RegistryPortNumber) then
            PortNumber := AdjustPortNumber(ReadInteger(RegistryPortNumber));
          if ValueExists(RegistryBaudRate) then
            BaudRate := AdjustBaudRate(ReadInteger(RegistryBaudRate));
          if ValueExists(RegistryParity) then
            try Parity := ReadInteger(RegistryParity); except end;
          if ValueExists(RegistryParity) then
            try StopBits := ReadInteger(RegistryStopBits); except end;
        end;
        with FProgrammer.Programmer do
        begin
          if ValueExists(RegistrySendBeforeConnect) then
            SendBeforeConnect := ReadBool(RegistrySendBeforeConnect);
          if ValueExists(RegistrySendBeforeConnectString) then
            SendBeforeConnectString := ReadString(RegistrySendBeforeConnectString);
          if ValueExists(RegistryWaitAfterSendBeforeConnect) then
            WaitAfterSendBeforeConnect := ReadInteger(RegistryWaitAfterSendBeforeConnect);
        end;
        if ValueExists(RegistryHexFile) then
          HexFileEdit.Text := ReadString(RegistryHexFile);
      end;
    finally
      Free;
    end;
end;

procedure TMainForm.SaveConfiguration;
begin
  with TRegistry.Create do
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKey(ConfigurationRegistryPath, True) then
      begin
        WriteInteger(RegistryMainFormTop, Top);
        WriteInteger(RegistryMainFormLeft, Left);
        with FProgrammer.ComPort do
        begin
          WriteInteger(RegistryPortNumber, PortNumber);
          WriteInteger(RegistryBaudRate, BaudRate);
          WriteInteger(RegistryParity, Parity);
          WriteInteger(RegistryStopBits, StopBits);
        end;
        with FProgrammer.Programmer do
        begin
          WriteBool(RegistrySendBeforeConnect, SendBeforeConnect);
          WriteString(RegistrySendBeforeConnectString, SendBeforeConnectString);
          WriteInteger(RegistryWaitAfterSendBeforeConnect, WaitAfterSendBeforeConnect);
        end;
        WriteString(RegistryHexFile, HexFileEdit.Text);
      end;
    finally
      Free;
    end;
end;

procedure TMainForm.BeginWatchComPorts;
var
  Key: HKEY;
  Status: Cardinal;
begin
  Key := 0;
  Status := RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'HARDWARE\DEVICEMAP\SERIALCOMM', 0, KEY_READ, Key);
  if Status = ERROR_SUCCESS then
    FRegistryChangeWatcher.BeginWatch(Key, False, REG_NOTIFY_CHANGE_NAME or REG_NOTIFY_CHANGE_LAST_SET, @RefreshComPortList);
end;

procedure TMainForm.RefreshComPortList(Key: HKEY);
var
  SelectedComPort, SelectedItemIndex, I, ItemIndex: Integer;
  ComPortDescriptions: Array of String;
  ComPortDescription: String;

  procedure EnumComPorts;
  var
    NameBuffer: Array[0..255] of Char;
    ValueBuffer: Array[0..16] of Char;
    NameBufferLength, ValueBufferLength, ValueType, Index, Status: Cardinal;
    ComPortNumber, I: Integer;
    C: Char;
  begin
    Index := 0;
    repeat
      NameBufferLength := Length(NameBuffer);
      ValueBufferLength := Length(ValueBuffer);
      Status := RegEnumValue(Key, Index, @NameBuffer, @NameBufferLength, nil, @ValueType, @ValueBuffer, @ValueBufferLength);
      if Status <> ERROR_SUCCESS then
        Break;

      Inc(Index);
      if (ValueType = REG_SZ)
      and (ValueBufferLength >= 4)
      and (ValueBuffer[0] = 'C')
      and (ValueBuffer[1] = 'O')
      and (ValueBuffer[2] = 'M') then
      begin
        ComPortNumber := 0;
        for I := 3 to Pred(ValueBufferLength) do
        begin
          C := ValueBuffer[I];
          if not (C in ['0'..'9']) then
            Break;

          ComPortNumber := ComPortNumber * 10 + (Ord(C) - Ord('0'));
        end;

        if ComPortNumber < 256 then
        begin
          if ComPortNumber >= Length(ComPortDescriptions) then
            SetLength(ComPortDescriptions, Succ(ComPortNumber));
          SetLength(ComPortDescriptions[ComPortNumber], NameBufferLength);
          if NameBufferLength > 0 then
            Move(NameBuffer, ComPortDescriptions[ComPortNumber][1], NameBufferLength);
        end;
      end;
    until False;
  end;

begin
  SelectedComPort := FProgrammer.ComPort.PortNumber;
  if SelectedComPort > 0 then
  begin
    SetLength(ComPortDescriptions, Succ(SelectedComPort));
    ComPortDescriptions[SelectedComPort] := 'not connected';
  end
  else
    SetLength(ComPortDescriptions, 0);

  EnumComPorts;

  with ComPortComboBox.Items do
    try
      BeginUpdate;
      Clear;
      SelectedItemIndex := 0;
      for I := 1 to Pred(Length(ComPortDescriptions)) do
      begin
        ComPortDescription := ComPortDescriptions[I];
        if ComPortDescription <> '' then
        begin
          ItemIndex := AddObject('COM%d: %s', [I, ComPortDescription], TObject(PtrInt(I)));
          if I = SelectedComPort then
            SelectedItemIndex := ItemIndex;
        end;
      end;
      if SelectedItemIndex < Count then
      begin
        ComPortComboBox.ItemIndex := SelectedItemIndex;
        SelectedComPort := PtrInt(Objects[SelectedItemIndex]);
      end
      else
        SelectedComPort := -1;
    finally
      EndUpdate;
    end;

  FProgrammer.ComPort.PortNumber := SelectedComPort;
  ReconfigureControls;
end;

procedure TMainForm.ReconfigureControls;
var
  StateNotConnected, StateConnecting, StateConnected, StateNotProgramming, StateNotCanceling: Boolean;
  PreConnectControlsEnabled, ConnectButtonEnabled, ProgramFlashButtonEnabled: Boolean;
  TheActiveControl: TWinControl;

  function IsPreConnectControlsEnabled: Boolean;
  begin
    Result := False;
    if StateNotConnected then
      if SendBeforeConnectCheckBox.Checked then
        Result := True;
  end;

  function IsConnectButtonEnabled: Boolean;
  var
    Buffer: Array[0..16] of Char;
    SelectedComPort: Integer;
    Status, ErrorCode, BaudRate: Cardinal;
  begin
    Result := StateNotConnected;
    if Result then
    begin
      with ComPortComboBox do
        if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
          Result := False
        else
        begin
          SelectedComPort := PtrInt(Items.Objects[ItemIndex]);
          Status := QueryDosDevice(PChar(Format('COM%d', [SelectedComPort])), Buffer, Pred(Length(Buffer)));
          ErrorCode := GetLastError;
          if (Status = 0) and (ErrorCode <> ERROR_INSUFFICIENT_BUFFER) then
            Result := False;
        end;

      if Result then
      begin
        with BaudRateComboBox do
        begin
          BaudRate := StrToIntDef(Text, 0);
          if (BaudRate < MIN_BAUDRATE) or (BaudRate > MAX_BAUDRATE) then
            Result := False;
        end;

        if Result then
        begin
          with ParityComboBox do
            if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
              Result := False;

          if Result then
          begin
            with StopBitsComboBox do
              if (ItemIndex < 0) or (ItemIndex >= Items.Count) then
                Result := False;

            if Result then
              if SendBeforeConnectCheckBox.Checked then
                if StrToIntDef(WaitAfterSendBeforeConnectEdit.Text, -1) < 0 then
                  Result := False;
          end;
        end;
      end;
    end;
  end;

  function IsProgramFlashButtonEnabled: Boolean;
  begin
    Result := False;
    if StateConnected then
      if FileExists(HexFileEdit.Text) then
        Result := True;
  end;

begin
  with FProgrammer do
  begin
    StateNotConnected := (State = apsNotConnected);
    StateConnecting := (State = apsConnecting);
    StateConnected := (State = apsConnected);
    StateNotProgramming := (State <> apsProgramming);
    StateNotCanceling := not Canceling;
  end;

  PreConnectControlsEnabled := IsPreConnectControlsEnabled;
  ConnectButtonEnabled := IsConnectButtonEnabled;
  ProgramFlashButtonEnabled := IsProgramFlashButtonEnabled;

  TheActiveControl := ActiveControl;

  ComPortLabel.Enabled := StateNotConnected;
  ComPortComboBox.Enabled := StateNotConnected;
  BaudRateLabel.Enabled := StateNotConnected;
  BaudRateComboBox.Enabled := StateNotConnected;
  ParityLabel.Enabled := StateNotConnected;
  ParityComboBox.Enabled := StateNotConnected;
  StopBitsLabel.Enabled := StateNotConnected;
  StopBitsComboBox.Enabled := StateNotConnected;
  SendBeforeConnectCheckBox.Enabled := StateNotConnected;
  MsecLabel.Enabled := PreConnectControlsEnabled;
  SendBeforeConnectEdit.Enabled := PreConnectControlsEnabled;
  WaitAfterSendBeforeConnectEdit.Enabled := PreConnectControlsEnabled;
  HexFileLabel.Enabled := StateNotProgramming;
  HexFileEdit.Enabled := StateNotProgramming;
  SelectHexFileButton.Enabled := StateNotProgramming;
  ConnectButton.Enabled := ConnectButtonEnabled;
  StopConnectingButton.Enabled := StateNotCanceling;
  DisconnectButton.Enabled := StateConnected;
  ProgramFlashButton.Enabled := ProgramFlashButtonEnabled;
  CancelProgrammingButton.Enabled := StateNotCanceling;

  ConnectButton.Visible := StateNotConnected;
  StopConnectingButton.Visible := StateConnecting;
  DisconnectButton.Visible := not (StateNotConnected or StateConnecting);
  ProgramFlashButton.Visible := StateNotProgramming;
  CancelProgrammingButton.Visible := not StateNotProgramming;

  if Assigned(TheActiveControl) then
    with TheActiveControl do
      if not (Visible and Enabled) then
        if StateNotConnected then
          ActiveControl := ComPortComboBox
        else if StateNotProgramming then
          ActiveControl := SelectHexFileButton
        else if StateNotCanceling then
          ActiveControl := CancelProgrammingButton;
end;

procedure TMainForm.ProgrammerStateChanged;
begin
  if FProgrammer <> nil then
    ReconfigureControls;
end;

procedure TMainForm.ProgrammerProgress(ProgressValue, MaxProgressValue: Cardinal);
begin
  if FProgrammer <> nil then
    with ProgressBar do
    begin
      Max := MaxProgressValue;
      Position := ProgressValue;
    end;
end;

procedure TMainForm.ProgrammerLogMessage(const AMessage: String; AOption: TLogMessageOption);
begin
  if FProgrammer <> nil then
    with CommunicationLogMemo.Lines do
      if Count <> 0 then
        case AOption of
          lmoStartNewLine:
            Append(AMessage);
          lmoAppendToLastLine:
            Strings[Pred(Count)] := Strings[Pred(Count)] + AMessage;
          lmoReplaceLastLine:
            Strings[Pred(Count)] := AMessage;
        end {case}
      else
        Append(AMessage)
end;

end.

