unit ComPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, windows, SysUtils;

type
  TByteSize = 5..8;
  TParity = NOPARITY..SPACEPARITY;
  TStopBits = ONESTOPBIT..TWOSTOPBITS;

  { EComPortError }

  EComPortError = class(EInOutError)
  public
    constructor Create(const AMsg: String; AErrorCode: Integer);
    constructor CreateFmt(const AMsg: String; const Args: Array of Const; AErrorCode: Integer);
  end;

  { TComPort }

  TComPort = class
  private
    FDCB: DCB;
    FDCBChanged: Boolean;
    FTimeouts: COMMTIMEOUTS;
    FTimeoutsChanged: Boolean;
    FPortNumber: Integer;
    FHandle: HANDLE;

    function GetBaudRate: Cardinal;
    procedure SetBaudRate(ABaudRate: Cardinal);
    function GetByteSize: TByteSize;
    procedure SetByteSize(AByteSize: TByteSize);
    function GetParity: TParity;
    procedure SetParity(AParity: TParity);
    function GetStopBits: TStopBits;
    procedure SetStopBits(AStopBits: TStopBits);

    procedure SetReadIntervalTimeout(AReadIntervalTimeout: Cardinal);
    procedure SetReadTotalTimeoutMultiplier(AReadTotalTimeoutMultiplier: Cardinal);
    procedure SetReadTotalTimeoutConstant(AReadTotalTimeoutConstant: Cardinal);
    procedure SetWriteTotalTimeoutMultiplier(AWriteTotalTimeoutMultiplier: Cardinal);
    procedure SetWriteTotalTimeoutConstant(AWriteTotalTimeoutConstant: Cardinal);

    procedure RaiseIfNotOpen;
    procedure RaiseLastError;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function IsOpen: Boolean;
    procedure Reconfigure;
    procedure Flush(Flags: Cardinal);
    procedure FlushInput;
    procedure FlushOutput;
    procedure FlushBuffers;
    function Read(var Buffer; BytesToRead: Cardinal): Cardinal;
    function Write(const Data; BytesToWrite: Cardinal): Cardinal;

    property PortNumber: Integer read FPortNumber write FPortNumber;
    property BaudRate: Cardinal read GetBaudRate write SetBaudRate;
    property ByteSize: TByteSize read GetByteSize write SetByteSize;
    property Parity: TParity read GetParity write SetParity;
    property StopBits: TStopBits read GetStopBits write SetStopBits;

    property ReadIntervalTimeout: Cardinal read FTimeouts.ReadIntervalTimeout write SetReadIntervalTimeout;
    property ReadTotalTimeoutMultiplier: Cardinal read FTimeouts.ReadTotalTimeoutMultiplier write SetReadTotalTimeoutMultiplier;
    property ReadTotalTimeoutConstant: Cardinal read FTimeouts.ReadTotalTimeoutConstant write SetReadTotalTimeoutConstant;
    property WriteTotalTimeoutMultiplier: Cardinal read FTimeouts.WriteTotalTimeoutMultiplier write SetWriteTotalTimeoutMultiplier;
    property WriteTotalTimeoutConstant: Cardinal read FTimeouts.WriteTotalTimeoutConstant write SetWriteTotalTimeoutConstant;
  end;

implementation

{ EComPortError }

constructor EComPortError.Create(const AMsg: String; AErrorCode: Integer);
begin
  inherited Create(AMsg);
  ErrorCode := AErrorCode;
end;

constructor EComPortError.CreateFmt(const AMsg: String; const Args: Array of Const; AErrorCode: Integer);
begin
  inherited CreateFmt(AMsg, Args);
  ErrorCode := AErrorCode;
end;

{ TComPort }

function TComPort.GetBaudRate: Cardinal;
begin
  Result := FDCB.BaudRate;
end;

procedure TComPort.SetBaudRate(ABaudRate: Cardinal);
begin
  if FDCB.BaudRate <> ABaudRate then
  begin
    FDCB.BaudRate := ABaudRate;
    FDCBChanged := True;
  end;
end;

function TComPort.GetByteSize: TByteSize;
begin
  Result := FDCB.ByteSize;
end;

procedure TComPort.SetByteSize(AByteSize: TByteSize);
begin
  if (Integer(AByteSize) < Low(TByteSize)) or (Integer(AByteSize) > High(TByteSize)) then
    raise EComPortError.CreateFmt('ByteSize value %d is out of range', [AByteSize], ERROR_INVALID_PARAMETER);

  if FDCB.ByteSize <> AByteSize then
  begin
    if AByteSize = 5 then
    begin
      if FDCB.StopBits = TWOSTOPBITS then
        FDCB.StopBits := ONE5STOPBITS;
    end
    else
    begin
      if FDCB.StopBits = ONE5STOPBITS then
        FDCB.StopBits := TWOSTOPBITS;
    end;
    FDCB.ByteSize := AByteSize;
    FDCBChanged := True;
  end;
end;

function TComPort.GetParity: TParity;
begin
  Result := FDCB.Parity;
end;

procedure TComPort.SetParity(AParity: TParity);
begin
  if (Integer(AParity) < Low(TParity)) or (Integer(AParity) > High(TParity)) then
    raise EComPortError.CreateFmt('Parity value %d is out of range', [AParity], ERROR_INVALID_PARAMETER);

  if FDCB.Parity <> AParity then
  begin
    if AParity <> NOPARITY then
      FDCB.flags := FDCB.flags or $0002 { Set fParity flag }
    else
      FDCB.flags := FDCB.flags and not $0002; { Clear fParity flag }
    FDCB.Parity := AParity;
    FDCBChanged := True;
  end;
end;

function TComPort.GetStopBits: TStopBits;
begin
  Result := FDCB.StopBits;
end;

procedure TComPort.SetStopBits(AStopBits: TStopBits);
begin
  if (Integer(AStopBits) < Low(TStopBits)) or (Integer(AStopBits) > High(TStopBits)) then
    raise EComPortError.CreateFmt('StopBits value %d is out of range', [AStopBits], ERROR_INVALID_PARAMETER);

  if FDCB.StopBits <> AStopBits then
  begin
    if ((FDCB.ByteSize = 5) and (AStopBits = TWOSTOPBITS))
    or ((FDCB.ByteSize <> 5) and (AStopBits = ONE5STOPBITS)) then
      raise EComPortError.Create('Invalid ByteSize and StopBits combination', ERROR_INVALID_PARAMETER);

    FDCB.StopBits := AStopBits;
    FDCBChanged := True;
  end;
end;

procedure TComPort.SetReadIntervalTimeout(AReadIntervalTimeout: Cardinal);
begin
  if FTimeouts.ReadIntervalTimeout <> AReadIntervalTimeout then
  begin
    FTimeouts.ReadIntervalTimeout := AReadIntervalTimeout;
    FTimeoutsChanged := True;
  end;
end;

procedure TComPort.SetReadTotalTimeoutMultiplier(AReadTotalTimeoutMultiplier: Cardinal);
begin
  if FTimeouts.ReadTotalTimeoutMultiplier <> AReadTotalTimeoutMultiplier then
  begin
    FTimeouts.ReadTotalTimeoutMultiplier := AReadTotalTimeoutMultiplier;
    FTimeoutsChanged := True;
  end;
end;

procedure TComPort.SetReadTotalTimeoutConstant(AReadTotalTimeoutConstant: Cardinal);
begin
  if FTimeouts.ReadTotalTimeoutConstant <> AReadTotalTimeoutConstant then
  begin
    FTimeouts.ReadTotalTimeoutConstant := AReadTotalTimeoutConstant;
    FTimeoutsChanged := True;
  end;
end;

procedure TComPort.SetWriteTotalTimeoutMultiplier(AWriteTotalTimeoutMultiplier: Cardinal);
begin
  if FTimeouts.WriteTotalTimeoutMultiplier <> AWriteTotalTimeoutMultiplier then
  begin
    FTimeouts.WriteTotalTimeoutMultiplier := AWriteTotalTimeoutMultiplier;
    FTimeoutsChanged := True;
  end;
end;

procedure TComPort.SetWriteTotalTimeoutConstant(AWriteTotalTimeoutConstant: Cardinal);
begin
  if FTimeouts.WriteTotalTimeoutConstant <> AWriteTotalTimeoutConstant then
  begin
    FTimeouts.WriteTotalTimeoutConstant := AWriteTotalTimeoutConstant;
    FTimeoutsChanged := True;
  end;
end;

procedure TComPort.RaiseIfNotOpen; noreturn;
begin
  if FHandle = 0 then
    raise EComPortError.Create('Port is not open', ERROR_INVALID_HANDLE);
end;

procedure TComPort.RaiseLastError; noreturn;
var
  LastError: DWORD;
begin
  LastError := GetLastError;
  raise EComPortError.Create(SysErrorMessage(LastError), LastError);
end;

constructor TComPort.Create;
begin
  FillChar(FDCB, SizeOf(FDCB), 0);
  FDCB.DCBlength := SizeOf(FDCB);
  FDCB.BaudRate := CBR_9600;
  FDCB.flags := $1011; { RTS_CONTROL_ENABLE | DTR_CONTROL_ENABLE | fBinary }
  FDCB.ByteSize := 8;
  FDCB.Parity := NOPARITY;
  FDCB.StopBits := ONESTOPBIT;
  FDCBChanged := True;

  FillChar(FTimeouts, SizeOf(FTimeouts), 0);
  FTimeouts.ReadIntervalTimeout := 500;
  FTimeouts.ReadTotalTimeoutConstant := 5000;
  FTimeoutsChanged := True;

  FPortNumber := 1;
  FHandle := 0;
end;

destructor TComPort.Destroy;
begin
  Close;
end;

procedure TComPort.Open;
begin
  Close;
  FHandle := CreateFile(
    LPCSTR(Format('\\.\COM%d', [FPortNumber])),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0);
  if FHandle = INVALID_HANDLE_VALUE then
  begin
    FHandle := 0;
    RaiseLastError;
  end;

  FDCBChanged := True;
  FTimeoutsChanged := True;
  try
    Reconfigure;
  except
    Close;
    raise;
  end;
end;

procedure TComPort.Close;
begin
  if FHandle <> 0 then
  begin
    CloseHandle(FHandle);
    FHandle := 0;
  end;
end;

function TComPort.IsOpen: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TComPort.Reconfigure;
begin
  if FHandle <> 0 then
  begin
    if FDCBChanged then
    begin
      if not SetCommState(FHandle, FDCB) then
        RaiseLastError;
      FDCBChanged := False;
    end;

    if FTimeoutsChanged then
    begin
      if not SetCommTimeouts(FHandle, FTimeouts) then
        RaiseLastError;
      FTimeoutsChanged := False;
    end;
  end;
end;

procedure TComPort.Flush(Flags: Cardinal);
begin
  RaiseIfNotOpen;

  if not PurgeComm(FHandle, Flags) then
    RaiseLastError;
end;

procedure TComPort.FlushInput;
begin
  Flush(PURGE_RXABORT or PURGE_RXCLEAR);
end;

procedure TComPort.FlushOutput;
begin
  Flush(PURGE_TXABORT or PURGE_TXCLEAR);
end;

procedure TComPort.FlushBuffers;
begin
  RaiseIfNotOpen;

  if not FlushFileBuffers(FHandle) then
    RaiseLastError;
end;

function TComPort.Read(var Buffer; BytesToRead: Cardinal): Cardinal;
var
  BytesRead: Cardinal = 0;
begin
  RaiseIfNotOpen;

  if not ReadFile(FHandle, Buffer, BytesToRead, BytesRead, nil) then
    RaiseLastError;

  Result := BytesRead;
end;

function TComPort.Write(const Data; BytesToWrite: Cardinal): Cardinal;
var
  BytesWritten: Cardinal = 0;
begin
  RaiseIfNotOpen;

  if not WriteFile(FHandle, Data, BytesToWrite, BytesWritten, nil) then
    RaiseLastError;

  Result := BytesWritten;
end;

end.

