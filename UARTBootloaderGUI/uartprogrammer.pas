unit UARTProgrammer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, ComPort, HexFileParser, LazUTF8;

type
  EUARTProgrammerError = class(Exception);

  { TUARTProgrammer }

  TUARTProgrammer = class
  public type
    TProgressCallback = procedure(ProgressValue: Cardinal; var Cancel: Boolean) of object;
  private type
    TCommand = Cardinal;
    TReply = Cardinal;
    TProgressLimits = record
      LowerLimit, UpperLimit: Cardinal;
    end;
  private
    FComPort: TComPort;
    FSendBeforeConnect: Boolean;
    FSendBeforeConnectString: String;
    FWaitAfterSendBeforeConnect: Cardinal;

    FConnected: Boolean;
    FPageSize: Cardinal;
    FPageCount: Cardinal;
    FHardwareId: String;

    FProgressCallback: TProgressCallback;
    FProgressValue, FProgressLowerLimit, FProgressUpperLimit, FProgressMaxValue: Cardinal;
    FCanceled: Boolean;
    procedure InitProgressLimits(out Limits: TProgressLimits);
    procedure SaveProgressLimits(out Limits: TProgressLimits);
    procedure RestoreProgressLimits(const Limits: TProgressLimits);
    procedure SetProgressLimits(ALowerLimit, AUpperLimit, AMaxValue: Cardinal; const Limits: TProgressLimits);
    procedure ReportProgress(AValue, AMaxValue: Cardinal);

    procedure SendAutobaudPattern;
    procedure SendStringBeforeConnectAndWait;
    procedure SendCommand(ACommand: TCommand);
    procedure SendByte(AByte: Byte);
    procedure SendWord(AWord: Word);
    procedure DrainReceiver;
    function Receive(out Buffer; BytesToReceive: Cardinal): Boolean;
    function ReceiveReply(out AReply: TReply): Boolean;
    function ReceiveByte(out AByte: Byte): Boolean;
    function ReceiveWord(out AWord: Word): Boolean;
    function ReceiveHardwareId: Boolean;
    function TryConnectToBootloader: Boolean;
    function TryDisconnectFromBootloader: Boolean;
    function TryWriteFlashMemoryPage(PageNumber: Cardinal; const Data): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetProgressCallback(ACallback: TProgressCallback; AProgressMaxValue: Cardinal = 100);
    procedure ConnectToBootloader;
    procedure DisconnectFromBootloader;
    procedure ProgramFlashMemory(const MemoryContent: TMemoryContent);

    property ComPort: TComPort read FComPort;
    property SendBeforeConnect: Boolean read FSendBeforeConnect write FSendBeforeConnect;
    property SendBeforeConnectString: String read FSendBeforeConnectString write FSendBeforeConnectString;
    property WaitAfterSendBeforeConnect: Cardinal read FWaitAfterSendBeforeConnect write FWaitAfterSendBeforeConnect;

    property Connected: Boolean read FConnected;
    property PageSize: Cardinal read FPageSize;
    property PageCount: Cardinal read FPageCount;
    property HardwareId: String read FHardwareId;
  end;

implementation

function GetTimeMSecs: Comp;
begin
  Result := TimeStampToMSecs(DateTimeToTimeStamp(Now));
end;

function ToString(Symbol: Char): String;
begin
  if Ord(Symbol) in [32..127] then
    Result := Symbol
  else
    Result := Format('\x%.2x', [Ord(Symbol)]);
end;

function ToHex(Symbol: Char): Byte;
begin
  if Symbol in ['0'..'9'] then
    Result := Ord(Symbol) - Ord('0')
  else if Symbol in ['A'..'F'] then
    Result := Ord(Symbol) - (Ord('A') - 10)
  else if Symbol in ['a'..'f'] then
    Result := Ord(Symbol) - (Ord('a') - 10)
  else
    raise EUARTProgrammerError.CreateFmt('Invalid character in escape sequence ''%s''', [ToString(Symbol)]);
end;

function ConvertToBytesAndUnescape(const S: String): TBytes;
var
  Buffer: TBytes;
  I, L, Size: Integer;
begin
  L := Length(S);
  SetLength(Buffer, L);

  I := 1;
  Size := 0;
  while I <= L do
  begin
    if S[I] = '\' then
    begin
      Inc(I);
      if I > L then
        Break;

      case S[I] of
      'r':
        Buffer[Size] := 13;
      'n':
        Buffer[Size] := 10;
      't':
        Buffer[Size] := 9;
      '0':
        Buffer[Size] := 0;
      'x':
        begin
          Inc(I);
          if I >= L then
            Break;

          Buffer[Size] := (ToHex(S[I]) * 16) + ToHex(S[I + 1]);
          Inc(I);
        end;
      else
        Buffer[Size] := Ord(S[I]);
      end;
    end
    else
      Buffer[Size] := Ord(S[I]);
    Inc(I);
    Inc(Size);
  end;

  SetLength(Buffer, Size);
  Result := Buffer;
end;

const
  BLST = Ord('B') + (Ord('L') shl 8) + (Ord('S') shl 16) + (Ord('T') shl 24);
  BLPG = Ord('B') + (Ord('L') shl 8) + (Ord('P') shl 16) + (Ord('G') shl 24);
  BLXT = Ord('B') + (Ord('L') shl 8) + (Ord('X') shl 16) + (Ord('T') shl 24);

  BlstReply = Ord('b') + (Ord('l') shl 8) + (Ord('s') shl 16) + (Ord('t') shl 24);
  BlpgReply = Ord('b') + (Ord('l') shl 8) + (Ord('p') shl 16) + (Ord('g') shl 24);
  BlxtReply = Ord('b') + (Ord('l') shl 8) + (Ord('x') shl 16) + (Ord('t') shl 24);

  MAX_ATTEMPTS = 3;

{ TUARTProgrammer }

procedure TUARTProgrammer.InitProgressLimits(out Limits: TProgressLimits);
begin
  FProgressValue := 0;
  FProgressLowerLimit := 0;
  FProgressUpperLimit := FProgressMaxValue;
  SaveProgressLimits(Limits);
  FCanceled := False;
  ReportProgress(0, FProgressMaxValue);
end;

procedure TUARTProgrammer.SaveProgressLimits(out Limits: TProgressLimits);
begin
  with Limits do
  begin
    LowerLimit := FProgressLowerLimit;
    UpperLimit := FProgressUpperLimit;
  end;
end;

procedure TUARTProgrammer.RestoreProgressLimits(const Limits: TProgressLimits);
begin
  with Limits do
  begin
    FProgressLowerLimit := LowerLimit;
    FProgressUpperLimit := UpperLimit;
  end;
end;

procedure TUARTProgrammer.SetProgressLimits(ALowerLimit, AUpperLimit, AMaxValue: Cardinal; const Limits: TProgressLimits);
var
  Delta: Cardinal;
begin
  with Limits do
  begin
    Delta := UpperLimit - LowerLimit;
    FProgressLowerLimit := (ALowerLimit * Delta) div AMaxValue + LowerLimit;
    FProgressUpperLimit := (AUpperLimit * Delta) div AMaxValue + LowerLimit;
  end;
end;

procedure TUARTProgrammer.ReportProgress(AValue, AMaxValue: Cardinal);
begin
  if Assigned(FProgressCallback) and not FCanceled then
  begin
    AValue := (AValue * (FProgressUpperLimit - FProgressLowerLimit)) div AMaxValue + FProgressLowerLimit;
    if AValue > FProgressValue then
      FProgressValue := AValue;

    FProgressCallback(FProgressValue, FCanceled);
    if FCanceled then
      raise EOperationCanceled.Create('Canceled by user');
  end;
end;

procedure TUARTProgrammer.SendAutobaudPattern;
const
  ROUNDS = 10;
  MAX_PROGRESS = 2 * ROUNDS + 2;
var
  Round: Integer;
begin
  ReportProgress(0, MAX_PROGRESS);
  Sleep(100);
  ReportProgress(1, MAX_PROGRESS);
  for Round := 1 to ROUNDS do
  begin
    SendByte(Ord('U'));
    ReportProgress(2 * Round, MAX_PROGRESS);
    Sleep(5);
    ReportProgress(2 * Round + 1, MAX_PROGRESS);
  end;
  Sleep(300);
  ReportProgress(MAX_PROGRESS, MAX_PROGRESS);
end;

procedure TUARTProgrammer.SendStringBeforeConnectAndWait;
var
  Buffer: TBytes;
  Size: Integer;
begin
  ReportProgress(0, 3);

  Buffer := ConvertToBytesAndUnescape(UTF8ToWinCP(FSendBeforeConnectString));
  Size := Length(Buffer);
  if Size <> 0 then
  begin
    FComPort.Write(Buffer, Size);
    ReportProgress(1, 3);
    DrainReceiver;
  end;

  if FWaitAfterSendBeforeConnect <> 0 then
  begin
    ReportProgress(2, 3);
    Sleep(FWaitAfterSendBeforeConnect);
  end;

  ReportProgress(3, 3);
end;

procedure TUARTProgrammer.SendCommand(ACommand: TCommand);
begin
  FComPort.Write(ACommand, SizeOf(ACommand));
end;

procedure TUARTProgrammer.SendByte(AByte: Byte);
begin
  FComPort.Write(AByte, SizeOf(AByte));
end;

procedure TUARTProgrammer.SendWord(AWord: Word);
begin
  FComPort.Write(AWord, SizeOf(AWord));
end;

function TUARTProgrammer.Receive(out Buffer; BytesToReceive: Cardinal): Boolean;
var
  StartTimeMSecs: Comp;
  BytesReceived: Cardinal;
begin
  StartTimeMSecs := GetTimeMSecs;
  BytesReceived := 0;
  repeat
    BytesReceived := BytesReceived + FComPort.Read(PByteArray(@Buffer)^[BytesReceived], BytesToReceive - BytesReceived);
    Result := (BytesReceived >= BytesToReceive);
  until Result or (GetTimeMSecs - StartTimeMSecs >= FComPort.ReadTotalTimeoutConstant);
end;

function TUARTProgrammer.ReceiveReply(out AReply: TReply): Boolean;
begin
  Result := Receive(AReply, SizeOf(AReply));
end;

function TUARTProgrammer.ReceiveByte(out AByte: Byte): Boolean;
begin
  Result := Receive(AByte, SizeOf(AByte));
end;

function TUARTProgrammer.ReceiveWord(out AWord: Word): Boolean;
begin
  Result := Receive(AWord, SizeOf(AWord));
end;

function TUARTProgrammer.ReceiveHardwareId: Boolean;
var
  Buffer: Array[0..14] of Byte;
  S: String;
begin
  Result := Receive(Buffer, SizeOf(Buffer));
  if Result then
  begin
    SetLength(S, SizeOf(Buffer));
    Move(Buffer, S[1], SizeOf(Buffer));
    FHardwareId := WinCPToUTF8(S);
  end;
end;

procedure TUARTProgrammer.DrainReceiver;
var
  AByte: Byte;
begin
  while ReceiveByte(AByte) do
    ReportProgress(0, 1); { Allow user to cancel operation while draining }
end;

function TUARTProgrammer.TryConnectToBootloader: Boolean;
var
  Limits: TProgressLimits;
  AReply: TReply;
  APageSize: Byte;
  APageCount: Word;
begin
  Result := False;

  SaveProgressLimits(Limits);
  try
    SetProgressLimits(0, 2, 7, Limits);
    SendAutobaudPattern;
  finally
    RestoreProgressLimits(Limits);
  end;

  SendCommand(BLST);
  ReportProgress(3, 7);

  if not ReceiveReply(AReply) then
    Exit;
  if AReply <> BlstReply then
    Exit;
  ReportProgress(4, 7);

  if not ReceiveByte(APageSize) then
    Exit;
  FPageSize := APageSize * 2; { Convert from words to bytes }
  ReportProgress(5, 7);

  if not ReceiveWord(APageCount) then
    Exit;
  FPageCount := APageCount;
  ReportProgress(6, 7);

  if not ReceiveHardwareId then
    Exit;
  ReportProgress(7, 7);

  Result := True;
end;

function TUARTProgrammer.TryDisconnectFromBootloader: Boolean;
var
  Limits: TProgressLimits;
  AReply: TReply;
begin
  Result := False;

  SaveProgressLimits(Limits);
  try
    SetProgressLimits(0, 2, 4, Limits);
    SendAutobaudPattern;
  finally
    RestoreProgressLimits(Limits);
  end;

  SendCommand(BLXT);
  ReportProgress(3, 4);

  if not ReceiveReply(AReply) then
    Exit;
  if AReply <> BlxtReply then
    Exit;
  ReportProgress(4, 4);

  Result := True;
end;

function TUARTProgrammer.TryWriteFlashMemoryPage(PageNumber: Cardinal; const Data): Boolean;
var
  I: Cardinal;
  C, Crc: Byte;
  AReply: TReply;
  APageNumber: Word;

  procedure UpdateCrc;
  var
    I: Cardinal;
  begin
    Crc := Crc xor C;
    for I := 0 to 7 do
      if (Crc and $80) <> 0 then
        Crc := (Crc shl 1) xor $CF
      else
        Crc := (Crc shl 1);
  end;

begin
  Result := False;

  SendCommand(BLPG);
  SendWord(PageNumber);

  Crc := $FF;
  for I := 0 to Pred(FPageSize) do
  begin
    C := PByteArray(@Data)^[I];
    SendByte(C);
    UpdateCrc;
  end;
  SendByte(Crc);

  if not ReceiveReply(AReply) then
    Exit;
  if AReply <> BlpgReply then
    Exit;

  if not ReceiveWord(APageNumber) then
    Exit;
  if APageNumber <> PageNumber then
    Exit;

  Result := True;
end;

constructor TUARTProgrammer.Create;
begin
  FComPort := TComPort.Create;
  FSendBeforeConnect := False;
  FSendBeforeConnectString := '';
  FWaitAfterSendBeforeConnect := 1000;
  FConnected := False;
  FPageSize := 0;
  FPageCount := 0;
  FHardwareId := '';
  FProgressCallback := nil;
  FProgressValue := 0;
  FProgressLowerLimit := 0;
  FProgressUpperLimit := 0;
  FProgressMaxValue := 0;
  FCanceled := False;
end;

destructor TUARTProgrammer.Destroy;
begin
  FreeAndNil(FComPort);
end;

procedure TUARTProgrammer.SetProgressCallback(ACallback: TProgressCallback; AProgressMaxValue: Cardinal);
begin
  if Assigned(ACallback) and (AProgressMaxValue = 0) then
    raise EArgumentException.Create('Invalid ProgressMaxValue');

  FProgressCallback := ACallback;
  FProgressMaxValue := AProgressMaxValue;
end;

procedure TUARTProgrammer.ConnectToBootloader;
var
  Limits: TProgressLimits;
begin
  InitProgressLimits(Limits);
  if FConnected then
    raise EUARTProgrammerError.Create('Invalid state: already connected');

  SetProgressLimits(0, 1, 10, Limits);
  if FSendBeforeConnect then
    SendStringBeforeConnectAndWait;

  SetProgressLimits(1, 10, 10, Limits);
  while not TryConnectToBootloader do
    DrainReceiver;

  FConnected := True;
end;

procedure TUARTProgrammer.DisconnectFromBootloader;
var
  Limits: TProgressLimits;
  Attempt: Integer;
begin
  InitProgressLimits(Limits);
  if not FConnected then
    raise EUARTProgrammerError.Create('Invalid state: not connected');

  for Attempt := 1 to MAX_ATTEMPTS do
  begin
    if TryDisconnectFromBootloader then
      Break;
    DrainReceiver;
  end;

  ReportProgress(1, 1);
  FConnected := False;
end;

procedure TUARTProgrammer.ProgramFlashMemory(const MemoryContent: TMemoryContent);
var
  ChunkIndex, OffsetInChunk: Cardinal;

  procedure RaiseIfTrue(Condition: Boolean; Message: String);
  begin
    if Condition then
      raise EUARTProgrammerError.Create(Message);
  end;

  procedure SwitchBootloaderToReadyState;
  var
    Attempt: Integer;
  begin
    try
      Attempt := 0;
      while not TryConnectToBootloader do
      begin
        Inc(Attempt);
        RaiseIfTrue(Attempt >= MAX_ATTEMPTS, 'Lost connection to bootloader');
        DrainReceiver;
      end;
    except
      on E: Exception do
      begin
        if (E is EComPortError) or (E is EUARTProgrammerError) then
          FConnected := False;
        raise;
      end;
    end;
  end;

  procedure WriteFlashMemoryPage(PageNumber: Cardinal; const Data);
  var
    Attempt: Integer;
  begin
    try
      Attempt := 0;
      while not TryWriteFlashMemoryPage(PageNumber, Data) do
      begin
        Inc(Attempt);
        RaiseIfTrue(Attempt >= MAX_ATTEMPTS, 'Lost connection to bootloader');
        DrainReceiver;
      end;
    except
      on E: Exception do
      begin
        if (E is EComPortError) or (E is EUARTProgrammerError) then
          FConnected := False;
        raise;
      end;
    end;
  end;

  function CalcNumberOfPagesToWrite: Cardinal;
  var
    Chunk: ^TMemoryChunk;
    ChunksCount,
    ChunkIndex,
    OffsetInChunk,
    ChunkStartAddress,
    ChunkLength,
    PageStartAddress,
    OffsetInPage,
    Delta,
    UpperBound: Cardinal;

    procedure VerifyPageIsInBounds;
    begin
      if PageStartAddress >= UpperBound then
        raise EUARTProgrammerError.CreateFmt('Cannot program flash memory at 0x%x, the address is too high', [PageStartAddress]);
    end;

  begin
    Result := 0;
    ChunksCount := Length(MemoryContent);
    if ChunksCount <> 0 then
    begin
      UpperBound := FPageSize * FPageCount;
      PageStartAddress := 0;
      OffsetInPage := 0;
      for ChunkIndex := 0 to Pred(ChunksCount) do
      begin
        Chunk := @MemoryContent[ChunkIndex];
        ChunkStartAddress := Chunk^.StartAddress;
        ChunkLength := Length(Chunk^.Content);
        OffsetInChunk := 0;

        if OffsetInPage <> 0 then
          if ChunkStartAddress - PageStartAddress >= FPageSize then
          begin
            Inc(Result);
            OffsetInPage := 0;
          end;

        if OffsetInPage = 0 then
        begin
          OffsetInPage := ChunkStartAddress mod FPageSize;
          PageStartAddress := ChunkStartAddress - OffsetInPage;
          VerifyPageIsInBounds;
        end;

        OffsetInPage := ChunkStartAddress - PageStartAddress;
        Delta := FPageSize - OffsetInPage;
        if Delta > ChunkLength then
        begin
          Inc(OffsetInPage, ChunkLength);
          Continue {for};
        end;

        Inc(Result);
        OffsetInPage := 0;
        OffsetInChunk := Delta;
        Delta := ChunkLength - OffsetInChunk;
        if Delta <> 0 then
        begin
          Inc(Result, Delta div FPageSize);
          OffsetInPage := Delta mod FPageSize;
          {$push}{$warn 4079 off}
          PageStartAddress := ChunkStartAddress + ChunkLength - OffsetInPage;
          {$pop}
          VerifyPageIsInBounds;
        end;
      end {for};
      if OffsetInPage <> 0 then
        Inc(Result);
    end;
  end;

  procedure WritePage;
  var
    Chunk: ^TMemoryChunk;
    ChunksCount,
    ChunkStartAddress,
    ChunkLength,
    PageStartAddress,
    OffsetInPage,
    BytesToMove: Cardinal;
    Buffer: TBytes;
  begin
    ChunksCount := Length(MemoryContent);
    RaiseIfTrue(ChunkIndex >= ChunksCount, 'Internal error: ChunkIndex >= ChunksCount');

    Chunk := @MemoryContent[ChunkIndex];
    ChunkStartAddress := Chunk^.StartAddress;
    ChunkLength := Length(Chunk^.Content);
    RaiseIfTrue(OffsetInChunk >= ChunkLength, 'Internal error: OffsetInChunk >= ChunkLength');

    OffsetInPage := (ChunkStartAddress + OffsetInChunk) mod FPageSize;
    {$push}{$warn 4079 off}
    PageStartAddress := ChunkStartAddress + OffsetInChunk - OffsetInPage;
    {$pop}
    if (OffsetInPage = 0) and (OffsetInChunk + FPageSize <= ChunkLength) then
    begin
      WriteFlashMemoryPage(PageStartAddress div FPageSize, Chunk^.Content[OffsetInChunk]);
      Inc(OffsetInChunk, FPageSize);
      if OffsetInChunk >= ChunkLength then
      begin
        Inc(ChunkIndex);
        OffsetInChunk := 0;
      end;
      Exit;
    end;

    SetLength(Buffer, FPageSize);
    FillByte(Buffer[0], FPageSize, $FF);
    repeat
      BytesToMove := Min(FPageSize - OffsetInPage, ChunkLength - OffsetInChunk);
      Move(Chunk^.Content[OffsetInChunk], Buffer[OffsetInPage], BytesToMove);
      Inc(OffsetInChunk, BytesToMove);
      if OffsetInChunk >= ChunkLength then
      begin
        Inc(ChunkIndex);
        OffsetInChunk := 0;
      end;
      Inc(OffsetInPage, BytesToMove);
      if (OffsetInPage >= FPageSize) or (ChunkIndex >= ChunksCount) then
        Break;

      Chunk := @MemoryContent[ChunkIndex];
      ChunkStartAddress := Chunk^.StartAddress;
      ChunkLength := Length(Chunk^.Content);
      OffsetInPage := ChunkStartAddress - PageStartAddress;
    until OffsetInPage >= FPageSize;

    WriteFlashMemoryPage(PageStartAddress div FPageSize, Buffer[0]);
  end;

var
  Limits: TProgressLimits;
  I, NumberOfPagesToWrite: Cardinal;
begin
  InitProgressLimits(Limits);
  if not FConnected then
    raise EUARTProgrammerError.Create('Invalid state: not connected');

  SetProgressLimits(0, 1, 100, Limits);
  SwitchBootloaderToReadyState;

  SetProgressLimits(1, 99, 100, Limits);
  NumberOfPagesToWrite := CalcNumberOfPagesToWrite;
  if NumberOfPagesToWrite <> 0 then
    try
      ChunkIndex := 0;
      OffsetInChunk := 0;
      for I := 1 to NumberOfPagesToWrite do
      begin
        WritePage;
        ReportProgress(I, NumberOfPagesToWrite);
      end;
    except
      on EOperationCanceled do
      begin
        SwitchBootloaderToReadyState;
        raise;
      end;
    end;

  SetProgressLimits(99, 100, 100, Limits);
  SwitchBootloaderToReadyState;
end;

end.

