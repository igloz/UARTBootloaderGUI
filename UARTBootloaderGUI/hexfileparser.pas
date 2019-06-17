unit HexFileParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TMemoryChunk = record
    StartAddress: Cardinal;
    Content: TBytes;
  end;

  TMemoryContent = Array of TMemoryChunk;

  EOperationCanceled = class(Exception);
  EHexFileParserError = class(EParserError);

  { THexFileParser }

  THexFileParser = class
  public type
    TProgressCallback = procedure(ProgressValue: Cardinal; var Cancel: Boolean) of object;
  private
    FProgressCallback: TProgressCallback;
    FProgressMaxValue: Cardinal;
  public
    constructor Create;
    procedure SetProgressCallback(ACallback: TProgressCallback; AProgressMaxValue: Cardinal = 100);
    function Parse(const FileName: String): TMemoryContent;
  end;

implementation

{ THexFileParser }

constructor THexFileParser.Create;
begin
  FProgressCallback := nil;
  FProgressMaxValue := 0;
end;

procedure THexFileParser.SetProgressCallback(ACallback: TProgressCallback; AProgressMaxValue: Cardinal);
begin
  if Assigned(ACallback) and (AProgressMaxValue = 0) then
    raise EArgumentException.Create('Invalid ProgressMaxValue');

  FProgressCallback := ACallback;
  FProgressMaxValue := AProgressMaxValue;
end;

function THexFileParser.Parse(const FileName: String): TMemoryContent;
var
  MemoryContent: TMemoryContent;
  FileStream: TFileStream;
  FileSize, FilePosition, ReportProgressPosition, ProgressQuant: Cardinal;
  LineNumber: Cardinal;
  BaseAddress: Cardinal;
  LinearMode: Boolean;
  EndOfFileRecordSeen: Boolean;
  RecordType, DataLength, CheckSum: Byte;
  Offset: Word;
  Data: Array[Byte] of Byte;

  procedure ReportProgress;
  var
    Cancel: Boolean;
  begin
    if FilePosition >= ReportProgressPosition then
    begin
      Cancel := False;
      FProgressCallback((FilePosition * FProgressMaxValue) div FileSize, Cancel);
      if Cancel then
        raise EOperationCanceled.Create('Canceled by user');

      Inc(ReportProgressPosition, ProgressQuant);
    end;
  end;

  procedure OpenFileStream;
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    if FileStream.Size = 0 then
      raise EHexFileParserError.Create('File is empty');

    FileSize := FileStream.Size;
    FilePosition := 0;
    LineNumber := 1;
    BaseAddress := 0;
    LinearMode := True;
    EndOfFileRecordSeen := False;
    if Assigned(FProgressCallback) then
    begin
      ReportProgressPosition := 0;
      ProgressQuant := (FileSize + Pred(FProgressMaxValue)) div FProgressMaxValue;
      ReportProgress;
    end;
  end;

  procedure CloseFileStream;
  begin
    FreeAndNil(FileStream);
  end;

  procedure Error(const Description: String); noreturn;
  begin
    raise EHexFileParserError.CreateFmt('Line %u: %s', [LineNumber, Description]);
  end;

  procedure ErrorFmt(const Description: String; const Args: Array of Const); noreturn;
  begin
    Error(Format(Description, Args));
  end;

  function ReadByte: Byte;
  begin
    if FilePosition >= FileSize then
      Error('Unexpected end of file');

    Result := FileStream.ReadByte;
    Inc(FilePosition);
    if Result = 10 then
      Inc(LineNumber);
    if Assigned(FProgressCallback) then
      ReportProgress;
  end;

  function ToString(Symbol: Byte): String;
  begin
    if Symbol in [32..127] then
      Result := Chr(Symbol)
    else
      Result := Format('\x%.2x', [Symbol]);
  end;

  function ToHex(Symbol: Byte): Byte;
  begin
    if Chr(Symbol) in ['0'..'9'] then
      Result := Symbol - Ord('0')
    else if Chr(Symbol) in ['A'..'F'] then
      Result := Symbol - (Ord('A') - 10)
    else if Chr(Symbol) in ['a'..'f'] then
      Result := Symbol - (Ord('a') - 10)
    else
      ErrorFmt('Invalid character ''%s''', [ToString(Symbol)]);
  end;

  procedure ParseRecordMark;
  var
    Symbol: Byte;
  begin
    repeat
      Symbol := ReadByte;
    until not (Symbol in [10, 13]);

    if Symbol <> Ord(':') then
      ErrorFmt(''':'' expected but ''%s'' found', [ToString(Symbol)]);
    CheckSum := 0;
  end;

  function ParseByte: Byte;
  var
    Low, High: Byte;
  begin
    High := ToHex(ReadByte);
    Low := ToHex(ReadByte);
    Result := High * 16 + Low;
    Inc(CheckSum, Result);
  end;

  function ParseWord: Word;
  var
    Low, High: Word;
  begin
    High := ParseByte;
    Low := ParseByte;
    Result := High * 256 + Low;
  end;

  procedure ParseDataLength;
  begin
    DataLength := ParseByte;
  end;

  procedure ParseOffset;
  begin
    Offset := ParseWord;
  end;

  procedure ParseRecordType;
  begin
    RecordType := ParseByte;
  end;

  procedure ParseData;
  var
    I: Byte;
  begin
    if DataLength <> 0 then
      for I := 0 to Pred(DataLength) do
        Data[I] := ParseByte;
  end;

  procedure ParseCheckSum;
  begin
    ParseByte;
    if CheckSum <> 0 then
      Error('Invalid checksum');
  end;

  procedure VerifyDataLengthIs(AValue: Byte);
  begin
    if DataLength <> AValue then
      ErrorFmt('Invalid data length 0x%.2x', [DataLength]);
  end;

  procedure VerifyOffsetIs(AValue: Word);
  begin
    if Offset <> AValue then
      ErrorFmt('Invalid offset 0x%.4x', [Offset]);
  end;

  function GetUpperBaseAddress: Cardinal;
  var
    Low, High: Cardinal;
  begin
    High := Data[0];
    Low := Data[1];
    Result := High * 256 + Low;
  end;

  procedure AddMemoryContent(const AContent; AStartAddress, ALength: Cardinal);
  var
    I, J, MemoryContentLength, PrevLength, NextLength, Delta: Cardinal;
    Prev, Next: ^TMemoryChunk;
    AbutToPrev, AbutToNext: Boolean;
  begin
    MemoryContentLength := Length(MemoryContent);
    I := MemoryContentLength;
    while I > 0 do
      if MemoryContent[Pred(I)].StartAddress > AStartAddress then
        Dec(I)
      else
        Break;

    { (I = 0) or (MemoryContent[Pred(I)].StartAddress <= AStartAddress) }
    if I = 0 then
    begin
      Prev := nil;
      AbutToPrev := False;
    end
    else
    begin
      Prev := @MemoryContent[Pred(I)];
      PrevLength := Length(Prev^.Content);
      Delta := AStartAddress - Prev^.StartAddress;
      if Delta < PrevLength then
        Error('Memory ranges overlap');
      AbutToPrev := (Delta = PrevLength);
    end;

    { (I = MemoryContentLength or (MemoryContent[I].StartAddress > AStartAddress) }
    if I = MemoryContentLength then
    begin
      Next := nil;
      AbutToNext := False;
    end
    else
    begin
      Next := @MemoryContent[I];
      NextLength := Length(Next^.Content);
      Delta := Next^.StartAddress - AStartAddress;
      if Delta < ALength then
        Error('Memory ranges overlap');
      AbutToNext := (Delta = ALength);
    end;

    if AbutToPrev and AbutToNext then
    begin
      { Unite with Prev and with Next. Extend Prev.Content, insert AContent and Next.Content at the end of Prev.Content, then delete Next }
      SetLength(Prev^.Content, PrevLength + ALength + NextLength);
      Move(AContent, Prev^.Content[PrevLength], ALength);
      Move(Next^.Content[0], Prev^.Content[PrevLength + ALength], NextLength);
      while Succ(I) < MemoryContentLength do
      begin
        MemoryContent[I] := MemoryContent[Succ(I)];
        Inc(I);
      end;
      SetLength(MemoryContent, Pred(MemoryContentLength));
    end
    else if AbutToPrev then
    begin
      { Unite with Prev. Extend Prev.Content and insert AContent at the end of Prev.Content }
      SetLength(Prev^.Content, PrevLength + ALength);
      Move(AContent, Prev^.Content[PrevLength], ALength);
    end
    else if AbutToNext then
    begin
      { Unite with Next. Extend Next.Content and insert AContent at the beginning of Next.Content }
      SetLength(Next^.Content, ALength + NextLength);
      Move(Next^.Content[0], Next^.Content[ALength], NextLength);
      Move(AContent, Next^.Content[0], ALength);
      Next^.StartAddress := AStartAddress;
    end
    else
    begin
      { Insert new TMemoryChunk at index I }
      SetLength(MemoryContent, Succ(MemoryContentLength));
      J := MemoryContentLength;
      while J > I do
      begin
        MemoryContent[J] := MemoryContent[Pred(J)];
        Dec(J);
      end;
      Next := @MemoryContent[I];
      SetLength(Next^.Content, ALength);
      Move(AContent, Next^.Content[0], ALength);
      Next^.StartAddress := AStartAddress;
    end;
  end;

  procedure ProcessDataRecord;
  var
    StartAddress, EndAddress: Cardinal;
  begin
    if DataLength <> 0 then
      if LinearMode then
      begin
        StartAddress := BaseAddress + Offset;
        EndAddress := StartAddress + DataLength;
        if (StartAddress < EndAddress) or (EndAddress = 0) then
          AddMemoryContent(Data, StartAddress, DataLength)
        else
        begin
          AddMemoryContent(Data, StartAddress, DataLength - EndAddress);
          AddMemoryContent(Data[DataLength - EndAddress], 0, EndAddress);
        end;
      end
      else { not LinearMode }
      begin
        StartAddress := Offset;
        EndAddress := Word(StartAddress + DataLength);
        if (StartAddress < EndAddress) or (EndAddress = 0) then
          AddMemoryContent(Data, BaseAddress + StartAddress, DataLength)
        else
        begin
          AddMemoryContent(Data, BaseAddress + StartAddress, DataLength - EndAddress);
          AddMemoryContent(Data[DataLength - EndAddress], BaseAddress, EndAddress);
        end;
      end;
  end;

  procedure ProcessEndOfFileRecord;
  var
    Symbol: Byte;
  begin
    VerifyDataLengthIs(0);
    VerifyOffsetIs(0);

    while FilePosition < FileSize do
    begin
      Symbol := ReadByte;
      if not (Symbol in [10, 13]) then
        Error('Extra characters at the end');
    end;

    EndOfFileRecordSeen := True;
  end;

  procedure ProcessExtendedSegmentAddressRecord;
  begin
    VerifyDataLengthIs(2);
    VerifyOffsetIs(0);

    BaseAddress := GetUpperBaseAddress shl 4;
    LinearMode := False;
  end;

  procedure ProcessStartSegmentAddressRecord;
  begin
    VerifyDataLengthIs(4);
    VerifyOffsetIs(0);
  end;

  procedure ProcessExtendedLinearAddressRecord;
  begin
    VerifyDataLengthIs(2);
    VerifyOffsetIs(0);

    BaseAddress := GetUpperBaseAddress shl 16;
    LinearMode := True;
  end;

  procedure ProcessStartLinearAddressRecord;
  begin
    VerifyDataLengthIs(4);
    VerifyOffsetIs(0);
  end;

  procedure ParseRecord;
  begin
    ParseRecordMark;
    ParseDataLength;
    ParseOffset;
    ParseRecordType;
    ParseData;
    ParseCheckSum;
    case RecordType of
    0: ProcessDataRecord;
    1: ProcessEndOfFileRecord;
    2: ProcessExtendedSegmentAddressRecord;
    3: ProcessStartSegmentAddressRecord;
    4: ProcessExtendedLinearAddressRecord;
    5: ProcessStartLinearAddressRecord;
    else
      ErrorFmt('Unknown record type 0x%.2x', [RecordType]);
    end;
  end;

begin
  SetLength(MemoryContent, 0);
  OpenFileStream;
  try
    repeat
      ParseRecord;
    until EndOfFileRecordSeen;
  finally
    CloseFileStream;
  end;
  Result := MemoryContent;
end;

end.

