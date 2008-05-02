unit TSTimeShift;

interface

uses
  Windows, Classes, SysUtils, MPEGConst, MPEGParser, MPEGUtils, BaseClass, Math;

type
  TStorageType = (
    stUndefined,
    stFile,
    stRAM
  );

  TTSTimeShift = class;

  TTSTimeShiftStorage = class
  protected
    FParent: TTSTimeShift;
  public
    constructor Create(AParent: TTSTimeShift);
    destructor Destroy; override;
  end;

  TTSTimeShiftFileStorage = class(TTSTimeShiftStorage)
  public
    constructor Create(AParent: TTSTimeShift; AFileName: WideString);
    destructor Destroy; override;
  end;

  TTSTimeShiftRAMStorage = class(TTSTimeShiftStorage)
  end;

  TTSTimeShift = class
  protected
    FInitialized: Boolean;
    FStorageType: TStorageType;
    FTempStorePath: WideString;
    FLock: TBCCritSec;
    FPIDS: array of Integer;
    FMaxDurationMS: Int64;
    FSplitDurationMS: Int64;

    function GetDurationMS: Int64;
    function GetPositionMS: Int64;
    procedure SetPositionMS(APositionMS: Int64);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(ATempStorePath: WideString; AMaxDurationMS: Int64; ASplitDurationMS: Int64; AStorageType: TStorageType);
    procedure Reset;

    procedure SetFilterPIDS(APIDS: array of Integer);
    procedure ClearFilterPIDS;
    procedure ResetStorage;

    procedure AddData(ABuffer: PByte; ASize: Integer);

    property DurationMS: Int64 read GetDurationMS;
    property PositionMS: Int64 read GetPositionMS write SetPositionMS;
  end;

implementation

(*** TTSTimeShiftStorage ******************************************************)

constructor TTSTimeShiftStorage.Create(AParent: TTSTimeShift);
begin
  inherited Create;

  FParent := AParent;
  Assert(FParent <> nil);
end;

destructor TTSTimeShiftStorage.Destroy;
begin
  inherited Destroy;
end;

(*** TTSTimeShiftFileStorage **************************************************)

constructor TTSTimeShiftFileStorage.Create(AParent: TTSTimeShift; AFileName: WideString);
begin
  inherited Create(AParent);

  Assert(AFileName <> '');
end;

destructor TTSTimeShiftFileStorage.Destroy;
begin
  inherited Destroy;
end;

(*** TTSTimeShift *************************************************************)

function TTSTimeShift.GetDurationMS: Int64;
begin
  Result := 0;
end;

function TTSTimeShift.GetPositionMS: Int64;
begin
  Result := 0;
end;

procedure TTSTimeShift.SetPositionMS(APositionMS: Int64);
begin
//
end;

constructor TTSTimeShift.Create;
begin
  inherited Create;

  FLock := TBCCritSec.Create;
end;

destructor TTSTimeShift.Destroy;
begin
  Reset;
  FLock.Free;
  inherited Destroy;
end;

procedure TTSTimeShift.Init(ATempStorePath: WideString; AMaxDurationMS: Int64; ASplitDurationMS: Int64; AStorageType: TStorageType);
var
  i, storage_count: Integer;
begin
  Reset;

  FTempStorePath := ATempStorePath;
  FMaxDurationMS := AMaxDurationMS;
  FSplitDurationMS := ASplitDurationMS;
  FStorageType := AStorageType;

  storage_count := Ceil(FMaxDurationMS / FSplitDurationMS);
  for i := 0 to storage_count - 1 do
  begin

  end;

end;

procedure TTSTimeShift.Reset;
begin
  FLock.Lock;
  try
    FInitialized := False;
    FStorageType := stUndefined;
    FTempStorePath := '';
    SetLength(FPIDS, 0);
  finally
    FLock.UnLock;
  end;
end;

procedure TTSTimeShift.SetFilterPIDS(APIDS: array of Integer);
var
  i: Integer;
begin
  FLock.Lock;
  try
    SetLength(FPIDS, High(APIDS) + 1);
    for i := 0 to High(APIDS)
      do FPIDS[i] := APIDS[i];

  finally
    FLock.UnLock;
  end;
end;

procedure TTSTimeShift.ClearFilterPIDS;
begin
  FLock.Lock;
  try
    SetLength(FPIDS, 0);
  finally
    FLock.UnLock;
  end;
end;

procedure TTSTimeShift.ResetStorage;
begin
  FLock.Lock;
  try

  finally
    FLock.UnLock;
  end;
end;

procedure TTSTimeShift.AddData(ABuffer: PByte; ASize: Integer);
begin
  FLock.Lock;
  try
    if (not FInitialized)
      then Exit;

  finally
    FLock.UnLock;
  end;
end;


end.
