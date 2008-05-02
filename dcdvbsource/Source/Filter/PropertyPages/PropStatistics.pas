(* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  Copyright (C) 2004-2006 Milenko "DCoder" Mitrovic                        *
 *  Mail: dcoder@dsp-worx.de                                                 *
 *  Web:  http://www.dsp-worx.de                                             *
 *                                                                           *
 *  This Program is free software; you can redistribute it and/or modify     *
 *  it under the terms of the GNU General Public License as published by     *
 *  the Free Software Foundation; either version 2, or (at your option)      *
 *  any later version.                                                       *
 *                                                                           *
 *  This Program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the             *
 *  GNU General Public License for more details.                             *
 *                                                                           *
 *  You should have received a copy of the GNU General Public License        *
 *  along with GNU Make; see the file COPYING.  If not, write to             *
 *  the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.    *
 *  http://www.gnu.org/copyleft/gpl.html                                     *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

unit PropStatistics;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  ExtCtrls, Forms, BaseClass, ComObj, StdVcl, AxCtrls, DirectShow9, Dialogs,
  DVBInterface, ShellAPI, pngimage, ComCtrls, IDVBSource, DSUtil, DVBVideoAnalyzer;

type
  TFormPropStatistics = class;

  TMyThread = class(TThread)
  private
    FOwner: TFormPropStatistics;
    FLock: TBCCritSec;
    FEvent: THandle;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFormPropStatistics; ALock: TBCCritSec);
  end;

  TPaintData = record
    Valid: Boolean;
    TS: Int64;
    Audio: Int64;
    Video: Int64;
    DSMCC: Int64;
  end;

  TFormPropStatistics = class(TFormPropertyPage)
    TabControl1: TTabControl;
    GroupBox4: TGroupBox;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label31: TLabel;
    Timer1: TTimer;
    Image1: TImage;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FDVBFilter: IDCDVBSource;
    FLock: TBCCritSec;
    FTimer: TMyThread;
    FAvarage: Int64;
    FTSBytes: Int64;
    FVideoBytes: Int64;
    FVideoAvarage: Int64;
    FDSMCCBytes: Int64;
    FDSMCCAvarage: Int64;
    FAudioBytes: Int64;
    FAudioAvarage: Int64;
    FBMP: TBitmap;
    FPaintData: array[0..370] of TPaintData;
    procedure PaintBitmap;
    procedure OnTimer;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;

  end;

implementation

uses Math, StrUtils;

{$R *.DFM}

constructor TMyThread.Create(AOwner: TFormPropStatistics; ALock: TBCCritSec);
begin
  inherited Create(True);
  FOwner := AOwner;
  FLock := ALock;
  FEvent := CreateEvent(nil, False, False, nil);
  Resume;
end;

procedure TMyThread.Execute;
begin
  Priority := tpLowest;
  while not Terminated do
  begin
    FLock.Lock;
    try
      FOwner.OnTimer;
    finally
      FLock.UnLock;
    end;
    if WaitForSingleObject(FEvent, 1000) = WAIT_OBJECT_0
      then break;
  end;

  CloseHandle(FEvent);
end;

procedure TFormPropStatistics.FormCreate(Sender: TObject);
begin
  FBMP := TBitmap.Create;
  FBMP.PixelFormat := pf32bit;
  FBMP.Width := Image1.Width;
  FBMP.Height := Image1.Height;
  GroupBox4.DoubleBuffered := True;
  FLock := TBCCritSec.Create;
  FTimer := TMyThread.Create(Self, FLock);
end;

procedure TFormPropStatistics.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;
  SetEvent(FTimer.FEvent);
  FTimer.Terminate;
  FTimer.WaitFor;
  FTimer.Free;
  FLock.Free;
  FDVBFilter := nil;
end;

procedure TFormPropStatistics.OnTimer;
var
  strength: Integer;
  quality: Integer;
  present: Longbool;
  locked: LongBool;
begin
  if Assigned(FDVBFilter) then
  begin
    if FDVBFilter.get_SignalStatistics(strength, quality, present, locked) = S_OK then
    begin
      Label8.Caption := inttostr(strength);
      Label1.Caption := inttostr(quality);
      Label13.Caption := IfThen(present, 'True', 'False');
      Label12.Caption := IfThen(locked, 'True', 'False');
      ProgressBar1.Position := Round(Abs(strength / 100));
      ProgressBar2.Position := quality;
    end else
    begin
      Label8.Caption := '-1';
      ProgressBar1.Position := 0;
      ProgressBar2.Position := 0;
      Label1.Caption := '-1';
      Label13.Caption := 'False';
      Label12.Caption := 'False';
    end;
  end;
end;

function TFormPropStatistics.OnConnect(Unknown: IUnKnown): HRESULT;
begin
  Result := S_OK;
  FLock.Lock;
  try
    Unknown.QueryInterface(IID_IDCDVBSource, FDVBFilter);
    Timer1.Enabled := True;
    Timer1Timer(Self);
  finally
    FLock.UnLock;
  end;
end;

function GetBytesString(ACountByte: Int64): String;
begin
  if ACountByte > 1048576 then
  begin
    Result := Format('%.2f Mb/s', [ACountByte / 1048576]);
  end else
  begin
    Result := Format('%.2f Kb/s', [ACountByte / 1024]);
  end;
end;

procedure TFormPropStatistics.Timer1Timer(Sender: TObject);
var
  stream_info: TStreamInfo;
begin
  if Assigned(FDVBFilter) then
  begin
    FDVBFilter.get_StreamInfo(stream_info);

    FTSBytes := 0;
    FVideoBytes := 0;

    move(FPaintData[1], FPaintData[0], SizeOf(TPaintData) * 370);

    FPaintData[370].Valid := True;
    FPaintData[370].TS := 0;
    FPaintData[370].Audio := 0;
    FPaintData[370].Video := 0;
    FPaintData[370].DSMCC := 0;

    if stream_info.AudioPresent then
    begin
      Label18.Caption := '0x' + inttohex(stream_info.AudioPID, 4);
      case stream_info.AudioType of
        atMPEG1Audio: Label19.Caption := 'MPEG-1';
        atMPEG2Audio: Label19.Caption := 'MPEG-2';
        atAC3:        Label19.Caption := 'AC3';
        else          Label19.Caption := 'Unknown';
      end;
      Label20.Caption := inttostr(stream_info.AudioChannels);
      Label31.Caption := inttostr(stream_info.AudioSamplerate);
      Label28.Caption := inttostr(stream_info.AudioBitrate) + ' kbps';

      if FAudioAvarage = 0 then
      begin
        FAudioAvarage := stream_info.AudioTotalBytes;
      end else
      begin
        FAudioBytes := stream_info.AudioTotalBytes - FAudioAvarage;
        FAudioAvarage := stream_info.AudioTotalBytes;
      end;
      FPaintData[370].Audio := FAudioBytes;
    end else
    begin
      Label18.Caption := '---';
      Label19.Caption := '---';
      Label20.Caption := '---';
      Label31.Caption := '---';
      Label28.Caption := '---';
    end;
    
    if stream_info.VideoPresent then
    begin
      Label21.Caption := '0x' + inttohex(stream_info.VideoPID, 4);
      case stream_info.VideoType of
        dmMPEG2: Label22.Caption := 'MPEG-2';
        dmH264:  Label22.Caption := 'H264';
        else     Label22.Caption := 'Unknown';
      end;
      Label23.Caption := inttostr(stream_info.VideoWidth) + 'x' + inttostr(stream_info.VideoHeight);
      Label24.Caption := GetAspectRatioString(stream_info.VideoAspectRatio);
      Label26.Caption := inttostr(stream_info.VideoBitRate div 1000) + ' kbps';
      Label25.Caption := GetFrameRateString(stream_info.VideoFrameRate);

      if FVideoAvarage = 0 then
      begin
        FVideoAvarage := stream_info.VideoTotalBytes;
      end else
      begin
        FVideoBytes := stream_info.VideoTotalBytes - FVideoAvarage;
        FVideoAvarage := stream_info.VideoTotalBytes;
      end;
      FPaintData[370].Video := FVideoBytes;
    end else
    begin
      Label21.Caption := '---';
      Label22.Caption := '---';
      Label23.Caption := '---';
      Label24.Caption := '---';
      Label26.Caption := '---';
      Label25.Caption := '---';
    end;

    if FDSMCCAvarage = 0 then
    begin
      FDSMCCAvarage := stream_info.DSMCCTotalBytes;
    end else
    begin
      FDSMCCBytes := stream_info.DSMCCTotalBytes - FDSMCCAvarage;
      FDSMCCAvarage := stream_info.DSMCCTotalBytes;
    end;
    FPaintData[370].DSMCC := FDSMCCBytes;

    if FAvarage = 0 then
    begin
      FAvarage := stream_info.TotalTSBytes;
    end else
    begin
      FTSBytes := stream_info.TotalTSBytes - FAvarage;
      FAvarage := stream_info.TotalTSBytes;
    end;
    FPaintData[370].TS := FTSBytes;

    PaintBitmap;
  end;
end;

procedure TFormPropStatistics.PaintBitmap;
var
  cv: TCanvas;
  i, c, x, y: Integer;
  s: String;
  max: Int64;
  avg: Single;
begin
  cv := FBMP.Canvas;

  cv.Pen.Color := clBlack;
  cv.Brush.Color := clBlack;
  cv.Rectangle(cv.ClipRect);

  cv.Pen.Color := $00408000;

  c := FBMP.Height;
  i := 0;
  while (c > 0) do
  begin
    if (i = 0) or (c = 1)
      then cv.MoveTo(0, i)
      else cv.MoveTo(60, i);
    cv.LineTo(FBMP.Width, i);
    inc(i, 10);
    dec(c, 10);
  end;

  c := FBMP.Width;
  i := 0;
  while (c > 0) do
  begin
    cv.MoveTo(i, 0);
    cv.LineTo(i, FBMP.Height);
    if i = 0
      then inc(i, 60)
      else inc(i, 10);
    dec(c, 10);
  end;

  cv.Font.Name := 'Tahoma';
  cv.Font.Size := 7;
  cv.Brush.Style := bsClear;

  cv.Font.Color := clRed;
  cv.TextOut(5, 4, 'TS');
  s := GetBytesString(FTSBytes * 8);
  cv.TextOut(5, 14, s);

  cv.Font.Color := clYellow;
  cv.TextOut(5, 34, 'Video (ES)');
  s := GetBytesString(FVideoBytes * 8);
  cv.TextOut(5, 44, s);

  cv.Font.Color := clSkyBlue;
  cv.TextOut(5, 64, 'Audio (ES)');
  s := GetBytesString(FAudioBytes * 8);
  cv.TextOut(5, 74, s);

  cv.Font.Color := clLime;
  cv.TextOut(5, 94, 'DSMCC');
  s := GetBytesString(FDSMCCBytes * 8);
  cv.TextOut(5, 104, s);

  max := 0;
  for i := 0 to 370 do
  begin
    if FPaintData[i].Valid and (FPaintData[i].TS > max)
      then max := FPaintData[i].TS;
  end;

  avg := max / (FBMP.Height - 2);

  cv.Pen.Color := clRed;
  for i := 0 to 370 do
  begin
    x := i + 60;

    if avg = 0
      then y := FBMP.Height -1
      else y := FBMP.Height - 1 - Round(FPaintData[i].TS / avg);

    if i = 0
      then cv.MoveTo(x, y)
      else cv.LineTo(x, y);
  end;

  cv.Pen.Color := clYellow;
  for i := 0 to 370 do
  begin
    x := i + 60;

    if avg = 0
      then y := FBMP.Height -1
      else y := FBMP.Height - 1 - Round(FPaintData[i].Video / avg);

    if i = 0
      then cv.MoveTo(x, y)
      else cv.LineTo(x, y);
  end;

  cv.Pen.Color := clLime;
  for i := 0 to 370 do
  begin
    x := i + 60;

    if avg = 0
      then y := FBMP.Height -1
      else y := FBMP.Height - 1 - Round(FPaintData[i].DSMCC / avg);

    if i = 0
      then cv.MoveTo(x, y)
      else cv.LineTo(x, y);
  end;

  cv.Pen.Color := clSkyBlue;
  for i := 0 to 370 do
  begin
    x := i + 60;

    if avg = 0
      then y := FBMP.Height -1
      else y := FBMP.Height - 1 - Round(FPaintData[i].Audio / avg);

    if i = 0
      then cv.MoveTo(x, y)
      else cv.LineTo(x, y);
  end;

  Image1.Canvas.Draw(0, 0, FBMP);
end;

initialization

  TBCClassFactory.CreatePropertyPage(TFormPropStatistics, CLSID_PropertyPageStatistics);

end.
