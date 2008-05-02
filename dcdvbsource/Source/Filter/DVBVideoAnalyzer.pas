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

unit DVBVideoAnalyzer;

interface

uses
  BaseClass, ActiveX, DirectShow9, Windows, DSUtil, MMSystem, DVBInterface,
  BDAConst, MPEGUtils, Classes, SysUtils, DVBSubtitleFilter, DVBSubtitlingParser,
  IDVBSource, MPEGConst, IffDecoder_com, BitReader, Math;

type
  TSequenceHeader = class
  private
    FWidth: Integer;
    FHeight: Integer;
    FAspectRatio: TAspectRatio;
    FFrameRate: TFrameRate;
    FBitRate: Int64;
  public
    procedure ParseBuffer(ABuffer: PByte);
  published
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property AspectRatio: TAspectRatio read FAspectRatio;
    property FrameRate: TFrameRate read FFrameRate;
    property BitRate: Int64 read FBitRate;
  end;

  function GetFrameRateString(AFrameRate: TFrameRate): String;
  function GetAspectRatioString(AAspectRatio: TAspectRatio): String;

type
  TDVBVideoFilter = class(TBCTransInPlaceFilter, IVideoFilter)
  private
    FFFDShow: IffDecoder;
    FVideoLock: TBCCritSec;
    FEnabled: Boolean;
    FCurrentAspectRatio: TAspectRatio;
    FCallback: ISubtitleCallback;
    FVideoCallback: TVideoInfoCallback;
    FBytesCallback: TVideoBytesCallback;
    FSequenceHeader: TSequenceHeader;
    FNextFlush: Boolean;
    FDecoderMode: TVideoType;
    FCurrentWidth: Integer;
    FCurrentHeight: Integer;
    FAspectX: Integer;
    FAspectY: Integer;
    FInterlaced: Boolean;
    procedure ResizeFFDShow;
  public
    constructor Create;
    destructor Destroy; override;

    function Transform(Sample: IMediaSample): HRESULT; override;
    function CheckInputType(mtIn: PAMMediaType): HRESULT; override;
    function CompleteConnect(dir: _PinDirection; ReceivePin: IPin): HRESULT; override;

    // IVideoFilter
    procedure put_Callback(ACallback: ISubtitleCallback);
    procedure put_InfoCallback(ACallback: TVideoInfoCallback);
    procedure put_BytesCallback(ACallback: TVideoBytesCallback);
    procedure put_Enabled(AEnabled: Boolean);
    procedure put_FlushStream(AFlush: Boolean);
  end;

implementation

function GetFrameRateString(AFrameRate: TFrameRate): String;
begin
  case AFrameRate of
    frForbidden:  Result := 'Forbidden';
    fr23_976:     Result := '23.976 fps';
    fr24:         Result := '24 fps';
    fr25:         Result := '25 fps';
    fr29_97:      Result := '29.97 fps';
    fr30:         Result := '30 fps';
    fr50:         Result := '50 fps';
    fr59_94:      Result := '59.94 fps';
    fr60:         Result := '60 fps';
    else          Result := 'Unknown';
  end;
end;

function GetAspectRatioString(AAspectRatio: TAspectRatio): String;
begin
  case AAspectRatio of
    ar4_3:    Result := '4:3';
    ar16_9:   Result := '16:9';
    ar221_1:  Result := '2.21:1';
    else      Result := 'Unknown';
  end;
end;

procedure TSequenceHeader.ParseBuffer(ABuffer: PByte);
begin
  inc(ABuffer, 4); // skip Sequency Header
  FWidth := GetWordBits(ABuffer, 0, 12);
  inc(ABuffer);
  FHeight := GetWordBits(ABuffer, 4, 12);
  inc(ABuffer, 2);
  case (ABuffer^ shr 4) of
    2: FAspectRatio := ar4_3;
    3: FAspectRatio := ar16_9;
    4: FAspectRatio := ar221_1;
  end;
  case (ABuffer^ and $0F) of
    1: FFrameRate := fr23_976;
    2: FFrameRate := fr24;
    3: FFrameRate := fr25;
    4: FFrameRate := fr29_97;
    5: FFrameRate := fr30;
    6: FFrameRate := fr50;
    7: FFrameRate := fr59_94;
    8: FFrameRate := fr60;
  end;
  inc(ABuffer);
  FBitRate := GetLongBits(ABuffer, 0, 18) * 400;
end;

(*** TDVBVideoFilter **********************************************************)

constructor TDVBVideoFilter.Create;
var
  hr: HRESULT;
begin
  inherited Create('DC-DVB Video Analyzer', nil, CLSID_VideoFilter, hr);
  FVideoLock := TBCCritSec.Create;
  FEnabled := False;
  FCallback := nil;
  FCurrentAspectRatio := ar4_3;
  FFFDShow := nil;
  FSequenceHeader := TSequenceHeader.Create;
  FCurrentWidth := 1920;
  FCurrentHeight := 1080;
  FAspectX := 1;
  FAspectY := 1;
  FInterlaced := False;
end;

destructor TDVBVideoFilter.Destroy;
begin
  if Assigned(FFFDShow) then
  begin
    FFFDShow.putParam(IDFF_isResize, 0);
    FFFDShow := nil;
  end;
  FCallback := nil;
  FEnabled := False;
  FVideoLock.Free;
  FSequenceHeader.Free;
  inherited Destroy;
end;

function TDVBVideoFilter.CheckInputType(mtin: PAMMediaType): HRESULT;
begin
  Result := S_OK;
end;

function TDVBVideoFilter.Transform(Sample: IMediaSample): HRESULT;
const
  SEQUENCE_HEADER = $B3010000;
  EXTENDED_SAR = 255;
var
  buffer: PByte;
  size: Integer;
  t1, t2, t3, t4: Int64;
  str: String;
  i, id, j, next, s: Integer;
  reader: TBitReader;
  profile: Integer;
  level: Integer;
  pic_order_cnt_type: Int64;
  pic_width_in_mbs_minus1,
  pic_height_in_map_units_minus1: Int64;
  frame_mbs_only_flag: Byte;
  w, h, ax, ay: Integer;
  sa: Int64;
  frame_cropping_flag: Integer;
  vui_parameters_present_flag: Integer;
  aspect_ratio_info_present_flag: Integer;
  aspect_ratio_idc: Integer;
  interlaced: Boolean;
  overscan_info_flag: Integer;
  overscan_appropriate_flag: Integer;

  function se: Int64;
  begin
    Result := reader.GolombUE;
	Result := Round(Power(-1, Result) + Ceil(Result mod 2));
  end;

begin
  Result := S_OK;

  Sample.GetTime(t1, t2);
  StreamTime(t3);

//[3916] 12060920 - 0 - 1 - 0 - 6972
//[3916] 13614444 - 0 - 1 - 1 - 3474
//[3916] 14014444 - 0 - 1 - 1 - 3666
//[3916] 15614444 - 0 - 1 - 1 - 6596
//[3916] 14814444 - 0 - 1 - 1 - 3298
//[3916] 15214444 - 0 - 1 - 1 - 3290
//[3916] 16814444 - 0 - 1 - 1 - 5684
//[3916] 16014444 - 0 - 1 - 1 - 2922
//[3916] 16414444 - 0 - 1 - 1 - 2930
//[3916] 18014444 - 0 - 1 - 1 - 8192
//[3916] 8395472727889768652 - 1 - 1 - 1 - 8192
//[3916] 8395472727889768652 - 1 - 1 - 1 - 8192
//[3916] 8395472727889768652 - 1 - 1 - 1 - 8192
//[3916] 8395472727889768652 - 1 - 1 - 1 - 8192
//[3916] 8395472727889768652 - 1 - 1 - 1 - 5164
//[3916] 17214444 - 0 - 1 - 1 - 3290
//[3916] 17614444 - 0 - 1 - 1 - 3114
//[3916] 19214444 - 0 - 1 - 1 - 8192
//[3916] 8395472727889768732 - 1 - 1 - 1 - 428
//[3916] 18414444 - 0 - 1 - 1 - 3842
//[3916] 18814444 - 0 - 1 - 1 - 3666
//[3916] 20414444 - 0 - 1 - 1 - 6044
//[3916] 19614444 - 0 - 1 - 1 - 3114
//[3916] 20014444 - 0 - 1 - 1 - 3106
//[3916] 21614444 - 0 - 1 - 1 - 5676
//[3916] 20814444 - 0 - 1 - 1 - 3114
//[3916] 21214444 - 0 - 1 - 1 - 3298
//[3916] 22814444 - 0 - 1 - 1 - 8192

//  OutputDebugString(PChar(inttostr(t1) + ' - ' + inttostr(t3) + ' - ' + inttostr(Sample.IsSyncPoint) + ' - ' + inttostr(Sample.IsPreroll) + ' - ' + inttostr(Sample.IsDiscontinuity) + ' - ' + inttostr(Sample.GetActualDataLength)));
//  OutputDebugString(PChar(inttostr(t1) + ' - ' + inttostr(t2) + ' - ' + inttostr(t3) + ' - ' + inttostr(Sample.GetActualDataLength)));

  FVideoLock.Lock;
  try
    if (* not FEnabled or *) not Assigned(Sample)
      then Exit;

    // Check for Aspect Ratio

    if FNextFlush then
    begin
      if Assigned(FFFDShow) then
      begin
        Sample.SetMediaType(FInput.CurrentMediaType.MediaType);
      end;
      Sample.SetDiscontinuity(True);
      FOutput.DeliverBeginFlush;
      FOutput.DeliverEndFlush;
      FNextFlush := False;
    end;

    buffer := nil;
    Sample.GetPointer(buffer);
    size := Sample.GetActualDataLength;

    if not Assigned(buffer) or (size <= 0)
      then Exit;

    if Assigned(FBytesCallback)
      then FBytesCallback(size);

    if FDecoderMode = dmH264 then
    begin
      reader := TBitReader.Create;
      while (size > 8) do
      begin

        if GetLong(buffer) = 1 then
        begin
          // DRAFT ISO/IEC 14496-10 : 2002 (E)
          // Google -> JVT-G050.pdf
          reader.SetBuffer(buffer, size);
          reader.Read(32);
          id := reader.Read(8);

          if(((id and $9F) = $07) and ((id and $60) <> 0)) then
          begin

            profile := reader.Read(8); // profile
            reader.Read(8);
            level := reader.Read(8); // level

            reader.GolombUE;

      			if(profile >= 100) then
            begin
  				    if(reader.GolombUE = 3)
                then reader.Read(1);

                reader.GolombUE;
                reader.GolombUE;

                reader.Read(1);
      				if(reader.Read(1) > 0) then
              begin
      					for i := 0 to 7 do
                begin
      						if(reader.Read(1) > 0) then
                  begin
                    if i < 6 then s := 16 else s := 64;
                    next := 8;
                    j := 0;

                    while ((j < s) and (next <> 0)) do
                    begin
      								next := (next + se + 256) and 255;
                      inc(j);
                    end;
                  end;
                end;
              end;
            end;

            reader.GolombUE;

            pic_order_cnt_type := reader.GolombUE;

            if(pic_order_cnt_type = 0) then
            begin
              reader.GolombUE;
            end else
            if(pic_order_cnt_type = 1) then
            begin
              reader.Read(1);
              se;
              se;
              sa := reader.GolombUE;
              for i := 0 to sa -1
                do se;
            end;

            reader.GolombUE;
            reader.Read(1);

            pic_width_in_mbs_minus1 := reader.GolombUE;
            pic_height_in_map_units_minus1 := reader.GolombUE;
            frame_mbs_only_flag := reader.Read(1);

            interlaced := frame_mbs_only_flag = 0;

            w := (pic_width_in_mbs_minus1 + 1) * 16;
            h := (2 - frame_mbs_only_flag) * (pic_height_in_map_units_minus1 + 1) * 16;

            if (0 = frame_mbs_only_flag)
              then reader.Read(1);// mb_adaptive_frame_field_flag

            reader.Read(1); // direct_8x8_inference_flag
            frame_cropping_flag := reader.Read(1); // direct_8x8_inference_flag
            if (frame_cropping_flag = 1) then
            begin
              t1 := reader.GolombUE; // frame_crop_left_offset
              t2 := reader.GolombUE; // frame_crop_right_offset
              t3 := reader.GolombUE; // frame_crop_top_offset
              t4 := reader.GolombUE; // frame_crop_bottom_offset
//              OutputDebugString(PChar(inttostr(t1) + ' - ' + inttostr(t2) + ' - ' + inttostr(t3) + ' - ' + inttostr(t4)));
            end;

            ax := w;
            ay := h;


//            ue;
//            reader.Read(2);


            vui_parameters_present_flag := reader.Read(1);
            if (vui_parameters_present_flag = 1) then
            begin
              aspect_ratio_info_present_flag := reader.Read(1);
              if (aspect_ratio_info_present_flag = 1) then
              begin
                aspect_ratio_idc := reader.Read(8);
//                OutputDebugString(pchar(inttostr(aspect_ratio_idc)));

                case aspect_ratio_idc of
                  1:
                  begin
                    ax := w * 1;
                    ay := h * 1;
//                    ax := 1;   21600
//                    ay := 1;   11880
                  end;
                  2:
                  begin
                    ax := 12 * w;
                    ay := 11 * h;
                  end;
                  3:
                  begin
                    ax := 10 * w;
                    ay := 11 * h;
                  end;
                  4:
                  begin
                    ax := 16 * w;
                    ay := 11 * h;
                  end;
                  5:
                  begin
                    ax := 40 * w;
                    ay := 33 * h;
                  end;
                  6:
                  begin
                    ax := 24 * w;
                    ay := 11 * h;
                  end;
                  7:
                  begin
                    ax := 20 * w;
                    ay := 11 * h;
                  end;
                  8:
                  begin
                    ax := 32 * w;
                    ay := 11 * h;
                  end;
                  9:
                  begin
                    ax := 80 * w;
                    ay := 33 * h;
                  end;
                  10:
                  begin
                    ax := 18 * w;
                    ay := 11 * h;
                  end;
                  11:
                  begin
                    ax := 15 * w;
                    ay := 11 * h;
                  end;
                  12:
                  begin
                    ax := 64 * w;
                    ay := 33 * h;
                  end;
                  13:
                  begin
                    ax := 160 * w;
                    ay := 99  * h;
                  end;
                  EXTENDED_SAR:
                  begin
                    ax := reader.Read(16) * w;
                    ay := reader.Read(16) * h;
                  end;
                end;
              end;
            end;

            // TODO setup rcTarget using overscan_info_flag
            overscan_info_flag := reader.Read(1);
//            OutputDebugString(PChar('overscan_info_flag: ' + inttostr(overscan_info_flag)));

            if (overscan_info_flag = 1) then
            begin
              overscan_appropriate_flag := reader.read(1);
//              OutputDebugString(PChar('overscan_appropriate_flag: ' + inttostr(overscan_appropriate_flag)));
            end;

            if ax = -1 then
              ax := 16;
            if ay = -1 then
              ay := 9;

            if (ax = 16) and (ay = 9)
              then FCurrentAspectRatio := ar16_9
              else FCurrentAspectRatio := ar4_3;

            if Assigned(FVideoCallback) then
            begin
              FVideoCallback(FDecoderMode, w, h, FCurrentAspectRatio, frForbidden, 0);
            end;

            if Assigned(FCallback) then
            begin
              FCallback.put_AspectRatio(FCurrentAspectRatio);
            end;

            if (FCurrentWidth <> w) or (FCurrentHeight <> h) or
               (FAspectX <> ax) or (FAspectY <> ay) or
               (FInterlaced <> interlaced) then
            begin
              FCurrentWidth := w;
              FCurrentHeight := h;

              FAspectX := ax;
              FAspectY := ay;
              FInterlaced := interlaced;

              PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat).bmiHeader.biWidth := FCurrentWidth;
              PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat).bmiHeader.biHeight := FCurrentHeight;
              PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat).bmiHeader.biSizeImage := FCurrentWidth * FCurrentHeight * PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat).bmiHeader.biBitCount div 8;
              PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat).dwPictAspectRatioX := ax;
              PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat).dwPictAspectRatioY := ay;
              if FInterlaced
                then PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat).dwInterlaceFlags := AMINTERLACE_IsInterlaced
                else PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat).dwInterlaceFlags := 0;

              sample.SetMediaType(FInput.CurrentMediaType.MediaType);
              sample.SetDiscontinuity(True);
              FOutput.QueryAccept(FInput.CurrentMediaType.MediaType^);
              FOutput.DeliverBeginFlush;
              FOutput.DeliverEndFlush;

              OutputDebugString(PChar('Mediatype changed'));
            end;

            str := inttostr(w) + ' x '+
                   inttostr(h) + ' x ' +
                   inttostr(pic_width_in_mbs_minus1) + ' x ' +
                   inttostr(frame_mbs_only_flag) + ' x ' +
                   inttostr(pic_height_in_map_units_minus1);

            OutputDebugString(PChar(str));
          end;


        end;
        inc(buffer);
        dec(size);
      end;
    end else
    begin
      while (size > 8) do
      begin
        if PCardinal(buffer)^ = SEQUENCE_HEADER then
        begin
          FSequenceHeader.ParseBuffer(buffer);
          inc(buffer, 7);
          if (FCurrentAspectRatio <> FSequenceHeader.AspectRatio) then
          begin
            FCurrentAspectRatio := FSequenceHeader.AspectRatio;
            if Assigned(FCallback)
              then FCallback.put_AspectRatio(FCurrentAspectRatio);
            ResizeFFDShow;
  //          OutputDebugString(PChar(
  //            'Size: ' + inttostr(FSequenceHeader.Width) + 'x' + inttostr(FSequenceHeader.Height) +
  //            ' AspectRatio: ' + GetAspectRatioString(FSequenceHeader.AspectRatio) +
  //            ' FrameRate: ' + GetFrameRateString(FSequenceHeader.FrameRate) +
  //            ' BitRate: ' + inttostr(FSequenceHeader.BitRate div 1000) + ' kbps'
  //          ));
          end;
          if Assigned(FVideoCallback) then
          begin
            FVideoCallback(FDecoderMode, FSequenceHeader.Width, FSequenceHeader.Height, FSequenceHeader.AspectRatio, FSequenceHeader.FrameRate, FSequenceHeader.BitRate);
          end;
          Exit;
        end;
        inc(buffer);
        dec(size);
      end;
    end;

  finally
    FVideoLock.UnLock;
  end;
end;

procedure TDVBVideoFilter.put_Callback(ACallback: ISubtitleCallback);
begin
  FVideoLock.Lock;
  try
    FCallback := ACallback;
  finally
    FVideoLock.UnLock;
  end;
end;

procedure TDVBVideoFilter.put_Enabled(AEnabled: Boolean);
begin
  FVideoLock.Lock;
  try
    FEnabled := AEnabled;
  finally
    FVideoLock.UnLock;
  end;
end;

procedure TDVBVideoFilter.put_InfoCallback(ACallback: TVideoInfoCallback);
begin
  FVideoLock.Lock;
  try
    FVideoCallback := ACallback;
  finally
    FVideoLock.UnLock;
  end;
end;

procedure TDVBVideoFilter.put_BytesCallback(ACallback: TVideoBytesCallback);
begin
  FVideoLock.Lock;
  try
    FBytesCallback := ACallback;
  finally
    FVideoLock.UnLock;
  end;
end;

procedure TDVBVideoFilter.put_FlushStream(AFlush: Boolean);
begin
  FVideoLock.Lock;
  try
    FNextFlush := AFlush;
  finally
    FVideoLock.UnLock;
  end;
end;

function TDVBVideoFilter.CompleteConnect(dir: _PinDirection; ReceivePin: IPin): HRESULT;
var
  pin_info: TPinInfo;
const
  MEDIASUBTYPE_H264: TGuid = '{8D2D71CB-243F-45E3-B2D8-5FD7967EC09B}';
begin
  Result := inherited CompleteConnect(dir, ReceivePin);
  if (Result = S_OK) and (dir = PINDIR_OUTPUT) then
  begin
    if ReceivePin.QueryPinInfo(pin_info) = S_OK then
    begin
      FFFDShow := nil;
      if pin_info.pFilter.QueryInterface(IID_IffDecoder, FFFDShow) = S_OK then
      begin
        FFFDShow.putParam(IDFF_resizeMode, 1);
        FFFDShow.putParam(IDFF_isAspect, 0);
        FFFDShow.putParam(IDFF_isResize, 1);
      end;
      ResizeFFDShow;
    end;

    if IsEqualGUID(FInput.CurrentMediaType.MediaType.subtype, MEDIASUBTYPE_H264) or
       IsEqualGUID(FInput.CurrentMediaType.MediaType.subtype, FOURCCMap(FCC('h264'))) then
    begin
      FDecoderMode   := dmH264;
      FCurrentWidth  := PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat)^.bmiHeader.biWidth;
      FCurrentHeight := PVideoInfoHeader2(FInput.CurrentMediaType.MediaType.pbFormat)^.bmiHeader.biHeight;
    end else
    begin
      FDecoderMode := dmMPEG2;
    end;
  end;
end;

procedure TDVBVideoFilter.ResizeFFDShow;
begin
  if Assigned(FFFDShow) then
  begin
    case FCurrentAspectRatio of
      ar4_3:
      begin
        FFFDShow.putParam(IDFF_resizeA1, 4);
        FFFDShow.putParam(IDFF_resizeA2, 3);
      end;
      ar16_9:
      begin
        FFFDShow.putParam(IDFF_resizeA1, 16);
        FFFDShow.putParam(IDFF_resizeA2, 9);
      end;
      ar221_1:
      begin
        FFFDShow.putParam(IDFF_resizeA1, 221);
        FFFDShow.putParam(IDFF_resizeA2, 100);
      end;
    end;
  end;
end;

end.
