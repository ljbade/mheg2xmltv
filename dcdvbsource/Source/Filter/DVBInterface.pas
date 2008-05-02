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

unit DVBInterface;

interface

uses
  MPEGSections, MPEGConst, DVBMHPParser, DVBRecordings, IDVBSource, DVBEPG,
  BDAConst, DVBSubtitlingParser, Graphics, DirectShow9;

(******************************************************************************)

const
  IID_ITeletextFilter         : TGuid = '{08233107-0223-42B3-8098-B25CEA1C0E42}';

type
  ITeletextFilter = interface(IUnknown)
  ['{08233107-0223-42B3-8098-B25CEA1C0E42}']
    function put_TeletextPID(APID: Integer): HRESULT; stdcall;
    function put_TeletextShow(Show: LongBool): HRESULT; stdcall;
    function put_TeletextTransparent(Transparent: LongBool): HRESULT; stdcall;
    function put_TeletextPage(Page: Integer; SubPage: Integer): HRESULT; stdcall;
    function get_TeletextPage(out Page: Integer; out SubPage: Integer): HRESULT; stdcall;
    function put_TeletextTransparency(Transparency: Integer): HRESULT; stdcall;
    function put_TeletextClear: HRESULT; stdcall;
    function put_SaveTeletext(AType: TSaveTeletextType; AFilename: PWideChar): HRESULT; stdcall;
    function put_TeletextNumber(ANumber: Integer): HRESULT; stdcall;
    function put_SizeMode(ASizeMode: TTeletextSizeMode): HRESULT; stdcall;
    function put_TeletextAspectRatio(AAspectRatio: Integer): HRESULT; stdcall;
    function put_TeletextFastext(AFastext: TTeletextFastext): HRESULT; stdcall;
    function put_ClickAt(AX, AY, AWidth, AHeight, AOffset: Integer): HRESULT; stdcall;
    function get_TeletextCursorHand(AX, AY, AWidth, AHeight, AOffset: Integer; out AHand: Boolean): HRESULT; stdcall;
    function put_TeletextFPS(AFPS: Integer): HRESULT; stdcall;
    function put_CycleTeletext: HRESULT; stdcall;
  end;

(******************************************************************************)

type
  ITeletextCallBack = interface(IUnknown)
  ['{6A4E9118-2D76-44F2-B991-73C136B6153B}']
    procedure OnTeletextBuffer(ABuffer: PByte; ASize: Integer);
  end;

  IDVBSourceFilter = interface(IUnknown)
  ['{922FDE08-3554-4203-94B6-7706B3CCC526}']
    procedure get_TSCallback(out ACallback: TBufferCallback);
    procedure put_TunerDevice(ATunerDevice: IUnknown);
    procedure put_Discontinuity;
  end;

(******************************************************************************)

const
  CLSID_AudioFilter           : TGuid = '{C1EE8E85-C41C-492B-A19E-235C67430157}';

type
  TAudioBytesCallback = procedure(ASize: Integer) of Object;
  TAudioInfoCallback = procedure(AChannel: Integer; ABitrate: Integer; ASamplerate: Integer) of Object;

  IAudioFilter = interface(IUnknown)
  ['{FAD3A1F0-8DBD-4E38-8EE5-6296D2AEBCD6}']
    procedure put_MediaType(MediaType: TAudioMediaType);
    procedure put_SBEEnabled(AEnabled: LongBool);
    procedure put_AudioBytesCallback(ACallback: TAudioBytesCallback);
    procedure put_AudioInfoCallback(ACallback: TAudioInfoCallback);
    procedure put_FlushStream(AFlush: Boolean);
  end;

(******************************************************************************)

const
  CLSID_DCDVBOSDFilter : TGuid = '{55397253-1262-4721-A4F1-4F5E1C8FBD14}';

type
  TOSDColor = record
    StartColor: TColor;
    EndColor: TColor;
    FontColor: TColor;
    FontShadowColor: TColor;
  end;

  IOSDFilter = interface(IUnknown)
  ['{A957CA95-1385-4241-84F0-E64D1564960B}']
    procedure put_OSDText(AChannel: String; ANow: String; ANext: String; AAlpha: Integer; ADuration: Integer);
    procedure put_OSDColor(AColor: TOSDColor);
    procedure put_OSDChannel(AChannel: String; AAlpha: Integer; ADuration: Integer);
    procedure put_OSDRect(ARect: VMR9NormalizedRect);
  end;

  IPCRCallback = interface(IUnknown)
  ['{5F904B52-BBF6-4F44-AB30-AD466FBA1DDB}']
    procedure OnPCRData(ABuffer: PByte; ASize: Integer);
  end;

  ISubtitleCallback = interface(IUnknown)
  ['{70E99725-013E-4170-8422-195C6B1BEF5C}']
    procedure OnSubtitleData(ABuffer: PByte; ASize: Integer);
    procedure put_PID(APCRPID: Integer; APID: Integer; ACompositionPageID: Integer; AAncillaryPageID: Integer);
    procedure put_AspectRatio(AAspectRatio: TAspectRatio);
  end;

(******************************************************************************)

const
  CLSID_EPGNIFilter: TGuid = '{5FB6167F-9CB7-4CD5-8342-3234630C465C}';

type
  TDSMCCBytesCallback = procedure(ASize: Integer) of Object;
  TPMTCallback = procedure(APMT: TProgramMapSection) of Object;

(******************************************************************************)

const
  CLSID_VideoFilter           : TGuid = '{3B037C44-5E0D-4EE9-AF30-371E69AB1F27}';

type
  TVideoInfoCallback = procedure(AMode: TVideoType; AWidth: Integer; AHeight: Integer; AAspectRatio: TAspectRatio; AFrameRate: TFrameRate; ABitRate: Int64) of Object;
  TVideoBytesCallback = procedure(ASize: Integer) of Object;

  IVideoFilter = interface(IUnknown)
  ['{EF16C743-3AD4-4E05-8BCF-8A1958F22825}']
    procedure put_Callback(ACallback: ISubtitleCallback);
    procedure put_InfoCallback(ACallback: TVideoInfoCallback);
    procedure put_BytesCallback(ACallback: TVideoBytesCallback);
    procedure put_Enabled(AEnabled: Boolean);
    procedure put_FlushStream(AFlush: Boolean);
  end;

(******************************************************************************)
const
  CLSID_PropertyPageSettings    : TGuid = '{B3EA3ACC-6948-4D2D-BEB5-5981ED82C537}';
  CLSID_PropertyPageAbout       : TGuid = '{6DC6C3D0-D3D3-4297-8655-5A0478F1DAB4}';
  CLSID_PropertyPageEPG         : TGuid = '{1D3B5BB2-3E9E-49AE-B088-7A2F37C0DAEA}';
  CLSID_PropertyPageMHP         : TGuid = '{80015312-DB1C-44CF-8F14-FF7252EA927B}';
  CLSID_PropertyPageRecordings  : TGuid = '{4DE3AFAD-FE96-46A8-B89A-B1774456ECDA}';
  CLSID_PropertyPageStatistics  : TGuid = '{1B65837B-2D49-4DF8-954F-A582B6B47278}';
  CLSID_PropertyPagePlugins     : TGuid = '{A25A418D-F986-4D22-87B8-597FDC46AA21}';


implementation

end.
