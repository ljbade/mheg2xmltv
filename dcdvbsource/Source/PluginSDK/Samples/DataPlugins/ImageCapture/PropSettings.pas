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

unit PropSettings;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls, ActiveX,
  ExtCtrls, Forms, DCDVBDataPlugins, ComCtrls, Menus, Spin, DCDVBShared,
  DirectShow9, Dialogs;

const
  CLSID_ImageCapturePropertyPage:   TGuid = '{92E137EB-0284-4486-9A41-B5E621D5926F}';
  IID_IImageCapture:                TGuid = '{1D1C4D0D-F7DB-455B-B512-A69519E65E57}';

type
  IImageCapture = interface(IUnknown)
  ['{1D1C4D0D-F7DB-455B-B512-A69519E65E57}']
    function get_VMRMixer(out AMixer: IVMRMixerBitmap9): HRESULT; stdcall;
  end;

  TfrmPropSettings = class(TDCDVBFormPropertyPage)
    TabControl1: TTabControl;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
  private
    FIntf: IImageCapture;
  published
    function OnConnect(Unknown: IInterface): HRESULT; override;
    function OnDisconnect: HRESULT; override;
  end;

implementation

{$R *.DFM}

function TfrmPropSettings.OnConnect(Unknown: IInterface): HRESULT;
begin
  if Unknown.QueryInterface(IID_IImageCapture, FIntf) <> S_OK
    then FIntf := nil;

  Result := S_OK;
end;

function TfrmPropSettings.OnDisconnect: HRESULT;
begin
  FIntf := nil;
  Result := S_OK;
end;

procedure TfrmPropSettings.Button1Click(Sender: TObject);
var
  mixer: IVMRMixerBitmap9;
  bitmap: TVMR9AlphaBitmap;
  bmp: TBitmap;
begin
  if Assigned(FIntf) then
  begin
    if FIntf.get_VMRMixer(mixer) = S_OK then
    begin
      bmp := TBitmap.Create;
      bmp.LoadFromFile('C:\sheep.bmp');
      bitmap.dwFlags := VMR9AlphaBitmap_hDC;
      bitmap.hdc := bmp.Canvas.Handle;
      bitmap.pDDS := nil;
      bitmap.rSrc := bmp.Canvas.ClipRect;
      bitmap.rDest.left := 0;
      bitmap.rDest.top := 0;
      bitmap.rDest.right := 0.5;
      bitmap.rDest.bottom := 0.5;
      bitmap.fAlpha := 0.5;
      bitmap.clrSrcKey := 0;//ColorToRGB(clWhite);
      bitmap.dwFilterMode := 0;
      mixer.SetAlphaBitmap(@bitmap);
      bmp.Free;
    end;
  end;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TfrmPropSettings,
    CLSID_ImageCapturePropertyPage
  );

end.
