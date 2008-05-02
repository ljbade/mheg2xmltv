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

unit UImageCapture;

interface

uses
  ActiveX, Windows, Classes, SysUtils, DCDVBDataPlugins, SyncObjs, DirectShow9,
  PropSettings, PropAbout, DCDVBShared;

const
  CLSID_ImageCapture: TGuid = '{9ED83352-EDE6-4915-B170-1CD48AEC481A}';

type
  TImageCapture = class(TDCDVBDataPlugin, IImageCapture)
  protected
    procedure Initialize; override;
  public
    // ICaptureControl
    function get_VMRMixer(out AMixer: IVMRMixerBitmap9): HRESULT; stdcall;
  end;

implementation

procedure TImageCapture.Initialize;
begin
  AddPropertyPage(CLSID_ImageCapturePropertyPage);
  AddPropertyPage(CLSID_ImageCapturePropertyPageAbout);
end;

function TImageCapture.get_VMRMixer(out AMixer: IVMRMixerBitmap9): HRESULT;
var
  enum: IEnumFilters;
  filter: IBaseFilter;
begin
  if not Assigned(@AMixer) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  if not Assigned(FFilterGraph) then
  begin
    Result := E_FAIL;
    Exit;
  end;

  if FFilterGraph.EnumFilters(enum) = S_OK then
  begin
    while enum.Next(1, filter, nil) = S_OK do
    begin
      if filter.QueryInterface(IID_IVMRMixerBitmap9, AMixer) = S_OK then
      begin
        Result := S_OK;
        Exit;
      end;
    end;
  end;

  Result := E_FAIL;
end;

initialization

  TDCDVBClassFactory.Create
  (
    TImageCapture,
    CLSID_ImageCapture,
    'Image Capture',
    'Image Capture',
    'Milenko Mitrovic',
    '0.0.0.1'
  );

end.
