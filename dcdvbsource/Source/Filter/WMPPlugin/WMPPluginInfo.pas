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

unit WMPPluginInfo;

interface

uses
  Windows, WMPPluginAPI, ActiveX, formWMPPluginInfo, formWMPAbout, Forms, SysUtils;

const
  CLSID_WMPPluginInfo: TGuid = '{5E6D8602-4C94-4FEA-90A5-B94551CCF4A5}';

type
  TWMPPluginInfo = class(TWMPUIPlugin)
  public
    procedure OpenStateChange(ANewState: Integer); override; stdcall;
  end;

implementation

procedure TWMPPluginInfo.OpenStateChange(ANewState: Integer);
begin
  if TWMPOpenState(ANewState) = wmposMediaOpen then
  begin
    if Assigned(FForm)
      then TfrmWMPPluginInfo(FForm).UpdateInfo;
    Exit;
  end;

  if TWMPOpenState(ANewState) = wmposPlaylistOpenNoMedia then
  begin
    if Assigned(FForm)
      then TfrmWMPPluginInfo(FForm).ClearInfo;
    Exit;
  end;
end;

initialization

  TWMPUIPluginClassFactory.Create
  (

    TWMPPluginInfo,                               // TWMPUIPlugin sublcass
    'TV Info',                            // Plugin Name
    'TV Info Plugin for DC-DVB Filter',   // Plugin Description
    CLSID_WMPPluginInfo,                          // Unique Plugin Class ID
    // Plugin Type. Must be one of PLUGIN_TYPE_XXX and can be combined with
    // PlUGIN_FLAGS_XXX. Check the WMPSDK Help for more Informations.
    PLUGIN_FLAGS_INSTALLAUTORUN or PLUGIN_FLAGS_HASPROPERTYPAGE or PLUGIN_TYPE_SETTINGSAREA,
    TfrmWMPPluginInfo,                            // Window Form
    TfrmWMPAbout                              // Property Form (The one in "Tools -> Options -> Plug-Ins")
  );

end.
