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

unit WMPPlugin;

interface

uses
  Windows, WMPPluginAPI, ActiveX, formWMPPlugin, formWMPAbout, Forms, SysUtils;

const
  CLSID_WMPPlugin: TGuid = '{63E7A5AC-F9F6-430E-8F9D-10D2F654CBD9}';

type
  TWMPPlugin = class(TWMPUIPlugin)
  public
    procedure OpenStateChange(ANewState: Integer); override; stdcall;
  end;

implementation

procedure TWMPPlugin.OpenStateChange(ANewState: Integer);
begin
  if TWMPOpenState(ANewState) = wmposMediaOpen then
  begin
    if Assigned(FForm)
      then TfrmWMPPlugin(FForm).UpdateList;
    Exit;
  end;

  if TWMPOpenState(ANewState) = wmposPlaylistOpenNoMedia then
  begin
    if Assigned(FForm)
      then TfrmWMPPlugin(FForm).ClearList;
    Exit;
  end;
end;

initialization

  TWMPUIPluginClassFactory.Create
  (

    TWMPPlugin,                               // TWMPUIPlugin sublcass
    'TV Channels',                            // Plugin Name
    'TV Channels Plugin for DC-DVB Filter',   // Plugin Description
    CLSID_WMPPlugin,                          // Unique Plugin Class ID
    // Plugin Type. Must be one of PLUGIN_TYPE_XXX and can be combined with
    // PlUGIN_FLAGS_XXX. Check the WMPSDK Help for more Informations.
    PLUGIN_FLAGS_INSTALLAUTORUN or PLUGIN_FLAGS_HASPROPERTYPAGE or PLUGIN_TYPE_METADATAAREA,
    TfrmWMPPlugin,                            // Window Form
    TfrmWMPAbout                              // Property Form (The one in "Tools -> Options -> Plug-Ins")
  );

end.
