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

unit WMPPluginAPI;

interface

{$MINENUMSIZE 4}
{$ALIGN ON}
{$RANGECHECKS OFF}

{$IFDEF VER150}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses
  Windows, ActiveX, Classes, ComObj, Forms, SysUtils;

const
  PLUGIN_TYPE_BACKGROUND                = $00000001;
  PLUGIN_TYPE_SEPARATEWINDOW            = $00000002;
  PLUGIN_TYPE_DISPLAYAREA               = $00000003;
  PLUGIN_TYPE_SETTINGSAREA              = $00000004;
  PLUGIN_TYPE_METADATAAREA              = $00000005;
  
  PLUGIN_FLAGS_HASPROPERTYPAGE          = $80000000;
  PLUGIN_FLAGS_INSTALLAUTORUN	          = $40000000;
  PLUGIN_FLAGS_LAUNCHPROPERTYPAGE       = $20000000;
  PLUGIN_FLAGS_ACCEPTSMEDIA	            = $10000000;
  PLUGIN_FLAGS_ACCEPTSPLAYLISTS	        = $08000000;
  PLUGIN_FLAGS_HASPRESETS	              = $04000000;
  PLUGIN_FLAGS_HIDDEN	                  = $02000000;

  PLUGIN_MISC_PRESETCOUNT               = 'PresetCount';
  PLUGIN_MISC_PRESETNAMES               = 'PresetNames';
  PLUGIN_MISC_CURRENTPRESET             = 'CurrentPreset';
  PLUGIN_SEPARATEWINDOW_RESIZABLE       = 'Resizable';
  PLUGIN_SEPARATEWINDOW_DEFAULTWIDTH    = 'DefaultWidth';
  PLUGIN_SEPARATEWINDOW_DEFAULTHEIGHT   = 'DefaultHeight';
  PLUGIN_SEPARATEWINDOW_MINWIDTH        = 'MinWidth';
  PLUGIN_SEPARATEWINDOW_MINHEIGHT       = 'MinHeight';
  PLUGIN_SEPARATEWINDOW_MAXWIDTH        = 'MaxWidth';
  PLUGIN_SEPARATEWINDOW_MAXHEIGHT       = 'MaxHeight';
  PLUGIN_MISC_QUERYDESTROY              = 'QueryDestroy';
  PLUGIN_ALL_MEDIASENDTO                = 'MediaSendTo';
  PLUGIN_ALL_PLAYLISTSENDTO             = 'PlaylistSendTo';

  CLSID_WMPSkinManager: TGuid           = '{B2A7FD52-301F-4348-B93A-638C6DE49229}';

  IID_IWMPErrorItem: TGuid              = '{3614C646-3B3B-4de7-A81E-930E3F2127B3}';
  IID_IWMPError: TGuid                  = '{A12DCF7D-14AB-4c1b-A8CD-63909F06025B}';
  IID_IWMPMedia: TGuid                  = '{94D55E95-3FAC-11d3-B155-00C04F79FAA6}';
  IID_IWMPControls: TGuid               = '{74C09E02-F828-11d2-A74B-00A0C905F36E}';
  IID_IWMPSettings: TGuid               = '{9104D1AB-80C9-4fed-ABF0-2E6417A6DF14}';
  IID_IWMPClosedCaption: TGuid          = '{4F2DF574-C588-11d3-9ED0-00C04FB6E937}';
  IID_IWMPPlaylist: TGuid               = '{D5F0F4F1-130C-11d3-B14E-00C04F79FAA6}';
  IID_IWMPCdrom: TGuid                  = '{cfab6e98-8730-11d3-b388-00c04f68574b}';
  IID_IWMPCdromCollection: TGuid        = '{EE4C8FE2-34B2-11d3-A3BF-006097C9B344}';
  IID_IWMPStringCollection: TGuid       = '{4a976298-8c0d-11d3-b389-00c04f68574b}';
  IID_IWMPMediaCollection: TGuid        = '{8363BC22-B4B4-4b19-989D-1CD765749DD1}';
  IID_IWMPPlaylistArray: TGuid          = '{679409c0-99f7-11d3-9fb7-00105aa620bb}';
  IID_IWMPPlaylistCollection: TGuid     = '{10A13217-23A7-439b-B1C0-D847C79B7774}';
  IID_IWMPNetwork: TGuid                = '{EC21B779-EDEF-462d-BBA4-AD9DDE2B29A7}';
  IID_IWMPCore: TGuid                   = '{D84CCA99-CCE2-11d2-9ECC-0000F8085981}';
  IID_IWMPPlayer: TGuid                 = '{6BF52A4F-394A-11d3-B153-00C04F79FAA6}';
  IID_IWMPPlayer2: TGuid                = '{0E6B01D1-D407-4c85-BF5F-1C01F6150280}';
  IID_IWMPMedia2: TGuid                 = '{AB7C88BB-143E-4ea4-ACC3-E4350B2106C3}';
  IID_IWMPControls2: TGuid              = '{6F030D25-0890-480f-9775-1F7E40AB5B8E}';
  IID_IWMPDVD: TGuid                    = '{8DA61686-4668-4a5c-AE5D-803193293DBE}';
  IID_IWMPCore2: TGuid                  = '{BC17E5B7-7561-4c18-BB90-17D485775659}';
  IID_IWMPPlayer3: TGuid                = '{54062B68-052A-4c25-A39F-8B63346511D4}';
  IID_IWMPErrorItem2: TGuid             = '{F75CCEC0-C67C-475c-931E-8719870BEE7D}';
  IID_IWMPRemoteMediaServices: TGuid    = '{CBB92747-741F-44fe-AB5B-F1A48F3B2A59}';
  IID_IWMPSkinManager: TGuid            = '{076F2FA6-ED30-448B-8CC5-3F3EF3529C7A}';
  IID_IWMPMetadataPicture: TGuid        = '{5C29BBE0-F87D-4c45-AA28-A70F0230FFA9}';
  IID_IWMPMetadataText: TGuid           = '{769A72DB-13D2-45e2-9C48-53CA9D5B7450}';
  IID_IWMPMedia3: TGuid                 = '{F118EFC7-F03A-4fb4-99C9-1C02A5C1065B}';
  IID_IWMPSettings2: TGuid              = '{FDA937A4-EECE-4da5-A0B6-39BF89ADE2C2}';
  IID_IWMPControls3: TGuid              = '{A1D1110E-D545-476a-9A78-AC3E4CB1E6BD}';
  IID_IWMPClosedCaption2: TGuid         = '{350BA78B-6BC8-4113-A5F5-312056934EB6}';
  IID_IWMPPlayerApplication: TGuid      = '{40897764-CEAB-47be-AD4A-8E28537F9BBF}';
  IID_IWMPCore3: TGuid                  = '{7587C667-628F-499f-88E7-6A6F4E888464}';
  IID_IWMPPlayer4: TGuid                = '{6C497D62-8919-413c-82DB-E935FB3EC584}';
  IID_IWMPPlayerServices: TGuid         = '{1D01FBDB-ADE2-4c8d-9842-C190B95C3306}';
  IID_IWMPSyncDevice: TGuid             = '{82A2986C-0293-4fd0-B279-B21B86C058BE}';
  IID_IWMPSyncServices: TGuid           = '{8B5050FF-E0A4-4808-B3A8-893A9E1ED894}';
  IID_IWMPPlayerServices2: TGuid        = '{1BB1592F-F040-418a-9F71-17C7512B4D70}';
  IID_IWMPEvents: TGuid                 = '{19A6627B-DA9E-47c1-BB23-00B5E668236A}';
  IID_IWMPEvents2: TGuid                = '{1E7601FA-47EA-4107-9EA9-9004ED9684FF}';
  IID_IWMPPluginUI: TGuid               = '{4C5E8F9F-AD3E-4bf9-9753-FCD30D6D38DD}';

type
  TWMPOpenState = (
    wmposUndefined	                    = 0,
    wmposPlaylistChanging	              = wmposUndefined + 1,
    wmposPlaylistLocating	              = wmposPlaylistChanging + 1,
    wmposPlaylistConnecting	            = wmposPlaylistLocating + 1,
    wmposPlaylistLoading	              = wmposPlaylistConnecting + 1,
    wmposPlaylistOpening	              = wmposPlaylistLoading + 1,
    wmposPlaylistOpenNoMedia	          = wmposPlaylistOpening + 1,
    wmposPlaylistChanged	              = wmposPlaylistOpenNoMedia + 1,
    wmposMediaChanging	                = wmposPlaylistChanged + 1,
    wmposMediaLocating	                = wmposMediaChanging + 1,
    wmposMediaConnecting	              = wmposMediaLocating + 1,
    wmposMediaLoading	                  = wmposMediaConnecting + 1,
    wmposMediaOpening	                  = wmposMediaLoading + 1,
    wmposMediaOpen	                    = wmposMediaOpening + 1,
    wmposBeginCodecAcquisition       	  = wmposMediaOpen + 1,
    wmposEndCodecAcquisition	          = wmposBeginCodecAcquisition + 1,
    wmposBeginLicenseAcquisition       	= wmposEndCodecAcquisition + 1,
    wmposEndLicenseAcquisition	        = wmposBeginLicenseAcquisition + 1,
    wmposBeginIndividualization	        = wmposEndLicenseAcquisition + 1,
    wmposEndIndividualization	          = wmposBeginIndividualization + 1,
    wmposMediaWaiting	                  = wmposEndIndividualization + 1,
    wmposOpeningUnknownURL	            = wmposMediaWaiting + 1
  );

  TWMPPlayState = (
    wmppsUndefined      	              = 0,
    wmppsStopped	                      = wmppsUndefined + 1,
    wmppsPaused	                        = wmppsStopped + 1,
    wmppsPlaying	                      = wmppsPaused + 1,
    wmppsScanForward       	            = wmppsPlaying + 1,
    wmppsScanReverse       	            = wmppsScanForward + 1,
    wmppsBuffering	                    = wmppsScanReverse + 1,
    wmppsWaiting	                      = wmppsBuffering + 1,
    wmppsMediaEnded	                    = wmppsWaiting + 1,
    wmppsTransitioning                  = wmppsMediaEnded + 1,
    wmppsReady	                        = wmppsTransitioning + 1,
    wmppsReconnecting	                  = wmppsReady + 1,
    wmppsLast	                          = wmppsReconnecting + 1
  );

  TWMPPlaylistChangeEventType = (
    wmplcUnknown	                      = 0,
    wmplcClear	                        = wmplcUnknown + 1,
    wmplcInfoChange	                    = wmplcClear + 1,
    wmplcMove	                          = wmplcInfoChange + 1,
    wmplcDelete	                        = wmplcMove + 1,
    wmplcInsert	                        = wmplcDelete + 1,
    wmplcAppend	                        = wmplcInsert + 1,
    wmplcPrivate	                      = wmplcAppend + 1,
    wmplcNameChange	                    = wmplcPrivate + 1,
    wmplcMorph	                        = wmplcNameChange + 1,
    wmplcSort	                          = wmplcMorph + 1,
    wmplcLast	                          = wmplcSort + 1
  );

  TWMPSyncState = (
    wmpssUnknown	                      = 0,
    wmpssSynchronizing	                = wmpssUnknown + 1,
    wmpssStopped	                      = wmpssSynchronizing + 1,
    wmpssLast	                          = wmpssStopped + 1
  );

  TWMPDeviceStatus = (
    wmpdsUnknown	                      = 0,
    wmpdsPartnershipExists	            = wmpdsUnknown + 1,
    wmpdsPartnershipDeclined	          = wmpdsPartnershipExists + 1,
    wmpdsPartnershipAnother	            = wmpdsPartnershipDeclined + 1,
    wmpdsManualDevice	                  = wmpdsPartnershipAnother + 1,
    wmpdsNewDevice	                    = wmpdsManualDevice + 1,
    wmpdsLast	                          = wmpdsNewDevice + 1
  );

  IWMPErrorItem = interface(IDispatch)
  ['{3614C646-3B3B-4de7-A81E-930E3F2127B3}']
    function get_errorCode(out AErrorCode: HRESULT): HRESULT; stdcall;
    function get_errorDescription(out ADescription: PWideChar): HRESULT; stdcall;
    function get_errorContext(out AContext: POleVariant): HRESULT; stdcall;
    function get_remedy(out ARemedy: Integer): HRESULT; stdcall;
    function get_customUrl(out ACustomUrl: PWideChar): HRESULT; stdcall;
  end;

  IWMPError = interface(IDispatch)
  ['{A12DCF7D-14AB-4c1b-A8CD-63909F06025B}']
    function clearErrorQueue: HRESULT; stdcall;
    function get_errorCount(out ANumErrors: Integer): HRESULT; stdcall;
    function get_item(AIndex: Integer; out AErrorItem: IWMPErrorItem): HRESULT; stdcall;
    function webHelp: HRESULT; stdcall;
  end;

  IWMPPlaylist = interface;

  IWMPMedia = interface(IDispatch)
  ['{94D55E95-3FAC-11d3-B155-00C04F79FAA6}']
    function get_isIdentical(AWMPMedia: IWMPMedia; out AIdentical: WordBool): HRESULT; stdcall;
    function get_sourceURL(out ASourceURL: PWideChar): HRESULT; stdcall;
    function get_name(out AName: PWideChar): HRESULT; stdcall;
    function put_name(AName: PWideChar): HRESULT; stdcall;
    function get_imageSourceWidth(out AWidth: Integer): HRESULT; stdcall;
    function get_imageSourceHeight(out AHeight: Integer): HRESULT; stdcall;
    function get_markerCount(out AMarkerCount: Integer): HRESULT; stdcall;
    function getMarkerTime(AMarkerNum: Integer; out AMarkerTime: Double): HRESULT; stdcall;
    function getMarkerName(AMarkerNum: Integer; out AMarkerName: PWideChar): HRESULT; stdcall;
    function get_duration(out ADuration: Double): HRESULT; stdcall;
    function get_durationString(out ADuration: PWideChar): HRESULT; stdcall;
    function get_attributeCount(out ACount: Integer): HRESULT; stdcall;
    function getAttributeName(AIndex: Integer; out AItemName: PWideChar): HRESULT; stdcall;
    function getItemInfo(AItemName: PWideChar; out AValue: PWideChar): HRESULT; stdcall;
    function setItemInfo(AItemName: PWideChar; AValue: PWideChar): HRESULT; stdcall;
    function getItemInfoByAtom(AAtom: Integer; out AVaue: PWideChar): HRESULT; stdcall;
    function isMemberOf(APlaylist: IWMPPlaylist; out AIsMemberOf: WordBool): HRESULT; stdcall;
    function isReadOnlyItem(AItemName: PWideChar; out AIsReadOnly: WordBool): HRESULT; stdcall;
  end;

  IWMPControls = interface(IDispatch)
  ['{74C09E02-F828-11d2-A74B-00A0C905F36E}']
    function get_isAvailable(AItem: PWideChar; out AIsAvailable: WordBool): HRESULT; stdcall;
    function play: HRESULT; stdcall;
    function stop: HRESULT; stdcall;
    function pause: HRESULT; stdcall;
    function fastForward: HRESULT; stdcall;
    function fastReverse: HRESULT; stdcall;
    function get_currentPosition(out ACurrentPosition: Double): HRESULT; stdcall;
    function put_currentPosition(ACurrentPosition: Double): HRESULT; stdcall;
    function get_currentPositionString(out ACurrentPosition: PWideChar): HRESULT; stdcall;
    function next: HRESULT; stdcall;
    function previous: HRESULT; stdcall;
    function get_currentItem(out AMedia: IWMPMedia): HRESULT; stdcall;
    function put_currentItem(AMedia: IWMPMedia): HRESULT; stdcall;
    function get_currentMarker(out AMarker: Integer): HRESULT; stdcall;
    function put_currentMarker(AMarker: Integer): HRESULT; stdcall;
    function playItem(AMedia: IWMPMedia): HRESULT; stdcall;
  end;

  IWMPSettings = interface(IDispatch)
  ['{9104D1AB-80C9-4fed-ABF0-2E6417A6DF14}']
    function get_isAvailable(AItem: PWideChar; out AIsAvailable: WordBool): HRESULT; stdcall;
    function get_autoStart(out AAutoStart: WordBool): HRESULT; stdcall;
    function put_autoStart(AAutoStart: WordBool): HRESULT; stdcall;
    function get_baseURL(out ABaseURL: PWideChar): HRESULT; stdcall;
    function put_baseURL(ABaseURL: PWideChar): HRESULT; stdcall;
    function get_defaultFrame(out ADefaultFrame: PWideChar): HRESULT; stdcall;
    function put_defaultFrame(ADefaultFrame: PWideChar): HRESULT; stdcall;
    function get_invokeURLs(out AInvokeURLs: WordBool): HRESULT; stdcall;
    function put_invokeURLs(AInvokeURLs: WordBool): HRESULT; stdcall;
    function get_mute(out AMute: WordBool): HRESULT; stdcall;
    function put_mute(AMute: WordBool): HRESULT; stdcall;
    function get_playCount(out ACount: Integer): HRESULT; stdcall;
    function put_playCount(ACount: Integer): HRESULT; stdcall;
    function get_rate(out ARate: Double): HRESULT; stdcall;
    function put_rate(ARate: Double): HRESULT; stdcall;
    function get_balance(out ABalance: Integer): HRESULT; stdcall;
    function put_balance(ABalance: Integer): HRESULT; stdcall;
    function get_volume(out AVolume: Integer): HRESULT; stdcall;
    function put_volume(AVolume: Integer): HRESULT; stdcall;
    function getMode(AMode: PWideChar; out AModeSet: WordBool): HRESULT; stdcall;
    function setMode(AMode: PWideChar; AModeSet: WordBool): HRESULT; stdcall;
    function get_enableErrorDialogs(out AEnableErrorDialogs: WordBool): HRESULT; stdcall;
    function put_enableErrorDialogs(AEnableErrorDialogs: WordBool): HRESULT; stdcall;
  end;

  IWMPClosedCaption = interface(IDispatch)
  ['{4F2DF574-C588-11d3-9ED0-00C04FB6E937}']
    function get_SAMIStyle(out ASAMIStyle: PWideChar): HRESULT; stdcall;
    function put_SAMIStyle(ASAMIStyle: PWideChar): HRESULT; stdcall;
    function get_SAMILang(out ASAMILang: PWideChar): HRESULT; stdcall;
    function put_SAMILang(ASAMILang: PWideChar): HRESULT; stdcall;
    function get_SAMIFileName(out ASAMIFileName: PWideChar): HRESULT; stdcall;
    function put_SAMIFileName(ASAMIFileName: PWideChar): HRESULT; stdcall;
    function get_captioningId(out ACaptioningID: PWideChar): HRESULT; stdcall;
    function put_captioningId(ACaptioningID: PWideChar): HRESULT; stdcall;
  end;
    
  IWMPPlaylist = interface(IDispatch)
  ['{D5F0F4F1-130C-11d3-B14E-00C04F79FAA6}']
    function get_count(out ACount: Integer): HRESULT; stdcall;
    function get_name(out AName: PWideChar): HRESULT; stdcall;
    function put_name(AName: PWideChar): HRESULT; stdcall;
    function get_attributeCount(out ACount: Integer): HRESULT; stdcall;
    function get_attributeName(AIndex: Integer; out AAttributeName: PwideChar): HRESULT; stdcall;
    function get_item(AIndex: Integer; out AWMPMedia: IWMPMedia): HRESULT; stdcall;
    function getItemInfo(AName: PwideChar; out AValue: PWideChar): HRESULT; stdcall;
    function setItemInfo(AName: PWideChar; AValue: PwideChar): HRESULT; stdcall;
    function get_isIdentical(AWMPPlaylist: IWMPPlaylist; out AValueSet: WordBool): HRESULT; stdcall;
    function clear: HRESULT; stdcall;
    function insertItem(AIndex: Integer; AWMPMedia: IWMPMedia): HRESULT; stdcall;
    function appendItem(AWMPMedia: IWMPMedia): HRESULT; stdcall;
    function removeItem(AWMPMedia: IWMPMedia): HRESULT; stdcall;
    function moveItem(AIndexOld: Integer; AIndexNew: Integer): HRESULT; stdcall;
  end;
    
  IWMPCdrom = interface(IDispatch)
  ['{cfab6e98-8730-11d3-b388-00c04f68574b}']
    function get_driveSpecifier(out ADrive: PWideChar): HRESULT; stdcall;
    function get_playlist(out APlaylist: IWMPPlaylist): HRESULT; stdcall;
    function eject: HRESULT; stdcall;
  end;

  IWMPCdromCollection = interface(IDispatch)
  ['{EE4C8FE2-34B2-11d3-A3BF-006097C9B344}']
    function get_count(out ACount: Integer): HRESULT; stdcall;
    function item(AIndex: Integer; out AItem: IWMPCdrom): HRESULT; stdcall;
    function getByDriveSpecifier(ADriveSpecifier: PWideChar; out ACDRom: IWMPCdrom): HRESULT; stdcall;
  end;

  IWMPStringCollection = interface(IDispatch)
  ['{4a976298-8c0d-11d3-b389-00c04f68574b}']
    function get_count(out ACount: Integer): HRESULT; stdcall;
    function item(AIndex: Integer; out AString: PWideChar): HRESULT; stdcall;
 end;

  IWMPMediaCollection = interface(IDispatch)
  ['{8363BC22-B4B4-4b19-989D-1CD765749DD1}']
    function add(AURL: PWideChar; out AItem: IWMPMedia): HRESULT; stdcall;
    function getAll(out AMediaItems: IWMPPlaylist): HRESULT; stdcall;
    function getByName(AName: PWideChar; out MediaItems: IWMPPlaylist): HRESULT; stdcall;
    function getByGenre(AGenre: PwideChar; out AMediaItems: IWMPPlaylist): HRESULT; stdcall;
    function getByAuthor(AAuthor: PWideChar; out AMediaItems: IWMPPlaylist): HRESULT; stdcall;
    function getByAlbum(AAlbum: PWideChar; out AMediaItems: IWMPPlaylist): HRESULT; stdcall;
    function getByAttribute(AAttribute: PWideChar; AValue: PWideChar; out AMediaItems: IWMPPlaylist): HRESULT; stdcall;
    function remove(AItem: IWMPMedia; ADeleteFile: WordBool): HRESULT; stdcall;
    function getAttributeStringCollection(AAttribute: PWideChar; AMediaType: PWideChar; out AStringCollection: IWMPStringCollection): HRESULT; stdcall;
    function getMediaAtom(AItemName: PWideChar; out AAtom: Integer): HRESULT; stdcall;
    function setDeleted(AItem: IWMPMedia; AIsDeleted: WordBool): HRESULT; stdcall;
    function isDeleted(AItem: IWMPMedia; out AIsDeleted: WordBool): HRESULT; stdcall;
  end;
    
  IWMPPlaylistArray = interface(IDispatch)
  ['{679409c0-99f7-11d3-9fb7-00105aa620bb}']
    function get_count(out ACount: Integer): HRESULT; stdcall;
    function item(AIndex: Integer; out AItme: IWMPPlaylist): HRESULT; stdcall;
  end;
    
  IWMPPlaylistCollection = interface(IDispatch)
  ['{10A13217-23A7-439b-B1C0-D847C79B7774}']
    function newPlaylist(AName: PWideChar; out AItem: IWMPPlaylist): HRESULT; stdcall;
    function getAll(out AWMPPlaylistArray: IWMPPlaylistArray): HRESULT; stdcall;
    function getByName(AName: PWideChar; out APlaylistArray: IWMPPlaylistArray): HRESULT; stdcall;
    function remove(AItem: IWMPPlaylist): HRESULT; stdcall;
    function setDeleted(AItem: IWMPPlaylist; AIsDeleted: WordBool): HRESULT; stdcall;
    function isDeleted(AItem: IWMPPlaylist; AIsDeleted: WordBool): HRESULT; stdcall;
    function importPlaylist(AItem: IWMPPlaylist; out AImportedItem: IWMPPlaylist): HRESULT; stdcall;
  end;

  IWMPNetwork = interface(IDispatch)
  ['{EC21B779-EDEF-462d-BBA4-AD9DDE2B29A7}']
    function get_bandWidth(out ABandwidth: Integer): HRESULT; stdcall;
    function get_recoveredPackets(out ARecoveredPackets: Integer): HRESULT; stdcall;
    function get_sourceProtocol(out ASourceProtocol: PWideChar): HRESULT; stdcall;
    function get_receivedPackets(out AReceivedPackets: Integer): HRESULT; stdcall;
    function get_lostPackets(out ALostPackets: Integer): HRESULT; stdcall;
    function get_receptionQuality(out AReceptionQuality: Integer): HRESULT; stdcall;
    function get_bufferingCount(out ABufferingCount: Integer): HRESULT; stdcall;
    function get_bufferingProgress(out ABufferingProgress: Integer): HRESULT; stdcall;
    function get_bufferingTime(out ABufferingTime: Integer): HRESULT; stdcall;
    function put_bufferingTime(ABufferingTime: Integer): HRESULT; stdcall;
    function get_frameRate(out AFrameRate: Integer): HRESULT; stdcall;
    function get_maxBitRate(out ABitRate: Integer): HRESULT; stdcall;
    function get_bitRate(out ABitRate: Integer): HRESULT; stdcall;
    function getProxySettings(AProtocol: PWideChar; out AProxySetting: Integer): HRESULT; stdcall;
    function setProxySettings(AProtocol: PWideChar; AProxySetting: Integer): HRESULT; stdcall;
    function getProxyName(AProtocol: PWideChar; out AProxyName: PWideChar): HRESULT; stdcall;
    function setProxyName(AProtocol: PWideChar; AProxyName: PWideChar): HRESULT; stdcall;
    function getProxyPort(AProtocol: PWideChar; out AProxyPort: Integer): HRESULT; stdcall;
    function setProxyPort(AProtocol: PWideChar; AProxyPort: Integer): HRESULT; stdcall;
    function getProxyExceptionList(AProtocol: PWideChar; out AExceptionList: PWideChar): HRESULT; stdcall;
    function setProxyExceptionList(AProtocol: PWideChar; AExceptionList: PWideChar): HRESULT; stdcall;
    function getProxyBypassForLocal(AProtocol: PWideChar; out ABypassForLocal: WordBool): HRESULT; stdcall;
    function setProxyBypassForLocal(AProtocol: PWideChar; ABypassForLocal: WordBool): HRESULT; stdcall;
    function get_maxBandwidth(out AMaxBandwidth: Integer): HRESULT; stdcall;
    function put_maxBandwidth(out AMaxBandwidth: Integer): HRESULT; stdcall;
    function get_downloadProgress(out ADownloadProgress: Integer): HRESULT; stdcall;
    function get_encodedFrameRate(out AFrameRate: Integer): HRESULT; stdcall;
    function get_framesSkipped(out AFrames: Integer): HRESULT; stdcall;
  end;
    
  IWMPCore = interface(IDispatch)
  ['{D84CCA99-CCE2-11d2-9ECC-0000F8085981}']
    function close: HRESULT; stdcall;
    function get_URL(out AURL: PWideChar): HRESULT; stdcall;
    function put_URL(AURL: PWideChar): HRESULT; stdcall;
    function get_openState(out AOpenState: TWMPOpenState): HRESULT; stdcall;
    function get_playState(out APlayState: TWMPPlayState): HRESULT; stdcall;
    function get_controls(out AControl: IWMPControls): HRESULT; stdcall;
    function get_settings(out ASettings: IWMPSettings): HRESULT; stdcall;
    function get_currentMedia(out AMedia: IWMPMedia): HRESULT; stdcall;
    function put_currentMedia(AMedia: IWMPMedia): HRESULT; stdcall;
    function get_mediaCollection(out AMediaCollection: IWMPMediaCollection): HRESULT; stdcall;
    function get_playlistCollection(out APlaylistCollection: IWMPPlaylistCollection): HRESULT; stdcall;
    function get_versionInfo(out AVersionInfo: PWideChar): HRESULT; stdcall;
    function launchURL(AURL: PWideChar): HRESULT; stdcall;
    function get_network(out ANetwork: IWMPNetwork): HRESULT; stdcall;
    function get_currentPlaylist(out APlaylist: IWMPPlaylist): HRESULT; stdcall;
    function put_currentPlaylist(APlaylist: IWMPPlaylist): HRESULT; stdcall;
    function get_cdromCollection(out ACDRomCollection: IWMPCdromCollection): HRESULT; stdcall;
    function get_closedCaption(out AClosedCaption: IWMPClosedCaption): HRESULT; stdcall;
    function get_isOnline(out AOnline: WordBool): HRESULT; stdcall;
    function get_error(out AError: IWMPError): HRESULT; stdcall;
    function get_status(out AStatus: PWideChar): HRESULT; stdcall;
  end;

  IWMPPlayer = interface(IWMPCore)
  ['{6BF52A4F-394A-11d3-B153-00C04F79FAA6}']
    function get_enabled(out AEnabled: WordBool): HRESULT; stdcall;
    function put_enabled(AEnabled: WordBool): HRESULT; stdcall;
    function get_fullScreen(out AFullScreen: WordBool): HRESULT; stdcall;
    function put_fullScreen(AFullScreen: WordBool): HRESULT; stdcall;
    function get_enableContextMenu(out AEnableContextMenu: WordBool): HRESULT; stdcall;
    function put_enableContextMenu(AEnableContextMenu: WordBool): HRESULT; stdcall;
    function put_uiMode(AMode: PWideChar): HRESULT; stdcall;
    function get_uiMode(out AMode: PWideChar): HRESULT; stdcall;
        
  end;

  IWMPPlayer2 = interface(IWMPCore)
  ['{0E6B01D1-D407-4c85-BF5F-1C01F6150280}']
    function get_enabled(out AEnabled: WordBool): HRESULT; stdcall;
    function put_enabled(AEnabled: WordBool): HRESULT; stdcall;
    function get_fullScreen(out AFullScreen: WordBool): HRESULT; stdcall;
    function put_fullScreen(AFullScreen: WordBool): HRESULT; stdcall;
    function get_enableContextMenu(out AEnableContextMenu: WordBool): HRESULT; stdcall;
    function put_enableContextMenu(AEnableContextMenu: WordBool): HRESULT; stdcall;
    function put_uiMode(AMode: PWideChar): HRESULT; stdcall;
    function get_uiMode(out AMode: PWideChar): HRESULT; stdcall;
    function get_stretchToFit(out AStretchToFit: WordBool): HRESULT; stdcall;
    function put_stretchToFit(AStretchToFit: WordBool): HRESULT; stdcall;
    function get_windowlessVideo(out AWindowLess: WordBool): HRESULT; stdcall;
    function put_windowlessVideo(AWindowLess: WordBool): HRESULT; stdcall;
  end;

  IWMPMedia2 = interface(IWMPMedia)
  ['{AB7C88BB-143E-4ea4-ACC3-E4350B2106C3}']
    function get_error(out AWMPErrorItem: IWMPErrorItem): HRESULT; stdcall;         
  end;

  IWMPControls2 = interface(IWMPControls)
  ['{6F030D25-0890-480f-9775-1F7E40AB5B8E}']
    function step(AStep: Integer): HRESULT; stdcall;
  end;

  IWMPDVD = interface(IDispatch)
  ['{8DA61686-4668-4a5c-AE5D-803193293DBE}']
    function get_isAvailable(AItem: PWideChar; out AAvailable: WordBool): HRESULT; stdcall;
    function get_domain(out ADomain: PWideChar): HRESULT; stdcall;
    function topMenu: HRESULT; stdcall;
    function titleMenu: HRESULT; stdcall;
    function back: HRESULT; stdcall;
    function resume: HRESULT; stdcall;
  end;

  IWMPCore2 = interface(IWMPCore)
  ['{BC17E5B7-7561-4c18-BB90-17D485775659}']
    function get_dvd(out ADVD: IWMPDVD): HRESULT; stdcall;
  end;

  IWMPPlayer3 = interface(IWMPCore2)
  ['{54062B68-052A-4c25-A39F-8B63346511D4}']
    function get_enabled(out AEnabled: WordBool): HRESULT; stdcall;
    function put_enabled(AEnabled: WordBool): HRESULT; stdcall;
    function get_fullScreen(out AFullScreen: WordBool): HRESULT; stdcall;
    function put_fullScreen(AFullScreen: WordBool): HRESULT; stdcall;
    function get_enableContextMenu(out AEnableContextMenu: WordBool): HRESULT; stdcall;
    function put_enableContextMenu(AEnableContextMenu: WordBool): HRESULT; stdcall;
    function put_uiMode(AMode: PWideChar): HRESULT; stdcall;
    function get_uiMode(out AMode: PWideChar): HRESULT; stdcall;
    function get_stretchToFit(out AStretchToFit: WordBool): HRESULT; stdcall;
    function put_stretchToFit(AStretchToFit: WordBool): HRESULT; stdcall;
    function get_windowlessVideo(out AWindowLess: WordBool): HRESULT; stdcall;
    function put_windowlessVideo(AWindowLess: WordBool): HRESULT; stdcall;
  end;

  IWMPErrorItem2 = interface(IWMPErrorItem)
  ['{F75CCEC0-C67C-475c-931E-8719870BEE7D}']
    function get_condition(out ACondition: Integer): HRESULT; stdcall;
  end;

  IWMPRemoteMediaServices = interface(IUnknown)
  ['{CBB92747-741F-44fe-AB5B-F1A48F3B2A59}']
    function GetServiceType(out AType: PWideChar): HRESULT; stdcall;
    function GetApplicationName(out AName: PWideChar): HRESULT; stdcall;
    function GetScriptableObject(out AName: PWideChar; out ADispatch: IDispatch): HRESULT; stdcall;
    function GetCustomUIMode(out AFile: PWideChar): HRESULT; stdcall;
  end;

  IWMPSkinManager = interface(IUnknown)
  ['{076F2FA6-ED30-448B-8CC5-3F3EF3529C7A}']
    function SetVisualStyle(APath: PWideChar): HRESULT; stdcall;        
  end;

  IWMPMetadataPicture = interface(IDispatch)
  ['{5C29BBE0-F87D-4c45-AA28-A70F0230FFA9}']
    function get_mimeType(out AMimeType: PWideChar): HRESULT; stdcall;
    function get_pictureType(out APictureType: PWideChar): HRESULT; stdcall;
    function get_description(out ADescription: PWideChar): HRESULT; stdcall;
    function get_URL(out AURL: PWideChar): HRESULT; stdcall;
  end;

  IWMPMetadataText = interface(IDispatch)
  ['{769A72DB-13D2-45e2-9C48-53CA9D5B7450}']
    function get_description(out ADescription: PWideChar): HRESULT; stdcall;
    function get_text(out AText: PWideChar): HRESULT; stdcall;
  end;

  IWMPMedia3 = interface(IWMPMedia2)
  ['{F118EFC7-F03A-4fb4-99C9-1C02A5C1065B}']
    function getAttributeCountByType(AType: PWideChar; ALanguage: PWideChar; out ACount: Integer): HRESULT; stdcall;
    function getItemInfoByType(AType: PWideChar; ALanguage: PWideChar; AIndex: Integer; out AValue: POleVariant): HRESULT; stdcall;
  end;

  IWMPSettings2 = interface(IWMPSettings)
  ['{FDA937A4-EECE-4da5-A0B6-39BF89ADE2C2}']
    function get_defaultAudioLanguage(out ALangID: Integer): HRESULT; stdcall;
    function get_mediaAccessRights(out ARights: PWideChar): HRESULT; stdcall;
    function requestMediaAccessRights(ADesiredAccess: PWideChar; out AAccepted: WordBool): HRESULT; stdcall;
  end;

  IWMPControls3 = interface(IWMPControls2)
  ['{A1D1110E-D545-476a-9A78-AC3E4CB1E6BD}']
    function get_audioLanguageCount(ACount: Integer): HRESULT; stdcall;
    function getAudioLanguageID(AIndex: Integer; out ALangID: Integer): HRESULT; stdcall;
    function getAudioLanguageDescription(AIndex: Integer; out AlangDesc: PWideChar): HRESULT; stdcall;
    function get_currentAudioLanguage(out ALangID: Integer): HRESULT; stdcall;
    function put_currentAudioLanguage(ALangID: Integer): HRESULT; stdcall;
    function get_currentAudioLanguageIndex(out ALangID: Integer): HRESULT; stdcall;
    function put_currentAudioLanguageIndex(AIndex: Integer): HRESULT; stdcall;
    function getLanguageName(ALangID: Integer; out ALangName: PWideChar): HRESULT; stdcall;
    function get_currentPositionTimecode(out ATimecode: PWideChar): HRESULT; stdcall;
    function put_currentPositionTimecode(ATimecode: PWideChar): HRESULT; stdcall;
  end;

  IWMPClosedCaption2 = interface(IWMPClosedCaption)
  ['{350BA78B-6BC8-4113-A5F5-312056934EB6}']
    function get_SAMILangCount(out ACount: Integer): HRESULT; stdcall;
    function getSAMILangName(AIndex: Integer; out AName: PWideChar): HRESULT; stdcall;
    function getSAMILangID(AIndex: Integer; out ALangID: Integer): HRESULT; stdcall;
    function get_SAMIStyleCount(out ACount: Integer): HRESULT; stdcall;
    function getSAMIStyleName(AIndex: Integer; out AName: PWideChar): HRESULT; stdcall;
  end;

  IWMPPlayerApplication = interface(IDispatch)
  ['{40897764-CEAB-47be-AD4A-8E28537F9BBF}']
    function switchToPlayerApplication: HRESULT; stdcall;
    function switchToControl: HRESULT; stdcall;
    function get_playerDocked(out APlayerDocked: WordBool): HRESULT; stdcall;
    function get_hasDisplay(out AHasDisplay: WordBool): HRESULT; stdcall;
  end;

  IWMPCore3 = interface(IWMPCore2)
  ['{7587C667-628F-499f-88E7-6A6F4E888464}']
    function newPlaylist(AName: PWideChar; AURL: PWideChar; out APlaylist: IWMPPlaylist): HRESULT; stdcall;
    function newMedia(AURL: PWideChar; out AMedia: IWMPMedia): HRESULT; stdcall;
  end;

  IWMPPlayer4 = interface(IWMPCore3)
  ['{6C497D62-8919-413c-82DB-E935FB3EC584}']
    function get_enabled(out AEnabled: WordBool): HRESULT; stdcall;
    function put_enabled(AEnabled: WordBool): HRESULT; stdcall;
    function get_fullScreen(out AFullScreen: WordBool): HRESULT; stdcall;
    function put_fullScreen(AFullScreen: WordBool): HRESULT; stdcall;
    function get_enableContextMenu(out AEnableContextMenu: WordBool): HRESULT; stdcall;
    function put_enableContextMenu(AEnableContextMenu: WordBool): HRESULT; stdcall;
    function put_uiMode(AMode: PWideChar): HRESULT; stdcall;
    function get_uiMode(out AMode: PWideChar): HRESULT; stdcall;
    function get_stretchToFit(out AStretchToFit: WordBool): HRESULT; stdcall;
    function put_stretchToFit(AStretchToFit: WordBool): HRESULT; stdcall;
    function get_windowlessVideo(out AWindowless: WordBool): HRESULT; stdcall;
    function put_windowlessVideo(AWindowless: WordBool): HRESULT; stdcall;
    function get_isRemote(out AIsRemote: WordBool): HRESULT; stdcall;
    function get_playerApplication(out AWMPPlayerApplication: IWMPPlayerApplication): HRESULT; stdcall;
    function openPlayer(AURL: PWideChar): HRESULT; stdcall;
  end;

  IWMPPlayerServices = interface(IUnknown)
  ['{1D01FBDB-ADE2-4c8d-9842-C190B95C3306}']
    function activateUIPlugin(APlugin: PWideChar): HRESULT; stdcall;
    function setTaskPane(ATaskPane: PWideChar): HRESULT; stdcall;
    function setTaskPaneURL(ATaskPane: PWideChar; AURL: PWideChar; AFriendlyName: PWideChar): HRESULT; stdcall;
  end;

  IWMPSyncDevice = interface(IUnknown)
  ['{82A2986C-0293-4fd0-B279-B21B86C058BE}']
    function get_friendlyName(out AName: PWideChar): HRESULT; stdcall;
    function put_friendlyName(AName: PWideChar): HRESULT; stdcall;
    function get_deviceName(out AName: PWideChar): HRESULT; stdcall;
    function get_deviceId(out ADeviceID: PWideChar): HRESULT; stdcall;
    function get_partnershipIndex(out AIndex: Integer): HRESULT; stdcall;
    function get_connected(out AConnected: WordBool): HRESULT; stdcall;
    function get_status(out AStatus: TWMPDeviceStatus): HRESULT; stdcall;
    function get_syncState(out ASyncState: TWMPSyncState): HRESULT; stdcall;
    function get_progress(out AProgress: Integer): HRESULT; stdcall;
    function getItemInfo(AItemName: PWideChar; out AValue: PWideChar): HRESULT; stdcall;
    function createPartnership(AShowUI: WordBool): HRESULT; stdcall;
    function deletePartnership: HRESULT; stdcall;
    function start: HRESULT; stdcall;
    function stop: HRESULT; stdcall;
    function showSettings: HRESULT; stdcall;
    function isIdentical(ADevice: IWMPSyncDevice; out AValue: WordBool): HRESULT; stdcall;
  end;

  IWMPSyncServices = interface(IUnknown)
  ['{8B5050FF-E0A4-4808-B3A8-893A9E1ED894}']
    function get_deviceCount(out ACount: Integer): HRESULT; stdcall;
    function getDevice(AIndex: Integer; out ADevice: IWMPSyncDevice): HRESULT; stdcall;
  end;

  IWMPPlayerServices2 = interface(IWMPPlayerServices)
  ['{1BB1592F-F040-418a-9F71-17C7512B4D70}']
    function setBackgroundProcessingPriority(APriority: PWideChar): HRESULT; stdcall;
  end;

  IWMPEvents = interface(IUnknown)
  ['{19A6627B-DA9E-47c1-BB23-00B5E668236A}']
    procedure OpenStateChange(ANewState: Integer); stdcall;
    procedure PlayStateChange(ANewState: Integer); stdcall;
    procedure AudioLanguageChange(ALangID: Integer); stdcall;
    procedure StatusChange; stdcall;
    procedure ScriptCommand(AType: PWideChar; AParam: PWideChar); stdcall;
    procedure NewStream; stdcall;
    procedure Disconnect(AResult: Integer); stdcall;
    procedure Buffering(AStart: WordBool); stdcall;
    procedure Error; stdcall;
    procedure Warning(AWarningType: Integer; AParam: Integer; ADescription: PWideChar); stdcall;
    procedure EndOfStream(AResult: Integer); stdcall;
    procedure PositionChange(AOldPosition: Double; ANewPosition: Double); stdcall;
    procedure MarkerHit(AMarkerNum: Integer); stdcall;
    procedure DurationUnitChange(ANewDurationUnit: Integer); stdcall;
    procedure CdromMediaChange(CdromNum: Integer); stdcall;
    procedure PlaylistChange(APlaylist: IDispatch; AChange: TWMPPlaylistChangeEventType); stdcall;
    procedure CurrentPlaylistChange(AChange: TWMPPlaylistChangeEventType); stdcall;
    procedure CurrentPlaylistItemAvailable(AItemName: PWideChar); stdcall;
    procedure MediaChange(AItem: IDispatch); stdcall;
    procedure CurrentMediaItemAvailable(AItemName: PWideChar); stdcall;
    procedure CurrentItemChange(AMedia: IDispatch); stdcall;
    procedure MediaCollectionChange; stdcall;
    procedure MediaCollectionAttributeStringAdded(AAttribName: PWideChar; AAttribVal: PWideChar); stdcall;
    procedure MediaCollectionAttributeStringRemoved(AAttribName: PWideChar; AAttribVal: PWideChar); stdcall;
    procedure MediaCollectionAttributeStringChanged(AAttribName: PWideChar; AOldAttribVal: PWideChar; ANewAttribVal: PWideChar); stdcall;
    procedure PlaylistCollectionChange; stdcall;
    procedure PlaylistCollectionPlaylistAdded(APlaylistName: PWideChar); stdcall;
    procedure PlaylistCollectionPlaylistRemoved(APlaylistName: PWideChar); stdcall;
    procedure PlaylistCollectionPlaylistSetAsDeleted(APlaylistName: PWideChar; AIsDeleted: WordBool); stdcall;
    procedure ModeChange(AModeName: PWideChar; ANewValue: WordBool); stdcall;
    procedure MediaError(AMediaObject: IDispatch); stdcall;
    procedure OpenPlaylistSwitch(AItem: IDispatch); stdcall;
    procedure DomainChange(ADomain: PWideChar); stdcall;
    procedure SwitchedToPlayerApplication; stdcall;
    procedure SwitchedToControl; stdcall;
    procedure PlayerDockedStateChange; stdcall;
    procedure PlayerReconnect; stdcall;
    procedure Click(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); stdcall;
    procedure DoubleClick(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); stdcall;
    procedure KeyDown(AKeyCode: SmallInt; AShiftState: SmallInt); stdcall;
    procedure KeyPress(AKeyAscii: SmallInt); stdcall;
    procedure KeyUp(AKeyCode: SmallInt; AShiftState: SmallInt); stdcall;
    procedure MouseDown(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); stdcall;
    procedure MouseMove(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); stdcall;
    procedure MouseUp(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); stdcall;
  end;

  IWMPEvents2 = interface(IWMPEvents)
  ['{1E7601FA-47EA-4107-9EA9-9004ED9684FF}']
    procedure DeviceConnect(ADevice: IWMPSyncDevice); stdcall;
    procedure DeviceDisconnect(ADevice: IWMPSyncDevice); stdcall;
    procedure DeviceStatusChange(ADevice: IWMPSyncDevice; ANewStatus: TWMPDeviceStatus); stdcall;
    procedure DeviceSyncStateChange(ADevice: IWMPSyncDevice; ANewState: TWMPSyncState); stdcall;
    procedure DeviceSyncError(ADevice: IWMPSyncDevice; AMedia: IDispatch); stdcall;
    procedure CreatePartnershipComplete(ADevice: IWMPSyncDevice; AResult: HRESULT); stdcall;
  end;

  IWMPPluginUI = interface(IUnknown)
  ['{4C5E8F9F-AD3E-4bf9-9753-FCD30D6D38DD}']
    function SetCore(ACore: IWMPCore): HRESULT; stdcall;
    function Create_(AParent: THandle; out AWindow: THandle): HRESULT; stdcall;
    function Destroy_: HRESULT; stdcall;
    function DisplayPropertyPage(AParent: THandle): HRESULT; stdcall;
    function GetProperty(AName: PWideChar; AProperty: POleVariant): HRESULT; stdcall;
    function SetProperty(AName: PWideChar; AProperty: POleVariant): HRESULT; stdcall;
    function TranslateAccelerator(AMsg: PMsg): HRESULT; stdcall;
  end;

  TWMPUIPluginInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;

    procedure AfterConstruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;

  TWMPUIPlugin = class;

  TWMPUIPluginForm = class(TForm)
  protected
    FPlugin: TWMPUIPlugin;
  end;
  TWMPUIPluginFormClass = class of TWMPUIPluginForm;

  TWMPUIPlugin = class(TWMPUIPluginInterfacedObject, IWMPPluginUI, IWMPEvents, IWMPEvents2)
  protected
    FForm: TWMPUIPluginForm;
    FFormClass: TWMPUIPluginFormClass;
    FPropClass: TFormClass;
    FConnectionPoint: IConnectionPoint;
    FAdviseCookie: Cardinal;
    FWMPCore: IWMPCore;
    procedure ReleaseCore;
  public
    constructor Create(AFormClass: TWMPUIPluginFormClass; APropClass: TFormClass); virtual;
    destructor Destroy; override;
    // IWMPPluginUI
    function SetCore(ACore: IWMPCore): HRESULT; virtual; stdcall;
    function Create_(AParent: THandle; out AWindow: THandle): HRESULT; virtual; stdcall;
    function Destroy_: HRESULT; virtual; stdcall;
    function DisplayPropertyPage(AParent: THandle): HRESULT; virtual; stdcall;
    function GetProperty(AName: PWideChar; AProperty: POleVariant): HRESULT; virtual; stdcall;
    function SetProperty(AName: PWideChar; AProperty: POleVariant): HRESULT; virtual; stdcall;
    function TranslateAccelerator(AMsg: PMsg): HRESULT; virtual; stdcall;
    // IWMPEvents
    procedure OpenStateChange(ANewState: Integer); virtual; stdcall;
    procedure PlayStateChange(ANewState: Integer); virtual; stdcall;
    procedure AudioLanguageChange(ALangID: Integer); virtual; stdcall;
    procedure StatusChange; virtual; stdcall;
    procedure ScriptCommand(AType: PWideChar; AParam: PWideChar); virtual; stdcall;
    procedure NewStream; virtual; stdcall;
    procedure Disconnect(AResult: Integer); virtual; stdcall;
    procedure Buffering(AStart: WordBool); virtual; stdcall;
    procedure Error; virtual; stdcall;
    procedure Warning(AWarningType: Integer; AParam: Integer; ADescription: PWideChar); virtual; stdcall;
    procedure EndOfStream(AResult: Integer); virtual; stdcall;
    procedure PositionChange(AOldPosition: Double; ANewPosition: Double); virtual; stdcall;
    procedure MarkerHit(AMarkerNum: Integer); virtual; stdcall;
    procedure DurationUnitChange(ANewDurationUnit: Integer); virtual; stdcall;
    procedure CdromMediaChange(CdromNum: Integer); virtual; stdcall;
    procedure PlaylistChange(APlaylist: IDispatch; AChange: TWMPPlaylistChangeEventType); virtual; stdcall;
    procedure CurrentPlaylistChange(AChange: TWMPPlaylistChangeEventType); virtual; stdcall;
    procedure CurrentPlaylistItemAvailable(AItemName: PWideChar); virtual; stdcall;
    procedure MediaChange(AItem: IDispatch); virtual; stdcall;
    procedure CurrentMediaItemAvailable(AItemName: PWideChar); virtual; stdcall;
    procedure CurrentItemChange(AMedia: IDispatch); virtual; stdcall;
    procedure MediaCollectionChange; virtual; stdcall;
    procedure MediaCollectionAttributeStringAdded(AAttribName: PWideChar; AAttribVal: PWideChar); virtual; stdcall;
    procedure MediaCollectionAttributeStringRemoved(AAttribName: PWideChar; AAttribVal: PWideChar); virtual; stdcall;
    procedure MediaCollectionAttributeStringChanged(AAttribName: PWideChar; AOldAttribVal: PWideChar; ANewAttribVal: PWideChar); virtual; stdcall;
    procedure PlaylistCollectionChange; virtual; stdcall;
    procedure PlaylistCollectionPlaylistAdded(APlaylistName: PWideChar); virtual; stdcall;
    procedure PlaylistCollectionPlaylistRemoved(APlaylistName: PWideChar); virtual; stdcall;
    procedure PlaylistCollectionPlaylistSetAsDeleted(APlaylistName: PWideChar; AIsDeleted: WordBool); virtual; stdcall;
    procedure ModeChange(AModeName: PWideChar; ANewValue: WordBool); virtual; stdcall;
    procedure MediaError(AMediaObject: IDispatch); virtual; stdcall;
    procedure OpenPlaylistSwitch(AItem: IDispatch); virtual; stdcall;
    procedure DomainChange(ADomain: PWideChar); virtual; stdcall;
    procedure SwitchedToPlayerApplication; virtual; stdcall;
    procedure SwitchedToControl; virtual; stdcall;
    procedure PlayerDockedStateChange; virtual; stdcall;
    procedure PlayerReconnect; virtual; stdcall;
    procedure Click(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); virtual; stdcall;
    procedure DoubleClick(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); virtual; stdcall;
    procedure KeyDown(AKeyCode: SmallInt; AShiftState: SmallInt); virtual; stdcall;
    procedure KeyPress(AKeyAscii: SmallInt); virtual; stdcall;
    procedure KeyUp(AKeyCode: SmallInt; AShiftState: SmallInt); virtual; stdcall;
    procedure MouseDown(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); virtual; stdcall;
    procedure MouseMove(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); virtual; stdcall;
    procedure MouseUp(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer); virtual; stdcall;
    // IWMPEvents2
    procedure DeviceConnect(ADevice: IWMPSyncDevice); virtual; stdcall;
    procedure DeviceDisconnect(ADevice: IWMPSyncDevice); virtual; stdcall;
    procedure DeviceStatusChange(ADevice: IWMPSyncDevice; ANewStatus: TWMPDeviceStatus); virtual; stdcall;
    procedure DeviceSyncStateChange(ADevice: IWMPSyncDevice; ANewState: TWMPSyncState); virtual; stdcall;
    procedure DeviceSyncError(ADevice: IWMPSyncDevice; AMedia: IDispatch); virtual; stdcall;
    procedure CreatePartnershipComplete(ADevice: IWMPSyncDevice; AResult: HRESULT); virtual; stdcall;
  published
    property WMPCore: IWMPCore read FWMPCore;
  end;
  TWMPUIPluginClass = class of TWMPUIPlugin;

  TWMPUIPluginClassFactory = class(TObject, IUnknown, IClassFactory)
  private
    FPluginClass: TWMPUIPluginClass;
    FName: WideString;
    FDescription: WideString;
    FCapabilities: Cardinal;
    FClassID: TGUID;
    FFormClass: TWMPUIPluginFormClass;
    FPropClass: TFormClass;
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IClassFactory
    function CreateInstance(const UnkOuter: IUnknown; const IID: TGUID; out Obj): HResult; stdcall;
    function LockServer(fLock: BOOL): HResult; stdcall;
  public
    constructor Create(APluginClass: TWMPUIPluginClass; const AName: WideString; const ADescription: WideString; const AClassID: TGUID; ACapabilities: Cardinal; AFormClass: TWMPUIPluginFormClass; APropClass: TFormClass);

    procedure RegisterServer;
    procedure UnregisterServer;
  end;

  TWMPUIPluginClassTemplate = class
  private
    FFactoryList: TList;
    procedure AddFactory(AFactory: TWMPUIPluginClassFactory);
  public
    constructor Create;
    destructor Destroy; override;

    class function GetTemplate: TWMPUIPluginClassTemplate;

    function RegisterServer(ARegister: Boolean): Boolean;
    function GetFactoryFromClassID(const ACLSID: TGUID): TWMPUIPluginClassFactory;
  end;

  function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult; stdcall;
  function DllCanUnloadNow: HResult; stdcall;
  function DllRegisterServer: HResult; stdcall;
  function DllUnregisterServer: HResult; stdcall;

implementation

uses Controls, Types;

var
  ObjectCount: Integer = 0;
  ClassFactoryCount: Integer = 0;
  ClassTemplate: TWMPUIPluginClassTemplate = nil;

function WMPNotifyPluginAddRemove: Boolean;
begin
  Result := PostMessage(HWND_BROADCAST, RegisterWindowMessageA('WMPlayer_PluginAddRemove'), 0, 0);
end;

(*** TWMPUIPluginInterfacedObject *********************************************)

procedure TWMPUIPluginInterfacedObject.AfterConstruction;
begin
  InterlockedDecrement(FRefCount);
end;

class function TWMPUIPluginInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TWMPUIPluginInterfacedObject(Result).FRefCount := 1;
end;

function TWMPUIPluginInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TWMPUIPluginInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TWMPUIPluginInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

(*** TWMPUIPlugin *************************************************************)

constructor TWMPUIPlugin.Create(AFormClass: TWMPUIPluginFormClass; APropClass: TFormClass);
begin
  inherited Create;
  FFormClass := AFormClass;
  FPropClass := APropClass;
end;

destructor TWMPUIPlugin.Destroy;
begin
  ReleaseCore;
  Destroy_;
  inherited Destroy;
end;

procedure TWMPUIPlugin.ReleaseCore;
begin
  if Assigned(FConnectionPoint) then
  begin
    if (FAdviseCookie <> 0) then
    begin
      FConnectionPoint.Unadvise(FAdviseCookie);
      FAdviseCookie := 0;
    end;
    FConnectionPoint := nil;
  end;

  FWMPCore := nil;
end;

procedure CreateRegKeyD(const Key, ValueName: String; Value: Integer; RootKey: DWord = HKEY_CLASSES_ROOT);
var
  Handle: HKey;
  Status, Disposition: Integer;
begin
  Status := RegCreateKeyEx(RootKey, PChar(Key), 0, '',
    REG_OPTION_NON_VOLATILE, KEY_READ or KEY_WRITE, nil, Handle,
    @Disposition);
  if Status = 0 then
  begin
    RegSetValueEx(Handle, PChar(ValueName), 0, REG_DWORD,
      @Value, SizeOf(Integer));
    RegCloseKey(Handle);
  end;
end;

function TWMPUIPlugin.SetCore(ACore: IWMPCore): HRESULT;
var
  conn: IConnectionPointContainer;
  unk: IUnknown;
begin
  Result := S_OK;
  ReleaseCore;

  FWMPCore := ACore;
  if (FWMPCore = nil)
    then Exit;

  if FWMPCore.QueryInterface(IConnectionPointContainer, conn) = S_OK then
  begin
    QueryInterface(IUnknown, unk);
    if conn.FindConnectionPoint(IID_IWMPEvents2, FConnectionPoint) = S_OK then
    begin
      if (FConnectionPoint.Advise(unk, Integer(FAdviseCookie)) <> S_OK) or (FAdviseCookie = 0) then
      begin
        FConnectionPoint := nil;
        Result := E_FAIL;
        Exit;
      end;
    end else
    if conn.FindConnectionPoint(IID_IWMPEvents, FConnectionPoint) = S_OK then
    begin
      QueryInterface(IUnknown, unk);
      if (FConnectionPoint.Advise(unk, Integer(FAdviseCookie)) <> S_OK) or (FAdviseCookie = 0) then
      begin
        FConnectionPoint := nil;
        Result := E_FAIL;
        Exit;
      end;
    end;
  end;
end;

function TWMPUIPlugin.Create_(AParent: THandle; out AWindow: THandle): HRESULT;
begin
  if (@AWindow = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  if (FFormClass = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  Destroy_;

  FForm := FFormClass.CreateParented(AParent);
  FForm.FPlugin := Self;
  FForm.BorderStyle := bsNone;
  FForm.Visible := True;

  AWindow := FForm.Handle;

  Result := S_OK;
end;

function TWMPUIPlugin.Destroy_: HRESULT;
begin
  if not Assigned(FForm) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  FForm.Free;
  FForm := nil;

  Result := S_OK;
end;

function TWMPUIPlugin.DisplayPropertyPage(AParent: THandle): HRESULT;
var
  form: TForm;
  rect: TRect;
begin
  if not Assigned(FPropClass) then
  begin
    Result := E_NOTIMPL;
    Exit;
  end;

  form := FPropClass.Create(nil);
  GetWindowRect(AParent, rect);
  form.Left := rect.Left - ((form.Width - (rect.Right - rect.Left)) div 2);
  form.Top := rect.Top - ((form.Height - (rect.Bottom - rect.Top)) div 2);
  form.ShowModal;
  form.Free;

  Result := S_OK;
end;

function TWMPUIPlugin.GetProperty(AName: PWideChar; AProperty: POleVariant): HRESULT;
begin
  if (@AProperty = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  Result := S_OK;

  if (AName = PLUGIN_SEPARATEWINDOW_DEFAULTWIDTH) then
  begin
    AProperty^ := Integer(300);
  end else
  if (AName = PLUGIN_SEPARATEWINDOW_DEFAULTHEIGHT) then
  begin
    AProperty^ := Integer(200);
  end else
  begin
    Result := E_NOTIMPL;
  end; 
end;

function TWMPUIPlugin.SetProperty(AName: PWideChar; AProperty: POleVariant): HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TWMPUIPlugin.TranslateAccelerator(AMsg: PMsg): HRESULT;
begin
  if (FForm = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  if (AMsg = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;

  Result := S_OK;
end;

procedure TWMPUIPlugin.OpenStateChange(ANewState: Integer);
begin

end;

procedure TWMPUIPlugin.PlayStateChange(ANewState: Integer);
begin

end;

procedure TWMPUIPlugin.AudioLanguageChange(ALangID: Integer);
begin

end;

procedure TWMPUIPlugin.StatusChange;
begin

end;

procedure TWMPUIPlugin.ScriptCommand(AType: PWideChar; AParam: PWideChar);
begin

end;

procedure TWMPUIPlugin.NewStream;
begin

end;

procedure TWMPUIPlugin.Disconnect(AResult: Integer);
begin

end;

procedure TWMPUIPlugin.Buffering(AStart: WordBool);
begin

end;

procedure TWMPUIPlugin.Error;
begin

end;

procedure TWMPUIPlugin.Warning(AWarningType: Integer; AParam: Integer; ADescription: PWideChar);
begin

end;

procedure TWMPUIPlugin.EndOfStream(AResult: Integer);
begin

end;

procedure TWMPUIPlugin.PositionChange(AOldPosition: Double; ANewPosition: Double);
begin

end;

procedure TWMPUIPlugin.MarkerHit(AMarkerNum: Integer);
begin

end;

procedure TWMPUIPlugin.DurationUnitChange(ANewDurationUnit: Integer);
begin

end;

procedure TWMPUIPlugin.CdromMediaChange(CdromNum: Integer);
begin

end;

procedure TWMPUIPlugin.PlaylistChange(APlaylist: IDispatch; AChange: TWMPPlaylistChangeEventType);
begin

end;

procedure TWMPUIPlugin.CurrentPlaylistChange(AChange: TWMPPlaylistChangeEventType);
begin

end;

procedure TWMPUIPlugin.CurrentPlaylistItemAvailable(AItemName: PWideChar);
begin

end;

procedure TWMPUIPlugin.MediaChange(AItem: IDispatch);
begin

end;

procedure TWMPUIPlugin.CurrentMediaItemAvailable(AItemName: PWideChar);
begin

end;

procedure TWMPUIPlugin.CurrentItemChange(AMedia: IDispatch);
begin

end;

procedure TWMPUIPlugin.MediaCollectionChange;
begin

end;

procedure TWMPUIPlugin.MediaCollectionAttributeStringAdded(AAttribName: PWideChar; AAttribVal: PWideChar);
begin

end;

procedure TWMPUIPlugin.MediaCollectionAttributeStringRemoved(AAttribName: PWideChar; AAttribVal: PWideChar);
begin

end;

procedure TWMPUIPlugin.MediaCollectionAttributeStringChanged(AAttribName: PWideChar; AOldAttribVal: PWideChar; ANewAttribVal: PWideChar);
begin

end;

procedure TWMPUIPlugin.PlaylistCollectionChange;
begin

end;

procedure TWMPUIPlugin.PlaylistCollectionPlaylistAdded(APlaylistName: PWideChar);
begin

end;

procedure TWMPUIPlugin.PlaylistCollectionPlaylistRemoved(APlaylistName: PWideChar);
begin

end;

procedure TWMPUIPlugin.PlaylistCollectionPlaylistSetAsDeleted(APlaylistName: PWideChar; AIsDeleted: WordBool);
begin

end;

procedure TWMPUIPlugin.ModeChange(AModeName: PWideChar; ANewValue: WordBool);
begin

end;

procedure TWMPUIPlugin.MediaError(AMediaObject: IDispatch);
begin

end;

procedure TWMPUIPlugin.OpenPlaylistSwitch(AItem: IDispatch);
begin

end;

procedure TWMPUIPlugin.DomainChange(ADomain: PWideChar);
begin

end;

procedure TWMPUIPlugin.SwitchedToPlayerApplication;
begin

end;

procedure TWMPUIPlugin.SwitchedToControl;
begin

end;

procedure TWMPUIPlugin.PlayerDockedStateChange;
begin

end;

procedure TWMPUIPlugin.PlayerReconnect;
begin

end;

procedure TWMPUIPlugin.Click(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer);
begin

end;

procedure TWMPUIPlugin.DoubleClick(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer);
begin

end;

procedure TWMPUIPlugin.KeyDown(AKeyCode: SmallInt; AShiftState: SmallInt);
begin

end;

procedure TWMPUIPlugin.KeyPress(AKeyAscii: SmallInt);
begin

end;

procedure TWMPUIPlugin.KeyUp(AKeyCode: SmallInt; AShiftState: SmallInt);
begin

end;

procedure TWMPUIPlugin.MouseDown(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer);
begin

end;

procedure TWMPUIPlugin.MouseMove(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer);
begin

end;

procedure TWMPUIPlugin.MouseUp(AButton: SmallInt; AShiftState: SmallInt; AX: Integer; AY: Integer);
begin

end;

procedure TWMPUIPlugin.DeviceConnect(ADevice: IWMPSyncDevice);
begin

end;

procedure TWMPUIPlugin.DeviceDisconnect(ADevice: IWMPSyncDevice);
begin

end;

procedure TWMPUIPlugin.DeviceStatusChange(ADevice: IWMPSyncDevice; ANewStatus: TWMPDeviceStatus);
begin

end;

procedure TWMPUIPlugin.DeviceSyncStateChange(ADevice: IWMPSyncDevice; ANewState: TWMPSyncState);
begin

end;

procedure TWMPUIPlugin.DeviceSyncError(ADevice: IWMPSyncDevice; AMedia: IDispatch);
begin

end;

procedure TWMPUIPlugin.CreatePartnershipComplete(ADevice: IWMPSyncDevice; AResult: HRESULT);
begin

end;

(*** TWMPUIPluginClassFactory *************************************************)

constructor TWMPUIPluginClassFactory.Create(APluginClass: TWMPUIPluginClass; const AName: WideString; const ADescription: WideString; const AClassID: TGUID; ACapabilities: Cardinal; AFormClass: TWMPUIPluginFormClass; APropClass: TFormClass);
begin
  inherited Create;
  TWMPUIPluginClassTemplate.GetTemplate.AddFactory(Self);
  FPluginClass := APluginClass;
  FName := AName;
  FDescription := ADescription;
  FClassID := AClassID;
  FCapabilities := ACapabilities;
  FFormClass := AFormClass;
  FPropClass := APropClass;
end;

function TWMPUIPluginClassFactory.CreateInstance(const unkOuter: IUnknown; const iid: TIID; out obj): HResult;
var
  plugin: TWMPUIPlugin;
begin
  if (@obj = nil) then
  begin
    Result := E_POINTER;
    Exit;
  end;
  Pointer(obj) := nil;

  if (FPluginClass = nil) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  plugin := FPluginClass.Create(FFormClass, FPropClass);
  Result := plugin.QueryInterface(IID, obj);
  if (plugin.RefCount = 0)
    then plugin.Free;
end;

procedure TWMPUIPluginClassFactory.RegisterServer;
var
  file_name: array[0..MAX_PATH-1] of Char;
  classID, serverKeyName: String;
begin
  classID := GUIDToString(FClassID);
  ServerKeyName := 'CLSID\' + classID + '\' + 'InprocServer32';

  CreateRegKey('CLSID\' + classID, '', 'TWMPPlugin Class');
  GetModuleFileName(HInstance, file_name, MAX_PATH);
  CreateRegKey(ServerKeyName, '', file_name);
  CreateRegKey(ServerKeyName, 'ThreadingModel', 'Apartment');

  CreateRegKey('Software\Microsoft\MediaPlayer\UIPlugins\' + classID, 'FriendlyName', FName, HKEY_LOCAL_MACHINE);
  CreateRegKey('Software\Microsoft\MediaPlayer\UIPlugins\' + classID, 'Description', FDescription, HKEY_LOCAL_MACHINE);
  CreateRegKeyD('Software\Microsoft\MediaPlayer\UIPlugins\' + classID, 'Capabilities', FCapabilities, HKEY_LOCAL_MACHINE);
end;

procedure TWMPUIPluginClassFactory.UnregisterServer;
var
  classID, serverKeyName: String;
begin
  classID := GUIDToString(FClassID);
  ServerKeyName := 'CLSID\' + classID + '\' + 'InprocServer32';

  DeleteRegKey(ServerKeyName);
  DeleteRegKey('CLSID\' + classID);
  DeleteRegKey('Software\Microsoft\MediaPlayer\UIPlugins\' + classID, HKEY_LOCAL_MACHINE);
end;

function TWMPUIPluginClassFactory._AddRef: Integer;
begin
  Result := InterlockedIncrement(ClassFactoryCount);
end;

function TWMPUIPluginClassFactory._Release: Integer;
begin
  Result := InterlockedDecrement(ClassFactoryCount);
end;

function TWMPUIPluginClassFactory.LockServer(fLock: BOOL): HResult;
begin
  Result := CoLockObjectExternal(Self, fLock, True);
  if fLock
    then InterlockedIncrement(ObjectCount)
    else InterlockedDecrement(ObjectCount);
end;

function TWMPUIPluginClassFactory.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
    then Result := S_OK
    else Result := E_NOINTERFACE;
end;

(*** TWMPUIPluginClassTemplate ************************************************)

constructor TWMPUIPluginClassTemplate.Create;
begin
  inherited Create;
  FFactoryList := TList.Create;
end;

procedure TWMPUIPluginClassTemplate.AddFactory(AFactory: TWMPUIPluginClassFactory);
begin
  FFactoryList.Add(AFactory);
end;

class function TWMPUIPluginClassTemplate.GetTemplate: TWMPUIPluginClassTemplate;
begin
  if (ClassTemplate = nil)
    then ClassTemplate := TWMPUIPluginClassTemplate.Create;
  Result := ClassTemplate;
end;

destructor TWMPUIPluginClassTemplate.Destroy;
var
  i: Integer;
begin
  for i := 0 to FFactoryList.Count -1
    do TWMPUIPluginClassFactory(FFactoryList[i]).Free;
  FFactoryList.Clear;
  FFactoryList.Free;
  inherited Destroy;
end;

function TWMPUIPluginClassTemplate.GetFactoryFromClassID(const ACLSID: TGUID): TWMPUIPluginClassFactory;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FFactoryList.Count -1 do
  begin
    if IsEqualGUID(TWMPUIPluginClassFactory(FFactoryList[i]).FClassID, ACLSID) then
    begin
      Result := FFactoryList[i];
      Exit;
    end;
  end;
end;

function TWMPUIPluginClassTemplate.RegisterServer(ARegister: Boolean): Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to FFactoryList.Count -1 do
  begin
    with TWMPUIPluginClassFactory(FFactoryList[i]) do
    begin
      UnregisterServer;
      if ARegister
        then RegisterServer;
    end;
  end;
end;

(*** DLL Exports **************************************************************)

function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult;
var
  factory: TWMPUIPluginClassFactory;
begin
  factory := TWMPUIPluginClassTemplate.GetTemplate.GetFactoryFromClassID(CLSID);
  if (factory <> nil) then
  begin
    if factory.GetInterface(IID, Obj)
      then Result := S_OK
      else Result := E_NOINTERFACE;
  end else
  begin
    Pointer(Obj) := nil;
    Result := CLASS_E_CLASSNOTAVAILABLE;
  end;
end;

function DllCanUnloadNow: HResult;
begin
  if (ObjectCount or ClassFactoryCount) = 0
    then Result := S_OK
    else Result := S_FALSE;
end;

function DllRegisterServer: HResult;
begin
  if TWMPUIPluginClassTemplate.GetTemplate.RegisterServer(True)
    then Result := S_OK
    else Result := E_FAIL;
  WMPNotifyPluginAddRemove;
end;

function DllUnregisterServer: HResult;
begin
  if TWMPUIPluginClassTemplate.GetTemplate.RegisterServer(False)
    then Result := S_OK
    else Result := E_FAIL;
  WMPNotifyPluginAddRemove;
end;

initialization
  DisableThreadLibraryCalls(HInstance);

finalization

  if (ClassTemplate <> nil) then
  begin
    ClassTemplate.Free;
    ClassTemplate := nil;
  end;

end.
