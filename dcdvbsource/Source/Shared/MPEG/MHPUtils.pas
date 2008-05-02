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

unit MHPUtils;

{$I Compiler.inc}

interface

uses
  MPEGUtils, MPEGConst, MHPConst, MHPSections, MHPDescriptors, MPEGSections,
  MPEGDescriptors, SysUtils;

  function GetMHPAplicationTypeString(AValue: Word): String;
  function GetMHPDescriptorIDString(AValue: Byte): String;
  procedure PrintMHPAIT(AIT: TMHPApplicationInformationSection);

implementation

uses
  DSMCCSections;

function GetMHPAplicationTypeString(AValue: Word): String;
begin
  case AValue of
    $0000:  Result := 'Reserved for future use';
    $0001:  Result := 'DVB-J Application';
    $0002:  Result := 'DVB-HTML Application';
    else    Result :=  'Subject to registration with DVB';
  end;
end;

function GetMHPDescriptorIDString(AValue: Byte): String;
begin
  Result := 'Descriptor (0x' + IntToHex(AValue, 2) + ') -> ';
  case (AValue) of
    MHP_DESCRIPTOR_TAG_APPLICATION:                         Result := Result + 'Application';
    MHP_DESCRIPTOR_TAG_APPLICATION_NAME:                    Result := Result + 'Application Name';
    MHP_DESCRIPTOR_TAG_TRANSPORT_PROTOCOL:                  Result := Result + 'Transport Protocol';
    MHP_DESCRIPTOR_TAG_DVBJ_APPLICATION:                    Result := Result + 'DVB-J Application';
    MHP_DESCRIPTOR_TAG_DVBJ_APPLICATION_LOCATION:           Result := Result + 'DVB-J Application Location';
    MHP_DESCRIPTOR_TAG_EXTERNAL_APPLICATION_AUTHORISATION:  Result := Result + 'External Application Authorisation';
    MHP_DESCRIPTOR_TAG_DATA_BROADCAST_ID:                   Result := Result + 'Data Broadcast ID';
    MHP_DESCRIPTOR_TAG_APPLICATION_SIGNALLING:              Result := Result + 'Application Signalling';
//    MHP_DESCRIPTOR_TAG_SERVICE_IDENTIFIER:                  Result := Result + 'Service Identifier';
    MHP_DESCRIPTOR_TAG_LABEL:                               Result := Result + 'Label';
    MHP_DESCRIPTOR_TAG_CACHING_PRIORITY:                    Result := Result + 'Caching Priority';
    MHP_DESCRIPTOR_TAG_CONTENT_TYPE:                        Result := Result + 'Content Type';
    else                                                    Result := Result + 'Unknown';
  end;
  Result := Result + ' (MHP)';
end;

procedure PrintMHPAIT(AIT: TMHPApplicationInformationSection);
var
  i: Integer;
begin
  dbg('------------------------ MHP_TABLE_ID_AIT ------------------------');
  dbg('|');
  dbg('|  TestApplicationFlag: ' + inttostr(AIT.TestApplicationFlag));
  dbg('|  ApplicationType: ' + GetMHPAplicationTypeString(AIT.ApplicationType));
  dbg('|');
  dbg('|  Number of Applications: ' + inttostr(AIT.Applications.Count));
  dbg('|');
  for i := 0 to AIT.Applications.Count -1 do
  begin
    dbg('|    Application ' + inttostr(i));
    dbg('|');
    dbg('|      OrganisationID: ' + inttostr(AIT.Applications[i].ApplicationIDentifier.OrganisationID));
    dbg('|      ApplicationID: ' + inttostr(AIT.Applications[i].ApplicationIDentifier.ApplicationID));
    dbg('|      ApplicationControlCode: ' + inttostr(AIT.Applications[i].ApplicationControlCode));
    dbg('|');
    PrintDescriptors(AIT.Applications[i].Descriptors, '    ');
  end;
  PrintDescriptors(AIT.Descriptors, '');
end;



end.
 