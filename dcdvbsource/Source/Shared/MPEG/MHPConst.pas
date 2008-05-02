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

unit MHPConst;

{$I Compiler.inc}

interface

uses
  ISO639LanguageCode;             

const
  MHP_TABLE_ID_AIT                                        = $74;

  MHP_DESCRIPTOR_TAG_APPLICATION                          = $00;
  MHP_DESCRIPTOR_TAG_APPLICATION_NAME                     = $01;
  MHP_DESCRIPTOR_TAG_TRANSPORT_PROTOCOL                   = $02;
  MHP_DESCRIPTOR_TAG_DVBJ_APPLICATION                     = $03;
  MHP_DESCRIPTOR_TAG_DVBJ_APPLICATION_LOCATION            = $04;
  MHP_DESCRIPTOR_TAG_EXTERNAL_APPLICATION_AUTHORISATION   = $05;

  MHP_DESCRIPTOR_TAG_DATA_BROADCAST_ID                    = $66;
  MHP_DESCRIPTOR_TAG_APPLICATION_SIGNALLING               = $6F;
  MHP_DESCRIPTOR_TAG_SERVICE_IDENTIFIER                   = $71;

  MHP_DESCRIPTOR_TAG_LABEL                                = $70;
  MHP_DESCRIPTOR_TAG_CACHING_PRIORITY                     = $71;
  MHP_DESCRIPTOR_TAG_CONTENT_TYPE                         = $72;

type
  TMHPApplicationIdentifier = packed record
    OrganisationID: Cardinal;
    ApplicationID: Word;
  end;
  PMHPApplicationIdentifier = ^TMHPApplicationIdentifier;

  TMHPApplicationSignallingDescriptorItem = packed record
    ApplicationType: Word;
    AITVersionNumber: Byte;
  end;
  PMHPApplicationSignallingDescriptorItem = ^TMHPApplicationSignallingDescriptorItem;

  TMHPApplicationName = packed record
    Language: TISO6392LanguageCode;
    Name: String;
  end;
  PMHPApplicationName = ^TMHPApplicationName;

  TMHPApplicationProfile = packed record
    ApplicationProfile: Word;
    MajorVersion: Byte;
    MinorVersion: Byte;
    MicroVersion: Byte;
  end;
  PMHPApplicationProfile = ^TMHPApplicationProfile;

  TDVBJParameter = packed record
    Parameter: String;
  end;
  PDVBJParameter = ^TDVBJParameter;

  TMHPApplicationAuthorisation = packed record
    ApplicationIndentifier: TMHPApplicationIdentifier;
    ApplicationPriority: Byte;
  end;
  PMHPApplicationAuthorisation = ^TMHPApplicationAuthorisation;
  
implementation

end.
 