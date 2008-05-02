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

unit DVBUtils;

{$I Compiler.inc}

interface

uses
  MPEGUtils, MPEGConst, DVBConst, DVBDescriptors, MPEGDescriptors, SysUtils;

  function GetDVBDescriptorIDString(AValue: Byte): String;

implementation

function GetDVBDescriptorIDString(AValue: Byte): String;
begin
  Result := 'Descriptor (0x' + IntToHex(AValue, 2) + ') -> ';
  case (AValue) of
    DVB_DESCRIPTOR_TAG_COMPRESSED_MODULE:                   Result := Result + 'Compressed Module';
    else                                                    Result := Result + 'Unknown';
  end;
  Result := Result + ' (DVB)';
end;

end.
