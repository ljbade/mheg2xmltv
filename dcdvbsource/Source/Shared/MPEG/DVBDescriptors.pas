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

unit DVBDescriptors;

{$I Compiler.inc}

interface

uses
  MPEGDescriptors, MPEGConst, DVBConst;

type
  TDVBBaseDescriptor = class(TBaseDescriptor)
  public
    constructor Create; override;
    class function CreateDescriptor(ABuffer: PByte): TBaseDescriptor; override;
  end;

  TDVBCompressedModuleDescriptor = class(TDVBBaseDescriptor)
  protected
    FCompressionMethod: Byte;
    FOriginalSize: Cardinal;
  public
    procedure Clear; override;
    procedure ParseBuffer(ABuffer: PByte); override;

    property CompressionMethod: Byte read FCompressionMethod;
    property OriginalSize: Cardinal read FOriginalSize;
  end;

implementation

uses
  MPEGUtils;

(*** TDVBBaseDescriptor *******************************************************)

constructor TDVBBaseDescriptor.Create;
begin
  inherited Create;
  FDescriptorType := dtDVB;
end;

class function TDVBBaseDescriptor.CreateDescriptor(ABuffer: PByte): TBaseDescriptor;
begin
  case (ABuffer^) of
    DVB_DESCRIPTOR_TAG_COMPRESSED_MODULE:                   Result := TDVBCompressedModuleDescriptor.Create(ABuffer);
    else                                                    Result := TDVBBaseDescriptor.Create(ABuffer);
  end;
end;

(*** TDVBCompressedModuleDescriptor *******************************************)

procedure TDVBCompressedModuleDescriptor.Clear;
begin
  inherited Clear;
  FCompressionMethod := 0;
  FOriginalSize := 0;
end;

procedure TDVBCompressedModuleDescriptor.ParseBuffer(ABuffer: PByte);
begin
  inherited ParseBuffer(ABuffer);
  inc(ABuffer, 2);

  FCompressionMethod := ABuffer^;
  inc(ABuffer);

  FOriginalSize := GetLong(ABuffer);
end;

end.
 