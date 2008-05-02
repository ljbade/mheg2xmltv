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

unit BitReader;

interface

uses
  Math;

type
  TBitReader = class
  private
    FBuffer: PByte;
    FBufferSize: Integer;
    FBitPosition: Int64;
    function GetBufferPos: Integer;
    procedure Align(ABits: Integer);
  public
    procedure SetBuffer(ABuffer: PByte; ASize: Integer);
    procedure Reset;
    function Read(ABits: Integer): Int64;
    procedure Skip(ABits: Integer);
    procedure Align8;

    function GolombUE: Int64;
    function GolombSE: Int64;

    property BufferPos: Integer read GetBufferPos;
  end;

implementation

procedure TBitReader.SetBuffer(ABuffer: PByte; ASize: Integer);
begin
  FBuffer := ABuffer;
  FBufferSize := ASize;
  Reset;
end;

procedure TBitReader.Reset;
begin
  FBitPosition := 0;
end;

function TBitReader.Read(ABits: Integer): Int64;
var
  tmp: PByte;
  tmp2: Integer;
  m1: Cardinal;
begin
	ASSERT((ABits >= 0) and (ABits <= 64));

  Result := 0;

  if (ABits <= 0) or (ABits > 64)
    then Exit;

  tmp :=  FBuffer;
  inc(tmp, FBitPosition div 8);

  m1 := FBitPosition mod 8;
  tmp2 := ABits;
  if (m1 <> 0) and (m1 + Int64(ABits) > 8)
    then inc(ABits, 8);

  while (ABits > 0) do
  begin
    Result := Result or (Int64(tmp^) and $FF);
    dec(ABits, 8);
    if (ABits > 0) then
    begin
      inc(tmp);
      Result := Result shl 8;
    end;
  end;

  inc(FBitPosition, tmp2);
  m1 := FBitPosition mod 8;

  if (m1 <> 0)
    then Result := Result shr (8 - m1);

  Result := Result and ((Int64(1) shl tmp2) - 1);
end;

procedure TBitReader.Skip(ABits: Integer);
begin
  Read(ABits);
end;

procedure TBitReader.Align(ABits: Integer);
var
  al: Integer;
begin
  al := ABits - (FBitPosition mod ABits);
  if (al = ABits)
    then Exit;
  Read(al);
end;

procedure TBitReader.Align8;
begin
  Align(8);
end;

function TBitReader.GetBufferPos: Integer;
begin
  Result := FBitPosition div 8;
end;

function TBitReader.GolombUE: Int64;
var
  leadingZeroBits: Integer;
  b: Byte;
begin
  leadingZeroBits := -1;
  b := 0;

  while (b = 0) do
  begin
    b := Read(1);
    inc(leadingZeroBits);
  end;

  Result := 1;
  Result := (Result shl leadingZeroBits) - 1 + Read(leadingZeroBits);
end;

function TBitReader.GolombSE: Int64;
begin
  Result := GolombUE;
	Result := Round(Power(-1, Result) + Ceil(Result mod 2));
end;

end.

