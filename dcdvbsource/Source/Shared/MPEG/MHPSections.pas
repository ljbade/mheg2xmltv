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

unit MHPSections;

{$I Compiler.inc}

interface

uses
  MPEGSections, MPEGConst, Classes, MPEGDescriptors, SysUtils, MHPDescriptors,
  MHPConst, Windows;

type
  TMHPApplication = class
  private
    FApplicationIDentifier: TMHPApplicationIdentifier;
    FApplicationControlCode: Byte;
    FTotalLength: Integer;
    FDescriptors: TDescriptorList;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PByte); overload;
    destructor Destroy; override;

    procedure Clear;
    function ParseBuffer(ABuffer: PByte): Boolean;

    property ApplicationIDentifier: TMHPApplicationIdentifier read FApplicationIDentifier;
    property ApplicationControlCode: Byte read FApplicationControlCode;
    property Descriptors: TDescriptorList read FDescriptors;
    property TotalLength: Integer read FTotalLength;
  end;

  TMHPApplicationList = class(TList)
  private
    function Get(Index: Integer): TMHPApplication;
    procedure Put(Index: Integer; Item: TMHPApplication);
  public
    function Add(Item: TMHPApplication): Integer;
    procedure Clear; override;
    procedure Delete(Index: Integer);

    property Items[Index: Integer]: TMHPApplication read Get write Put; default;
  end;

  TMHPApplicationInformationSection = class(TBaseSectionSyntax)
  private
    FTestApplicationFlag: Byte;
    FApplicationType: Word;
    FDescriptors: TDescriptorList;
    FApplicationList: TMHPApplicationList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear; override;
    function ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean; override;

    property TestApplicationFlag: Byte read FTestApplicationFlag;
    property ApplicationType: Word read FApplicationType;
    property Descriptors: TDescriptorList read FDescriptors;
    property Applications: TMHPApplicationList read FApplicationList;
  end;

implementation

uses
  MPEGUtils, MHPUtils;

(*** TMHPApplication **********************************************************)

constructor TMHPApplication.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
  Clear;
end;

constructor TMHPApplication.Create(ABuffer: PByte);
begin
  Create;
  ParseBuffer(ABuffer);
end;

destructor TMHPApplication.Destroy;
begin
  Clear;
  FDescriptors.Free;
  inherited Destroy;
end;

procedure TMHPApplication.Clear;
begin
  FApplicationIDentifier.OrganisationID := 0;
  FApplicationIDentifier.ApplicationID := 0;
  FApplicationControlCode := 0;
  FDescriptors.Clear;
  FTotalLength := 0;
end;

function TMHPApplication.ParseBuffer(ABuffer: PByte): Boolean;
var
  application_descriptors_loop_length: Integer;
  descriptor: TBaseDescriptor;
begin
  FApplicationIDentifier.OrganisationID := GetLong(ABuffer);
  inc(ABuffer, 4);

  FApplicationIDentifier.ApplicationID := GetWord(ABuffer);
  inc(ABuffer, 2);

  FApplicationControlCode := ABuffer^;
  inc(ABuffer);

  application_descriptors_loop_length := GetWordBits(ABuffer, 4, 12);
  FTotalLength := application_descriptors_loop_length + 9;
  inc(ABuffer, 2);

  while (application_descriptors_loop_length > 0) do
  begin
    descriptor := TMHPBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(application_descriptors_loop_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  Result := True;
end;

(*** TMHPApplicationList ******************************************************)

function TMHPApplicationList.Get(Index: Integer): TMHPApplication;
begin
  Result := inherited Get(Index);
end;

procedure TMHPApplicationList.Put(Index: Integer; Item: TMHPApplication);
begin
  inherited Put(Index, Item);
end;

function TMHPApplicationList.Add(Item: TMHPApplication): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TMHPApplicationList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count -1
    do TMHPApplication(Items[i]).Free;
  inherited Clear;
end;

procedure TMHPApplicationList.Delete(Index: Integer);
begin
  TMHPApplication(Items[Index]).Free;
  inherited Delete(Index);
end;

(*** TApplicationInformationSection *******************************************)

constructor TMHPApplicationInformationSection.Create;
begin
  inherited Create;
  FDescriptors := TDescriptorList.Create;
  FApplicationList := TMHPApplicationList.Create;
end;

destructor TMHPApplicationInformationSection.Destroy;
begin
  FreeAndNil(FDescriptors);
  FreeAndNil(FApplicationList);
  inherited Destroy;
end;

procedure TMHPApplicationInformationSection.Clear;
begin
  inherited Clear;

  FTestApplicationFlag := 0;
  FApplicationType := 0;

  if Assigned(FDescriptors)
    then FDescriptors.Clear;
  if Assigned(FApplicationList)
    then FApplicationList.Clear;
end;

function TMHPApplicationInformationSection.ParseBuffer(ABuffer: PByte; ASize: Integer): Boolean;
var
  common_descriptors_length: Integer;
  application_loop_length: Integer;
  descriptor: TBaseDescriptor;
  app: TMHPApplication;
begin
  Result := False;

  if not inherited ParseBuffer(ABuffer, ASize)
    then Exit;

  if (FTableID <> MHP_TABLE_ID_AIT) or (FSectionLength > 1021) or (ASize < 16) then
  begin
    FValid := False;
    Exit;
  end;

  inc(ABuffer, 3);

  FTestApplicationFlag := GetByteBits(ABuffer, 0, 1);
  FApplicationType := GetWordBits(ABuffer, 1, 15);
  inc(ABuffer, 2);

  inc(ABuffer, 3);

  common_descriptors_length := GetWordBits(ABuffer, 4, 12);
  inc(ABuffer, 2);

  while (common_descriptors_length > 0) do
  begin
    descriptor := TMHPBaseDescriptor.CreateDescriptor(ABuffer);
    FDescriptors.Add(descriptor);
    dec(common_descriptors_length, descriptor.TotalLength);
    inc(ABuffer, descriptor.TotalLength);
  end;

  application_loop_length := GetWordBits(ABuffer, 4, 12);
  inc(ABuffer, 2);

  while (application_loop_length > 0) do
  begin
    app := TMHPApplication.Create(ABuffer);
    FApplicationList.Add(app);
    dec(application_loop_length, app.TotalLength);
    inc(ABuffer, app.TotalLength);
  end;

  Result := True;
end;

end.
