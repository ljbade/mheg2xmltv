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

unit Logger;

interface

uses
  Windows, SysUtils, Classes;

type
  TLogger = class
  protected
    FEnabled: Boolean;
  public
    procedure Log(AClass: TObject; AMethod: String; AText: String); virtual; abstract;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TOnCallbackLog = procedure(AText: String) of Object;

  TCallbackLogger = class(TLogger)
  private
    FOnLog: TOnCallbackLog;
  public
    procedure Log(AClass: TObject; AMethod: String; AText: String); override;
  published
    property OnLog: TOnCallbackLog read FOnLog write FOnLog;
  end;

  TFileLogger = class(TLogger)
  private
    FFile: TFileStream;
  public
    constructor Create(AFilename: String);
    destructor Destroy; override;
    procedure Log(AClass: TObject; AMethod: String; AText: String); override;
  end;

  TDbgStrLogger = class(TLogger)
  public
    procedure Log(AClass: TObject; AMethod: String; AText: String); override;
  end;

  procedure Log(ALogger: TLogger; AClass: TObject; AMethod: String; AText: String);

implementation

uses
  MPEGUtils;

procedure Log(ALogger: TLogger; AClass: TObject; AMethod: String; AText: String);
begin
  if Assigned(ALogger)
    then ALogger.Log(AClass, AMethod, AText);
end;

function GetLogString(AClass: TObject; AMethod: String; AText: String): String;
begin
  if Assigned(AClass)
    then Result := '[' + FormatDateTime('hh:nn:ss', Now) + ']  [' + AClass.ClassName + '@' + inttohex(Integer(AClass), 8) + '::' + AMethod + '] -> '
    else Result := '[' + FormatDateTime('hh:nn:ss', Now) + ']  [' + AMethod + '] -> ';

  Result := Result + AText;
end;

(*** TCallbackLogger **********************************************************)

procedure TCallbackLogger.Log(AClass: TObject; AMethod: String; AText: String);
begin
  if FEnabled and Assigned(FOnLog)
    then FOnLog(GetLogString(AClass, AMethod, AText));
end;

(*** TFileLogger **************************************************************)

constructor TFileLogger.Create(AFilename: String);
begin
  inherited Create;
  try
    FFile := TFileStream.Create(AFilename, fmCreate);
  except
    FFile := nil;
  end;
end;

destructor TFileLogger.Destroy;
begin
  SFree(FFile);
  inherited Destroy;
end;

procedure TFileLogger.Log(AClass: TObject; AMethod: String; AText: String);
var
  s: String;
begin
  if FEnabled and Assigned(FFile) then
  begin
    s := GetLogString(AClass, AMethod, AText) + #13#10;
    FFile.Write(s[1], Length(s));
  end;
end;

(*** TDbgStrLogger ************************************************************)

procedure TDbgStrLogger.Log(AClass: TObject; AMethod: String; AText: String);
var
  s: String;
begin
  if FEnabled then
  begin
    s := GetLogString(AClass, AMethod, AText);
    OutputDebugString(PChar(s));
  end;
end;

end.
