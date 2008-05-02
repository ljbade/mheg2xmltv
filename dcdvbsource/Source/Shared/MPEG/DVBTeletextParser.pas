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

unit DVBTeletextParser;

interface

uses
  Windows, Classes, MPEGConst, Graphics, MPEGParser, IDVBSource, SysUtils,
  MPEGUtils, Math, DSUtil, SyncObjs;

type
  TTeletextPage = class
  private
    FLines: array[0..30] of TTeletextLine;
    FPageID: Integer;
    FMagazine: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetLine(AIndex: Integer; ALine: PTeletextLine);
    function GetLine(AIndex: Integer): TTeletextLine;
    procedure Clear;
    property PageID: Integer read FPageID write FPageID;
    property Magazine: Integer read FMagazine write FMagazine;
  end;

  TTeletextBuffer = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AddPage(APage: TTeletextPage);
    function GetPage(APageID: Integer): TTeletextPage;
  end;

  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..500000] of Cardinal;
  TTeletextArray = array[0..30] of TTeletextLine;

  TDVBTeletextParser = class
  private
    FPageFound: Boolean;
    FLock: TCriticalSection;
    FNeedErase: Boolean;
    FLastMagazine: Integer;
    FCurrentNumber: String;
    FTeletextBuffer: TTeletextBuffer;
    FIgnoreLine: Integer;
    FFont: TFont;
    FBitmap: THandle;
    FDevice: THandle;
    FBuffer: PCardinalArray;
    FBitmapWidth: Integer;
    FBitmapHeight: Integer;
    FColor: TColor;
    FText: TTeletextArray;
    FTSParser: TTSParser;
    FTeletextParser: TTeletextParser;
    FShowTeletext: Boolean;
    FPage: Integer;
    FSubPage: Integer;
    FLanguage: Integer;
    FLastPage: Integer;
    FTextHeight: Integer;
    FTextWidth: Integer;
    FTransparent: Boolean;
    FSubTitle: Boolean;
    FSupressHeader: Boolean;
    FInhibitDisplay: Boolean;
    FNewsFlash: Boolean;
    FErasePage: Boolean;
    FUpdateIndicator: Boolean;
    FInterruptedSequence: Boolean;
    FMagazineSerial: Boolean;
    FInternalBuffer: Boolean;
    FLastVisiblePage: Integer;
    procedure DrawLine(Line: Integer; Text: PTeletextLine);
    procedure FillRect(X1, Y1, X2, Y2: Integer; Value: TColor);
    procedure TextOut(X, Y: Integer; const Text: String);
    procedure OnTeletextLine(Magazine: Integer; Line: Integer; Text: PTeletextLine);
    procedure SetPID(APID: Integer);
    function GetPID: Integer;
    procedure SetPage(APage: Integer);
    procedure SetShowTeletext(AShow: Boolean);
    procedure SetTransparent(ATransparent: Boolean);
    procedure UpdateTransparency;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure SaveTeletext(AType: TSaveTeletextType; AFilename: WideString);
    procedure ParseTeletextBuffer(ABuffer: PByte; ASize: Integer);
    procedure SetPageNumber(ANumber: Integer);
    procedure SetFastext(AFastext: TTeletextFastext);

    procedure CycleTeletextPage;
    function GetBuffer: PCardinalArray;

    property PID: Integer read GetPID write SetPID;
    property Page: Integer read FPage write SetPage;
    property SubPage: Integer read FSubPage write FSubPage;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Visible: Boolean read FShowTeletext write SetShowTeletext;
    property Width: Integer read FBitmapWidth;
    property Height: Integer read FBitmapHeight;
    property Text: TTeletextArray read FText;
    function HasColorAtChar(AIndex: Integer; AColor: TColor): Boolean;
    function LineIsDouble(AIndex: Integer): Boolean;
  end;

implementation

(*** TTeletextPage ************************************************************)

constructor TTeletextPage.Create;
begin
  inherited Create;
  Clear;
end;

destructor TTeletextPage.Destroy;
begin
  inherited Destroy;
end;

procedure TTeletextPage.SetLine(AIndex: Integer; ALine: PTeletextLine);
begin
  Move(ALine^, FLines[AIndex], 40);
end;

function TTeletextPage.GetLine(AIndex: Integer): TTeletextLine;
begin
  Move(FLines[AIndex], Result, 40);
end;

procedure TTeletextPage.Clear;
begin
  FillChar(FLines, 30 * 40, $20);
end;

(*** TTeletextBuffer **********************************************************)

constructor TTeletextBuffer.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TTeletextBuffer.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TTeletextBuffer.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1
    do TTeletextPage(FList[i]).Free;
  FList.Clear;
end;

procedure TTeletextBuffer.AddPage(APage: TTeletextPage);
begin
  FList.Add(APage);
end;

function TTeletextBuffer.GetPage(APageID: Integer): TTeletextPage;
var
  i: Integer;
begin
  for i := 0 to FList.Count -1 do
  begin
    Result := FList[i];
    if Result.FPageID = APageID
      then Exit;
  end;

  Result := nil;
end;

(*** TDVBTeletextParser *******************************************************)

procedure GetFontWidth(Font: TFont; out Width: Integer; out Height: Integer);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.Canvas.Font.Assign(Font);
  Height := bmp.Canvas.TextHeight('X');
  Width := bmp.Canvas.TextWidth('X');
  bmp.Free;
end;

function Get32Color(Color: TColor): TColor;
asm
  bswap     eax
  ror       eax, 8
  or        eax, $FF000000
end;

function TextToStr(Text: PTeletextLine): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to 39
    do Result := Result + inttohex(integer(Text^[i]), 2);
end;

constructor TDVBTeletextParser.Create;
var
  bitmap_info: TBitmapInfo;
  log_font: TLogFont;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FTeletextBuffer := TTeletextBuffer.Create;

  FillChar(FText, SizeOf(TTeletextArray), $20);

  FFont := TFont.Create;
  FFont.Name := 'Lucida Console';
  FFont.Size := 16;
  FFont.Style := [fsBold];
  FFont.Color := clWhite;

  FIgnoreLine := -1;

  GetObject(FFont.Handle, SizeOf(log_font), @log_font);
  log_font.lfQuality := ANTIALIASED_QUALITY;
  FFont.Handle := CreateFontIndirect(log_font);

  GetFontWidth(FFont, FTextWidth, FTextHeight);
  FBitmapWidth := FTextWidth * 40;
  FBitmapHeight := FTextHeight * 25;

  FillChar(bitmap_info, SizeOf(TBitmapInfo), 0);
  with bitmap_info.bmiHeader do
  begin
    biSize := SizeOf(TBitmapInfoHeader);
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biWidth := FBitmapWidth;
    biHeight := -FBitmapHeight;
  end;

  FBitmap := CreateDIBSection(0, bitmap_info, DIB_RGB_COLORS, Pointer(FBuffer), 0, 0);
  FDevice := CreateCompatibleDC(0);
  SelectObject(FDevice, FBitmap);

  SelectObject(FDevice, FFont.Handle);
  SetBkMode(FDevice, Windows.TRANSPARENT);

  FillChar(FBuffer[0], FBitmapHeight * FBitmapWidth * 4, $00);

  FLanguage := 0;
  FLastMagazine := -1;
  FLastPage := -1;
  FSupressHeader := False;
  FSubTitle := False;
  FTransparent := False;
  FPageFound := False;

  SetPage(100);
  FShowTeletext := False;
  UpdateTransparency;

  FTSParser := TTSParser.Create(True);
  FTeletextParser := TTeletextParser.Create(FTSParser, -1, OnTeletextLine, True);
  FInternalBuffer := False;
  FCurrentNumber := '';
  FLastVisiblePage := -1;
end;

destructor TDVBTeletextParser.Destroy;
begin
  DeleteDC(FDevice);
  DeleteObject(FBitmap);
  FFont.Free;
  FTeletextBuffer.Free;
  FTeletextParser.Free;
  FTSParser.Free;
  FLock.Free;
end;

procedure TDVBTeletextParser.UpdateTransparency;
var
  i, k: Integer;
  c: PCardinal;
begin
  for k := 0 to FBitmapHeight -1 do
  begin
    c := PCardinal(@FBuffer[k * FBitmapWidth]);
    if FNewsFlash or FSubTitle or FTransparent then
    begin
      for i := 0 to FBitmapWidth -1 do
      begin
        if ((c^ and $FFFFFF) = 0) or (((c^ and $FFFFFF) = 1) and FTransparent)
          then c^ := $00000000
          else c^ := c^ or $FF000000;
        inc(c);
      end;
    end else
    begin
      for i := 0 to FBitmapWidth -1 do
      begin
        c^ := c^ or $FF000000;
        inc(c);
      end;
    end;
  end;
end;

procedure TDVBTeletextParser.DrawLine(Line: Integer; Text: PTeletextLine);
var
  doubleheight: Boolean;
//  doublewidth: Boolean;
  doublesize: Boolean;
  flash: Boolean;
  ck: TColor;
  holdmosaic: Boolean;

  procedure PaintMosaic(x, y: Integer; t: array of Integer);
  var
    i: Integer;
    c, m: TColor;
  begin
    c := IfThen(FColor = 0, 1, FColor);
    FillRect(x, y, x + FTextWidth, y + FTextHeight, c);
    FColor := Get32Color(FFont.Color);
    FColor := IfThen(FColor = 0, 1, FColor);
    if holdmosaic
      then m := Get32Color(ck) else m := FColor;
    for i := 0 to Length(t) -1 do
    begin
      case t[i] of
        0: FillRect(x, y, x + 7, y + 6, m);
        1: FillRect(x + 7, y, x + 14, y + 6, m);
        2: FillRect(x, y + 6, x + 7, y + 14, m);
        3: FillRect(x + 7, y + 6, x + 14, y + 14, m);
        4: FillRect(x, y + 14, x + 7, y + 21, m);
        5: FillRect(x + 7, y + 14, x + 14, y + 21, m);
      end;
    end;
    FColor := c;
  end;

var
  s: String;
  i, k: Integer;
  b: Byte;
  c: PCardinal;
  mosaic: Boolean;
  lastchar: Byte;
  changemosaic: Boolean;
  conceal: Boolean;
  boxed: Boolean;
  nextboxed: Boolean;
  nextboxedend: Boolean;
//  old_mode: Integer;
begin
  if FIgnoreLine = Line then
  begin
    FIgnoreLine := -1;
    Exit;
  end;

  lastchar := $20;
  holdmosaic := False;
  changemosaic := False;
  conceal := False;
  boxed := False;

  FFont.Color := clWhite;
  FColor := clBlack;

  doubleheight := False;
//  doublewidth := False;
  doublesize := False;
  flash := False;
  ck := clBlack;

  if Line <> 0 then
  begin
    UnParityTeletextLine(Text);
  end else
  begin
    if not (FSupressHeader or FNewsFlash or FSubTitle) then
    begin
      if not FPageFound
        then FFont.Color := clYellow;
      if Length(FCurrentNumber) > 0 then
      begin
        s := format('%.3d/%.2d', [FPage, FSubPage]);
        Text[1] := '?';
        Text[2] := '?';
        Text[3] := '?';

        for i := 0 to Length(FCurrentNumber) -1 do
        begin
          Text[i+1] := FCurrentNumber[i+1];
        end;
      end else
      begin
        s := format('%.3d/%.2d', [FPage, FSubPage]);
        Text[1] := s[1];
        Text[2] := s[2];
        Text[3] := s[3];
      end;
      Text[4] := s[4];
      if not FPageFound then
      begin
        Text[5] := '?';
        Text[6] := '?';
      end else
      begin
        Text[5] := s[5];
        Text[6] := s[6];
      end;
    end;
  end;

  FillRect(0, Line * FTextHeight, FBitmapWidth, Line * FTextHeight + FTextHeight, clBlack);

  s := '';
  mosaic := False;
  nextboxed := False;
  nextboxedend := False;
  
  if (Line <> 0) or not (FSupressHeader or FNewsFlash or FSubTitle) then
  begin
    for i := 0 to 39 do
    begin
      if nextboxed then
      begin
        boxed := True;
        nextboxed := False;
      end;
      if nextboxedend then
      begin
        boxed := False;
        nextboxedend := False;
      end;
      if changemosaic then
      begin
        holdmosaic := False;
        lastchar := $20;
        changemosaic := False;
      end;
      if flash then
      begin
        ck := FFont.Color;
        FFont.Color := FColor;
        FColor := ck;
        flash := False;
      end;
      b := Byte(Text[i]);
      case b of
        $00 .. $07:
        begin
          ck := FFont.Color;
          FFont.Color := GetTeletextColor(b);
          mosaic := False;
          changemosaic := False;
          conceal := False;
  //        holdmosaic := False;
  //        lastchar := $20;
        end;
        $08:
        begin
          // Flash ("Set-After")
          // This code causes the foreground pixels of the following alphanumeric and mosaics
          // characters to alternate between the foreground and background colours. The flash action is
          // cancelled by a Steady command (0/9) or by the start of a new row.
          flash := True;
//          OutputDebugString(PChar('Flash' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $09:
        begin
          // Steady ("Set-At") - Start-of-row default condition.
          // This code cancels the flash action of code 0/8.
          flash := False;
          ck := clBlack;
          FFont.Color := clWhite;
          FColor := clBlack;

//          OutputDebugString(PChar('Steady' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $0A:
        begin
          // End Box ("Set-After") - Start-of-row default condition.
          // This code cancels the action of the Start Box code 0/B
//          OutputDebugString(PChar('End Box' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
          nextboxedend := True;
        end;
        $0B:
        begin
          // Start Box ("Set-After")
          // On pages with the C5 or C6 bits set (Newsflash or subtitle), this code defines (on each
          // appropriate row) the start of an area that is to be boxed into the normal video picture.
          // Characters outside this area are not displayed, but changes in display mode, colour, height
          // etc., will affect the boxed area. Cancelled by an End Box code (0/A) or by the start of a new row.
          // NOTE: Protection against false operation is provided by double transmission of Start Box
          // control characters, with the action taking place between them.
//          OutputDebugString(PChar('Start Box' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
          
          // NOTE: This is out of the Specs, but for me Subtitles looks better without a Black Rectangle 
          // in the Background ...
//          boxed := False;
          if (FSubTitle or FNewsFlash)// and not FTransparent
            then nextboxed := True;
        end;
        $0C:
        begin
          // Normal Size ("Set-At") - Start-of-row default condition.
          // This code cancels the action of the double height, double width and double size codes, and
          // restores the characters to normal, single row height and single character width.
          doubleheight := False;
  //        doublewidth := False;
          doublesize := False;
          changemosaic := False;

//          OutputDebugString(PChar('Normal Size' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $0D:
        begin
          // Double Height ("Set-After")
          // The characters and mosaics following a double height code are stretched into the following
          // row. The origin of a character is the upper character position. The whole of an enlarged
          // character is displayed with the attributes that apply to the origin of the character.
          // When double height (or double size) characters are used on a given row, the row below
          // normal height characters on that row is displayed with the same local background colour
          // and no foreground data. Any transmitted Level 1 characters and attributes for the lower row
          // are ignored. ("Local background colour" is defined as the background colour invoked for the
          // character cell immediately above as a result of processing Level 1 data, active objects and
          // local enhancement data.)
          // NOTE 1: The broadcaster should not insert double height control characters in rows 23 or
          // 24.
          // NOTE 2: The application of one size-related control code (double height, double width or
          // double size) terminates the action of any other Level 1 spacing size-related
          // attribute or Level 2.5/3.5 non-spacing size-related attribute.
          doubleheight := True;
          FIgnoreLine := Line + 1;
          changemosaic := False;

//          OutputDebugString(PChar('Double Height' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $0E:
        begin
          // Double Width ("Set-After")
          // Characters are to be stretched horizontally to occupy the next character-space.
          // NOTE 4: Double Width should be used with caution as it is interpreted by some existing
          // Level 1 and Level 1.5 decoders. Since this attribute has a "Set-After" function, to
          // ensure correct displqys it must not be inserted in either of the last two columns
          // positions of any display row, nor immediately preceding the edge of a boxed
          // area".
          // NOTE 5: The application of one size-related control code (double height, double width or
          // double size) terminates the action of any other Level 1 spacing size-related
          // attribute or Level 2.5/3.5 non-spacing size-related attribute.
  //        doublewidth := True;

          changemosaic := False;
//          OutputDebugString(PChar('Double Width' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $0F:
        begin
          // Double Size ("Set-After")
          // Characters are to be stretched horizontally and vertically as for both double height and
          // double width characters.
          // When double size characters are used on a given row, the row below normal height
          // characters on that row is displayed with the same local background colour and no
          // foreground data. Any transmitted Level one characters and attributes for the lower row are
          // ignored. ("Local background colour" is defined as the background colour invoked for the
          // character cell immediately above as a result of processing Level 1 data, active objects and
          // local enhancement data.)
          // Characters defined by enhancement data (local X/26 or Objects) may be addressed to
          // locations on the lower row that are not occupied by the lower parts of double height/size
          // characters and will be displayed.
          doublesize := True;
          changemosaic := False;
          FIgnoreLine := Line + 1;

//          OutputDebugString(PChar('Double Size' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $10 .. $17:
        begin
          FFont.Color := GetTeletextColor(b - $10);
          changemosaic := True;
          mosaic := True;
          conceal := False;
        end;
        $18:
        begin
          // Conceal ("Set-At")
          // The following characters up to the end of the row, or until a Colour Code attribute (codes 0/0
          // to 0/7 or 1/0 to 1/7) is encountered, are to be displayed as SPACES until revealed by a
          // decoder or user operation.
          conceal := True;
//          OutputDebugString(PChar('Conceal' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $19:
        begin
          // Contiguous Mosaic Graphics ("Set-At") - Start-of-row default condition
          // The blocks of a mosaics character adjoin one another.
          // NOTE 11: This code has effect only on characters defined at Levels 1 and 1.5.
//          OutputDebugString(PChar('Contiguous Mosaic Graphics' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $1A:
        begin
          // Separated Mosaic Graphics ("Set-At")
          // Each block of a mosaics character is surrounded by a border of the background colour.
          // NOTE 12: This code has effect only on characters defined at Levels 1 and 1.5.
//          OutputDebugString(PChar('Separated Mosaic Graphics' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $1B:
        begin
          // ESC (or Switch) ("Set-After")
          // Toggles between the first and second G0 sets defined by packets X/28/0 Format 1, X/28/4,
          // M/29/0 or M/29/4. The default at the start of each row is the default G0 set. This does not
          // affect characters written via packets X/26 or as part of an object.
          // NOTE 13: May also be used in connection with data for processing.
//          OutputDebugString(PChar('ESC (or Switch)' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $1C:
        begin
          FColor := clBlack;
        end;
        $1D:
        begin
          FColor := Get32Color(FFont.Color);
        end;
        $1E:
        begin
          // Hold Mosaics ("Set-At")
          // Generally, all spacing attributes are displayed as spaces, implying at least one space
          // between characters or mosaics with different colours in the same row. In mosaics mode, the
          // "Hold Mosaics" option allows a limited range of attribute changes without intervening
          // spaces. A mosaic character from the G1 set (referred to as the "Held-Mosaic" character) is
          // displayed in place of the character "SPACE" corresponding to a control character.
          // Substitution only takes place in mosaics mode when Hold Mosaics mode is in force. At a
          // screen location where substitution is permitted, the "Held-Mosaic" character inserted is the
          // most recent mosaics character with bit 6 = '1' in its code on that row. The "Held-Mosaic"
          // character is reset to "SPACE" at the start of each row, on a change of
          // alphanumeric/mosaics mode or on a change of size. It is not reset by reinforcement of the
          // existing size setting. It is not reset by a change in Hold Mosaics mode.
          // The "Held-Mosaic" character is always displayed in its original contiguous or separated form
          // regardless of the mode prevailing at the time of substitution.
          // NOTE 14: This code has effect only on characters defined at Levels 1 and 1.5. Size and
          // character set changes invoked by enhancement triplets at Levels 2.5 and 3.5 do
          // not cancel Hold Mosaics.
          holdmosaic := True;
          ck := FFont.Color;

//          OutputDebugString(PChar('Hold Mosaics' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end;
        $1F:
        begin
          // Release Mosaics ("Set-After") - Start-of-row default condition.
          // This code cancels the Hold Mosaics mode.
          // NOTE 15: This code has effect only on characters defined at Levels 1 and 1.5.
          changemosaic := False;

//          OutputDebugString(PChar('Release Mosaics' + ' at ' + inttostr(Line) + '/' + inttostr(i)));
        end else
        begin
          if not holdmosaic
            then lastchar := b;
        end;
      end;

      if mosaic then
      begin
        if (FSubTitle or FNewsFlash) and not boxed
          then Continue;

        if holdmosaic
          then b := lastchar;

        case b of
          $21: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0]);
          $22: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1]);
          $23: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1]);
          $24: PaintMosaic(i * FTextWidth, Line * FTextHeight, [2]);
          $25: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 2]);
          $26: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 2]);
          $27: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 2]);
          $28: PaintMosaic(i * FTextWidth, Line * FTextHeight, [3]);
          $29: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 3]);
          $2A: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 3]);
          $2B: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 3]);
          $2C: PaintMosaic(i * FTextWidth, Line * FTextHeight, [2, 3]);
          $2D: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 2, 3]);
          $2E: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 2, 3]);
          $2F: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 2, 3]);

          $30: PaintMosaic(i * FTextWidth, Line * FTextHeight, [4]);
          $31: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 4]);
          $32: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 4]);
          $33: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 4]);
          $34: PaintMosaic(i * FTextWidth, Line * FTextHeight, [2, 4]);
          $35: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 2, 4]);
          $36: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 2, 4]);
          $37: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 2, 4]);
          $38: PaintMosaic(i * FTextWidth, Line * FTextHeight, [3, 4]);
          $39: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 3, 4]);
          $3A: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 3, 4]);
          $3B: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 3, 4]);
          $3C: PaintMosaic(i * FTextWidth, Line * FTextHeight, [2, 3, 4]);
          $3D: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 2, 3, 4]);
          $3E: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 2, 3, 4]);
          $3F: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 2, 3, 4]);

          $60: PaintMosaic(i * FTextWidth, Line * FTextHeight, [5]);
          $61: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 5]);
          $62: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 5]);
          $63: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 5]);
          $64: PaintMosaic(i * FTextWidth, Line * FTextHeight, [2, 5]);
          $65: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 2, 5]);
          $66: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 2, 5]);
          $67: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 2, 5]);
          $68: PaintMosaic(i * FTextWidth, Line * FTextHeight, [3, 5]);
          $69: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 3, 5]);
          $6A: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 3, 5]);
          $6B: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 3, 5]);
          $6C: PaintMosaic(i * FTextWidth, Line * FTextHeight, [2, 3, 5]);
          $6D: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 2, 3, 5]);
          $6E: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 2, 3, 5]);
          $6F: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 2, 3, 5]);

          $70: PaintMosaic(i * FTextWidth, Line * FTextHeight, [4, 5]);
          $71: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 4, 5]);
          $72: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 4, 5]);
          $73: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 4, 5]);
          $74: PaintMosaic(i * FTextWidth, Line * FTextHeight, [2, 4, 5]);
          $75: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 2, 4, 5]);
          $76: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 2, 4, 5]);
          $77: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 2, 4, 5]);
          $78: PaintMosaic(i * FTextWidth, Line * FTextHeight, [3, 4, 5]);
          $79: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 3, 4, 5]);
          $7A: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 3, 4, 5]);
          $7B: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 3, 4, 5]);
          $7C: PaintMosaic(i * FTextWidth, Line * FTextHeight, [2, 3, 4, 5]);
          $7D: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 2, 3, 4, 5]);
          $7E: PaintMosaic(i * FTextWidth, Line * FTextHeight, [1, 2, 3, 4, 5]);
          $7F: PaintMosaic(i * FTextWidth, Line * FTextHeight, [0, 1, 2, 3, 4, 5]);
          else FillRect(i * FTextWidth, Line * FTextHeight, i * FTextWidth + FTextWidth, Line * FTextHeight + FTextHeight, IfThen(FColor = 0, 1, FColor));
        end;
      end else
      begin
        b := Byte(GetVTXChar(Char(b), FLanguage));
        if (b < $20) or (b = $7F)
          then b := $20;
        if conceal
          then b := $20;

        if (FSubTitle or FNewsFlash) and not boxed then
        begin
          FillRect(i * FTextWidth, Line * FTextHeight, i * FTextWidth + FTextWidth, Line * FTextHeight + FTextHeight, 0);
          Continue;
        end;

        if boxed and not FTransparent
          then FillRect(i * FTextWidth, Line * FTextHeight, i * FTextWidth + FTextWidth, Line * FTextHeight + FTextHeight, IfThen(FColor = 0, 1, FColor))
          else FillRect(i * FTextWidth, Line * FTextHeight, i * FTextWidth + FTextWidth, Line * FTextHeight + FTextHeight, FColor);
        TextOut(i * FTextWidth, Line * FTextHeight, String(Char(b)));

        changemosaic := False;
      end;
    end;
  end;

  for k := Line * FTextHeight to Line * FTextHeight + FTextHeight -1 do
  begin
    c := PCardinal(@FBuffer[k * FBitmapWidth]);
    if FSubTitle or FTransparent or FNewsFlash then
    begin
      for i := 0 to FBitmapWidth -1 do
      begin
        if ((c^ and $FFFFFF) = 0) or (((c^ and $FFFFFF) = 1) and FTransparent)
          then c^ := $00000000
          else c^ := c^ or $FF000000;
        inc(c);
      end;
    end else
    begin
      for i := 0 to FBitmapWidth -1 do
      begin
        c^ := c^ or $FF000000;
        inc(c);
      end;
    end;
  end;

  if doubleheight or doublesize then
  begin
//    old_mode := SetStretchBltMode(FDevice, Windows.HALFTONE);
    StretchBlt(FDevice, 0, Line * FTextHeight, 40 * FTextWidth, 2 * FTextHeight, FDevice, 0, Line * FTextHeight, FTextWidth * 40, FTextHeight, SRCCOPY);
//    SetStretchBltMode(FDevice, old_mode);
  end;

  FFont.Color := clWhite;
  FColor := clBlack;
end;

procedure TDVBTeletextParser.FillRect(X1, Y1, X2, Y2: Integer; Value: TColor);
var
  j: Integer;
  i: Integer;
  p: PCardinal;
begin
  for j := Y1 to Y2 - 1 do
  begin
    i := x2 - x1;
    p := @FBuffer[j * FBitmapWidth + X1];
    while (i > 0) do
    begin
      p^ := Cardinal(Value) or $FF000000;
      inc(p);
      dec(i);
    end;
  end;
end;

procedure TDVBTeletextParser.TextOut(X, Y: Integer; const Text: String);
begin
  SetTextColor(FDevice, ColorToRGB(IfThen(FFont.Color = 0, 1, FFont.Color)));
  ExtTextout(FDevice, X, Y, 0, nil, PChar(Text), Length(Text), nil);
end;

procedure TDVBTeletextParser.OnTeletextLine(Magazine: Integer; Line: Integer; Text: PTeletextLine);
var
  i: Integer;
  pg, pg2: Integer;
  page: TTeletextPage;
  valid: Boolean;
  lower: Integer;
  designation: Integer;
//  t: TTeletextLine;
//  a: Integer;
begin
//
//  if line = 0 then
//  begin
//    a := (Magazine shl 8) or HammingDecode(PByteArray(Text));
//    if not TryStrToInt(IntToHex(a, 4), a) then
//    begin
//      a := -1;
//    end;
//    OutputDebugString(PChar('HEADER: ' + inttostr(Magazine) + ' - ' + inttostr(Line) + ' - ' + inttostr(a)));
//  end else
//  begin
//    OutputDebugString(PChar('LINE: ' + inttostr(Magazine) + ' - ' + inttostr(Line)));
//  end;
//

  // TOP Text
//  if FLastPage = 1 then
//  begin
//    t := Text^;
//    UnParityTeletextLine(@t);
//    OutputDebugString(PChar(inttostr(Line) + ' ' + String(t) + ' ' + GetHexString(@t, 40)));
//  end;

  if Line = 0 then
  begin
    lower := HammingDecode(PByteArray(Text));
    if lower = $FF
      then Exit;

    pg := (Magazine shl 8) or lower;
    valid := TryStrToInt(IntToHex(pg, 4), pg2) and (pg2 > 99) and (pg2 < 900);
    FLastPage := pg2;

//    OutputDebugString(PChar(inttostr(FTeletextBuffer.FList.Count)));

    if (FLastPage > 0) and not FInternalBuffer then
    begin
      page := FTeletextBuffer.GetPage(FLastPage);
      if Assigned(page) then
      begin
//          OutputDebugString(PChar('updating page for ' + inttostr(FLastPAge)));
        page.Clear;
        page.Magazine := Magazine;
        page.SetLine(Line, Text);
      end else
      begin
//          OutputDebugString(PChar('creating new TT Page for ' + inttostr(FLastPAge)));
        page := TTeletextPage.Create;
        page.PageID := FLastPage;
        page.Magazine := Magazine;
        page.SetLine(Line, Text);
        FTeletextBuffer.AddPage(page);
      end;
    end;

    if (FLastPage = FPage) then
    begin
      FErasePage := (Byte(Text[3]) and $80) = $80;
      FNewsflash := (Byte(Text[5]) and $20) = $20;
      FSubtitle := (Byte(Text[5]) and $80) = $80;
      FSupressHeader := (Byte(Text[6]) and $02) = $02;
      FUpdateIndicator := (Byte(Text[6]) and $08) = $08;
      FInterruptedSequence := (Byte(Text[6]) and $20) = $20;
      FInhibitDisplay := (Byte(Text[6]) and $80) = $80;
      FMagazineSerial := (Byte(Text[7]) and $02) = $02;

      FSubPage := ((HammingDecode(PByteArray(@Text[4])) shl 8) or HammingDecode(PByteArray(@Text[2]))) and $3F7F;
      FLanguage := (HammingDecode(PByteArray(@Text[6])) shr 5) and $07;

      FLastMagazine := Magazine;
      FPageFound := True;
      FIgnoreLine := -1;
//if FInternalBuffer
//    then   OutputDebugString(PChar('NEW: ' + inttostr(FPage) + ' - ' + inttostr(FLastPage) + ' - ' + inttostr(FLastMagazine)));

      case FLanguage of
        3:
        begin
          if FFont.Charset <> EASTEUROPE_CHARSET
            then FFont.Charset := EASTEUROPE_CHARSET;
        end;
        else
        begin
          if FFont.Charset <> DEFAULT_CHARSET
            then FFont.Charset := DEFAULT_CHARSET;
        end;
      end;

      FillChar(FText, SizeOf(TTeletextArray), $20);

      if FSupressHeader and FShowTeletext
        then FillRect(0, 0, FBitmapWidth, FTextHeight, clBlack);

// Page 100 -> 0 - 0 - 0 - 0 - 1 - 1 - 0 - 1
// Page 333->  0 - 1 - 0 - 0 - 0 - 0 - 0 - 1

//        OutputDebugString(PChar(
//          inttostr(integer(FErasePage)) + ' - ' +
//          inttostr(integer(FNewsflash)) + ' - ' +
//          inttostr(integer(FSubtitle)) + ' - ' +
//          inttostr(integer(FSupressHeader)) + ' - ' +
//          inttostr(integer(FUpdateIndicator)) + ' - ' +
//          inttostr(integer(FInterruptedSequence)) + ' - ' +
//          inttostr(integer(FInhibitDisplay)) + ' - ' +
//          inttostr(integer(FMagazineSerial))
//        ));

      FFont.Color := clWhite;
      FColor := clBlack;

      if (FErasePage or FNeedErase) and FShowTeletext
        then FillRect(0, FTextHeight, FBitmapWidth, FBitmapHeight, clBlack);
      FNeedErase := False;

      if FShowTeletext
        then UpdateTransparency;
    end;

//    if FLastMagazine = Magazine then
//    begin
    if valid then
    begin
      UnParityTeletextLine(Text);
      FText[0] := Text^;

      if not FSupressHeader and FShowTeletext then
      begin
        for i := 0 to 7
          do Byte(Text[i]) := $20;
        DrawLine(Line, Text);
      end;
    end;
    Exit;
  end;

//  if FInternalBuffer
//    then OutputDebugString(PChar('MAG: ' + inttostr(Magazine) + ' - ' + inttostr(FLastMagazine) + ' - ' + inttostr(FLastPage) + ' - ' + inttostr(FPage) + ' - ' + inttostr(Line) + ' - ' + inttostr(integer(FInhibitDisplay))));

  if (Line > 29) or (Line < 0)
    then Exit;

//  OutputDebugString(PChar(inttostr(Magazine)));

  if (FLastPage > 0) and not FInternalBuffer then
  begin
    page := FTeletextBuffer.GetPage(FLastPage);
    if Assigned(page) and (page.Magazine = Magazine) then
    begin
      if Line = 27 then
      begin
        designation := HammingDecode(PByteArray(@Text[0]));
        if (designation and $F) = 0 then
        begin
          page.SetLine(Line, Text);
        end;
      end else
      begin
//        then  OutputDebugString(PChar('TT Page Setline for ' + inttostr(FLastPAge) + ' - ' + inttostr(Line) + ' - ' + String(Text^)));
        page.SetLine(Line, Text);
      end;
    end;
  end;

//  OutputDebugString(PChar('PAGE: ' + inttostr(FPage) + ' - ' + inttostr(FLastPage)));

  if (Line > 24)
    then Exit;

  if (FLastPage <> FPage) or FInhibitDisplay or (FLastMagazine <> Magazine)
    then Exit;

  if FShowTeletext
    then DrawLine(Line, Text);
    
  FText[Line] := Text^;
end;

procedure TDVBTeletextParser.SetPID(APID: Integer);
begin
  FLock.Enter;
  try
    FErasePage := False;
    FNewsflash := False;
    FSubtitle := False;
    FSupressHeader := False;
    FUpdateIndicator := False;
    FInterruptedSequence := False;
    FInhibitDisplay := False;
    FMagazineSerial := False;
    FTeletextParser.PID := APID;
    SetPage(FPage);
  finally
    FLock.Leave;
  end;
end;

function TDVBTeletextParser.GetPID: Integer;
begin
  Result := FTeletextParser.PID;
end;

procedure TDVBTeletextParser.SetPage(APage: Integer);
var
  p: TTeletextPage;
  i: Integer;
  line: TTeletextLine;
  k: Integer;
begin
  FLock.Enter;
  try
    FCurrentNumber := '';
    k := FLastPage;
    FInternalBuffer := True;
    FLastVisiblePage := FPage;
    FPage := APage;
    p := FTeletextBuffer.GetPage(APage);
    FNeedErase := True;
    FPageFound := Assigned(p);
    if Assigned(p) then
    begin
      FillRect(0, FTextHeight, FBitmapWidth, FBitmapHeight, clBlack);
      for i := 0 to 24 do
      begin
        line := p.GetLine(i);
        OnTeletextLine(p.Magazine, i, @line);
      end;
    end else
    begin
      FErasePage := False;
      FNewsflash := False;
      FSubtitle := False;
      FSupressHeader := False;
      FUpdateIndicator := False;
      FInterruptedSequence := False;
      FInhibitDisplay := False;
      FMagazineSerial := False;
    end;
    FInternalBuffer := False;
    FLastPage := k;
  finally
    FLock.Leave;
  end;
end;

procedure TDVBTeletextParser.SetShowTeletext(AShow: Boolean);
begin
  FLock.Enter;
  try
    FShowTeletext := AShow;
  finally
    FLock.Leave;
  end;
end;

procedure TDVBTeletextParser.SetTransparent(ATransparent: Boolean);
begin
  FLock.Enter;
  try
    FTransparent := ATransparent;
    SetPage(FPage);
    UpdateTransparency;
  finally
    FLock.Leave;
  end;
end;

procedure TDVBTeletextParser.Clear;
begin
  FLock.Enter;
  try
    FLastPage := -1;
    FFont.Color := clWhite;
    FColor := clBlack;
    FillRect(0, 0, FBitmapWidth, FBitmapHeight, clBlack);
    FillChar(FText, SizeOf(TTeletextLine) * 25, $20);
    FTeletextBuffer.Clear;
    UpdateTransparency;
  finally
    FLock.Leave;
  end;
end;

procedure TDVBTeletextParser.SaveTeletext(AType: TSaveTeletextType; AFilename: WideString);
var
  str: TFileStream;
  bfh: TBitmapFileHeader;
  bi: TBitmapInfo;
  i, c: Integer;
  s: String;
  text: array[0..24] of TTeletextLine;
begin
  case AType of
    ttBitmap:
    begin
      str := TFileStream.Create(AFilename, fmCreate);

      FillChar(bfh, SizeOf(TBitmapFileHeader), 0);
      FillChar(bi, SizeOf(TBitmapInfo), 0);

      try
        bi.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
        bi.bmiHeader.biWidth := FBitmapWidth;
        bi.bmiHeader.biHeight := -FBitmapHeight;
        bi.bmiHeader.biPlanes := 1;
        bi.bmiHeader.biBitCount := 32;
        bi.bmiHeader.biCompression := BI_RGB;
        bi.bmiHeader.biSizeImage := GetBitmapSize(@bi.bmiHeader);
        bi.bmiHeader.biXPelsPerMeter := 0;
        bi.bmiHeader.biYPelsPerMeter := 0;
        bi.bmiHeader.biClrUsed := 0;
        bi.bmiHeader.biClrImportant := 0;

        bfh.bfReserved1 := 0;
        bfh.bfReserved2 := 0;
        bfh.bfSize := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader) + bi.bmiHeader.biSizeImage;
        bfh.bfType := $4D42;
        bfh.bfOffBits := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader);

        str.Write(bfh, SizeOf(TBitmapFileHeader));
        str.Write(bi.bmiHeader, SizeOf(TBitmapInfoHeader));
        str.Write(FBuffer[0],FBitmapHeight * FBitmapWidth * 4);
      finally
        str.Free;
      end;
    end;
    ttVTX:
    begin
      str := TFileStream.Create(AFilename, fmCreate);
      for i := 0 to 24 do
      begin
        s := FText[i] + #13#10;
        str.Write(s[1], 42);
      end;
      str.Free;
    end;
    ttTXT:
    begin
      Move(FText, text, 40 * 25);
      for i := 0 to 24 do
      begin
        for c := 0 to 39 do
        begin
          text[i][c] := GetVTXChar(text[i][c], FLanguage);
          if (Byte(text[i][c]) < $20) or (Byte(text[i][c]) = $7F)
            then text[i][c] := ' ';
        end;
      end;
      str := TFileStream.Create(AFilename, fmCreate);
      for i := 0 to 24 do
      begin
        s := text[i] + #13#10;
        str.Write(s[1], 42);
      end;
      str.Free;
    end;
  end;
end;

procedure TDVBTeletextParser.ParseTeletextBuffer(ABuffer: PByte; ASize: Integer);
begin
  FLock.Enter;
  try
    if (* not FShowTeletext or *) FInternalBuffer
      then Exit;

    FTSParser.ParseBuffer(ABuffer, ASize);
  finally
    FLock.Leave;
  end;
end;

function TDVBTeletextParser.GetBuffer: PCardinalArray;
begin
  Result := FBuffer;
end;

procedure TDVBTeletextParser.SetPageNumber(ANumber: Integer);
begin
  FLock.Enter;
  try
    try
      if (Length(FCurrentNumber) = 0) and ((ANumber < 1) or (ANumber > 8))
        then Exit;

      if Length(FCurrentNumber) = 3
        then FCurrentNumber := '';

      FSupressHeader := False;
      FSubTitle := False;
      FNewsFlash := False;

      FCurrentNumber := FCurrentNumber + inttostr(ANumber);

      if Length(FCurrentNumber) = 3
        then SetPage(strtoint(FCurrentNumber));
    except
      FCurrentNumber := '';
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TDVBTeletextParser.SetFastext(AFastext: TTeletextFastext);
var
  page: TTeletextPage;
  line: TTeletextLine;
  pagenr, pg: Integer;
  designation: Integer;
  magazine: Integer;
  x: Integer;
begin
  FLock.Enter;
  try
    page := FTeletextBuffer.GetPage(FPage);
    if Assigned(page) then
    begin
      line := page.GetLine(27);
      designation := HammingDecode(PByteArray(@line[0]));
      if designation and $F = 0 then
      begin
        x := (Integer(AFastext) * 6) + 1;
        pagenr := HammingDecode(PByteArray(@line[x]));

        if (pagenr = $FF) then
        begin
          if AFastext = tfeInfo
            then SetPage(100);
          Exit;
        end;

        // The Magazine is coded in the 1st, 2nd and 9th most significant Bit of
        // the Subpage and XOR'ed with the current Magazine ... totally insane ...
        magazine := HammingDecode(PByteArray(@line[x+4]));
        magazine := (magazine shl 8) or HammingDecode(PByteArray(@line[x+2]));
        magazine := (((magazine shr 13) and $06) or ((magazine and $80) shr 7)) xor page.Magazine;

        pagenr := (magazine shl 8) or pagenr;
        if TryStrToInt(IntToHex(pagenr, 4), pg) and (pg > 99) and (pg < 900) then
        begin
          SetPage(pg);
        end;
      end;
    end else
    begin
      if AFastext = tfeInfo
        then SetPage(100);
    end;
  finally
    FLock.Leave;
  end;
end;

function TDVBTeletextParser.HasColorAtChar(AIndex: Integer; AColor: TColor): Boolean;
var
  i: Integer;
  c: Integer;
  p: PCardinal;
  cl: Cardinal;
begin
  cl := Get32Color(AColor) and $00FFFFFF;
  for i := 0 to FTextHeight -1 do
  begin
    p := @FBuffer[(24 * FTextHeight * FBitmapWidth) + (i * FBitmapWidth) + (AIndex * FTextWidth)];
    for c := 0 to FTextWidth -1 do
    begin
      if p^ and $00FFFFFF = cl then
      begin
        Result := True;
        Exit;
      end;
      inc(p);
    end;
  end;
  Result := False;
end;

function TDVBTeletextParser.LineIsDouble(AIndex: Integer): Boolean;
var
  i: Integer;
  line: TTeletextLine;
begin
  line := FText[AIndex];
  UnParityTeletextLine(@line);
  for i := 0 to 39 do
  begin
    if Byte(line[i]) = $0D then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

procedure TDVBTeletextParser.CycleTeletextPage;
begin
  if FLastVisiblePage = -1 
    then Exit;

  SetPage(FLastVisiblePage);
end;

end.
