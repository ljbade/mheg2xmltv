unit formOSDColor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, pngimage, ExtCtrls, ComCtrls;

type
  TOSDColor = record
    StartColor: TColor;
    EndColor: TColor;
    FontColor: TColor;
    FontShadowColor: TColor;
  end;

  TfrmOSDColor = class(TForm)
    TabControl1: TTabControl;
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    ColorDialog1: TColorDialog;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    FOSDColor: TOSDColor;
  public
    procedure SetOSDColor(AColor: TOSDColor);
    function GetOSDColor: TOSDColor;
  end;

var
  frmOSDColor: TfrmOSDColor;

implementation

{$R *.dfm}

procedure DrawGradient(StartColor : TColor; EndColor : TColor; Bitmap : TBitmap);
var
  j : Single;
  deltas : array[0..2] of Single;
  i : integer;
  r : TRect;
begin
  deltas[0] := (GetRValue(EndColor) - GetRValue(StartColor)) / Bitmap.Height;
  deltas[1] := (GetGValue(EndColor) - GetGValue(StartColor)) / Bitmap.Height;
  deltas[2] := (GetBValue(EndColor) - GetBValue(StartColor)) / Bitmap.Height;
  Bitmap.Canvas.Brush.Style := bsSolid;
  j := 1;
  for i := 0 to Bitmap.Height do
  begin
    r.Left := Bitmap.Width;
    r.Right := 0;
    r.Top := Round(i * j);
    r.Bottom := Round((i + 1) * j);
    Bitmap.Canvas.Brush.Color := RGB(Round(GetRValue(StartColor) + i * Deltas[0]), Round(GetGValue(StartColor) + i *
    Deltas[1]), Round(GetBValue(StartColor) + i * Deltas[2]));
    Bitmap.Canvas.FillRect(r);
  end;
end;

procedure TfrmOSDColor.FormCreate(Sender: TObject);
begin
  BorderIcons := [biSystemMenu];
  Image1.Canvas.Rectangle(0, 0, 2, 2);
end;

procedure TfrmOSDColor.SetOSDColor(AColor: TOSDColor);
var
  log_font: TLogFont;
begin
  FOSDColor := AColor;
  Panel2.Color := FOSDColor.FontColor;
  Panel4.Color := FOSDColor.FontShadowColor;
  Panel1.Color := FOSDColor.StartColor;
  Panel3.Color := FOSDColor.EndColor;

  DrawGradient(FOSDColor.StartColor, FOSDColor.EndColor, Image1.Picture.Bitmap);

  Image1.Canvas.Font.Name := 'Tahoma';
  Image1.Canvas.Font.Style :=  Image1.Canvas.Font.Style + [fsBold];
  Image1.Canvas.Brush.Style := bsClear;

  Image1.Canvas.Font.Color := FOSDColor.FontShadowColor;
  Image1.Canvas.Font.Size := 36;
  GetObject(Image1.Canvas.Font.Handle, SizeOf(log_font), @log_font);
  log_font.lfQuality := ANTIALIASED_QUALITY;
  Image1.Canvas.Font.Handle := CreateFontIndirect(log_font);
  Image1.Canvas.TextOut(16, 19, 'Sample Text');

  Image1.Canvas.Font.Color := FOSDColor.FontColor;
  Image1.Canvas.Font.Size := 36;
  GetObject(Image1.Canvas.Font.Handle, SizeOf(log_font), @log_font);
  log_font.lfQuality := ANTIALIASED_QUALITY;
  Image1.Canvas.Font.Handle := CreateFontIndirect(log_font);
  Image1.Canvas.TextOut(14, 17, 'Sample Text');

  Image1.Invalidate;
end;

function TfrmOSDColor.GetOSDColor: TOSDColor;
begin
  Result := FOSDColor;
end;

procedure TfrmOSDColor.Button3Click(Sender: TObject);
begin
  ColorDialog1.Color := Panel2.Color;
  if ColorDialog1.Execute then
  begin
    FOSDColor.FontColor := ColorDialog1.Color;
    SetOSDColor(FOSDColor);
  end;
end;

procedure TfrmOSDColor.Button4Click(Sender: TObject);
begin
  ColorDialog1.Color := Panel4.Color;
  if ColorDialog1.Execute then
  begin
    FOSDColor.FontShadowColor := ColorDialog1.Color;
    SetOSDColor(FOSDColor);
  end;
end;

procedure TfrmOSDColor.Button5Click(Sender: TObject);
begin
  ColorDialog1.Color := Panel1.Color;
  if ColorDialog1.Execute then
  begin
    FOSDColor.StartColor := ColorDialog1.Color;
    SetOSDColor(FOSDColor);
  end;
end;

procedure TfrmOSDColor.Button6Click(Sender: TObject);
begin
  ColorDialog1.Color := Panel3.Color;
  if ColorDialog1.Execute then
  begin
    FOSDColor.EndColor := ColorDialog1.Color;
    SetOSDColor(FOSDColor);
  end;
end;

procedure TfrmOSDColor.Button7Click(Sender: TObject);
begin
  FOSDColor.StartColor := $00B07D44;
  FOSDColor.EndColor := $00754923;
  FOSDColor.FontColor := clWhite;
  FOSDColor.FontShadowColor := clBlack;
  SetOSDColor(FOSDColor);
end;

end.
