unit EPGWindow;

interface

uses
  Windows, Classes, Forms, SysUtils, Graphics, Controls;

type
  TEPGChannel = class
  private
    FName: String;
  end;

  TEPGWindow = class(TCustomControl)
  private
    FChannels: TList;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    procedure AddChannel(const AChannelName: String);
  end;

implementation

constructor TEPGWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChannels := TList.Create;
end;

destructor TEPGWindow.Destroy;
begin
  Clear;
  FChannels.Free;
  inherited Destroy;
end;

procedure TEPGWindow.Clear;
var
  i: Integer;
begin
  for i := 0 to FChannels.Count -1
    do TEPGChannel(FChannels[i]).Free;
  FChannels.Clear;

  Invalidate;
end;

procedure TEPGWindow.AddChannel(const AChannelName: String);
var
  channel: TEPGChannel;
begin
  channel := TEPGChannel.Create;
  channel.FName := AChannelName;
  FChannels.Add(channel);

  Invalidate;
end;

procedure TEPGWindow.Paint;
var
  i: Integer;
  largest_text_width: Integer;
  c: Integer;
  cnt: Integer;
begin
  Canvas.Pen.Color := clWindow;
  Canvas.Brush.Color := clWindow;
  Canvas.Rectangle(Canvas.ClipRect);

  Canvas.Font.Name := 'Tahoma';
  Canvas.Font.Color := clWindowText;


  largest_text_width := 0;
  for i := 0 to FChannels.Count -1 do
  begin
    c := Canvas.TextWidth(TEPGChannel(FChannels[i]).FName);
    if c > largest_text_width
      then largest_text_width := c;
  end;

  if largest_text_width > 0 then
  begin
    Canvas.Pen.Color := clDkGray;
    Canvas.Brush.Color := clDkGray;
    Canvas.Rectangle(0, 0, largest_text_width + 10, Height);

    c := 15;
    cnt := FChannels.Count;
    while c < Height do
    begin
      dec(cnt);
      if cnt < 0
        then break;
      Canvas.Pen.Color := clGrayText;
      Canvas.MoveTo(0, c);
      Canvas.LineTo(Width, c);
      inc(c, 15);
    end;

    for i := 0 to FChannels.Count -1 do
    begin
      Canvas.TextOut(5, i * 15 + 1, TEPGChannel(FChannels[i]).FName);
    end;
  end;
end;

end.
