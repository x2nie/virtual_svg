unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  VirtualSVG, StdCtrls;

type
  TForm1 = class(TForm)
    btn1: TButton;
    mmo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    svg : TSVG;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  r : ^TDataRectangle;
  p : PVirtualNode;
begin
  svg := TSVG.Create;
  p := svg.AddChild(nil, SizeOf(TDataRectangle));
  r := svg.GetData(p);
  r.x := 100;
  r.y := 200;
  r.rx := 300;
  r.ry := 400;

end;

procedure TForm1.btn1Click(Sender: TObject);
var
  r : ^TDataRectangle;
  p : PVirtualNode;
begin
  p := svg.RootTag.FirstChild;
  r := svg.GetData(p);

  mmo1.Lines.Add(Format('x:%f y:%f r:%f,%f',[r.x, r.y, r.rx, r.ry]) );

end;

end.
