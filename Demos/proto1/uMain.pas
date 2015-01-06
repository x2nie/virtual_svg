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
    mmo2: TMemo;
    btn2: TButton;
    mmo3: TMemo;
    btn3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
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

type
  PSvgVirtualNode = PVirtualNode;

procedure TForm1.FormCreate(Sender: TObject);
var
  r : ^TDataRectangle;
  p : PSvgVirtualNode;
begin
  svg := TSVG.Create;


end;

procedure TForm1.btn1Click(Sender: TObject);
var
  r : ^TDataRectangle;
  p : PSvgVirtualNode;
begin
  svg.DeleteChildren(svg.RootTag,True);
  p := svg.AddChild(nil, SizeOf(TDataRectangle));

  r := svg.GetData(p);
  r.x := 100;
  r.y := 200;
  r.rx := 300;
  r.ry := 400;

  p := svg.RootTag.FirstChild;
  r := svg.GetData(p);

  mmo1.Lines.Add(Format('x:%f y:%f r:%f,%f',[r.x, r.y, r.rx, r.ry]) );

end;

procedure TForm1.btn2Click(Sender: TObject);
var
  r0 : TDataRectangle;
  r : ^TDataRectangle;
  p : PSvgVirtualNode;
begin
  r0.x := 100;
  r0.y := 200;
  r0.rx := 300;
  r0.ry := 400;

  svg.DeleteChildren(svg.RootTag,True);
  p := svg.AddChild(nil, SizeOf(TDataRectangle), @r0, True);



  p := svg.RootTag.FirstChild;
  r := svg.GetData(p);

  mmo2.Lines.Add(Format('x:%f y:%f r:%f,%f',[r.x, r.y, r.rx, r.ry]) );

end;

procedure TForm1.btn3Click(Sender: TObject);
var
  r0 : TDataRectangle;
  r : ^TDataRectangle;
  p : PSvgVirtualNode;
begin
  r0.x := 100;
  r0.y := 200;
  r0.rx := 300;
  r0.ry := 400;

  svg.DeleteChildren(svg.RootTag,True);
  //p := svg.AddChild(nil, SizeOf(Tdatarectangle), r0);
  p := svg.AddChild(nil, SizeOf(r0), r0);



  p := svg.RootTag.FirstChild;
  r := svg.GetData(p);

  mmo3.Lines.Add(Format('x:%.0f y:%.0f r:%.0f,%.0f',[r.x, r.y, r.rx, r.ry]) );

end;

end.
