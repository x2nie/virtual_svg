program VS_Project1;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  VirtualSVG in '..\..\Source\VirtualSVG.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
