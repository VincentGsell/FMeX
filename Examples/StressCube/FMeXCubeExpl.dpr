program FMeXCubeExpl;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmain in 'fmain.pas' {Form1},
  FMeX.Types3D in '..\..\..\..\..\2017\_R&D\FMEX\FMeX_old2016\Sources\FMeX.Types3D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
