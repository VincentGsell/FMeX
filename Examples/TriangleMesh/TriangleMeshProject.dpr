program TriangleMeshProject;

uses
  System.StartUpCopy,
  FMX.Forms,
  TriangleMesh.fmain in 'TriangleMesh.fmain.pas' {Form1},
  FMeX.Types3D.GSCoreBridge in '..\..\Sources\FMeX.Types3D.GSCoreBridge.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
