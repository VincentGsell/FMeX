program ProjectVoxelsFMeX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Voxels.fmain in 'Voxels.fmain.pas' {Form1},
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas',
  Voxels.Controler in 'Voxels.Controler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
