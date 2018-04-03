program FMeXParticlesExpl;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  FMeX.Images in '..\..\Sources\FMeX.Images.pas',
  FMeX.Ticks in '..\..\Sources\FMeX.Ticks.pas',
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas',
  FMeX.Particles2D in '..\..\Sources\FMeX.Particles2D.pas',
  FMeX.Types in '..\..\Sources\FMeX.Types.pas',
  FMeX.Vectors in '..\..\Sources\FMeX.Vectors.pas',
  GS.ParticleEngine2D in '..\..\Sources\ThirdParts\GS.ParticleEngine2D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
