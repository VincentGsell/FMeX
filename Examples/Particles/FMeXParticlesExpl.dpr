 program FMeXParticlesExpl;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  FMeX.Images in '..\..\Sources\FMeX.Images.pas',
  FMeX.Ticks in '..\..\Sources\FMeX.Ticks.pas',
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas',
  FMeX.Graph in '..\..\Sources\FMeX.Graph.pas',
  FMeX.Gx.Image in '..\..\Sources\FMeX.Gx.Image.pas',
  FMeX.Gx.Particles2D in '..\..\Sources\FMeX.Gx.Particles2D.pas',
  GS.ParticleEngine2D in '..\..\Sources\ThirdParts\GS.ParticleEngine2D.pas',
  FMeX.Gx.Types in '..\..\Sources\FMeX.Gx.Types.pas',
  GS.System.Cadencer in '..\..\..\GS\pascal\GS.Core\GS.System.Cadencer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
