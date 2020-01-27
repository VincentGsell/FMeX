 program FMeXParticlesExpl;

uses
  System.StartUpCopy,
  FMX.Forms,
  fMain in 'fMain.pas' {Form1},
  FMeX.Images in '..\..\Sources\FMeX.Images.pas',
  FMeX.Ticks in '..\..\Sources\FMeX.Ticks.pas',
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas',
  GS.Direction in '..\..\..\GS\GS.Core\GS.Direction.pas',
  FMeX.Graph in '..\..\Sources\FMeX.Graph.pas',
  FMeX.Gx.Image in '..\..\Sources\FMeX.Gx.Image.pas',
  FMeX.Gx.Particles2D in '..\..\Sources\FMeX.Gx.Particles2D.pas',
  GS.ParticleEngine2D in '..\..\Sources\ThirdParts\GS.ParticleEngine2D.pas',
  FMeX.Gx.Types in '..\..\Sources\FMeX.Gx.Types.pas',
  Gx.Types in '..\..\..\Gx\Gx.Types.pas',
  Gx.Graph.Elem.Rect in '..\..\..\Gx\Gx.Graph.Elem.Rect.pas',
  Gx.Graph.FMX in '..\..\..\Gx\Gx.Graph.FMX.pas',
  Gx.Graph.Types in '..\..\..\Gx\Gx.Graph.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
