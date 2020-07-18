program SpineExpl;



uses
  System.StartUpCopy,
  FMX.Forms,
  fmainSpine in 'fmainSpine.pas' {Form1},
  FMeX.Images in '..\..\Sources\FMeX.Images.pas',
  FMeX.Ticks in '..\..\Sources\FMeX.Ticks.pas',
  FMeX.FileFormat2D.Spine in '..\..\Sources\FMeX.FileFormat2D.Spine.pas',
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas',
  Spine in '..\..\Sources\ThirdParts\Spine.pas',
  FMeX.HUD in '..\..\Sources\FMeX.HUD.pas',
  FMeX.Graph in '..\..\Sources\FMeX.Graph.pas',
  FMeX.Gx.Types in '..\..\Sources\FMeX.Gx.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
