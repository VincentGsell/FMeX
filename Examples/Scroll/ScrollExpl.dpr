program ScrollExpl;

uses
  System.StartUpCopy,
  FMX.Forms,
  fscroll in 'fscroll.pas' {Form5},
  FMeX.Ticks in '..\..\Sources\FMeX.Ticks.pas',
  FMeX.TileScrolling in '..\..\Sources\FMeX.TileScrolling.pas',
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas',
  FMeX.Images in '..\..\Sources\FMeX.Images.pas',
  FMeX.Graph in '..\..\Sources\FMeX.Graph.pas',
  FMeX.Gx.Types in '..\..\Sources\FMeX.Gx.Types.pas',
  FMeX.Animation in '..\..\Sources\FMeX.Animation.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
