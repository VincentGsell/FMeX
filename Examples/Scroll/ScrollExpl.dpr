program ScrollExpl;

uses
  System.StartUpCopy,
  FMX.Forms,
  fscroll in 'fscroll.pas' {Form5},
  FMeX.TileScrolling in '..\..\..\Sources\FMeX.TileScrolling.pas',
  FMeX.Animation in '..\..\..\Sources\FMeX.Animation.pas',
  FMeX.Ticks in '..\..\..\Sources\FMeX.Ticks.pas',
  FMeX.Images in '..\..\..\Sources\FMeX.Images.pas',
  FMeX.Types in '..\..\..\Sources\FMeX.Types.pas',
  FMeX.Types3D in '..\..\..\Sources\FMeX.Types3D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
