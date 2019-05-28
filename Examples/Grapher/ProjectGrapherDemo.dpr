program ProjectGrapherDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmainGrapher in 'fmainGrapher.pas' {Form23},
  GS.Direction in '..\..\..\GS\GS.Core\GS.Direction.pas',
  FMeX.Types2D.GeometryCommon in '..\..\Sources\FMeX.Types2D.GeometryCommon.pas',
  clipper in '..\..\Sources\ThirdParts\clipper.pas',
  FMeX.Types3D.GeometryCommon in '..\..\Sources\FMeX.Types3D.GeometryCommon.pas',
  FMeX.Types3D.Grapher in '..\..\Sources\FMeX.Types3D.Grapher.pas',
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm23, Form23);
  Application.Run;
end.
