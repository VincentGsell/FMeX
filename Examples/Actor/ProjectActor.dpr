program ProjectActor;

uses
  FMX.Forms,
  fmActor in 'fmActor.pas' {Form7},
  FMeX.Types3D in '..\..\FMeX.Types3D.pas',
  FMeX.Types3D.MD2 in '..\..\FMeX.Types3D.MD2.pas',
  FMeX.Types3D.ActorModels in '..\..\FMeX.Types3D.ActorModels.pas',
  FileMD2 in '..\..\..\..\..\glscene-code-6639-branches-GLScene_FMX\glscene-code-6639-branches-GLScene_FMX\Source\FileMD2.pas',
  GLS.VectorGeometry in '..\..\..\..\..\glscene-code-6639-branches-GLScene_FMX\glscene-code-6639-branches-GLScene_FMX\Source\GLS.VectorGeometry.pas',
  GLS.VectorTypes in '..\..\..\..\..\glscene-code-6639-branches-GLScene_FMX\glscene-code-6639-branches-GLScene_FMX\Source\GLS.VectorTypes.pas',
  TypesMD2 in '..\..\..\..\..\glscene-code-6639-branches-GLScene_FMX\glscene-code-6639-branches-GLScene_FMX\Source\TypesMD2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
