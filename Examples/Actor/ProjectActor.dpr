program ProjectActor;

uses
  FMX.Forms,
  fmActor in 'fmActor.pas' {Form7},
  FMeX.Types3D.ActorModels in '..\..\Sources\FMeX.Types3D.ActorModels.pas',
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas',
  FMeX.FileFormat3D.MD2 in '..\..\Sources\FMeX.FileFormat3D.MD2.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
