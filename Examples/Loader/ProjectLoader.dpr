program ProjectLoader;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmainloader in 'fmainloader.pas' {Form21},
  FMeX.Types3D in '..\..\Sources\FMeX.Types3D.pas',
  FMeX.FileFormat3D.OBJ in '..\..\Sources\FMeX.FileFormat3D.OBJ.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm21, Form21);
  Application.Run;
end.
