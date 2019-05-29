unit FMeX.Types3D.ActorModels;

interface

uses
  SysUtils, Classes, Types,
  FMX.Types3D, FMX.Materials,
  FMeX.Types3D, FMX.Forms3D, FMX.Controls3D, FMX.Graphics, FMX.StdCtrls, FMX.MaterialSources;

Type

  TFrame = Record
    FrameName : String;
    FrameIndexStart : Integer;
    FrameCount : Integer;
  End;

  TFrames = Array of TFrame;

  TFMeXModelRessource = Class
  private
    FMaterial: TMaterialSource; //Pointer : Instanciate is Heritate's responsability.
    FData : TMeshData;
    function GetFrameText: String; //Pointer : Idem.
  Public
    Frames: TFrames;

    Procedure LoadFromFile(aFile : String); Virtual; Abstract;
    //Note that frame is single : This is for
    Procedure GetFrame(aFrame : Single; InData : TMeshData); Virtual; Abstract;
    Constructor Create;
    Destructor Destroy; Override;

    Property Material : TMaterialSource read FMaterial Write FMaterial;
    Property DefaultData : TMeshData read FData Write FData;
    Property FrameText : String Read GetFrameText;
  End;

  TFMeXActor = Class(TeCustomMesh)
  Private
    FRessource: TFMexModelRessource;

    procedure SetRessource(const Value: TFMexModelRessource);
 Public
   constructor Create(AOwner: TComponent); override;
   Destructor Destroy; Override;

//   Procedure Play(FrameName : string); ??
//   Procedure UpdatePlay;

   Property ModelRessource : TFMexModelRessource read FRessource Write SetRessource;
 End;



implementation

{ TFMeXModelRessource }

constructor TFMeXModelRessource.Create;
begin
  Inherited Create;
end;

destructor TFMeXModelRessource.Destroy;
begin
  if Assigned(FMaterial) then
  begin
    FreeAndNil(FMaterial);
  end;
  if Assigned(FData) then
  begin
    FreeAndNil(FData);
  end;
  inherited;
end;

function TFMeXModelRessource.GetFrameText: String;
var i : integer;
begin
  for i := Low(Frames) to High(Frames) do
  begin
    Result := Result + Frames[i].FrameName + #13#10;
  end;

end;

constructor TFMeXActor.Create(AOwner: TComponent);
var a : TMemoryStream;
begin
  Inherited Create(aOwner);
end;

destructor TFMeXActor.Destroy;
begin
  if Assigned(FRessource) then
  begin
    FreeAndNil(FRessource);
  end;
  inherited;
end;

procedure TFMeXActor.SetRessource(const Value: TFMexModelRessource);
begin
  FRessource := Value;
  if Assigned(FRessource) then
  begin
    Data.Assign(FRessource.FData);
    If Assigned(FRessource.FMaterial) then
      MaterialSource := FRessource.FMaterial;
  end
  else
  begin
    Data.Clear;
    MaterialSource := nil;
  end;
  Repaint;
end;


end.
