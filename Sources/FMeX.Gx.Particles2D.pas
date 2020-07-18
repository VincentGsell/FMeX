unit FMeX.Gx.Particles2D;

interface

Uses
  System.Types,
  System.Generics.Collections,
  System.Classes,
  FMX.Types3D,
  System.UITypes,
  System.SysUtils,
  FMX.Materials,
  FMX.MaterialSources ,
  FMX.Graphics,
  FMX.Controls3D,
  System.Math.Vectors,
  System.RTLConsts,
  GS.Engine.Particles2D,
  FMeX.Types3D,
  FMeX.Gx.Types,
  FMeX.Gx.Image;

Type


TeParticle2dType = (ptNone, ptStarfield, ptExplode, ptExplodeAdLib, ptEmiter);
TeParticle2d = Class(TeCtrlImageTextured)
protected
  FParticleEngine: TGSCustomParticleEngine2D;
  FParticleType: TeParticle2dType;
  FParticleCount: Cardinal;
  FParticleBecomeSmaller: Boolean;

  procedure SetParticleType(const Value: TeParticle2dType);
  procedure SetParticleCount(const Value: Cardinal);

  Procedure InternalBuild;

  Procedure MeshNotification(Sender : TObject); Override;
    function GetMaxDurationInMS: Cardinal;
    function GetMaxVelocity: Cardinal;
    function GetQuickSpeedMeansQuickDie: Boolean;
    procedure SetMaxDurationInMS(const Value: Cardinal);
    procedure SetMaxVelocity(const Value: Cardinal);
    procedure SetQuickSpeedMeansQuickDie(const Value: Boolean);
    function GetSpreadAngle: Integer;
    procedure SetSpreadAngle(const Value: Integer);
    function GetSpreadDirection: Single;
    procedure SetSpreadDirection(const Value: Single);

Public

  Procedure SetupRect(x,y,width,height : single);
  Procedure Reset;
  Procedure UpdateParticles;

  Procedure AddLocalForce(x,y : single);

  constructor Create; override;
  Destructor Destroy; Override;

Published
  Property ParticleType : TeParticle2dType read FParticleType Write SetParticleType;
  Property ParticleCount : Cardinal read FParticleCount Write SetParticleCount;

  Property BehaviourParticleBecomeSmallerDuringLife : Boolean read FParticleBecomeSmaller Write FParticleBecomeSmaller;

  //Apply if heritated from TofParticleEngine2D_Explode
  property BehaviourMaxVelocity : Cardinal read GetMaxVelocity Write SetMaxVelocity;
  property BehaviourMaxDurationInMS : Cardinal read GetMaxDurationInMS Write SetMaxDurationInMS;
  property BehaviourQuickSpeedMeansQuickDie : Boolean read GetQuickSpeedMeansQuickDie Write SetQuickSpeedMeansQuickDie;
  Property BehaviourSpreadAngle : Integer read GetSpreadAngle Write SetSpreadAngle;
  Property BehaviourSpreadDirection : Single read GetSpreadDirection Write SetSpreadDirection;

  Property Engine : TGSCustomParticleEngine2D read FParticleEngine;


End;



implementation


{ TeParticle2d }

procedure TeParticle2d.UpdateParticles;
var i : integer;
    VerticeIndice : Cardinal;
    p : PPoint3D;

    epsilon : single;
begin
  FParticleEngine.Update;

  VerticeIndice := 0;
  for i := 0 to FParticleEngine.ParticleCount-1 do
  begin
    p := FData.VertexBuffer.VerticesPtr[VerticeIndice];
    //Note : Above instead of "    //Data.VertexBuffer.Vertices[VerticeIndice] := ..." --> Not notable perf. difference.

    epsilon := 1;
    if FParticleBecomeSmaller then
      epsilon := (FParticleEngine.ParticleList[i].TimeRelevantInMilliSec * 100 / FParticleEngine.ParticleList[i].TimeToLiveInMilliSec) / 100;

    p^.X := FParticleEngine.ParticleList[i].x - 0.5 * epsilon;
    p^.Y := FParticleEngine.ParticleList[i].y - 0.5 * epsilon;
    p^.Z := 0;

    //inc(p,Data.VertexBuffer.VertexSize);
    //Data.VertexBuffer.Vertices[VerticeIndice] := Point3D(FParticleEngine.ParticleList[i].x - 0.5, FParticleEngine.ParticleList[i].y - 0.5, 0);
    p := FData.VertexBuffer.VerticesPtr[VerticeIndice+1];
    p^.X := FParticleEngine.ParticleList[i].x + 0.5 * epsilon;
    p^.Y := FParticleEngine.ParticleList[i].y - 0.5 * epsilon;
    p^.Z := 0;
    //Data.VertexBuffer.Vertices[VerticeIndice+1] := Point3D(FParticleEngine.ParticleList[i].x + 0.5, FParticleEngine.ParticleList[i].y - 0.5, 0);

    p := FData.VertexBuffer.VerticesPtr[VerticeIndice+2];
    p^.X := FParticleEngine.ParticleList[i].x + 0.5 * epsilon;
    p^.Y := FParticleEngine.ParticleList[i].y + 0.5 * epsilon;
    p^.Z := 0;
    //Data.VertexBuffer.Vertices[VerticeIndice+2] := Point3D(FParticleEngine.ParticleList[i].x + 0.5, FParticleEngine.ParticleList[i].y + 0.5, 0);

    p := FData.VertexBuffer.VerticesPtr[VerticeIndice+3];
    p^.X := FParticleEngine.ParticleList[i].x - 0.5 * epsilon;
    p^.Y := FParticleEngine.ParticleList[i].y + 0.5 * epsilon;
    p^.Z := 0;
    //Data.VertexBuffer.Vertices[VerticeIndice+3] := Point3D(FParticleEngine.ParticleList[i].x - 0.5, FParticleEngine.ParticleList[i].y + 0.5, 0);

    VerticeIndice := VerticeIndice + 4;
  end;
end;

procedure TeParticle2d.AddLocalForce(x, y: single);
begin
  FParticleEngine.ForceManagerEnabled := true;
  FParticleEngine.Object_ForceList.AddForce(x,y);
end;

constructor TeParticle2d.Create;
begin
  inherited;
  FParticleEngine := Nil;
  FParticleCount := 100;
  ParticleType := ptStarfield;
  FParticleBecomeSmaller := False;
end;

destructor TeParticle2d.Destroy;
begin
  FreeAndNil(FParticleEngine);
  inherited;
end;

function TeParticle2d.GetMaxDurationInMS: Cardinal;
begin
  Result := 0;
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    result := TGSParticleEngine2D_Explode(FParticleEngine).BehaviourMaxDurationInMS;
  end;
end;

function TeParticle2d.GetMaxVelocity: Cardinal;
begin
  Result := 0;
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    result := TGSParticleEngine2D_Explode(FParticleEngine).BehaviourMaxVelocity;
  end;
end;

function TeParticle2d.GetQuickSpeedMeansQuickDie: Boolean;
begin
  Result := False;
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    result := TGSParticleEngine2D_Explode(FParticleEngine).BehaviourQuickSpeedMeansQuickDie;
  end;
end;

function TeParticle2d.GetSpreadAngle: Integer;
begin
  Result := 0;
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    Result := TGSParticleEngine2D_Explode(FParticleEngine).AngleDistribution;
  end;
end;

function TeParticle2d.GetSpreadDirection: Single;
begin
  Result := 0.0;
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    Result := TGSParticleEngine2D_Explode(FParticleEngine).AngleDistributionDirection;
  end;
end;

procedure TeParticle2d.InternalBuild;
var
  ref0 : TPoint3d;
  ref1 : TPoint3d;
  ref2 : TPoint3d;
  ref3 : TPoint3d;

  tex0 : TPointF;
  tex1 : TPointF;
  tex2 : TPointF;
  tex3 : TPointF;

  idx0 : Integer;
  idx1 : Integer;
  idx2 : Integer;
  idx3 : Integer;
  idx4 : Integer;
  idx5 : Integer;

  i : integer;
  fid : integer;
  fidprime : integer;
begin
  if Assigned(FParticleEngine) then
  begin
    FreeAndNil(FParticleEngine);
  end;

  case FParticleType of
    ptStarfield :
    begin
      FParticleEngine := TGSParticleEngine2D_StarField.Create(FParticleCount);
    end;
    ptExplode :
    begin
      FParticleEngine := TGSParticleEngine2D_Explode.Create(FParticleCount);
    end;
    ptExplodeAdLib :
    begin
      FParticleEngine := TGSParticleEngine2D_ExplodeAdLib.Create(FParticleCount);
    end;
    ptEmiter :
    begin
      FParticleEngine := TGSParticleEngine2D_Emiter.Create(FParticleCount);
    end;
  end;

  FData.VertexBuffer.Length := FParticleCount * 4;
  FData.IndexBuffer.Length  := FParticleCount * 6;

  ref0 := Point3D(-0.5 * Width, -0.5 * Height, 0);
  ref1 := Point3D(0.5 * Width, -0.5 * Height, 0);
  ref2 := Point3D(0.5 * Width, 0.5 * Height, 0);
  ref3 := Point3D(-0.5 * Width, 0.5, 0);

  Tex0 := PointF(0, 0);
  Tex1 := PointF(1, 0);
  Tex2 := PointF(1, 1);
  Tex3 := PointF(0, 1);

  Idx0 := 0;
  Idx1 := 1;
  Idx2 := 2;
  Idx3 := 2;
  Idx4 := 3;
  Idx5 := 0;

  //Texture setup.
  fid := 0;
  for I := 0 to FParticleCount-1 do
  begin
    FData.VertexBuffer.TexCoord0[fid] := Tex0;
    FData.VertexBuffer.TexCoord0[fid+1] := Tex1;
    FData.VertexBuffer.TexCoord0[fid+2] := Tex2;
    FData.VertexBuffer.TexCoord0[fid+3] := Tex3;
    fid := fid + 4;
  end;

  //Index Setup (Quad)
  fid := 0;
  fidPrime := 0;
  for I := 0 to FParticleCount-1 do
  begin
    FData.IndexBuffer[fid] := fidPrime + idx0;
    FData.IndexBuffer[fid+1] := fidPrime + idx1;
    FData.IndexBuffer[fid+2] := fidPrime + idx2;
    FData.IndexBuffer[fid+3] := fidPrime + idx3;
    FData.IndexBuffer[fid+4] := fidPrime + idx4;
    FData.IndexBuffer[fid+5] := fidPrime + idx5;

    fid := fid + 6;
    fidPrime := fidPrime + 4;
  end;

  FParticleEngine.initialize;

end;

procedure TeParticle2d.MeshNotification(Sender: TObject);
var fid,i,ii : Integer;
begin
  inherited;
  if Sender = SourceConfiguration then
  begin
    MaterialSource := SourceConfiguration.TextureData.MaterialSource;
    //only texture is touched.
    fid := 0;
    ii := SourceConfiguration.TextureItemIndex;
    for I := 0 to FParticleCount-1 do
    begin

      FData.VertexBuffer.TexCoord0[fid] := Pointf( SourceConfiguration.TextureData.TextureAtlasItems[ii].TextureSourceX1,
                                                  SourceConfiguration.TextureData.TextureAtlasItems[ii].TextureSourceY1);
      FData.VertexBuffer.TexCoord0[fid+1] := Pointf( SourceConfiguration.TextureData.TextureAtlasItems[ii].TextureSourceX2,
                                                    SourceConfiguration.TextureData.TextureAtlasItems[ii].TextureSourceY1);
      FData.VertexBuffer.TexCoord0[fid+2] := Pointf( SourceConfiguration.TextureData.TextureAtlasItems[ii].TextureSourceX2,
                                                    SourceConfiguration.TextureData.TextureAtlasItems[ii].TextureSourceY2);
      FData.VertexBuffer.TexCoord0[fid+3] := Pointf( SourceConfiguration.TextureData.TextureAtlasItems[ii].TextureSourceX1,
                                                    SourceConfiguration.TextureData.TextureAtlasItems[ii].TextureSourceY2);
      fid := fid + 4;
    end;
    //InternalBuild;
  end;
end;

procedure TeParticle2d.Reset;
begin
  FParticleEngine.initialize;
end;

procedure TeParticle2d.SetMaxDurationInMS(const Value: Cardinal);
begin
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    TGSParticleEngine2D_Explode(FParticleEngine).BehaviourMaxDurationInMS := Value;
  end;
end;

procedure TeParticle2d.SetMaxVelocity(const Value: Cardinal);
begin
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    TGSParticleEngine2D_Explode(FParticleEngine).BehaviourMaxVelocity := Value;
  end;
end;

procedure TeParticle2d.SetParticleCount(const Value: Cardinal);
begin
  if FParticleCount<>Value then
  begin
    FParticleCount := Value;
    InternalBuild;
  end;
end;

procedure TeParticle2d.SetParticleType(const Value: TeParticle2dType);
begin
  if Value<>FParticleType then
  begin
    FParticleType := Value;
    InternalBuild;
  end;
end;

procedure TeParticle2d.SetQuickSpeedMeansQuickDie(const Value: Boolean);
begin
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    TGSParticleEngine2D_Explode(FParticleEngine).BehaviourQuickSpeedMeansQuickDie := Value;
  end;
end;

procedure TeParticle2d.SetSpreadAngle(const Value: Integer);
begin
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    TGSParticleEngine2D_Explode(FParticleEngine).AngleDistribution := Value;
  end;
end;

procedure TeParticle2d.SetSpreadDirection(const Value: Single);
begin
  if FParticleEngine is TGSParticleEngine2D_Explode then
  begin
    TGSParticleEngine2D_Explode(FParticleEngine).AngleDistributionDirection := Value*Pi/180;
  end;
end;

procedure TeParticle2d.SetupRect(x, y, width, height : Single);
begin
  FParticleEngine.Position.X := x;
  FParticleEngine.Position.Y := x;
  FParticleEngine.Offset.X := Width/2;
  FParticleEngine.Offset.Y := Height/2;
  FParticleEngine.initialize;
end;

end.
