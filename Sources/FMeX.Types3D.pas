unit FMeX.Types3D;

interface

Uses
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  System.Math.Vectors,
  FMX.Types3D,
  FMX.Objects3D,
  FMX.Materials,
  FMX.Graphics,
  FMX.Controls3D,
  FMX.MaterialSources;

Type
TeCustomMeshDrawType = (mdtNone, mdtPointFrame, mdtWireFrame, mdtFull);
TeCustomMesh = Class(TCustomMesh)
private
Protected
  FCurrentBoundingBox : TBoundingBox;
  FDrawOverlap: Boolean;
  FDrawFrameType: TeCustomMeshDrawType;

  FPreAllocatedVertexIndex : Uint32;
  FPreAllocatedIndexIndex : Uint32;

  procedure SetDrawFrameType(const Value: TeCustomMeshDrawType);
  procedure SetDrawOverlap(const Value: Boolean);
  Procedure DrawOverlap;

  //Procedure InternalMergeFrom To do.
Public
  Property Data; //Put in public -> Important : For exploitation of data from outside !

  Procedure MergeFrom(aTeCustomMesh : TeCustomMesh; Const PreallocateMeshSize : Boolean = false; const ReCalcBoundingBox : boolean = true); Overload; //Mesh + relative TeCustomMesh position
  Procedure MergeFrom(aMesh : TMeshData; Const PreallocateMeshSize : Boolean = false; const ReCalcBoundingBox : boolean = true); Overload; //Only Mesh coord (no relative TeCustomMesh position)

  constructor Create(AOwner: TComponent); override;

  Procedure Render; Overload; Override;

  //Some component, such as texture configuration, will trig above method, if this object is registered in their own list.
  Procedure MeshNotification(Sender : TObject); Virtual; Abstract;

  Procedure ProcessBoundingBox;
Published
  Property DrawFrameType : TeCustomMeshDrawType read FDrawFrameType Write SetDrawFrameType;
  Property DrawOverlapShape : Boolean read FDrawOverlap Write SetDrawOverlap;
End;


TeCube = class(TeCustomMesh)
  private
    FSubdivisionsWidth: Integer;
    FSubdivisionsDepth: Integer;
    FSubdivisionsHeight: Integer;
    procedure SetSubdivisionsDepth(const Value: Integer);
    procedure SetSubdivisionsHeight(const Value: Integer);
    procedure SetSubdivisionsWidth(const Value: Integer);
    procedure SetSideLength(const Value: Integer);
    procedure SetSideSubdivision(const Value: Integer);
  protected
    procedure RebuildMesh;
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320) or Defined(VER330) or Defined(VER340)}
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  {$ELSE}
    function DoRayCastIntersect(const RayPos, RayDir: TVector3D; var Intersection: TVector3D): Boolean; override;
  {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;

    Property SideSubdivisions : Integer Write SetSideSubdivision;
    Property SideLength : Integer Write SetSideLength;
  published
    property SubdivisionsDepth: Integer read FSubdivisionsDepth write SetSubdivisionsDepth default 1;
    property SubdivisionsHeight: Integer read FSubdivisionsHeight write SetSubdivisionsHeight default 1;
    property SubdivisionsWidth: Integer read FSubdivisionsWidth write SetSubdivisionsWidth default 1;
  end;

TeSphere = Class(TeCustomMesh)
  private
    FSubdivisionsAxes: Integer;
    FSubdivisionsHeight: Integer;
    procedure SetSubdivisionsAxes(const Value: Integer);
    procedure SetSubdivisionsHeight(const Value: Integer);
    procedure SetRadius(const Value: Single);
  protected
    procedure RebuildMesh;
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320) or Defined(VER330) or Defined(VER340)}
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  {$ELSE}
    function DoRayCastIntersect(const RayPos, RayDir: TVector3D; var Intersection: TVector3D): Boolean; override;
  {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
  published
    property SubdivisionsAxes: Integer read FSubdivisionsAxes write SetSubdivisionsAxes default 16;
    property SubdivisionsHeight: Integer read FSubdivisionsHeight write SetSubdivisionsHeight default 12;
    Property Radius : Single Write SetRadius;
End;


procedure rtAssert(condition : boolean; const errorDesc : String);

implementation

procedure rtAssert(condition : boolean; const errorDesc : String);
begin
  if not condition then
    raise Exception.Create(errordesc);
end;

{ TeCube2 }

constructor TeCube.Create(AOwner: TComponent);
begin
  inherited;
  SideSubdivisions := 1;
end;

procedure TeCube.RebuildMesh;
var
  X, Y: Integer;
  Face: Integer;
  FaceVertexLength: Integer;
  FaceIndexLength: Integer;
  VertexOffset: Integer;
  IndexOffset: Integer;
begin
  Data.VertexBuffer.Length :=
    (FSubdivisionsWidth + 1) * (FSubdivisionsHeight + 1) * 2 +
    (FSubdivisionsDepth + 1) * (FSubdivisionsWidth + 1) * 2 +
    (FSubdivisionsDepth + 1) * (FSubdivisionsHeight + 1) * 2;
  Data.IndexBuffer.Length :=
    (FSubdivisionsWidth) * (FSubdivisionsHeight) * 6 * 2 +
    (FSubdivisionsDepth) * (FSubdivisionsWidth) * 6 * 2 +
    (FSubdivisionsDepth) * (FSubdivisionsHeight) * 6 * 2;

  VertexOffset := 0;
  IndexOffset := 0;
  FaceVertexLength := (FSubdivisionsWidth + 1) * (FSubdivisionsHeight + 1);
  FaceIndexLength := FSubdivisionsWidth * FSubdivisionsHeight * 6;
  for Face := 0 to 1 do
  begin
    for Y := 0 to FSubdivisionsHeight do
      for X := 0 to FSubdivisionsWidth do
      begin
        if not Odd(Face) then
        begin
          Data.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), -0.5 + (Y / FSubdivisionsHeight), -0.5);
          Data.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, 0, -1);
          Data.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := PointF(X / FSubdivisionsWidth, Y / FSubdivisionsHeight);
        end
        else
        begin
          Data.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), -0.5 + (Y / FSubdivisionsHeight), 0.5);
          Data.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, 0, 1);
          Data.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := PointF(1 - X / FSubdivisionsWidth, Y / FSubdivisionsHeight);
        end;
      end;
    for Y := 0 to FSubdivisionsHeight - 1 do
      for X := 0 to FSubdivisionsWidth - 1 do
      begin
        if Odd(Face) then
        begin
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
        end
        else
        begin
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
        end;
      end;
  end;
  VertexOffset := VertexOffset + FaceVertexLength * 2;
  IndexOffset := IndexOffset + FaceIndexLength * 2;
  FaceVertexLength := (FSubdivisionsDepth + 1) * (FSubdivisionsWidth + 1);
  FaceIndexLength := FSubdivisionsDepth * FSubdivisionsWidth * 6;
  for Face := 0 to 1 do
  begin
    for Y := 0 to FSubdivisionsDepth do
      for X := 0 to FSubdivisionsWidth do
      begin
        if Odd(Face) then
        begin
          Data.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), -0.5, -0.5 + (Y / FSubdivisionsDepth));
          Data.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, -1, 0);
          Data.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := PointF(X / FSubdivisionsWidth, 1 - Y / FSubdivisionsDepth);
        end
        else
        begin
          Data.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(-0.5 + (X / FSubdivisionsWidth), 0.5, -0.5 + (Y / FSubdivisionsDepth));
          Data.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := Point3D(0, 1, 0);
          Data.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1))] := PointF(X / FSubdivisionsWidth, Y / FSubdivisionsDepth);
        end;
      end;
    for Y := 0 to FSubdivisionsDepth - 1 do
      for X := 0 to FSubdivisionsWidth - 1 do
      begin
        if Odd(Face) then
        begin
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
        end
        else
        begin
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsWidth + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsWidth + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsWidth + 1));
        end;
      end;
  end;
  VertexOffset := VertexOffset + FaceVertexLength * 2;
  IndexOffset := IndexOffset + FaceIndexLength * 2;
  FaceVertexLength := (FSubdivisionsDepth + 1) * (FSubdivisionsHeight + 1);
  FaceIndexLength := FSubdivisionsDepth * FSubdivisionsHeight * 6;
  for Face := 0 to 1 do
  begin
    for Y := 0 to FSubdivisionsDepth do
      for X := 0 to FSubdivisionsHeight do
      begin
        if Odd(Face) then
        begin
          Data.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := Point3D(-0.5, -0.5 + (X / FSubdivisionsHeight), -0.5 + (Y / FSubdivisionsDepth));
          Data.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := Point3D(-1, 0, 0);
          Data.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := PointF(1 - Y / FSubdivisionsDepth, X / FSubdivisionsHeight);
        end
        else
        begin
          Data.VertexBuffer.Vertices[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := Point3D(0.5, -0.5 + (X / FSubdivisionsHeight), -0.5 + (Y / FSubdivisionsDepth));
          Data.VertexBuffer.Normals[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := Point3D(1, 0, 0);
          Data.VertexBuffer.TexCoord0[VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1))] := PointF(Y / FSubdivisionsDepth, X / FSubdivisionsHeight);
        end;
      end;
    for Y := 0 to FSubdivisionsDepth - 1 do
      for X := 0 to FSubdivisionsHeight - 1 do
      begin
        if not Odd(Face) then
        begin
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsHeight + 1));
        end
        else
        begin
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 0] := VertexOffset + (Face * FaceVertexLength) + X + (Y * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 1] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 2] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 3] := VertexOffset + (Face * FaceVertexLength) + X + ((Y + 1) * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 4] := VertexOffset + (Face * FaceVertexLength) + X + 1 + (Y * (FSubdivisionsHeight + 1));
          Data.IndexBuffer[IndexOffset + (Face * FaceIndexLength) + (X + (Y * (FSubdivisionsHeight + 0))) * 6 + 5] := VertexOffset + (Face * FaceVertexLength) + X + 1 + ((Y + 1) * (FSubdivisionsHeight + 1));
        end;
      end;
  end;
  ProcessBoundingBox;
end;

{$IF Defined(VER280) OR Defined(VER310) or Defined(VER320) or Defined(VER330) or Defined(VER340)}
function TeCube.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
{$ELSE}
function TeCube.DoRayCastIntersect(const RayPos, RayDir: TVector3D; var Intersection: TVector3D): Boolean;
{$ENDIF}
var
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320) or Defined(VER330) or Defined(VER340)}
  INear, IFar: TPoint3D;
{$ELSE}
  INear, IFar: TVector3D;
{$ENDIF}
begin
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320) or Defined(VER330) or Defined(VER340)}
  Result := RayCastCuboidIntersect(RayPos, RayDir, Point3D(0,0,0), Width, Height, Depth, INear, IFar) > 0;
{$ELSE}
  Result := RayCastCuboidIntersect(RayPos, RayDir, Vector3D(0,0,0), Width, Height, Depth, INear, IFar) > 0;
{$ENDIF}
  if Result then
    Intersection := LocalToAbsoluteVector(INear);
end;

{
procedure TeCube.DrawOverlap;
var sContext : TContext3D;
begin
  sContext := Context;
  sContext.DrawCube(NullVector3D, Vector3D(Width/2, Height/2, Depth/2), AbsoluteOpacity, TAlphaColorRec.Blue);
end;
}
procedure TeCube.SetSideLength(const Value: Integer);
begin
  BeginUpdate;
  try
    Width := value;
    Height := Value;
  finally
    EndUpdate;
  end;
  Depth := Value;
end;

procedure TeCube.SetSideSubdivision(const Value: Integer);
var fValue : Integer;
begin
  fValue := value;
  if fvalue<1 then
    fValue :=  1;
  FSubdivisionsDepth := fValue;
  FSubdivisionsHeight := fValue;
  FSubdivisionsWidth := fValue;
  RebuildMesh;
end;

procedure TeCube.SetSubdivisionsDepth(const Value: Integer);
begin
  if FSubdivisionsDepth <> Value then
  begin
    FSubdivisionsDepth := Value;
    if FSubdivisionsDepth < 1 then
      FSubdivisionsDepth := 1;
    RebuildMesh;
  end;
end;

procedure TeCube.SetSubdivisionsHeight(const Value: Integer);
begin
  if FSubdivisionsHeight <> Value then
  begin
    FSubdivisionsHeight := Value;
    if FSubdivisionsHeight < 1 then
      FSubdivisionsHeight := 1;
    RebuildMesh;
  end;
end;

procedure TeCube.SetSubdivisionsWidth(const Value: Integer);
begin
  if FSubdivisionsWidth <> Value then
  begin
    FSubdivisionsWidth := Value;
    if FSubdivisionsWidth < 1 then
      FSubdivisionsWidth := 1;
    RebuildMesh;
  end;
end;

{ TeCustomMesh }

constructor TeCustomMesh.Create(AOwner: TComponent);
begin
  inherited;
  Data.OnChanged := Nil;
  Data.Clear;
  WrapMode := TMeshWrapMode.Resize;
  FDrawFrameType := mdtFull;
  FDrawOverlap := True;
  ZWrite := true;
  TwoSide := True;
  HitTest := False;
  FPreAllocatedVertexIndex := 0;
  FPreAllocatedIndexIndex := 0;
end;

procedure TeCustomMesh.DrawOverlap;
begin
  Context.DrawCube( NullVector3D,
            Vector3D( FCurrentBoundingBox.Width,
                      FCurrentBoundingBox.Height,
                      FCurrentBoundingBox.Depth) / 2,
            AbsoluteOpacity,
            TAlphaColorRec.Blue);
end;

procedure TeCustomMesh.MergeFrom(aMesh: TMeshData; Const PreallocateMeshSize : Boolean; const ReCalcBoundingBox : boolean);
var
  I: Integer;
  iold, vold : Integer;
  iv : Integer;
begin
  if PreallocateMeshSize then
  begin
    vold := FPreAllocatedVertexIndex;
    iv := 0;

    for I := vold to vold + aMesh.VertexBuffer.Length - 1 do
    begin
      Data.VertexBuffer.Vertices[I] := Point3d( aMesh.VertexBuffer.Vertices[iv].X,
                                                aMesh.VertexBuffer.Vertices[iv].Y,
                                                aMesh.VertexBuffer.Vertices[iv].Z);
      Data.VertexBuffer.Normals[I] := Point3d( aMesh.VertexBuffer.Normals[iv].X,
                                               aMesh.VertexBuffer.Normals[iv].Y,
                                               aMesh.VertexBuffer.Normals[iv].Z);
      Data.VertexBuffer.TexCoord0[I] := aMesh.VertexBuffer.TexCoord0[iv];
      iv := iv + 1;
    end;
    FPreAllocatedVertexIndex := FPreAllocatedVertexIndex + iv;

    iold := FPreAllocatedIndexIndex;
    for I := iold to iold + aMesh.IndexBuffer.Length - 1 do
    begin
      Data.IndexBuffer[I] := vold + aMesh.IndexBuffer[i-iold];
    end;
    FPreAllocatedIndexIndex := FPreAllocatedIndexIndex + aMesh.IndexBuffer.Length;
  end
  else
  begin
    vold := Data.VertexBuffer.Length;
    iv := 0;

    Data.VertexBuffer.Length := Data.VertexBuffer.Length + aMesh.VertexBuffer.Length;

    for I := vold to Data.VertexBuffer.Length - 1 do
    begin
      Data.VertexBuffer.Vertices[I] := Point3d( aMesh.VertexBuffer.Vertices[iv].X,
                                                aMesh.VertexBuffer.Vertices[iv].Y,
                                                aMesh.VertexBuffer.Vertices[iv].Z);
      Data.VertexBuffer.Normals[I] := Point3d( aMesh.VertexBuffer.Normals[iv].X,
                                               aMesh.VertexBuffer.Normals[iv].Y,
                                               aMesh.VertexBuffer.Normals[iv].Z);
      Data.VertexBuffer.TexCoord0[I] := aMesh.VertexBuffer.TexCoord0[iv];
      iv := iv + 1;
    end;

    iold := Data.IndexBuffer.Length;
    iv := 0;
    Data.IndexBuffer.Length := Data.IndexBuffer.Length + aMesh.IndexBuffer.Length;
    for I := iold to Data.IndexBuffer.Length - 1 do
    begin
      Data.IndexBuffer[I] := vold + aMesh.IndexBuffer[iv];
      iv := iv+1;
    end;
  end;

  if ReCalcBoundingBox then
    ProcessBoundingBox;
end;

procedure TeCustomMesh.MergeFrom(aTeCustomMesh: TeCustomMesh; Const PreallocateMeshSize : Boolean; const ReCalcBoundingBox : boolean);
var
  I: Integer;
  vold : Integer;
  iold : Integer;
begin
  if PreallocateMeshSize then
  begin
    vold := FPreAllocatedVertexIndex;
  end
  else
  begin
    vold := Data.VertexBuffer.Length;
    Data.VertexBuffer.Length := Data.VertexBuffer.Length + aTeCustomMesh.Data.VertexBuffer.Length;
  end;

  for I := vold to vold + aTeCustomMesh.Data.VertexBuffer.Length - 1 do
  begin
    Data.VertexBuffer.Vertices[I] := Point3d( aTeCustomMesh.Data.VertexBuffer.Vertices[I-vold].X + aTeCustomMesh.Position.X,
                                              aTeCustomMesh.Data.VertexBuffer.Vertices[I-vold].Y + aTeCustomMesh.Position.Y,
                                              aTeCustomMesh.Data.VertexBuffer.Vertices[I-vold].Z + aTeCustomMesh.Position.Z);
    Data.VertexBuffer.Normals[I] := Point3d( aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].X  + aTeCustomMesh.Position.X,
                                             aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].Y  + aTeCustomMesh.Position.Y,
                                             aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].Z  + aTeCustomMesh.Position.Z);
    Data.VertexBuffer.Normals[I] := Point3d( aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].X,
                                             aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].Y,
                                             aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].Z);
    Data.VertexBuffer.TexCoord0[I] := aTeCustomMesh.Data.VertexBuffer.TexCoord0[I-vold];
  end;

  if PreallocateMeshSize then
  begin
    FPreAllocatedVertexIndex := FPreAllocatedVertexIndex + aTeCustomMesh.Data.VertexBuffer.Length;
    iold := FPreAllocatedIndexIndex;
    for I := iold to iold + aTeCustomMesh.Data.IndexBuffer.Length - 1 do
      Data.IndexBuffer[I] := vold + aTeCustomMesh.Data.IndexBuffer[i-iold];
    FPreAllocatedIndexIndex := FPreAllocatedIndexIndex + aTeCustomMesh.Data.IndexBuffer.Length;
  end
  else
  begin
    iold := Data.IndexBuffer.Length;
    Data.IndexBuffer.Length := Data.IndexBuffer.Length + aTeCustomMesh.Data.IndexBuffer.Length;
    for I := iold to Data.IndexBuffer.Length - 1 do
      Data.IndexBuffer[I] := vold + aTeCustomMesh.Data.IndexBuffer[I-iold];
  end;

  //Update Bounding box.
  if ReCalcBoundingBox then
    ProcessBoundingBox;
end;


procedure TeCustomMesh.ProcessBoundingBox;
begin
  Data.BoundingBoxNeedsUpdate;
  FCurrentBoundingBox := Data.GetBoundingBox;
  FWidth := FCurrentBoundingBox.Width;
  FHeight := FCurrentBoundingBox.Height;
  FDepth := FCurrentBoundingBox.Depth;
end;

procedure TeCustomMesh.Render;
begin
  Context.SetMatrix(GetMeshMatrix*AbsoluteMatrix);


  case FDrawFrameType of
    mdtFull:
    begin
      Context.DrawTriangles(Data.VertexBuffer, Data.IndexBuffer, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity);
    End;
    mdtPointFrame:
    begin
      Context.DrawPoints(Data.VertexBuffer, Data.IndexBuffer, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity);
    end;
    mdtWireFrame:
    begin
      Context.DrawLines(Data.VertexBuffer, Data.IndexBuffer, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity);
    end;
    mdtNone: ;
  end;

  if FDrawOverlap then
  begin
    DrawOverlap;
  end;
end;

procedure TeCustomMesh.SetDrawFrameType(const Value: TeCustomMeshDrawType);
begin
  FDrawFrameType := Value;
  Repaint;
end;

procedure TeCustomMesh.SetDrawOverlap(const Value: Boolean);
begin
  FDrawOverlap := Value;
  FCurrentBoundingBox := Data.GetBoundingBox;
  Repaint;
end;


{ TeSphere }

constructor TeSphere.Create(AOwner: TComponent);
begin
  inherited;
  FSubdivisionsAxes := 16;
  FSubdivisionsHeight := 12;
  RebuildMesh;
end;

{$IF Defined(VER280) OR Defined(VER310) or Defined(VER320) or Defined(VER330) or Defined(VER340)}
function TeSphere.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3d): Boolean;
{$ELSE}
function TeSphere.DoRayCastIntersect(const RayPos, RayDir: TVector3D; var Intersection: TPoint3d): Boolean;
{$ENDIF}
var
{$IF Defined(VER280) OR Defined(VER310) or Defined(VER320) or Defined(VER330) or Defined(VER340)}
  INear, IFar: TPoint3D;
{$ELSE}
  INear, IFar: TVector3D;
{$ENDIF}
begin
{$IF Defined(VER280) OR Defined(VER310) or Defined(VER320) or Defined(VER330) or Defined(VER340)}
  Result := RayCastEllipsoidIntersect(RayPos, RayDir, Point3D(0,0,0), Width/2, Height/2, Depth/2, INear, IFar) > 0;
{$ELSE}
  Result := RayCastEllipsoidIntersect(RayPos, RayDir, Vector3D(0,0,0), Width/2, Height/2, Depth/2, INear, IFar) > 0;
{$ENDIF}
  if Result then
    Intersection := LocalToAbsoluteVector(INear);
end;

{
procedure TeSphere.DrawOverlap;
var sContext : TContext3D;
begin
  sContext := Context;
  sContext.DrawCube(NullVector3D, Vector3D(Width/10, Height/10, Depth/10), AbsoluteOpacity, TAlphaColorRec.Blue);
end;
}


procedure TeSphere.RebuildMesh;
var
  A, H, AA, HH: Integer;
  Theta, Phi: Single;
  DTheta, DPhi: Single;
  ThetaSin, ThetaCos: Extended;
  PhiSin, PhiCos: Extended;
  IdxCount: Integer;
  VerticesWidth: Integer;
begin
  VerticesWidth := (FSubdivisionsAxes + 1);
  Data.VertexBuffer.Length := (FSubdivisionsHeight + 1) * VerticesWidth - 1;
  Data.IndexBuffer.Length := (FSubdivisionsHeight - 2) * FSubdivisionsAxes * 6 + (FSubdivisionsAxes * 3) + (FSubdivisionsAxes * 3);
  DTheta := DegToRad(180) / FSubdivisionsHeight;
  DPhi := DegToRad(360) / FSubdivisionsAxes;
  IdxCount := 0;
  // fill indices
  Theta := -DegToRad(90);
  for H := 0 to FSubdivisionsHeight - 1 do
  begin
    Phi := 0;
    for A := 0 to FSubdivisionsAxes - 1 do
    begin
      SinCos(Theta, ThetaSin, ThetaCos);
      SinCos(Phi, PhiSin, PhiCos);
      Data.VertexBuffer.Vertices[A + (H * VerticesWidth)] := Point3D(ThetaCos * PhiCos * 0.5, ThetaSin * 0.5, ThetaCos * PhiSin * 0.5);
      Data.VertexBuffer.TexCoord0[A + (H * VerticesWidth)] := PointF(A / FSubdivisionsAxes, H / FSubdivisionsHeight);
      Data.VertexBuffer.Normals[A + (H * VerticesWidth)] := Point3D(ThetaCos * PhiCos, ThetaSin, ThetaCos * PhiSin).Normalize;
      if A = 0 then
      begin
        Data.VertexBuffer.Vertices[FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(ThetaCos * PhiCos * 0.5, ThetaSin * 0.5, ThetaCos * PhiSin * 0.5);
        Data.VertexBuffer.TexCoord0[FSubdivisionsAxes + (H * VerticesWidth)] := PointF(1, H / FSubdivisionsHeight);
        Data.VertexBuffer.Normals[FSubdivisionsAxes + (H * VerticesWidth)] := Point3D(ThetaCos * PhiCos, ThetaSin, ThetaCos * PhiSin).Normalize;
      end;
      AA := A + 1;
      HH := H + 1;
      if H = 0 then
      begin
        Data.VertexBuffer.TexCoord0[A + (H * VerticesWidth)] := PointF((A + 0.5) / FSubdivisionsAxes, 0);
        Data.IndexBuffer.Indices[IdxCount + 0] := A;
        Data.IndexBuffer.Indices[IdxCount + 1] := AA + (HH * VerticesWidth);
        Data.IndexBuffer.Indices[IdxCount + 2] := A + (HH * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      if H = FSubdivisionsHeight - 1 then
      begin
        Data.VertexBuffer.Vertices[A + (FSubdivisionsHeight * VerticesWidth)] := Point3D(0, 0.5, 0);
        Data.VertexBuffer.TexCoord0[A + (FSubdivisionsHeight * VerticesWidth)] := PointF((A + 0.5) / FSubdivisionsAxes, 1);
        Data.VertexBuffer.Normals[A + (FSubdivisionsHeight * VerticesWidth)] := Point3D(0, 1.0, 0);

        Data.IndexBuffer.Indices[IdxCount + 0] := A + (H * VerticesWidth);
        Data.IndexBuffer.Indices[IdxCount + 1] := AA + (H * VerticesWidth);
        Data.IndexBuffer.Indices[IdxCount + 2] := A + (HH * VerticesWidth);
        IdxCount := IdxCount + 3;
      end
      else
      begin
        Data.IndexBuffer.Indices[IdxCount + 0] := A + (H * VerticesWidth);
        Data.IndexBuffer.Indices[IdxCount + 1] := AA + (HH * VerticesWidth);
        Data.IndexBuffer.Indices[IdxCount + 2] := A + (HH * VerticesWidth);
        Data.IndexBuffer.Indices[IdxCount + 3] := A + (H * VerticesWidth);
        Data.IndexBuffer.Indices[IdxCount + 4] := AA + (H * VerticesWidth);
        Data.IndexBuffer.Indices[IdxCount + 5] := AA + (HH * VerticesWidth);
        IdxCount := IdxCount + 6;
      end;
      Phi := Phi + DPhi;
    end;
    Theta := Theta + DTheta;
  end;
  ProcessBoundingBox;
end;

procedure TeSphere.SetRadius(const Value: Single);
begin
  BeginUpdate;
  try
    Width := Value;
    Height := Value;
  finally
    EndUpdate;
  end;
  Depth := Value;
end;

procedure TeSphere.SetSubdivisionsAxes(const Value: Integer);
begin
  if FSubdivisionsAxes <> Value then
  begin
    FSubdivisionsAxes := Value;
    if FSubdivisionsAxes < 3 then
      FSubdivisionsAxes := 3;
    if FSubdivisionsAxes > 50 then
      FSubdivisionsAxes := 50;
    RebuildMesh;
  end;
end;

procedure TeSphere.SetSubdivisionsHeight(const Value: Integer);
begin
  if FSubdivisionsHeight <> Value then
  begin
    FSubdivisionsHeight := Value;
    if FSubdivisionsHeight < 2 then
      FSubdivisionsHeight := 2;
    if FSubdivisionsHeight > 50 then
      FSubdivisionsHeight := 50;
    RebuildMesh;
  end;
end;


end.
