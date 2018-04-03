unit FMeX.Types3D;

interface

Uses
  FMX.Objects3D, System.Types,
  System.Generics.Collections, Classes, FMX.Types3D,
  System.UITypes, System.SysUtils,
  FMX.Materials
  {$IFDEF VER260}
  ,FMX.Graphics, FMX.Controls3D,
  FMX.Viewport3D, FMX.MaterialSources
  {$ENDIF}
  {$IF Defined(VER280) or Defined(VER310) or Defined(VER320)}
  ,FMX.Graphics, FMX.Controls3D,
  FMX.Viewport3D, FMX.MaterialSources, System.Math.Vectors
  {$ENDIF}
  ;

Type


TFMeXCustomGraph = Class;
TFMeXGraph = Class;      //This Graph use fmx tree (ie. parent) as renderer, but allow many proxy : Use it for multiview (one graph, one proxy by view).
TFMeXGraphFor2D = Class; //This a specialized Graph for 2d : Managing object in a list and rendering them sequentially.
//TFMeXGraphForFPS...      ;)
//TFMeXGraphForAction3D... ;))

TFMeXProxy = Class(TControl3D)
Private
  FGraph: TFMeXCustomGraph;
  procedure SetGraph(const Value: TFMeXCustomGraph);
Public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  Procedure Analyze;
  Procedure Render; Override;

  Procedure Add(aControl3D : TControl3D);

  Procedure GlobalSetUpContext;

  Property Graph : TFMeXCustomGraph read FGraph Write SetGraph;
End;

TFMeXCustomGraph = Class(TComponent)
Public
  ObjectList : TList<TControl3D>;
  Proxy : TFMeXProxy;

  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  Procedure ProcessGraph; Virtual; Abstract;
  Procedure Add(aControl3D : TControl3D);
End;

TFMeXGraphFor2D = Class(TFMeXCustomGraph)
Public
  constructor Create(AOwner: TComponent); override;

  Procedure ProcessGraph; Override;
End;

TFMeXGraph = Class(TFMeXCustomGraph)
Private
Public
  ProxyList : TList<TFMeXProxy>;

  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  Procedure ProcessGraph; Override;
End;

TFMeXControl3d = Class(TControl3D)
private
Protected
  FColor : TAlphaColor;
Public
  constructor Create(AOwner: TComponent); override;
Published
  Property DefaultColor : TAlphaColor read FColor Write FColor;
end;

TeCubeSimple = Class(TFMeXControl3d)
Private
  Ver: TVertexBuffer;
  Idx: TIndexBuffer;
  Mat: TColorMaterial;

  Procedure BuildMesh;
Protected
  procedure SetHeight(const Value: Single); Override;
  procedure SetWidth(const Value: Single); Override;
  procedure SetDepth(const Value: Single); Override;
Public
  Procedure Render; Override;
  constructor Create(AOwner: TComponent); override;
  Destructor Destroy; Override;
Published
End;


TeCustomMeshDrawType = (mdtNone, mdtPointFrame, mdtWireFrame, mdtFull);
TeCustomMesh = Class(TCustomMesh)
private
Protected
  FCurrentBoundingBox : TBoundingBox;
  FDrawOverlap: Boolean;
  FDrawFrameType: TeCustomMeshDrawType;

  procedure SetDrawFrameType(const Value: TeCustomMeshDrawType);
  procedure SetDrawOverlap(const Value: Boolean);
  Procedure DrawOverlap(aContext : TContext3D); Virtual;
  Procedure ProcessBoundingBox;
Public
  Property Data; //Put in public -> Important : For exploitation of data from outside !

  Procedure MergeFrom(aTeCustomMesh : TeCustomMesh);  //For Static object (You will never need to modify it after.)

  constructor Create(AOwner: TComponent); override;

  Procedure Render; Overload; Override;              //Called by classic FPM System.
  Procedure Render(aContext : TContext3D); Overload; Virtual; //Called by graph.


  //Some component, such as texture configuration, will trig above method, if this object is registered in their own list.
  Procedure Notification(Sender : TObject); Virtual; Abstract;

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
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
    function DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean; override;
  {$ELSE}
    function DoRayCastIntersect(const RayPos, RayDir: TVector3D; var Intersection: TVector3D): Boolean; override;
  {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
//    Procedure DrawOverlap; Override;

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
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
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



implementation

Uses Math;


{ TFMeXControl3d }

constructor TFMeXControl3d.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FColor := TAlphaColorRec.Blueviolet;
end;


{ TeCube }

procedure TeCubeSimple.BuildMesh;
var
  I: Integer;
  a, b, Center, Size: TVector3D;
begin
  Center := NullVector3D;
  Size := Vector3D(Width,Height,Depth);
  {$IFDEF VER260}
  a := Center - (Size * 0.5);
  b := Center + (Size * 0.5);
  {$ELSE}

  {$ENDIF}

  begin
    Ver.Vertices[0] := TPoint3D.Create(a.X, a.Y, b.Z);
    Ver.TexCoord0[0] := PointF(0, 0);
    Ver.Vertices[1] := TPoint3D.Create(b.X, a.Y, b.Z);
    Ver.TexCoord0[1] := PointF(1, 0);
    Ver.Vertices[2] := TPoint3D.Create(a.X, a.Y, a.Z);
    Ver.TexCoord0[2] := PointF(0, 0);
    Ver.Vertices[3] := TPoint3D.Create(b.X, a.Y, a.Z);
    Ver.TexCoord0[3] := PointF(1, 0);
    Ver.Vertices[4] := TPoint3D.Create(a.X, b.Y, b.Z);
    Ver.TexCoord0[4] := PointF(0, 0);
    Ver.Vertices[5] := TPoint3D.Create(b.X, b.Y, b.Z);
    Ver.TexCoord0[5] := PointF(1, 0);
    Ver.Vertices[6] := TPoint3D.Create(a.X, b.Y, a.Z);
    Ver.TexCoord0[6] := PointF(0, 0);
    Ver.Vertices[7] := TPoint3D.Create(b.X, b.Y, a.Z);
    Ver.TexCoord0[7] := PointF(1, 0);

    Ver.Vertices[8] := TPoint3D.Create(a.X, a.Y, a.Z);
    Ver.Vertices[9] := TPoint3D.Create(a.X, b.Y, a.Z);
    Ver.Vertices[10] := TPoint3D.Create(a.X, a.Y, b.Z);
    Ver.Vertices[11] := TPoint3D.Create(a.X, b.Y, b.Z);
    Ver.Vertices[12] := TPoint3D.Create(b.X, a.Y, a.Z);
    Ver.Vertices[13] := TPoint3D.Create(b.X, b.Y, a.Z);
    Ver.Vertices[14] := TPoint3D.Create(b.X, a.Y, b.Z);
    Ver.Vertices[15] := TPoint3D.Create(b.X, b.Y, b.Z);

    Ver.Vertices[16] := TPoint3D.Create(a.X, a.Y, a.Z);
    Ver.Vertices[17] := TPoint3D.Create(a.X, a.Y, b.Z);
    Ver.Vertices[18] := TPoint3D.Create(b.X, a.Y, a.Z);
    Ver.Vertices[19] := TPoint3D.Create(b.X, a.Y, b.Z);
    Ver.Vertices[20] := TPoint3D.Create(a.X, b.Y, a.Z);
    Ver.Vertices[21] := TPoint3D.Create(a.X, b.Y, b.Z);
    Ver.Vertices[22] := TPoint3D.Create(b.X, b.Y, a.Z);
    Ver.Vertices[23] := TPoint3D.Create(b.X, b.Y, b.Z);
  end;
  for I := 0 to Ver.Length - 1 do
    Idx[I] := I;
  Mat.Color := FColor;
end;

constructor TeCubeSimple.Create(AOwner: TComponent);
begin
  inherited;
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
    Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 24);
  {$ELSE}
    Ver := TVertexBuffer.Create([TVertexFormat.vfVertex, TVertexFormat.vfTexCoord0], 24);
  {$ENDIF}
  Idx := TIndexBuffer.Create(24);
  Mat := TColorMaterial.Create;
  BuildMesh;
end;

destructor TeCubeSimple.Destroy;
begin
  if Assigned(Idx) then
    FreeAndNil(Idx);
  if Assigned(ver) then
    FreeAndNil(Ver);
  if Assigned(Mat) then
    FreeAndNil(Mat);
  inherited;
end;

procedure TeCubeSimple.Render;
begin
  {$IFDEF VER260}
  Context.SetMatrix(TMatrix3D.CreateScaling(Point3D(Width, Height, Depth)) * AbsoluteMatrix);
  {$ENDIF}

  {$IF Defined(VER280) or Defined(VER310)}
  Context.SetMatrix(TMatrix3D.CreateScaling(Point3D(Width, Height, Depth)) * AbsoluteMatrix);
  {$ENDIF}

  {$IFDEF VER240}
  Context.SetMatrix(Matrix3DMultiply(CreateScaleMatrix3D(Vector3D(Width, Height, Depth)), AbsoluteMatrix));
  {$ENDIF}
  Context.DrawLines(Ver, Idx, Mat, Opacity);
end;

procedure TeCubeSimple.SetDepth(const Value: Single);
begin
  inherited;
  BuildMesh;
end;

procedure TeCubeSimple.SetHeight(const Value: Single);
begin
  inherited;
  BuildMesh;
end;

procedure TeCubeSimple.SetWidth(const Value: Single);
begin
  inherited;

  BuildMesh;
end;

{ TFMeXProxy }

procedure TFMeXProxy.Add(aControl3D: TControl3D);
begin
  if Assigned(FGraph) then
  begin
    FGraph.Add(aControl3D);
  end
  else
  begin
    raise Exception.Create('FMeXGraph instance needed : Please assign Graph property.');
  end;
end;

procedure TFMeXProxy.Analyze;
begin

end;

constructor TFMeXProxy.Create(AOwner: TComponent);
begin
  inherited;
  FGraph := TFMeXGraph.Create(nil);
  HitTest := False;
end;

destructor TFMeXProxy.Destroy;
begin
  inherited;
end;


procedure TFMeXProxy.GlobalSetUpContext;
begin
  Context.SetMatrix(AbsoluteMatrix);
  if FZWrite then
  begin
    Context.SetContextState(TContextState.csZWriteOn);
    Context.SetContextState(TContextState.csZTestOn);
  end
  else
  begin
    Context.SetContextState(TContextState.csZWriteOff);
    Context.SetContextState(TContextState.csZTestOff);
  end;
  if Projection = TProjection.Camera then
    Context.SetContextState(TContextState.cs3DScene)
  else
    Context.SetContextState(TContextState.cs2DScene);
  if TwoSide then
    Context.SetContextState(TContextState.csAllFace)
  else
    Context.SetContextState(TContextState.csFrontFace);

  if Opaque then
    Context.SetContextState(TContextState.csAlphaBlendOff)
  else
    Context.SetContextState(TContextState.csAlphaBlendOn);
end;

procedure TFMeXProxy.Render;
begin
  Context.SetMatrix(AbsoluteMatrix);
  if assigned(FGraph) then
  begin
    FGraph.ProcessGraph;
  end;
end;

procedure TFMeXProxy.SetGraph(const Value: TFMeXCustomGraph);
begin
  FGraph := Value;
  FGraph.Proxy := Self;
end;

{ TFMeXGraph }

procedure TFMeXGraph.ProcessGraph;
var b : TControl3D;
    p : TFMeXProxy;
begin
  for p in ProxyList  do
  begin
    for b in ObjectList do
    begin
      b.Parent := p;
    end;
    p.Repaint;
  end;
end;

constructor TFMeXGraph.Create(AOwner: TComponent);
begin
  inherited;
  ProxyList := TList<TFMeXProxy>.Create;
end;

destructor TFMeXGraph.Destroy;
begin
  FreeAndNil(ProxyList);
  inherited;
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

  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
function TeCube.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3D): Boolean;
{$ELSE}
function TeCube.DoRayCastIntersect(const RayPos, RayDir: TVector3D; var Intersection: TVector3D): Boolean;
{$ENDIF}
var
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
  INear, IFar: TPoint3D;
{$ELSE}
  INear, IFar: TVector3D;
{$ENDIF}
begin
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
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
  WrapMode := TMeshWrapMode.Resize;
  FDrawFrameType := mdtFull;
  FDrawOverlap := False;
  ZWrite := True;
  TwoSide := True;
  HitTest := False;
end;

procedure TeCustomMesh.DrawOverlap(aContext : TContext3D);
begin
  aContext.DrawCube( NullVector3D,
                    Vector3D( FCurrentBoundingBox.Width,
                              FCurrentBoundingBox.Height,
                              FCurrentBoundingBox.Depth) / 2,
                    AbsoluteOpacity,
                    TAlphaColorRec.Blue);
end;

procedure TeCustomMesh.MergeFrom(aTeCustomMesh: TeCustomMesh);
var
  I: Integer;
  vold : Integer;
  iold : Integer;
begin
  vold := Data.VertexBuffer.Length;
  Data.VertexBuffer.Length := Data.VertexBuffer.Length + aTeCustomMesh.Data.VertexBuffer.Length;
  for I := vold to Data.VertexBuffer.Length - 1 do
  begin
    Data.VertexBuffer.Vertices[I] := Point3d( aTeCustomMesh.Data.VertexBuffer.Vertices[I-vold].X + aTeCustomMesh.Position.X,
                                              aTeCustomMesh.Data.VertexBuffer.Vertices[I-vold].Y + aTeCustomMesh.Position.Y,
                                              aTeCustomMesh.Data.VertexBuffer.Vertices[I-vold].Z + aTeCustomMesh.Position.Z);
    Data.VertexBuffer.Normals[I] := Point3d( aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].X  + aTeCustomMesh.Position.X,
                                             aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].Y  + aTeCustomMesh.Position.Y,
                                             aTeCustomMesh.Data.VertexBuffer.Normals[I-vold].Z  + aTeCustomMesh.Position.Z);
    //Data.VertexBuffer.Normals[I] := Point3d( aTeCustomMesh.Data.VertexBuffer.Normals[I-iold].X,
    //                                         aTeCustomMesh.Data.VertexBuffer.Normals[I-iold].Y,
    //                                         aTeCustomMesh.Data.VertexBuffer.Normals[I-iold].Z);
    Data.VertexBuffer.TexCoord0[I] := aTeCustomMesh.Data.VertexBuffer.TexCoord0[I-vold];
  end;

  iold := Data.IndexBuffer.Length;
  Data.IndexBuffer.Length := Data.IndexBuffer.Length + aTeCustomMesh.Data.IndexBuffer.Length;
  for I := iold to Data.IndexBuffer.Length - 1 do
    Data.IndexBuffer[I] := vold + aTeCustomMesh.Data.IndexBuffer[I-iold];

  //Update Bounding box.
  ProcessBoundingBox;
end;


procedure TeCustomMesh.ProcessBoundingBox;
begin
  FCurrentBoundingBox := Data.GetBoundingBox;
end;

procedure TeCustomMesh.Render;
begin
  Render(Context);
end;

procedure TeCustomMesh.Render(aContext : TContext3D);
begin
  {$IFDEF VER260}
  aContext.SetMatrix(TMatrix3D.CreateScaling(Point3D(Width, Height, Depth)) * AbsoluteMatrix);
  {$ENDIF}

  {$IF Defined(VER280) or Defined(VER310) or Defined(VER320)}
  aContext.SetMatrix(TMatrix3D.CreateScaling(Point3D(Width, Height, Depth)) * AbsoluteMatrix);
  {$ENDIF}

  {$IFDEF VER240}
  aContext.SetMatrix(Matrix3DMultiply(CreateScaleMatrix3D(Vector3D(Width, Height, Depth)), AbsoluteMatrix));
  {$ENDIF}

  case FDrawFrameType of
    mdtFull:
    begin
      aContext.DrawTriangles(Data.VertexBuffer, Data.IndexBuffer, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity);
    End;
    mdtPointFrame:
    begin
      aContext.DrawPoints(Data.VertexBuffer, Data.IndexBuffer, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity);
    end;
    mdtWireFrame:
    begin
      aContext.DrawLines(Data.VertexBuffer, Data.IndexBuffer, TMaterialSource.ValidMaterial(MaterialSource), AbsoluteOpacity);
    end;
    mdtNone: ;
  end;

  if FDrawOverlap then
  begin
    DrawOverlap(aContext);
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

  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
function TeSphere.DoRayCastIntersect(const RayPos, RayDir: TPoint3D; var Intersection: TPoint3d): Boolean;
{$ELSE}
function TeSphere.DoRayCastIntersect(const RayPos, RayDir: TVector3D; var Intersection: TPoint3d): Boolean;
{$ENDIF}
var
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
  INear, IFar: TPoint3D;
{$ELSE}
  INear, IFar: TVector3D;
{$ENDIF}
begin
  {$IF Defined(VER280) OR Defined(VER310) or Defined(VER320)}
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

{ TFMeXCustomGrap }

constructor TFMeXCustomGraph.Create(AOwner: TComponent);
begin
  inherited;
  ObjectList := TList<TControl3D>.Create;
end;

destructor TFMeXCustomGraph.Destroy;
begin
  FreeAndNil(ObjectList);
  inherited;
end;


procedure TFMeXCustomGraph.Add(aControl3D: TControl3D);
begin
  ObjectList.Add(aControl3D);
end;


{ TFMeXGraphFor2D }

constructor TFMeXGraphFor2D.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TFMeXGraphFor2D.ProcessGraph;
var a : TControl3D;
begin
  Assert(Assigned(Proxy));
  //Object are render in their strict list sequence.
  //Beware of Z position : It must be mastered because if Z is near the camera, but Z is renderer first, you
  //will certainly loose the transparency.
  Proxy.GlobalSetUpContext;
  for a in ObjectList do
  begin
    TeCustomMesh(a).Render(Proxy.Context);
  end;
end;

end.
