unit FMeX.Gx.Types;


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
  FMX.MaterialSources,
  GS.Geometry.Direction;

Type

TP2f = Record
  X,Y : Single;
end;

TeRect = record
  top,left,right,bottom : single;
end;

TeQuad = record
  topleft,
  topRight,
  bottomRight,
  bottomLeft : TPt2f;

  procedure Build(tl_x,tl_y,tr_x,tr_y,br_x,br_y,bl_x,bl_y : single);
end;

TeBox = record
  left,top,near : single;
  right,bottom,far : single;
end;

TeRawMesh = record
  meshes : Array of TPt;
  Indexes : Array of Integer;

//  procedure SetCapacity(aMeshCount : UInt32);
  procedure BuildLine(const x,y,x2,y2 : single; const Resolution : Uint32 = 1; const Thikness : Single = 2.0);

  //Generate to triangle, in a square shape, upper of the x,y,x2,y2 line.
  procedure GenerateSquareMesh(const xy,x2y2 : TPt; const Thikness : Single = 2.0; const Up : Boolean = true);
end;

Type
TeGraphElement = class;
TeGraphElement2d = class;
TeGraphElement3d = class;
TeGraph2d = class;

TeGraphElement = class abstract
end;

TeGraphElement2d = class(TeGraphElement)
private
protected
  FLocalMatrix : TMatrix;
  FX: single;
  FY: single;
  FWidth: single;
  FHeight: single;
  FAngle: single;
  FData : TeRawMesh;

  procedure InternalMatrixProcess;
  function GetBoundingRect: TeRect;
  function GetPosition: TPointF;
  procedure SetHeight(const Value: single);
  procedure SetPosition(const Value: TPointF);
  procedure SetWidth(const Value: single);
  procedure SetX(const Value: single);
  procedure SetY(const Value: single);
  procedure SetAngle(const Value: single);
Public
  constructor create; virtual;

  property Position : TPointF read GetPosition Write SetPosition;
  property X : single read FX Write SetX;
  property Y : single read FY Write SetY;
  property angle : single read FAngle Write SetAngle;

  property BoundingBox : TeRect read GetBoundingRect;
  property Width : single read FWidth Write SetWidth;
  property Height : single read FHeight Write SetHeight;

  property localMatrix : TMatrix read FLocalMatrix;
end;

TeGraphElement3D = class(TeGraphElement)
  private
protected
  FLocalMatrix : TMatrix3D;
  FQr : TQuaternion3D;
  FZ: single;
  FX: single;
  FY: single;
  FWidth: single;
  FDepth: single;
  FHeight: single;

  FXR, FXRS: single;
  FYR, FYRS: single;
  FZR, FZRS: single;

  procedure InternalMatrixProcess;

  procedure SetX(const Value: single);
  procedure SetY(const Value: single);
  procedure SetZ(const Value: single);

  procedure SetXR(const Value: single);
  procedure SetYR(const Value: single);
  procedure SetZR(const Value: single);

  procedure SetDepth(const Value: single);
  procedure SetHeight(const Value: single);
  procedure SetWidth(const Value: single);

  function GetPosition: TPoint3D;
  procedure SetPosition(const Value: TPoint3D);

  function GetDepth: single; virtual; abstract;
  function GetHeight: single; virtual; abstract;
  function GetWidth: single; virtual; abstract;

  function GetBoundingBox: TeBox; virtual;
public
  constructor create; virtual;

  property Position : TPoint3D read GetPosition Write SetPosition;
  property X : single read FX Write SetX;
  property Y : single read FY Write SetY;
  property Z : single read FZ Write SetZ;

  Property XRotate : single read FXR Write SetXR;
  Property YRotate : single read FYR Write SetYR;
  Property ZRotate : single read FZR Write SetZR;


  property Box : TeBox read GetBoundingBox;
  property Width : single read FWidth Write SetWidth;
  property Height : single read FHeight Write SetHeight;
  property Depth : single read FDepth Write SetDepth;

  property localMatrix : TMatrix3D read FLocalMatrix;
end;

TeGraph2d = class
private
protected
  Elems : TObjectList<TeGraphElement2d>;
public
  constructor Create; Virtual;
  destructor Destroy; Override;
  function Add(anElement : TeGraphElement2d; const AllowDuplicate : Boolean = true) : TeGraphElement2d; Virtual;
end;


TFMeXGraph3D = class(TeGraphElement3d)
protected
public
  procedure Render(acontext : TContext3D; const aFXMControl : TControl3d = nil); virtual;
end;

TeMesh3D = class(TFMeXGraph3D)
private
protected
  FMaterialSource: TMaterialSource;
  FData : TMeshData;

  procedure BuildMesh; virtual;
public
  constructor Create; override;
  destructor destroy; Override;
  //For ressource change, asset modification notification.
  procedure MeshNotification(Sender : TObject); virtual; abstract;

  procedure Render(acontext : TContext3D; const aFXMControl : TControl3d = nil); Override;

  property MaterialSource : TMaterialSource read FMaterialSource write FMaterialSource;
end;




implementation


{ TFMeXeControl3D }

procedure TFMeXGraph3D.Render(acontext: TContext3D; const aFXMControl : TControl3d = nil);
begin
  assert(assigned(aContext));

  if Assigned(aFXMControl) then
    acontext.SetMatrix(localMatrix*aFXMControl.AbsoluteMatrix)
  else
    acontext.SetMatrix(localMatrix);

  acontext.DrawCube(Position,Point3D(1,1,1),1.0,TAlphaColorRec.Blue);
end;


{ TeMesh }

procedure TeMesh3D.BuildMesh;
begin
  FData.BoundingBoxNeedsUpdate;
end;

constructor TeMesh3D.Create;
begin
  inherited;
  FData := TMeshData.Create;
end;

destructor TeMesh3D.destroy;
begin
  FreeAndNil(FData);
  inherited;
end;


procedure TeMesh3D.Render(acontext: TContext3D; const aFXMControl: TControl3d);
var MeshMatrix : TMatrix3D;
begin
  if FData.VertexBuffer.Length>0 then
  begin
    acontext.SetMatrix(localMatrix*aFXMControl.AbsoluteMatrix);
    acontext.DrawTriangles(FData.VertexBuffer,FData.IndexBuffer,MaterialSource.Material,1.0);

    //Draw bounding box.
    aContext.DrawCube( NullPoint3D,
                      Vector3D( Width,
                                Height,
                                Depth),
                      1.0,
                      TAlphaColorRec.Blue);
  end
  else
  begin
    inherited;
  end;
end;

{ TeRawMesh }

procedure TeRawMesh.BuildLine(const x, y, x2, y2: single; const Resolution : UInt32;
  const Thikness: Single);
var l : TDirectionalObject;
begin
  l := TDirectionalObject.Create(x,y,Thikness);
  try
    //To do : Resolution ?
    l.PointAt(Point(x2,y2));
    GenerateSquareMesh(l.Origin,l.GetPointedCoord,Thikness);
    GenerateSquareMesh(l.Origin,l.GetPointedCoord,Thikness,false);
  finally
    l.Free;
  end;
end;

procedure TeRawMesh.GenerateSquareMesh(const xy, x2y2: TPt;
  const Thikness: Single;const Up : Boolean);
var l : TDirectionalObject;
    a,b,c,d : TPt;
    idx : Uint32;
begin
  l := TDirectionalObject.Create(xy.x,xy.y,Thikness/2);
  try
    l.LookAt(x2y2);

    a := l.Origin;
    b := x2y2;

    if Up then
      l.TurnLeft
    else
      l.TurnRight;

    l.MoveAhead;
    c := l.Origin;

    l.SetOrigin(x2y2.X,x2y2.Y,0);
    l.Norm := Thikness/2;
    l.LookAt(xy);

    if Up then
      l.TurnRight
    else
      l.TurnLeft;

    l.MoveAhead;
    d := l.Origin;

    idx := Length(meshes);
    SetLength(meshes,Length(meshes)+4);
    SetLength(Indexes,Length(Indexes)+6);
    meshes[idx] := a;
    meshes[idx+1] := b;
    meshes[idx+2] := c;
    meshes[idx+3] := d;

    Indexes[idx] := idx;
    Indexes[idx+1] := idx+1;
    Indexes[idx+2] := idx+2;
    Indexes[idx+3] := idx+2;
    Indexes[idx+4] := idx+3;
    Indexes[idx+5] := idx+1;

  finally
    l.Free;
  end;

end;

{ TeQuad }

procedure TeQuad.Build(tl_x, tl_y, tr_x, tr_y, br_x, br_y, bl_x, bl_y: single);
begin
  topleft := Point2f(tl_x,tl_y);
  topRight := Point2f(tr_x,tr_y);
  bottomRight := Point2f(br_x,br_y);
  bottomLeft := Point2f(bl_x,bl_y);
end;

{ TeGraphElement3D }


constructor TeGraphElement3D.create;
begin
  inherited;
  FLocalMatrix := TMatrix3D.Identity;
  FQr := TQuaternion3D.Identity;;
  Position := NullPoint3D;
  FWidth := 1;
  FHeight := 1;
  FDepth := 1;

  InternalMatrixProcess;
end;

function TeGraphElement3D.GetBoundingBox: TeBox;
begin
  result.left := FX - FWidth/2;
  result.top := FY - FHeight/2;
  result.near := FZ - FDepth/2;
  result.right := FX + FWidth/2;
  result.bottom := FY + FHeight/2;
  result.far := FZ + FDepth/2;
end;

function TeGraphElement3D.GetPosition: TPoint3D;
begin
  result := Point3D(FX,FY,FZ);
end;


procedure TeGraphElement3D.InternalMatrixProcess;
procedure RotationProcess;
var
  a: Single;
//  l: TPoint3D;
begin
  if not(SameValue(FXR, 0, TEpsilon.Vector) and SameValue(FXR, 0, TEpsilon.Vector) and
    SameValue(FZR, 0, TEpsilon.Vector)) then
  begin
//    FQr := TQuaternion3D.Identity;
    //X rotate.
    a := DegNormalize(FXR - FXRS);
    if a <> 0 then
    begin
      FQr := FQr * TQuaternion3D.Create(Point3D(1, 0, 0), DegToRad(a));
      FXR := DegNormalize(FXR);
      FXRS := FXR;
    end;

    //Y rotate.
    a := DegNormalize(FYR - FYRS);
    if a <> 0 then
    begin
      FQr := FQr * TQuaternion3D.Create(Point3D(0, 1, 0), DegToRad(a));
      FYR := DegNormalize(FYR);
      FYRS := FYR;
    end;


    //Z rotate.
    a := DegNormalize(FZR - FZRS);
    if a <> 0 then
    begin
      FQr := FQr * TQuaternion3D.Create(Point3D(0, 0, 1), DegToRad(a));
      FZR := DegNormalize(FZR);
      FZRS := FZR;
    end;


    FLocalMatrix := FLocalMatrix * FQr;
  end


//  a := DegNormalize(RotationAngle.Y - FSavedRotationAngle.Y);
//  if a <> 0 then
//  begin
//    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(0, 1, 0) { AbsoluteDirection }, DegToRad(a));
//    NeedChange := True;
//    NewValue.Y := DegNormalize(RotationAngle.Y);
//  end;

//  a := DegNormalize(RotationAngle.Z - FSavedRotationAngle.Z);
//  if a <> 0 then
//  begin
//    FQuaternion := FQuaternion * TQuaternion3D.Create(Point3D(0, 0, 1) { AbsoluteUp }, DegToRad(a));
//    NeedChange := True;
//    NewValue.Z := DegNormalize(RotationAngle.Z);
//  end;
//  if NeedChange then
//  begin
//    FSavedRotationAngle := RotationAngle.Point;
//    RotationAngle.SetPoint3DNoChange(NewValue);
//    MatrixChanged(Sender);
//  end;
end;

begin
  FLocalMatrix := TMatrix3D.Identity;
  RotationProcess;
  //Translation.
  FLocalMatrix := FLocalMatrix * TMatrix3D.CreateTranslation(Position); { TODO : create a translation matrix "FTranslationMatrix }
//  InternalRecalcAbsolute;
end;


procedure TeGraphElement3D.SetDepth(const Value: single);
begin
  FDepth := Value;
end;

procedure TeGraphElement3D.SetHeight(const Value: single);
begin
  FHeight := Value;
end;

procedure TeGraphElement3D.SetPosition(const Value: TPoint3D);
begin
  if not( SameValue(FX, Value.X, TEpsilon.Position) and
          SameValue(FY, Value.Y, TEpsilon.Position) and
          SameValue(FZ, Value.Z, TEpsilon.Position) ) then
  begin
    FX := Value.X;
    FY := Value.Y;
    FZ := Value.Z;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetWidth(const Value: single);
begin
  FWidth := Value;
end;

procedure TeGraphElement3D.SetX(const Value: single);
begin
  if not(SameValue(FX, Value, TEpsilon.Position)) Then
  begin
    FX := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetXR(const Value: single);
begin
  if not(SameValue(FXR, Value, TEpsilon.Angle)) Then
  begin
    FXR := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetY(const Value: single);
begin
  if not(SameValue(FY, Value, TEpsilon.Position)) Then
  begin
    FY := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetYR(const Value: single);
begin
  if not(SameValue(FYR, Value, TEpsilon.Angle)) Then
  begin
    FYR := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetZ(const Value: single);
begin
  if not(SameValue(FZ, Value, TEpsilon.Position)) Then
  begin
    FZ := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement3D.SetZR(const Value: single);
begin
  if not(SameValue(FZR, Value, TEpsilon.Angle)) Then
  begin
    FZR := Value;
    InternalMatrixProcess;
  end;
end;

{ TeGraphElement2d }

constructor TeGraphElement2d.create;
begin
  inherited;
  FLocalMatrix := TMatrix.Identity;
  Position := pointf(0,0);
  FWidth := 1;
  FHeight := 1;
  FAngle := 0;
  InternalMatrixProcess;
end;

function TeGraphElement2d.GetBoundingRect: TeRect;
begin
  result.left := FX - FWidth/2;
  result.top := FY - FHeight/2;
  result.right := FX + FWidth/2;
  result.bottom := FY + FHeight/2;
end;

function TeGraphElement2d.GetPosition: TPointF;
begin
  result := Pointf(FX,FY);
end;

procedure TeGraphElement2d.InternalMatrixProcess;
begin
  FLocalMatrix := TMatrix.Identity;
  FLocalMatrix := FLocalMatrix * TMatrix.CreateTranslation(FX,FY)*TMatrix.CreateRotation(FAngle);
end;

procedure TeGraphElement2d.SetAngle(const Value: single);
begin
  if not(SameValue(FAngle, Value, TEpsilon.Position)) Then
  begin
    FAngle := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement2d.SetHeight(const Value: single);
begin
  FHeight := Value;
end;

procedure TeGraphElement2d.SetPosition(const Value: TPointF);
begin
  if not( SameValue(FX, Value.X, TEpsilon.Position) and
          SameValue(FY, Value.Y, TEpsilon.Position) ) then
  begin
    FX := Value.X;
    FY := Value.Y;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement2d.SetWidth(const Value: single);
begin
  FWidth := Value;
end;

procedure TeGraphElement2d.SetX(const Value: single);
begin
  if not(SameValue(FX, Value, TEpsilon.Position)) Then
  begin
    FX := Value;
    InternalMatrixProcess;
  end;
end;

procedure TeGraphElement2d.SetY(const Value: single);
begin
  if not(SameValue(FY, Value, TEpsilon.Position)) Then
  begin
    FY := Value;
    InternalMatrixProcess;
  end;
end;

{ TeGraph2d }

function TeGraph2d.Add(anElement: TeGraphElement2d; const AllowDuplicate : Boolean = true) : TeGraphElement2d;
begin
  Assert(Assigned(anElement));
  result := anElement;
  If not AllowDuplicate then
  begin
    if Elems.IndexOf(anElement)>-1 then
      Exit;
  end;
  Elems.Add(anElement);
end;

constructor TeGraph2d.Create;
begin
  inherited;
  Elems := TObjectList<TeGraphElement2d>.Create;
end;

destructor TeGraph2d.Destroy;
begin
  FreeAndNil(Elems);
  inherited;
end;



end.
