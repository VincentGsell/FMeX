//CREDITS : Authors : http://delphiscience.wordpress.com/ (Mehmed Ali Caliskan)
unit FMeX.Types3D.Pipe;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Math,System.UIConsts, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Types3D, FMX.Objects3D, FMX.Layers3D, FMX.Objects, FMX.Menus, FMX.Edit, FMX.Colors,
  System.StrUtils,Generics.Collections, FMX.Ani,FMX.Materials,Generics.Defaults,
  FMeX.Types3D;


Type
  TSectionType = (sctNone,sctTop,sctBottom);
  TFrameType = (ftEllipse,ftRectangle);
  TPointArray = array of TPoint3d;
  TDummyPointLayer = class;
  TPointLayer = class(TObject)
  private
    FParent: TPointLayer;
    FChild: TPointLayer;
    FContent: TPointLayer;
    FPosition: TPosition3d;
    FLocalMatrix: TMatrix3D;
    FRotationAngle: TPosition3D;
    FQuaternion: TQuaternion3D;
    FScale: TPosition3D;
    FSavedAbsoluteMatrix: TMatrix3d;
    FAbsMatrixNeedRefresh: Boolean;
    FGapLayer: Boolean;
    function GetLength: Integer;
    procedure SetPointsLength(const Value: Integer);
    Procedure MatrixChanged(Sender:TObject);
    procedure RotationChanged(Sender: TObject); virtual;
    function GetAbsoluteMatrix: TMatrix3D;
    function GetRealParent: TPointLayer;
    function GetRealChild: TPointLayer;
    function GetLayerCount: Integer;
    function GetFirstParent: TPointLayer;
    function GetDummyChild: TPointLayer;
    function GeAbsoluteCenter: TPoint3d;
    function GetLayerH: Single;
  protected
    Procedure CreateDummies;virtual;
  public
    Points:TPointArray;
    Constructor Create;
    Destructor Destroy;override;
    Function CreateChildAtPosition(CPos:TPoint3d; RepeatNbr:Integer):TPointLayer;
    Procedure AddChild(CPointLayer:TPointLayer);
    Function LastChild:TPointLayer;
    Function RemoveFirstChild:TPointLayer;
    Function Index:Integer;
    Function GetLayer(LIndex:Integer):TPointLayer;
    Function AbsPoint(i:Integer):TPoint3d;
    Function GetTotalTurn:Single;
    Function Content:TPointLayer;
    Procedure InvalidateAbsoluteMatrix;
    Property FirstParent:TPointLayer read GetFirstParent;
    Property RealParent:TPointLayer read GetRealParent;
    Property RealChild:TPointLayer read GetRealChild;
    Property DummyChild:TPointLayer read GetDummyChild;
    Property Length:Integer read GetLength write SetPointsLength;
    Property Position:TPosition3d read FPosition write FPosition;
    property AbsoluteMatrix: TMatrix3D read GetAbsoluteMatrix;
    property LocalMatrix: TMatrix3D read FLocalMatrix;
    Property AbsoluteCenter:TPoint3d read GeAbsoluteCenter;
    Property LayerH: Single read GetLayerH;
    Property GapLayer:Boolean read FGapLayer write FGapLayer;
    property RotationAngle: TPosition3D read FRotationAngle write FRotationAngle;
    property Scale: TPosition3D read FScale write FScale;
    Property LayerCount:Integer read GetLayerCount;
  end;

  TDummyPointLayer = class(TPointLayer)
  protected
    Procedure CreateDummies;override;
  end;

  TLayerList = TList<TPointLayer>;
  TeAnnulus = class(TeCustomMesh)
  private
    FSectionType: TSectionType;
    FSectionDegree: Integer;
    FInnerFrameType: TFrameType;
    FOuterFrameType: TFrameType;
    FDrawBounds: Boolean;
    procedure SetThickness(const Value: Single);
    procedure SetSubdivisionsAxes(const Value: Integer);
    procedure SetSectionDegree(const Value: Integer);
    procedure SetSectionType(const Value: TSectionType);
    procedure setInnerFrameType(const Value: TFrameType);
    procedure setOuterFrameType(const Value: TFrameType);
    procedure SetDrawBounds(const Value: Boolean);
  protected
    FSubdivisionsAxes:Integer;
    FUnitWidth: Single;
    FUnitHeight: Single;
    FThickness: Single;
    FRenderScale:Single;
    FStartAngle: Single;
    FTotalAngle: Single;
    FDistAngle:Single;
    InnerPoints:TPointArray;
    OuterPoints:TPointArray;
    Function ExtendPointToPlane(point:TPoint3d;Plane,PlaneNormal:TVector3d;var Distance:Single; var nPoint:TPoint3d):Boolean;
    Procedure CalcPoints;virtual;
    Procedure GetAnnulusPointsForPosY(PosY:Single;var IPoints,OPoints:TPointArray);virtual;
    procedure BuildAnnulus(IPoints,OPoints:TPointArray;Back:Boolean);virtual;
    procedure RebuildMesh;virtual;
    procedure Render; override;
    function FixHeight:Boolean;virtual;
    procedure SetHeight(const Value: Single); override;
    procedure SetWidth(const Value: Single); override;
    procedure SetDepth(const Value: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Data;
    Property Thickness:Single read FThickness write SetThickness;
    Property SubdivisionsAxes:Integer read FSubdivisionsAxes write SetSubdivisionsAxes;
    Property SectionType:TSectionType read FSectionType write SetSectionType;
    Property SectionDegree:Integer read FSectionDegree write SetSectionDegree;
    Property InnerFrameType:TFrameType read FInnerFrameType write setInnerFrameType;
    Property OuterFrameType:TFrameType read FOuterFrameType write setOuterFrameType;
    Property RenderScale:Single read FRenderScale;
    Property DrawBounds:Boolean read FDrawBounds write SetDrawBounds;
  end;

  TePipe = class;
  TPipeModifier = class(TFMXObject)
  private
    FPipe: TePipe;
    FStartPosition: Single;
    FEndPosition: Single;
    FSubdivisions:Integer;
    FUseGap:Boolean;
    FFirstCenter: TPoint3d;
    FLastCenter: TPoint3d;
    FModifyMargins: Boolean;
    procedure SetStartPosition(const Value: Single);
    procedure SetEndPosition(const Value: Single);
    procedure SetSubdivisions(const Value: Integer);
    function InsertPointLayer(StartLayer: TPointLayer; layerH: Single; UseGap: Boolean=False): TPointLayer;
    procedure SetModifyMargins(const Value: Boolean);
  protected
    FStartMargin: Single;
    FEndMargin:Single;
    FLayerCount: Integer;
    StartLayer,EndLayer,StartMLayer,EndMLayer:TPointLayer;
    Procedure BeginModify(StartPoints:TPointLayer);virtual;
  public
    Constructor Create(aPipe:TePipe);virtual;
    Procedure ModifySubPoints(sPoints:TPointLayer;isInner:Boolean);virtual;abstract;
    Procedure DoModify(StartPoints:TPointLayer);virtual;
    Procedure EndModify;virtual;
  published
    Property StartPosition:Single read FStartPosition write SetStartPosition;
    Property EndPosition:Single read FEndPosition write SetEndPosition;
    Property Subdivisions:Integer read FSubdivisions write SetSubdivisions;
    Property UseGap:Boolean read FUseGap write FUseGap;
    Property FirstCenter:TPoint3d read FFirstCenter;
    Property LastCenter:TPoint3d read FLastCenter;
    Property ModifyMargins:Boolean read FModifyMargins write SetModifyMargins;
  end;

  TBendModifier = class(TPipeModifier)
  private
    FBendAngle: Single;
    FTurnAngle: Single;
    procedure SetBendAngle(const Value: Single);
    procedure SetTurnAngle(const Value: Single);
  public
    Constructor Create(aPipe:TePipe);override;
    Destructor Destroy;override;
    Procedure ModifySubPoints(sPoints:TPointLayer; isInner:Boolean);override;
  published
    Property BendAngle:Single read FBendAngle write SetBendAngle;
    Property TurnAngle:Single read FTurnAngle write SetTurnAngle;
  end;

  TBreakModifier = class(TBendModifier)
  private
    procedure SetEndMargin(const Value: Single);
    procedure SetStartMargin(const Value: Single);
  public
    Constructor Create(aPipe:TePipe);override;
    Procedure ModifySubPoints(sPoints:TPointLayer; isInner:Boolean);override;
    Property StartMargin:Single read FStartMargin write SetStartMargin;
    Property EndMargin:Single read FEndMargin write SetEndMargin;
  end;

  TTwistModifier = class(TPipeModifier)
  private
    FTotalRotation: Single;
    FOldCenter: TPoint3d;
    procedure SetTotalRotation(const Value: Single);
  public
    Constructor Create(aPipe:TePipe);override;
    Procedure ModifySubPoints(sPoints:TPointLayer; isInner:Boolean);override;
  published
    Property TotalRotation:Single read FTotalRotation write SetTotalRotation;
  end;

  TEmbossModifier = class(TPipeModifier)
  private
    FThicknessRatio: Single;
    procedure SetThicknessRatio(const Value: Single);
  public
    Constructor Create(aPipe:TePipe);override;
    Procedure ModifySubPoints(sPoints:TPointLayer; isInner:Boolean);override;
    Property ThicknessRatio:Single read FThicknessRatio write SetThicknessRatio;
  end;

  TePipe = class(TeAnnulus)
  private
    FModifiers: TList<TPipeModifier>;
    FOnZAxis: Boolean;
    FFirstCenter: TPoint3d;
    FLastCenter: TPOint3D;
    FScaleBeforeRender: Boolean;
    Procedure SortModifiers;
    procedure SetOnZAxis(const Value: Boolean);
    procedure SetScaleBeforeRender(const Value: Boolean);
  protected
    function FixHeight:Boolean;override;
    procedure SetHeight(const Value: Single); override;
    Procedure GetAnnulusPointsForPosY(PosY:Single;var IPoints,OPoints:TPointArray);override;
    procedure BuildSectionSurfaces(OuterSectionPoints,InnerSectionPoints:TPointArray);virtual;
    procedure BuildCylinder(Points:TPointArray;Back:Boolean; var SectionPoints,FirstPoints,LastPoints:TPointArray);virtual;
    procedure RebuildMesh;override;
    Procedure Render;override;
    Procedure ModifiersNotify(Sender:Tobject;Const Item:TPipeModifier;Action: TCollectionNotification);
  public
    constructor Create(AOwner: TComponent); override;
    Procedure ClearModifiers;
    destructor Destroy;override;
    Property Modifiers:TList<TPipeModifier> read FModifiers;
    Property OnZAxis:Boolean read FOnZAxis write SetOnZAxis;
    Property FirstCenter:TPoint3d Read FFirstCenter;
    Property LastCenter:TPOint3D read FLastCenter;
    Property ScaleBeforeRender:Boolean read FScaleBeforeRender write SetScaleBeforeRender;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('3D Shapes', [TeAnnulus,TePipe]);
end;

{ TePipe }

procedure TeAnnulus.BuildAnnulus(IPoints,OPoints:TPointArray;Back:Boolean);
var FData:TMeshData;
    i: Integer;
    vertexIdx:Integer;
    indexIdx:Integer;
begin

  FData := Self.Data;
  VertexIdx := FData.VertexBuffer.Length;
  IndexIdx := FData.IndexBuffer.Length;
  FData.VertexBuffer.Length := VertexIdx + FSubdivisionsAxes * 2 ;

  for i := 0 to FSubdivisionsAxes - 1 do
  begin
    FData.VertexBuffer.Vertices[VertexIdx+i] := iPoints[i];
    FData.VertexBuffer.TexCoord0[VertexIdx+i] := Pointf((iPoints[i].X+FUnitWidth/2)/FunitWidth,(iPoints[i].Z+FUnitHeight/2)/FUnitHeight);
    FData.VertexBuffer.Vertices[VertexIdx+i+FSubdivisionsAxes] := oPoints[i];
    FData.VertexBuffer.TexCoord0[VertexIdx+i+FSubdivisionsAxes] := Pointf((oPoints[i].X+FUnitWidth/2)/FUnitWidth,(oPoints[i].Z+FUnitHeight/2)/FUnitHeight);
  end;

  FData.IndexBuffer.Length := IndexIdx + FSubdivisionsAxes * 6 ;
  if (FSectionType <> sctNone) then FData.IndexBuffer.Length := FData.IndexBuffer.Length - 6;

  for i := 0 to FSubdivisionsAxes - 1 do
  begin
    if (i = FSubdivisionsAxes -1) then
    begin
      if (FSectionType = sctNone) then
      begin
        FData.IndexBuffer.Indices[IndexIdx+i*6+0]   := VertexIdx+i;
        FData.IndexBuffer.Indices[IndexIdx+i*6+1] := VertexIdx+0;
        FData.IndexBuffer.Indices[IndexIdx+i*6+2] := VertexIdx+i+FSubdivisionsAxes;
        FData.IndexBuffer.Indices[IndexIdx+i*6+3] := VertexIdx+0;
        FData.IndexBuffer.Indices[IndexIdx+i*6+4] := VertexIdx+FSubdivisionsAxes;
        FData.IndexBuffer.Indices[IndexIdx+i*6+5] := VertexIdx+i+FSubdivisionsAxes;

        if back then
        begin
          FData.IndexBuffer.Indices[IndexIdx+i*6+2]   := VertexIdx+i;
          FData.IndexBuffer.Indices[IndexIdx+i*6+1] := VertexIdx+0;
          FData.IndexBuffer.Indices[IndexIdx+i*6+0] := VertexIdx+i+FSubdivisionsAxes;
          FData.IndexBuffer.Indices[IndexIdx+i*6+5] := VertexIdx+0;
          FData.IndexBuffer.Indices[IndexIdx+i*6+4] := VertexIdx+FSubdivisionsAxes;
          FData.IndexBuffer.Indices[IndexIdx+i*6+3] := VertexIdx+i+FSubdivisionsAxes;
        end;

      end;
    end else begin
      FData.IndexBuffer.Indices[IndexIdx+i*6]   := VertexIdx+i;
      FData.IndexBuffer.Indices[IndexIdx+i*6+1] := VertexIdx+i+1;
      FData.IndexBuffer.Indices[IndexIdx+i*6+2] := VertexIdx+i+FSubdivisionsAxes;
      FData.IndexBuffer.Indices[IndexIdx+i*6+3] := VertexIdx+i+1;
      FData.IndexBuffer.Indices[IndexIdx+i*6+4] := VertexIdx+i+FSubdivisionsAxes+1;
      FData.IndexBuffer.Indices[IndexIdx+i*6+5] := VertexIdx+i+FSubdivisionsAxes;
      if back then
      begin
        FData.IndexBuffer.Indices[IndexIdx+i*6+2]   := VertexIdx+i;
        FData.IndexBuffer.Indices[IndexIdx+i*6+1] := VertexIdx+i+1;
        FData.IndexBuffer.Indices[IndexIdx+i*6+0] := VertexIdx+i+FSubdivisionsAxes;
        FData.IndexBuffer.Indices[IndexIdx+i*6+5] := VertexIdx+i+1;
        FData.IndexBuffer.Indices[IndexIdx+i*6+4] := VertexIdx+i+FSubdivisionsAxes+1;
        FData.IndexBuffer.Indices[IndexIdx+i*6+3] := VertexIdx+i+FSubdivisionsAxes;
      end;
    end;
  end;
end;


procedure TeAnnulus.CalcPoints;
var
  PhiSin, PhiCos: Extended;
  iWidth,iHeight:Single;
  rThickness:Single;
  A: Integer;
  Angle: Single;
  rPoint:TPoint3d;
  iPoint:TPoint3d;
  iDist: Single;
  uiWidth,uiHeight:Single;
begin
  SetLength(OuterPoints,FSubdivisionsAxes);
  SetLength(InnerPoints,FSubdivisionsAxes);
  FUnitWidth  := 1;
  FUnitHeight := 1;
  if Width > Depth then FUnitWidth := Width/Depth;
  if Depth > Width  then FUnitHeight := Depth/Width;
  rThickness := FThickness * (FUnitWidth/Width);
  FRenderScale := Width/FUnitWidth;

  iWidth := 1;
  iHeight := 1;
  if (FThickness*2 = Depth) or (FThickness*2 = Width) then FThickness := FThickness - 0.1;


  if Width > Depth then iWidth := (Width - (FThickNess*2))/(Depth-(FThickNess*2));
  if Depth > Width then iHeight := (Depth-(FThickNess*2))/(Width - (FThickNess*2));

  FStartAngle := 0;
  FTotalAngle := 360;
  if FSectionType <> sctNone then FTotalAngle := 360 - FSectionDegree;
  if FSectionType = sctBottom then FStartAngle := -(180 - FSectionDegree)/2;
  if FSectionType = sctTop then FStartAngle := 180-(180 - FSectionDegree)/2;

  FDistAngle := FTotalAngle / FSubdivisionsAxes;
  if FSectionType <> sctNone then FDistAngle := FTotalAngle / (FSubdivisionsAxes-1);

  for A := 0 to FSubdivisionsAxes - 1 do
  begin
    Angle := DegToRad(FStartAngle)+DegToRad(FDistAngle)*A;
    SinCos(Angle, PhiSin, PhiCos);

    if FOuterFrameType = ftEllipse then
    begin
      OuterPoints[A] := Point3D(PhiCos * 0.5 *FUnitWidth , 0, PhiSin * 0.5 *FUnitHeight );
    end else begin
      rPoint := Point3D(PhiCos * 0.5 *FUnitWidth , 0, PhiSin * 0.5 *FUnitHeight );
      iDist := -1;
      iPoint := rPoint;
      Self.ExtendPointToPlane(rPoint,Vector3d(FUnitWidth/2,0,0),Vector3d(-1,0,0),iDist,iPoint);
      Self.ExtendPointToPlane(rPoint,Vector3d(0,0,FUnitHeight/2),Vector3d(0,0,-1),iDist,iPoint);
      Self.ExtendPointToPlane(rPoint,Vector3d(-FUnitWidth/2,0,0),Vector3d(1,0,0),iDist,iPoint);
      Self.ExtendPointToPlane(rPoint,Vector3d(0,0,-FUnitHeight/2),Vector3d(0,0,1),iDist,iPoint);
      OuterPoints[A] := iPoint;
    end;
    if FInnerFrameType = ftEllipse then
    begin
      InnerPoints[A] := Point3D(PhiCos * (0.5-rThickness)*iWidth , 0, PhiSin * (0.5-rThickness)*iHeight);
    end else  begin
      rPoint := Point3D(PhiCos * (0.5-rThickness)*iWidth , 0, PhiSin * (0.5-rThickness)*iHeight);
      uiWidth := (0.5-rThickness)*iWidth;
      uiHeight := (0.5-rThickness)*iheight;
      iDist := -1;
      iPoint := rPoint;
      Self.ExtendPointToPlane(rPoint,Vector3d(uiWidth,0,0),Vector3d(-1,0,0),iDist,iPoint);
      Self.ExtendPointToPlane(rPoint,Vector3d(0,0,uiHeight),Vector3d(0,0,-1),iDist,iPoint);
      Self.ExtendPointToPlane(rPoint,Vector3d(-uiWidth,0,0),Vector3d(1,0,0),iDist,iPoint);
      Self.ExtendPointToPlane(rPoint,Vector3d(0,0,-uiHeight),Vector3d(0,0,1),iDist,iPoint);
      InnerPoints[A] := iPoint;
    end;
  end;
end;

constructor TeAnnulus.Create(AOwner: TComponent);
begin
  inherited;
  FThickness := 0.2;
  FSubdivisionsAxes := 180;
  FSectionType := sctNone;
  FSectionDegree := 180;
  FOuterFrameType := ftEllipse;
  FInnerFrameType := ftEllipse;
  RebuildMesh;
end;

function TeAnnulus.ExtendPointToPlane(point: TPoint3d; Plane,
  PlaneNormal: TVector3d; var Distance: Single; var nPoint:TPoint3d): Boolean;
var iPoint:Tvector3d;
    aDist:Single;
begin
  Result := False;
  if RayCastPlaneIntersect(Vector3d(0,0,0),Vector3d(point),Plane,PlaneNormal,iPoint) then
  begin
    aDist := Sqrt(iPoint.Distance(Vector3d(0,0,0)));
    if Distance = -1 then begin
      Distance := aDist;
      nPoint := Point3d(iPoint);
      Result := True;
    end else if aDist < Distance  then begin
      Distance := aDist;
      nPoint := Point3d(iPoint);
      Result := True;
    end;
  end;

end;

function TeAnnulus.FixHeight: Boolean;
begin
  FHeight := 0.001;
  result := True;
end;

procedure TeAnnulus.GetAnnulusPOintsForPosY(PosY: Single; var IPoints,
  OPoints: TPointArray);
var
  I: Integer;
begin
  SetLength(IPoints,Length(InnerPoints));
  SetLength(OPoints,Length(OuterPoints));
  for I := 0 to High(InnerPoints) do
  begin
    IPoints[i] := Point3d(InnerPoints[i].X,PosY,InnerPoints[i].Z);
    OPoints[i] := Point3d(OuterPoints[i].X,PosY,OuterPoints[i].Z);
  end;
end;

procedure TeAnnulus.RebuildMesh;
var iPoints,oPoints:TPointArray;
begin
  CalcPoints;
  Data.VertexBuffer.Length := 0;
  Data.IndexBuffer.Length := 0;
  GetAnnulusPointsForPosY(-0.001,iPoints,oPoints);
  BuildAnnulus(iPoints,oPoints,True);
  GetAnnulusPointsForPosY(0.001,iPoints,oPoints);
  BuildAnnulus(iPoints,oPoints,False);
  Data.CalcFaceNormals;
end;

procedure TeAnnulus.Render;
begin
  {$IFDEF VER260}
  Context.SetMatrix(TMatrix3D.CreateScaling(Point3D(FRenderScale, Height, FRenderScale)));
  {$ELSE}
  Context.SetMatrix(Matrix3DMultiply(CreateScaleMatrix3D(Vector3d(FRenderScale, Height, FRenderScale)),AbsoluteMatrix));
  {$ENDIF}
  Inherited;
  if FDrawBounds then
  begin
    Context.SetMatrix(AbsoluteMatrix);
    Context.DrawCube(Vector3D(0, 0, 0), Vector3D(Width, 0, Depth), AbsoluteOpacity,TalphaColors.Red);
  end;
end;

procedure TeAnnulus.SetDepth(const Value: Single);
var FRefresh:Boolean;
begin
  FRefresh := (Self.Depth <> Value);
  inherited;
  if FRefresh then RebuildMesh;
end;

procedure TeAnnulus.SetDrawBounds(const Value: Boolean);
begin
  FDrawBounds := Value;
  Render;
end;

procedure TeAnnulus.SetHeight(const Value: Single);
begin
  if not FixHeight then inherited;
end;

procedure TeAnnulus.setInnerFrameType(const Value: TFrameType);
begin
  FInnerFrameType := Value;
  RebuildMesh;
end;

procedure TeAnnulus.setOuterFrameType(const Value: TFrameType);
begin
  FOuterFrameType := Value;
  RebuildMesh;
end;

procedure TeAnnulus.SetSectionDegree(const Value: Integer);
begin
  FSectionDegree := Value;
  RebuildMesh;
end;

procedure TeAnnulus.SetSectionType(const Value: TSectionType);
begin
  FSectionType := Value;
  RebuildMesh;
end;

procedure TeAnnulus.SetSubdivisionsAxes(const Value: Integer);
begin
  FSubdivisionsAxes := Value;
  RebuildMesh;
end;

procedure TeAnnulus.SetThickness(const Value: Single);
begin
  FThickness := Value;
  RebuildMesh;
end;

procedure TeAnnulus.SetWidth(const Value: Single);
var FRefresh:Boolean;
begin
  FRefresh := (Self.Width <> Value);
  inherited;
  if FRefresh then RebuildMesh;
end;

{ TePipe }

procedure TePipe.BuildCylinder(Points:TPointArray; Back:Boolean; var SectionPoints,FirstPoints,LastPoints:TPointArray);
var FData:TMeshData;
    i,h,k: Integer;
    vertexIdx,pVertexIdx:Integer;
    indexIdx:Integer;
    hDist,hPos: Single;
    PhiSin, PhiCos: Extended;
    cntIndexInRow:Integer;
    cntVertexInRow:Integer;
    backM:Integer;
    Angle: Single;
    StartPoints:TPointLayer;
    EndPoints:TPointLayer;
    SubPoints:TPointArray;
    done: Boolean;
    PointsLen:Integer;
    pModifier: TPipeModifier;
    pLayer: TPointLayer;
    LayerCount:Integer;
    AbsStart: TPoint3d;
    sctIndex: Integer;
begin
  FData := Self.Data;
  PointsLen := Length(Points);
  StartPoints := TPointlayer.Create;
  if FOnZAxis then StartPoints.RotationAngle.Vector := Vector3d(90,90,0);
  EndPoints := TPointlayer.Create;
  StartPoints.AddChild(EndPoints);

  StartPoints.Length := PointsLen;
  EndPoints.Length := PointsLen;

  StartPoints.Position.Point := Point3d(0,0,0);
  EndPoints.Position.Point := Point3d(0,Height,0);

  for i := 0 to High(Points) do begin
    StartPoints.Points[i] := Point3d(Points[i].X,0,Points[i].Z);
    EndPoints.Points[i] := Point3d(Points[i].X,0,Points[i].Z);
  end;

  backM := 1;
  if back then backM := -1;

  for pModifier in FModifiers do
  begin
    pModifier.DoModify(StartPoints);
  end;

  LayerCount := StartPoints.LayerCount;

  CntIndexInRow := PointsLen*6;
  if FSectionType <> sctNone  then begin
    CntIndexInRow := (PointsLen-1)*6;
  end;

  if FScaleBeforeRender then
  begin
    for i := 0 to LayerCount-1 do
    begin
      pLayer := StartPoints.GetLayer(i);
      pLayer.Content.Scale.Point := Point3d(pLayer.Content.Scale.Point.X*FRenderScale,
                                            pLayer.Content.Scale.Point.Y,
                                            pLayer.Content.Scale.Point.Z*FRenderScale);
    end;
  end;

  AbsStart := Point3d(0,-Height/2,0);
  StartPoints.InvalidateAbsoluteMatrix;
  for i := 0 to LayerCount-1 do
  begin
    VertexIdx := FData.VertexBuffer.Length;
    IndexIdx := FData.IndexBuffer.Length;
    pLayer := StartPoints.GetLayer(i);
    FData.VertexBuffer.Length := VertexIdx + PointsLen;
    for k := 0 to PointsLen-1 do
    begin
      FData.VertexBuffer.Vertices[VertexIdx+k] := pLayer.AbsPoint(k)+AbsStart;
      FData.VertexBuffer.TexCoord0[VertexIdx+k] := PointF(k /(PointsLen-1),pLayer.Position.Y/Height);
    end;

    if (FSectionType <> sctNone) and (not pLayer.GapLayer) then
    begin
      sctIndex := Length(SectionPoints);
      SetLength(SectionPoints,sctIndex+2);
      SectionPoints[sctIndex] := pLayer.AbsPoint(PointsLen-1)+AbsStart;
      SectionPoints[sctIndex+1] := pLayer.AbsPoint(0)+AbsStart;
    end;

    if (i > 0) and (not pLayer.GapLayer) then begin
      FData.IndexBuffer.Length := IndexIdx + CntIndexInRow;
      pVertexIdx := VertexIdx - PointsLen;
      for k := 0 to PointsLen-1 do
      begin
        if k = PointsLen-1  then begin
          if FSectionType = sctNone then
          begin
            FData.IndexBuffer.Indices[IndexIdx+k*6+0] := VertexIdx;
            FData.IndexBuffer.Indices[IndexIdx+k*6+1] := VertexIdx+k;
            FData.IndexBuffer.Indices[IndexIdx+k*6+2] := pVertexIdx+k;

            FData.IndexBuffer.Indices[IndexIdx+k*6+3] := VertexIdx;
            FData.IndexBuffer.Indices[IndexIdx+k*6+4] := pVertexIdx+k;
            FData.IndexBuffer.Indices[IndexIdx+k*6+5] := pVertexIdx;
            if Back then begin
              FData.IndexBuffer.Indices[IndexIdx+k*6+2] := VertexIdx;
              FData.IndexBuffer.Indices[IndexIdx+k*6+1] := VertexIdx+k;
              FData.IndexBuffer.Indices[IndexIdx+k*6+0] := pVertexIdx+k;
              FData.IndexBuffer.Indices[IndexIdx+k*6+5] := VertexIdx;
              FData.IndexBuffer.Indices[IndexIdx+k*6+4] := pVertexIdx+k;
              FData.IndexBuffer.Indices[IndexIdx+k*6+3] := pVertexIdx;
            end;
          end;
        end else begin
          FData.IndexBuffer.Indices[IndexIdx+k*6+0] := VertexIdx+k+1;
          FData.IndexBuffer.Indices[IndexIdx+k*6+1] := VertexIdx+k;
          FData.IndexBuffer.Indices[IndexIdx+k*6+2] := pVertexIdx+k;
          FData.IndexBuffer.Indices[IndexIdx+k*6+3] := VertexIdx+k+1;
          FData.IndexBuffer.Indices[IndexIdx+k*6+4] := pVertexIdx+k;
          FData.IndexBuffer.Indices[IndexIdx+k*6+5] := pVertexIdx+k+1;
          if Back then begin
            FData.IndexBuffer.Indices[IndexIdx+k*6+2] := VertexIdx+k+1;
            FData.IndexBuffer.Indices[IndexIdx+k*6+1] := VertexIdx+k;
            FData.IndexBuffer.Indices[IndexIdx+k*6+0] := pVertexIdx+k;

            FData.IndexBuffer.Indices[IndexIdx+k*6+5] := VertexIdx+k+1;
            FData.IndexBuffer.Indices[IndexIdx+k*6+4] := pVertexIdx+k;
            FData.IndexBuffer.Indices[IndexIdx+k*6+3] := pVertexIdx+k+1;
          end;

        end;
      end;
    end;
  end;

  SetLength(FirstPoints,PointsLen);
  SetLength(LastPoints,PointsLen);
  for i := 0 to StartPoints.Length-1 do
    firstPoints[i] := StartPoints.AbsPoint(i)+AbsStart;
  for i := 0 to EndPoints.Length-1 do
    lastPoints[i] := EndPoints.AbsPoint(i)+AbsStart;

  FFirstCenter := StartPoints.AbsoluteCenter;
  FLastCenter := EndPoints.AbsoluteCenter;

  for pModifier in FModifiers do
  begin
    pModifier.EndModify;
  end;

  StartPoints.Free;
end;


procedure TePipe.BuildSectionSurfaces(OuterSectionPoints,InnerSectionPoints:TPointArray);
var p1,p2: TPoint3d;
    i: Integer;
    FData: TMeshData;
    VertexIdx,IndexIdx,vIdx: Integer;
    LevelCount: Integer;
begin

  FData := Self.Data;

  LevelCount := System.Length(OuterSectionPoints) div 2;

  //left
  VertexIdx := FData.VertexBuffer.Length;
  FData.VertexBuffer.Length := VertexIdx+(LevelCount)*2;
  for I := 0 to LevelCount-1 do
  begin
    p1 := OuterSectionPoints[i*2];
    p2 := InnerSectionPoints[i*2];
    FData.VertexBuffer.Vertices[VertexIdx+i*2+0] := p1;
    FData.VertexBuffer.Vertices[VertexIdx+i*2+1] := p2;
  end;

  IndexIdx := FData.IndexBuffer.Length;
  FData.IndexBuffer.Length := IndexIdx+ (LevelCount-1)*6;
  for i := 0 to LevelCount-2 do
  begin
    vIdx := VertexIdx+i*2+0;
    FData.IndexBuffer[IndexIdx+i*6+0] := vIdx+1;
    FData.IndexBuffer[IndexIdx+i*6+1] := vIdx+2;
    FData.IndexBuffer[IndexIdx+i*6+2] := vIdx+0;
    FData.IndexBuffer[IndexIdx+i*6+3] := vIdx+3;
    FData.IndexBuffer[IndexIdx+i*6+4] := vIdx+2;
    FData.IndexBuffer[IndexIdx+i*6+5] := vIdx+1;
  end;

  //right
  VertexIdx := FData.VertexBuffer.Length;
  FData.VertexBuffer.Length := VertexIdx+(LevelCount)*2;
  for I := 0 to LevelCount-1 do
  begin
    p1 := OuterSectionPoints[i*2+1];
    p2 := InnerSectionPoints[i*2+1];
    FData.VertexBuffer.Vertices[VertexIdx+i*2+0] := p1;
    FData.VertexBuffer.Vertices[VertexIdx+i*2+1] := p2;
  end;

  IndexIdx := FData.IndexBuffer.Length;
  FData.IndexBuffer.Length := IndexIdx+ (LevelCount-1)*6;
  for i := 0 to LevelCount-2 do
  begin
    vIdx := VertexIdx+i*2+0;
    FData.IndexBuffer[IndexIdx+i*6+0] := vIdx+0;
    FData.IndexBuffer[IndexIdx+i*6+1] := vIdx+2;
    FData.IndexBuffer[IndexIdx+i*6+2] := vIdx+1;
    FData.IndexBuffer[IndexIdx+i*6+3] := vIdx+1;
    FData.IndexBuffer[IndexIdx+i*6+4] := vIdx+2;
    FData.IndexBuffer[IndexIdx+i*6+5] := vIdx+3;
  end;

end;


procedure TePipe.ClearModifiers;
var pModifier:TPipeModifier;
begin
  for pModifier in Self.FModifiers do
    pModifier.Free;
  FModifiers.Clear;
end;

constructor TePipe.Create(AOwner: TComponent);
begin
  inherited;
  Self.TwoSide := True;
  FModifiers := TList<TPipeModifier>.Create;
  FModifiers.OnNotify := ModifiersNotify;
  FOnZAxis := False;
  FScaleBeforeRender := False;
  RebuildMesh;
end;

destructor TePipe.Destroy;
var pModifier:TpipeModifier;
begin
  ClearModifiers;
  FModifiers.Free;
  inherited;
end;

function TePipe.FixHeight: Boolean;
begin
  result := false;
end;

procedure TePipe.GetAnnulusPointsForPosY(PosY: Single; var IPoints,
  OPoints: TPointArray);
var
  I: Integer;
begin
  SetLength(IPoints,Length(InnerPoints));
  SetLength(OPoints,Length(OuterPoints));
  for I := 0 to High(InnerPoints) do
  begin
    IPoints[i] := Point3d(InnerPoints[i].X,PosY,InnerPoints[i].Z);
    OPoints[i] := Point3d(OuterPoints[i].X,PosY,OuterPoints[i].Z);
  end;
end;

procedure TePipe.ModifiersNotify(Sender:Tobject;Const Item:TPipeModifier;Action: TCollectionNotification);
begin
  SortModifiers;
  RebuildMesh;
end;

procedure TePipe.RebuildMesh;
var OuterSectionPoints,InnerSectionPoints:TPointArray;
    InnerFirstPoints,InnerLastPoints,OuterFirstPoints,OuterLastPoints:TPointArray;
begin
  if FModifiers = nil then exit;
  CalcPoints;
  Data.VertexBuffer.Length := 0;
  Data.IndexBuffer.Length := 0;
  BuildCylinder(InnerPoints,True,InnerSectionPoints,InnerFirstPoints,InnerLastPoints);
  BuildCylinder(OuterPoints,False,OuterSectionPoints,OuterFirstPoints,OuterLastPoints);
  if FSectionType <> sctNone then BuildSectionSurfaces(OuterSectionPoints,InnerSectionPoints);
  BuildAnnulus(innerLastPoints,outerLastPoints,True);
  BuildAnnulus(innerFirstPoints,outerFirstPoints,False);
  Data.CalcFaceNormals;
end;

procedure TePipe.Render;
begin
  if not FScaleBeforeRender then
    Context.SetMatrix(Matrix3DMultiply(CreateScaleMatrix3D(Vector3D(FRenderScale, 1, FRenderScale)), AbsoluteMatrix));
  Inherited;
end;

procedure TePipe.SetHeight(const Value: Single);
var FRefresh:Boolean;
begin
  FRefresh := (Self.Height <> Value);
  inherited;
  if FRefresh then RebuildMesh;
end;




procedure TePipe.SetOnZAxis(const Value: Boolean);
begin
  FOnZAxis := Value;
  RebuildMesh;
end;

procedure TePipe.SetScaleBeforeRender(const Value: Boolean);
begin
  FScaleBeforeRender := Value;
  RebuildMesh;
end;

function CompareLevels(Item1, Item2: TPipeModifier): Integer;
begin
  result := 0;
  if TPipeModifier(Item1).StartPosition >  TpipeModifier(Item2).StartPosition then
  begin
    result := 1;
  end else if TPipeModifier(Item1).StartPosition < TpipeModifier(Item2).StartPosition then
  begin
    result := -1;
  end;
end;


procedure TePipe.SortModifiers;
var Comparer: IComparer<TPipeModifier>;
begin
  Comparer := TDelegatedComparer<TpipeModifier>.Create(

    function(const Left, Right: TpipeModifier): Integer
    begin
      result := Ceil(Left.StartPosition -  Right.StartPosition);
      if (result = 0) and (left is TTwistModifier) then result := 1;
    end);

  Fmodifiers.Sort(Comparer);
end;

{ TPipeModifier }


Function TPipeModifier.InsertPointLayer(StartLayer:TPointLayer;layerH:Single;UseGap:Boolean=False):TPointLayer;
var lParent: TPointLayer;
    FLayer:TPointLayer;
begin
  result := nil;
  FLayer := StartLayer;
  repeat
    if abs(layerH - FLayer.LayerH) < 0.00001 then begin
      result := FLayer;
      if UseGap then begin
        result := Result.CreateChildAtPosition(Point3d(0,0,0),1);
        Result.GapLayer := True;
      end;
    end else if (FLayer.LayerH > layerH) then
    begin
      if assigned(FLayer.RealParent) then
      begin
        lParent := FLayer.RealParent;
        result := lParent.CreateChildAtPosition(Point3d(0,layerH-lParent.LayerH,0),1);
        if UseGap then begin
          result := Result.CreateChildAtPosition(Point3d(0,0,0),1);
          Result.GapLayer := True;
        end;
      end;
    end else if (result = nil) and (FLayer.RealChild = nil) then
    begin
      result := FLayer.CreateChildAtPosition(Point3d(0,layerH-FLayer.LayerH,0),1);
      if UseGap then begin
        result := Result.CreateChildAtPosition(Point3d(0,0,0),1);
        Result.GapLayer := True;
      end;
    end;
    FLayer := FLayer.RealChild;
  until (result <> nil) or (FLayer = nil) ;
end;

procedure TPipeModifier.BeginModify(StartPoints:TPointLayer);
var
  i: Integer;
  FLayer:TPointLayer;
  mLen,dLen: Single;
  h1,h2,dh: Single;
  sCnt,k: Integer;
  tempList: Tlist<TPointLayer>;
  divCount: Integer;
begin

  StartLayer := InsertPointLayer(StartPoints,FStartPosition,FUseGap);
  EndLayer := InsertPointLayer(StartPoints,FEndPosition,FUseGap);

  StartMLayer := nil;
  EndMLayer := nil;

  divCount := (FSubdivisions+1);
  if (FStartMargin > 0) then begin
    StartMLayer := InsertPointLayer(StartPoints,FStartPosition+FStartMargin);
    divCount := divCount-1;
  end;
  if (FEndMargin > 0) then begin
    EndMLayer := InsertPointLayer(StartPoints,FEndPosition-FEndMargin);
    divCount := divCount-1;
  end;

  mLen := Self.EndPosition-Self.StartPosition-(FEndMargin+FStartMargin);
  dLen := mLen/divCount;

  if assigned(StartLayer) and assigned(EndLayer)  then
  begin
    tempList := TList<TPointLayer>.Create;
    FLayer := StartLayer;
    if assigned(StartMLayer) then FLayer := StartMLayer;
    repeat
      tempList.Add(FLayer);
      FLayer := Flayer.RealChild;
    until  (FLayer = EndLayer) or (FLayer = EndMLayer);
    if assigned(FLayer) then tempList.Add(FLayer);
    for i := 0 to templist.Count -2 do
    begin
      h1 := templist[i].LayerH;
      h2 := templist[i+1].LayerH;
      sCnt := Round((h2-h1)/dLen);
      if scnt > 1 then begin
        dh := (h2-h1)/scnt;
        templist[i].CreateChildAtPosition(Point3d(0,dh,0),sCnt-1);
      end;
    end;
    FLayerCount := EndLayer.Index - StartLayer.Index +1;
    Templist.Free;
  end;
end;

constructor TPipeModifier.Create(aPipe: TePipe);
begin
  inherited Create(aPipe);
  FPipe := aPipe;
  FSubDivisions := 10;
  FStartPosition := -FPipe.Height/4;
  FEndPosition := FPipe.Height/4;
  FStartMargin := 0;
  FEndMargin := 0;
  FModifyMargins := False;
end;

procedure TPipeModifier.DoModify(StartPoints:TPointLayer);
var FLayer: TPointLayer;
begin
  if (FStartPosition > FEndPosition) then exit;
  if (FStartPosition = FEndPosition) then exit;
  BeginModify(StartPoints);
  if (not assigned(StartLayer)) or (not assigned(EndLayer)) then
    raise Exception.Create('Modifier Position Indexes cant be arranged');
  FLayer := StartLayer;
  if (not fModifyMargins)  and assigned(StartMLayer) then FLayer := StartMLayer;
  self.ModifySubPoints(FLayer,False);
  repeat
    FLayer := FLayer.RealChild;
    if assigned(FLayer) then self.ModifySubPoints(FLayer,False);
  until (Flayer = nil) or ((FLayer = EndMLayer) and (not fModifyMargins)) or (FLayer = EndLayer);
end;

procedure TPipeModifier.EndModify;
begin
  if assigned(StartLayer) then FFirstCenter := StartLayer.AbsoluteCenter;
  if assigned(EndLayer) then FLastCenter := EndLayer.AbsoluteCenter;
end;

procedure TPipeModifier.SetEndPosition(const Value: Single);
begin
  FEndPosition := Value;
  FPipe.RebuildMesh;
end;

procedure TPipeModifier.SetModifyMargins(const Value: Boolean);
begin
  FModifyMargins := Value;
  FPipe.RebuildMesh;
end;

procedure TPipeModifier.SetStartPosition(const Value: Single);
begin
  FStartPosition := Value;
  FPipe.RebuildMesh;
end;

procedure TPipeModifier.SetSubdivisions(const Value: Integer);
begin
  FSubdivisions := Value;
  FPipe.RebuildMesh;
end;

{ TBendModifier }


constructor TBendModifier.Create(aPipe: TePipe);
begin
  inherited;
  FEndPosition := FPipe.Height/4;
  FBendAngle := 90;
  FTurnAngle := 0;
end;


destructor TBendModifier.Destroy;
begin

  inherited;
end;

procedure TBendModifier.ModifySubPoints(sPoints:TPointLayer;isInner:Boolean);
var
  Index: Integer;
  FCurrentBendAngle: Single;
begin
  FCurrentBendAngle := (FBendAngle/(FlayerCount-1));
  Index := SPoints.Index;
  if sPoints = StartLayer then begin
    sPoints.DummyChild.RotationAngle.Z := FCurrentBendAngle/2;
    sPoints.RotationAngle.Y := FTurnAngle;
  end else if (Index > StartLayer.Index) and (Index <= EndLayer.Index) then begin
    sPoints.RotationAngle.Z := FCurrentBendAngle/2;
    if sPoints <> EndLayer then
    begin
      sPoints.DummyChild.RotationAngle.Z := FCurrentBendAngle/2;
    end;
  end;
end;


procedure TBendModifier.SetBendAngle(const Value: Single);
begin
  FBendAngle := Value;
  FPipe.RebuildMesh;
end;

procedure TBendModifier.SetTurnAngle(const Value: Single);
begin
  FTurnAngle := Value;
  FPipe.RebuildMesh;
end;

{ TTwistModifier }

constructor TTwistModifier.Create(aPipe: TePipe);
begin
  inherited;
  FTotalRotation := 45;
end;

procedure TTwistModifier.ModifySubPoints(sPoints: TPointLayer;
  isInner: Boolean);
var
  ya: Single;
  totalH,thisH:Single;
  cIndex,sIndex,eIndex: Integer;
begin
  sIndex := StartLayer.Index;
  cIndex := sPoints.Index;
  eIndex := EndLayer.Index;
  if (cIndex > SIndex) and (cIndex <= EIndex) then
  begin
    TotalH := FEndPosition-FStartPosition;
    thisH := sPoints.GetLayerH-FStartPosition;
    ya := (FTotalRotation / totalH)*thisH;
    sPoints.Content.RotationAngle.Y := ya;
  end;
end;

procedure TTwistModifier.SetTotalRotation(const Value: Single);
begin
  FTotalRotation := Value;
  FPipe.RebuildMesh;
end;

{ TPointLayer }
function TPointLayer.AbsPoint(i: Integer): TPoint3d;
var tTurn: Single;
begin
  tTurn := GetTotalTurn;
  FContent.FContent.RotationAngle.Y := -TTurn;
  Result := Point3d(Vector3dTransform(Vector3d(Points[i]),FContent.FContent.AbsoluteMatrix));
end;

procedure TPointLayer.AddChild(CPointLayer: TPointLayer);
var FOldChild:TPointLayer;
begin
  FOldChild := FChild.FChild;
  Self.FChild.FChild := CPointLayer;
  CPointLayer.FParent := Self.FChild;
  if assigned(FOldChild) then
  begin
    CPointLayer.LastChild.AddChild(FOldChild);
  end;
end;

function TPointLayer.Content: TPointLayer;
begin
  result := FContent;
end;

constructor TPointLayer.Create;
begin
  inherited;
  FLocalMatrix := IdentityMatrix3D;
  FQuaternion := IdentityQuaternion;
  FPosition := TPosition3d.Create(Point3d(0,0,0));
  FPosition.OnChange := MatrixChanged;
  FRotationAngle := TPosition3d.Create(Point3d(0,0,0));
  FRotationAngle.OnChange := RotationChanged;
  FScale := TPosition3D.Create(Point3D(1, 1, 1));
  FScale.OnChange := MatrixChanged;
  FAbsMatrixNeedRefresh := True;
  FGapLayer := False;
  CreateDummies;
end;

function TPointLayer.CreateChildAtPosition(CPos: TPoint3d; RepeatNbr:Integer): TPointLayer;
var i: Integer;
begin
  result := TPointLayer.Create;
  Result.Length := Self.Length;
  for i := 0 to Length-1 do
    Result.Points[i] := Self.Points[i];
  Result.Position.Point := CPos;
  if assigned(FChild.FChild) then
  begin
    FChild.FChild.Position.Point := FChild.FChild.Position.Point - CPos;
  end;
  Self.AddChild(Result);
  RepeatNbr := RepeatNbr-1;
  if RepeatNbr > 0 then
    Result := Result.CreateChildAtPosition(CPos,RepeatNbr);
end;

procedure TPointLayer.CreateDummies;
var FContentContent: TPointLayer;
begin
  FChild := TDummyPointLayer.Create;
  FChild.FParent := Self;
  FContent := TDummyPointLayer.Create;
  FContent.FParent := Self;
  FContentContent := TDummyPointLayer.Create;
  FContentContent.FParent := FContent;
  FContent.FContent := FContentContent;
end;

destructor TPointLayer.Destroy;
begin
  FreeAndNil(FChild);
  FreeAndNil(FRotationAngle);
  FreeAndNil(FScale);
  FreeAndNil(FPosition);
  FreeAndNil(FContent);
  inherited;
end;

function TPointLayer.GeAbsoluteCenter: TPoint3d;
var tTurn: Single;
begin
  tTurn := GetTotalTurn;
  FContent.FContent.RotationAngle.Y := -TTurn;
  Result := Point3d(Vector3dTransform(Vector3d(0,0,0),FContent.FContent.AbsoluteMatrix));
end;

function TPointLayer.GetAbsoluteMatrix: TMatrix3D;
begin
  if not FAbsMatrixNeedRefresh then begin
    Result := FSavedAbsoluteMatrix;
  end else begin
    if Assigned(FParent) and (FParent is TPointLayer) then
      Result := Matrix3DMultiply(FLocalMatrix, TPointLayer(FParent).AbsoluteMatrix)
    else
      Result := FLocalMatrix;
    FSavedAbsoluteMatrix := Result;
    FAbsMatrixNeedRefresh := False;
  end;
end;


function TPointLayer.GetDummyChild: TPointLayer;
begin
  if assigned(FChild) and (FCHild is TDummyPointLayer) then result := FChild;
end;

function TPointLayer.GetFirstParent: TPointLayer;
begin
  result := Self;
  if assigned(FParent.FParent) then
  begin
    result := FParent.FParent.FirstParent;
  end;
end;

function TPointLayer.GetLayer(LIndex: Integer): TPointLayer;
begin
  if LIndex = 0 then result := Self
  else if assigned(FChild.FChild) and (LIndex > 0) then begin
    result := FChild.FChild.GetLayer(LIndex-1);
  end else result := nil;
end;

function TPointLayer.GetLayerCount: Integer;
begin
  result := 1;
  if assigned(FChild.FChild) then result := 1 + FChild.FChild.LayerCount;
end;

function TPointLayer.GetLayerH: Single;
begin
  result := Self.Position.Y;
  if assigned(RealParent) then result := result+realParent.GetLayerH;
end;

function TPointLayer.GetLength: Integer;
begin
  result := System.Length(Points);
end;

function TPointLayer.GetRealChild: TPointLayer;
begin
  Result := FChild.FChild;
end;

function TPointLayer.GetRealParent: TPointLayer;
begin
  Result := nil;
  if Assigned(FParent) then result :=  FParent.FParent;
end;

function TPointLayer.GetTotalTurn: Single;
begin
  result := RotationAngle.Y;
  if assigned(FParent) then result := result+FParent.GetTotalTurn;
end;

function TPointLayer.Index: Integer;
begin
  result := 0;
  if assigned(Fparent) and assigned(FParent.FParent) then result := 1+FParent.FParent.Index;
end;

procedure TPointLayer.InvalidateAbsoluteMatrix;
begin
  FAbsMatrixNeedRefresh := True;
  if assigned(FChild) then FChild.InvalidateAbsoluteMatrix;
end;

function TPointLayer.LastChild: TPointLayer;
begin
  Result := Self;
  if assigned(FChild.FChild) then result := FChild.FChild.LastChild;
end;

procedure TPointLayer.MatrixChanged(Sender: TObject);
var
  LeftVector, DirectionVector, UpVector: TVector3D;
  RotMatrix: TMatrix3D;
begin
  UpVector := Vector3D(0, 1, 0);
  DirectionVector := Vector3D(0, 0, 1);
  if (FRotationAngle.X <> 0) or (FRotationAngle.Y <> 0) or (FRotationAngle.Z <> 0) then
  begin
    rotMatrix := QuaternionToMatrix(FQuaternion);
    UpVector := Vector3DTransform(UpVector, RotMatrix);
    DirectionVector := Vector3DTransform(DirectionVector, RotMatrix);
  end else
  begin
    FQuaternion := IdentityQuaternion;
  end;
  LeftVector := UpVector.CrossProduct(DirectionVector);
  FLocalMatrix.M[0] := LeftVector.Scale(FScale.X);
  FLocalMatrix.m14 := 0;
  FLocalMatrix.M[1] := UpVector.Scale(FScale.Y);
  FLocalMatrix.m24 := 0;
  FLocalMatrix.M[2] := DirectionVector.Scale(FScale.Z);
  FLocalMatrix.m34 := 0;
  FLocalMatrix.m41 := FPosition.X;
  FLocalMatrix.m42 := FPosition.Y;
  FLocalMatrix.m43 := FPosition.Z;
  FAbsMatrixNeedRefresh := True;
end;

Function TPointLayer.RemoveFirstChild:TPointLayer;
begin
  if assigned(FChild.FChild) then begin
    result :=  FChild.FChild;
    FChild.FChild := nil;
    if assigned(result.FChild.FChild) then begin
      Self.AddChild(result.FChild.FChild);
      result.FChild.FChild := nil;
    end;
  end;
end;

procedure TPointLayer.RotationChanged(Sender: TObject);
var
  q: TQuaternion3D;
  a: Single;
  NeedChange: Boolean;
  v: TVector3d;
begin
  NeedChange := False;
  FQuaternion := IdentityQuaternion;

  a := DegNormalize(RotationAngle.X);
  if a <> 0 then
  begin
    q := QuaternionFromAngleAxis(a, Vector3D(1, 0, 0) { AbsoluteRight } );
    FQuaternion := QuaternionMultiply(FQuaternion, q);
  end;
  a := DegNormalize(RotationAngle.Y);
  if a <> 0 then
  begin
    q := QuaternionFromAngleAxis(a, Vector3D(0, 1, 0) { AbsoluteDirection } );
    FQuaternion := QuaternionMultiply(FQuaternion, q);
  end;
  a := DegNormalize(RotationAngle.Z);
  if a <> 0 then
  begin
    q := QuaternionFromAngleAxis(a, Vector3D(0, 0, 1) { AbsoluteUp } );
    FQuaternion := QuaternionMultiply(FQuaternion, q);
  end;
  MatrixChanged(Sender);
end;

procedure TPointLayer.SetPointsLength(const Value: Integer);
begin
  Setlength(Points,Value);
end;

{ TEmbossModifier }

constructor TEmbossModifier.Create(aPipe: TePipe);
begin
  inherited;
  FStartMargin := 0.02;
  FEndMargin := 0.02;
  FThicknessRatio := 0.1;
end;

procedure TEmbossModifier.ModifySubPoints(sPoints: TPointLayer;
  isInner: Boolean);
begin
  SPoints.Content.Scale.Point := Point3d(1+FThicknessRatio,0,1+FThicknessRatio);
end;

procedure TEmbossModifier.SetThicknessRatio(const Value: Single);
begin
  FThicknessRatio := Value;
  FPipe.RebuildMesh;
end;

{ TDummyPointLayer }

procedure TDummyPointLayer.CreateDummies;
begin
  // Do Nothing
end;

{ TBreakModifier }


constructor TBreakModifier.Create(aPipe: TePipe);
begin
  inherited;
  FModifyMargins := True;
end;

procedure TBreakModifier.ModifySubPoints(sPoints: TPointLayer;
  isInner: Boolean);
var
  Index: Integer;
  FCurrentBendAngle: Single;
  elpR: Single;
begin
  FCurrentBendAngle := (FBendAngle/(FlayerCount-2));
  FCurrentBendAngle := FCurrentBendAngle/2;
  elpR := 1 / cos((FCurrentBendAngle)*(pi/180));
  Index := SPoints.Index;
  if (Index > StartLayer.Index) and (Index < EndLayer.Index) then begin
    sPoints.RotationAngle.Z := FCurrentBendAngle;
    sPoints.Content.Scale.Point := Point3d(elpR,1,1);
    sPoints.DummyChild.RotationAngle.Z := FCurrentBendAngle;
  end;
end;

procedure TBreakModifier.SetEndMargin(const Value: Single);
begin
  FEndMargin := Value;
  FPipe.RebuildMesh;
end;

procedure TBreakModifier.SetStartMargin(const Value: Single);
begin
  FStartMargin := Value;
  FPipe.RebuildMesh;
end;

initialization
  RegisterFmxClasses([TPipeModifier,TBendModifier,TTwistModifier,TEmbossModifier]);

end.
