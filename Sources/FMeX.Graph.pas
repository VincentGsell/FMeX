unit FMeX.Graph;


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
  FMeX.Gx.Types;

Type


TFMeXCustomGraph = Class;
TFMeXGraphFor2D = Class; //This a specialized Graph for 2d : Managing object in a list and rendering them sequentially.
//TFMeXGraphForFPS, TFMeXGraphForShootemUp... To dig...;)

//This object is not intended to be displayed :
// -> It is just an entry point for display teControl3D in FMX
// -> It drive the rendering throught "graph" object.
TFMeXProxy = Class(TDummy)
Private
  FGraph: TFMeXCustomGraph;
  procedure SetGraph(const Value: TFMeXCustomGraph);
Public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;

  Procedure Render; Override;

  Property Graph : TFMeXCustomGraph read FGraph Write SetGraph;
End;

TFMeXCustomGraph = Class(TComponent)
private
  FProxy: TFMeXProxy;
Public
  Procedure ProcessGraph; Virtual; Abstract;

  function add(aCtrl : TFMeXGraph3D) : TFMeXGraph3D; virtual; Abstract;

  property Proxy : TFMeXProxy read FProxy Write FProxy;
End;

TFMeXGraphFor2D = Class(TFMeXCustomGraph)
Private
  FObjLst : TObjectList<TFMeXGraph3D>;
    function GetObj(Index: Uint32): TFMeXGraph3D;
Public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; Override;

  function add(aCtrl : TFMeXGraph3D) : TFMeXGraph3D; Override;

  Procedure ProcessGraph; Override;

  property Obj[Index : Uint32] : TFMeXGraph3D read GetObj;

End;


implementation

{ TFMeXProxy }
constructor TFMeXProxy.Create(AOwner: TComponent);
begin
  inherited;
  FGraph := nil;
  HitTest := False;
end;

destructor TFMeXProxy.Destroy;
begin
  inherited;
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


{ TFMeXGraphFor2D }

function TFMeXGraphFor2D.add(aCtrl: TFMeXGraph3D): TFMeXGraph3D;
begin
  assert(assigned(aCtrl));
  result := aCtrl;
  FObjLst.Add(aCtrl);
end;

constructor TFMeXGraphFor2D.Create(AOwner: TComponent);
begin
  inherited;
  FObjLst := TObjectList<TFMeXGraph3D>.Create;
end;

destructor TFMeXGraphFor2D.Destroy;
begin
  FObjLst.Free;
  inherited;
end;

function TFMeXGraphFor2D.GetObj(Index: Uint32): TFMeXGraph3D;
begin
  result := FObjLst[Index];
end;

procedure TFMeXGraphFor2D.ProcessGraph;
var a : TFMeXGraph3D;
begin
  Assert(Assigned(Proxy));

  //We are in 3d mode, but render in flat 2d : Desactive Z test.
  Proxy.Context.SetContextState(TContextState.csZWriteOff);
  Proxy.Context.SetContextState(TContextState.csZTestOff);

  //Object are render in their strict list sequence.
  for a in FObjLst do
  begin
    TFMeXGraph3D(a).Render(Proxy.Context,Proxy);
  end;
end;



end.
