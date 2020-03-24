unit Voxels.fmain;

///
///  WORK IN PROGRESS : Gizmo is buggy.
///

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs,
  System.Math.Vectors,
  System.Math,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.MaterialSources,
  FMX.Menus,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layers3D,

  FMeX.Types3d,
  GS.Math.PerlinNoise,
  Voxels.Controler;

type

  TForm1 = class(TForm3D)
    Camera1: TCamera;
    DummyXY: TDummy;
    DummyYZ: TDummy;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    DummyRoot: TDummy;
    Layer3D1: TLayer3D;
    Button1: TButton;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Form3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button1Click(Sender: TObject);
    procedure Form3DCreate(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
  private
    { Private declarations }
  public
//   MyCursor : TCursor3D;
   mapvox : integer;
    { Public declarations }


   procedure Update(Sender: TObject);


  end;

const
  CST_DEFAULT_MAP_SIZE = 32;
/// Dev Note : Memory issue  : Note about VOXLEN = 128 :
/// Vertexbuffer.length is integer based memory allocation. 128^3 * cubeVertices * CurrentVertexSize > SizeOf(integer) :(
///  Solution : Introduce Voxel Chunck. TODO

Type

  TVoxel = Record
    enabled : boolean;
    color : TAlphaColor;
  end;

  TVoxelArray = Array of Array of Array of TVoxel;

  TVoxelWorld = class
  protected
    CubeCount : Integer;
    Voxels : TVoxelArray;
    Function Build : TeCustomMesh;

    constructor create(mapSize : integer = CST_DEFAULT_MAP_SIZE); virtual;
  end;

  TVoxelGenerator = class
  public
    class procedure GenerateFullRandom(world : TVoxelWorld; const CubeGenerationPercentage : Byte = 50);
    class procedure GeneratePerlinNoise(world : TVoxelWorld; const noisescale : integer = 11; const maxDensity : integer = -4);
  end;

  TVoxelDrawer = class
  protected
  public
//    procedure voxel(x,y,z : integer);
//    procedure plan(x,y,x1,y1 : integer);
  end;



  ///
  ///
  ///
  ///
  ///
  ///
  ///
  ///
  ///
  ///

var
  Form1: TForm1;
  mb,mbr : TPointf;

implementation

{$R *.fmx}



procedure TForm1.Button1Click(Sender: TObject);
var v : TVoxelWorld;
    l : TeCustomMesh;
begin
  v := TVoxelWorld.Create(mapvox);
  try
    //TVoxelGenerator.GenerateFullRandom(v,30);
    TVoxelGenerator.GeneratePerlinNoise(v);
    l := v.Build;
    caption := format('FMX Voxels : %d  cubes %d Tri ',[v.CubeCount,v.CubeCount*12]);
  finally
    FreeAndNil(v);
  end;
  l.MaterialSource := LightMaterialSource1;
  DummyRoot.AddObject(l);

//  MyCursor.SetNewClient(l);
end;

procedure TForm1.Form3DCreate(Sender: TObject);
var F : TFMXObject;
begin
// for F in Children do
//   F.SetDesign(True);
// MyCursor := TCursor3D.Create(Nil);
// MyCursor.Parent := Self;
// MyCursor.OnTracking := Update;    // Link
 mapvox := 8;
end;

procedure TForm1.Form3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  mb := pointf(x,y);
  mbr := pointf(DummyXY.RotationAngle.X, DummyYZ.RotationAngle.Y);
end;

procedure TForm1.Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if ssRight in shift then
  begin
    DummyXY.RotationAngle.X := mbr.x - (y - mb.Y)/5;
    DummyYZ.RotationAngle.Y := mbr.y + (x - mb.x)/5;
  end;
end;

procedure TForm1.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta/10;
end;

procedure TForm1.RadioButton1Change(Sender: TObject);
begin
  mapvox := TRadioButton(Sender).Tag;
end;

procedure TForm1.Update(Sender: TObject);
var
 P, W: TPoint3D;
 s: String;
begin
 with TControl3D(Sender) do begin
   P := Position.Point;
   W := RotationAngle.Point;
   s := Name;
 end;

{
 lbCoord.Text := 'Pos: ' + 'X: ' + FloatToStrF(P.X, ffFixed, 7, 2) + Space +
                           'Y: ' + FloatToStrF(P.Y, ffFixed, 7, 2) + Space +
                           'Z: ' + FloatToStrF(P.Z, ffFixed, 7, 2) + Space + Space +
                 'Ang: ' + 'X: ' + FloatToStrF(W.X, ffFixed, 7, 2) + Space +
                           'Y: ' + FloatToStrF(W.Y, ffFixed, 7, 2) + Space +
                           'Z: ' + FloatToStrF(W.Z, ffFixed, 7, 2);
}
end;



{ TVoxelWorld }

function TVoxelWorld.Build: TeCustomMesh;
var x,y,z : integer;
    c : TeCube;
    ccount : Integer;
    voxlen : Integer;
    offset : Integer;

begin
  CubeCount := 0;

  //Calculate number of voxel.
  voxlen := length(voxels)-1;
  ccount := 0;
  for x := 0 to voxlen do
  for y := 0 to voxlen do
  for z := 0 to voxlen do
    if Voxels[x,y,z].enabled then
      inc(ccount);

  //One cube : 24 vertices. 36 Indices.
  result := TeCustomMesh.Create(nil);
  result.Data.VertexBuffer.Length := 24*ccount;
  result.Data.IndexBuffer.Length := 36*ccount;

  offset := Round((voxlen+1) / 2);
  c := TeCube.Create(nil);
  try
    for x := 0 to voxlen do
    for y := 0 to voxlen do
    for z := 0 to voxlen do
    begin
      if Voxels[x,y,z].enabled then
      begin
        c.Position.Point := Point3D(x - offset,y - offset,z - offset);
        result.MergeFrom(c,true,false);
        inc(CubeCount);
      end;
    end;
  finally
    freeAndNil(c);
  end;
  result.ProcessBoundingBox;
end;

constructor TVoxelWorld.create(mapSize: integer);
var x,y : integer;
begin
  inherited create;
  setLength(voxels,mapSize); //x
  for x := 0 to mapSize-1 do
  begin
    setLength(voxels[x],mapSize); //all y
    for y := 0 to mapSize-1 do
      setLength(voxels[x][y],mapSize); //all z
  end;
end;

{ TVoxelGenerator }

class procedure TVoxelGenerator.GenerateFullRandom(world: TVoxelWorld; const CubeGenerationPercentage : Byte);
var x,y,z : integer;
    mapSize : integer;
begin
  Assert(assigned(world));
  Randomize;
  mapSize := length(world.Voxels);
  for x := 0 to mapSize-1 do
  for y := 0 to mapSize-1 do
  for z := 0 to mapSize-1 do
  begin
    world.Voxels[x,y,z].enabled := Random(100) >= 100-CubeGenerationPercentage;
  end;
end;

class procedure TVoxelGenerator.GeneratePerlinNoise(world: TVoxelWorld; const noisescale : integer; const maxDensity : integer);
var seed : single;
    amp : integer;
    x,y,z : integer;
    xnoise,ynoise,znoise : double;
    density : double;
    mapSize : Integer;
begin
  Randomize;
  seed := randomrange(-1000000000, 1000000000) mod randomrange(-1000000000, 1000000000);
  amp := 20;
  mapSize := length(world.Voxels)-1;
  for x := 0 to mapSize do
  for y := 0 to mapSize do
  for z := 0 to mapSize do
  begin
    xnoise := noise(y / NoiseScale, z / NoiseScale, Seed) * Amp;
    ynoise := noise(x / NoiseScale, z / NoiseScale, Seed) * Amp;
    znoise := noise(x / NoiseScale, y / NoiseScale, Seed) * Amp;

    density := Xnoise + Ynoise + Znoise;

    world.Voxels[x,y,z].enabled :=  density<maxDensity;
  end;
end;





end.
