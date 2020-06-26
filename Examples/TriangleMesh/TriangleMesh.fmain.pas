unit TriangleMesh.fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMeX.Types3D, System.Math.Vectors, FMX.Objects3D, FMX.Controls3D,
  FMX.Viewport3D, FMX.Types3D, FMX.MaterialSources,
  GS.Soft3D.Types,
  GS.Soft3d, FMX.Objects, FMX.Layers3D;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Camera1: TCamera;
    TextureMaterialSource1: TTextureMaterialSource;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    Timer1: TTimer;
    Dummy1: TDummy;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  FMeX.Types3D.GSCoreBridge;

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var l : TMesh3d;

   function addMe(a : TMesh3d) : TeCustomMesh;
   begin
     result := TeCustomMesh.Create(nil);

     TMeshTransfert.TransfertGSCoreMesh3DToFMeX(l,result);

     result.Data.CalcSmoothNormals(TMeshData.TCalculateNormalMethod.Slowest);
     result.MaterialSource := LightMaterialSource1;

     result.DrawFrameType := TeCustomMeshDrawType.mdtFull;
     Viewport3D1.AddObject(result);
   end;

begin
  l := TGSCubeMesh.Create;
  addMe(l).Position.Point := Point3D(0,0,0);
  l := TGSPlaneTriMesh.Create;
  addMe(l).Position.Point := Point3D(-2,0,0);
  l := TGSPlaneMesh.Create;
  addMe(l).Position.Point := Point3D(2,0,0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i : integer;
    m : TeCustomMesh;
begin
  for i := 0 to Viewport3D1.ChildrenCount-1 do
  begin
    if Viewport3D1.Children[i] is TeCustomMesh then
    begin
      m := TeCustomMesh(Viewport3D1.Children[i]);
      m.RotationAngle.x := m.RotationAngle.X +1;
      m.RotationAngle.y := m.RotationAngle.y +2;
      m.RotationAngle.z := m.RotationAngle.Z +1;
    end;
  end;
end;

procedure TForm1.Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z - WheelDelta/100;
end;

end.
