unit fmainGrapher;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.Controls3D, FMX.MaterialSources,
  FMX.Layers3D,
  FmeX.Types3D,
  FMX.Objects3D,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TForm23 = class(TForm3D)
    Layer3D1: TLayer3D;
    Camera1: TCamera;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Button1: TButton;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    procedure Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Form3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    ll : TeCustomMesh;
    mx, my : single;
    { Public declarations }
  end;

var
  Form23: TForm23;

implementation

uses FMeX.Types3D.GeometryCommon;

{$R *.fmx}

procedure TForm23.Button1Click(Sender: TObject);
var  ltool : TFMeXBasedAngleShapeGenerator;
  I: Integer;
begin
  for I := 0 to ChildrenCount-1 do if Children[i] is TeCustomMesh then RemoveObject(i);

  ll := TeCustomMesh.Create(nil);
  ltool := TFMeXBasedAngleShapeGenerator.Create;
  try
    ltool.Process(ll.Data,Round(TrackBar1.Value),4);
  finally
    freeAndNil(ltool);
  end;
  AddObject(ll);
  ll.MaterialSource := LightMaterialSource1;
end;

procedure TForm23.Form3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  mx := x;
  my := y;
end;

procedure TForm23.Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if ssLeft in Shift then
  begin
    ll.RotationAngle.Point := Point3D(x-mx,y+my,0);
  end;
end;

end.
