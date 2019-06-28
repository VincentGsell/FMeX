unit fmainloader;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Controls3D, FMX.Layers3D,
  FMeX.Types3D,
  FMeX.FileFormat3D.Obj, FMX.MaterialSources, FMX.Objects3D;

type
  TForm21 = class(TForm3D)
    Layer3D1: TLayer3D;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    Camera1: TCamera;
    OpenDialog1: TOpenDialog;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Transparence: TLabel;
    TextureMaterialSource1: TTextureMaterialSource;
    DummyX: TDummy;
    DummyY: TDummy;
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure Form3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
  public
    FDown: TPointF;
    OriginCube :  TeCube;
    { Public declarations }

    procedure DoZoom(aIn: Boolean);
  end;

var
  Form21: TForm21;

implementation

const
  CAMERA_MAX_Z = -2;
  CAMERA_MIN_Z = -200;
  ZOOM_STEP = 2;


{$R *.fmx}

procedure TForm21.Button1Click(Sender: TObject);
const exts : array[0..3] of string = ('.jpg','.png','.pcx','.bmp');
var onlyFileNameAndPathWithoutExt : string;
    i : integer;

  function removeExt(aFileName : string) : string;
  begin
    result := aFileName;
    if Pos('.',aFileName)>0 then
      Result := Copy(aFileName,1,Pos('.',aFileName)-1);
  end;
begin
  if OpenDialog1.Execute then
  begin
    Assert(lowerCase(ExtractFileExt(OpenDialog1.FileName)) = '.obj');
    TextureMaterialSource1.Texture.SetSize(0,0);
    OriginCube.MaterialSource := LightMaterialSource1;

    onlyFileNameAndPathWithoutExt := lowerCase(ExtractFilePath(OpenDialog1.FileName) +
                                     lowerCase(removeExt(ExtractFileName(OpenDialog1.FileName))));
    //Sometimes, there are no Mat file given in obj file : it the case with chalet.obj example.
    for I := 0 to Length(exts)-1 do
      if FileExists(onlyFileNameAndPathWithoutExt+exts[i]) then
      begin
        TextureMaterialSource1.Texture.LoadFromFile(onlyFileNameAndPathWithoutExt+exts[i]);
        OriginCube.MaterialSource := TextureMaterialSource1;
        break;
      end;

    //Direct reading one (Not remove duplicate vertices.)
    FMeXLoadRawOBJex(OpenDialog1.FileName,TeCustomMesh(originCube));
    //loading with removing duplicate vertices. (slower, but better)
    //FMeXLoadRawOBJ(OpenDialog1.FileName,TeCustomMesh(originCube));
  end;
end;

procedure TForm21.Form3DCreate(Sender: TObject);
begin
  OriginCube := TeCube.Create(Self);
  OriginCube.MaterialSource := TextureMaterialSource1;
  AddObject(OriginCube);
  OriginCube.DrawOverlapShape := false;
end;

procedure TForm21.Form3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FDown := PointF(X, Y);
end;

procedure TForm21.Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if (ssLeft in Shift) then
  begin
    DummyX.RotationAngle.X := DummyX.RotationAngle.X - ((Y - FDown.Y) * 0.3);
    DummyY.RotationAngle.Y := DummyY.RotationAngle.Y + ((X - FDown.X) * 0.3);
  end;
  if (ssRight in Shift) then
  begin
//    DummyX.Position.X := DummyX.Position.X - ((X - FDown.X) * 0.02);
//    DummyY.Position.Y := DummyY.Position.Y - ((Y - FDown.Y) * 0.02);

//    DummyX.Position.Y := DummyY.Position.Y;
//    DummyY.Position.X := DummyX.Position.X;
  end;
  FDown := PointF(X, Y);
end;

procedure TForm21.DoZoom(aIn: Boolean);
var
  newZ: Single;
begin
  if aIn then
    newZ := Camera1.Position.Z + ZOOM_STEP
  else
    newZ := Camera1.Position.Z - ZOOM_STEP;

  if (newZ < CAMERA_MAX_Z) and (newZ > CAMERA_MIN_Z) then
    Camera1.Position.Z := newZ;
end;

procedure TForm21.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
    DoZoom(WheelDelta > 0);
end;

procedure TForm21.TrackBar1Change(Sender: TObject);
var v : Single;
begin
  v :=TrackBar1.Value/100;
  OriginCube.Scale.Point := Point3d(v,v,v);
end;

procedure TForm21.TrackBar2Change(Sender: TObject);
begin
  OriginCube.Opacity := TrackBar2.Value / 100;
end;

end.
