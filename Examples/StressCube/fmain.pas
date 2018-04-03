unit fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMeX.Types3d, System.Math.Vectors, FMX.Controls3D,
  FMX.MaterialSources, FMX.StdCtrls, FMX.Layers3D, FMX.Objects3D,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm3D)
    LightMaterialSource1: TLightMaterialSource;
    Layer3D1: TLayer3D;
    Label1: TLabel;
    Button1: TButton;
    Light1: TLight;
    Camera1: TCamera;
    Dummy1: TDummy;
    Label2: TLabel;
    Button2: TButton;
    Light2: TLight;
    procedure Form3DCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Form3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Form3DRender(Sender: TObject; Context: TContext3D);
  private
    { Déclarations privées }

    Procedure applicationidle(sender : TObject; Var Done : Boolean);
  public
    { Déclarations publiques }
    OriginCube : TeCube;
    mb : TPointf;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button2Click(Sender: TObject);
var i,j,k : integer;
    p, o, c : TeCube;
    l : TLight;
    CubeCount : Integer;
    CubeMergedCount : Integer;
begin
  Button2.Enabled := false;
  CubeCount := 0;

  for k := 0 to 4 do
  begin
    p := TeCube.Create(nil);
    p.Position.Point := Point3D(0,0,0);

    l:= TLight.Create(Self);
    AddObject(l);
    l.LightType := TLightType.Point;
    l.Color := TAlphaColor(i*1500+j*100+k*1500+50000);
    l.Position.Point := Point3D(10*i-50,15*j-50,3*k);

    for I := 0 to 99 do
    begin
      o := TeCube.Create(nil);
      o.Position.Point := Point3D(35,0,0);
      for j := 0 to 99 do
      begin
        c := TeCube.Create(nil);
        c.MaterialSource := LightMaterialSource1;
        c.Position.Point := Point3D((k+1)*10 + 1.5*i-50,1.5*j-50,3*k); //Merge will keep relative position : We move it *before* merge.
        inc(CubeCount);
        o.MergeFrom(c); //Here : We merge "c" cube definition (vertices, indices...) into "o" mesh.
        c.Free;
      end;
      p.MergeFrom(o); //Once o finished, we merged it into "p".
      o.Free;
    end;
    OriginCube.MergeFrom(p); //Finally, we merge the result into a single cube.
    p.Free; //we can release the original.
  end;
  Button2.Text := IntToStr(CubeCount)+' cubes.';
end;

procedure TForm1.Form3DCreate(Sender: TObject);
begin
  OriginCube := TeCube.Create(Self);
  OriginCube.MaterialSource := LightMaterialSource1;
  AddObject(OriginCube);
  Application.OnIdle := applicationidle;
end;



procedure TForm1.Button1Click(Sender: TObject);
var i,j,k : integer;
    p, o, c : TeCube;
    l : TLight;
    CubeCount : Integer;
    CubeMergedCount : Integer;
begin
  Button1.Enabled := False;
  Button2.Enabled := True;
  CubeCount := 0;

  //Base object, for final build.
  p := TeCube.Create(nil);
  p.Position.Point := Point3D(0,0,0);

  //Build 1000 cubes object.
  o := TeCube.Create(nil);
  inc(CubeCount);
  o.Position.Point := Point3D(0,0,0);
  o.MaterialSource := LightMaterialSource1;
  for i := 0 to 9 do
    for j := 0 to 9 do
      for k := 0 to 9 do
      begin
        c := TeCube.Create(nil);
        inc(CubeCount);
        c.MaterialSource := LightMaterialSource1;
        c.Position.Point := Point3D((i-5)*2,(j-5)*2,(k-5)*2); //Merge will keep relative position : We move it *before* merge.
        //Now merge !
        o.MergeFrom(c);
        //This api make huge memory manipulation for vertex. Use it carefully.
        //For example, this will work bad above thousands consecutives merges.
        // --> Instead, try to merge huge object several time instead of many merge of little object.
        //     It is what we done here : The 1000 cube object is merge 10 time in final cube.
        //      Make 10000 cube merge is difficult with actual system memory.

        //Not need C anymore; because only it 3d data has been merge. Remaining stuff became useless here.
        c.Free;
      end;

  //Merge Object O several time in Origin cube : 1001 cubes * 4 * 3 * 3 = 36 036 cubes
  CubeMergedCount := 0;
  for i := 0 to 3 do
  begin
    l:= TLight.Create(Self);
    AddObject(l);
    l.LightType := TLightType.Point;
    l.Color := TAlphaColorRec.Red;
    l.Position.Point := Point3D((i-1)*50,50,50);
    for j := 1 to 3 do
      for k := 1 to 3 do
      begin
        o.Position.Point := Point3D((i-2)*25,(j-2)*25,(k-2)*25); //1000 cubes object (o) has side of 20. We move it *before* merge.
        o.Position.X := (i-2) * 25;
        p.MergeFrom(o);
        Inc(CubeMergedCount,CubeCount);
      end;
  end;
  o.Free;
  OriginCube.MergeFrom(p);
  p.Free;

  Button1.Text := IntToStr(CubeMergedCount)+' cubes.';

  Invalidate;

  Camera1.AnimateFloat('Position.Z',-90,1.5);

end;

procedure TForm1.Form3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  mb := PointF(x,y);
end;

procedure TForm1.Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  if ssleft in  Shift then
  begin
    Dummy1.RotationAngle.X := Dummy1.RotationAngle.X + (y - mb.Y)/100;
    Dummy1.RotationAngle.Y := Dummy1.RotationAngle.Y - (x - mb.x)/100;
  end;
end;

procedure TForm1.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta / 100;
end;


procedure TForm1.Form3DRender(Sender: TObject; Context: TContext3D);
begin
  Label2.Text := 'FPS : ' + FloaTtoStr(Context.FPS);
end;

procedure TForm1.applicationidle(sender: TObject; var Done: Boolean);
begin
  Invalidate;
end;


end.
