unit fmActor;

interface

uses
  System.SysUtils,
  System.Types,
  System.Variants,
  System.UITypes,
  System.Math.Vectors,
  System.Classes,
  FMX.Types,
  FMX.Dialogs,
  FMX.Types3D,
  FMX.Forms,
  FMX.Forms3D,
  FMX.Controls3D,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.MaterialSources,
  FMX.Controls,
  FMX.Layers3D,
  FMX.Controls.Presentation,
  FMX.Objects3D,
  FMX.Materials,
  FMX.Layouts,
  FMX.ListBox,
  FMeX.Types3D,
  FMeX.Types3D.ActorModels,
  FMeX.FileFormat3D.MD2;

type
  TForm7 = class(TForm3D)
    Layer3D1: TLayer3D;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Camera1: TCamera;
    Dummy1: TDummy;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    TextureMaterialSource1: TTextureMaterialSource;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Texture: TCheckBox;
    ListBox1: TListBox;
    Button2: TButton;
    Label2: TLabel;
    TimerAnim: TTimer;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Form3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Form3DCreate(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TextureChange(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure TimerAnimTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    dx,dy : single;
    Current : TFMeXActor;

    anim_current : Single;
    anim_start : Single;
    anim_end : Single;
    Procedure StartStopAnim;

    Function IHMToDrawType : TeCustomMeshDrawType;
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.fmx}

procedure TForm7.Button1Click(Sender: TObject);
var
    b : TFMeXModelRessourceMD2;
    c: TFMeXActor;
begin
  if OpenDialog1.Execute then
  begin
    if Assigned(Current) then
    begin
      RemoveObject(Current);
      FreeAndNil(Current);
    end;
    c := TFMeXActor.Create(nil);
    b :=TFMeXModelRessourceMD2.Create;
    b.LoadFromFile(OpenDialog1.FileName);
    c.ModelRessource := b;
    c.Scale.X := 0.1;
    c.Scale.Y := 0.1;
    c.Scale.Z := 0.1;
    AddObject(c);
    if Not(Assigned(c.MaterialSource)) then
    begin
      c.MaterialSource := LightMaterialSource1;
    end;
    Current := c;
//    ListBox1.Items.Text := c.ModelRessource.FrameText;
  end;
end;

procedure TForm7.Button2Click(Sender: TObject);
begin
  StartStopAnim;
end;

procedure TForm7.Form3DCreate(Sender: TObject);
var
  b : TFMeXModelRessourceMD2;
  c: TFMeXActor;
  ff : string;
begin
  ff := '..\..\Media\Warrior\warrior.md2';
  c := TFMeXActor.Create(nil);
  b :=TFMeXModelRessourceMD2.Create;
  b.LoadFromFile(ff);
  c.ModelRessource := b;
  c.Scale.X := 0.1;
  c.Scale.Y := 0.1;
  c.Scale.Z := 0.1;
  c.DrawFrameType := IHMToDrawType;
  AddObject(c);
  if Not(Assigned(c.MaterialSource)) then
  begin
    c.MaterialSource := LightMaterialSource1;
  end;
  Current := c;
//    ListBox1.Items.Text := c.ModelRessource.FrameText;
end;

procedure TForm7.Form3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  dx := x;
  dy := y;
end;

procedure TForm7.Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var v : TVector3D;
begin
  if Shift = [ssleft] then
  begin
    Dummy1.RotationAngle.X := (dy-y);
    dummy1.RotationAngle.Y := (dx-x);
  end
  else
  if shift = [ssCTrl, ssLeft] then
  begin
    if assigned(Current) then
    begin
      //if Current.RayCastIntersect(Vector3d(0,0,0),Vector3d(x,y,100),v) then
      begin
        Current.Position.X := X / 100;
        Current.Position.Y := Y / 100;
      end;

    end;
  end;
end;

procedure TForm7.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta / 100;
end;

function TForm7.IHMToDrawType: TeCustomMeshDrawType;
begin
  result := TeCustomMeshDrawType.mdtFull;

  if RadioButton1.IsChecked then
    result := TeCustomMeshDrawType.mdtPointFrame;
  if RadioButton2.IsChecked then
    result := TeCustomMeshDrawType.mdtWireFrame;
  if RadioButton3.IsChecked then
    result := TeCustomMeshDrawType.mdtFull;

end;

procedure TForm7.ListBox1Change(Sender: TObject);
begin
  if ListBox1.ItemIndex>-1 then
  begin
//    Current.ModelRessource.GetFrame(Current.ModelRessource.Frames[ListBox1.ItemIndex].FrameIndexStart,Current.Data);
//    Label2.Text := IntToStr(Current.ModelRessource.Frames[ListBox1.ItemIndex].FrameCount)+' frame(s)';
    Current.Repaint;
  end;
end;

procedure TForm7.RadioButton1Change(Sender: TObject);
begin
  if Assigned(Current) then
  begin
    Current.DrawFrameType := IHMToDrawType;
  end;
end;

procedure TForm7.StartStopAnim;
begin
  if TimerAnim.Enabled then
  begin
    TimerAnim.Enabled := False;
    ListBox1.Enabled := True;
    Button1.Enabled := True;
    Button2.Text := '>';
  end
  else
  begin
    ListBox1.Enabled := false;
    Button1.Enabled := False;
    Button2.Text := 'Stop';
//    anim_start := Current.ModelRessource.Frames[ListBox1.ItemIndex].FrameIndexStart;
    anim_current := anim_start;
//    anim_end := Current.ModelRessource.Frames[ListBox1.ItemIndex].FrameIndexStart + Current.ModelRessource.Frames[ListBox1.ItemIndex].FrameCount-1;
    TimerAnim.Enabled := True;
  end;
end;

procedure TForm7.TextureChange(Sender: TObject);
begin
  if Assigned(Current) then
  begin
    if Texture.IsChecked then
    begin
      Current.MaterialSource := TextureMaterialSource1;
    end
    else
    begin
      Current.MaterialSource := LightMaterialSource1;
    end;
  end;
end;

procedure TForm7.TimerAnimTimer(Sender: TObject);
begin
//  Current.ModelRessource.GetFrame(anim_current,Current.Data);
//  Current.Repaint;
  anim_current := anim_current + 0.1;
  if Trunc(anim_current) >= Trunc(anim_end) then
    anim_current := anim_start;
  Label3.Text := FloatToStrF(anim_current,TFloatFormat.ffFixed,2,2);
end;

procedure TForm7.TrackBar1Change(Sender: TObject);
var v : Single;
begin
  if Assigned(Current) then
  begin
    v := TrackBar1.Value / 100;
    Current.Scale.Point := Point3d(v,v,v);
  end;
end;

end.
