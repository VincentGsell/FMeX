unit fmainSpine;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.MaterialSources,
  System.Math.Vectors, FMX.Controls3D, FMeX.Ticks, FMeX.Images, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layers3D,
  FMX.Objects3D,
  FMeX.Types3D,
  FMeX.HUD,
  FMeX.Graph,
  FMeX.FileFormat2D.Spine;

type
  TForm1 = class(TForm3D)
    Timer1: TTimer;
    Camera1: TCamera;
    procedure Form3DCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure Form3DMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Form3DMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Form3DRender(Sender: TObject; Context: TContext3D);

  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    HUD : THudControler;
    Selector : THudShape;
    SpineObj, SpineObj2 : TFMeXSpineObject;
    Theater : TeImage;

    TargetImage : TeImage;
    Target2 : TeImage;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Form3DCreate(Sender: TObject);
var i : integer;
begin
  HUD := THudControler.Create(Context);
//  HUD.ReferenceObject := Grid3D1;
  Selector :=  THudShape.Create;
  Selector.Visible := False;
  Selector.Ability := [THudShapeAbility.saDragAndDropAbility,THudShapeAbility.saMouseAbility];
  Hud.AddHud(Selector);

  SpineObj := TFMeXSpineObject.Create;
  SpineObj.Init;
  SpineObj.CurrentSkin := 'goblingirl';
  SpineObj.CurrentAnimationIndex := 1;
  SpineObj.Adapt(0,3.5,0.02,0.02); //this value adapt the "spine editor" creator coords to the FMX current rendering. To get it, try, fail, and retry ;)

  SpineObj2 := TFMeXSpineObject.Create;
  SpineObj2.Init;
  SpineObj2.CurrentSkin := 'goblin'; //Trig SetCurrentSkin.
  SpineObj2.CurrentAnimationIndex := 0;
  SpineObj2.Adapt(0,3.5,0.02,0.02); //It is the same than before because sckin is more or less on the same size.

  TargetImage := TeImage.Create(Self);
//  TargetImage.Parent := Self;
  TargetImage.DrawOverlapShape := true;

  Target2 := TeImage.Create(Self);
//  Target2.Parent := Self;
  Target2.DrawOverlapShape := True;
//  Target2.RotationAngle.Z := 45;

  Target2.Position.X := -4;


  AddObject(TargetImage);
  AddObject(Target2);
end;

procedure TForm1.Form3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  HUD.InData_MousePos := PointF(X,Y);
  HUD.InData_MouseLeftButton := ssLeft in Shift;
  HUD.Process;
  Invalidate;
end;

procedure TForm1.Form3DMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  HUD.InData_MousePos := PointF(X,Y);
  HUD.InData_MouseLeftButton := ssLeft in Shift;
  HUD.Process;

end;

procedure TForm1.Form3DMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);

var RPv,RDv : TVector3D;
    RP,RD,Inter : TPoint3D;
    i : integer;
    Control : TControl3D;
begin
  HUD.InData_MousePos := PointF(X,Y);
  HUD.InData_MouseLeftButton := ssLeft in Shift;
  HUD.Process;

  if not(HUD.WorkInProgress) then
  begin
     Context.Pick(X,Y,TProjection.Camera,RPv,RDv);
     for i := 0 to Children.Count-1 do
     begin
       if Not (Children[i] is TeCustomMesh) then
         Continue;

       Control := TControl3D(Children[i]);

       RP :=  Control.AbsoluteToLocalVector(RPv);
       RD :=  Control.AbsoluteToLocalDirection(RDv);

       if Control.RayCastIntersect(RP,RD,Inter) then
       begin

              if Selector.Target = Control then
              begin
                if Selector.Visible then
                begin
                  Selector.Visible := False;
                end
                else
                begin
                  Selector.Target := Control;
                  Selector.Visible := true;
                end;
              end
              else
              begin
                if Not(Selector.Visible) then
                begin
                  Selector.Visible := true;
                end;
                Selector.Target := Control;
              end;
              Invalidate;
              Exit;

       end;
     end;
  end;
end;

procedure TForm1.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta/50;
end;

procedure TForm1.Form3DRender(Sender: TObject; Context: TContext3D);
begin
  HUD.Render;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  TheCadencer.Update;

  SpineObj.Update;
  SpineObj.Draw(TargetImage);

  SpineObj2.Update;
  SpineObj2.Draw(Target2);

  Caption := 'FPS '+  Format('%f',[Context.FPS]);
  Invalidate;
end;

end.
