unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Objects, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Objects3D, FMX.Controls3D, FMX.Viewport3D, FMX.MaterialSources,
  FMeX.Types3D, FMeX.Ticks, FMeX.Images, FMeX.Particles2D;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    Grid3D1: TGrid3D;
    Disk1: TDisk;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label6: TLabel;
    Selection1: TSelection;
    TextureMaterialSource1: TTextureMaterialSource;
    Camera1: TCamera;
    TimerStats: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Grid3D1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
    procedure Grid3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single; RayPos, RayDir: TVector3D);
    procedure Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure TrackBar1Tracking(Sender: TObject);
    procedure TrackBar2Tracking(Sender: TObject);
    procedure TimerStatsTimer(Sender: TObject);
  private
    { Private declarations }
    Procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    procedure SetUpAtlas;
    procedure SetUpObject;

  public
    { Public declarations }
    Atlas : TeTextureAtlas; //Global image atlas.

    MyPizza : TeSpeedImage;
    MyImageWall : TeRepeatedImage;
    MyDino : TeImage;

    MyStarField,
    MystrawberryExplosion,
    MyMusicMouseControledParticle,
    MyRedFlowerParticle : TeParticle2d;

    cycle : Cardinal;
    mcycle : Cardinal;

    LocalGraph : TFMeXGraphFor2D;
    LocalProxy : TFMeXProxy;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Viewport3D1.Align := TAlignLayout.Client;
  Selection1.HideSelection := true;

  SetUpAtlas;
  SetUpObject;


  LocalProxy := TFMeXProxy.Create(nil);

  //Ask for a simple 2d graph : simple Object list, rendered sequentialy.
  LocalGraph := TFMeXGraphFor2D.Create(nil);
  LocalProxy.Graph := LocalGraph;

  Viewport3D1.AddObject(LocalProxy);

  //Change draw sequence here to change draw order. First is farer, last is nearer.
  LocalGraph.Add(MyImageWall);
  LocalGraph.Add(MyStarField);
  LocalGraph.Add(MyRedFlowerParticle);
  LocalGraph.Add(MystrawberryExplosion);
  LocalGraph.Add(MyMusicMouseControledParticle);
  LocalGraph.Add(MyDino);
  LocalGraph.Add(MyPizza);

  Application.OnIdle := OnAppIdle;

  TheCadencer.Reset;
end;

procedure TForm1.OnAppIdle(Sender: TObject; var Done: Boolean);
var lparticleClount : Integer;
begin
  inc(cycle);

  TheCadencer.Update;

  lparticleClount := 0;

  if assigned(MyMusicMouseControledParticle) then
  begin
    MyMusicMouseControledParticle.UpdateParticles;
    inc(lparticleClount,MyMusicMouseControledParticle.ParticleCount);
  end;
  MyStarField.UpdateParticles;
  inc(lparticleClount,MyStarField.ParticleCount);
  MystrawberryExplosion.UpdateParticles;
  inc(lparticleClount,MystrawberryExplosion.ParticleCount);
  MyRedFlowerParticle.UpdateParticles;
  inc(lparticleClount,MyRedFlowerParticle.ParticleCount);

  MyImageWall.RotationAngle.Z := MyImageWall.RotationAngle.Z + TheCadencer.TimeSliceValue(50);

  Label6.Text := 'Total parcicle count : ' + IntToStr(lparticleClount);
  Label3.Text := IntToStr(mcycle)+' cycles';

  Invalidate;
end;
procedure TForm1.SetUpAtlas;
var IndexX, IndexY : Integer;
    margeX, margeY : Single;
begin
   Atlas := TeTextureAtlas.Create;
   Atlas.TextureTitle := 'Taito !';
   Atlas.MaterialSource := TextureMaterialSource1;
   Atlas.AddItem('Default',0,0,1,1); //plain size.

   //Calculus with this source is not possible (certainly hand made)
   //It is juste an example how to write an atlas from code, so,
   //I apologize for the not exactly accurate of image cut !
   margeX := (1/500) * 0.5;
   margeY := (1/500) * 1;
   IndexX := 1;
   IndexY := 1;

   Atlas.AddItem('Grey Heart', margeX + (1 / 15) * (IndexX - 1),
                          margeY + (1 / 16) * (IndexY-1),
                          margeX + (1 / 15) * IndexX,
                          margeY + (1 / 16) * IndexY);

   IndexX := 1;
   IndexY := 2;
   Atlas.AddItem('Pizza', margeX + (1 / 15) * (IndexX - 1),
                          margeY + (1 / 16) * (IndexY-1),
                          margeX + (1 / 15) * IndexX,
                          margeY + (1 / 16) * IndexY);

   //...And so on !
   IndexX :=2; IndexY := 1; Atlas.AddItem('Money Sun', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=3; IndexY := 1; Atlas.AddItem('Money Moon', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=4; IndexY := 1; Atlas.AddItem('Ice Star', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=5; IndexY := 1; Atlas.AddItem('Snow Star', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=6; IndexY := 1; Atlas.AddItem('Japan Soap', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=2; IndexY := 3; Atlas.AddItem('Strawberry', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=2; IndexY := 4; Atlas.AddItem('Necklace', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=13; IndexY := 2; Atlas.AddItem('PopUp Dino', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1),margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=12; IndexY := 1; Atlas.AddItem('Saphir', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=7; IndexY := 2; Atlas.AddItem('Red Flower', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
   IndexX :=4; IndexY := 2; Atlas.AddItem('Music note', margeX + (1 / 15) * (IndexX - 1), margeY + (1 / 16) * (IndexY-1), margeX + (1 / 15) * IndexX, margeY + (1 / 16) * IndexY);
end;

procedure TForm1.SetUpObject;
begin
   //Main Image component : Used it widely, it is fast :)
   MyDino := TeImage.Create(nil);
   //this one maintain a TeTextureConfiguration, witch pointed on an TextureAtlas object.
   MyDino.SourceConfiguration.TextureData := Atlas;
   MyDino.SourceConfiguration.TextureItem := 'PopUp Dino';
   //And it is finished ! :)


   //Repeated image
   MyImageWall := TeRepeatedImage.Create(nil);

   MyImageWall.TextureRepeatX := 30;
   MyImageWall.TextureRepeatY := 30;

   //As TeImage, source texture is base on a TeTextureConfiguration.
   MyImageWall.SourceConfiguration.TextureData := Atlas;
   MyImageWall.SourceConfiguration.TextureItem := 'music note';


   MyImageWall.Position.X := -15;
   MyImageWall.Position.Y := -15;
   MyImageWall.Width := 1;
   MyImageWall.Height := 1;

   MyImageWall.RotationAngle.Z := 35;

   MyImageWall.RotationCenter.X := MyImageWall.TextureRepeatX div 2;
   MyImageWall.RotationCenter.Y := MyImageWall.TextureRepeatY div 2;
   //See TimerSwap, we change dyamicaly the image.


   MyPizza := TeSpeedImage.Create(nil);
   MyPizza.SourceConfiguration.TextureData := Atlas;
   MyPizza.SourceConfiguration.TextureItem := 'Pizza';
   MyPizza.DrawSpeedEffect := true;
   MyPizza.Position.X := 0;
   MyPizza.Position.Y := 0;
   MyPizza.Width := 1;
   MyPizza.Height := 1;


   MystrawberryExplosion := TeParticle2d.Create(nil);
   MystrawberryExplosion.ParticleCount := 1000;
   MystrawberryExplosion.ParticleType := TeParticle2dType.ptExplodeAdLib;
   MystrawberryExplosion.SetupRect(0,0,10,10);
   MystrawberryExplosion.Position.x := 4;
   MystrawberryExplosion.Position.y := 4;
   MystrawberryExplosion.SourceConfiguration.TextureData := Atlas;
   MystrawberryExplosion.SourceConfiguration.TextureItem := 'Strawberry';
   MystrawberryExplosion.BehaviourParticleBecomeSmallerDuringLife := True;

   MyRedFlowerParticle := TeParticle2d.Create(nil);
   MyRedFlowerParticle.ParticleCount := 3000;
   MyRedFlowerParticle.ParticleType := TeParticle2dType.ptExplodeAdLib;
   MyRedFlowerParticle.BehaviourMaxDurationInMS := MyRedFlowerParticle.BehaviourMaxDurationInMS * 4;
   MyRedFlowerParticle.SetupRect(-10,7,10,10);
   MyRedFlowerParticle.Position.x := 4;
   MyRedFlowerParticle.Position.y := 4;
   MyRedFlowerParticle.SourceConfiguration.TextureData := Atlas;
   MyRedFlowerParticle.SourceConfiguration.TextureItem := 'Red Flower';
   MyRedFlowerParticle.BehaviourParticleBecomeSmallerDuringLife := True;

   MyStarField := TeParticle2d.Create(nil);
   MyStarField.ParticleCount := 500;
   MyStarField.ParticleType := TeParticle2dType.ptStarfield;
   MyStarField.SetupRect(0,0,200, 20);
   MyStarField.Position.x := -50;
   MyStarField.Position.y := -5;
   MyStarField.SourceConfiguration.TextureData := Atlas;
   MyStarField.SourceConfiguration.TextureItem := 'Ice Star';

   MyMusicMouseControledParticle := TeParticle2d.Create(Self);
   MyMusicMouseControledParticle.ParticleCount := 1000;
   MyMusicMouseControledParticle.ParticleType := TeParticle2dType.ptExplode;
   MyMusicMouseControledParticle.SetupRect(0,0,10,10);
   MyMusicMouseControledParticle.BehaviourMaxVelocity := 30;
   MyMusicMouseControledParticle.SourceConfiguration.TextureData := Atlas;
   MyMusicMouseControledParticle.SourceConfiguration.TextureItem := 'Music Note';
   MyMusicMouseControledParticle.BehaviourParticleBecomeSmallerDuringLife := True;
end;

procedure TForm1.TimerStatsTimer(Sender: TObject);
begin
  if Assigned(MyImageWall) then
  begin
  //Change image randomly, throught available Data in atlas. -> -1)+1 in order to not display "default" picture.
   MyImageWall.SourceConfiguration.TextureItemIndex := Random(MyImageWall.SourceConfiguration.TextureData.TextureAtlasItems.Count-1)+1;
  end;
  Label1.Text := IntToStr(cycle - mcycle) + ' FPS';
  mcycle := cycle;
end;

procedure TForm1.TrackBar1Tracking(Sender: TObject);
begin
  if TrackBar1.Value<100 then
    TheCadencer.TimeScaler := 1 - ((100-TrackBar1.Value)/100)
  else
    TheCadencer.TimeScaler := 1 + ((TrackBar1.Value-100)/100)
end;

procedure TForm1.TrackBar2Tracking(Sender: TObject);
begin
  Camera1.Position.Z := TrackBar2.Value;
end;

procedure TForm1.Viewport3D1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z - WheelDelta/100;
end;

procedure TForm1.Grid3D1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single; RayPos, RayDir: TVector3D);
var i : TPoint3d;
begin
  Grid3D1.RayCastIntersect(RayPos,RayDir,I);
  MyDino.Position.Point := Point3D(i.X,i.Y,0);
end;

procedure TForm1.Grid3D1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
var i : TPoint3d;
begin
  Grid3D1.RayCastIntersect(RayPos,RayDir,I);
  Disk1.Position.X := X;
  Disk1.Position.Y := Y;

  MyMusicMouseControledParticle.SetupRect(0,0,100,100);
  MyMusicMouseControledParticle.Position.x := i.x;
  MyMusicMouseControledParticle.Position.y := i.y;
  MyMusicMouseControledParticle.Reset;
end;

end.
