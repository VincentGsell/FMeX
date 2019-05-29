unit fscroll;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.MaterialSources,
  System.Math.Vectors, FMX.Controls3D, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Objects, FMX.Layers3D,
  FMeX.Graph,
  FMeX.Ticks,
  FMeX.Animation,
  FMeX.TileScrolling;

type
  TForm5 = class(TForm3D)
    TextureMaterialSource1: TTextureMaterialSource;
    Camera1: TCamera;
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
  private
    { Private declarations }
  public
    LocalProxy : TFMeXProxy;
    LocalGraph : TFMeXGraphFor2D;
    Scroller : TTileScrolling;
    Mover,Mover2 : TLinearTileScrollingLayerMover;
    Procedure MyAppIdle(Sender : TObject; Var Done : Boolean);
    { Public declarations }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

{ TForm5 }

procedure TForm5.Form3DCreate(Sender: TObject);
var myLayer : TScrollingLayer;
    myChunk : TScrollingChunck;
begin
  Scroller :=  TTileScrolling.Create;

  LocalProxy := TFMeXProxy.Create(nil);

  //Ask for a simple 2d graph : simple Object list, rendered sequentialy.
  LocalGraph := TFMeXGraphFor2D.Create(nil);
  LocalProxy.Graph := LocalGraph;

  LocalGraph.add(Scroller);

  LocalProxy.Parent := Self;

///  Uncomment above to make your own scroller.
///  --> Scroller object create for convenience purpose defaut "hello world"

{ TODO : CLARIFY }


  myChunk :=  TScrollingChunck.Create(nil);
  myChunk.SideX := 3;
  myChunk.SideY := 3;
  myChunk.OffsetY := 5;
  myChunk.DebugMode := true;
  myChunk.DebugChunckColor := TAlphaColorRec.Azure;

  myLayer := TScrollingLayer.Create;
  myLayer.Add(myChunk);

  //SCroller has, by default, a default layer (Layer index 0) : this is already loaded by a standart Chunk.
  Scroller.AddLayer(myLayer); //A second one.
  //Animation.
  Mover := TLinearTileScrollingLayerMover.Create;
  Mover.TileScrolling := Scroller;
  Mover.LayerIndex := 1;
  Mover.Duration := 2500;
  Mover.PositionStart := Point(-220,0);
  Mover.PositionStop := Point(120,0);
  Mover.AnimationControlTypeAtEnd := TofAnimationControl.ofacLoop;

  Mover2 := TLinearTileScrollingLayerMover.Create;
  Mover2.TileScrolling := Scroller;
  Mover2.LayerIndex := 0;
  Mover2.Duration := 3000;
  Mover2.PositionStart := Point(-120,0);
  Mover2.PositionStop := Point(120,0);
  Mover2.AnimationControlTypeAtEnd := TofAnimationControl.ofacLoop;

  Application.OnIdle := MyAppIdle;

end;

procedure TForm5.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z - WheelDelta*0.1;
end;

procedure TForm5.MyAppIdle(Sender: TObject; var Done: Boolean);
begin
 Mover.Process(TheCadencer.DeltaTime);
 Mover2.Process(TheCadencer.DeltaTime);
 TheCadencer.Update;
 Invalidate;
end;

end.
