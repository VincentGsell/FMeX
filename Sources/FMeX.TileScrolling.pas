unit FMeX.TileScrolling;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects,
  Generics.Collections, FMX.Types3D, FMX.Materials,
  FMX.Graphics,FMX.Controls3D, FMX.MaterialSources, System.Math.Vectors,
  FMeX.Animation,
  FMeX.Gx.Types,
  FMex.Types3D,
  FMeX.Images;


type
  TTileScrolling = Class;

  TScrollingChunck = Class(TComponent)
  private
    FString: TStrings;
    FXS: Single;
    FYS: Single;
    FEnabled: Boolean;
    FRepeatOnX: Boolean;
    FRepeatOnY: Boolean;
    FX: Single;
    FY: Single;
    FDebugMode: Boolean;
    FDebugColor: TAlphaColor;
    FImage: TeImage;
  Public
    Constructor Create(aOwner : TComponent); Override;
    Destructor Destroy; Override;

    Property LayerDescription : TStrings read FString Write FString;
    Property SideX : Single read FXS Write FXS;
    Property SideY : Single read FYS Write FYS;
    Property Enabled : Boolean read FEnabled Write FEnabled;
    Property OffsetX : Single read FX Write FX;
    Property OffsetY : Single read FY Write FY;

    Property DebugChunckColor : TAlphaColor read FDebugColor Write FDebugColor;
    Property DebugMode : Boolean read FDebugMode Write FDebugMode;

    Property TileImage : TeImage read FImage Write FImage;
  End;

  TScrollingLayer = Class(TList<TScrollingChunck>)
  End;

  TTileScrolling = Class(TeMesh3D)
  Private
    Ver : TVertexBuffer;
    Mat : TColorMaterial;
    MatDebug : TColorMaterial;
    Idx : TIndexBuffer;
    FTM: TTextureMaterialSource;

    FLayerList : TList<TScrollingLayer>;

    Procedure Quad(x,y,w,h : Single; Const z : Single = 0.0);
    Procedure QuadTex(x,y,xx,yy : Single);

    function GetLayer(Index: Cardinal): TScrollingLayer;
    function GetLayerCount: Cardinal;
    procedure SetLayer(Index: Cardinal; const Value: TScrollingLayer);
  Public
    Procedure Render(acontext : TContext3D; const aFXMControl : TControl3d = nil); Override;

    //Move all chunk of a given layer.
    Procedure LayerOffsetMoveBy(LayerIndex : Cardinal; MoveByValueX, MoveByValueY : Single);
    Procedure LayerOffsetReset(LayerIndex : Cardinal; ValueX, ValueY : Single);

    Constructor Create; Override;
    Destructor Destroy; Override;

    Procedure AddLayer(aLayer : TScrollingLayer);

    //Chunk method generation
    Function BuildRect(InLayerIndex : Integer; aChar : Char; x,y,aWidth,aHeight : Integer) : TScrollingChunck;
    Procedure RemoveAllChunckFromLayer(LayerIndex : Integer);


    Property Layers[Index : Cardinal] : TScrollingLayer read GetLayer Write SetLayer; Default;
    Property LayerCount : Cardinal read GetLayerCount;

  End;

  //Simple Animation object : Fit for simple scroll effect.
  TLinearTileScrollingLayerMover = Class(TofUpdater)
  private
    FPosStop: Tpointf;
    FPosStart: TPointf;
    FTileScrolling: TTileScrolling;
    FLayerIndex: Cardinal;
    FRunning : Boolean;

    Function GetIsRunning : Boolean; Override;
  Public
    Constructor Create; Override;
    Procedure Process(MilliSecondPassedFromLastCall : Double); Override;

    Property PositionStart : Tpointf read FPosStart Write FPosStart;
    Property PositionStop : Tpointf read FPosStop Write FPosStop;

    Property TileScrolling : TTileScrolling read FTileScrolling Write FTileScrolling;
    Property LayerIndex : Cardinal read FLayerIndex Write FLayerIndex;
  End;



implementation


{ TTileScrolling }

procedure TTileScrolling.AddLayer(aLayer: TScrollingLayer);
begin
  FLayerList.Add(aLayer);
end;

Function TTileScrolling.BuildRect(InLayerIndex: Integer; aChar: Char; x, y,
  aWidth, aHeight: Integer) : TScrollingChunck;
var a : TScrollingChunck;
    i, j : integer;
    s : String;
begin
  a := TScrollingChunck.Create(nil);
  a.FString.Clear;
  s := '';
  For i := 0 to aWidth-1 do
  begin
    s := s + aChar;
  end;

  for j := 0 to aHeight-1 do
  begin
    a.FString.Add(s);
  end;
  a.OffsetX := x;
  a.OffsetY := y;

  if LayerCount>InLayerIndex then
  begin
    Layers[InLayerIndex].Add(a);
  end;

  Result := a;
end;

constructor TTileScrolling.Create;
var a : TScrollingLayer;
begin
  inherited;
  Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4);
  Mat := TColorMaterial.Create;
  Mat.Color := TAlphaColorRec.Blue;
  MatDebug := TColorMaterial.Create;
  MatDebug.Color := TAlphaColorRec.Red;
  Idx := TIndexBuffer.Create(6);

  FLayerList :=TList<TSCrollingLayer>.Create;
  a := TScrollingLayer.Create;
  a.Add(TScrollingChunck.Create(nil));
  FLayerList.Add(a);

  Width := 1;
  Height := 1;

  Quad(0,0,Width,Height);
  QuadTex(0,0,1,1);
  Idx[0] := 0;
  Idx[1] := 1;
  Idx[2] := 3;
  Idx[3] := 3;
  Idx[4] := 1;
  Idx[5] := 2;

end;

destructor TTileScrolling.Destroy;
begin
  Ver.Free;
  Idx.Free;
  Mat.Free;
  MatDebug.Free;
  FreeAndNil(FLayerList);
  inherited;
end;

function TTileScrolling.GetLayer(Index: Cardinal): TScrollingLayer;
begin
  Result := TScrollingLayer(FLayerList[Index]);
end;

function TTileScrolling.GetLayerCount: Cardinal;
begin
  result := FLayerList.Count;
end;

procedure TTileScrolling.LayerOffsetMoveBy(LayerIndex: Cardinal;
  MoveByValueX, MoveByValueY: Single);
var i : integer;
begin
  if LayerCount>LayerIndex then
  begin
    for i := 0 to Layers[LayerIndex].Count-1 do
    begin
      Layers[LayerIndex][i].OffsetX := Layers[LayerIndex][i].OffsetX + MoveByValueX;
      Layers[LayerIndex][i].OffsetY := Layers[LayerIndex][i].OffsetY + MoveByValueY;
    end;
  end;
end;

procedure TTileScrolling.LayerOffsetReset(LayerIndex: Cardinal; ValueX,
  ValueY: Single);
var i : integer;
begin
  if LayerCount>LayerIndex then
  begin
    for i := 0 to Layers[LayerIndex].Count-1 do
    begin
      Layers[LayerIndex][i].OffsetX := ValueX;
      Layers[LayerIndex][i].OffsetY := ValueY;
    end;
  end;
end;

procedure TTileScrolling.Quad(x, y, w, h: Single; Const z  : Single = 0.0);
begin
  Ver.Vertices[0] := Point3D(x, y, z);
  Ver.Vertices[1] := Point3D(x+w, y, z);
  Ver.Vertices[2] := Point3D(x+w, y+h, z);
  Ver.Vertices[3] := Point3D(x, y+h, z);
end;

procedure TTileScrolling.QuadTex(x, y, xx, yy: Single);
begin
  Ver.TexCoord0[0] := PointF(x, y);
  Ver.TexCoord0[1] := PointF(xx, y);
  Ver.TexCoord0[2] := PointF(xx, yy);
  Ver.TexCoord0[3] := PointF(x, yy);
end;

procedure TTileScrolling.RemoveAllChunckFromLayer(LayerIndex : Integer);
var i : integer;
begin
  if LayerCount>LayerIndex then
  begin
    for I := 0 to Layers[LayerIndex].Count-1 do
    begin
      Layers[LayerIndex][i].Free;
    end;
    Layers[LayerIndex].Clear;
  end;
end;

procedure TTileScrolling.Render(acontext : TContext3D; const aFXMControl : TControl3d = nil);
var l,c,i,j : Integer;
    la : TScrollingLayer;
    s : TScrollingChunck;
begin
  {$IFDEF VER260}
  Context.SetMatrix(TMatrix3D.CreateScaling(Point3D(Width, Height, Depth)) * AbsoluteMatrix);
  {$ELSE}
  {$ENDIF}

  for l := 0 to FLayerList.Count-1 do
  begin
    la := TScrollingLayer(FLayerList[l]);

    for c := 0 to la.Count-1 do
    begin
      s := la[c];

      if s.Enabled then
      begin
        for i := 0 to s.LayerDescription.Count-1 do
        begin

          if Assigned(s.TileImage) then
          begin
            FTM := TTextureMaterialSource(s.TileImage.SourceConfiguration.TextureData.MaterialSource);
            QuadTex(s.TileImage.SourceConfiguration.GetTextureItemDetail.TextureSourceX1,
                    s.TileImage.SourceConfiguration.GetTextureItemDetail.TextureSourceY1,
                    s.TileImage.SourceConfiguration.GetTextureItemDetail.TextureSourceX2,
                    s.TileImage.SourceConfiguration.GetTextureItemDetail.TextureSourceY2);
          end
          else
          begin
            FTM := nil;
            QuadTex(0,0,1,1);
          end;

          for j := 1 to Length(s.LayerDescription[i])-1 do
          begin

            if s.LayerDescription[i][j] <> ' ' then
            begin

              Quad( s.OffsetX + s.SideX*j,
                    s.OffsetY + s.SideY*i,
                    s.SideX,
                    s.SideY,
                    -(l+1)*(c+1));

              if Assigned(FTM) then
              begin
                acontext.DrawTriangles(Ver, Idx, ftm.Material, 1.0);
              end
              else
                acontext.DrawTriangles(Ver, Idx, Mat, 1.0);

              if s.DebugMode then
              begin
                acontext.DrawLines(Ver, Idx, MatDebug, 1.0);
              end;
            end;

          end;

        end;
      end;
    end;
  end;
end;

procedure TTileScrolling.SetLayer(Index: Cardinal; const Value: TScrollingLayer);
begin
  FLayerList[Index] := Value;
end;

{ TScrollingChunck }

constructor TScrollingChunck.Create(aOwner: TComponent);
begin
  inherited;
  FString := TStringList.Create;
  FString.Add('x   x  xxxx x    x     xxx   ');
  FString.Add('x   x x     x    x    x   x  ');
  FString.Add('xxxxx xxxx  x    x    x   x  ');
  FString.Add('x   x x     x    x    x   x   ');
  FString.Add('x   x  xxxx xxxx xxxx  xxx   ');
  FString.Add('');
  FString.Add('                                  xxx');
  FString.Add('  x   x   x  xxx   xxx  x   xxx   xxx');
  FString.Add('  x   x   x x   x x   x x   x  x  xxx');
  FString.Add('   x x x x  x   x xxxx  x   x  x   ');
  FString.Add('    x   x    xxx  x   x xxx xxx   xxx');
  FString.Add('');
  FXS := 1;
  FYS := 1;
  FEnabled := True;
  FRepeatOnX := True;
  FRepeatOnY := False;
  FDebugMode := False;
  FDebugColor := $FFFF0000;
  FImage := Nil;
end;

destructor TScrollingChunck.Destroy;
begin
  FreeAndNil(FString);
  inherited;
end;

{ TLinearTileScrollingLayerMover }

constructor TLinearTileScrollingLayerMover.Create;
begin
  inherited;
  FPosStart := PointF(0,0);
  FPosStop := PointF(0,0);
  FRunning := False;
end;

function TLinearTileScrollingLayerMover.GetIsRunning: Boolean;
begin
  result := FRunning;
end;

procedure TLinearTileScrollingLayerMover.Process(
  MilliSecondPassedFromLastCall: Double);
var a,b,d : Double;
begin
  Inherited;
  d := FTotalTimeEllapsed / FDuration;
  FRunning := d<=1;
  if d>1 then
  begin
    //Finnished.
    if Assigned(FTileScrolling) then
    begin
      FTileScrolling.LayerOffsetReset(FLayerIndex,FPosStop.X,FPosStop.Y);
      //FTileScrolling.Repaint;
    end;
    //Trig something ?
    case AnimationControlTypeAtEnd of
      ofacNone :
      begin
        //Nothing.
        //Enabled := false;
      end;
      ofacLoop :
      begin
        FTileScrolling.LayerOffsetReset(FLayerIndex,FPosStart.X,FPosStart.Y);
        FTotalTimeEllapsed := 0;
        //FTileScrolling.Repaint;
      end;
      ofacInOutLoop :
      begin
        //...
      end;
    end;
  end
  else
  begin


    a := Abs(FPosStop.X) + Abs(FPosStart.X);
    a := a*d;
    if FPosStop.X<FPosStart.X then
      a := a * -1
    else
      if FPosStop.X = FPosStart.X then
        a := FPosStop.X;

    b := Abs(FPosStop.Y) + Abs(FPosStart.Y);
    b := b*d;
    if FPosStop.Y<FPosStart.Y then
      b := b * -1
    else
      if FPosStop.Y = FPosStart.Y then
        b := FPosStop.Y;

    if Assigned(FTileScrolling) then
    begin
      FTileScrolling.LayerOffsetReset(FLayerIndex,FPosStart.X,FPosStart.Y);
      FTileScrolling.LayerOffsetMoveBy(FLayerIndex,a,b);
      //FTileScrolling.Repaint;
    end;
  end;
end;

end.


