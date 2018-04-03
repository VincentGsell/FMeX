unit FMeX.Images;

interface

Uses
  FMX.Objects3D, System.Types,
  System.Generics.Collections, Classes, FMX.Types3D,
  System.UITypes, System.SysUtils,
  FMX.Materials, FMX.MaterialSources ,FMX.Graphics, FMX.Controls3D,
  System.Math.Vectors, System.RTLConsts ,FMeX.Types3D
  ;
Type

//------------------------------------------------------------------------------
TeTextureAtlasItem = Class
Public
  PlainTextName : String;
  TextureSourceX1 : Single;
  TextureSourceY1 : Single;
  TextureSourceX2 : Single;
  TextureSourceY2 : Single;
End;

//------------------------------------------------------------------------------
TeTextureAtlas = Class
Public
  TextureTitle : String;
  MaterialSource : TMaterialSource;
  TextureAtlasItems : TList<TeTextureAtlasItem>;

  Procedure AddItem(AtitleOfItem : String; X1, Y1, X2, Y2: Single);
  Function FindItem(ByTextureTitle : String) : Integer;

  Constructor Create;
  Destructor Destroy; Override;

//  Procedure LoadFromFile(aFileName : String);
//  Procedure SaveToFile(aFileName : String);
End;

//------------------------------------------------------------------------------
TeTextureConfiguration = Class
Private
  FTextureData: TeTextureAtlas;
  FTextureItem: String;
  FTextureItemIndex: Integer;
  FSubscripter: TeCustomMesh;

  procedure SetTextureData(const Value: TeTextureAtlas);
  procedure SetTextureItem(const Value: String);
  procedure SetTextureItemIndex(const Value: Integer);
  Procedure InternalSetTextureItem(aTextureItem : TeTextureAtlasItem);
Public
  Constructor Create;
  Destructor Destroy; Override;

  Procedure ApplyConfigurationOn(aTarget : TeTextureConfiguration);

  Function GetTextureItemDetail : TeTextureAtlasItem;

  Property TextureData : TeTextureAtlas read FTextureData Write SetTextureData;
  Property TextureItem : String read FTextureItem Write SetTextureItem;
  Property TextureItemIndex : Integer read FTextureItemIndex Write SetTextureItemIndex;
  Property Subscripter : TeCustomMesh read FSubscripter Write FSubscripter;

End;

//------------------------------------------------------------------------------

TeCustomMeshTextured = class(teCustomMesh)
Private
Protected
  FTX1: Single;
  FTY1: Single;
  FTX2: Single;
  FTY2: Single;
  FSourceConfiguration: TeTextureConfiguration;
  Procedure Notification(Sender : TObject); Override;
Public
  Procedure InternalBuildMesh; Virtual; Abstract;
  Constructor Create(AOwner: TComponent); override;
  Destructor Destroy; Override;
Published
  Property SourceConfiguration : TeTextureConfiguration read FSourceConfiguration;
End;

TeImage = Class(TeCustomMeshTextured)
Private
  Procedure InternalBuildMesh; Override;
Public
  Constructor Create(AOwner: TComponent); override;
  Destructor Destroy; Override;
End;

TeSpeedImage = class(TeImage)
private
protected
  FDrawSpeedEffect: Boolean;
  procedure SetDrawSpeedEffect(const Value: Boolean);
public
  Procedure Render(aContext : TContext3D); Override;
  Property DrawSpeedEffect : Boolean read FDrawSpeedEffect Write SetDrawSpeedEffect;
end;


//------------------------------------------------------------------------------
TeRepeatedImage = Class(TeCustomMeshTextured)
Private
  FVertexCapaX : Cardinal;
  FVertexCapaY : Cardinal;

  procedure SetVertexCapaX(const Value: Cardinal);
  procedure SetVertexCapaY(const Value: Cardinal);

  Procedure InternalBuildMesh; Override;

Public
  Constructor Create(AOwner: TComponent); override;
  Destructor Destroy; override;
Published
  Property TextureRepeatX : Cardinal read FVertexCapaX Write SetVertexCapaX;
  Property TextureRepeatY : Cardinal read FVertexCapaY Write SetVertexCapaY;
End;


implementation


{ TeTextureAtlas }

procedure TeTextureAtlas.AddItem(AtitleOfItem: String; X1, Y1, X2, Y2: Single);
var a : TeTextureAtlasItem;
begin
  if FindItem(AtitleOfItem)=-1 then
  begin
    a := TeTextureAtlasItem.Create;
    a.PlainTextName := AtitleOfItem;
    a.TextureSourceX1 := X1;
    a.TextureSourceY1 := Y1;
    a.TextureSourceX2 := X2;
    a.TextureSourceY2 := Y2;
    TextureAtlasItems.Add(a);
  end
  else
    raise Exception.Create('TeTextureAtlas.AddItem : "'+AtitleOfItem+'" already exists : Change its name.');
end;

constructor TeTextureAtlas.Create;
begin
  inherited;
  TextureAtlasItems := TList<TeTextureAtlasItem>.Create;
end;

destructor TeTextureAtlas.Destroy;
begin
  FreeAndNil(TextureAtlasItems);
  inherited;
end;

function TeTextureAtlas.FindItem(ByTextureTitle: String): Integer;
var i : integer;
    a : TeTextureAtlasItem;
begin
  i := -1;
  Result := -1;
  for a in TextureAtlasItems do
  begin
    inc(i);
    if Trim(UpperCase(a.PlainTextName)) = Trim(UpperCase(ByTextureTitle)) then
    begin
      result := i;
      Break;
    end;
  end;
end;

{ TeTextureConfiguration }

procedure TeTextureConfiguration.ApplyConfigurationOn(
  aTarget: TeTextureConfiguration);
begin
  if Assigned(aTarget) then
  begin
    if aTarget<>Self then
    begin
      aTarget.TextureData := TextureData;
      aTarget.TextureItem := TextureItem;
    end;
  end;
end;

constructor TeTextureConfiguration.Create;
begin
  inherited;
  FTextureItem := EmptyStr;
  FTextureItemIndex := -1;
end;

destructor TeTextureConfiguration.Destroy;
begin
  inherited;
end;

function TeTextureConfiguration.GetTextureItemDetail: TeTextureAtlasItem;
begin
  Result := Nil;
  if FTextureItemIndex>-1 then //No need further control : FTextureItemIndex is controled by other prop. and setter.
  begin
    result := TextureData.TextureAtlasItems[FTextureItemIndex];
  end;
  Assert(Assigned(Result));
end;

procedure TeTextureConfiguration.InternalSetTextureItem(
  aTextureItem: TeTextureAtlasItem);
begin
  if assigned(aTextureItem) then
  begin
    if Assigned(FSubscripter) then
    begin
      FSubscripter.Notification(Self);
    end;
  end;
end;

procedure TeTextureConfiguration.SetTextureData(const Value: TeTextureAtlas);
begin
  FTextureItem := 'None';
  FTextureItemIndex := -1;
  FTextureData := Value;
  if Assigned(FTextureData) then
  begin
    if FTextureData.TextureAtlasItems.Count>0 then
    begin
      TextureItemIndex := 0; //ByDefault.  (The editor will set "plain image" (all the picture) on position 0, by default)
    end;
  end;
end;

procedure TeTextureConfiguration.SetTextureItem(const Value: String);
var a : integer;
begin
  FTextureItem := Value;
  if Assigned(FTextureData) then
  begin
    a := FTextureData.FindItem(FTextureItem);
    if a>-1 then
    begin
      FTextureItemIndex := a;
      InternalSetTextureItem(FTextureData.TextureAtlasItems[a]);
    end;
  end
  else
  begin
    FTextureItem := EmptyStr;
    FTextureItemIndex := -1;
  end;
end;

procedure TeTextureConfiguration.SetTextureItemIndex(const Value: Integer);
begin
  if (Value>-1) And (Value < TextureData.TextureAtlasItems.Count) then
  begin
    FTextureItemIndex := Value;
    FTextureItem := TextureData.TextureAtlasItems[Value].PlainTextName;
    InternalSetTextureItem(FTextureData.TextureAtlasItems[FTextureItemIndex]);
  end;
end;

{ TeImage }

constructor TeImage.Create(AOwner: TComponent);
begin
  inherited;
  Data.VertexBuffer.Length := 4;
  Data.IndexBuffer.Length := 6;
  FSourceConfiguration := TeTextureConfiguration.Create;
  FSourceConfiguration.Subscripter := Self;
  InternalBuildMesh;
end;

destructor TeImage.Destroy;
begin
  FreeAndNil(FSourceConfiguration);
  inherited;
end;

{
procedure TeImage.DrawOverlap;
begin
  Context.DrawCube(NullVector3D, Vector3D(Width, Height, 0.01), AbsoluteOpacity, TAlphaColorRec.Blue);
end;
}

procedure TeImage.InternalBuildMesh;
begin
  Data.VertexBuffer.Vertices[0] := Point3D(-0.5 * Width, -0.5 * Height, 0);
  Data.VertexBuffer.Vertices[1] := Point3D(0.5 * Width, -0.5 * Height, 0);
  Data.VertexBuffer.Vertices[2] := Point3D(0.5 * Width, 0.5 * Height, 0);
  Data.VertexBuffer.Vertices[3] := Point3D(-0.5 * Width, 0.5, 0);

  Data.VertexBuffer.TexCoord0[0] := PointF(FTX1, FTY1);
  Data.VertexBuffer.TexCoord0[1] := PointF(FTX2, FTY1);
  Data.VertexBuffer.TexCoord0[2] := PointF(FTX2, FTY2);
  Data.VertexBuffer.TexCoord0[3] := PointF(FTX1, FTY2);

  Data.IndexBuffer[0] := 0;
  Data.IndexBuffer[1] := 1;
  Data.IndexBuffer[2] := 2;
  Data.IndexBuffer[3] := 2;
  Data.IndexBuffer[4] := 3;
  Data.IndexBuffer[5] := 0;

  ProcessBoundingBox;
end;




{ TeRepeatedImage }

constructor TeRepeatedImage.Create(AOwner: TComponent);
begin
  Inherited;
  FVertexCapaX := 5;
  FVertexCapaY := 5;

  Data.VertexBuffer.Length := 4 * FVertexCapaX * FVertexCapaY;
  Data.IndexBuffer.Length := 6 * FVertexCapaX * FVertexCapaY;

  FTX1 := 0;
  FTY1 := 0;
  FTX2 := 1;
  FTY2 := 1;

  FSourceConfiguration := TeTextureConfiguration.Create;
  FSourceConfiguration.Subscripter := Self;

  InternalBuildMesh;
end;

destructor TeRepeatedImage.Destroy;
begin
  FreeAndNil(FSourceConfiguration);
  inherited;
end;

procedure TeRepeatedImage.InternalBuildMesh;
var i,j : integer;
    fid : integer;
    fidPrime : integer;

    ref0 : TPoint3d;
    ref1 : TPoint3d;
    ref2 : TPoint3d;
    ref3 : TPoint3d;

    tex0 : TPointF;
    tex1 : TPointF;
    tex2 : TPointF;
    tex3 : TPointF;

    idx0 : Integer;
    idx1 : Integer;
    idx2 : Integer;
    idx3 : Integer;
    idx4 : Integer;
    idx5 : Integer;

    ax,ay : Single;

begin
  Data.VertexBuffer.Length := 4 * FVertexCapaX * FVertexCapaY;
  Data.IndexBuffer.Length := 6 * FVertexCapaX * FVertexCapaY;

  ref0 := Point3D(-0.5 * Width, -0.5 * Height, 0);
  ref1 := Point3D(0.5 * Width, -0.5 * Height, 0);
  ref2 := Point3D(0.5 * Width, 0.5 * Height, 0);
  ref3 := Point3D(-0.5 * Width, 0.5, 0);

  Tex0 := PointF(FTX1, FTY1);
  Tex1 := PointF(FTX2, FTY1);
  Tex2 := PointF(FTX2, FTY2);
  Tex3 := PointF(FTX1, FTY2);

  Idx0 := 0;
  Idx1 := 1;
  Idx2 := 2;
  Idx3 := 2;
  Idx4 := 3;
  Idx5 := 0;

  fid := 0;
  for j := 0 to FVertexCapaY-1 do
  begin
    for I := 0 to FVertexCapaX-1 do
    begin
      ax := i;
      ay := j;
      Data.VertexBuffer.Vertices[fid] := Point3d(ref0.X + ax, ref0.Y + ay, 0);
      Data.VertexBuffer.Vertices[fid+1] := Point3d(ref1.X + ax, ref1.Y + ay, 0);
      Data.VertexBuffer.Vertices[fid+2] := Point3d(ref2.X + ax, ref2.Y + ay, 0);
      Data.VertexBuffer.Vertices[fid+3] := Point3d(ref3.X + ax, ref3.Y + ay, 0);

      Data.VertexBuffer.TexCoord0[fid] := Tex0;
      Data.VertexBuffer.TexCoord0[fid+1] := Tex1;
      Data.VertexBuffer.TexCoord0[fid+2] := Tex2;
      Data.VertexBuffer.TexCoord0[fid+3] := Tex3;
      fid := fid + 4;
    end;
  end;


  fid := 0;
  fidPrime := 0;
  for j := 0 to FVertexCapaY-1 do
  begin
    for I := 0 to FVertexCapaX-1 do
    begin
      Data.IndexBuffer[fid] := fidPrime + idx0;
      Data.IndexBuffer[fid+1] := fidPrime + idx1;
      Data.IndexBuffer[fid+2] := fidPrime + idx2;
      Data.IndexBuffer[fid+3] := fidPrime + idx3;
      Data.IndexBuffer[fid+4] := fidPrime + idx4;
      Data.IndexBuffer[fid+5] := fidPrime + idx5;

      fid := fid + 6;
      fidPrime := fidPrime + 4;
    end;
  end;


end;


procedure TeRepeatedImage.SetVertexCapaX(const Value: Cardinal);
begin
  FVertexCapaX := Value;
  if FVertexCapaX=0 then
    FVertexCapaX := 1;
  InternalBuildMesh;
  Repaint;
end;

procedure TeRepeatedImage.SetVertexCapaY(const Value: Cardinal);
begin
  FVertexCapaY := Value;
  if FVertexCapaY=0 then
    FVertexCapaY := 1;
  InternalBuildMesh;
  Repaint;
end;

{ TeSpeedImage }

procedure TeSpeedImage.Render(aContext: TContext3D);
var i : integer;
begin
  {$IFDEF VER260}
  aContext.SetMatrix(TMatrix3D.CreateScaling(Point3D(Width, Height, Depth)) * AbsoluteMatrix);
  {$ENDIF}

  {$IF Defined(VER280) or Defined(VER310) or Defined(VER320)}
  aContext.SetMatrix(TMatrix3D.CreateScaling(Point3D(Width, Height, Depth)) * AbsoluteMatrix);
  {$ENDIF}

  {$IFDEF VER240}
  aContext.SetMatrix(Matrix3DMultiply(CreateScaleMatrix3D(Vector3D(Width, Height, Depth)), AbsoluteMatrix));
  {$ENDIF}

  Data.VertexBuffer.Vertices[0] := Point3D(-0.5 * Width, -0.5 * Height, 0);
  Data.VertexBuffer.Vertices[1] := Point3D(0.5 * Width, -0.5 * Height, 0);
  Data.VertexBuffer.Vertices[2] := Point3D(0.5 * Width, 0.5 * Height, 0);
  Data.VertexBuffer.Vertices[3] := Point3D(-0.5 * Width, 0.5, 0);

  if DrawSpeedEffect then
  begin
    for i := 10 downto 1 do
    begin
      Data.VertexBuffer.Vertices[0] := Point3d(Data.VertexBuffer.Vertices[0].X + i / 100, Data.VertexBuffer.Vertices[0].Y, 0);
      Data.VertexBuffer.Vertices[1] := Point3d(Data.VertexBuffer.Vertices[1].X + i / 100, Data.VertexBuffer.Vertices[1].Y, 0);
      Data.VertexBuffer.Vertices[2] := Point3d(Data.VertexBuffer.Vertices[2].X + i / 100, Data.VertexBuffer.Vertices[2].Y, 0);
      Data.VertexBuffer.Vertices[3] := Point3d(Data.VertexBuffer.Vertices[3].X + i / 100, Data.VertexBuffer.Vertices[3].Y, 0);
      aContext.DrawTriangles(Data.VertexBuffer, Data.IndexBuffer, SourceConfiguration.TextureData.MaterialSource.Material, 0.3);
    end;
  end
  else
  begin
    aContext.DrawTriangles(Data.VertexBuffer, Data.IndexBuffer, SourceConfiguration.TextureData.MaterialSource.Material, AbsoluteOpacity);
  end;
end;

procedure TeSpeedImage.SetDrawSpeedEffect(const Value: Boolean);
begin
  FDrawSpeedEffect := Value;
  Repaint;
end;

{ TeCustomMeshTextured }

constructor TeCustomMeshTextured.Create(AOwner: TComponent);
begin
  Inherited;
  FSourceConfiguration := TeTextureConfiguration.Create;
  FSourceConfiguration.Subscripter := Self;
end;

destructor TeCustomMeshTextured.Destroy;
begin
  FreeAndNil(FSourceConfiguration);
  inherited;
end;

procedure TeCustomMeshTextured.Notification(Sender: TObject);
var a : integer;
begin
  if Sender = SourceConfiguration then //Ping from our SourceConfiguration object.
  begin
    //Something change on source configuration. Update all related data.
    MaterialSource := SourceConfiguration.TextureData.MaterialSource;
    if SourceConfiguration.TextureItemIndex>-1 then
    begin
      a := SourceConfiguration.TextureItemIndex;
      FTX1 := SourceConfiguration.TextureData.TextureAtlasItems[a].TextureSourceX1;
      FTY1 := SourceConfiguration.TextureData.TextureAtlasItems[a].TextureSourceY1;
      FTX2 := SourceConfiguration.TextureData.TextureAtlasItems[a].TextureSourceX2;
      FTY2 := SourceConfiguration.TextureData.TextureAtlasItems[a].TextureSourceY2;
      InternalBuildMesh;
      Repaint;
    end;
  end;
end;

end.
