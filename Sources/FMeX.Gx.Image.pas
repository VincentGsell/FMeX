unit FMeX.Gx.Image;


interface

Uses
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  System.Math.Vectors,
  FMX.Types3D,
  FMX.Objects3D,
  FMX.Materials,
  FMX.Graphics,
  FMX.Controls3D,
  FMX.MaterialSources,
  FmeX.Images {For Atlas only. Todo : Make a atlas unit},
  FMeX.Gx.Types;

Type
TeMeshTextureConfiguration = class(TeTextureConfiguration)
private
protected
  FSubscripter: TeMesh3D;
  Procedure InternalSetTextureItem(aTextureItem : TeTextureAtlasItem); override;
public
  Property Subscripter : TeMesh3D read FSubscripter Write FSubscripter;
end;


TeCtrlImageTextured = class(TeMesh3D)
Private
Protected
  FTX1: Single;
  FTY1: Single;
  FTX2: Single;
  FTY2: Single;
  FSourceConfiguration: TeMeshTextureConfiguration;
  Procedure MeshNotification(Sender : TObject); Override;
Public
  Constructor Create; override;
  Destructor Destroy; Override;
Published
  Property SourceConfiguration : TeMeshTextureConfiguration read FSourceConfiguration;
End;


TeCtrlImage = Class(TeCtrlImageTextured)
private
protected
  Procedure BuildMesh; Override;
public
  Constructor Create; override;
End;

//------------------------------------------------------------------------------
TeCtrlRepeatedImage = Class(TeCtrlImageTextured)
Private
  FVertexCapaX : Cardinal;
  FVertexCapaY : Cardinal;

  procedure SetVertexCapaX(const Value: Cardinal);
  procedure SetVertexCapaY(const Value: Cardinal);

  Procedure BuildMesh; Override;

Public
  constructor Create; override;
Published
  Property TextureRepeatX : Cardinal read FVertexCapaX Write SetVertexCapaX;
  Property TextureRepeatY : Cardinal read FVertexCapaY Write SetVertexCapaY;
End;



implementation

{ TeMesh3DTextured }

constructor TeCtrlImageTextured.Create;
begin
  inherited;
  FSourceConfiguration := TeMeshTextureConfiguration.Create;
  FSourceConfiguration.Subscripter := Self;
end;

destructor TeCtrlImageTextured.Destroy;
begin
  FreeAndNil(FSourceConfiguration);
  inherited;
end;

procedure TeCtrlImageTextured.MeshNotification(Sender: TObject);
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
      BuildMesh; //Todo : Only update Texture side in mesh.
    end;
  end;
end;

{ TeCtrlImage }

constructor TeCtrlImage.Create;
begin
  inherited;
  FData.VertexBuffer.Length := 4;
  FData.IndexBuffer.Length := 6;
  BuildMesh;
end;

procedure TeCtrlImage.BuildMesh;
begin
  Inherited;
  FData.VertexBuffer.Vertices[0] := Point3D(-0.5 * Width, -0.5 * Height, 0);
  FData.VertexBuffer.Vertices[1] := Point3D(0.5 * Width, -0.5 * Height, 0);
  FData.VertexBuffer.Vertices[2] := Point3D(0.5 * Width, 0.5 * Height, 0);
  FData.VertexBuffer.Vertices[3] := Point3D(-0.5 * Width, 0.5 * Height, 0);
                                                            //TODO : Fix with/heigh :/
  FData.VertexBuffer.Vertices[0] := Point3D(-0.5, -0.5, 0);
  FData.VertexBuffer.Vertices[1] := Point3D(0.5 , -0.5, 0);
  FData.VertexBuffer.Vertices[2] := Point3D(0.5 , 0.5, 0);
  FData.VertexBuffer.Vertices[3] := Point3D(-0.5, 0.5, 0);

  FData.VertexBuffer.TexCoord0[0] := PointF(FTX1, FTY1);
  FData.VertexBuffer.TexCoord0[1] := PointF(FTX2, FTY1);
  FData.VertexBuffer.TexCoord0[2] := PointF(FTX2, FTY2);
  FData.VertexBuffer.TexCoord0[3] := PointF(FTX1, FTY2);

  FData.IndexBuffer[0] := 0;
  FData.IndexBuffer[1] := 1;
  FData.IndexBuffer[2] := 2;
  FData.IndexBuffer[3] := 2;
  FData.IndexBuffer[4] := 3;
  FData.IndexBuffer[5] := 0;
end;

{ TeMeshTextureConfiguration }

procedure TeMeshTextureConfiguration.InternalSetTextureItem(
  aTextureItem: TeTextureAtlasItem);
begin
  if assigned(aTextureItem) then
  begin
    if Assigned(FSubscripter) then
    begin
      FSubscripter.MeshNotification(Self);
    end;
  end;
end;

{ TeCtrlRepeatedImage }

procedure TeCtrlRepeatedImage.BuildMesh;
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
  Inherited;
  FData.VertexBuffer.Length := 4 * FVertexCapaX * FVertexCapaY;
  FData.IndexBuffer.Length := 6 * FVertexCapaX * FVertexCapaY;

  ref0 := Point3D(-0.5 * Width, -0.5 * Height, 0);
  ref1 := Point3D(0.5 * Width, -0.5 * Height, 0);
  ref2 := Point3D(0.5 * Width, 0.5 * Height, 0);
  ref3 := Point3D(-0.5 * Width, 0.5, 0);
                                             //Todo : Fix With Height :/
  ref0 := Point3D(-0.5 , -0.5 , 0);
  ref1 := Point3D(0.5 , -0.5 , 0);
  ref2 := Point3D(0.5 , 0.5 , 0);
  ref3 := Point3D(-0.5 , 0.5, 0);


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
      FData.VertexBuffer.Vertices[fid] := Point3d(ref0.X + ax, ref0.Y + ay, 0);
      FData.VertexBuffer.Vertices[fid+1] := Point3d(ref1.X + ax, ref1.Y + ay, 0);
      FData.VertexBuffer.Vertices[fid+2] := Point3d(ref2.X + ax, ref2.Y + ay, 0);
      FData.VertexBuffer.Vertices[fid+3] := Point3d(ref3.X + ax, ref3.Y + ay, 0);

      FData.VertexBuffer.TexCoord0[fid] := Tex0;
      FData.VertexBuffer.TexCoord0[fid+1] := Tex1;
      FData.VertexBuffer.TexCoord0[fid+2] := Tex2;
      FData.VertexBuffer.TexCoord0[fid+3] := Tex3;
      fid := fid + 4;
    end;
  end;


  fid := 0;
  fidPrime := 0;
  for j := 0 to FVertexCapaY-1 do
  begin
    for I := 0 to FVertexCapaX-1 do
    begin
      FData.IndexBuffer[fid] := fidPrime + idx0;
      FData.IndexBuffer[fid+1] := fidPrime + idx1;
      FData.IndexBuffer[fid+2] := fidPrime + idx2;
      FData.IndexBuffer[fid+3] := fidPrime + idx3;
      FData.IndexBuffer[fid+4] := fidPrime + idx4;
      FData.IndexBuffer[fid+5] := fidPrime + idx5;

      fid := fid + 6;
      fidPrime := fidPrime + 4;
    end;
  end;

end;

constructor TeCtrlRepeatedImage.Create;
begin
  inherited;
  FVertexCapaX := 5;
  FVertexCapaY := 5;

  FTX1 := 0;
  FTY1 := 0;
  FTX2 := 1;
  FTY2 := 1;

  BuildMesh;
end;

procedure TeCtrlRepeatedImage.SetVertexCapaX(const Value: Cardinal);
begin
  FVertexCapaX := Value;
  if FVertexCapaX=0 then
    FVertexCapaX := 1;
  BuildMesh;
end;

procedure TeCtrlRepeatedImage.SetVertexCapaY(const Value: Cardinal);
begin
  FVertexCapaY := Value;
  if FVertexCapaY=0 then
    FVertexCapaY := 1;
  BuildMesh;
end;

end.
