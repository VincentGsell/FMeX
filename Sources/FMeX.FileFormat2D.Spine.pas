unit FMeX.FileFormat2D.Spine;

interface

uses
  Classes,
  SysUtils, System.Types,
  FMX.Materials, FMX.MaterialSources ,FMX.Graphics,
  Spine,
  FMeX.Types3D, FMX.Types3D, System.Math.Vectors, FMeX.Ticks, FMeX.Images;

Type
  TFMeXSpineTexture = class(TSpineTexture)
  public
    Texture: TMaterialSource;
    constructor Create(const TextureName: AnsiString);
    destructor Destroy; override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
  end;

  TFMeXSpineTextureLoader = class(TSpineTextureLoader)
  Private
    FPath: String;
  public
    function LoadTexture(const TextureName: AnsiString): TSpineTexture; override;
    Property Path : String read FPath Write FPath;
  end;

  TFMeXSpineRender = class(TSpineRender)
  private
    FInput : TeImage;
  public
    Output : TeImage; //pointer

    Constructor Create;
    procedure Render(const Texture: TSpineTexture; const Vertices: PSpineVertexArray); override;
  end;


  TFMeXSpineObject = Class
  Private
    Skeleton : TSpineSkeleton;
    Renderer : TFMeXSpineRender;
    FAnimations : Array of TSpineAnimation;
    FGlobalTimeAdapter: Single;
    FCurrentSkin: String;
    FAnimationIndex: Integer;
    procedure SetCurrentSkin(const Value: String);
    function GetAnimation(Index: Integer): TSpineAnimation;
    procedure SetAnimationIndex(const Value: Integer);
  Public
    Constructor Create;
    Procedure Init;
    procedure Adapt(OffsetCorrectionX, OffsetCorrectionY, ScaleCorrectionX, ScaleCorrectionY : Single);

    Procedure Update;
    Procedure Draw(aImage : TeImage);

    Property GlobalTimeAdapter : Single read FGlobalTimeAdapter Write FGlobalTimeAdapter;
    Property CurrentSkin : String read FCurrentSkin Write SetCurrentSkin;
    Property Animations[Index : Integer] : TSpineAnimation read GetAnimation;
    property CurrentAnimationIndex : Integer read FAnimationIndex Write SetAnimationIndex;

  End;


Implementation


{ TFMeXSpineTexture }

constructor TFMeXSpineTexture.Create(const TextureName: AnsiString);
begin
  if Assigned(Texture) then
  begin
    FreeAndNil(Texture);
  end;
  Texture := TTextureMaterialSource.Create(nil);
  TTextureMaterialSource(Texture).texture.LoadFromFile(TextureName);
end;

destructor TFMeXSpineTexture.Destroy;
begin
  FreeAndNil(Texture);
  inherited;
end;

function TFMeXSpineTexture.GetHeight: Integer;
begin
  Result := TTextureMaterialSource(Texture).Texture.Height;
end;

function TFMeXSpineTexture.GetWidth: Integer;
begin
  Result := TTextureMaterialSource(Texture).Texture.Width;
end;

{ TFMeXSpineTextureLoader }

function TFMeXSpineTextureLoader.LoadTexture(
  const TextureName: AnsiString): TSpineTexture;
begin
  Result := TFMeXSpineTexture.Create(Path + TextureName);
end;

{ TFMeXSpineRender }

constructor TFMeXSpineRender.Create;
begin
  Inherited;
  FInput := TeImage.Create(nil);

//  {$IFDEF VER280}
//  Ver := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4);
//  {$ELSE}
//  Ver := TVertexBuffer.Create([TVertexFormat.vfVertex, TVertexFormat.vfTexCoord0], 4);
//  {$ENDIF}
//  Idx := TIndexBuffer.Create(6);

end;

procedure TFMeXSpineRender.Render(const Texture: TSpineTexture;
  const Vertices: PSpineVertexArray);
var
  pv: PSpineVertexArray absolute Vertices;
  //vert, text: array of zglTPoint2D;

//  Mat := TColorMaterial.Create;
//  Mat.Color := TAlphaColorRec.Blue;
//  ZWrite := True;
//  TwoSide := True;

begin
  inherited;
  //raise Exception.Create(IntToStr(High(Vertices^)));

  // Vertices
//SetLength(vert, 6);
  // first triangle


  FInput.Data.VertexBuffer.Vertices[0] := Point3d(pv^[0].x, pv^[0].y, 0);
  FInput.Data.VertexBuffer.Vertices[1] := Point3d(pv^[1].x, pv^[1].y, 0);
  FInput.Data.VertexBuffer.Vertices[2] := Point3d(pv^[2].x, pv^[2].y, 0);
  FInput.Data.VertexBuffer.Vertices[3] := Point3d(pv^[3].x, pv^[3].y, 0);

  // second triangle

//  FInput.Data.VertexBuffer.FInput.Data.VertexBuffertices[3] := Point3d(pv^[0].x, pv^[0].y, 0);
//  FInput.Data.VertexBuffer.FInput.Data.VertexBuffertices[4] := Point3d(pv^[2].x, pv^[2].y, 0);
//  FInput.Data.VertexBuffer.FInput.Data.VertexBuffertices[5] := Point3d(pv^[3].x, pv^[3].y, 0);

  // Texture
  //SetLength(text, 6);
  // first triangle
  FInput.Data.VertexBuffer.TexCoord0[0] := Pointf(pv^[0].u, pv^[0].v);
  FInput.Data.VertexBuffer.TexCoord0[1] := Pointf(pv^[1].u, pv^[1].v);
  FInput.Data.VertexBuffer.TexCoord0[2] := Pointf(pv^[2].u, pv^[2].v);
  FInput.Data.VertexBuffer.TexCoord0[3] := Pointf(pv^[3].u, pv^[3].v);
  // second triangle

  //FInput.Data.VertexBuffer.TexCoord0[3] := Pointf(pv^[0].u, 1 - pv^[0].v);
  //FInput.Data.VertexBuffer.TexCoord0[4] := Pointf(pv^[2].u, 1 - pv^[2].v);
  //FInput.Data.VertexBuffer.TexCoord0[5] := Pointf(pv^[3].u, 1 - pv^[3].v);

  FInput.Data.IndexBuffer[0] := 0;
  FInput.Data.IndexBuffer[1] := 1;
  FInput.Data.IndexBuffer[2] := 3;
  FInput.Data.IndexBuffer[3] := 1;
  FInput.Data.IndexBuffer[4] := 2;
  FInput.Data.IndexBuffer[5] := 3;


  Output.MaterialSource := TFMeXSpineTexture(Texture).Texture;
  Output.MergeFrom(FInput);
  //Context.DrawTriangles(Ver,Idx,TFMeXSpineTexture(Texture).Texture.Material,1.0);

  //pr2d_TriList(TG2SpineTexture(Texture).Texture, @vert[0], @text[0], 0, length(vert) - 1, $FFFFFF, 255, FX_BLEND or PR2D_FILL);

end;




{ TFMeXSpineObject }

procedure TFMeXSpineObject.Adapt(OffsetCorrectionX, OffsetCorrectionY,
  ScaleCorrectionX, ScaleCorrectionY : Single);
var b : TSpineBone;
begin
  b := Skeleton.GetRootBone;
  b.x := OffsetCorrectionX;
  b.y := OffsetCorrectionY;
  b.ScaleX := ScaleCorrectionX;
  b.ScaleY := ScaleCorrectionY;
  Skeleton.UpdateWorldTransform;
end;

constructor TFMeXSpineObject.Create;
begin
  Inherited;
  FGlobalTimeAdapter := 0.001;
end;

procedure TFMeXSpineObject.Draw(aImage : TeImage);
begin
  Renderer.Output := aImage;
  Renderer.Output.Data.Clear;
  Skeleton.Draw(Renderer);
end;

function TFMeXSpineObject.GetAnimation(Index: Integer): TSpineAnimation;
begin
  result := FAnimations[index];
end;


procedure TFMeXSpineObject.Init;
const
  CharacterName = 'goblins';
var
  tl : TFMeXSpineTextureLoader;
  TextureAtlas : TSpineTextureAtlas;
  sb : TSpineSkeletonBinary;
  sd : TSpineSkeletonData;

  path : string;
  i : integer;
begin

  path := '';

  tl := TFMeXSpineTextureLoader.Create;
  tl.Path := path;
  TextureAtlas := TSpineTextureAtlas.Create(path + CharacterName + '.atlas', tl);
  tl.Free;

  sb := TSpineSkeletonBinary.Create(TextureAtlas);
  sd := sb.ReadSkeletonData(path + CharacterName + '.skel');
  sb.Free;

  Skeleton := TSpineSkeleton.Create(sd);
  SetLength(FAnimations,Length(sd.Animations));
  for I := Low(sd.Animations) to High(sd.Animations) do
    FAnimations[i] := sd.Animations[i];
  CurrentAnimationIndex := 0;
  Renderer := TFMeXSpineRender.Create;
end;

procedure TFMeXSpineObject.SetAnimationIndex(const Value: Integer);
begin
  FAnimationIndex := 0;
  if (Value<=High(Fanimations)) and (Value>=Low(Fanimations)) then
    FAnimationIndex := Value;
end;

procedure TFMeXSpineObject.SetCurrentSkin(const Value: String);
begin
  FCurrentSkin := Value;
  Skeleton.SetSkin(FCurrentSkin);
  Skeleton.SetToBindPose;
end;

procedure TFMeXSpineObject.Update;
begin
  if Assigned(Animations[CurrentAnimationIndex]) then
  begin
    Animations[CurrentAnimationIndex].Apply(Skeleton,TheCadencer.Tic  * FGlobalTimeAdapter,true);
    Skeleton.UpdateWorldTransform;
  end;
end;

end.
