////////////////////////////////////////////////////////////////////////////////
//Generic pascal runtime v1.0 for Spine animation tool                        //
//Runtime port by Dan (dan.soft.studio@gmail.com)                             //
//                                                                            //
//Lisence:                                                                    //
//  The contents of this software are used with permission, subject to        //
//  the Mozilla Public License Version 1.1 (the "License"); you may           //
//  not use this software except in compliance with the License. You may      //
//  obtain a copy of the License at                                           //
//  http://www.mozilla.org/MPL/MPL-1.1.html                                   //
//                                                                            //
//  Software distributed under the License is distributed on an               //
//  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or            //
//  implied. See the License for the specific language governing              //
//  rights and limitations under the License.                                 //
//                                                                            //
//Usage:                                                                      //
//  The end user of the runtime must override 3 classes:                      //
//  TSpineTexture                                                             //
//    GetWidth - needs to provide the width of the texture                    //
//    GetHeight - needs to provice the height of the texture                  //
//                                                                            //
//  TSpineTextureLoader                                                       //
//    LoadTexture - needs to load a texture useing the TextureName parameter  //
//                                                                            //
//  TSpineRender                                                              //
//    Render - needs to render the data provided by this runtime              //
//                                                                            //
//History:                                                                    //
//  08/03/2013 - 1.0 - First release                                          //
//  24/04/2013 - Skeleton format has been updated to include the animations   //
////////////////////////////////////////////////////////////////////////////////
unit Spine;

interface

uses
  Classes,
  SysUtils;

type
  TSpineRender = class;
  TSpineSkeleton = class;
  TSpineSkeletonData = class;
  TSpineSlot = class;
  TSpineSlotData = class;
  TSpineBone = class;
  TSpineBoneData = class;
  TSpineSkin = class;
  TSpineAttachment = class;
  TSpineRegionAttachment = class;
  TSpineRegionSequenceAttachment = class;
  TSpineAttachmentResolver = class;
  TSpineTextureAtlasAttachmentResolver = class;
  TSpineAnimation = class;
  TSpineTimeline = class;
  TSpineCurveTimeline = class;
  TSpineRotateTimeline = class;
  TSpineTranslateTimeline = class;
  TSpineScaleTimeline = class;
  TSpineColorTimeline = class;
  TSpineAttachmentTimeline = class;
  TSpineTexture = class;

  TSpineFloatArray = array[Word] of Single;
  PSpineFloatArray = ^TSpineFloatArray;

  TSpineColor = object
  public
    r, g, b, a: Single;
  end;

  TSpineVertexData = packed record
    x, y, u, v, r, g, b, a: Single;
  end;
  TSpineVertexArray = array[Word] of TSpineVertexData;
  PSpineVertexArray = ^TSpineVertexArray;

  TSpineTexture = class
  public
    Ref: Integer;
    Name: AnsiString;
    function GetWidth: Integer; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    procedure RefInc;
    procedure RefDec;
  end;

  TSpineTextureLoader = class
  public
    function LoadTexture(const TextureName: AnsiString): TSpineTexture; virtual; abstract;
  end;

  TSpineRender = class
  public
    procedure Render(const Texture: TSpineTexture; const Vertices: PSpineVertexArray); virtual; abstract;
  end;

  TSpineQuickList = {$ifndef fpc}record{$else}object{$endif}
  private
    _Items: array of Pointer;
    _ItemCount: Integer;
    procedure SetItem(const Index: Integer; const Value: Pointer); inline;
    function GetItem(const Index: Integer): Pointer; inline;
    procedure SetCapacity(const Value: Integer); inline;
    function GetCapacity: Integer; inline;
    function GetFirst: Pointer; inline;
    function GetLast: Pointer; inline;
  public
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read _ItemCount;
    property Items[const Index: Integer]: Pointer read GetItem write SetItem; default;
    property First: Pointer read GetFirst;
    property Last: Pointer read GetLast;
    function Add(const Item: Pointer): Integer;
    function Pop: Pointer;
    function Insert(const Index: Integer; const Item: Pointer): Integer;
    procedure Delete(const Index: Integer);
    procedure Remove(const Item: Pointer);
    procedure Clear;
  end;
  PSpineQuickList = ^TSpineQuickList;

  TSpineObject = class
  protected
    procedure ObjectStore;
    procedure ObjectRemove;
  public
    destructor Destroy; override;
  end;

  TSpineAtlasPageFormat = (spfAlpha, spfIntensity, spfLuminanceAlpha, spfRGB565, spfRGBA4444, spfRGB888, spfRGBA8888);
  TSpineAtlasPageTextureFilter = (stfNearest, stfLinear, stfMipMap);
  TSpineAtlasPageWrap = (spwClamp, spwRepeat);

  TSpineAtlasPage = class (TSpineObject)
  public
    Name: AnsiString;
    Texture: TSpineTexture;
    UseMipMaps: Boolean;
    Format: TSpineAtlasPageFormat;
    MinFilter: TSpineAtlasPageTextureFilter;
    MagFilter: TSpineAtlasPageTextureFilter;
    WrapU: TSpineAtlasPageWrap;
    WrapV: TSpineAtlasPageWrap;
    constructor Create;
    destructor Destroy; override;
  end;

  TSpineTextureRegion = class (TSpineObject)
  public
    Texture: TSpineTexture;
    u, v, u2, v2: Single;
    RegionWidth, RegionHeight: Integer;
    constructor Create;
  end;

  TSpineAtlasRegion = class (TSpineTextureRegion)
  public
    Page: TSpineAtlasPage;
    Index: Integer;
    Name: AnsiString;
    OffsetX: Single;
    OffsetY: Single;
    PackedWidth: Integer;
    PackedHeight: Integer;
    OriginalWidth: Integer;
    OriginalHeight: Integer;
    Rotate: Boolean;
  end;

  TSpineTextureAtlas = class (TSpineObject)
  public
    Pages: TSpineQuickList;
    Regions: TSpineQuickList;
    constructor Create(const AtlasData: TStream; const TextureLoader: TSpineTextureLoader); overload;
    constructor Create(const AtlasFile: String; const TextureLoader: TSpineTextureLoader); overload;
    destructor Destroy; override;
    function FindRegion(const RegionName: AnsiString): TSpineAtlasRegion;
  end;

  TSpineInputData = class (TSpineObject)
  private
    _Stream: TStream;
    _StartPos: Int64;
    _Chars: array of AnsiChar;
  public
    property Stream: TStream read _Stream;
    constructor Create(const HijackStream: TStream);
    procedure Reset;
    function Read: Integer;
    function ReadInt(const OptimizePositive: Boolean): Integer;
    function ReadString: AnsiString;
    function ReadFloat: Single;
    function ReadColor: TSpineColor;
  end;

  TSpineSkeletonBinary = class  (TSpineObject)
  private
    function ReadSkin(const Input: TSpineInputData; SkinName: AnsiString): TSpineSkin;
    function ReadAttachment(const Input: TSpineInputData; AttachmentName: AnsiString): TSpineAttachment;
    procedure ReadCurve(const Input: TSpineInputData; KeyframeIndex: Integer; Timeline: TSpineCurveTimeline);
    function ReadAnimation(const Input: TSpineInputData; const SkeletonData: TSpineSkeletonData): TSpineAnimation;
  public
    AttachmentResolver: TSpineAttachmentResolver;
    Scale: Single;
    constructor Create(const Atlas: TSpineTextureAtlas); overload;
    constructor Create(const NewAttachmentResolver: TSpineAttachmentResolver); overload;
    destructor Destroy; override;
    function ReadSkeletonData(const Stream: TStream): TSpineSkeletonData; overload;
    function ReadSkeletonData(const FileName: String): TSpineSkeletonData; overload;
  end;

  TSpineSkeleton = class  (TSpineObject)
  public
    Data: TSpineSkeletonData;
    Bones: array of TSpineBone;
    Slots: array of TSpineSlot;
    DrawOrder: array of TSpineSlot;
    Skin: TSpineSkin;
    Color: TSpineColor;
    Time: Single;
    FlipX, FlipY: Boolean;
    constructor Create(const NewData: TSpineSkeletonData); overload;
    constructor Create(const NewSkeleton: TSpineSkeleton); overload;
    destructor Destroy; override;
    procedure UpdateWorldTransform;
    procedure SetToBindPose;
    procedure SetBonesToBindPose;
    procedure SetSlotsToBindPose;
    function GetRootBone: TSpineBone;
    function FindBone(const BoneName: AnsiString): TSpineBone;
    function FindBoneIndex(const BoneName: AnsiString): Integer;
    function FindSlot(const SlotName: AnsiString): TSpineSlot;
    function FindSlotIndex(const SlotName: AnsiString): Integer;
    procedure SetSkin(const SkinName: AnsiString); overload;
    procedure SetSkin(const NewSkin: TSpineSkin); overload;
    function GetAttachment(const SlotName, AttachmentName: AnsiString): TSpineAttachment; overload;
    function GetAttachment(const SlotIndex: Integer; const AttachmentName: AnsiString): TSpineAttachment; overload;
    procedure SetAttachment(const SlotName, AttachmentName: AnsiString);
    procedure Update(const Delta: Single);
    procedure Draw(const Render: TSpineRender);
  end;

  TSpineSkeletonData = class  (TSpineObject)
  public
    Bones: array of TSpineBoneData;
    Slots: array of TSpineSlotData;
    Skins: array of TSpineSkin;
    Animations: array of TSpineAnimation;
    DefaultSkin: TSpineSkin;
    AttachmentResolver: TSpineAttachmentResolver;
    constructor Create(const NewAttachmentResolver: TSpineAttachmentResolver); overload;
    constructor Create(const NewSkeletonData: TSpineSkeletonData); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure AddBone(const Bone: TSpineBoneData);
    function FindBone(const BoneName: AnsiString): TSpineBoneData;
    function FindBoneIndex(const BoneName: AnsiString): Integer;
    procedure AddSlot(const Slot: TSpineSlotData);
    function FindSlot(const SlotName: AnsiString): TSpineSlotData;
    function FindSlotIndex(const SlotName: AnsiString): Integer;
    procedure AddSkin(const Skin: TSpineSkin);
    function FindSkin(const SkinName: AnsiString): TSpineSkin;
    procedure AddAnimation(const Animation: TSpineAnimation);
    function FindAnimation(const AnimationName: AnsiString): TSpineAnimation;
  end;

  TSpineSlot = class  (TSpineObject)
  private
    _Attachment: TSpineAttachment;
    _AttachmentTime: Single;
    procedure SetAttachment(const Value: TSpineAttachment);
    procedure SetAttachmentTime(const Value: Single);
  public
    Data: TSpineSlotData;
    Bone: TSpineBone;
    Skeleton: TSpineSkeleton;
    Color: TSpineColor;
    property Attachment: TSpineAttachment read _Attachment write SetAttachment;
    property AttachmentTime: Single read _AttachmentTime write SetAttachmentTime;
    constructor Create(const NewData: TSpineSlotData; const NewSkeleton: TSpineSkeleton; const NewBone: TSpineBone); overload;
    constructor Create(const NewSlot: TSpineSlot; const NewSkeleton: TSpineSkeleton; const NewBone: TSpineBone); overload;
    destructor Destroy; override;
    procedure SetToBindPose(const SlotIndex: Integer); overload;
    procedure SetToBindPose; overload;
  end;

  TSpineSlotData = class  (TSpineObject)
  public
    Ref: Integer;
    Name: AnsiString;
    BoneData: TSpineBoneData;
    Color: TSpineColor;
    AttachmentName: AnsiString;
    constructor Create(const NewName: AnsiString; const NewBoneData: TSpineBoneData);
    procedure RefInc;
    procedure RefDec;
  end;

  TSpineBone = class  (TSpineObject)
  public
    Data: TSpineBoneData;
    Parent: TSpineBone;
    x, y: Single;
    Rotation: Single;
    ScaleX, ScaleY: Single;
    m00, m01, WorldX: Single;
    m10, m11, WorldY: Single;
    WorldRotation: Single;
    WorldScaleX, WorldScaleY: Single;
    constructor Create(const NewData: TSpineBoneData; const NewParent: TSpineBone); overload;
    constructor Create(const NewBone: TSpineBone; const NewParent: TSpineBone); overload;
    destructor Destroy; override;
    procedure UpdateWorldTransform(const FlipX, FlipY: Boolean);
    procedure SetToBindPose;
  end;

  TSpineBoneData = class  (TSpineObject)
  public
    Ref: Integer;
    Parent: TSpineBoneData;
    Name: AnsiString;
    Length: Single;
    x, y: Single;
    Rotation: Single;
    ScaleX, ScaleY: Single;
    constructor Create(const NewName: AnsiString; const NewParent: TSpineBoneData); overload;
    constructor Create(const NewBone: TSpineBoneData; const NewParent: TSpineBoneData); overload;
    procedure RefInc;
    procedure RefDec;
  end;

  TSpineSkinKey = record
    SlotIndex: Integer;
    Name: AnsiString;
    Attachment: TSpineAttachment;
  end;
  PSpineSkinKey = ^TSpineSkinKey;

  TSpineSkin = class (TSpineObject)
  private
    procedure AttachAll(const Skeleton: TSpineSkeleton; const OldSkin: TSpineSkin);
  public
    Ref: Integer;
    Name: AnsiString;
    Attachments: TSpineQuickList;
    constructor Create(const NewName: AnsiString);
    destructor Destroy; override;
    procedure AddAttachment(const SlotIndex: Integer; const KeyName: AnsiString; const Attachment: TSpineAttachment);
    function GetAttachment(const SlotIndex: Integer; const KeyName: AnsiString): TSpineAttachment;
    procedure RefInc;
    procedure RefDec;
  end;

  TSpineAttachment = class (TSpineObject)
  public
    Name: AnsiString;
    Resolved: Boolean;
    constructor Create(const NewName: AnsiString); virtual;
    procedure UpdateOffset; virtual; abstract;
    procedure Draw(const Render: TSpineRender; const Slot: TSpineSlot); virtual; abstract;
  end;

  TSpineRegionAttachment = class (TSpineAttachment)
  private
    _x0, _y0, _x1, _y1, _x2, _y2, _x3, _y3: Integer;
    _u0, _v0, _u1, _v1, _u2, _v2, _u3, _v3: Integer;
    _c0, _c1, _c2, _c3: Integer;
  public
    Region: TSpineTextureRegion;
    x, y, ScaleX, ScaleY, Rotation, Width, Height: Single;
    Vertices: array[0..31] of Single;
    Offset: array[0..7] of Single;
    constructor Create(const NewName: AnsiString); override;
    procedure UpdateOffset; override;
    procedure SetRegion(const NewRegion: TSpineTextureRegion);
    procedure Draw(const Render: TSpineRender; const Slot: TSpineSlot); override;
    procedure UpdateWorldVertices(const Bone: TSpineBone);
  end;

  TSpineRegionSequenceAttachmentMode = (amForward, amBackward, amForwardLoop, amBackwardLoop, amPingPong, amRandom);
  TSpineRegionSequenceAttachment = class (TSpineRegionAttachment)
  public
    Mode: TSpineRegionSequenceAttachmentMode;
    FrameTime: Single;
    Regions: array of TSpineTextureRegion;
    constructor Create(const NewName: AnsiString; const NewFrameTime: Single; const NewMode: TSpineRegionSequenceAttachmentMode); overload;
    procedure Draw(const Render: TSpineRender; const Slot: TSpineSlot); override;
  end;

  TSpineAttachmentResolver = class (TSpineObject)
  public
    Ref: Integer;
    constructor Create;
    procedure Resolve(const Attachment: TSpineAttachment); virtual; abstract;
    procedure RefInc;
    procedure RefDec;
  end;

  TSpineTextureAtlasAttachmentResolver = class (TSpineAttachmentResolver)
  public
    Atlas: TSpineTextureAtlas;
    constructor Create(const NewAtlas: TSpineTextureAtlas); overload;
    procedure Resolve(const Attachment: TSpineAttachment); override;
  end;

  TSpineTimelines = array of TSpineTimeline;

  TSpineAnimation = class (TSpineObject)
  public
    Ref: Integer;
    Name: AnsiString;
    Timelines: TSpineTimelines;
    Duration: Single;
    constructor Create(const AnimationName: AnsiString);
    destructor Destroy; override;
    procedure Apply(const Skeleton: TSpineSkeleton; const Time: Single; const Loop: Boolean);
    procedure Mix(const Skeleton: TSpineSkeleton; const Time: Single; const Loop: Boolean; const Alpha: Single);
    procedure AddTimeline(const Timeline: TSpineTimeline);
    procedure RefInc;
    procedure RefDec;
  end;

  TSpineTimeline = class (TSpineObject)
  public
    function GetDuration: Single; virtual; abstract;
    function GetKeyframeCount: Integer; virtual; abstract;
    procedure Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single); virtual; abstract;
  end;

  TSpineCurveTimeline = class (TSpineTimeline)
  private
    LINEAR: Single;// = 0;
    STEPPED: Single;// = -1;
    BEZIER_SEGMENTS: Integer;// = 10;
    _Curves: array of Single;
  public
    constructor Create(const NewKeyframeCount: Integer); virtual;
    procedure SetLinear(const KeyframeIndex: Integer);
    procedure SetStepped(const KeyframeIndex: Integer);
    procedure SetCurve(const KeyframeIndex: Integer; const cx1, cy1, cx2, cy2: Single);
    function GetCurvePercent(const KeyframeIndex: Integer; const Percent: Single): Single;
  end;

  TSpineRotateTimeline = class (TSpineCurveTimeline)
  private
    LAST_FRAME_TIME: Integer;// = -2;
    FRAME_VALUE: Integer;// = 1;
  public
    BoneIndex: Integer;
    Frames: array of Single;
    constructor Create(const NewKeyframeCount: Integer); override;
    function GetDuration: Single; override;
    function GetKeyframeCount: Integer; override;
    procedure SetKeyframe(const KeyframeIndex: Integer; const Time, Value: Single);
    procedure Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single); override;
  end;

  TSpineTranslateTimeline = class (TSpineCurveTimeline)
  protected
    LAST_FRAME_TIME: Integer;// = -3;
    FRAME_X: Integer;// = 1;
    FRAME_Y: Integer;// = 2;
  public
    BoneIndex: Integer;
    Frames: array of Single;
    constructor Create(const NewKeyframeCount: Integer); override;
    function GetDuration: Single; override;
    function GetKeyframeCount: Integer; override;
    procedure SetKeyframe(const KeyframeIndex: Integer; const Time, x, y: Single);
    procedure Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single);  override;
  end;

  TSpineScaleTimeline = class (TSpineTranslateTimeline)
  public
    procedure Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single); override;
  end;

  TSpineColorTimeline = class (TSpineCurveTimeline)
  private
    LAST_FRAME_TIME: Integer;// = -5;
    FRAME_R: Integer;// = 1;
    FRAME_G: Integer;// = 2;
    FRAME_B: Integer;// = 3;
    FRAME_A: Integer;// = 4;
  public
    SlotIndex: Integer;
    Frames: array of Single;
    constructor Create(const NewKeyframeCount: Integer); override;
    function GetDuration: Single; override;
    function GetKeyframeCount: Integer; override;
    procedure SetKeyframe(const KeyframeIndex: Integer; const Time, r, g, b, a: Single);
    procedure Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single); override;
  end;

  TSpineAttachmentTimeline = class (TSpineTimeline)
  public
    SlotIndex: Integer;
    Frames: array of Single;
    AttachmentNames: array of AnsiString;
    constructor Create(const NewKeyframeCount: Integer);
    function GetDuration: Single; override;
    function GetKeyframeCount: Integer; override;
    procedure SetKeyframe(const KeyframeIndex: Integer; const Time: Single; const AttachmentName: AnsiString);
    procedure Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single); override;
  end;

  function SpineColor(const r, g, b, a: Single): TSpineColor;
  function SpineClamp(const Value, MinLimit, MaxLimit: Single): Single;
  function SpineBinarySearch(const Values: PSpineFloatArray; const ValuesLength: Integer; const Target: Single; const Step: Integer): Integer;

  procedure SpineInitialize;
  procedure SpineFinalize;

const
  SpineRadToDeg = 180 / Pi;
  SpineDegToRad = Pi / 180;

implementation

var
  SpineObjects: TSpineQuickList;
  SpineTextures: TSpineQuickList;
  SpineCleanUp: Boolean = False;

const
  TIMELINE_SCALE = 0;
  TIMELINE_ROTATE = 1;
  TIMELINE_TRANSLATE = 2;
  TIMELINE_ATTACHMENT = 3;
  TIMELINE_COLOR = 4;

  ATTACHMENT_REGION = 0;
  ATTACHMENT_REGION_SEQUENCE = 1;

  //CURVE_LINEAR = 0;
  CURVE_STEPPED = 1;
  CURVE_BEZIER = 2;

//TSpineTexture BEGIN
procedure TSpineTexture.RefInc;
begin
  Ref := Ref + 1;
end;

procedure TSpineTexture.RefDec;
begin
  Ref := Ref - 1;
  if Ref <= 0 then
  begin
    SpineTextures.Remove(Self);
    Free;
  end;
end;
//TSpineTexture END

//TSpineQuickList BEGIN
procedure TSpineQuickList.SetItem(const Index: Integer; const Value: Pointer);
begin
  _Items[Index] := Value;
end;

function TSpineQuickList.GetItem(const Index: Integer): Pointer;
begin
  Result := _Items[Index];
end;

procedure TSpineQuickList.SetCapacity(const Value: Integer);
begin
  SetLength(_Items, Value);
end;

function TSpineQuickList.GetCapacity: Integer;
begin
  Result := Length(_Items);
end;

function TSpineQuickList.GetFirst: Pointer;
begin
  if _ItemCount > 0 then
  Result := _Items[0]
  else
  Result := nil;
end;

function TSpineQuickList.GetLast: Pointer;
begin
  if _ItemCount > 0 then
  Result := _Items[_ItemCount - 1]
  else
  Result := nil;
end;

function TSpineQuickList.Add(const Item: Pointer): Integer;
begin
  if Length(_Items) <= _ItemCount then
  SetLength(_Items, Length(_Items) + 32);
  _Items[_ItemCount] := Item;
  Result := _ItemCount;
  Inc(_ItemCount);
end;

function TSpineQuickList.Pop: Pointer;
begin
  if _ItemCount > 0 then
  begin
    Result := _Items[_ItemCount - 1];
    Delete(_ItemCount - 1);
  end
  else
  Result := nil;
end;

function TSpineQuickList.Insert(const Index: Integer; const Item: Pointer): Integer;
  var i: Integer;
begin
  if Length(_Items) <= _ItemCount then
  SetLength(_Items, Length(_Items) + 32);
  if Index < _ItemCount then
  begin
    for i := _ItemCount - 1 downto Index do
    _Items[i + 1] := _Items[i];
    _Items[Index] := Item;
    Result := Index;
  end
  else
  begin
    _Items[_ItemCount] := Item;
    Result := _ItemCount;
  end;
  Inc(_ItemCount);
end;

procedure TSpineQuickList.Delete(const Index: Integer);
  var i: Integer;
begin
  for i := Index to _ItemCount - 2 do
  _Items[i] := _Items[i + 1];
  Dec(_ItemCount);
end;

procedure TSpineQuickList.Remove(const Item: Pointer);
  var i: Integer;
begin
  for i := 0 to _ItemCount - 1 do
  if _Items[i] = Item then
  begin
    Delete(i);
    Exit;
  end;
end;

procedure TSpineQuickList.Clear;
begin
  _ItemCount := 0;
end;
//TSpineQuickList END

//TSpineObject BEGIN
procedure TSpineObject.ObjectStore;
  var i: Integer;
begin
  for i := 0 to SpineObjects.Count - 1 do
  if TSpineObject(SpineObjects[i]) = Self then
  Exit;
  SpineObjects.Add(Self);
end;

procedure TSpineObject.ObjectRemove;
begin
  SpineObjects.Remove(Self);
end;

destructor TSpineObject.Destroy;
begin
  ObjectRemove;
  inherited Destroy;
end;
//TSpineObject END

//TSpineAtlasPage BEGIN
constructor TSpineAtlasPage.Create;
begin
  inherited Create;
  ObjectStore;
end;

destructor TSpineAtlasPage.Destroy;
begin
  if not SpineCleanUp
  and (Texture <> nil) then
  Texture.RefDec;
  inherited Destroy;
end;
//TSpineAtlasPage END

//TSpineTextureRegion BEGIN
constructor TSpineTextureRegion.Create;
begin
  inherited Create;
  ObjectStore;
end;
//TSpineTextureRegion END

//TSpineTextureAtlas BEGIN
constructor TSpineTextureAtlas.Create(const AtlasData: TStream; const TextureLoader: TSpineTextureLoader);
  var EOF: Boolean;
  function ReadLine: AnsiString;
    var b, b1: Byte;
  begin
    Result := '';
    if AtlasData.Position = AtlasData.Size then
    begin
      EOF := True;
      Exit;
    end;
    while AtlasData.Position < AtlasData.Size do
    begin
      {$Hints off}
      AtlasData.Read(b, 1);
      {$Hints on}
      if (b <> $D) and (b <> $A) then
      Result := Result + AnsiChar(Chr(b))
      else
      Break;
    end;
    if AtlasData.Position < AtlasData.Size then
    {$Hints off}
    AtlasData.Read(b1, 1)
    {$Hints on}
    else
    Exit;
    while ((b1 = $D) or (b1 = $A)) and (b1 <> b) and (AtlasData.Position < AtlasData.Size) do
    AtlasData.Read(b1, 1);
    if AtlasData.Position < AtlasData.Size then
    AtlasData.Position := AtlasData.Position - 1;
  end;
  function Trim(const Str: AnsiString): AnsiString;
    var i, n: Integer;
  begin
    Result := Str;
    n := 0;
    for i := 1 to Length(Result) do
    if Result[i] <> ' ' then
    begin
      n := i - 1;
      Break;
    end;
    if n > 0 then
    Delete(Result, 1, n);
    for i := Length(Result) downto 1 do
    if Result[i] <> ' ' then
    begin
      n := i + 1;
      Break;
    end;
    if n < Length(Result) then
    Delete(Result, n, Length(Result) - n);
  end;
  function IsParameter(const Str: AnsiString): Boolean;
    var i: Integer;
  begin
    if Length(Str) < 2 then
    begin
      Result := False;
      Exit;
    end;
    for i := 1 to Length(Str) do
    if Str[i] = ':' then
    begin
      Result := True;
      Exit;
    end;
    Result := False;
  end;
  type TStrArr = array of AnsiString;
  function StrExplode(const Str: AnsiString; const Separator: AnsiString): TStrArr;
    var i, j: Integer;
    var CurElement: Integer;
    var PrevParamIndex: Integer;
    var b: Boolean;
  begin
    if Length(Separator) < 1 then
    begin
      SetLength(Result, 1);
      Result[0] := Str;
      Exit;
    end;
    Result := nil;
    SetLength(Result, Length(Str));
    CurElement := 0;
    PrevParamIndex := 1;
    for i := 1 to Length(Str) do
    begin
      b := True;
      for j := 0 to Length(Separator) - 1 do
      begin
        if Separator[j + 1] <> Str[i + j] then
        begin
          b := False;
          Break;
        end;
      end;
      if b then
      begin
        SetLength(Result[CurElement], i - PrevParamIndex);
        Move(Str[PrevParamIndex], Result[CurElement][1], i - PrevParamIndex);
        PrevParamIndex := i + Length(Separator);
        Inc(CurElement);
      end;
    end;
    if Length(Str) >= PrevParamIndex then
    begin
      SetLength(Result[CurElement], Length(Str) - PrevParamIndex + 1);
      Move(Str[PrevParamIndex], Result[CurElement][1], Length(Str) - PrevParamIndex + 1);
      Inc(CurElement);
    end;
    SetLength(Result, CurElement);
  end;
  var i: Integer;
  var Line, LineT: AnsiString;
  var p: TSpineAtlasPage;
  var r: TSpineAtlasRegion;
  var ParamName: AnsiString;
  var ParamValue: AnsiString;
  var StrArr: TStrArr;
  var su, sv: Single;
begin
  ObjectStore;
  Pages.Clear;
  Regions.Clear;
  EOF := False;
  p := nil;
  r := nil;
  while not EOF do
  begin
    Line := ReadLine;
    LineT := Trim(Line);
    if Length(LineT) > 0 then
    begin
      if p = nil then
      begin
        p := TSpineAtlasPage.Create;
        p.Name := LineT;
        p.Texture := nil;
        for i := 0 to SpineTextures.Count - 1 do
        if TSpineTexture(SpineTextures[i]).Name = LineT then
        begin
          p.Texture := TSpineTexture(SpineTextures[i]);
          TSpineTexture(SpineTextures[i]).RefInc;
          Break;
        end;
        if p.Texture = nil then
        begin
          p.Texture := TextureLoader.LoadTexture(LineT);
          p.Texture.Name := LineT;
          p.Texture.Ref := 1;
          SpineTextures.Add(p.Texture);
        end;
        p.Format := spfRGB888;
        p.MinFilter := stfNearest;
        p.MagFilter := stfNearest;
        p.UseMipMaps := False;
        p.WrapU := spwClamp;
        p.WrapV := spwClamp;
        Pages.Add(p);
      end
      else
      begin
        if IsParameter(LineT) then
        begin
          StrArr := StrExplode(LineT, ':');
          ParamName := LowerCase(Trim(StrArr[0]));
          ParamValue := LowerCase(Trim(StrArr[1]));
          if r = nil then
          begin
            if ParamName = 'format' then
            begin
              if ParamValue = 'alpha' then
              p.Format := spfAlpha
              else if ParamValue = 'intensity' then
              p.Format := spfIntensity
              else if ParamValue = 'luminancealpha' then
              p.Format := spfLuminanceAlpha
              else if ParamValue = 'rgb565' then
              p.Format := spfRGB565
              else if ParamValue = 'rgba4444' then
              p.Format := spfRGBA4444
              else if ParamValue = 'rgb888' then
              p.Format := spfRGB888
              else if ParamValue = 'rgba8888' then
              p.Format := spfRGBA8888;
            end
            else if ParamName = 'filter' then
            begin
              StrArr := StrExplode(ParamValue, ',');
              if StrArr[0] = 'nearest' then
              p.MinFilter := stfNearest
              else if StrArr[0] = 'linear' then
              p.MinFilter := stfLinear
              else
              begin
                p.MinFilter := stfMipMap;
                p.UseMipMaps := True;
              end;
              if StrArr[1] = 'nearest' then
              p.MagFilter := stfNearest
              else
              p.MagFilter := stfLinear;
            end
            else if ParamName = 'repeat' then
            begin
              if ParamValue = 'x' then
              p.WrapU := spwRepeat
              else if ParamValue = 'y' then
              p.WrapV := spwRepeat
              else if ParamValue = 'xy' then
              begin
                p.WrapU := spwRepeat;
                p.WrapV := spwRepeat;
              end;
            end;
          end
          else
          begin
            if ParamName = 'rotate' then
            r.Rotate := ParamValue = 'true'
            else if ParamName = 'xy' then
            begin
              StrArr := StrExplode(ParamValue, ',');
              r.u := StrToInt(Trim(StrArr[0])) / r.Texture.GetWidth;
              r.v := StrToInt(Trim(StrArr[1])) / r.Texture.GetHeight;
              r.u2 := r.u + r.RegionWidth / r.Texture.GetWidth;
              r.v2 := r.v + r.RegionHeight / r.Texture.GetHeight;
            end
            else if ParamName = 'size' then
            begin
              StrArr := StrExplode(ParamValue, ',');
              r.RegionWidth := StrToInt(Trim(StrArr[0]));
              r.RegionHeight := StrToInt(Trim(StrArr[1]));
              r.PackedWidth := r.RegionWidth;
              r.PackedHeight := r.RegionHeight;
              r.OriginalWidth := r.RegionWidth;
              r.OriginalHeight := r.RegionHeight;
              r.u2 := r.u + r.RegionWidth / r.Texture.GetWidth;
              r.v2 := r.v + r.RegionHeight / r.Texture.GetHeight;
            end
            else if ParamName = 'orig' then
            begin
              StrArr := StrExplode(ParamValue, ',');
              r.OriginalWidth := StrToInt(Trim(StrArr[0]));
              r.OriginalHeight := StrToInt(Trim(StrArr[1]));
            end
            else if ParamName = 'offset' then
            begin
              StrArr := StrExplode(ParamValue, ',');
              r.OffsetX := StrToFloat(Trim(StrArr[0]));
              r.OffsetY := StrToFloat(Trim(StrArr[1]));
            end
            else if ParamName = 'index' then
            r.Index := StrToInt(ParamValue);
          end;
        end
        else
        begin
          r := TSpineAtlasRegion.Create;
          r.Page := p;
          r.Texture := p.Texture;
          r.Index := 0;
          r.Name := LineT;
          r.OffsetX := 0;
          r.OffsetY := 0;
          r.OriginalWidth := 0;
          r.OriginalHeight := 0;
          r.PackedWidth := 0;
          r.PackedHeight := 0;
          r.Rotate := False;
          r.RegionWidth := 0;
          r.RegionHeight := 0;
          r.u := 0;
          r.v := 0;
          r.u2 := 0;
          r.v2 := 0;
          Regions.Add(r);
        end;
      end;
    end
    else
    begin
      p := nil;
      r := nil;
    end;
  end;
  for i := 0 to Regions.Count - 1 do
  if TSpineAtlasRegion(Regions[i]).Rotate then
  begin
    TSpineAtlasRegion(Regions[i]).RegionWidth := TSpineAtlasRegion(Regions[i]).RegionWidth xor TSpineAtlasRegion(Regions[i]).RegionHeight;
    TSpineAtlasRegion(Regions[i]).RegionHeight := TSpineAtlasRegion(Regions[i]).RegionWidth xor TSpineAtlasRegion(Regions[i]).RegionHeight;
    TSpineAtlasRegion(Regions[i]).RegionWidth := TSpineAtlasRegion(Regions[i]).RegionWidth xor TSpineAtlasRegion(Regions[i]).RegionHeight;
    TSpineAtlasRegion(Regions[i]).PackedWidth := TSpineAtlasRegion(Regions[i]).PackedWidth xor TSpineAtlasRegion(Regions[i]).PackedHeight;
    TSpineAtlasRegion(Regions[i]).PackedHeight := TSpineAtlasRegion(Regions[i]).PackedWidth xor TSpineAtlasRegion(Regions[i]).PackedHeight;
    TSpineAtlasRegion(Regions[i]).PackedHeight := TSpineAtlasRegion(Regions[i]).PackedWidth xor TSpineAtlasRegion(Regions[i]).PackedHeight;
    TSpineAtlasRegion(Regions[i]).OriginalWidth := TSpineAtlasRegion(Regions[i]).OriginalWidth xor TSpineAtlasRegion(Regions[i]).OriginalHeight;
    TSpineAtlasRegion(Regions[i]).OriginalHeight := TSpineAtlasRegion(Regions[i]).OriginalWidth xor TSpineAtlasRegion(Regions[i]).OriginalHeight;
    TSpineAtlasRegion(Regions[i]).OriginalWidth := TSpineAtlasRegion(Regions[i]).OriginalWidth xor TSpineAtlasRegion(Regions[i]).OriginalHeight;
    su := TSpineAtlasRegion(Regions[i]).u2 - TSpineAtlasRegion(Regions[i]).u;
    sv := TSpineAtlasRegion(Regions[i]).v2 - TSpineAtlasRegion(Regions[i]).v;
    TSpineAtlasRegion(Regions[i]).u2 := TSpineAtlasRegion(Regions[i]).u + sv;
    TSpineAtlasRegion(Regions[i]).v2 := TSpineAtlasRegion(Regions[i]).v + su;
  end;
end;

constructor TSpineTextureAtlas.Create(const AtlasFile: String; const TextureLoader: TSpineTextureLoader);
  var fs: TFileStream;
begin
  fs := TFileStream.Create(AtlasFile, fmOpenRead);
  Create(fs, TextureLoader);
  fs.Free;
end;

destructor TSpineTextureAtlas.Destroy;
  var i: Integer;
begin
  if not SpineCleanUp then
  begin
    for i := 0 to Regions.Count - 1 do
    TSpineAtlasRegion(Regions[i]).Free;
    Regions.Clear;
    for i := 0 to Pages.Count - 1 do
    TSpineAtlasPage(Pages[i]).Free;
    Pages.Clear;
  end;
  inherited Destroy;
end;

function TSpineTextureAtlas.FindRegion(const RegionName: AnsiString): TSpineAtlasRegion;
  var i: Integer;
begin
  for i := 0 to Regions.Count - 1 do
  if TSpineAtlasRegion(Regions[i]).Name = RegionName then
  begin
    Result := TSpineAtlasRegion(Regions[i]);
    Exit;
  end;
  Result := nil;
end;
//TSpineTextureAtlas END

//TSpineInputData BEGIN
constructor TSpineInputData.Create(const HijackStream: TStream);
begin
  inherited Create;
  ObjectStore;
  _Stream := HijackStream;
  _StartPos := _Stream.Position;
  SetLength(_Chars, 32);
end;

procedure TSpineInputData.Reset;
begin
  _Stream.Position := _StartPos;
end;

function TSpineInputData.Read: Integer;
begin
  if _Stream.Position + 1 > _Stream.Size then
  begin
    Result := -1;
    Exit;
  end;
  _Stream.Read(Result, 1);
end;

function TSpineInputData.ReadInt(const OptimizePositive: Boolean): Integer;
  var b: Integer;
begin
  b := Read;
  Result := b and $7F;
  if b and $80 <> 0 then
  begin
    b := Read;
    Result := Result or ((b and $7F) shl 7);
    if b and $80 <> 0 then
    begin
      b := Read;
      Result := Result or ((b and $7F) shl 14);
      if b and $80 <> 0 then
      begin
	b := Read;
	Result := Result or ((b and $7F) shl 21);
	if b and $80 <> 0 then
        begin
	  b := Read;
	  Result := Result or ((b and $7F) shl 28);
	end;
      end;
    end;
  end;
  if not OptimizePositive then
  Result := (((Result shr 1) and $7fffffff) xor -(Result and 1));
end;

function TSpineInputData.ReadString: AnsiString;
  procedure ReadUtf8_Slow(const CharCount, CharIndex, CurByte: Integer);
    var i, b: Integer;
  begin
    i := CharIndex;
    b := CurByte;
    while True do
    begin
      case b shr 4 of
        0, 1, 2, 3, 4, 5, 6, 7: _Chars[i] := AnsiChar(Chr(b));
        12, 13: _Chars[i] := AnsiChar(Chr((b and $1F) shl 6 or Read and $3F));
        14: _Chars[i] := AnsiChar(Chr((b and $0F) shl 12 or (Read and $3F) shl 6 or Read and $3F));
      end;
      Inc(i);
      if i >= CharCount then Break;
      b := Read and $ff;
    end;
  end;
  var Charcount, CharIndex, b: Integer;
begin
  CharCount := ReadInt(True);
  case CharCount of
    0, 1: begin Result := ''; Exit; end;
  end;
  Dec(CharCount);
  if Length(_Chars) < CharCount then SetLength(_Chars, CharCount);
  CharIndex := 0;
  b := 0;
  while CharIndex < CharCount do
  begin
    b := Read and $ff;
    if b > 127 then Break;
    _Chars[CharIndex] := AnsiChar(Chr(b));
    Inc(CharIndex);
  end;
  if CharIndex < CharCount then ReadUtf8_Slow(CharCount, CharIndex, b);
  SetLength(Result, CharCount); Move(_Chars[0], Result[1], CharCount);
end;

function TSpineInputData.ReadFloat: Single;
  var lw: LongWord;
begin
  lw := 0;
  lw := lw or ((LongWord(Read and $ff)) shl 24);
  lw := lw or ((LongWord(Read and $ff)) shl 16);
  lw := lw or ((LongWord(Read and $ff)) shl 8);
  lw := lw or (LongWord(Read and $ff));
  Result := PSingle(@lw)^;
end;

function TSpineInputData.ReadColor: TSpineColor;
  const RcpFF = 1 / $ff;
begin
  Result.a := (Read and $ff) * RcpFF;
  Result.b := (Read and $ff) * RcpFF;
  Result.g := (Read and $ff) * RcpFF;
  Result.r := (Read and $ff) * RcpFF;
end;
//TSpineInputData END

//TSpineSkeletonBinary BEGIN
function TSpineSkeletonBinary.ReadSkin(const Input: TSpineInputData; SkinName: AnsiString): TSpineSkin;
  var i, j, SlotIndex, SlotCount, AttachmentCount: Integer;
  var Name: AnsiString;
begin
  SlotCount := Input.ReadInt(True);
  if (SlotCount = 0) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TSpineSkin.Create(SkinName);
  for i := 0 to SlotCount - 1 do
  begin
    SlotIndex := Input.ReadInt(True);
    AttachmentCount := Input.ReadInt(True);
    for j := 0 to AttachmentCount - 1 do
    begin
      Name := Input.ReadString;
      Result.AddAttachment(SlotIndex, Name, ReadAttachment(Input, Name));
    end;
  end;
end;

function TSpineSkeletonBinary.ReadAttachment(const Input: TSpineInputData; AttachmentName: AnsiString): TSpineAttachment;
  var Name: AnsiString;
  var t: Integer;
  var fps: Single;
  var Mode: TSpineRegionSequenceAttachmentMode;
begin
  Name := Input.ReadString;
  if Length(Name) = 0 then Name := AttachmentName;
  t := Byte(Input.Read and $ff);
  case t of
    ATTACHMENT_REGION: Result := TSpineRegionAttachment.Create(Name);
    ATTACHMENT_REGION_SEQUENCE:
    begin
      fps := Input.ReadFloat;
      Mode := TSpineRegionSequenceAttachmentMode(Input.ReadInt(True));
      Result := TSpineRegionSequenceAttachment.Create(Name, 1 / fps, Mode);
    end;
    else
    Result := nil;
  end;
  if Result is TSpineRegionAttachment then
  begin
    TSpineRegionAttachment(Result).x := Input.ReadFloat * Scale;
    TSpineRegionAttachment(Result).y := Input.ReadFloat * Scale;
    TSpineRegionAttachment(Result).ScaleX := Input.ReadFloat;
    TSpineRegionAttachment(Result).ScaleY := Input.ReadFloat;
    TSpineRegionAttachment(Result).Rotation := Input.ReadFloat;
    TSpineRegionAttachment(Result).Width := Input.ReadFloat * Scale;
    TSpineRegionAttachment(Result).Height := Input.ReadFloat * Scale;
  end;
end;

procedure TSpineSkeletonBinary.ReadCurve(const Input: TSpineInputData; KeyframeIndex: Integer; Timeline: TSpineCurveTimeline);
  var cx1, cy1, cx2, cy2: Single;
begin
  case (Input.Read and $ff) of
    CURVE_STEPPED: Timeline.SetStepped(KeyframeIndex);
    CURVE_BEZIER:
    begin
      cx1 := Input.ReadFloat;
      cy1 := Input.ReadFloat;
      cx2 := Input.ReadFloat;
      cy2 := Input.ReadFloat;
      Timeline.SetCurve(KeyframeIndex, cx1, cy1, cx2, cy2);
    end;
  end;
end;

function TSpineSkeletonBinary.ReadAnimation(const Input: TSpineInputData; const SkeletonData: TSpineSkeletonData): TSpineAnimation;
  var Time, Value, TimelineScale, x, y: Single;
  var i, j, BoneCount, BoneIndex, ItemCount, TimelineType, KeyCount, KeyFrameIndex, SlotCount, SlotIndex: Integer;
  var BoneName, SlotName: AnsiString;
  var RotateTimeline: TSpineRotateTimeline;
  var TranslateTimeline: TSpineTranslateTimeline;
  var ColorTimeline: TSpineColorTimeline;
  var AttachmentTimeline: TSpineAttachmentTimeline;
  var Color: TSpineColor;
begin
  Result := TSpineAnimation.Create(Input.ReadString);
  BoneCount := Input.ReadInt(True);
  for i := 0 to BoneCount - 1 do
  begin
    BoneName := Input.ReadString;
    BoneIndex := SkeletonData.FindBoneIndex(BoneName);
    ItemCount := Input.ReadInt(True);
    for j := 0 to ItemCount - 1 do
    begin
      TimelineType := Input.Read and $ff;
      KeyCount := Input.ReadInt(True);
      case TimelineType of
        TIMELINE_ROTATE:
        begin
	  RotateTimeline := TSpineRotateTimeline.Create(KeyCount);
	  RotateTimeline.BoneIndex := BoneIndex;
	  for KeyframeIndex := 0 to KeyCount - 1 do
          begin
            Time := Input.ReadFloat;
            Value := Input.ReadFloat;
	    RotateTimeline.SetKeyframe(KeyframeIndex, Time, Value);
	    if KeyframeIndex < KeyCount - 1 then
            ReadCurve(Input, KeyframeIndex, RotateTimeline);
	  end;
          Result.AddTimeline(RotateTimeline);
        end;
        TIMELINE_TRANSLATE, TIMELINE_SCALE:
        begin
	  TimelineScale := 1;
	  if TimelineType = TIMELINE_SCALE then
	  TranslateTimeline := TSpineScaleTimeline.Create(KeyCount)
	  else
          begin
	    TranslateTimeline := TSpineTranslateTimeline.Create(KeyCount);
	    TimelineScale := scale;
	  end;
	  TranslateTimeline.BoneIndex := BoneIndex;
	  for KeyframeIndex := 0 to KeyCount - 1 do
          begin
            Time := Input.ReadFloat;
            x := Input.ReadFloat;
            y := Input.ReadFloat;
	    TranslateTimeline.SetKeyframe(KeyframeIndex, Time, x * TimelineScale, y * TimelineScale);
	    if KeyframeIndex < KeyCount - 1 then
            ReadCurve(Input, KeyframeIndex, TranslateTimeline);
	  end;
          Result.AddTimeline(TranslateTimeline);
	end;
      end;
    end;
  end;
  SlotCount := Input.ReadInt(True);
  for i := 0 to SlotCount - 1 do
  begin
    SlotName := Input.ReadString;
    SlotIndex := SkeletonData.FindSlotIndex(SlotName);
    ItemCount := Input.ReadInt(True);
    for j := 0 to ItemCount - 1 do
    begin
      TimelineType := Input.Read and $ff;
      KeyCount := Input.ReadInt(True);
      case TimelineType of
        TIMELINE_COLOR:
        begin
	  ColorTimeline := TSpineColorTimeline.Create(KeyCount);
	  ColorTimeline.SlotIndex := SlotIndex;
	  for KeyframeIndex := 0 to KeyCount - 1 do
          begin
	    Time := Input.ReadFloat;
            Color := Input.ReadColor;
	    ColorTimeline.SetKeyframe(KeyframeIndex, Time, Color.r, Color.g, Color.b, Color.a);
	    if KeyframeIndex < KeyCount - 1 then
            ReadCurve(Input, KeyframeIndex, ColorTimeline);
	  end;
          Result.AddTimeline(ColorTimeline);
        end;
        TIMELINE_ATTACHMENT:
        begin
	  AttachmentTimeline := TSpineAttachmentTimeline.Create(KeyCount);
	  AttachmentTimeline.SlotIndex := SlotIndex;
	  for KeyframeIndex := 0 to KeyCount - 1 do
          begin
            Time := Input.ReadFloat;
	    AttachmentTimeline.SetKeyframe(KeyframeIndex, Time, Input.ReadString);
          end;
          Result.AddTimeline(AttachmentTimeline);
        end;
      end;
    end;
  end;
end;

constructor TSpineSkeletonBinary.Create(const Atlas: TSpineTextureAtlas);
begin
  inherited Create;
  ObjectStore;
  Scale := 1;
  AttachmentResolver := TSpineTextureAtlasAttachmentResolver.Create(Atlas);
  AttachmentResolver.RefInc;
end;

constructor TSpineSkeletonBinary.Create(const NewAttachmentResolver: TSpineAttachmentResolver);
begin
  inherited Create;
  ObjectStore;
  Scale := 1;
  AttachmentResolver := NewAttachmentResolver;
  AttachmentResolver.RefInc;
end;

destructor TSpineSkeletonBinary.Destroy;
begin
  if not SpineCleanUp
  and (AttachmentResolver <> nil) then
  AttachmentResolver.RefDec;
  inherited Destroy;
end;

function TSpineSkeletonBinary.ReadSkeletonData(const Stream: TStream): TSpineSkeletonData;
  var Input: TSpineInputData;
  var i, n: Integer;
  var ParentName, BoneName, SlotName: AnsiString;
  var Parent, BoneData: TSpineBoneData;
  var SlotData: TSpineSlotData;
  var Skin: TSpineSkin;
  var Animation: TSpineAnimation;
begin
  Input := TSpineInputData.Create(Stream);
  Result := TSpineSkeletonData.Create(AttachmentResolver);
  n := Input.ReadInt(True);
  for i := 0 to n - 1 do
  begin
    BoneName := Input.ReadString;
    Parent := nil;
    ParentName := Input.ReadString;
    if Length(ParentName) > 0 then
    Parent := Result.FindBone(ParentName);
    BoneData := TSpineBoneData.Create(BoneName, Parent);
    BoneData.RefInc;
    BoneData.x := Input.ReadFloat * Scale;
    BoneData.y := Input.ReadFloat * Scale;
    BoneData.ScaleX := Input.ReadFloat;
    BoneData.ScaleY := Input.ReadFloat;
    BoneData.Rotation := Input.ReadFloat;
    BoneData.Length := Input.ReadFloat * Scale;
    Result.AddBone(BoneData);
  end;
  n := Input.ReadInt(True);
  for i := 0 to n - 1 do
  begin
    SlotName := Input.ReadString;
    BoneName := Input.ReadString;
    BoneData := Result.FindBone(BoneName);
    SlotData := TSpineSlotData.Create(SlotName, BoneData);
    SlotData.RefInc;
    SlotData.Color := Input.ReadColor;
    SlotData.AttachmentName := Input.ReadString;
    Result.AddSlot(SlotData);
  end;
  Skin := ReadSkin(Input, 'default');
  if Skin <> nil then
  begin
    Result.DefaultSkin := Skin;
    Skin.RefInc;
    Result.AddSkin(Skin);
  end;
  n := Input.ReadInt(True);
  for i := 0 to n - 1 do
  begin
    Skin := ReadSkin(Input, Input.ReadString);
    Skin.RefInc;
    Result.AddSkin(Skin);
  end;
  n := Input.ReadInt(True);
  for i := 0 to n - 1 do
  begin
    Animation := ReadAnimation(Input, Result);
    Animation.RefInc;
    Result.AddAnimation(Animation);
  end;
  Input.Free;
end;

function TSpineSkeletonBinary.ReadSkeletonData(const FileName: String): TSpineSkeletonData;
  var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  Result := ReadSkeletonData(fs);
  fs.Free;
end;
//TSpineSkeletonBinary END

//TSpineSkeleton BEGIN
constructor TSpineSkeleton.Create(const NewData: TSpineSkeletonData);
  var i, j, n, m: Integer;
  var BoneData: TSpineBoneData;
  var SlotData: TSpineSlotData;
  var Parent: TSpineBone;
  var Bone: TSpineBone;
  var Slot: TSpineSlot;
begin
  inherited Create;
  ObjectStore;
  Data := TSpineSkeletonData.Create(NewData);
  SetLength(Bones, Length(Data.Bones));
  n := 0;
  for i := 0 to High(Data.Bones) do
  begin
    BoneData := Data.Bones[i];
    if BoneData.Parent = nil then
    Parent := nil
    else
    begin
      Parent := nil;
      for j := 0 to High(Data.Bones) do
      if Data.Bones[j] = BoneData.Parent then
      begin
        Parent := Bones[j];
        Break;
      end;
    end;
    Bones[n] := TSpineBone.Create(BoneData, Parent); Inc(n);
  end;
  SetLength(Slots, Length(Data.Slots)); n := 0;
  SetLength(DrawOrder, Length(Data.Slots)); m := 0;
  for i := 0 to High(Data.Slots) do
  begin
    SlotData := Data.Slots[i];
    Bone := nil;
    for j := 0 to High(Data.Bones) do
    if Data.Bones[j] = SlotData.BoneData then
    begin
      Bone := Bones[j];
      Break;
    end;
    Slot := TSpineSlot.Create(SlotData, Self, Bone);
    Slots[n] := Slot; Inc(n);
    DrawOrder[m] := Slot; Inc(m);
  end;
  Color := SpineColor(1, 1, 1, 1);
  FlipX := False;
  FlipY := False;
end;

constructor TSpineSkeleton.Create(const NewSkeleton: TSpineSkeleton);
  var i, j, n: Integer;
  var Bone: TSpineBone;
  var Slot: TSpineSlot;
  var Parent: TSpineBone;
begin
  inherited Create;
  ObjectStore;
  Data := TSpineSkeletonData.Create(NewSkeleton.Data);
  SetLength(Bones, Length(NewSkeleton.Bones)); n := 0;
  for i := 0 to High(NewSkeleton.Bones) do
  begin
    Parent := nil;
    Bone := NewSkeleton.Bones[i];
    for j := 0 to High(NewSkeleton.Bones) do
    if NewSkeleton.Bones[j] = Bone.Parent then
    begin
      Parent := Bones[j];
      Break;
    end;
    Bones[n] := TSpineBone.Create(Bone, Parent); Inc(n);
  end;

  SetLength(Slots, Length(NewSkeleton.Slots)); n := 0;
  for i := 0 to High(NewSkeleton.Slots) do
  begin
    Bone := nil;
    Slot := NewSkeleton.Slots[i];
    for j := 0 to High(NewSkeleton.Bones) do
    if NewSkeleton.Bones[j] = Slot.Bone then
    begin
      Bone := Bones[j];
      Break;
    end;
    Slots[n] := TSpineSlot.Create(Slot, Self, Bone); Inc(n);
  end;

  SetLength(DrawOrder, Length(Slots));
  for i := 0 to High(Slots) do
  DrawOrder[i] := Slots[i];

  Skin := NewSkeleton.Skin;
  Color := NewSkeleton.Color;
  Time := NewSkeleton.Time;
  FlipX := NewSkeleton.FlipX;
  FlipY := NewSkeleton.FlipY;
end;

destructor TSpineSkeleton.Destroy;
begin
  if not SpineCleanUp
  and (Data <> nil) then
  Data.Free;
  inherited Destroy;
end;

procedure TSpineSkeleton.UpdateWorldTransform;
  var i: Integer;
begin
  for i := 0 to High(Bones) do
  Bones[i].UpdateWorldTransform(FlipX, FlipY);
end;

procedure TSpineSkeleton.SetToBindPose;
begin
  SetBonesToBindPose;
  SetSlotsToBindPose;
end;

procedure TSpineSkeleton.SetBonesToBindPose;
  var i: Integer;
begin
  for i := 0 to High(Bones) do
  Bones[i].SetToBindPose;
end;

procedure TSpineSkeleton.SetSlotsToBindPose;
  var i: Integer;
begin
  for i := 0 to High(Slots) do
  Slots[i].SetToBindPose(i);
end;

function TSpineSkeleton.GetRootBone: TSpineBone;
begin
  if Length(Bones) > 0 then
  Result := Bones[0]
  else
  Result := nil;
end;

function TSpineSkeleton.FindBone(const BoneName: AnsiString): TSpineBone;
  var i: Integer;
begin
  for i := 0 to High(Bones) do
  if Bones[i].Data.Name = BoneName then
  begin
    Result := Bones[i];
    Exit;
  end;
  Result := nil;
end;

function TSpineSkeleton.FindBoneIndex(const BoneName: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to High(Bones) do
  if Bones[i].Data.Name = BoneName then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

function TSpineSkeleton.FindSlot(const SlotName: AnsiString): TSpineSlot;
  var i: Integer;
begin
  for i := 0 to High(Slots) do
  if Slots[i].Data.Name = SlotName then
  begin
    Result := Slots[i];
    Exit;
  end;
  Result := nil;
end;

function TSpineSkeleton.FindSlotIndex(const SlotName: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to High(Slots) do
  if Slots[i].Data.Name = SlotName then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

procedure TSpineSkeleton.SetSkin(const SkinName: AnsiString);
  var NewSkin: TSpineSkin;
begin
  NewSkin := Data.FindSkin(SkinName);
  if NewSkin <> nil then
  SetSkin(NewSkin);
end;

procedure TSpineSkeleton.SetSkin(const NewSkin: TSpineSkin);
begin
  if (Skin <> nil) and (NewSkin <> nil) then
  NewSkin.AttachAll(Self, Skin);
  Skin := NewSkin;
end;

function TSpineSkeleton.GetAttachment(const SlotName, AttachmentName: AnsiString): TSpineAttachment;
begin
  Result := GetAttachment(Data.FindSlotIndex(SlotName), AttachmentName);
end;

function TSpineSkeleton.GetAttachment(const SlotIndex: Integer; const AttachmentName: AnsiString): TSpineAttachment;
begin
  if Data.DefaultSkin <> nil then
  begin
    Result := Data.DefaultSkin.GetAttachment(SlotIndex, AttachmentName);
    if Result <> nil then Exit;
  end;
  if Skin <> nil then
  begin
    Result := Skin.GetAttachment(SlotIndex, AttachmentName);
    Exit;
  end;
  Result := nil;
end;

procedure TSpineSkeleton.SetAttachment(const SlotName, AttachmentName: AnsiString);
  var i: Integer;
begin
  for i := 0 to High(Slots) do
  if Slots[i].Data.Name = SlotName then
  begin
    Slots[i].SetAttachment(GetAttachment(i, AttachmentName));
    Exit;
  end;
end;

procedure TSpineSkeleton.Update(const Delta: Single);
begin
  Time := Time + Delta;
end;

procedure TSpineSkeleton.Draw(const Render: TSpineRender);
  var i: Integer;
  var Slot: TSpineSlot;
  var Attachment: TSpineAttachment;
begin
  for i := 0 to High(DrawOrder) do
  begin
    Slot := DrawOrder[i];
    Attachment := Slot.Attachment;
    if Attachment <> nil then
    begin
      if not Attachment.Resolved then
        Data.AttachmentResolver.Resolve(Attachment);
      Attachment.UpdateOffset;
      Attachment.Draw(Render, Slot);
    end;
  end;
end;
//TSpineSkeleton END

//TSpineSkeletonData BEGIN
constructor TSpineSkeletonData.Create(const NewAttachmentResolver: TSpineAttachmentResolver);
begin
  inherited Create;
  ObjectStore;
  AttachmentResolver := NewAttachmentResolver;
  AttachmentResolver.RefInc;
end;

constructor TSpineSkeletonData.Create(const NewSkeletonData: TSpineSkeletonData);
  var i: Integer;
begin
  inherited Create;
  ObjectStore;
  AttachmentResolver := NewSkeletonData.AttachmentResolver;
  AttachmentResolver.RefInc;
  SetLength(Bones, Length(NewSkeletonData.Bones));
  SetLength(Slots, Length(NewSkeletonData.Slots));
  SetLength(Skins, Length(NewSkeletonData.Skins));
  SetLength(Animations, Length(NewSkeletonData.Animations));
  for i := 0 to High(Bones) do
  begin
    Bones[i] := NewSkeletonData.Bones[i];
    Bones[i].RefInc;
  end;
  for i := 0 to High(Slots) do
  begin
    Slots[i] := NewSkeletonData.Slots[i];
    Slots[i].RefInc;
  end;
  for i := 0 to High(Skins) do
  begin
    Skins[i] := NewSkeletonData.Skins[i];
    Skins[i].RefInc;
  end;
  for i := 0 to High(Animations) do
  begin
    Animations[i] := NewSkeletonData.Animations[i];
    Animations[i].RefInc;
  end;
  DefaultSkin := NewSkeletonData.DefaultSkin;
end;

destructor TSpineSkeletonData.Destroy;
begin
  if not SpineCleanUp then
  begin
    if (AttachmentResolver <> nil) then
    AttachmentResolver.RefDec;
    Clear;
  end;
  inherited Destroy;
end;

procedure TSpineSkeletonData.Clear;
  var i: Integer;
begin
  for i := 0 to High(Bones) do
  Bones[i].RefDec;
  for i := 0 to High(Slots) do
  Slots[i].RefDec;
  for i := 0 to High(Skins) do
  Skins[i].RefDec;
  for i := 0 to High(Animations) do
  Animations[i].RefDec;
  Bones := nil;
  Slots := nil;
  Skins := nil;
  Animations := nil;
  DefaultSkin := nil;
end;

procedure TSpineSkeletonData.AddBone(const Bone: TSpineBoneData);
begin
  SetLength(Bones, Length(Bones) + 1);
  Bones[High(Bones)] := Bone;
end;

function TSpineSkeletonData.FindBone(const BoneName: AnsiString): TSpineBoneData;
  var i: Integer;
begin
  for i := 0 to High(Bones) do
  if Bones[i].Name = BoneName then
  begin
    Result := Bones[i];
    Exit;
  end;
  Result := nil;
end;

function TSpineSkeletonData.FindBoneIndex(const BoneName: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to High(Bones) do
  if Bones[i].Name = BoneName then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

procedure TSpineSkeletonData.AddSlot(const Slot: TSpineSlotData);
begin
  SetLength(Slots, Length(Slots) + 1);
  Slots[High(Slots)] := Slot;
end;

function TSpineSkeletonData.FindSlot(const SlotName: AnsiString): TSpineSlotData;
  var i: Integer;
begin
  for i := 0 to High(Slots) do
  if Slots[i].Name = SlotName then
  begin
    Result := Slots[i];
    Exit;
  end;
  Result := nil;
end;

function TSpineSkeletonData.FindSlotIndex(const SlotName: AnsiString): Integer;
  var i: Integer;
begin
  for i := 0 to High(Slots) do
  if Slots[i].Name = SlotName then
  begin
    Result := i;
    Exit;
  end;
  Result := -1;
end;

procedure TSpineSkeletonData.AddSkin(const Skin: TSpineSkin);
begin
  SetLength(Skins, Length(Skins) + 1);
  Skins[High(Skins)] := Skin;
end;

function TSpineSkeletonData.FindSkin(const SkinName: AnsiString): TSpineSkin;
  var i: Integer;
begin
  for i := 0 to High(Skins) do
  if Skins[i].Name = SkinName then
  begin
    Result := Skins[i];
    Exit;
  end;
  Result := nil;
end;

procedure TSpineSkeletonData.AddAnimation(const Animation: TSpineAnimation);
begin
  SetLength(Animations, Length(Animations) + 1);
  Animations[High(Animations)] := Animation;
end;

function TSpineSkeletonData.FindAnimation(const AnimationName: AnsiString): TSpineAnimation;
  var i: Integer;
begin
  for i := 0 to High(Animations) do
  if Animations[i].Name = AnimationName then
  begin
    Result := Animations[i];
    Exit;
  end;
  Result := nil;
end;
//TSpineSkeletonData END

//TSpineSlot BEGIN
procedure TSpineSlot.SetAttachment(const Value: TSpineAttachment);
begin
  _Attachment := Value;
  _AttachmentTime := Skeleton.Time;
end;

procedure TSpineSlot.SetAttachmentTime(const Value: Single);
begin
  _AttachmentTime := Skeleton.Time - Value;
end;

constructor TSpineSlot.Create(const NewData: TSpineSlotData; const NewSkeleton: TSpineSkeleton; const NewBone: TSpineBone);
begin
  inherited Create;
  ObjectStore;
  Data := NewData;
  Data.RefInc;
  Skeleton := NewSkeleton;
  Bone := NewBone;
  Color := SpineColor(1, 1, 1, 1);
  SetToBindPose();
end;

constructor TSpineSlot.Create(const NewSlot: TSpineSlot; const NewSkeleton: TSpineSkeleton; const NewBone: TSpineBone);
begin
  inherited Create;
  ObjectStore;
  Data := NewSlot.Data;
  Data.RefInc;
  Skeleton := NewSkeleton;
  Bone := NewBone;
  Color := NewSlot.Color;
  _Attachment := NewSlot.Attachment;
  _AttachmentTime := NewSlot.AttachmentTime;
end;

destructor TSpineSlot.Destroy;
begin
  if not SpineCleanUp
  and (Data <> nil) then
  Data.RefDec;
  inherited Destroy;
end;

procedure TSpineSlot.SetToBindPose(const SlotIndex: Integer);
begin
  Color := Data.Color;
  if Length(Data.AttachmentName) = 0 then
  Attachment := nil
  else
  Attachment := Skeleton.GetAttachment(SlotIndex, Data.AttachmentName);
end;

procedure TSpineSlot.SetToBindPose;
  var i: Integer;
begin
  for i := 0 to High(Skeleton.Data.Slots) do
  if Skeleton.Data.Slots[i] = Data then
  begin
    SetToBindPose(i);
    Break;
  end;
end;
//TSpineSlot END

//TSpineSlotData BEGIN
constructor TSpineSlotData.Create(const NewName: AnsiString; const NewBoneData: TSpineBoneData);
begin
  inherited Create;
  ObjectStore;
  Ref := 0;
  Color := SpineColor(1, 1, 1, 1);
  Name := NewName;
  BoneData := NewBoneData;
end;

procedure TSpineSlotData.RefInc;
begin
  Ref := Ref + 1;
end;

procedure TSpineSlotData.RefDec;
begin
  Ref := Ref - 1;
  if Ref <= 0 then
  Free;
end;
//TSpineSlotData END

//TSpineBone BEGIN
constructor TSpineBone.Create(const NewData: TSpineBoneData; const NewParent: TSpineBone);
begin
  inherited Create;
  ObjectStore;
  ScaleX := 1;
  ScaleY := 1;
  Parent := NewParent;
  Data := NewData;
  Data.RefInc;
  SetToBindPose;
end;

constructor TSpineBone.Create(const NewBone: TSpineBone; const NewParent: TSpineBone);
begin
  inherited Create;
  ObjectStore;
  Parent := NewParent;
  Data := NewBone.Data;
  Data.RefInc;
  x := NewBone.x;
  y := NewBone.y;
  Rotation := NewBone.Rotation;
  ScaleX := NewBone.ScaleX;
  ScaleY := NewBone.ScaleY;
end;

destructor TSpineBone.Destroy;
begin
  if not SpineCleanUp
  and (Data <> nil) then
  Data.RefDec;
  inherited Destroy;
end;

procedure TSpineBone.UpdateWorldTransform(const FlipX, FlipY: Boolean);
  var p: TSpineBone;
  var c, s: Single;
begin
  p := Parent;
  if p <> nil then
  begin
    WorldX := x * p.m00 + y * p.m01 + p.WorldX;
    WorldY := x * p.m10 + y * p.m11 + p.WorldY;
    WorldScaleX := p.WorldScaleX * ScaleX;
    WorldScaleY := p.WorldScaleY * ScaleY;
    WorldRotation := p.WorldRotation + Rotation;
  end
  else
  begin
    WorldX := x;
    WorldY := y;
    WorldScaleX := ScaleX;
    WorldScaleY := ScaleY;
    WorldRotation := Rotation;
  end;
  c := Cos(WorldRotation * SpineDegToRad);
  s := Sin(WorldRotation * SpineDegToRad);
  m00 := c * WorldScaleX;
  m10 := -s * WorldScaleX;
  m01 := -s * WorldScaleY;
  m11 := -c * WorldScaleY;
  if FlipX then
  begin
    m00 := -m00;
    m01 := -m01;
  end;
  if (FlipY) then
  begin
    m10 := -m10;
    m11 := -m11;
  end;
end;

procedure TSpineBone.SetToBindPose;
begin
  x := Data.x;
  y := Data.y;
  Rotation := Data.Rotation;
  ScaleX := Data.ScaleX;
  ScaleY := Data.ScaleY;
end;
//TSpineBone END

//TSpineBoneData BEGIN
constructor TSpineBoneData.Create(const NewName: AnsiString; const NewParent: TSpineBoneData);
begin
  inherited Create;
  ObjectStore;
  Ref := 0;
  ScaleX := 1;
  ScaleY := 1;
  Name := NewName;
  Parent := NewParent;
end;

constructor TSpineBoneData.Create(const NewBone: TSpineBoneData; const NewParent: TSpineBoneData);
begin
  inherited Create;
  ObjectStore;
  Ref := 0;
  Parent := NewParent;
  Name := NewBone.Name;
  Length := NewBone.Length;
  x := NewBone.x;
  y := NewBone.y;
  Rotation := NewBone.Rotation;
  ScaleX := NewBone.ScaleX;
  ScaleY := NewBone.ScaleY;
end;

procedure TSpineBoneData.RefInc;
begin
  Ref := Ref + 1;
end;

procedure TSpineBoneData.RefDec;
begin
  Ref := Ref - 1;
  if Ref <= 0 then
  Free;
end;
//TSpineBoneData END

//TSpineSkin BEGIN
procedure TSpineSkin.AttachAll(const Skeleton: TSpineSkeleton; const OldSkin: TSpineSkin);
  var i: Integer;
  var Key: PSpineSkinKey;
  var Slot: TSpineSlot;
  var Attachment: TSpineAttachment;
begin
  for i := 0 to OldSkin.Attachments.Count - 1 do
  begin
    Key := PSpineSkinKey(OldSkin.Attachments[i]);
    Slot := Skeleton.Slots[Key^.SlotIndex];
    if Slot.Attachment = Key^.Attachment then
    begin
      Attachment := GetAttachment(Key^.SlotIndex, Key^.Name);
      if Attachment <> nil then Slot.Attachment := Attachment;
    end;
  end;
end;

constructor TSpineSkin.Create(const NewName: AnsiString);
begin
  inherited Create;
  ObjectStore;
  Ref := 0;
  Name := NewName;
  Attachments.Clear;
end;

destructor TSpineSkin.Destroy;
  var i: Integer;
begin
  for i := 0 to Attachments.Count - 1 do
  Dispose(PSpineSkinKey(Attachments[i]));
  Attachments.Clear;
  inherited Destroy;
end;

procedure TSpineSkin.AddAttachment(const SlotIndex: Integer; const KeyName: AnsiString; const Attachment: TSpineAttachment);
  var Key: PSpineSkinKey;
begin
  New(Key);
  Key^.SlotIndex := SlotIndex;
  Key^.Name := KeyName;
  Key^.Attachment := Attachment;
  Attachments.Add(Key);
end;

function TSpineSkin.GetAttachment(const SlotIndex: Integer; const KeyName: AnsiString): TSpineAttachment;
  var i: Integer;
begin
  for i := 0 to Attachments.Count - 1 do
  if (PSpineSkinKey(Attachments[i])^.SlotIndex = SlotIndex)
  and (PSpineSkinKey(Attachments[i])^.Name = KeyName) then
  begin
    Result := PSpineSkinKey(Attachments[i])^.Attachment;
    Exit;
  end;
  Result := nil;
end;

procedure TSpineSkin.RefInc;
begin
  Ref := Ref + 1;
end;

procedure TSpineSkin.RefDec;
begin
  Ref := Ref - 1;
  if Ref <= 0 then
  Free;
end;
//TSpineSkin END

//TSpineAttachment BEGIN
constructor TSpineAttachment.Create(const NewName: AnsiString);
begin
  inherited Create;
  ObjectStore;
  Name := NewName;
end;
//TSpineAttachment END

//TSpineRegionAttachment BEGIN
constructor TSpineRegionAttachment.Create(const NewName: AnsiString);
begin
  inherited Create(NewName);
  _x0 := 0; _y0 := 1; _u0 := 2; _v0 := 3; _c0 := 4;
  _x1 := 1 * 8 + 0; _y1 := 1 * 8 + 1; _u1 := 1 * 8 + 2; _v1 := 1 * 8 + 3; _c1 := 1 * 8 + 4;
  _x2 := 2 * 8 + 0; _y2 := 2 * 8 + 1; _u2 := 2 * 8 + 2; _v2 := 2 * 8 + 3; _c2 := 2 * 8 + 4;
  _x3 := 3 * 8 + 0; _y3 := 3 * 8 + 1; _u3 := 3 * 8 + 2; _v3 := 3 * 8 + 3; _c3 := 3 * 8 + 4;
end;

procedure TSpineRegionAttachment.UpdateOffset;
  var LocalX, LocalY, LocalX2, LocalY2, LocalXCos, LocalYCos, LocalX2Cos, LocalY2Cos, LocalXSin, LocalYSin, LocalX2Sin, LocalY2Sin, c, s: Single;
  var AtlasRegion: TSpineAtlasRegion;
begin
  LocalX2 := Width * 0.5;
  LocalY2 := Height * 0.5;
  LocalX := -LocalX2;
  LocalY := -LocalY2;
  if Region is TSpineAtlasRegion then
  begin
    AtlasRegion := TSpineAtlasRegion(Region);
    if AtlasRegion.Rotate then
    begin
      LocalX := LocalX + AtlasRegion.OffsetX / AtlasRegion.OriginalWidth * Height;
      LocalY := LocalY + AtlasRegion.OffsetY / AtlasRegion.OriginalHeight * Width;
      LocalX2 := LocalX2 - (AtlasRegion.OriginalWidth - AtlasRegion.OffsetX - AtlasRegion.PackedHeight) / AtlasRegion.OriginalWidth * Width;
      LocalY2 := LocalY2 - (AtlasRegion.OriginalHeight - AtlasRegion.OffsetY - AtlasRegion.PackedWidth) / AtlasRegion.OriginalHeight * Height;
    end
    else
    begin
      LocalX := LocalX + AtlasRegion.OffsetX / AtlasRegion.OriginalWidth * Width;
      LocalY := LocalY + AtlasRegion.OffsetY / AtlasRegion.OriginalHeight * Height;
      LocalX2 := LocalX2 - (AtlasRegion.OriginalWidth - AtlasRegion.OffsetX - AtlasRegion.PackedWidth) / AtlasRegion.OriginalWidth * Width;
      LocalY2 := LocalY2 - (AtlasRegion.OriginalHeight - AtlasRegion.OffsetY - AtlasRegion.PackedHeight) / AtlasRegion.OriginalHeight * Height;
    end;
  end;
  LocalX := LocalX * ScaleX;
  LocalY := LocalY * ScaleY;
  LocalX2 := LocalX2 * ScaleX;
  LocalY2 := LocalY2 * ScaleY;
  c := Cos(Rotation * SpineDegToRad);
  s := Sin(Rotation * SpineDegToRad);
  LocalXCos := LocalX * c + x;
  LocalXSin := LocalX * s;
  LocalYCos := LocalY * c + y;
  LocalYSin := LocalY * s;
  LocalX2Cos := LocalX2 * c + x;
  LocalX2Sin := LocalX2 * s;
  LocalY2Cos := LocalY2 * c + y;
  LocalY2Sin := LocalY2 * s;
  Offset[0] := LocalXCos - LocalYSin;
  Offset[1] := LocalYCos + LocalXSin;
  Offset[2] := LocalXCos - LocalY2Sin;
  Offset[3] := LocalY2Cos + LocalXSin;
  Offset[4] := LocalX2Cos - LocalY2Sin;
  Offset[5] := LocalY2Cos + LocalX2Sin;
  Offset[6] := LocalX2Cos - LocalYSin;
  Offset[7] := LocalYCos + LocalX2Sin;
end;

procedure TSpineRegionAttachment.SetRegion(const NewRegion: TSpineTextureRegion);
begin
  Region := NewRegion;
  if (Region is TSpineAtlasRegion) and (TSpineAtlasRegion(Region).Rotate) then
  begin
    Vertices[_u1] := Region.U;
    Vertices[_v1] := Region.V2;
    Vertices[_u2] := Region.U;
    Vertices[_v2] := Region.V;
    Vertices[_u3] := Region.U2;
    Vertices[_v3] := Region.V;
    Vertices[_u0] := Region.U2;
    Vertices[_v0] := Region.V2;
  end
  else
  begin
    Vertices[_u0] := Region.U;
    Vertices[_v0] := Region.V2;
    Vertices[_u1] := Region.U;
    Vertices[_v1] := Region.V;
    Vertices[_u2] := Region.U2;
    Vertices[_v2] := Region.V;
    Vertices[_u3] := Region.U2;
    Vertices[_v3] := Region.V2;
  end;
  UpdateOffset();
end;

procedure TSpineRegionAttachment.Draw(const Render: TSpineRender; const Slot: TSpineSlot);
  var c: TSpineColor;
begin
  c := SpineColor(
    Slot.Skeleton.Color.r * Slot.Color.r,
    Slot.Skeleton.Color.g * Slot.Color.g,
    Slot.Skeleton.Color.b * Slot.Color.b,
    Slot.Skeleton.Color.a * Slot.Color.a
  );
  Vertices[_c0] := c.r; Vertices[_c0 + 1] := c.g; Vertices[_c0 + 2] := c.b; Vertices[_c0 + 3] := c.a;
  Vertices[_c1] := c.r; Vertices[_c1 + 1] := c.g; Vertices[_c1 + 2] := c.b; Vertices[_c1 + 3] := c.a;
  Vertices[_c2] := c.r; Vertices[_c2 + 1] := c.g; Vertices[_c2 + 2] := c.b; Vertices[_c2 + 3] := c.a;
  Vertices[_c3] := c.r; Vertices[_c3 + 1] := c.g; Vertices[_c3 + 2] := c.b; Vertices[_c3 + 3] := c.a;
  UpdateWorldVertices(Slot.Bone);
  Render.Render(Region.Texture, @Vertices);
end;

procedure TSpineRegionAttachment.UpdateWorldVertices(const Bone: TSpineBone);
  var wx, wy, m00, m01, m10, m11: Single;
begin
  wx := Bone.WorldX;
  wy := Bone.WorldY;
  m00 := Bone.m00;
  m01 := Bone.m01;
  m10 := Bone.m10;
  m11 := Bone.m11;
  Vertices[_x0] := Offset[0] * m00 + Offset[1] * m01 + wx;
  Vertices[_y0] := Offset[0] * m10 + Offset[1] * m11 + wy;
  Vertices[_x1] := Offset[2] * m00 + Offset[3] * m01 + wx;
  Vertices[_y1] := Offset[2] * m10 + Offset[3] * m11 + wy;
  Vertices[_x2] := Offset[4] * m00 + Offset[5] * m01 + wx;
  Vertices[_y2] := Offset[4] * m10 + Offset[5] * m11 + wy;
  Vertices[_x3] := Offset[6] * m00 + Offset[7] * m01 + wx;
  Vertices[_y3] := Offset[6] * m10 + Offset[7] * m11 + wy;
end;
//TSpineRegionAttachment END

//TSpineRegionSequenceAttachment BEGIN
constructor TSpineRegionSequenceAttachment.Create(const NewName: AnsiString; const NewFrameTime: Single; const NewMode: TSpineRegionSequenceAttachmentMode);
begin
  inherited Create(NewName);
  FrameTime := NewFrameTime;
  Mode := NewMode;
end;

procedure TSpineRegionSequenceAttachment.Draw(const Render: TSpineRender; const Slot: TSpineSlot);
  var FrameIndex: Integer;
begin
  FrameIndex := Trunc(Slot.AttachmentTime / FrameTime);
  case Mode of
    amForward:
    begin
      if High(Regions) < FrameIndex then
      FrameIndex := High(Regions);
    end;
    amForwardLoop: FrameIndex := FrameIndex mod Length(Regions);
    amPingPong:
    begin
      FrameIndex := FrameIndex mod (Length(Regions) * 2);
      if FrameIndex >= Length(Regions) then
      FrameIndex := High(Regions) - (FrameIndex - Length(Regions));
    end;
    amRandom: FrameIndex := Random(Length(Regions));
    amBackward:
    begin
      if High(Regions) - frameIndex > 0 then
      FrameIndex := High(Regions) - frameIndex
      else
      FrameIndex := 0;
    end;
    amBackwardLoop:
    begin
      FrameIndex := FrameIndex mod Length(Regions);
      FrameIndex := High(Regions) - FrameIndex;
    end;
  end;
  SetRegion(Regions[FrameIndex]);
  inherited Draw(Render, Slot);
end;
//TSpineRegionSequenceAttachment END

//TSpineAttachmentResolver BEGIN
constructor TSpineAttachmentResolver.Create;
begin
  inherited Create;
  ObjectStore;
  Ref := 0;
end;

procedure TSpineAttachmentResolver.RefInc;
begin
  Ref := Ref + 1;
end;

procedure TSpineAttachmentResolver.RefDec;
begin
  Ref := Ref - 1;
  if Ref <= 0 then
  Free;
end;
//TSpineAttachmentResolver END

//TSpineTextureAtlasAttachmentResolver BEGIN
constructor TSpineTextureAtlasAttachmentResolver.Create(const NewAtlas: TSpineTextureAtlas);
begin
  inherited Create;
  Atlas := NewAtlas;
end;

procedure TSpineTextureAtlasAttachmentResolver.Resolve(const Attachment: TSpineAttachment);
  var Region: TSpineAtlasRegion;
begin
  if Attachment is TSpineRegionAttachment then
  begin
    Region := Atlas.FindRegion(Attachment.Name);
    TSpineRegionAttachment(Attachment).SetRegion(Region);
    Attachment.Resolved := True;
  end;
end;
//TSpineTextureAtlasAttachmentResolver END

//TSpineAnimation BEGIN
constructor TSpineAnimation.Create(const AnimationName: AnsiString);
begin
  inherited Create;
  ObjectStore;
  Ref := 0;
  Name := AnimationName;
  Timelines := nil;
  Duration := 0;
end;

destructor TSpineAnimation.Destroy;
  var i: Integer;
begin
  for i := 0 to High(Timelines) do
  Timelines[i].Free;
  Timelines := nil;
  Inherited Destroy;
end;

procedure TSpineAnimation.Apply(const Skeleton: TSpineSkeleton; const Time: Single; const Loop: Boolean);
  var i: Integer;
  var t: Single;
begin
  if Loop and (Duration > 0) then t := Time - Trunc(Time / Duration) * Duration else t := Time;
  for i := 0 to High(Timelines) do
  Timelines[i].Apply(Skeleton, t, 1);
end;

procedure TSpineAnimation.Mix(const Skeleton: TSpineSkeleton; const Time: Single; const Loop: Boolean; const Alpha: Single);
  var i: Integer;
  var t: Single;
begin
  if Loop and (Duration > 0) then t := Time - Trunc(Time / Duration) * Duration else t := Time;
  for i := 0 to High(Timelines) do
  Timelines[i].Apply(Skeleton, t, Alpha);
end;

procedure TSpineAnimation.AddTimeline(const Timeline: TSpineTimeline);
begin
  SetLength(Timelines, Length(Timelines) + 1);
  Timelines[High(Timelines)] := Timeline;
  if Timeline.GetDuration > Duration then
  Duration := Timeline.GetDuration;
end;

procedure TSpineAnimation.RefInc;
begin
  Ref := Ref + 1;
end;

procedure TSpineAnimation.RefDec;
begin
  Ref := Ref - 1;
  if Ref <= 0 then
  Free;
end;
//TSpineAnimation END

//TSpineCurveTimeline BEGIN
constructor TSpineCurveTimeline.Create(const NewKeyframeCount: Integer);
begin
  inherited Create;
  ObjectStore;
  LINEAR := 0;
  STEPPED := -1;
  BEZIER_SEGMENTS := 10;
  SetLength(_Curves, (NewKeyframeCount - 1) * 6);
end;

procedure TSpineCurveTimeline.SetLinear(const KeyframeIndex: Integer);
begin
  _Curves[KeyframeIndex * 6] := LINEAR;
end;

procedure TSpineCurveTimeline.SetStepped(const KeyframeIndex: Integer);
begin
  _Curves[KeyframeIndex * 6] := STEPPED;
end;

procedure TSpineCurveTimeline.SetCurve(const KeyframeIndex: Integer; const cx1, cy1, cx2, cy2: Single);
  var subdiv_step, subdiv_step2, subdiv_step3: Single;
  var pre1, pre2, pre4, pre5, tmp1x, tmp1y, tmp2x, tmp2y: Single;
  var i: Integer;
begin
  subdiv_step := 1 / BEZIER_SEGMENTS;
  subdiv_step2 := subdiv_step * subdiv_step;
  subdiv_step3 := subdiv_step2 * subdiv_step;
  pre1 := 3 * subdiv_step;
  pre2 := 3 * subdiv_step2;
  pre4 := 6 * subdiv_step2;
  pre5 := 6 * subdiv_step3;
  tmp1x := -cx1 * 2 + cx2;
  tmp1y := -cy1 * 2 + cy2;
  tmp2x := (cx1 - cx2) * 3 + 1;
  tmp2y := (cy1 - cy2) * 3 + 1;
  i := KeyframeIndex * 6;
  _Curves[i] := cx1 * pre1 + tmp1x * pre2 + tmp2x * subdiv_step3;
  _Curves[i + 1] := cy1 * pre1 + tmp1y * pre2 + tmp2y * subdiv_step3;
  _Curves[i + 2] := tmp1x * pre4 + tmp2x * pre5;
  _Curves[i + 3] := tmp1y * pre4 + tmp2y * pre5;
  _Curves[i + 4] := tmp2x * pre5;
  _Curves[i + 5] := tmp2y * pre5;
end;

function TSpineCurveTimeline.GetCurvePercent(const KeyframeIndex: Integer; const Percent: Single): Single;
  var CurveIndex, i: Integer;
  var x, y, lastx, lasty, dfx, dfy, ddfx, ddfy, dddfx, dddfy: Single;
begin
  CurveIndex := KeyframeIndex * 6;
  dfx := _Curves[CurveIndex];
  if dfx = LINEAR then
  begin
    Result := Percent;
    Exit;
  end;
  if dfx = STEPPED then
  begin
    Result := 0;
    Exit;
  end;
  dfy := _Curves[CurveIndex + 1];
  ddfx := _Curves[CurveIndex + 2];
  ddfy := _Curves[CurveIndex + 3];
  dddfx := _Curves[CurveIndex + 4];
  dddfy := _Curves[CurveIndex + 5];
  x := dfx; y := dfy;
  i := BEZIER_SEGMENTS - 2;
  while True do
  begin
    if x >= percent then
    begin
      lastx := x - dfx;
      lasty := y - dfy;
      Result := lasty + (y - lasty) * (Percent - lastx) / (x - lastx);
      Exit;
    end;
    if i = 0 then
    Break;
    Dec(i);
    dfx := dfx + ddfx;
    dfy := dfy + ddfy;
    ddfx := ddfx + dddfx;
    ddfy := ddfy + dddfy;
    x := x + dfx;
    y := y + dfy;
  end;
  Result := y + (1 - y) * (Percent - x) / (1 - x);
end;
//TSpineCurveTimeline END

//TSpineRotateTimeline BEGIN
constructor TSpineRotateTimeline.Create(const NewKeyframeCount: Integer);
begin
  inherited Create(NewKeyframeCount);
  SetLength(Frames, NewKeyframeCount * 2);
  LAST_FRAME_TIME := -2;
  FRAME_VALUE := 1;
end;

function TSpineRotateTimeline.GetDuration: Single;
begin
  Result := Frames[Length(Frames) - 2];
end;

function TSpineRotateTimeline.GetKeyframeCount: Integer;
begin
  Result := Length(Frames) div 2;
end;

procedure TSpineRotateTimeline.SetKeyframe(const KeyframeIndex: Integer; const Time, Value: Single);
  var i: Integer;
begin
  i := KeyframeIndex * 2;
  Frames[i] := Time;
  Frames[i + 1] := Value;
end;

procedure TSpineRotateTimeline.Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single);
  var Bone: TSpineBone;
  var Amount, LastFrameValue, FrameTime, Percent: Single;
  var FrameIndex: Integer;
begin
  if time < Frames[0] then Exit;
  Bone := Skeleton.Bones[boneIndex];
  if Time >= Frames[Length(Frames) - 2] then
  begin
    Amount := Bone.Data.Rotation + Frames[Length(Frames) - 1] - Bone.Rotation;
    while Amount > 180 do Amount := Amount - 360;
    while Amount < -180 do Amount := Amount + 360;
    Bone.Rotation := Bone.Rotation + Amount * Alpha;
    Exit;
  end;
  FrameIndex := SpineBinarySearch(@Frames[0], Length(Frames), Time, 2);
  LastFrameValue := Frames[FrameIndex - 1];
  FrameTime := Frames[FrameIndex];
  Percent := SpineClamp(1 - (Time - FrameTime) / (Frames[FrameIndex + LAST_FRAME_TIME] - FrameTime), 0, 1);
  Percent := GetCurvePercent(FrameIndex div 2 - 1, Percent);
  Amount := Frames[FrameIndex + FRAME_VALUE] - LastFrameValue;
  while Amount > 180 do Amount := Amount - 360;
  while Amount < -180 do Amount := Amount + 360;
  Amount := Bone.Data.Rotation + (LastFrameValue + Amount * Percent) - Bone.Rotation;
  while Amount > 180 do Amount := Amount - 360;
  while Amount < -180 do Amount := Amount + 360;
  Bone.Rotation := Bone.Rotation + Amount * Alpha;
end;
//TSpineRotateTimeline END

//TSpineTranslateTimeline BEGIN
constructor TSpineTranslateTimeline.Create(const NewKeyframeCount: Integer);
begin
  inherited Create(NewKeyframeCount);
  SetLength(Frames, NewKeyframeCount * 3);
  LAST_FRAME_TIME := -3;
  FRAME_X := 1;
  FRAME_Y := 2;
end;

function TSpineTranslateTimeline.GetDuration: Single;
begin
  Result := Frames[Length(Frames) - 3];
end;

function TSpineTranslateTimeline.GetKeyframeCount: Integer;
begin
  Result := Length(Frames) div 3;
end;

procedure TSpineTranslateTimeline.SetKeyframe(const KeyframeIndex: Integer; const Time, x, y: Single);
  var i: Integer;
begin
  i := KeyframeIndex * 3;
  Frames[i] := Time;
  Frames[i + 1] := x;
  Frames[i + 2] := y;
end;

procedure TSpineTranslateTimeline.Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single);
  var Bone: TSpineBone;
  var FrameIndex: Integer;
  var LastFrameX, LastFrameY, FrameTime, Percent: Single;
begin
  if Time < Frames[0] then Exit;
  Bone := Skeleton.Bones[BoneIndex];
  if Time >= Frames[Length(Frames) - 3] then
  begin
    Bone.x := Bone.x + (Bone.Data.x + Frames[Length(Frames) - 2] - bone.x) * Alpha;
    Bone.y := Bone.y + (Bone.Data.y + Frames[Length(Frames) - 1] - bone.y) * Alpha;
    Exit;
  end;
  FrameIndex := SpineBinarySearch(@Frames[0], Length(Frames), Time, 3);
  LastFrameX := Frames[FrameIndex - 2];
  LastFrameY := Frames[FrameIndex - 1];
  frameTime := Frames[FrameIndex];
  Percent := SpineClamp(1 - (Time - FrameTime) / (Frames[FrameIndex + LAST_FRAME_TIME] - FrameTime), 0, 1);
  Percent := GetCurvePercent(FrameIndex div 3 - 1, Percent);
  Bone.x := Bone.x + (Bone.Data.x + LastFrameX + (Frames[FrameIndex + FRAME_X] - LastFrameX) * Percent - Bone.x) * Alpha;
  Bone.y := Bone.y + (Bone.Data.y + LastFrameY + (Frames[FrameIndex + FRAME_Y] - LastFrameY) * Percent - Bone.y) * Alpha;
end;
//TSpineTranslateTimeline END

//TSpineScaleTimeline BEGIN
procedure TSpineScaleTimeline.Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single);
  var Bone: TSpineBone;
  var FrameIndex: Integer;
  var LastFrameX, LastFrameY, FrameTime, Percent: Single;
begin
  if Time < Frames[0] then Exit;
  Bone := Skeleton.Bones[BoneIndex];
  if Time >= Frames[Length(Frames) - 3] then
  begin
    Bone.ScaleX := Bone.ScaleX + (Bone.Data.ScaleX - 1 + Frames[Length(Frames) - 2] - Bone.ScaleX) * Alpha;
    Bone.ScaleY := Bone.ScaleY + (Bone.Data.ScaleY - 1 + Frames[Length(Frames) - 1] - Bone.ScaleY) * Alpha;
    Exit;
  end;
  FrameIndex := SpineBinarySearch(@Frames[0], Length(Frames), Time, 3);
  LastFrameX := Frames[FrameIndex - 2];
  LastFrameY := Frames[FrameIndex - 1];
  FrameTime := Frames[FrameIndex];
  Percent := SpineClamp(1 - (Time - FrameTime) / (Frames[FrameIndex + LAST_FRAME_TIME] - FrameTime), 0, 1);
  Percent := GetCurvePercent(FrameIndex div 3 - 1, Percent);
  Bone.ScaleX := Bone.ScaleX + (Bone.Data.ScaleX - 1 + LastFrameX + (Frames[FrameIndex + FRAME_X] - LastFrameX) * Percent - Bone.ScaleX) * Alpha;
  Bone.ScaleY := Bone.ScaleY + (Bone.Data.ScaleY - 1 + LastFrameY + (Frames[FrameIndex + FRAME_Y] - LastFrameY) * Percent - Bone.ScaleY) * Alpha;
end;
//TSpineScaleTimeline END

//TSpineColorTimeline BEGIN
constructor TSpineColorTimeline.Create(const NewKeyframeCount: Integer);
begin
  inherited Create(NewKeyframeCount);
  SetLength(Frames, NewKeyframeCount * 5);
  LAST_FRAME_TIME := -5;
  FRAME_R := 1;
  FRAME_G := 2;
  FRAME_B := 3;
  FRAME_A := 4;
end;

function TSpineColorTimeline.GetDuration: Single;
begin
  Result := Frames[Length(Frames) - 5];
end;

function TSpineColorTimeline.GetKeyframeCount: Integer;
begin
  Result := Length(Frames) div 5;
end;

procedure TSpineColorTimeline.SetKeyframe(const KeyframeIndex: Integer; const Time, r, g, b, a: Single);
  var i: Integer;
begin
  i := KeyframeIndex * 5;
  Frames[i] := time;
  Frames[i + 1] := r;
  Frames[i + 2] := g;
  Frames[i + 3] := b;
  Frames[i + 4] := a;
end;

procedure TSpineColorTimeline.Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single);
  var i, FrameIndex: Integer;
  var LastFrameR, LastFrameG, LastFrameB, LastFrameA, FrameTime, Percent, r, g, b, a: Single;
begin
  if Time < Frames[0] then Exit;
  if Time >= Frames[Length(Frames) - 5] then
  begin
    i := Length(Frames);
    Skeleton.Slots[SlotIndex].Color.r := Frames[i - 3];
    Skeleton.Slots[SlotIndex].Color.g := Frames[i - 2];
    Skeleton.Slots[SlotIndex].Color.b := Frames[i - 1];
    Skeleton.Slots[SlotIndex].Color.a := Frames[i];
    Exit;
  end;
  FrameIndex := SpineBinarySearch(@Frames[0], Length(Frames), Time, 5);
  LastFrameR := Frames[FrameIndex - 4];
  LastFrameG := Frames[FrameIndex - 3];
  LastFrameB := Frames[FrameIndex - 2];
  LastFrameA := Frames[FrameIndex - 1];
  FrameTime := Frames[FrameIndex];
  Percent := SpineClamp(1 - (Time - FrameTime) / (Frames[FrameIndex + LAST_FRAME_TIME] - FrameTime), 0, 1);
  Percent := GetCurvePercent(FrameIndex div 5 - 1, Percent);
  r := LastFrameR + (Frames[FrameIndex + FRAME_R] - LastFrameR) * Percent;
  g := LastFrameG + (Frames[FrameIndex + FRAME_G] - LastFrameG) * Percent;
  b := LastFrameB + (Frames[FrameIndex + FRAME_B] - LastFrameB) * Percent;
  a := LastFrameA + (Frames[FrameIndex + FRAME_A] - LastFrameA) * Percent;
  if Alpha < 1 then
  begin
    Skeleton.Slots[SlotIndex].Color.r := Skeleton.Slots[SlotIndex].Color.r + (r - Skeleton.Slots[SlotIndex].Color.r) * Alpha;
    Skeleton.Slots[SlotIndex].Color.g := Skeleton.Slots[SlotIndex].Color.g + (g - Skeleton.Slots[SlotIndex].Color.g) * Alpha;
    Skeleton.Slots[SlotIndex].Color.b := Skeleton.Slots[SlotIndex].Color.b + (b - Skeleton.Slots[SlotIndex].Color.b) * Alpha;
    Skeleton.Slots[SlotIndex].Color.a := Skeleton.Slots[SlotIndex].Color.a + (a - Skeleton.Slots[SlotIndex].Color.a) * Alpha;
  end
  else
  Skeleton.Slots[SlotIndex].Color := SpineColor(r, g, b, a);
end;
//TSpineColorTimeline END

//TSpineAttachmentTimeline BEGIN
constructor TSpineAttachmentTimeline.Create(const NewKeyframeCount: Integer);
begin
  inherited Create;
  ObjectStore;
  SetLength(Frames, NewKeyframeCount);
  SetLength(AttachmentNames, NewKeyframeCount);
end;

function TSpineAttachmentTimeline.GetDuration: Single;
begin
  Result := Length(Frames) - 1;
end;

function TSpineAttachmentTimeline.GetKeyframeCount: Integer;
begin
  Result := Length(Frames);
end;

procedure TSpineAttachmentTimeline.SetKeyframe(const KeyframeIndex: Integer; const Time: Single; const AttachmentName: AnsiString);
begin
  Frames[KeyframeIndex] := Time;
  AttachmentNames[KeyframeIndex] := AttachmentName;
end;

{$Hints off}
procedure TSpineAttachmentTimeline.Apply(const Skeleton: TSpineSkeleton; const Time, Alpha: Single);
  var FrameIndex: Integer;
  var AttachmentName: AnsiString;
begin
  if Time < Frames[0] then Exit;
  if Time >= Frames[Length(Frames) - 1] then
  FrameIndex := Length(Frames) - 1
  else
  FrameIndex := SpineBinarySearch(@Frames[0], Length(Frames), Time, 1) - 1;
  AttachmentName := AttachmentNames[FrameIndex];
  if Length(attachmentName) = 0 then
  Skeleton.Slots[SlotIndex].SetAttachment(nil)
  else
  Skeleton.Slots[SlotIndex].SetAttachment(Skeleton.GetAttachment(SlotIndex, AttachmentName));
end;
{$Hints on}
//TSpineAttachmentTimeline END

function SpineColor(const r, g, b, a: Single): TSpineColor;
begin
  Result.r := r;
  Result.g := g;
  Result.b := b;
  Result.a := a;
end;

function SpineClamp(const Value, MinLimit, MaxLimit: Single): Single;
begin
  if Value < MinLimit then Exit(MinLimit) else if Value > MaxLimit then Exit(MaxLimit) else Exit(Value);
end;

function SpineBinarySearch(const Values: PSpineFloatArray; const ValuesLength: Integer; const Target: Single; const Step: Integer): Integer;
  var l, h, c: Integer;
begin
  l := 0;
  h := ValuesLength div step - 2;
  if h = 0 then
  begin
    Result := Step;
    Exit;
  end;
  c := h shr 1;
  while True do
  begin
    if Values^[(c + 1) * Step] <= Target then
    l := c + 1
    else
    h := c;
    if l = h then
    begin
      Result := (l + 1) * Step;
      Exit;
    end;
    c := (l + h) shr 1;
  end;
  Result := 0
end;

procedure SpineInitialize;
begin
  SpineCleanUp := False;
  SpineObjects.Clear;
  SpineTextures.Clear;
end;

procedure SpineFinalize;
begin
  SpineCleanUp := True;
  while SpineObjects.Count > 0 do
  TSpineObject(SpineObjects[0]).Free;
  SpineObjects.Clear;
  while SpineTextures.Count > 0 do
  TSpineTexture(SpineTextures.Pop).Free;
  SpineTextures.Clear;
end;

initialization
begin
  SpineInitialize;
end;

finalization
begin
  SpineFinalize;
end;

end.
