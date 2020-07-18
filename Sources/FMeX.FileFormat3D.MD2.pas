unit FMeX.FileFormat3D.MD2;
// Credits :
// - Momor Prods 2002 - Emmanuel ICART - E-Mail: eicart@momorprods.com
// - JUST3D structures (polygonal renderer & support for MD2 meshes)
// - GLSCENE FileMD2 format and structure.

Interface


Uses
  SysUtils,
  Classes,
  System.Math.vectors,
  FMX.graphics,      //TBitmap
  FMX.Types,
  FMX.Types3D,    //TContext
  System.UITypes, //AlphaColor
  System.Types,   //Vector3D
  FMX.Objects3D,  //TCustomMesh;
  FMex.Types3d;

Type
  TSprite = TBitmap;

  PVertex=^TVertex;
  TVertex=record
   X,Y,Z:Single;    // 3D position
   rx,ry,rz:Single; // Rotated position
   sx,sy:integer;  // Screen position
   sz:Single;       // Depth
  end;
  TVertices=array[word] of TVertex;
  PVertices=^TVertices;

  TFace =class
   A,B,C:PVertex;  // 3 Vertices
   u1,v1:integer;  // Texture mapping coords (software)
   u2,v2:integer;  // Texture mapping coords (software)
   u3,v3:integer;  // Texture mapping coords (software)
   uu1,vv1:Single;  // Texture mapping coords (OpenGL)
   uu2,vv2:Single;  // Texture mapping coords (OpenGL)
   uu3,vv3:Single;  // Texture mapping coords (OpenGL)
   zz:Single;       //
   function GotPoint(P:PVertex):boolean;
   procedure ComputeZ;
   procedure Draw(var t:TSprite);
  end;

  // 3D object struct
  TMD2Object3D=class
   VertexCount:integer;
   Vertices   :PVertices;
   FacesO     :TList;
   texture    :TSprite;
   sx,sy,sz   :Single;
   Constructor Create;
   Destructor Destroy; override;
   Procedure Clear;
   function AddFace(a,b,c:integer):TFace;
   procedure Define(AVertexCount:integer);
  end;

 // MD2 structures
 TMD2Header=record
  Ident      :integer;
  Version    :integer;

  SkinWidth  :integer;
  SkinHeight :integer;
  FrameSize  :integer;

  SkinCount  :integer;
  VertexCount:integer;
  MapCount   :integer;
  FaceCount  :integer;
  GLCmdCount :integer;
  FrameCount :integer;

  SkinOfs    :integer;
  MapOfs     :integer;
  FaceOfs    :integer;
  FrameOfs   :integer;
  GLCmdOfs   :integer;
  EndOfFile  :integer;
 end;

 TSkinName =array[0..63] of ansichar;
 TSkinNames=array[0..31] of TSkinName;

 TMap=record u,v:smallint end;
 TMaps=array[0..2047] of TMap;

 TMD2Face=record
  a,b,c:smallint;
  ta,tb,tc:smallint;
 end;
 TMD2Faces=array[0..4095] of TMD2Face;

 TM2DVertex=record
  x,y,z:byte;
  light:byte;
 end;
 TMD2Frame=record
  Scale:record x,y,z:Single end;
  Trans:record x,y,z:Single end;
  Name :array[0..15] of Ansichar;
  Verts:array[0..2047] of TM2DVertex;
 end;
 TMD2Frames=array[0..511] of TMD2Frame;

 TMD2File=class
  fName:ansistring;
  header:TMD2Header;
  Skins :TSkinNames;
  Maps  :TMaps;
  Faces :TMD2Faces;
  Frames:TMD2Frames;
 public
  Constructor Create(AFileName:ansistring);
  Procedure GetPoint(frame,index:integer; var x,y,z:Single);
  function FrameName(frame:integer):ansistring;
 end;

 TFMeXModelRessource = Class
 Public
   Procedure BuildMesh(aData : TMeshData); Virtual; Abstract;
   Procedure BuildAnimList(aString : TStrings); Virtual; Abstract;
 End;

 TFMeXModelRessourceMD2 = Class(TFMeXModelRessource)
 Private
   InternalObject : TMD2Object3D;
   MD2File : TMD2File;
 Public
   Constructor Create;
   Destructor Destroy; Override;

   Procedure BuildMesh(aData : TMeshData); Override;
   Procedure BuildAnimList(aString : TStrings); Override;

   Procedure LoadFromFile(aMD2File : String; Const aTextureFile : string = '');
 End;

 TFMeXActor = Class(TeCustomMesh)
 Private
   FRessource : TFMexModelRessource;
   FAnim : TStrings;
   FCurrentFrame : Integer;

   Procedure SetRessource(Value : TFMexModelRessource);
   Procedure SetCurrentFrame(Value : Integer);
 Public
   constructor Create(AOwner: TComponent); override;
   destructor destroy; Override;

   Property Frames : TStrings read FAnim;
   Property CurrentFrame : Integer read FCurrentFrame Write SetCurrentFrame;

   Property ModelRessource : TFMexModelRessource read FRessource Write SetRessource;
 End;

// end of JUST3D structures definition

// ---------------------- Just3D/MD2 functions ----------------------
Procedure LoadMD2(Filename,ColorMap,AlphaMask:String;var MD2:TMD2File; var OBJ:TMD2Object3D);
Procedure GetMD2Frame(var MD2: TMD2File;frame:longint;var OBJ:TMD2Object3D);

implementation
{$G-}

Procedure LoadMD2(Filename,ColorMap,AlphaMask:String;var MD2:TMD2File; var OBJ:TMD2Object3D);
begin
 OBJ.Define(MD2.Header.VertexCount);
 OBJ.texture := TSprite(TBitmap.Create(0,0)); { TODO : Change to Use a TMaterial here... }
 if Length(ColorMap)>0 then
   OBJ.texture.LoadFromFile(ColorMap);
 OBJ.sx:=-1;
 GetMD2Frame(MD2,0,OBJ);
end;

Procedure GetMD2Frame(var MD2: TMD2File;frame:longint;var OBJ:TMD2Object3D);
var i:longint;
    f:TFace;
    xmin,xmax,ymin,ymax,zmin,zmax:Single;

    tw,th : integer;
begin
 xmin:=0;
 xmax:=0;
 ymin:=0;
 ymax:=0;
 zmin:=0;
 zmax:=0;

 for i:=0 to OBJ.VertexCount-1 do
 begin
   MD2.GetPoint(frame,i,OBJ.Vertices[i].x,OBJ.Vertices[i].y,OBJ.Vertices[i].z);
   if OBJ.Vertices[i].x<xmin then xmin:=OBJ.Vertices[i].x;
   if OBJ.Vertices[i].x>xmax then xmax:=OBJ.Vertices[i].x;
   if OBJ.Vertices[i].y<ymin then ymin:=OBJ.Vertices[i].y;
   if OBJ.Vertices[i].y>ymax then ymax:=OBJ.Vertices[i].y;
   if OBJ.Vertices[i].z<zmin then zmin:=OBJ.Vertices[i].z;
   if OBJ.Vertices[i].z>zmax then zmax:=OBJ.Vertices[i].z;
 end;

 // First time: set the faces
 if OBJ.sx<0 then
 begin
   OBJ.sx:=xmax-xmin;
   OBJ.sy:=ymax-ymin;
   OBJ.sz:=zmax-zmin;

   OBJ.FacesO.Clear;

   for i:=0 to MD2.Header.FaceCount-1 do
   begin
      f:=OBJ.AddFace(MD2.Faces[i].a,MD2.Faces[i].b,MD2.Faces[i].c);

{      A := Triangle.VertexIndex[2];
      B := Triangle.VertexIndex[1];
      C := Triangle.VertexIndex[0];
      A_S := TextureCoords[Triangle.TextureCoordIndex[2]][0] / Header.SkinWidth;
      A_T := TextureCoords[Triangle.TextureCoordIndex[2]][1] / Header.SkinHeight;
      B_S := TextureCoords[Triangle.TextureCoordIndex[1]][0] / Header.SkinWidth;
      B_T := TextureCoords[Triangle.TextureCoordIndex[1]][1] / Header.SkinHeight;
      C_S := TextureCoords[Triangle.TextureCoordIndex[0]][0] / Header.SkinWidth;
      C_T := TextureCoords[Triangle.TextureCoordIndex[0]][1] / Header.SkinHeight;
}

      tw := OBJ.texture.Width;
      th := OBJ.texture.Height;

      tw := MD2.header.SkinWidth;
      th := MD2.header.SkinHeight;

      f.u1:=MD2.Maps[MD2.Faces[i].ta].u; // / MD2.header.SkinWidth;
      f.v1:=MD2.Maps[MD2.Faces[i].ta].v; // / MD2.header.SkinHeight;
      f.u2:=MD2.Maps[MD2.Faces[i].tb].u; // / MD2.header.SkinWidth;
      f.v2:=MD2.Maps[MD2.Faces[i].tb].v; // / MD2.header.SkinHeight;
      f.u3:=MD2.Maps[MD2.Faces[i].tc].u; // / MD2.header.SkinWidth;
      f.v3:=MD2.Maps[MD2.Faces[i].tc].v; // / MD2.header.SkinHeight;

        f.uu1:= f.u1 / tw;
        f.vv1:= f.v1 / th;
        f.uu2:= f.u2 / tw;
        f.vv2:= f.v2 / th;
        f.uu3:= f.u3 / tw;
        f.vv3:= f.v3 / th;


{
      f.u1:=(MD2.Maps[MD2.Faces[i].ta].u*OBJ.texture.Width) div MD2.header.SkinWidth;
      f.v1:=(MD2.Maps[MD2.Faces[i].ta].v*OBJ.texture.Height) div MD2.header.SkinHeight;
      f.u2:=(MD2.Maps[MD2.Faces[i].tb].u*OBJ.texture.Width) div MD2.header.SkinWidth;
      f.v2:=(MD2.Maps[MD2.Faces[i].tb].v*OBJ.texture.Height) div MD2.header.SkinHeight;
      f.u3:=(MD2.Maps[MD2.Faces[i].tc].u*OBJ.texture.Width) div MD2.header.SkinWidth;
      f.v3:=(MD2.Maps[MD2.Faces[i].tc].v*OBJ.texture.Height) div MD2.header.SkinHeight;
}
{
      if (OBJ.texture.Width>0) And (OBJ.texture.Height>0) then
      begin
        tw := OBJ.texture.Width;
        th := OBJ.texture.Height;

        f.uu1 := MD2.Maps[MD2.Faces[i].ta].u / tw;
        f.vv1 := MD2.Maps[MD2.Faces[i].ta].v / th;
        f.uu2 := MD2.Maps[MD2.Faces[i].tb].u / tw;
        f.vv2 := MD2.Maps[MD2.Faces[i].tb].v / th;
        f.uu3 := MD2.Maps[MD2.Faces[i].tc].u / tw;
        f.vv3 := MD2.Maps[MD2.Faces[i].tc].v / th;
{
        f.uu1 := MD2.Maps[MD2.Faces[i].ta].u / tw;
        f.uu2 := MD2.Maps[MD2.Faces[i].tb].u / tw;
        f.uu3 := MD2.Maps[MD2.Faces[i].tc].u / tw;
        f.vv1 := MD2.Maps[MD2.Faces[i].ta].v / th;
        f.vv2 := MD2.Maps[MD2.Faces[i].tb].v / th;
        f.vv3 := MD2.Maps[MD2.Faces[i].tc].v / th;


        f.uu1:= f.u1 / tw;
        f.vv1:= f.v1 / th;
        f.uu2:= f.u2 / tw;
        f.vv2:= f.v2 / th;
        f.uu3:= f.u3 / tw;
        f.vv3:= f.v3 / th;
      end;
}

   end;
 end;
end;


Constructor TMD2Object3D.Create;
 begin
  VertexCount:=0;
  Vertices:=nil;
  FacesO:=TList.Create;
 end;

Destructor TMD2Object3D.Destroy;
 begin
  Clear;
  FacesO.Free;
  Inherited;
 end;


Procedure TMD2Object3D.Clear;
 begin
  FreeMem(Vertices);
  VertexCount:=0;
  FacesO.Clear;
 end;

function TMD2Object3D.AddFace(a,b,c:integer):TFace;
 begin
  Result:=TFace.Create;
  Result.A:=@Vertices^[a];
  Result.B:=@Vertices^[b];
  Result.C:=@Vertices^[c];
  FacesO.Add(Result);
 end;

procedure TMD2Object3D.Define(AVertexCount:integer);
 begin
  VertexCount:=AVertexCount;
  GetMem(Vertices,VertexCount*SizeOf(TVertex));
 end;


function TFace.GotPoint(P:PVertex):boolean;
 begin
  Result:=(A=P)or(B=P)or(C=P);
 end;

procedure TFace.ComputeZ;
 begin
  zz:=(a.sz+b.sz+c.sz)/3;
 end;


Procedure TFace.Draw(var t:TSprite);
var x1,y1,x2,y2,nz:Single;
 begin
  if zz>0.01 then
   begin
   // Back-face removal
   x1:=b.rx-a.rx;
   y1:=b.ry-a.ry;
   x2:=c.rx-a.rx;
   y2:=c.ry-a.ry;
   nz:=x1*y2-x2*y1;

   if nz<0 then
    begin

    {
    BeginPoly(a.sx,a.sy,a.sz,u1,v1);
    ScanPoly(b.sx,b.sy,b.sz,u2,v2);
    ScanPoly(c.sx,c.sy,c.sz,u3,v3);
    ScanPoly(a.sx,a.sy,a.sz,u1,v1);
    EndPoly(Round(zz),t,Camera);
    }
    end;
   end;
 end;


Constructor TMD2File.Create(AFileName:ansistring);
 var f:integer;
     s:longint;

     m : TMemoryStream;
 begin

  fName:=AFileName;
  if FileExists(fName)=false then
    raise exception.Create('Unable to Map file '+fName);


  m := TMemoryStream.Create;
  m.LoadFromFile(fName);

  m.Position := 0;
  m.Read(Header,sizeof(header));

  m.Position := header.SkinOfs;
  m.Read(Skins,sizeof(Skins));

  m.Position :=
  m.Read(Maps,header.MapCount*SizeOf(Maps[0]));

  m.Position := header.FaceOfs;
  m.Read(Faces,sizeof(Faces));

  For s:=0 to Header.FaceCount-1 do
  begin
    m.Position := header.FrameOfs+s*header.FrameSize;
    m.Read(Frames[s], header.FrameSize);
  end;
 end;


Procedure TMD2File.GetPoint(frame,index:integer; var x,y,z:Single);
// var
//  F:^TMD2Frame;
 begin
//  F:=pointer(integer(Frames)+frame*Header.FrameSize);
//  with F^ do begin
   With Frames[frame] do begin
   x:=Verts[index].x*Scale.x+Trans.x;
   y:=Verts[index].y*Scale.y+Trans.y;
   z:=Verts[index].z*Scale.z+Trans.z;
  end;
 end;

function TMD2File.FrameName(frame:integer):ansistring;
// var
//  F:^TMD2Frame;
 begin
//  F:=pointer(integer(Frames)+frame*Header.FrameSize);
//  Result:=F^.Name;
    Result:=Frames[frame].Name;
 end;


 //-------------------------------------------------------

Procedure TFMeXModelRessourceMD2.BuildMesh(aData : TMeshData);
 var
  i,vIndex,vIndexMax:integer;
  aFace : TFace;
 begin
    aData.VertexBuffer.Length := InternalObject.FacesO.Count * 3;
    aData.IndexBuffer.Length := InternalObject.FacesO.Count * 3;
    vIndex :=0;
    vIndexMAx := InternalObject.FacesO.Count * 3;

    for i:=0 to InternalObject.FacesO.Count-1 do
    begin
        aFace := TFace(InternalObject.FacesO[i]);

        aData.VertexBuffer.Vertices[vIndex] := Point3d(aFace.a.x,aFace.a.y,aFace.a.z);
        //if vIndex in [c,c+1] then
        aData.VertexBuffer.TexCoord0[vIndex] := PointF(aFace.uu1,aFace.vv1);
        aData.IndexBuffer[vIndex] := vIndex;
        vIndex := vIndex + 1;
        vIndexMax := vIndexMax - 1;

        aData.VertexBuffer.Vertices[vIndex] := Point3d(aFace.b.x,aFace.b.y,aFace.b.z);
        //if vIndex in [c,c+1] then
        aData.VertexBuffer.TexCoord0[vIndex] := PointF(aFace.uu2,aFace.vv2);
        aData.IndexBuffer[vIndex] := vIndex;
        vIndex := vIndex + 1;
        vIndexMax := vIndexMax - 1;

        aData.VertexBuffer.Vertices[vIndex] := Point3d(aFace.c.x,aFace.c.y,aFace.c.z);
        //if vIndex in [c,c+1] then
        aData.VertexBuffer.TexCoord0[vIndex] := PointF(aFace.uu3,aFace.vv3);
        aData.IndexBuffer[vIndex] := vIndex;
        vIndex := vIndex + 1;
        vIndexMax := vIndexMax - 1;
    end;

    { TODO : remove duplicates vertex }

    aData.CalcFaceNormals;
end;

Procedure TFMeXModelRessourceMD2.BuildAnimList(aString : TStrings);
var i : Integer;
begin
  Assert(Assigned(aString));
  aString.Clear;
  For i := 0 to MD2File.header.FrameCount-1 do
  begin
    aString.Add(MD2File.FrameName(i));
  end;
end;


Procedure TFMeXModelRessourceMD2.LoadFromFile(aMD2File : String; Const aTextureFile : string = '');
begin
   MD2File:=TMD2File.Create(aMD2File);
   LoadMD2(amd2File,aTextureFile,'',MD2File,InternalObject);
End;

Constructor TFMeXModelRessourceMD2.Create;
begin
  inherited;
  InternalObject:=TMD2Object3D.Create;
end;

Destructor TFMeXModelRessourceMD2.Destroy;
begin
  FreeAndNil(MD2File);
  FreeAndNil(InternalObject);
end;

Procedure TFMeXActor.SetRessource(Value : TFMexModelRessource);
begin
  if Assigned(Value) then
  begin
    //FRessource.Clean; TODO.
    FRessource:= Value;
    FRessource.BuildMesh(Data);
    FRessource.BuildAnimList(FAnim);
    CurrentFrame := 0;
    Repaint;
  end;
end;

Procedure TFMexActor.SetCurrentFrame(Value : Integer);
begin
  GetMD2Frame( TFMeXModelRessourceMD2(FRessource).MD2File, Value, TFMeXModelRessourceMD2(FRessource).InternalObject);
  TFMeXModelRessourceMD2(FRessource).BuildMesh(Data);
end;

constructor TFMexActor.Create(AOwner: TComponent);
begin
  inherited;
  FAnim := TStringList.Create;
end;
destructor TFMexActor.Destroy;
begin
  FreeAndNil(FAnim);
  inherited;
end;


end.
