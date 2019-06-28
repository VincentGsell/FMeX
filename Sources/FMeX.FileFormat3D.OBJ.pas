unit FMeX.FileFormat3D.OBJ;

interface

uses Generics.Collections,
     System.Types,
     System.Math,
     System.Math.Vectors,
     System.UITypes,
     FMX.Types3D,
     FMeX.Types3D,
     SysUtils,
     Classes;


Type
  TFMeXObjMaterial = Record
  end;
  TFMeXObjMaterialArray = Array of TFMeXObjMaterial;

procedure FMeXLoadRawOBJ(aFilename : String; var aMesh : TeCustomMesh; const RecalNormals : Boolean = true);
procedure FMeXLoadRawOBJEx(aFilename : String; var aMesh : TeCustomMesh; const RecalNormals : Boolean = true);

implementation

Type
  TRawFace = packed record
    VerticeID : Integer;
    NormalID : Integer;
    TextureID : Integer;
  end;

  TRawVertices = packed record
    vertice : TPoint3d;
    normal : TPoint3d;
    texture : TPointf;

    function ToString : String;
  end;
  PRawVertices = ^TRawVertices;

function SamePoint(const v1, v2: TPoint3D): Boolean; overload;
begin
  Result := CompareMem(@v1, @v2, SizeOf(v1));
end;

function SamePoint(const v1, v2: TPointF): Boolean; overload;
begin
  Result := CompareMem(@v1, @v2, SizeOf(v1));
end;

procedure FMeXLoadRawOBJ(aFilename : String; var aMesh : TeCustomMesh; const RecalNormals : Boolean = true);
var o,m : TStringList;
    dum : string;
    i : Integer;
    lold : Char;

    vf1,vf2,vf3 : Single;
    v_Indice : Integer;
    vn_Indice : Integer;
    vt_Indice : Integer;
    f_Indice : Integer;
    f_IndexIndice : integer;

    //DataSource
    RawVertices : Array of TPoint3d;
    RawNormals : Array of TPoint3d;
    RawUVMap : Array of TPointf;
//    RawFaces : Array of TRawFace;

    //DataTarget.
    Vertices : Array of TRawVertices;
    lv : PRawVertices;
    NewVertices : Array of PRawVertices;
    N,F : Integer;
    VFnd : Boolean;


    f_vi, f_vt, f_vn : Integer;

    verts : TDictionary<string,integer>;
    verta : TArray<Integer>;
    j : integer;


  LPos: Integer;
  LFile : TextFile;
  LLine : string;


    function IndiceFix(aStr : String) : Integer;
    begin
      //indice >0 -> form beginning, <0, from the end of vertices list.
      result := StrToIntDef(aStr,0);
      if result<0 then
        result := length(RawVertices) + result
      else
        result := result-1;
    end;

    function IndiceFixTexture(aStr : String) : Integer;
    begin
      //indice >0 -> form beginning, <0, from the end of vertices list.
      result := StrToIntDef(aStr,0);
      if result<0 then
        result := length(RawUVMap) + result
      else
        result := result-1;
    end;

    function IndiceFixNormal(aStr : String) : Integer;
    begin
      //indice >0 -> form beginning, <0, from the end of vertices list.
      result := StrToIntDef(aStr,0);
      if result<0 then
        result := length(RawNormals) + result
      else
        result := result-1;
    end;


    procedure AssignFaceData;
    begin
      Vertices[f_IndexIndice].vertice := RawVertices[f_vi];
      //RawFaces[f_IndexIndice].VerticeID := f_vi;
      if m.Count>1 then
      begin
        Vertices[f_IndexIndice].texture := pointf(0,0);
        //RawFaces[f_IndexIndice].TextureID := 0;
        f_vt := IndiceFixTexture(m[1]);
        if f_vt>0 then
        begin
          Vertices[f_IndexIndice].texture := RawUVMap[f_vt];
          //RawFaces[f_IndexIndice].TextureID := f_vt;
        end;
      end;
      if m.Count>2 then
      begin
        Vertices[f_IndexIndice].normal := Point3D(0,0,0);
        //RawFaces[f_IndexIndice].NormalID := 0;
        f_vn := IndiceFixNormal(m[2]);
        if f_vn>0 then
        begin
          Vertices[f_IndexIndice].normal := RawNormals[f_vn];
          //RawFaces[f_IndexIndice].NormalID := f_vn;
        end;
      end;
    end;

begin
  Assert(FileExists(aFilename));
  Assert(Assigned(aMesh));
  aMesh.Data.Clear;

  FormatSettings.DecimalSeparator := '.';
  o :=  TStringList.Create;
  m :=  TStringList.Create;
  try

    o.Delimiter := ' ';
    v_Indice :=0;
    vn_Indice :=0;
    vt_Indice :=0;
    f_Indice :=0;

    f_IndexIndice :=0;


    AssignFile(LFile, AFileName);
    Reset(LFile);
  //    LSubMesh := nil;
  //    LMesh := nil;
  //    FSmoothGroup := 0;


    try
      while not EOF(LFile) do
      begin
        Readln(LFile, LLine);
        if (LLine <> '') and (LLine.Chars[0] <> '#') then
        begin
          dum := LLine;
          case dum.Chars[0] of
            'm' :; //LMaterialsFile := ReadMaterials(LLine);
            'v' :
            begin
              if dum[2]= ' ' then //vertex.
                v_Indice := v_Indice + 1
              else
              if dum[2]= 'n' then //vertex normals.
                vn_Indice := vn_Indice + 1
              else
              if dum[2]= 't' then //vertex texture coord
                vt_Indice := vt_Indice + 1;
            end;
            'g' : ;//LMesh := ReadGeometry(LLine);
            'u' :
              begin
                //if LMesh = nil then
                //  LMesh := ReadGeometry('g default');
                //LSubMesh := LMesh.ReadUseMaterial(LLine, FVertexSource);
              end;
            's' : ;//ReadSmoothGroup(LLine);
            'f' :
              begin
                o.DelimitedText := dum;
                f_Indice := f_Indice + 1;
                if o.Count=5 then //Square (2 triangles)
                  f_Indice := f_Indice + 3;
                //if (LMesh = nil) then
                //   LMesh := ReadGeometry('g default');

                //if (LSubMesh = nil) then
                //    LSubMesh := LMesh.ReadUseMaterial('usemtl Default', FVertexSource);
                //  ReadFaces(LSubMesh, LLine.Trim);
              end;
          end;
        end;
      end;

{
    for i := 0 to l.Count-1 do
    begin
      //o.DelimitedText := trim(l[i]);
      //if o.Count=0 then
      //  Continue;
      dum := trim(l[i]);
      if length(dum)<1 then
        Continue;

      if dum[1] = 'v' then //vertex.
      begin
        if dum[2]= ' ' then //vertex.
          v_Indice := v_Indice + 1
        else
        if dum[2]= 'n' then //vertex normals.
          vn_Indice := vn_Indice + 1
        else
        if dum[2]= 't' then //vertex texture coord
          vt_Indice := vt_Indice + 1;
      end
      else
      if dum[1] = 'f' then //face
      begin
        o.DelimitedText := dum;
        f_Indice := f_Indice + 1;
        if o.Count=5 then //Square (2 triangles)
          f_Indice := f_Indice + 1;


      end;
    end;
}
      //Pre-Allocate source data
      SetLength(RawVertices,v_indice);
      SetLength(RawNormals,vn_Indice);
      SetLength(RawUVMap,vt_Indice);
      //Building target Data.
      SetLength(Vertices,f_Indice*3);

      v_Indice :=0;
      vn_Indice :=0;
      vt_Indice :=0;
      f_Indice :=0;

      //Gathering source data by rescanning file.

      reset(LFile);
      while not EOF(LFile) do
      begin
        Readln(LFile, LLine);
        if (LLine <> '') and (LLine.Chars[0] <> '#') then
        begin
          o.DelimitedText := trim(LLIne);
          if o.Count=0 then
            Continue;
          if o[0] = 'v' then //vertex.
          begin
            rtAssert(o.Count>=4,'Obj file error : Parsing "v" error Line '+IntToStr(i));
            vf1 := StrToFloatDef(o[1],0);
            vf2 := StrToFloatDef(o[2],0);
            vf3 := StrToFloatDef(o[3],0);

            RawVertices[v_Indice] := Point3D(vf1,vf2,vf3);
            //aMesh.Data.VertexBuffer.Vertices[v_Indice] := Point3D(vf1,vf2,vf3);
            //aMesh.Data.VertexBuffer.Normals[v_Indice] := Point3D(vf1,vf2,vf3); ?
            v_Indice := v_Indice + 1;
          end
          else
          if o[0] = 'vn' then //vertex normals
          begin
            rtAssert(o.Count=4,'Obj file error : Parsing "vn" error Line '+IntToStr(i));
            vf1 := StrToFloatDef(o[1],0);
            vf2 := StrToFloatDef(o[2],0);
            vf3 := StrToFloatDef(o[3],0);

            RawNormals[vn_Indice] := Point3D(vf1,vf2,vf3);
            vn_Indice := vn_Indice + 1;
          end
          else
          if o[0] = 'vt' then //vertex texture coord
          begin
            //rtAssert(o.Count=4,'Obj file error : Parsing "vt" error Line '+IntToStr(i));
            if o.Count>=3 then
            begin
              vf1 := StrToFloatDef(o[1],0);
              vf2 := StrToFloatDef(o[2],0);
              //vf3 := StrToFloatDef(o[3],0);
              //aMesh.Data.VertexBuffer.TexCoord0[vt_Indice] := Pointf(Vector(vf1,vf2));
              RawUVMap[vt_Indice] := Pointf(vf1, 1.0 - vf2);
              vt_Indice := vt_Indice + 1;
            end
          end
          else
          if o[0] = 'f' then //face
          begin
            m.Delimiter := ' ';
            if pos('/',LLine)>0 then
              m.Delimiter := '/';

            if o.Count>=4 then  //tri
            begin
              //supported :
              //f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3
              //f v1/vt1 v2/vt2 v3/vt3 ... vertex indice, texture indice.
              //f v1//vn1 v2//vn2 v3//vn3 ... (Without texture indice)
              //f v1 v2 v3 ....


              {
              0 1 2 3
              The division into two triangles would be one with the first 3 indices, and one with the first, third, and fourth. In this example:

              0 1 2
              0 2 3
              Let's try some ASCII art to illustrate this:

              3-------2
              |      /|
              |    /  |
              |  /    |
              |/      |
              0-------1
              Here you see 0 1 2 3 as the quad, 0 1 2 as the first triangle (bottom-right), and 0 2 3 as the second triangle (top left).

              More generally, for faces with n vertices, you generate triangles:

              0 (i) (i + 1)  [for i in 1..(n - 2)]
              If you don't insist on separate triangles, you can also use GL_TRIANGLE_FAN primitives, which are still in core OpenGL. That way, you can draw any convex polygon with a triangle fan, using the original sequence of indices. So a triangle fan with vertex sequence 0 1 2 3 describes the quad in this case, and it very easily generalizes to faces with more than 4 vertices.

              Edit: Since you still appear to have problems, let's see how this applies to the example in your post. I'll list the original index sequence of the quad for each face, and the index sequence for the two triangles after splitting the quad.

              f 1 2 3 4 --> (1 2 3) (1 3 4)
              f 8 7 6 5 --> (8 7 6) (8 6 5)
              f 4 3 7 8 --> (4 3 7) (4 7 8)
              f 5 1 4 8 --> (5 1 4) (5 4 8)
              f 5 6 2 1 --> (5 6 2) (5 2 1)
              f 2 6 7 3 --> (2 6 7) (2 7 3)
              }


              m.DelimitedText := o[1];
              f_vi := IndiceFix(m[0]);
              AssignFaceData;

              f_IndexIndice := f_IndexIndice + 1;

              m.DelimitedText := o[2];
              f_vi := IndiceFix(m[0]);
              AssignFaceData;

              f_IndexIndice := f_IndexIndice + 1;

              m.DelimitedText := o[3];
              f_vi := IndiceFix(m[0]);
              AssignFaceData;

              f_IndexIndice := f_IndexIndice + 1;

              if o.Count = 5  then    //Square. 2 triangle more...
              begin

                m.DelimitedText := o[1];
                f_vi := IndiceFix(m[0]);
                AssignFaceData;

                f_IndexIndice := f_IndexIndice + 1;

                m.DelimitedText := o[3];
                f_vi := IndiceFix(m[0]);
                AssignFaceData;

                f_IndexIndice := f_IndexIndice + 1;

                m.DelimitedText := o[4];
                f_vi := IndiceFix(m[0]);
                AssignFaceData;

                f_IndexIndice := f_IndexIndice + 1;
              end;

            end;
          end;
          f_Indice := f_Indice + 1;
        end;
      end;


    finally
      CloseFile(LFile);
    end;


    //snap memory.
    SetLength(RawVertices,0);
    SetLength(RawUVMap,0);
    SetLength(RawNormals,0);
//    SetLength(RawFaces,0);

    //Building final target.
//    aMesh.Locked := true;
//    aMesh.HitTest := false;

    aMesh.Data.IndexBuffer.Length := Length(Vertices);
    N := aMesh.Data.IndexBuffer.Length;

    SetLength(NewVertices, Length(Vertices));
    verts := TDictionary<String,integer>.Create;
    N := 0;
    for i := 0 to aMesh.Data.IndexBuffer.Length - 1 do
    begin
      lv := @Vertices[i];
      if verts.TryGetValue(Vertices[i].ToString,j) then
      begin
        aMesh.Data.IndexBuffer[i] := j;
      end
      else
      begin
        verts.add(Vertices[i].ToString,N);
        NewVertices[N] := lv;
        aMesh.Data.IndexBuffer[i] := N;
        inc(N);
      end;
    end;
    //verta := verts.Values.ToArray;
    verts.Free;

    aMesh.Data.VertexBuffer.Length := N;
    for I := 0 to N - 1 do
    begin
      //LVec := LNewTriangles[V].Pos * ATransform;
      aMesh.Data.VertexBuffer.Vertices[i] := NewVertices[i].vertice;
      //LVec := TPoint3D(TVector3D.Create(LNewTriangles[V].Nor, 0) * ANormalTransform).Normalize;
      aMesh.Data.VertexBuffer.Normals[i] := NewVertices[i].normal;
      aMesh.Data.VertexBuffer.TexCoord0[i] := NewVertices[i].texture;
    end;
    SetLength(Vertices,0);


   if RecalNormals then
     aMesh.Data.CalcFaceNormals;

  finally
    FreeAndNil(o);
    FreeAndNil(m);
  end;

end;




procedure FMeXLoadRawOBJEx(aFilename : String; var aMesh : TeCustomMesh; const RecalNormals : Boolean = true);
var o,m : TStringList;
    dum : string;
    i : Integer;
    lold : Char;

    vf1,vf2,vf3 : Single;
    v_Indice : Integer;
    vn_Indice : Integer;
    vt_Indice : Integer;
    f_Indice : Integer;
    f_IndexIndice : integer;

    //DataSource
    RawVertices : Array of TPoint3d;
    RawNormals : Array of TPoint3d;
    RawUVMap : Array of TPointf;

    f_vi, f_vt, f_vn : Integer;

    verts : TDictionary<string,integer>;
    verta : TArray<Integer>;
    j : integer;


  LPos: Integer;
  LFile : TextFile;
  LLine : string;


    function IndiceFix(aStr : String) : Integer;
    begin
      //indice >0 -> form beginning, <0, from the end of vertices list.
      result := StrToIntDef(aStr,0);
      if result<0 then
        result := length(RawVertices) + result
      else
        result := result-1;
    end;

    function IndiceFixTexture(aStr : String) : Integer;
    begin
      //indice >0 -> form beginning, <0, from the end of vertices list.
      result := StrToIntDef(aStr,0);
      if result<0 then
        result := length(RawUVMap) + result
      else
        result := result-1;
    end;

    function IndiceFixNormal(aStr : String) : Integer;
    begin
      //indice >0 -> form beginning, <0, from the end of vertices list.
      result := StrToIntDef(aStr,0);
      if result<0 then
        result := length(RawNormals) + result
      else
        result := result-1;
    end;


    procedure AssignFaceData;
    begin
      try
        aMesh.Data.VertexBuffer.Vertices[f_IndexIndice] := RawVertices[f_vi];
      Except
        on E : EXception do
        begin
          raise Exception.Create(e.Message+ IntToStr(aMesh.Data.IndexBuffer.Length)+ 'VERTEX  Vertices['+inttostr(f_IndexIndice)+'] := RawVertices['+IntToStr(f_vi)+'];');
        end;
      end;

      try
        aMesh.Data.IndexBuffer.Indices[f_IndexIndice] := f_IndexIndice;
      Except
        on E : EXception do
        begin
          raise Exception.Create(e.Message+ IntToStr(aMesh.Data.IndexBuffer.Length)+ 'INDEX  Indices['+inttostr(f_IndexIndice)+'] := '+IntToStr(f_vi)+';');
        end;
      end;


      //RawFaces[f_IndexIndice].VerticeID := f_vi;
      if m.Count>1 then
      begin
        aMesh.Data.VertexBuffer.TexCoord0[f_IndexIndice] := pointf(0,0);
        //RawFaces[f_IndexIndice].TextureID := 0;
        f_vt := IndiceFixTexture(m[1]);
        if f_vt>0 then
        begin
          aMesh.Data.VertexBuffer.TexCoord0[f_IndexIndice] := RawUVMap[f_vt];
          //RawFaces[f_IndexIndice].TextureID := f_vt;
        end;
      end;
      if m.Count>2 then
      begin
        aMesh.Data.VertexBuffer.Normals[f_IndexIndice] := Point3D(0,0,0);
        //RawFaces[f_IndexIndice].NormalID := 0;
        f_vn := IndiceFixNormal(m[2]);
        if f_vn>0 then
        begin
          aMesh.Data.VertexBuffer.Normals[f_IndexIndice] := RawNormals[f_vn];
          //RawFaces[f_IndexIndice].NormalID := f_vn;
        end;
      end;
    end;

begin
  Assert(FileExists(aFilename));
  Assert(Assigned(aMesh));
  aMesh.Data.Clear;

  FormatSettings.DecimalSeparator := '.';
  o :=  TStringList.Create;
  m :=  TStringList.Create;
  try

    o.Delimiter := ' ';
    v_Indice :=0;
    vn_Indice :=0;
    vt_Indice :=0;
    f_Indice :=0;

    f_IndexIndice :=0;


    AssignFile(LFile, AFileName);
    Reset(LFile);
  //    LSubMesh := nil;
  //    LMesh := nil;
  //    FSmoothGroup := 0;


    try
      while not EOF(LFile) do
      begin
        Readln(LFile, LLine);
        if (LLine <> '') and (LLine.Chars[0] <> '#') then
        begin
          dum := LLine;
          case dum.Chars[0] of
            'm' :; //LMaterialsFile := ReadMaterials(LLine);
            'v' :
            begin
              if dum[2]= ' ' then //vertex.
                v_Indice := v_Indice + 1
              else
              if dum[2]= 'n' then //vertex normals.
                vn_Indice := vn_Indice + 1
              else
              if dum[2]= 't' then //vertex texture coord
                vt_Indice := vt_Indice + 1;
            end;
            'g' : ;//LMesh := ReadGeometry(LLine);
            'u' :
              begin
                //if LMesh = nil then
                //  LMesh := ReadGeometry('g default');
                //LSubMesh := LMesh.ReadUseMaterial(LLine, FVertexSource);
              end;
            's' : ;//ReadSmoothGroup(LLine);
            'f' :
              begin
                o.DelimitedText := dum;
                f_Indice := f_Indice + 1;
                if o.Count=5 then //Square (2 triangles)
                  f_Indice := f_Indice + 1;
                //if (LMesh = nil) then
                //   LMesh := ReadGeometry('g default');

                //if (LSubMesh = nil) then
                //    LSubMesh := LMesh.ReadUseMaterial('usemtl Default', FVertexSource);
                //  ReadFaces(LSubMesh, LLine.Trim);
              end;
          end;
        end;
      end;

      //Pre-Allocate source data
      SetLength(RawVertices,v_indice);
      SetLength(RawNormals,vn_Indice);
      SetLength(RawUVMap,vt_Indice);

      aMesh.Data.VertexBuffer.Length := f_Indice*3;
      aMesh.Data.IndexBuffer.Length := aMesh.Data.VertexBuffer.Length;

      v_Indice :=0;
      vn_Indice :=0;
      vt_Indice :=0;
      f_Indice :=0;


      //Gathering source data by rescanning file.

      reset(LFile);
      while not EOF(LFile) do
      begin
        Readln(LFile, LLine);
        if (LLine <> '') and (LLine.Chars[0] <> '#') then
        begin
          o.DelimitedText := trim(LLIne);
          if o.Count=0 then
            Continue;
          if o[0] = 'v' then //vertex.
          begin
            rtAssert(o.Count>=4,'Obj file error : Parsing "v" error Line '+IntToStr(i));
            vf1 := StrToFloatDef(o[1],0);
            vf2 := StrToFloatDef(o[2],0);
            vf3 := StrToFloatDef(o[3],0);

            RawVertices[v_Indice] := Point3D(vf1,vf2,vf3);
            //aMesh.Data.VertexBuffer.Vertices[v_Indice] := Point3D(vf1,vf2,vf3);
            //aMesh.Data.VertexBuffer.Normals[v_Indice] := Point3D(vf1,vf2,vf3); ?
            v_Indice := v_Indice + 1;
          end
          else
          if o[0] = 'vn' then //vertex normals
          begin
            rtAssert(o.Count=4,'Obj file error : Parsing "vn" error Line '+IntToStr(i));
            vf1 := StrToFloatDef(o[1],0);
            vf2 := StrToFloatDef(o[2],0);
            vf3 := StrToFloatDef(o[3],0);

            RawNormals[vn_Indice] := Point3D(vf1,vf2,vf3);
            vn_Indice := vn_Indice + 1;
          end
          else
          if o[0] = 'vt' then //vertex texture coord
          begin
            //rtAssert(o.Count=4,'Obj file error : Parsing "vt" error Line '+IntToStr(i));
            if o.Count>=3 then
            begin
              vf1 := StrToFloatDef(o[1],0);
              vf2 := StrToFloatDef(o[2],0);
              //vf3 := StrToFloatDef(o[3],0);
              //aMesh.Data.VertexBuffer.TexCoord0[vt_Indice] := Pointf(Vector(vf1,vf2));
              RawUVMap[vt_Indice] := Pointf(vf1, 1.0 - vf2);
              vt_Indice := vt_Indice + 1;
            end
          end
          else
          if o[0] = 'f' then //face
          begin
            m.Delimiter := ' ';
            if pos('/',LLine)>0 then
              m.Delimiter := '/';

            if o.Count>=4 then  //tri
            begin
              //supported :
              //f v1/vt1/vn1 v2/vt2/vn2 v3/vt3/vn3
              //f v1/vt1 v2/vt2 v3/vt3 ... vertex indice, texture indice.
              //f v1//vn1 v2//vn2 v3//vn3 ... (Without texture indice)
              //f v1 v2 v3 ....


              {
              0 1 2 3
              The division into two triangles would be one with the first 3 indices, and one with the first, third, and fourth. In this example:

              0 1 2
              0 2 3
              Let's try some ASCII art to illustrate this:

              3-------2
              |      /|
              |    /  |
              |  /    |
              |/      |
              0-------1
              Here you see 0 1 2 3 as the quad, 0 1 2 as the first triangle (bottom-right), and 0 2 3 as the second triangle (top left).

              More generally, for faces with n vertices, you generate triangles:

              0 (i) (i + 1)  [for i in 1..(n - 2)]
              If you don't insist on separate triangles, you can also use GL_TRIANGLE_FAN primitives, which are still in core OpenGL. That way, you can draw any convex polygon with a triangle fan, using the original sequence of indices. So a triangle fan with vertex sequence 0 1 2 3 describes the quad in this case, and it very easily generalizes to faces with more than 4 vertices.

              Edit: Since you still appear to have problems, let's see how this applies to the example in your post. I'll list the original index sequence of the quad for each face, and the index sequence for the two triangles after splitting the quad.

              f 1 2 3 4 --> (1 2 3) (1 3 4)
              f 8 7 6 5 --> (8 7 6) (8 6 5)
              f 4 3 7 8 --> (4 3 7) (4 7 8)
              f 5 1 4 8 --> (5 1 4) (5 4 8)
              f 5 6 2 1 --> (5 6 2) (5 2 1)
              f 2 6 7 3 --> (2 6 7) (2 7 3)
              }


              m.DelimitedText := o[1];
              f_vi := IndiceFix(m[0]);
              AssignFaceData;

              f_IndexIndice := f_IndexIndice + 1;

              m.DelimitedText := o[2];
              f_vi := IndiceFix(m[0]);
              AssignFaceData;

              f_IndexIndice := f_IndexIndice + 1;

              m.DelimitedText := o[3];
              f_vi := IndiceFix(m[0]);
              AssignFaceData;

              f_IndexIndice := f_IndexIndice + 1;

              if o.Count = 5  then    //Square. 1 more triangles for a square face...
              begin

                m.DelimitedText := o[1];
                f_vi := IndiceFix(m[0]);
                AssignFaceData;

                f_IndexIndice := f_IndexIndice + 1;

                m.DelimitedText := o[3];
                f_vi := IndiceFix(m[0]);
                AssignFaceData;

                f_IndexIndice := f_IndexIndice + 1;

                m.DelimitedText := o[4];
                f_vi := IndiceFix(m[0]);
                AssignFaceData;

                f_IndexIndice := f_IndexIndice + 1;
              end;

              { TODO : polygone general case...  (5/6/n...) ? }

            end;
          end;
        end;
      end;


    finally
      CloseFile(LFile);
    end;

   if RecalNormals then
     aMesh.Data.CalcFaceNormals;

  finally
    FreeAndNil(o);
    FreeAndNil(m);
  end;

end;





{ TRawVertices }

function TRawVertices.ToString: String;
begin
  result := FloatToStr(vertice.X)+FloatToStr(vertice.Y)+FloatToStr(vertice.Z)+
            FloatToStr(normal.X)+FloatToStr(normal.Y)+FloatToStr(normal.Z)+
            FloatToStr(texture.X)+FloatToStr(texture.Y);
end;

end.
