unit FMeX.FileFormat3D.PLY;

interface

uses Generics.Collections,
     System.Math.Vectors,
     FMX.Types3D,
     FMeX.Types3D,
     SysUtils,
     Classes;

procedure FMeXLoadRawPLY(aFileName : String; var aMesh : TeCustomMesh; const RecalNormals : Boolean = true);

implementation

procedure FMeXLoadRawPLY(aFileName : String; var aMesh : TeCustomMesh; const RecalNormals : Boolean = true);
var
  ii,i, nbVertices, nbFaces: Integer;
  sl : TStringList;
  o : TStringList;
  vf1,vf2,vf3 : single;

  st : TMemoryStream;
begin
  Assert(FileExists(aFilename));
  Assert(Assigned(aMesh));
  aMesh.Data.Clear;
  FormatSettings.DecimalSeparator := '.';
  sl := TStringList.Create;
  o :=  TStringList.Create;
  try
    sl.LoadFromFile(aFileName, TEncoding.ASCII);
    o.Delimiter := ' ';
    if sl[0] <> 'ply' then
      raise Exception.Create('Not a valid ply file !');
    nbVertices := 0;
    nbFaces := 0;
    i := 0;
    while i < sl.Count do
    begin
      if sl[i] = 'end_header' then
        Break;
      if Copy(sl[i], 1, 14) = 'element vertex' then
        nbVertices := StrToIntDef(Copy(sl[i], 16, MaxInt), 0);
      if Copy(sl[i], 1, 12) = 'element face' then
        nbFaces := StrToIntDef(Copy(sl[i], 14, MaxInt), 0);
      Inc(i);
    end;
    Inc(i);
    // vertices
    aMesh.Data.VertexBuffer.Length := nbVertices;
    ii := 0;
    while (i < sl.Count) and (nbVertices > 0) do
    begin
      o.DelimitedText := sl[i];
      vf1 := StrToFloatDef(o[0],0);
      vf2 := StrToFloatDef(o[1],0);
      vf3 := StrToFloatDef(o[2],0);
      aMesh.Data.VertexBuffer.Vertices[ii] := Point3D(vf1,vf2,vf3);
      Dec(nbVertices);
      Inc(i);
      Inc(ii);
    end;
    // faces
    aMesh.Data.IndexBuffer.Length := nbFaces*3;
    ii := 0;
    while (i < sl.Count) and (nbFaces > 0) do
    begin
      o.DelimitedText :=sl[i];
      aMesh.Data.IndexBuffer[ii] := StrToIntDef(o[1],0); Inc(ii);
      aMesh.Data.IndexBuffer[ii] := StrToIntDef(o[2],0); Inc(ii);
      aMesh.Data.IndexBuffer[ii] := StrToIntDef(o[3],0); Inc(ii);
      Dec(nbFaces);
      Inc(i);
    end;

    if RecalNormals then
      aMesh.Data.CalcFaceNormals;
  finally
    sl.Free;
    o.free;
  end;
end;


end.
