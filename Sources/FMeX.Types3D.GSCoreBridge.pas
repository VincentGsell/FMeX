unit FMeX.Types3D.GSCoreBridge;

interface

uses SysUtils,
     Classes,
     System.RTLConsts,
     System.Math.Vectors,
     FMX.Types3d,
     FMeX.Types3d,
     GS.Soft3D.Types,
     GS.Soft3d;

type

TMeshTransfert = Class
  class procedure TransfertGSCoreMesh3DToFMeX(Source : TMesh3d; Target : TeCustomMesh);
End;

implementation

{ TMeshTransfert }

class procedure TMeshTransfert.TransfertGSCoreMesh3DToFMeX(Source: TMesh3d;
  Target: TeCustomMesh);
var i : integer;
begin
  Target.Data.Clear;

  Target.Data.VertexBuffer.Length := length(Source.meshData.vertices);
  Target.Data.IndexBuffer.Length := length(Source.meshData.indices);

  for i := 0 to Target.Data.VertexBuffer.Length-1 do
  begin
    Target.Data.VertexBuffer.Vertices[i] := Point3D( Source.meshData.vertices[i].x,
                                                Source.meshData.vertices[i].y,
                                                Source.meshData.vertices[i].z);
  end;
  for i := 0 to Target.Data.IndexBuffer.Length-1 do
  begin
    Target.Data.IndexBuffer.Indices[i] := Source.meshData.indices[i];
  end;
end;

end.
