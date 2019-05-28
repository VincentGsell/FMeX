{ -----------------------------------------------------------------------------
    This program is free software: Under statement of join file README - LGPL.txt
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-----------------------------------------------------------------------------
 Unit Name : FMeX.Types2D.GeometryCommon
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : Basic and independant 3D graphic shape generator.
 Date:     : ?
 History   :
 20190208 - Put this unit in GS collection. Freeing from Types dependancy.
-----------------------------------------------------------------------------}
unit FMeX.Types3D.GeometryCommon;

Interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math.Vectors,
  FMX.Types3D,
  FMeX.Types2D.GeometryCommon;

Type

TFMeXCustom3DGeometryGenerator = Class
Protected
  Procedure Process(aData : TMeshData); Virtual; Abstract;
End;

TFMeXBasedAngleShapeGenerator = Class(TFMeXCustom3DGeometryGenerator)
public
  Procedure Process( aData : TMeshData;
                     const Subdiv : integer = 3;
                     const Radius : Integer = 1); Reintroduce;
End;

{
TFMeXCubeGenerator = Class(TFMeXBasedAngleShapeGenerator)
Protected
//  Procedure Process(aData : TMeshData); Override;
End;

TFMeXSphereGenerator = Class(TFMeXCustom3DGeometryGenerator)
Protected
//  Procedure Process(aData : TMeshData); Override;
End;
}



implementation



{ TFMeXBasedAngleShapeGenerator }

procedure TFMeXBasedAngleShapeGenerator.Process(aData: TMeshData; const Subdiv : integer; const Radius : Integer);
var
  Geometry2D : TFMeXGeometry2DData;
  Tools : TFMeXDiskGenerator;
  i : integer;
  lface : integer;
begin
  Geometry2D := TFMeXGeometry2DData.Create;
  Tools := TFMeXDiskGenerator.Create;

  Tools.Subdivision := SubDiv;
  Tools.Radius := Radius;
  Tools.Generate(Geometry2D);

  //Transfert into mesh data.
  aData.Clear;
  aData.VertexBuffer.Length := Length(Geometry2D.Mesh)*3;
  aData.IndexBuffer.Length := Length(Geometry2D.Mesh)*3;

  lface := 0;
  for I := Low(Geometry2D.Mesh) to High(Geometry2D.Mesh) do
  begin
    aData.VertexBuffer.Vertices[lface] := Point3d(Geometry2D.Mesh[i].P1.X, Geometry2D.Mesh[i].P1.Y, 0);
    inc(lface);
    aData.VertexBuffer.Vertices[lface] := Point3D(Geometry2D.Mesh[i].P2.X, Geometry2D.Mesh[i].P2.Y, 0);
    inc(lface);
    aData.VertexBuffer.Vertices[lface] := Point3D(Geometry2D.Mesh[i].P3.X, Geometry2D.Mesh[i].P3.Y, 0);
    inc(lface);
  end;

  for I := 0 to aData.IndexBuffer.Length-1 do
  begin
    aData.IndexBuffer.Indices[i] := i;
  end;

  aData.CalcFaceNormals;

end;

end.
