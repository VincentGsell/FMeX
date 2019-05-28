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
 Purpose   : Basic and independant 2D graphic shape generator.
 Date:     : ?
 History   :
 20180426 - Put this unit in GS collection. Freeing from Types dependancy.
-----------------------------------------------------------------------------}
unit FMeX.Types2D.GeometryCommon;

Interface

uses
  System.SysUtils, System.Types, System.Classes,
  GS.Direction,
  clipper;

Type

TFMeXTriangle = Record
  P1,P2,P3 : TPointf;
End;
TFMeXTriangleArray = Array of TFMeXTriangle;

TFMeXEdgeCode = (fecDraw, fecIgnore);
TFMeXEdge = Record
  P1 : TPointf;
  Code : TFMeXEdgeCode;
End;
TFMeXEdgeArray = Array of TFMeXEdge;


TFMeXGeometry2DData = Class
Private
  Procedure ClearArray;
Public
  Mesh : TFMeXTriangleArray;
  Border : TFMeXEdgeArray;

  Procedure Clear;
  Procedure SetCapacity(aCapa : Integer);

  Procedure Scale(xScale, yScale : Single);
  Procedure Translate(xAmount, yAmount : Single);

//  Function ToClipperPaths : TPaths;
//  Procedure FromClipperPaths(aClipperPaths : TPaths); //Use Delaunay for Mesh rebuild.
End;

TFMeXGeometryGenerator = Class
Public
End;

TFMeX2DGeometryGenerator = Class(TFMeXGeometryGenerator)
Private
Protected
  LocalGeometryTool : TDirectionalObject;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;
  Procedure Generate(var aData : TFMeXGeometry2DData); Virtual; Abstract;
End;

//--------------------------------------------------- Disk.
TFMeXDiskGenerator = Class(TFMeX2DGeometryGenerator)
Private
  FRadius: Single;
  FSubdivision: Integer;
Public
  Procedure Generate(var aData : TFMeXGeometry2DData); Override;
  Constructor Create; Override;

Published
  Property Radius : Single read FRadius WRite FRadius;
  Property Subdivision : Integer Read FSubdivision Write FSubdivision;
End;

//-------------------------------------------------- Donut (from Disk).
TFMeXDiskDonutGenerator = Class(TFMeXDiskGenerator)
Private
  FInnerRadius: Single;
  FSecondaryGeometryTool : TDirectionalObject;

  procedure SetInnerRadius(const Value: Single);
Public
  Procedure Generate(var aData : TFMeXGeometry2DData); Override;
  Constructor Create; Virtual;
  Destructor Destroy; Override;
Published
  Property InnerRadius : Single read FInnerRadius WRite SetInnerRadius;
End;

//------------------------------------------------- Sub Operation.
TFMeX2DSubOpp = Class(TFMeX2DGeometryGenerator)
private
  FSubject: TFMeXGeometry2DData;
  FSubOp: TFMeXGeometry2DData;
Public
  Procedure Generate(var aData : TFMeXGeometry2DData); Override;

  Property Subject : TFMeXGeometry2DData Read FSubject Write FSubject;
  Property SubOp : TFMeXGeometry2DData read FSubOp Write FSubOp;
End;

implementation


{ TFMeXDiskGenerator }

constructor TFMeXDiskGenerator.Create;
begin
  inherited;
  Subdivision := 30;
  Radius := 1;
end;

procedure TFMeXDiskGenerator.Generate(var aData : TFMeXGeometry2DData);
var i : integer;
    FStepAngle : Single;
begin
  aData.SetCapacity(FSubdivision);

  LocalGeometryTool.SetOrigin(0,0,0);
  LocalGeometryTool.Norm := FRadius;
  LocalGeometryTool.Angle := 0;

  FStepAngle := 360/FSubdivision;

  for i := 0 to FSubdivision-1 do
  begin
    aData.Mesh[i].P1 := Pointf(0,0);
//    aData.Mesh[i].P1_XY_Normal_Angle := 0;
    aData.Mesh[i].P2 := Pointf(LocalGeometryTool.GetPointedCoord.X,LocalGeometryTool.GetPointedCoord.Y);
//    aData.Mesh[i].P2_XY_Normal_Angle := LocalGeometryTool.AngleInDegree;
    LocalGeometryTool.TurnBy(FStepAngle);
    aData.Mesh[i].P3 := Pointf(LocalGeometryTool.GetPointedCoord.X,LocalGeometryTool.GetPointedCoord.Y);
//    aData.Mesh[i].P3_XY_Normal_Angle := LocalGeometryTool.AngleInDegree;

    aData.Border[i].P1 := aData.Mesh[i].P2;
  end;
end;

{ TFMeXDiskData }

procedure TFMeXGeometry2DData.Clear;
begin
  ClearArray;
end;


procedure TFMeXGeometry2DData.ClearArray;
var i : Integer;
begin
  for i := Low(Mesh) to High(Mesh) do
  begin
    Mesh[i].P1:= PointF(0,0);
    Mesh[i].P2:= PointF(0,0);
    Mesh[i].P3:= PointF(0,0);
  end;
  for i := Low(Border) to High(Border) do
  begin
    Border[i].P1:= PointF(0,0);
    Border[i].Code := fecDraw;
  end;
end;

procedure TFMeXGeometry2DData.Scale(xScale, yScale: Single);
var i : Integer;
begin
  for i := Low(Mesh) to High(Mesh) do
  begin
    Mesh[i].P1:= Pointf(Mesh[i].P1.X * xScale, Mesh[i].P1.Y * yScale);
    Mesh[i].P2:= Pointf(Mesh[i].P2.X * xScale, Mesh[i].P2.Y * yScale);
    Mesh[i].P3:= Pointf(Mesh[i].P3.X * xScale, Mesh[i].P3.Y * yScale);
  end;
  for i := Low(Border) to High(Border) do
  begin
    Border[i].P1:= Pointf(Border[i].P1.X * xScale, Border[i].P1.Y * yScale);
    //Border[i].Code := fecDraw;
  end;
end;

procedure TFMeXGeometry2DData.SetCapacity(aCapa: Integer);
begin
  SetLength(Mesh,aCapa);
  SetLength(Border,aCapa);
  ClearArray;
end;

procedure TFMeXGeometry2DData.Translate(xAmount, yAmount: Single);
var i : Integer;
begin
  for i := Low(Mesh) to High(Mesh) do
  begin
    Mesh[i].P1:= Pointf(Mesh[i].P1.X + xAmount, Mesh[i].P1.Y + yAmount);
    Mesh[i].P2:= Pointf(Mesh[i].P2.X + xAmount, Mesh[i].P2.Y + yAmount);
    Mesh[i].P3:= Pointf(Mesh[i].P3.X + xAmount, Mesh[i].P3.Y + yAmount);
  end;
  for i := Low(Border) to High(Border) do
  begin
    Border[i].P1:= Pointf(Border[i].P1.X + xAmount, Border[i].P1.Y + yAmount);
    //Border[i].Code := fecDraw;
  end;
end;

{ TFMeXDiskDonutGenerator }

constructor TFMeXDiskDonutGenerator.Create;
begin
  Inherited;
  FSecondaryGeometryTool := TDirectionalObject.Create(0,0,1);
  InnerRadius := 0.5;
end;

destructor TFMeXDiskDonutGenerator.Destroy;
begin
  FreeAndNil(FSecondaryGeometryTool);
  inherited;
end;

procedure TFMeXDiskDonutGenerator.Generate(var aData: TFMeXGeometry2DData);
var i,j : integer;
    FStepAngle : Single;
begin
  aData.SetCapacity(FSubdivision*2); //2 Circles. = twice triangle quantity, and twice border long.

  LocalGeometryTool.SetOrigin(0,0,0);
  LocalGeometryTool.Norm := FRadius;
  LocalGeometryTool.Angle := 0;

  FSecondaryGeometryTool.SetOrigin(0,0,0);
  FSecondaryGeometryTool.Norm := FInnerRadius;
  FSecondaryGeometryTool.Angle := 0;


  FStepAngle := 360/FSubdivision;

  //Outer circle.
  j := 0;
  for i := 0 to FSubdivision-1 do
  begin
    aData.Border[j].Code := fecDraw;
    aData.Border[j].P1 := Pointf(LocalGeometryTool.GetPointedCoord.X,LocalGeometryTool.GetPointedCoord.Y);
    LocalGeometryTool.TurnBy(FStepAngle);
    inc(j);
  end;

  //Jump !
  aData.Border[j].P1 := Pointf(FSecondaryGeometryTool.GetPointedCoord.X,FSecondaryGeometryTool.GetPointedCoord.Y);
  aData.Border[j].Code := fecIgnore;
  FSecondaryGeometryTool.TurnBy(FStepAngle);
  inc(j);

  //Inner circle.
  for i := 0 to FSubdivision-1 do
  begin
    aData.Border[j].Code := fecDraw;
    aData.Border[j].P1 := Pointf(FSecondaryGeometryTool.GetPointedCoord.X,FSecondaryGeometryTool.GetPointedCoord.Y);
    FSecondaryGeometryTool.TurnBy(FStepAngle);
    inc(j);
  end;
end;

procedure TFMeXDiskDonutGenerator.SetInnerRadius(const Value: Single);
begin
  FInnerRadius := Value;
  if FInnerRadius>(Radius - 0.001*Radius) then
    FInnerRadius := Radius - 0.001*Radius;
end;



{ TFMeX2DGeometryGenerator }

constructor TFMeX2DGeometryGenerator.Create;
begin
  Inherited;
  LocalGeometryTool := TDirectionalObject.Create(0,0,1);
end;

destructor TFMeX2DGeometryGenerator.Destroy;
begin
  FreeAndNil(LocalGeometryTool);
  inherited;
end;

{ TFMeX2DSubOpp }

procedure TFMeX2DSubOpp.Generate(var aData: TFMeXGeometry2DData);
var c : TClipper;
    a,b,r : TPaths;

    i : integer;
begin
  Assert(Assigned(Subject));
  Assert(Assigned(SubOp));

  //Todo : Paths with other dim (Jump sequence in border)
  SetLength(a, 1);
  SetLength(a[0], Length(Subject.Border));
  SetLength(b, 1);
  SetLength(b[0], Length(SubOp.Border));

  for I := Low(Subject.Border) to High(Subject.Border) do
  begin
    a[0][i].X := Round(Subject.Border[i].P1.X*1000);
    a[0][i].Y := Round(Subject.Border[i].P1.Y*1000);
  end;

  for I := Low(SubOp.Border) to High(SubOp.Border) do
  begin
    b[0][i].X := Round(SubOp.Border[i].P1.X*1000);
    b[0][i].Y := Round(SubOp.Border[i].P1.Y*1000);
  end;

  //Use Clipper lib to generate aData starting from Subject and SubOp.
  c := TClipper.Create;
  c.AddPaths(a,ptSubject,true);
  c.AddPaths(b,ptClip,true);
  c.Execute(ctDifference,r,pftNonZero,pftNonZero);

  aData.SetCapacity(LEngth(r[0]));
  for I := Low(r[0]) to High(r[0]) do
  begin
    aData.Border[i].P1.X := r[0][i].X/1000;
    aData.Border[i].P1.Y := r[0][i].Y/1000;
  end;

  c.Free;
end;

end.
