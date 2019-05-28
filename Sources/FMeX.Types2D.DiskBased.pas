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
 Unit Name : FMeX.Types2D.DiskBased
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : FMX2D Control : Exemple of waht is possible with FMeX.Types2D.GeometryCommon.
 Date:     : 2010.. ?
 History   :
 20190426 - Introducing GS collection : Freeing from Types dependancy.
-----------------------------------------------------------------------------}
unit FMeX.Types2D.DiskBased;

interface

Uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  FMX.Types, FMX.Controls, FMX.Objects, System.Math.Vectors,
  FMeX.Types2D.GeometryCommon, FMX.Graphics;

Type

TofShapeCommon = Class(TControl)
Private
  InternalData : TFMeXGeometry2DData;
  InternalBorder : TPathData;
Protected
  Procedure Rebuild; Virtual; Abstract;
  Procedure RebuildBorder; Virtual;
Public
  procedure SetHeight(const Value: Single); Override;
  procedure SetWidth(const Value: Single); Override;
  procedure SetBounds(X, Y, AWidth, AHeight: Single); Override;

  constructor Create(AOwner: TComponent); override;
  Destructor Destroy; override;
  Procedure Paint; Override;

  Property Data : TFMeXGeometry2DData read InternalData;
End;

  TofDisk2D = Class(TofShapeCommon)
  Private
    FSubdivision: Integer;

    procedure SetSubdivision(const Value: Integer);
  Protected
    Procedure Rebuild; Override;
  Public
    constructor Create(AOwner: TComponent); override;
  Published
    Property Subdivision : Integer read FSubdivision Write SetSubdivision;
  End;

    TofDonut2D = Class(TofDisk2D)
    Private
    Protected
      Procedure Rebuild; Override;
    Public
    End;

implementation

{ TofDisk2D }

constructor TofDisk2D.Create(AOwner: TComponent);
begin
  Inherited;
  FSubdivision := 50;
  Rebuild;
  RebuildBorder;
end;


procedure TofDisk2D.Rebuild;
var MyGenerator : TFMeXDiskGenerator;
begin
  inherited;
  MyGenerator := TFMeXDiskGenerator.Create;
  try
    MyGenerator.Subdivision := FSubdivision;
    MyGenerator.Generate(InternalData);
  finally
    FreeAndNil(MyGenerator);
  end;
end;


procedure TofDisk2D.SetSubdivision(const Value: Integer);
begin
  FSubdivision := Value;
  Rebuild;
  RebuildBorder;
  Repaint;
end;


{ TofDonut2D }

procedure TofDonut2D.Rebuild;
var MyGenerator : TFMeXDiskDonutGenerator;
begin
  MyGenerator := TFMeXDiskDonutGenerator.Create;
  try
    MyGenerator.Subdivision := FSubdivision;
    MyGenerator.Generate(InternalData);
  finally
    FreeAndNil(MyGenerator);
  end;
end;


{ TofShapeCommon }

constructor TofShapeCommon.Create(AOwner: TComponent);
begin
  inherited;
  InternalData := TFMeXGeometry2DData.Create;
  InternalBorder := TPathData.Create;
end;

destructor TofShapeCommon.Destroy;
begin
  FreeAndNil(InternalData);
  FreeAndNil(InternalBorder);
  inherited;
end;

procedure TofShapeCommon.Paint;
begin
  //Optimization : InternalCircle is recompute in RebuildCircle.
  Canvas.Fill.Color := TAlphaColorRec.White;
  Canvas.FillPath(InternalBorder,AbsoluteOpacity);

  Canvas.Stroke.Color := TAlphaColorRec.Blue;
  Canvas.Stroke.Thickness := 3;
  Canvas.DrawPath(InternalBorder,AbsoluteOpacity);
end;

procedure TofShapeCommon.RebuildBorder;
var c : Tpointf;
    l : Single;
    i : Integer;
begin
  if Length(InternalData.Border) = 0 then
    Exit;

  InternalBorder.Clear;
  l := width/2;
  if Height < Width then
    l := height/2;
  c := PointF(Width/2,Height/2);

  i := Low(InternalData.Border);
  InternalBorder.MoveTo(Pointf(InternalData.Border[i].P1.X,InternalData.Border[i].P1.Y));

  for I := Low(InternalData.Border)+1 to High(InternalData.Border) do
  begin
    if InternalData.Border[i].Code = fecIgnore then
    begin
      InternalBorder.ClosePath;
      InternalBorder.MoveTo(Pointf(InternalData.Border[i].P1.X,InternalData.Border[i].P1.Y));
    end
    else
    begin
      InternalBorder.LineTo(Pointf(InternalData.Border[i].P1.X,InternalData.Border[i].P1.Y));
    end;
  end;
  InternalBorder.ClosePath;

  InternalBorder.Scale(l,l);
  InternalBorder.Translate(c.X,c.y);
end;

procedure TofShapeCommon.SetBounds(X, Y, AWidth, AHeight: Single);
begin
  inherited;
  RebuildBorder;
  Repaint;
end;

procedure TofShapeCommon.SetHeight(const Value: Single);
begin
  inherited;
  RebuildBorder;
  Repaint;
end;

procedure TofShapeCommon.SetWidth(const Value: Single);
begin
  inherited;
  RebuildBorder;
  Repaint;
end;

end.
