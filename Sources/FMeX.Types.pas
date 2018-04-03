// ObjectUp Library (c) 2010.
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
 Unit Name : FMeX.Types
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Url       :
 Purpose   :
 Date:     :
 Purpose   :
 History   :
 20091120 - Clearing code v1.0
-----------------------------------------------------------------------------}
unit FMeX.Types;

interface

{$IFDEF VER240}
Uses System.Classes, System.UITypes, FMX.Types;
{$ENDIF}
{$IFDEF VER250}
Uses System.Classes, System.UITypes, FMX.Graphics;
{$ENDIF}
{$IFDEF VER260}
Uses System.Classes, System.UITypes, FMX.Graphics;
{$ENDIF}
{$IF (Defined(VER280)) or (Defined(VER310)) or (Defined(VER320))}
Uses System.Classes, System.UITypes;
{$ENDIF}

const GLB_Math_PrecisionTolerance = 1.0E-12;
      GLB_PrecisionTolerance: Double = 1.0E-6;
      GLB_Pi = 3.141592654;
      GLB_TwoPi: Double   = 6.283185307179586476925286766559;
      GLB_PiOn2: Double   = 1.5707963267948966192313216916398;
      GLB_GripTolerance = 5.0;
      GLB_VeryLittle : Double = -999999999999999;
      GLB_VeryLarge : Double = 999999999999999;
      GLB_DegreeCst : Double = 180 / Pi;
      GLB_RadianCst : Double = Pi / 180;

{$M+}
Type
//Double = Extended;
//TofBoolean = Boolean;
//TofCardinal = Cardinal;
//TofInteger = Integer;
//TofString = String;
//TofShiftState = TShiftState;

TofColor = TAlphaColor;
TofBrushStyle = (ofbsClear, ofbsSolid); //Use for polygon : Clear --> only edge.
//TofFontStyle = TFontStyle;
//TofFontStyles = Set of TFontStyle;
//TofBitmap = TBitmap;

TOfPoint = Record
  X,Y,Z : Double;
end;

TOfPointVertex = Record
  X,Y,Z : Double;
  U,V : Double;
end;

TOfLine = Record
  A, B : TofPoint;
end;

TofTriangle = Record
  A,B,C : TofPoint;
end;

TofRect = Record
//  Top,left,Deep, right,Bottom,Front : Double;
  Case Integer of
      0 : (Top,left,Deep, right,Bottom,Front : Double);
      1: (TopLeft, BottomRight: TofPoint);
end;

TofEllipse = Record
  A,B : tofPoint;
  Ray0 : Double;
  Ray90 : Double;
end;

TofPointList = Array of TofPoint;
TofTriangleList = Array of TofTriangle;
TofEllipseList = Array of TofEllipse;

TofPolygon = TofPointList;
TofPolygonList = Array of TofPolygon;

TofPolyTriangle = Array[0..2] of TOfPoint;
TofPolyRectangle = Array[0..3] of TOfPoint;

TofCubicBezier = array [0..3] of TofPoint;


TofVectorArray = array [0..2] of Double;
TOfVector = Record
case Integer of
  0: (V :TofVectorArray);
  1: (X,Y,Z : Double);
end;

TofMatrixArray = array [0..2] of tofVector;
TofMatrix = packed record
case Integer of
  0: (M : TofMatrixArray);
  1: (
      m11, m12, m13: Double;
      m21, m22, m23: Double;
      m31, m32, m33: Double;
     );
end;

//used for internal. Same as TofVectorObject, but in record style. Use this last preferably.
TofPointVector = Record
  Origin : TofPoint;
  Vector : TofVector;
end;

TofVectorList = Array of TofVector;

TofBase = Class end;
TofBaseObjectList = Class(TList) end;

TofVectorObject = class(tofBase)
Public
  Origin : TofPoint;
  Direction : TofVector;
  Constructor Create(X,Y,Norm : Double);
end;

TofVectorObjectList = class(TofBaseObjectList)
  private
    function getVector(Index: Cardinal): TofVectorObject;
    procedure setVector(Index: Cardinal; const Value: TofVectorObject);
Public
  Property Vectors[Index : Cardinal] : TofVectorObject read getVector write setVector; Default;
end;


//Poly class
TofCodedPoint = Class(Tobject)
Public
  Point : TOfPoint;
  Code : Integer;
  TexturePoint : TOfPoint;
  Color : TofColor;
  Attached : TObject;
  Tag : Integer;

  Constructor Create;
end;

TofCodedPolygon = Class(TList)
  private


    FAngle: Double;
    function GetPoint(index: Cardinal): TofCodedPoint;
    procedure SetPoint(index: Cardinal; const Value: TofCodedPoint);
Public

  Constructor Create; Virtual;
  Destructor Destroy; OVerride;

  Procedure ClearPoints;
  Procedure ResetPoints;

  Procedure GetMinAndMax(Var XMin, YMin, XMax, YMax, TotalAxisX, TotalAxisY : Double);

  Procedure AddPoint(x,y,z : Double; Code : Integer; tag : Integer = 0; obj : TObject= nil);
  Property Points[index : Cardinal] : TofCodedPoint read GetPoint write SetPoint; default;

  Property AngleInDegree : Double read FAngle Write FAngle;
end;

TofCodedPolygonList = class(TList)
  private
    function GetPoly(Index: Cardinal): TofCodedPolygon;
    procedure SetPoly(Index: Cardinal; const Value: TofCodedPolygon);
Public
  Procedure ClearPoly;
  Function AddPoly : TofCodedPolygon;
  property Poly[Index : Cardinal] : TofCodedPolygon read GetPoly Write SetPoly; default;
end;

TListOfList = class(TList)
  private
    FColCount: Cardinal;
    FRowCount: Cardinal;
    function GetList(Index: Cardinal): TList;
    procedure SetList(Index: Cardinal; const Value: TList);
Public
  Constructor Create(Xsize, YSize : Cardinal); Reintroduce;
  property Columns[Index : Cardinal] : TList read GetList Write SetList; Default;
  property ColCount : Cardinal read FColCount;
  property RowCount : Cardinal Read FRowCount;
end;

TofCodedPolygonArray  = Class(TListOfList)
Public
  Function GetPolygon(x,y : Cardinal) : TofCodedPolygon;
  Procedure PutPolygon(x,y : Cardinal; Val : TofCodedPolygon);
end;


//Main constructor functions.
function ofLine(var P1, P2: TofPoint): TofLine; Overload;
function ofLine(X, Y, Z, X1, Y1, Z1: Double): TofLine; Overload;
function ofPoint(X, Y, Z: Double): TofPoint; Overload;
function ofPoint(X,Y : Double) : TofPoint; Overload;
Function ofRect(x,y,xx,yy : Double) : TofRect;
function OfUnionRect(const ARect1, ARect2: TofRect): TofRect;
function ofRectWidth(const R: TofRect): Double;
function ofRectHeight(const R: TofRect): Double;
procedure ofRectOffset(var R: TofRect; const Dx, Dy: Double);
function ofRectCenter(var R: TofRect; Bounds: TofRect): TofRect;
function ofRectFit(var R: TofRect; BoundsRect: TofRect): Double;
function ofIsRectEmpty(Rect: TofRect): boolean;
Function ofEllipse(var P1, P2: TofPoint; R1, R2: Double): TofEllipse;Overload;
function ofEllipse(X, Y, Z, X1, Y1, Z1, R1,  R2: Double): TofEllipse;Overload;
procedure ofReset(var Point: TofPoint);Overload;
procedure ofReset(var Point: TofVector);Overload;
procedure ofReset(var Polygon: TofPolygon);Overload;
function ofTriangle(var P1, P2,  P3: TofPoint): TofTriangle;Overload;
function ofTriangle(X, Y, Z, X1, Y1, Z1, X2, Y2,  Z2: Double): TofTriangle;Overload;
function ofVector(X, Y, Z: Double): TofVector;Overload;
function ofVector(Norm, Angle: Double): TofVector;Overload;
Procedure ofVector(var Vector: TofVector; X, Y, Z: Double);OVerload;
function ofVector(var Point: TofPoint): TofVector;Overload;
function ofPointsAreEquals(p1, p2: TofPoint): boolean;Overload;
function ofPointsAreEquals(Tolerance: double; p1, p2: TofPoint) : Boolean;Overload;
Function ofRectAreEquals(p1, p2: Tofrect): boolean;Overload;
Function ofRectAreEquals(Tolerance: double; p1, p2: Tofrect): boolean;Overload;
function ofPtInRect(Px,Py: Double;var Rect : TofRect):Boolean; OVerload;
function ofPtInPolygon(Px,Py: Double; Polygon:TofCodedPolygon):Boolean; OVerload;
function ofPtInPolygon(Px,Py: Double; Polygon:TofPointList):Boolean; OVerload;

Procedure swapmin(var a,b : Integer); Overload;
Procedure swapmin(var a,b : Double); Overload;
function ofMax(A1, A2: Double): Double;
function ofMin(A1, A2: Double): Double;
function ofMinMax(x, mi, ma: Double): Double;

function ofRadToDeg(const Degrees: Double): Double;
function ofDegToRad(const Degrees: Double): Double;
procedure ofSinCos(const Theta: single; var Sin, Cos: single);


//function ofFloatToStr(Value: Double): string;
//function ofStrToFloat(Value: String): Double;

function ofVectorTransform(const V: TofVector; const M: TofMatrix): TofVector;
function ofCreateRotationMatrix(angle: Double): TofMatrix;
function ofMatrixMultiply(const M1, M2: TofMatrix): TofMatrix;



Const
  CST_ClosePoly: TofPoint = (X: $FFFF; Y: $FFFF);

  CST_Kappa = 0.5522847498;
  CST_KappaInv = 1 - CST_Kappa;

  CST_IdentityMatrix: TofMatrix = (m11:1.0;m12:0.0;m13:0.0;
                                   m21:0.0;m22:1.0;m23:0.0;
                                   m31:0.0;m32:0.0;m33:1.0);
  CST_ZeroMatrix: TofMatrix     = (m11:0.0;m12:0.0;m13:0.0;
                                   m21:0.0;m22:0.0;m23:0.0;
                                   m31:0.0;m32:0.0;m33:0.0);

  iX = 0;
  iY = 1;
  iZ = 2;
  iW = 3;


implementation

Uses sysUtils, Math;

Procedure SwapMin(var a,b : Integer);
var t : integer;
begin
  if b<a then
  begin
    t:=b;
    b:=a;
    a:=t;
  end;
end;

Procedure SwapMin(var a,b : Double);
var t : Double;
begin
  if b<a then
  begin
    t:=b;
    b:=a;
    a:=t;
  end;
end;

function ofMax(A1, A2: Double): Double;
begin
  if A1 > A2 then
    Result := A1
  else
    Result := A2;
end;

function ofMin(A1, A2: Double): Double;
begin
  if A1 < A2 then
    Result := A1
  else
    Result := A2;
end;

function ofMinMax(x, mi, ma: Double): Double;
begin
  if (x < mi) then Result := mi
    else if (x > ma) then Result := ma else Result := x;
end;

function ofRadToDeg(const Degrees: Double): Double;
begin
  Result := Degrees * GLB_DegreeCst;
end;

function ofDegToRad(const Degrees: Double): Double;
begin
   Result := Degrees * GLB_RadianCst;
end;

procedure ofSinCos(const Theta: single; var Sin, Cos: single);
var
  s, c : Extended;
begin
  Math.SinCos(Theta, s, c);
  Sin:=s;
  Cos:=c;
end;

{function ofFloatToStr(Value: Double): string;
var
  S: char;
begin
  S :=  DecimalSeparator;
  try
    DecimalSeparator := '.';
    if Frac(Value) <> 0 then
      Result := Format('%.3f', [Value])
    else
      Result := IntToStr(Trunc(Value))
  finally
    DecimalSeparator := S;
  end;
end;

function ofStrToFloat(Value: String): Double;
var
  S: char;
begin
  S := DecimalSeparator;
  try
    DecimalSeparator := '.';
    Result := StrToFloat(Value);
  finally
    DecimalSeparator := S;
  end;
end;
}

function ofVectorTransform(const V: TofVector; const M: TofMatrix): TofVector;
begin
  Result.V[iX] := V.V[iX] * M.M[iX].V[iX] + V.V[iY] * M.M[iY].V[iX] + V.V[iZ] * M.M[iZ].V[iX];
  Result.V[iY] := V.V[iX] * M.M[iX].V[iY] + V.V[iY] * M.M[iY].V[iY] + V.V[iZ] * M.M[iZ].V[iY];
  Result.V[2] := 1.0;
end;

function ofCreateRotationMatrix(angle: Double): TofMatrix;
var
  cosine, sine: single;
begin
  ofSinCos(angle, sine, cosine);

  Result.m11 := cosine;
  Result.m12 := sine;
  Result.m13 := 0;
  Result.m21 := -sine;
  Result.m22 := cosine;
  Result.m23 := 0;

  Result.m31 := 0;
  Result.m32 := 0;
  Result.m33 := 1;
end;

function ofMatrixMultiply(const M1, M2: TofMatrix): TofMatrix;
begin
  Result.m11 := M1.m11 * M2.m11 + M1.m12 * M2.m21 + M1.m13 * M2.m31;
  Result.m12 := M1.m11 * M2.m12 + M1.m12 * M2.m22 + M1.m13 * M2.m32;
  Result.m13 := M1.m11 * M2.m13 + M1.m12 * M2.m23 + M1.m13 * M2.m33;
  Result.m21 := M1.m21 * M2.m11 + M1.m22 * M2.m21 + M1.m23 * M2.m31;
  Result.m22 := M1.m21 * M2.m12 + M1.m22 * M2.m22 + M1.m23 * M2.m32;
  Result.m23 := M1.m21 * M2.m13 + M1.m22 * M2.m23 + M1.m23 * M2.m33;
  Result.m31 := M1.m31 * M2.m11 + M1.m32 * M2.m21 + M1.m33 * M2.m31;
  Result.m32 := M1.m31 * M2.m12 + M1.m32 * M2.m22 + M1.m33 * M2.m32;
  Result.m33 := M1.m31 * M2.m13 + M1.m32 * M2.m23 + M1.m33 * M2.m33;
end;


function ofEllipse(var P1, P2: TofPoint; R1,
  R2: Double): TofEllipse;
begin
  Result.A:=P1;
  Result.B:=P2;
  Result.Ray0:=R1;
  Result.Ray90:=R2;
end;

function ofLine(var P1, P2: TofPoint): TofLine;
begin
  Result.A:=P1;
  Result.B:=P2;
end;

function ofLine(X, Y, Z, X1, Y1,
  Z1: Double): TofLine;
begin
  Result.A.X:=X;
  Result.A.Y:=Y;
  Result.A.Z:=Z;
  Result.B.X:=X1;
  Result.B.Y:=Y1;
  Result.B.Z:=Z1;
end;

function ofPoint(X, Y, Z: Double): TofPoint;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.Z:=Z;
end;

function ofPoint(X,Y : Double) : TofPoint; Overload;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.Z:=0;
end;

Function ofRect(x,y,xx,yy : Double) : TofRect;
begin
  result.Top:=y;
  result.Left:=x;
  result.right:=x+xx;
  result.Bottom:=y+yy;
end;

function OfUnionRect(const ARect1, ARect2: TofRect): TofRect;
begin
  Result.Left := ARect1.Left;
  if ARect2.Left < Result.Left then
    Result.Left := ARect2.Left;

  Result.Top := ARect1.Top;
  if ARect2.Top < Result.Top then
    Result.Top := ARect2.Top;

  Result.Right := ARect1.Right;
  if ARect2.Right > Result.Right then
    Result.Right := ARect2.Right;

  Result.Bottom := ARect1.Bottom;
  if ARect2.Bottom > Result.Bottom then
    Result.Bottom := ARect2.Bottom;
end;

function ofRectWidth(const R: TofRect): Double;
begin
  Result := R.Right - R.Left;
end;

function ofRectHeight(const R: TofRect): Double;
begin
  Result := R.Bottom - R.Top;
end;


procedure ofRectOffset(var R: TofRect; const Dx, Dy: Double);
begin
  R.Left := R.Left + Dx;
  R.Right := R.Right + Dx;
  R.Top := R.Top + Dy;
  R.Bottom := R.Bottom + Dy;
end;

function ofRectCenter(var R: TofRect; Bounds: TofRect): TofRect;
begin
  ofRectOffset(R, -R.Left, -R.Top);
  ofRectOffset(R, round((ofRectWidth(Bounds) - ofRectWidth(R)) / 2), round((ofRectHeight(Bounds) - ofRectHeight(R)) / 2));
  ofRectOffset(R, Bounds.Left, Bounds.Top);

  Result := R;
end;


function ofRectFit(var R: TofRect; BoundsRect: TofRect): Double;
var
  ratio: single;
begin
  Result := 1;
  if ofRectWidth(BoundsRect) * ofRectHeight(BoundsRect) = 0 then Exit;

  if (ofRectWidth(R) / ofRectWidth(BoundsRect)) > (ofRectHeight(R) / ofRectHeight(BoundsRect)) then
    ratio := ofRectWidth(R) / ofRectWidth(BoundsRect)
  else
    ratio := ofRectHeight(R) / ofRectHeight(BoundsRect);

  if ratio < 1 then
  begin
    R := ofRect(0, 0, ofRectWidth(R), ofRectHeight(R));
  end
  else
  begin
    R := ofRect(0, 0, round(ofRectWidth(R) / ratio), round(ofRectHeight(R) / ratio));
  end;

  Result := ratio;
  ofRectCenter(R, BoundsRect);
end;

function ofIsRectEmpty(Rect: TofRect): boolean;
begin
  Result := (ofRectWidth(Rect) <= 0) or (ofRectHeight(Rect) <= 0);
end;

procedure ofReset(var Point: TofPoint);
begin
  Point.X:=0;
  Point.Y:=0;
  Point.Z:=0;
end;

procedure ofReset(var Point: TofVector);
begin
  Point.X:=0;
  Point.Y:=0;
  Point.Z:=0;
end;


procedure ofReset(var Polygon: TofPolygon);
var i : Integer;
begin
    For i:=0 to High(Polygon)-1 do ofReset(Polygon[i]);
end;

function ofTriangle(var P1, P2,
  P3: TofPoint): TofTriangle;
begin
  Result.A:=P1;
  Result.B:=P2;
  Result.C:=P3;
end;

function ofTriangle(X, Y, Z, X1, Y1, Z1, X2, Y2,
  Z2: Double): TofTriangle;
begin
  Result.A.X:=X;
  Result.A.Y:=Y;
  Result.A.Z:=Z;
  Result.B.X:=X1;
  Result.B.Y:=Y1;
  Result.B.Z:=Z1;
  Result.C.X:=X2;
  Result.C.Y:=Y2;
  Result.C.Z:=Z2;
end;

function ofEllipse(X, Y, Z, X1, Y1, Z1, R1,
  R2: Double): TofEllipse;
begin
  Result.A.X:=X;
  Result.A.Y:=Y;
  Result.A.Z:=Z;
  Result.B.X:=X1;
  Result.B.Y:=Y1;
  Result.B.Z:=Z1;
  Result.Ray0:=R1;
  Result.Ray90:=R2;
end;

function ofVector(X, Y, Z: Double): TofVector;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.Z:=Z;
end;

function ofVector(Norm, Angle: Double): TofVector;
begin
  Result.x := Norm * cos(Angle);
  Result.y := Norm * sin(Angle);
end;

function ofVector(var Point: TofPoint): TofVector;
begin
  Result:=TofVector(Point);
end;

function ofPointsAreEquals(p1, p2: TofPoint): boolean;
begin
  Result := (p1.X = p2.X) and (p1.Y = p2.Y) And (p1.Z=p2.Z);
end;

function ofPointsAreEquals(Tolerance: double; p1, p2: TofPoint): boolean;
begin
  Result := ((p1.X >= p2.X-Tolerance) And (p1.x <= p2.X+Tolerance)) And
            ((p1.Y >= p2.Y-Tolerance) and (p1.Y <= p2.Y+Tolerance)) And
            ((p1.Z >= p2.Z-Tolerance) and (p1.Z <= p2.Z+Tolerance));
end;

Function ofRectAreEquals(p1, p2: Tofrect): boolean;Overload;
begin
  Result := (p1.Top= p2.Top) And (p1.left = p2.left) and (p1.right = p2.right) and (p1.Bottom = p2.Bottom);
end;

Function ofRectAreEquals(Tolerance : Double; p1, p2: Tofrect): boolean; OVerload;
begin
  Result := ((p1.Top >= p2.Top-Tolerance) And (p1.Top <= p2.Top+Tolerance)) And
            ((p1.left >= p2.left-Tolerance) and (p1.left <= p2.left+Tolerance)) And
            ((p1.Bottom >= p2.Bottom-Tolerance) and (p1.Bottom <= p2.Bottom+Tolerance)) And
            ((p1.right >= p2.right-Tolerance) and (p1.right <= p2.right+Tolerance));
end;

Procedure ofVector(var Vector: TofVector; X, Y, Z: Double);
begin
  Vector.X:=X;
  Vector.Y:=Y;
  Vector.Z:=Z;
end;

function ofPtInPolygon(Px,Py: Double; Polygon:TofCodedPolygon):Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := False;
  if Polygon.Count < 3 then
    Exit;
  j := Polygon.Count - 1;
  for i := 0 to Polygon.Count - 1 do
  begin
    if ((Polygon[i].Point.Y <= Py) and (Py < Polygon[j].Point.y)) or    // an upward crossing
       ((Polygon[j].Point.y <= Py) and (Py < Polygon[i].Point.y)) then  // a downward crossing
    begin
      (* compute the edge-ray intersect @ the x-coordinate *)
      if (Px - Polygon[i].Point.x < ((Polygon[j].Point.x - Polygon[i].Point.x) * (Py - Polygon[i].Point.y) / (Polygon[j].Point.y - Polygon[i].Point.y))) then
        Result := not Result;
    end;
    j := i;
  end;
end;

function ofPtInRect(Px,Py: Double;var Rect : TofRect):Boolean; OVerload;
begin
  Result := False;
  if (Px>Rect.Left) and (Px<Rect.Right) then
    Result :=(Py>Rect.Top) and (Py<Rect.Bottom);
end;


function ofPtInPolygon(Px,Py: Double; Polygon:TofPointList):Boolean;
var
  i : Integer;
  j : Integer;
  pc : Integer;
begin
  Result := False;
  pc:=high(Polygon);
  if pc < 3 then
    Exit;
  j := pc;
  for i := Low(Polygon) to pc do
  begin
    if ((Polygon[i].Y <= Py) and (Py < Polygon[j].y)) or    // an upward crossing
       ((Polygon[j].y <= Py) and (Py < Polygon[i].y)) then  // a downward crossing
    begin
      (* compute the edge-ray intersect @ the x-coordinate *)
      if (Px - Polygon[i].x < ((Polygon[j].x - Polygon[i].x) * (Py - Polygon[i].y) / (Polygon[j].y - Polygon[i].y))) then
        Result := not Result;
    end;
    j := i;
  end;
end;



{ TofVectorObject }

constructor TofVectorObject.Create(X,Y,Norm : Double);
begin
  Origin.X:=X;
  Origin.Y:=Y;
  Origin.Z:=0;
  Direction.X:=Norm;
  Direction.Y:=0;
  Direction.Z:=0;

end;


{ TofCodedPolygon }

procedure TofCodedPolygon.AddPoint(x, y, z: Double; Code: Integer; tag : Integer = 0; obj : TObject = nil);
var a : TofCodedPoint;
begin
{
  if count>1 then
    if (points[Count-1].Point.x=x) and (points[Count-1].Point.y=y) and (points[Count-1].Point.z=z) then
      Exit;
  try
    a:=TofCodedPoint.Create;
    Except
      on e : Exception do raise e;
  end;
}
  a:=TofCodedPoint.Create;
  Add(a);
  a.Point.x:=x;
  a.Point.y:=y;
  a.Point.z:=z;
  a.Code:=Code;
  a.Tag:=tag;
  a.Attached:=obj;
end;

procedure TofCodedPolygon.ClearPoints;
var i : integer;
begin
  for i := count-1 downto 0 do
    TofCodedPoint(Get(I)).Free;
  clear;
end;

constructor TofCodedPolygon.Create;
begin
  inherited;
  FAngle := 0;
end;

destructor TofCodedPolygon.Destroy;
begin
  ClearPoints;
  inherited;
end;

procedure TofCodedPolygon.GetMinAndMax(Var XMin, YMin, XMax, YMax, TotalAxisX, TotalAxisY : Double);
var i : integeR;
begin
  XMin := GLB_VeryLarge;
  YMin := GLB_VeryLarge;
  XMax := GLB_VeryLittle;
  YMax := GLB_VeryLittle;

  for i:=0 to Count-1 do
  begin
    if Points[i].Point.Y<YMin then
     YMin:=Points[i].Point.Y;
    if Points[i].Point.Y>YMax then
     YMax:=Points[i].Point.Y;

    if Points[i].Point.X<XMin then
     XMin:=Points[i].Point.X;
    if Points[i].Point.X>XMax then
     XMax:=Points[i].Point.X;
  end;

  TotalAxisY:= Abs(YMax-YMin);
  TotalAxisX:= Abs(XMax-XMin);
end;

function TofCodedPolygon.GetPoint(index: Cardinal): TofCodedPoint;
begin
  result:=TofCodedPoint(get(Index));
end;

procedure TofCodedPolygon.ResetPoints;
var i : integer;
begin
  for i := count-1 downto 0 do
  begin
    TofCodedPoint(Get(I)).Point.X :=0;
    TofCodedPoint(Get(I)).Point.Y :=0;
    TofCodedPoint(Get(I)).Point.Z :=0;
  end;
end;

procedure TofCodedPolygon.SetPoint(index: Cardinal;
  const Value: TofCodedPoint);
begin
  Put(Index,Value);
end;

{ TofCodedPolygonList }

function TofCodedPolygonList.AddPoly: TofCodedPolygon;
begin
  Result := TofCodedPolygon.Create;
  Add(result);
end;

procedure TofCodedPolygonList.ClearPoly;
var i : integer;
begin
  for i:=Count-1 downto 0 do
  begin
    Poly[i].Free;
  end;
  Clear;
end;

function TofCodedPolygonList.GetPoly(Index: Cardinal): TofCodedPolygon;
begin
  Result := TofCodedPolygon(get(Index));
end;

procedure TofCodedPolygonList.SetPoly(Index: Cardinal;
  const Value: TofCodedPolygon);
begin
  Put(index,Value);
end;

{ TListOfList }

constructor TListOfList.Create(Xsize, YSize: Cardinal);
var i,j : integer;
    c,d : Tlist;
begin
  Inherited Create;
  FColCount := Xsize;
  FRowCount := YSize;
  For I:=0 to XSize-1 do
  begin
    c := TList.Create;
    Add(c);
    For j:=0 to YSize-1 do
    begin
      d := TList.Create;
      c.Add(d);
    end;
  end;
end;

function TListOfList.GetList(Index: Cardinal): TList;
begin
  result := TList(Get(index));
end;

procedure TListOfList.SetList(Index: Cardinal; const Value: TList);
begin
  Put(Index,Value);
end;

{ TofcodedPolygonArray }

function TofcodedPolygonArray.GetPolygon(x, y: Cardinal): TofCodedPolygon;
begin
  Result := TofCodedPolygon(Columns[x][y]);
end;

procedure TofcodedPolygonArray.PutPolygon(x, y: Cardinal;
  Val: TofCodedPolygon);
begin
  Columns[x][y]:= Val;
end;

{ TofCodedPoint }

constructor TofCodedPoint.Create;
begin
  Inherited Create;
  Code:=-1;
  Point.x:=0;
  Point.y:=0;
  Point.z:=0;
  TexturePoint.x:=0;
  TexturePoint.y:=0;
  TexturePoint.z:=0;
  Attached := nil;
end;


{ TofVectorObjectList }

function TofVectorObjectList.getVector(Index: Cardinal): TofVectorObject;
begin
  Result := TofVectorObject(Get(index));
end;

procedure TofVectorObjectList.setVector(Index: Cardinal;
  const Value: TofVectorObject);
begin
  Put(index, value);
end;




end.


