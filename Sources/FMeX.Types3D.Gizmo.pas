unit FMeX.Types3D.Gizmo;

///
///  Gizmo code form "hartmutdavid"
///  https://github.com/hartmutdavid/FMX3DViewer
///
///  This a rename unit of the original code.
///

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs,
  System.Math.Vectors,
  System.Math,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.MaterialSources,
  FMX.Menus,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layers3D,
  FMeX.Types3d;

type
  //https://github.com/hartmutdavid/FMX3DViewer
 TAchse3D = class;

 TCursor3D = class(TControl3D)
 private
   FMoveMode: Boolean;
   FOnTracking: TNotifyEvent;
   FPlaneNorm:  TPoint3D;
   fX: TAchse3D;
   fY: TAchse3D;
   fZ: TAchse3D;
   function GetSlave: TControl3D;
 protected
   procedure Render; override;
   procedure MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
   procedure MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
   procedure MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure RecalcPos;
   procedure SetNewClient(const value: TFmxObject);
   procedure SwapMove(Sender: TObject);
   procedure DoTracking; virtual;
 published
   property Slave: TControl3D read GetSlave;        // Eigentuemer holen, wenn vorhanden
   property Move: Boolean read FMoveMode;         // Umschalten Drehen / Bewegen
   property OnTracking: TNotifyEvent read FOnTracking write FOnTracking;
 end;

 TAchse3D = class(TControl3D)
 private
   fCursor3D: TCursor3D;
 protected
   procedure Render; override;
   procedure DoMouseEnter; override;
   procedure DoMouseLeave; override;
   procedure MouseDown3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
   procedure MouseMove3D(Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
   procedure MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D); override;
 public
   constructor Create(AOwner: TComponent); override;
 published
   property Cursor3D: TCursor3D read fCursor3D;
 end;

implementation

//-------------------


{ --------------------------------- TCursor3D ----------------------------------------------}

constructor TCursor3D.Create(AOwner: TComponent);
var
 T,L: Single;
begin
 inherited;
 FPlaneNorm := TPoint3D.Create(0,0,-1); // Plane X/Y bevegen
 T := 0.3;  // Breite
 L := 0.8;  // Laenge
 Visible := False;
 Tag := 3;   // fuer X/Y Z=P1anNorm

 // X-Achse
 fX := TAchse3D.Create(Self);
 AddObject(fX);
 fX.Tag := 1;
 fX.SetSize(T,T,L);

 // Y-Achse
 fY := TAchse3D.Create(Self);
 AddObject(fY);
 fY.Tag := 2;
 fY.SetSize(L,T,T);

 // Z-Achse
 fZ := TAchse3D.Create(Self);
 AddObject(fZ);
 fZ.Tag:=3;
 fZ.SetSize(T,L,T);
end;

destructor TCursor3D.Destroy;
begin
 RemoveObject(fX);
 RemoveObject(fY);
 RemoveObject(fZ);
 if Assigned(fX) then
   FreeAndNil(fX);
 if Assigned(fY) then
   FreeAndNil(fY);
 if Assigned(fZ) then
   FreeAndNil(fZ);
 inherited;
end;

procedure TCursor3D.DoTracking;
begin
 if (not (csLoading in ComponentState)) and Assigned(FOnTracking) then
   FOnTracking(Slave);   // Uebergabe des Sklaven!
end;

function TCursor3D.GetSlave: TControl3D;
begin
 Result := Nil;
 if Assigned(Parent) and (Parent is TControl3D) then
   Result := TControl3D(Parent);
end;

procedure TCursor3D.MouseDown3D(Button: TMouseButton; Shift: TShiftState;
             X, Y: Single; RayPos, RayDir: TVector3D);
var
 I: TPoint3D;
begin
 if (ssLeft in Shift) and Assigned(Slave) then begin
   AutoCapture := True;
   I := NullPoint3D;
   RayCastPlaneIntersect(RayPos,RayDir,NullPoint3D,FPlaneNorm,I);  // Check die Oberflaeche!
   Slave.Position.DefaultValue := Slave.Position.Point - TPoint3D(LocalToAbsoluteVector(I));
   DoTracking;
 end;
 inherited;
end;

procedure TCursor3D.MouseMove3D(Shift: TShiftState;
             X, Y: Single; RayPos, RayDir: TVector3D);
var
 I: TPoint3D;
begin
 if (ssLeft in Shift) and Assigned(Slave) then begin
   I := NullPoint3D;
   RayCastPlaneIntersect(RayPos,RayDir,NullPoint3D,FPlaneNorm,I);  // Check die Oberflaeche!
   Slave.Position.Point := Slave.Position.DefaultValue + TPoint3D(LocalToAbsoluteVector(I));
   DoTracking;
 end;
 inherited;
end;

procedure TCursor3D.MouseUp3D(Button: TMouseButton; Shift: TShiftState;
             X, Y: Single; RayPos, RayDir: TVector3D);
begin
 inherited;
 AutoCapture := False;
end;

procedure TCursor3D.RecalcPos;
var
 S, D, Q: TPoint3D;
 R: Single;
begin
 if Assigned(Slave) then begin
   Visible := True;
   Self.SetSize(Slave.Width, Slave.Height, Slave.Depth);
   S := Slave.Scale.Point;
   D := TPoint3D.Create(Slave.Width * S.X, Slave.Height * S.Y, Slave.Depth * S.Z);
   Q := TPoint3D.Create(1/S.X, 1/S.Y, 1/S.Z);
   R := 1;   // Abstand
   fX.Position.Point := Point3D((D.X * 0.5) + R + fX.Width, 0, 0) * Q;
   fX.Scale.Point := Q;
   fY.Position.Point := Point3D(0, -((D.Y * 0.5) + R + fY.Height), 0) * Q;
   fY.Scale.Point := Q;
   fZ.Position.Point := Point3D(0, 0, -((D.Z * 0.5) + R + fZ.Depth)) * Q;
   fZ.Scale.Point := Q;
 end
 else
   Visible := False;
end;

procedure TCursor3D.Render;
var
 S: Single;
 C: TAlphaColor;
begin
 S := 1;
 C := TAlphaColors.Blue;
 if Move then begin // Wenn move, einfach Linie 1aenger machen
   S := 50;
   C := TAlphaColors.Ghostwhite;
 end;
 Context.DrawLine(NullPoint3D,fX.Position.Point * S, 1, C);   // X
 Context.DrawLine(NullPoint3D,fY.Position.Point * S, 1, C);   // Y
 Context.DrawLine(NullPoint3D,fZ.Position.Point * S, 1, C);   // Z
 Context.DrawCube(NullPoint3D,TPoint3D.Create(Width,Height,Depth),1,TAlphaColors.Blue);
end;

procedure TCursor3D.SetNewClient(const Value: TFmxObject);
begin
 Parent:= Value;  // Wechsel ausfuehren
 RecalcPos;       // Aktualisieren
end;

procedure TCursor3D.SwapMove(Sender: TObject);
begin
 FMoveMode := not FMoveMode;
 Repaint;
end;

{ --------------------------------- TAchsen3D ----------------------------------------------}

constructor TAchse3D.Create(AOwner: TComponent);
begin
 inherited;
 if not (AOwner is TCursor3D) then
   raise Exception.Create('Kein Cursor3D');
 fCursor3D := TCursor3D(AOwner);
 Cursor := crHandPoint;
 OnDblClick := fCursor3D.SwapMove;   // Umschalten Drehen/Bewegen
end;

procedure TAchse3D.Render;
var
 C,N: TAlphaColor;
 HierIstHinten: Boolean;  // False
begin
 if fCursor3D.Move then begin
   N := TAlphaColors.Black;
   if IsMouseOver then
     C := TAlphaColors.Yellow
   else
     C := TAlphaColors.Green;
 end
 else begin
   N := TAlphaColors.Blue;
   if IsMouseOver then
     C := TAlphaColors.Red
   else
     C := TAlphaColors.Deepskyblue;
 end;
 if fCursor3D.Tag = Tag then begin  // fuer PlanNorm
   if IsMouseOver then
     C := TAlphaColors.Chartreuse
   else
     C := TAlphaColors.Chocolate;
 end;
 Context.FillCube(NullPoint3D, TPoint3D.Create(Width,Height,Depth),1,C);
 Context.DrawCube(NullPoint3D, TPoint3D.Create(Width,Height,Depth),1,N);
end;

procedure TAchse3D.DoMouseEnter;
begin
 inherited;
 Repaint;
end;

procedure TAchse3D.DoMouseLeave;
begin
 inherited;
 Repaint;
end;

procedure TAchse3D.MouseDown3D(Button: TMouseButton;
                      Shift: TShiftState; X, Y: Single;
                      RayPos, RayDir: TVector3D);
var
 C: TPoint3D;
 Wc,Winkel:Single;
 W,P1,P2: TPoint3D;
begin
 if (ssleft in Shift) and Assigned(fCursor3D.Slave) then begin
   AutoCapture := True;
   case Tag of
     1: Cursor3D.FPlaneNorm := TPoint3D.Create(-1,0,0);
     2: Cursor3D.FPlaneNorm := TPoint3D.Create(0,-1,0);
     3: Cursor3D.FPlaneNorm := TPoint3D.Create(0,0,-1);   // Standard X/Y
   end;
   Cursor3D.Tag := Tag;    //Abgleich, was aktuell ist
   if Cursor3D.Move then begin
     P1 := RayPos + RayDir * RayPos.Length;
     P2 := AbsoluteToLocal3D(fCursor3D.Slave.Position.Point);
     case Tag of
       1: P2.X := P2.X - P1.X;   // X-Achse
       2: P2.Y := P2.Y - P1.Y;   // Y-Achse
       3: P2.Z := P2.Z - P1.Z;   // Z-Achse
     end;
     Position.DefaultValue := P2;
   end
   else begin
     RotationCenter.Point := fCursor3D.Slave.RotationAngle.Point;
     Position.DefaultValue:= Context.WorldToScreen(TProjection.Camera, fCursor3D.Slave.AbsolutePosition);
     Position.DefaultValue:= Point3D(Position.DefaultValue.X,Position.DefaultValue.Y,
                                     RadToDeg(ArcTan2(Position.DefaultValue.Y - Y,
                                                      X - Position.DefaultValue.X)));  // Winkel berechnen
   end;
   Repaint;
   fCursor3D.DoTracking;
 end;
 inherited;
end;




procedure TAchse3D.MouseMove3D(Shift: TShiftState; X, Y: Single;
 RayPos, RayDir: TVector3D);
var
 C: TPoint3D;
 Wc,Winkel:Single;
 W,P1,P2: TPoint3D;
begin
 if (ssleft in Shift) and Assigned(fCursor3D.Slave) and Assigned(Viewport) then begin
   if Cursor3D.Move then begin
     P1 := RayPos + RayDir * RayPos.Length;
       P2 := Position.DefaultValue;
       case Tag of
         1: P2.X := P2.X + P1.X;   // X-Achse
         2: P2.Y := P2.Y + P1.Y;   // Y-Achse
         3: P2.Z := P2.Z + P1.Z;   // Z-Achse
       end;
       fCursor3D.Slave.Position.Point := LocalToAbsolute3D(P2);
   end
   else begin
     C := Viewport.CurrentCamera.AbsolutePosition;
     C := AbsoluteToLocal3D(C).Normalize;
     Winkel := RadToDeg(ArcTan2(Position.DefaultValue.Y - Y, X - Position.DefaultValue.X)); // Winkel
     Wc := Winkel - Position.DefaultValue.Z;
     W := RotationCenter.Point;
     case tag of    // Auf welcher Achse soll gedreht werden?
       1: if C.Y > 0 then // X-Achse
            W.Y := W.Y + Wc
          else
            W.Y := W.Y - Wc;
       2: if C.Z > 0 then // Y-Achse
            W.Z := W.Z + Wc
          else
            W.Z := W.Z - Wc;
       3: if C.X > 0 then // Z-Achse
            W.X := W.X + Wc
          else
            W.X := W.X - Wc;
     end;
     fCursor3D.Slave.RotationAngle.Point := W;
   end;
   fCursor3D.DoTracking;
 end;
 inherited;
end;

procedure TAchse3D.MouseUp3D(Button: TMouseButton; Shift: TShiftState; X, Y: Single; RayPos, RayDir: TVector3D);
begin
 inherited;
 AutoCapture := False;
end;


end.
