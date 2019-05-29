unit FMeX.Animation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.DateUtils, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Platform,
  FMX.Ani, FMX.Objects, Generics.Collections, FMeX.Ticks
  ,FMX.Graphics, FMX.Controls3D,
  FMX.Viewport3D, FMX.MaterialSources, System.Math.Vectors;

type

  //Aim : Replace native FMX animation system by a simple one : not based on RTTI.
  TofCustomUpdater = Class
  Private
  Protected
    FObject: TControl;
  Public
    Procedure Process(MilliSecondPassedFromLastCall : Double); Virtual; Abstract;

    Property ControledObject : TControl read FObject Write FObject;
  End;

  TofAnimationControl = (ofacNone, ofacLoop, ofacInOutOnce, ofacInOutLoop);
  TofUpdater = Class(TofCustomUpdater)
  private
    FAnimationControl: TofAnimationControl;
  Protected
    FDuration: Double;
    FTotalTimeEllapsed : Double;
    FDelay: Double;

    procedure SetDuration(const Value: Double);
    Function GetIsRunning : Boolean; Virtual; Abstract;
  Public

    Constructor Create; Virtual;
    Procedure Process(MilliSecondPassedFromLastCall : Double); Virtual;

    Property Duration : Double read FDuration Write SetDuration;
    Property Delay : Double read FDelay Write FDelay;
    Property IsRunning : Boolean read GetIsRunning;

    Property AnimationControlTypeAtEnd : TofAnimationControl read FAnimationControl Write FAnimationControl;
  End;

  TofForce = Class
    Force : TVector;
    Ponctual : Boolean;
  end;

  TofUpdaterPseudoPhysics = Class(TofCustomUpdater)
  Private
    FVelocity: TVector;
  Protected
    Forces : TList;
  Public
    Constructor Create; Virtual;
    Destructor Destroy; override;

    Procedure Process(MilliSecondPassedFromLastCall : Double); Virtual;

    Procedure AddForces(x,y : Single; Const aPonctualOne : Boolean = True);

    Property CurrentVelocity : TVector read FVelocity;

  End;

  TofLinearPositionUpdate = Class(TofUpdater)
  private
    FPosStop: Tpointf;
    FPosStart: TPointf;
    FIsRunning : Boolean;

    Function GetIsRunning : Boolean; Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Process(MilliSecondPassedFromLastCall : Double); Override;

    Property PositionStart : Tpointf read FPosStart Write FPosStart;
    Property PositionStop : Tpointf read FPosStop Write FPosStop;
  End;

  TofLinearRotationUpdate = Class(TofUpdater)
  Private
    FTurnCount: Double;
    FAngleStart: Double;
    FIsRunning : Boolean;

    Function GetIsRunning : Boolean; Override;
  Public
    Constructor Create; Override;
    Procedure Process(MilliSecondPassedFromLastCall : Double); Override;

    Property TurnCount : Double read FTurnCount Write FTurnCount;
    Property RotationAngleStart : Double read FAngleStart Write FAngleStart;
  End;

  TofUpdaterManager = Class
  Protected
    FUpdateList : TList<TofUpdater>;
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Update; Virtual; Abstract;
  End;

  TofCadencer = Class(TofUpdaterManager)
  Private
    FCall : Double;
    FTotalTimeEllapsed : Double;

    Function FindObjectInListForLinearPosition(aObject : TControl) : TofLinearPositionUpdate;
    Function FindObjectInListForLinearRotation(aOBject : TControl) : TofLinearRotationUpdate;

  Public
    Constructor Create; Override;
    Function AddLinearPositionUpdaterOn( aObject : TControl;
                                          Target : TPointf;
                                          DurationInMilliSec : Double;
                                          Const UpdateIfObjectYetExists : Boolean = True;
                                          Const AnimationControlType : TofAnimationControl = ofacNone) : TofUpdater;
    Function AddLinearRotationUpdaterOn( aObject : TControl;
                                          TargetTurnCount,
                                          DurationInMilliSec : Double;
                                          Const UpdateIfObjectYetExists : Boolean = True) : TofUpdater;

    Procedure AddAnimationInstance( aInstance : TofUpdater );

    Procedure Update; Override;
  End;

  TofCadencerPseudoPhysics = Class(TofUpdaterManager)
  Private
    FCall : Double;

    Function FindObjectInListForPseudoPhysics(aObject : TControl) : TofUpdaterPseudoPhysics;
  Public
    Procedure Update; Override;

    Procedure AddAsStaticObstacle( aObject : TControl);

    Procedure AddForceOn( aObject : TControl; ForceOnX, ForceOnY : Single; Const ForcePonctual : Boolean = true);

  End;

Implementation



{ TofLinearPositionUpdate }

constructor TofLinearPositionUpdate.Create;
begin
  inherited;
  FPosStart := PointF(0,0);
  FPosStop := PointF(0,0);
  FIsRunning := False;
end;

destructor TofLinearPositionUpdate.Destroy;
begin
  FPosStart := PointF(0,0);
  inherited;
end;

function TofLinearPositionUpdate.GetIsRunning: Boolean;
begin
  result := FIsRunning;
end;

procedure TofLinearPositionUpdate.Process(MilliSecondPassedFromLastCall : Double);
var a,b,d : Double;
begin
  Inherited;
  d := FTotalTimeEllapsed / FDuration;
  FIsRunning := d<=1;
  if d>1 then
  begin
    //Finnished.
    if Assigned(FObject) then
    begin
      FObject.Position.Point := FPosStop;
    end;
    //Trig something ?
    case FAnimationControl of
      ofacNone :
      begin
        //Nothing.
        //Enabled := false;
      end;
      ofacLoop :
      begin
        FTotalTimeEllapsed := 0;
        FObject.Position.Point := FPosStart;
      end;
      ofacInOutOnce :
      begin
        FTotalTimeEllapsed := 0;
        FObject.Position.Point := FPosStop;
        FPosStop := FPosStart;
        FPosStart := FObject.Position.Point;
        //Here : put a count to make animation only once.
      end;
      ofacInOutLoop :
      begin
        FTotalTimeEllapsed := 0;
        FObject.Position.Point := FPosStop;
        FPosStop := FPosStart;
        FPosStart := FObject.Position.Point;
      end;
    end;
  end
  else
  begin

    a := FPosStart.X - FPosStop.X;
    a := a*d;
    b := FPosStart.Y - FPosStop.Y;
    b := b*d;

    if Assigned(FObject) then
    begin
      FObject.Position.Point := Pointf(FPosStart.X - a,FPosStart.Y - b);
    end;
  end;
end;

{ TofCadencer }

Procedure TofCadencer.AddAnimationInstance(aInstance: TofUpdater);
begin
  if Assigned(aInstance) then
  begin
    if FUpdateList.IndexOf(aInstance) = -1 then
      FUpdateList.Add(aInstance);
  end;
end;

Function TofCadencer.AddLinearPositionUpdaterOn(aObject: TControl;
  Target: TPointf; DurationInMilliSec: Double;
  Const UpdateIfObjectYetExists : Boolean = True;
  Const AnimationControlType : TofAnimationControl = ofacNone) : TofUpdater;
var a : TofLinearPositionUpdate;
begin
  Assert(Assigned(aObject));
  a := nil;

  if UpdateIfObjectYetExists then
  begin
    a := FindObjectInListForLinearPosition(aObject);
  end;

  if not(Assigned(a)) then
  begin
    a := TofLinearPositionUpdate.Create;
    FUpdateList.Add(a);
  end;

  a.PositionStart := aObject.Position.Point;
  a.PositionStop := Target;
  a.Duration := DurationInMilliSec;
  a.ControledObject := aObject;
  a.AnimationControlTypeAtEnd := AnimationControlType;

  Result := a;
end;

Function TofCadencer.AddLinearRotationUpdaterOn(aObject: TControl; TargetTurnCount,
  DurationInMilliSec: Double; const UpdateIfObjectYetExists: Boolean) : TofUpdater;
var a : TofLinearRotationUpdate;
begin
  Assert(Assigned(aObject));
  a := nil;

  if UpdateIfObjectYetExists then
  begin
    a := FindObjectInListForLinearRotation(aObject);
  end;

  if not(Assigned(a)) then
  begin
    a := TofLinearRotationUpdate.Create;
    FUpdateList.Add(a);
  end;

  a.TurnCount := TargetTurnCount;
  a.RotationAngleStart :=  TContent(aObject).RotationAngle;
  a.Duration := DurationInMilliSec;
  a.ControledObject := aObject;

  result := a;
end;

constructor TofCadencer.Create;
begin
  Inherited;
  FTotalTimeEllapsed := 0;
end;

function TofCadencer.FindObjectInListForLinearPosition(aObject : TControl) : TofLinearPositionUpdate;
var i : integer;
begin
  Result := Nil;
  for i := 0 to FUpdateList.Count-1 do
  begin
    if TofUpdater(FUpdateList[i]) is TofLinearPositionUpdate  then
    begin
      if TofLinearPositionUpdate(FUpdateList[i]).ControledObject = aObject then
      begin
        Result := TofLinearPositionUpdate(FUpdateList[i]);
        Break;
      end;
    end;
  end;
end;

function TofCadencer.FindObjectInListForLinearRotation(
  aOBject: TControl): TofLinearRotationUpdate;
var i : integer;
begin
  Result := Nil;
  for i := 0 to FUpdateList.Count-1 do
  begin
    if TofUpdater(FUpdateList[i]) is TofLinearRotationUpdate  then
    begin
      if TofLinearRotationUpdate(FUpdateList[i]).ControledObject = aObject then
      begin
        Result := TofLinearRotationUpdate(FUpdateList[i]);
        Break;
      end;
    end;
  end;
end;

procedure TofCadencer.Update;
var i : integer;
    s : Double;
    delta : Double;
    abc : TofUpdater;
begin
  s := GetTickCount;

  if FCall = 0 then
    FCall := s;

  delta := s - FCall;

  if delta<0 then
    Exit;

  FTotalTimeEllapsed := FTotalTimeEllapsed + Delta;

  for i  := 0 to FUpdateList.Count-1 do
  begin
    abc := TofUpdater(FUpdateList[i]);
    if FTotalTimeEllapsed>abc.Delay then
    begin
      abc.Process(delta);
    end;
  end;

  FCall := s;
end;

{ TofUpdater }

constructor TofUpdater.Create;
begin
  Inherited;
  Duration := 2000;
  FTotalTimeEllapsed := 0;
  FAnimationControl := ofacNone;
end;


procedure TofUpdater.Process(MilliSecondPassedFromLastCall : Double);
begin
  if FDuration = 0 then
    Exit;
  FTotalTimeEllapsed := FTotalTimeEllapsed + MilliSecondPassedFromLastCall;
end;


procedure TofUpdater.SetDuration(const Value: Double);
begin
  FDuration := Value;
  FTotalTimeEllapsed := 0;
end;

{ TofLinearRotationUpdate }

constructor TofLinearRotationUpdate.Create;
begin
  inherited;
  FTurnCount := 1;
  FIsRunning := False;
end;

function TofLinearRotationUpdate.GetIsRunning: Boolean;
begin
  result := FIsRunning;
end;

procedure TofLinearRotationUpdate.Process(MilliSecondPassedFromLastCall : Double);
var d,a : Double;
begin
  Inherited;

  d := FTotalTimeEllapsed / FDuration;
  FIsRunning := d<=1;
  if d>1 then
  begin
    //Finnished.
    if Assigned(FObject) then
    begin
      TContent(FObject).RotationAngle := FAngleStart + FTurnCount * 360;
    end;
    //Trig something ?
  end
  else
  begin

    a := FturnCount*360;
    a := a*d;
    a := a - FAngleStart;

    if Assigned(FObject) then
    begin
      TContent(FObject).RotationAngle := a;
    end;
  end;
end;

{ TofUpdaterPseudoPhysics }

procedure TofUpdaterPseudoPhysics.AddForces(x, y: Single;
  const aPonctualOne: Boolean);
var f : TofForce;
begin
  f := TofForce.Create;
  f.Force.X := x;
  f.Force.Y := y;
  f.Ponctual := aPonctualOne;
  Forces.Add(f);
end;

constructor TofUpdaterPseudoPhysics.Create;
begin
  inherited Create;
  Forces := TList.Create;
end;

destructor TofUpdaterPseudoPhysics.Destroy;
var i : integer;
begin
  for i := Forces.Count-1 downto 0 do TObject(Forces[i]).Free;
  FreeAndNil(Forces);
  inherited;
end;

procedure TofUpdaterPseudoPhysics.Process(
  MilliSecondPassedFromLastCall: Double);
var i : integer;
    v, p : TPointf;
begin
  p.X := 0;
  p.Y := 0;
  for i:= Forces.Count-1 downto 0 do
  begin
    p.X := p.X + TofForce(Forces[i]).Force.X;
    p.Y := p.Y + TofForce(Forces[i]).Force.Y;
    if TofForce(Forces[i]).Ponctual then
    begin
      TofForce(Forces[i]).Free;
      Forces.Delete(i);
    end;
  end;

  FVelocity.X := FVelocity.X + p.X;
  FVelocity.Y := FVelocity.Y + p.Y;


  if Assigned(FObject) then
  begin
    v := FObject.Position.Point;

    //Velocity is Velocity per sec.
    p.X := v.X + (FVelocity.X * MilliSecondPassedFromLastCall / 1000);
    p.Y := v.Y + (FVelocity.Y * MilliSecondPassedFromLastCall / 1000);

    FObject.Position.Point := p;
  end;

end;

{ TofUpdaterManager }

constructor TofUpdaterManager.Create;
begin
  Inherited Create;
  FUpdateList := TList<TofUpdater>.Create;
end;

destructor TofUpdaterManager.Destroy;
var i : integer;
begin
  for i := FUpdateList.Count-1 downto 0 do TObject(FUpdateList[i]).Free;
  FreeAndNil(FUpdateList);
  inherited;
end;

{ TofPseudoPhysics }

procedure TofCadencerPseudoPhysics.AddAsStaticObstacle(aObject: TControl);
begin

end;

procedure TofCadencerPseudoPhysics.AddForceOn(aObject: TControl;
  ForceOnX, ForceOnY: Single; Const ForcePonctual : Boolean);
var a : TofUpdaterPseudoPhysics;
begin
  Assert(Assigned(aObject));
  a := FindObjectInListForPseudoPhysics(aObject);

  if not(Assigned(a)) then
  begin
    a := TofUpdaterPseudoPhysics.Create;
    a.ControledObject := aObject;
    a.AddForces(ForceOnX,ForceOnY,ForcePonctual);
    FUpdateList.Add(TofUpdater(a));
  end;
end;

function TofCadencerPseudoPhysics.FindObjectInListForPseudoPhysics(
  aObject: TControl): TofUpdaterPseudoPhysics;
var i : integer;
begin
  Result := Nil;
  for i := 0 to FUpdateList.Count-1 do
  begin
    if TofCustomUpdater(FUpdateList[i]) is TofUpdaterPseudoPhysics  then
    begin
      if TofUpdaterPseudoPhysics(FUpdateList[i]).ControledObject = aObject then
      begin
        Result := TofUpdaterPseudoPhysics(FUpdateList[i]);
        Break;
      end;
    end;
  end;
end;

procedure TofCadencerPseudoPhysics.Update;
var i : integer;
    s : Double;
    delta : Double;
begin
  s := GetTickCount;

  if FCall = 0 then
    FCall := s;

  delta := s - FCall;

  if delta<0 then
    Exit;


  for i  := 0 to FUpdateList.Count-1 do
  begin
    TofUpdaterPseudoPhysics(FUpdateList[i]).Process(delta);
    //Idee : Resoudre une liste de proximité des control enregistré dans ce cadencer
    //et la passer à l'updater. Celui ci pourrait alors résoudre les collision par un event ?
    //Ou automatiquement si obstacle.
  end;

  FCall := s;
end;

end.


