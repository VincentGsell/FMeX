unit GS.ParticleEngine2D;

interface

Uses Classes, FMeX.Ticks, FMeX.Types, FMeX.Vectors;

type
  TGSCustomParticleEngine2D = class;
  TGSCustomParticleEngineClass = Class of TGSCustomParticleEngine2D;

  TGSParticle2D = record
    X,Y : Single;                      //Current Position

    TimeToLiveInMilliSec : Double;     //Total time that Particle have to live. (Constant)
    TimeRelevantInMilliSec : Double;   //Time to live ammount for particle.
    TimeStart : Cardinal;              //Internal use for estimate time.
    VelX,VelY : Double;                //Current Speed In pixels per seconds.
    TagOne : Cardinal;                 //General Usage Tag 1. (color, object link.)
    TagTwo : Cardinal;                 //General Usage Tag 2.
    ColX,ColY : Double;                //Coord of last collision.
    Collided : Boolean;                //If true, it is in collided situation.
  end;

  TGSForce2d = Class
  Public
    ForceVector : TofVector;
    Constructor Create;
  end;

  TGSForceList = class(Tlist)
  private
    function GetForce(index: cardinal): TGSForce2d;
    procedure SetForce(index: cardinal; const Value: TGSForce2d);
  Public
    Procedure AddForce(Fx,Fy : Double);
    //Procedure AddForce(F: TofVector);
    //Procedure AddForcePonctual(Fx,Fy : Integer); //remove automATICALY AFTER APPLY
    //Procedure ReplaceAllForceByOneResultForce;
    Property Forces[index : cardinal] : TGSForce2d read GetForce Write SetForce; Default;
  end;

  //this class must be heritate to define shape for collision. A particle is mathematicaly discreet, so, it need at least a line, to collide.
  TGSCollidable2d = Class
  Public


    Procedure Resolve(var aParticle : TGSParticle2D); virtual; Abstract;

//    Procedure Draw(re : ofRenderer); Virtual; Abstract;
  end;

  TGSCollidableList = class(Tlist)
  private
    function GetCollidable(Index: Cardinal): TGSCollidable2d;
    procedure SetCollidable(Index: Cardinal; const Value: TGSCollidable2d);
  Public
    Procedure AddCollideShape(aCollidable : TGSCollidable2d);
    Procedure AddCollideLine(x,y,x2,y2 : Double; EnergyRestitution : Double = 0.70);
    Property Collidables[Index : Cardinal] : TGSCollidable2d read GetCollidable Write SetCollidable; Default;
  end;

  TGSCollidable2d_StaticLine = class(TGSCollidable2d)
  private
    FEnergyRestitut: Double;
  Public
    X1,Y1,X2,Y2 : Double;
    Constructor Create(ax,ay,ax1,ay1 : Double; EnergyRestitution : Double = 0.70); Virtual;
//    Procedure Draw(re : ofRenderer); Override;
    Procedure Resolve(var aParticle : TGSParticle2D); Override;

    //For simplify calulus, we introduce a single var which make "aproximation" of vector calculus for particle dynamic.
    //This will simply added on velocity.
    Property EnergyRestitutionOnCollide : Double read FEnergyRestitut Write FEnergyRestitut;
  end;


  TGSCustomParticleEngine2D = class
  private
    FTimeManage: Boolean;
    FCollisionManage: Boolean;
    FForceManageEnabled: Boolean;
    FConstraintEnabled: Boolean;
    FMaximalVelocity: Single;
    function GetParticleCount: Cardinal;
    procedure SetParticleCount(const Value: Cardinal);
    function GetParticle(Index: Cardinal): TGSParticle2D;
    procedure SetParticle(Index: Cardinal; const Value: TGSParticle2D);
    function GetIsTimeElapsed: Boolean;
  protected
     Procedure ResetTimeEval;
     Procedure ResetPositionAndVelocity;
     Procedure ResetCollisitionStatus;

  Public
    Position : TofPoint;

    Offset : TofPoint;

    ParticleList : array of TGSParticle2D;

    Object_ForceList : TGSForceList;             //Global Force list. Apply to all particle.
    Object_CollideList : TGSCollidableList;      //Global Collision test. Apply to all particle.

    constructor Create(aParticleCount : Cardinal); virtual;
    destructor Destroy; Override;

    Procedure initialize; Virtual;
    Procedure Update; virtual;


    Function AddParticle(ax,ay,avelx,avely : Double) : TGSParticle2D;
//    Procedure Draw(re : ofRenderer);

    property ParticleCount : Cardinal read GetParticleCount Write SetParticleCount;
    property Particles[Index : Cardinal] : TGSParticle2D read GetParticle Write SetParticle; Default;
    property TimeManageEnabled : Boolean read FTimeManage write FTimeManage;
    PRoperty IsTimeElapsed : Boolean read GetIsTimeElapsed;

    property CollisionManageEnabled : Boolean read FCollisionManage Write FCollisionManage;
    property ForceManagerEnabled : Boolean read FForceManageEnabled Write FForceManageEnabled;
    Property ConstraintEnabled : Boolean read FConstraintEnabled Write FConstraintEnabled;

    Property MaximalVelocity : Single read FMaximalVelocity Write FMaximalVelocity;

  end;

  TGSParticleEngine2d = class(TGSCustomParticleEngine2D)   //General use. (especially for complexe entity, such video game hero which need collision or gravity :))
  end;


  //StarField. (simple, left to right)
  TGSParticleEngine2D_StarField = class(TGSCustomParticleEngine2D)
  private
    FMaxVelocity: Cardinal;
  public
    Constructor Create(aParticleCount : Cardinal); Override;

    Procedure Initialize; override;
    Procedure Update; Override;

    property BehaviourMaxVelocity : Cardinal read FMaxVelocity Write FMaxVelocity;
  end;

  //Base emiter : Emit all in one loop, then, reemit on end of life time.
  TGSCustomParticleEngine2D_DiscretEmiter = Class(TGSCustomParticleEngine2D)
  Private
    FParticleDuration: Cardinal;
  public
    Constructor Create(aParticleCount : Cardinal); Override;

    Procedure Initialize; override;

    property ParticleDefaultDurationMilliSec : Cardinal read FParticleDuration Write FParticleDuration;
  end;

  //Explode.
  TGSParticleEngine2D_Explode = class(TGSCustomParticleEngine2D_DiscretEmiter)
  Private
    FMaxVelocity: Cardinal;
    FMaxDurationInMS: Cardinal;
    FQQ: Boolean;
    FAD: Integer;
    FADD: Double;
  public
    Constructor Create(aParticleCount : Cardinal); Override;
    Procedure Initialize; override;

    property BehaviourMaxVelocity : Cardinal read FMaxVelocity Write FMaxVelocity;
    property BehaviourMaxDurationInMS : Cardinal read FMaxDurationInMS Write FMaxDurationInMS;
    property BehaviourQuickSpeedMeansQuickDie : Boolean read FQQ Write FQQ;

    //AngleDistribution : Default value is 360 degree. That is a "opening angle", it will distribute the particle's angle on 360 toward Start angle (0 by default)
    property AngleDistribution : Integer read FAD write FAD; //Usage : Put 30 degree value and you'll otain something such as "reactor burst effect"
    property AngleDistributionDirection : Double read FADD write FADD; //0 by default.
  end;

  //Explode with (re)emition. (Cool for magical star.)
  TGSParticleEngine2D_ExplodeAdLib = class(TGSParticleEngine2D_Explode)
  Private
  public
    Constructor Create(aParticleCount : Cardinal); Override;
    Procedure Update; Override;
  end;

  //Emitter - Emits particle. (cool for smoke behing engine)
  TGSParticleEngine2D_Emiter = class(TGSCustomParticleEngine2D_DiscretEmiter)
  Private
    FEmitParticleCount: Cardinal;
    FinternalCount : cardinal;
  public
    Constructor Create(aParticleCount : Cardinal); Override;

    Procedure Initialize; Override;
    Procedure Update; Override;

    Property EmitParticleQuantityByLoop : Cardinal Read FEmitParticleCount Write FEmitParticleCount;
  end;


  //Snow or leaf fallen.

  //rain, fire ?

  //Particle with collision ? (Bullet)

  //Particle with gravity (implenting the example in PROTOTYPES)

  //Image ? (with each pixels (or Each little quarter of pict.) is a particle --> Nice effect.)


implementation

uses Math;

{ TGSCustomParticleEngine2D }

function TGSCustomParticleEngine2D.AddParticle(ax, ay, avelx,
  avely: Double): TGSParticle2D;
var a : TGSParticle2D;
begin
  a.X:=ax;
  a.Y:=ay;
  a.VelX:=avelx;
  a.VelY:=avely;
end;

constructor TGSCustomParticleEngine2D.Create(aParticleCount: Cardinal);
begin
  ParticleCount := aParticleCount;
  FTimeManage := True;
  FCollisionManage := False;
  FForceManageEnabled := False;
  FConstraintEnabled:= False;
  Object_ForceList := TGSForceList.Create;
  Object_CollideList := TGSCollidableList.Create;
  Offset := ofPoint(100,100,0);
  FMaximalVelocity := 50;
end;

destructor TGSCustomParticleEngine2D.Destroy;
begin
  SetLength(ParticleList,0);
  Object_ForceList.Free;
  inherited;
end;

{
procedure TGSCustomParticleEngine2D.Draw(re : ofRenderer);
var i : integer;
begin
  if TimeManageEnabled then
  begin
    for i:=0 to ParticleCount-1 do
    begin
      if ParticleList[i].TimeRelevantInMilliSec>0 then
        re.Rectangle( ParticleList[i].X-3,Particles[i].Y-3,
                      ParticleList[i].X+3,Particles[i].Y+3);
    end;
  end
  else
  begin
    for i:=0 to ParticleCount-1 do
    begin
      re.Rectangle( ParticleList[i].X-3,Particles[i].Y-3,
                    ParticleList[i].X+3,Particles[i].Y+3);
    end;
  end;
end;
}
function TGSCustomParticleEngine2D.GetIsTimeElapsed: Boolean;
var i : integer;
begin
  Result := TimeManageEnabled;

  if Result then
  for i := 0 to ParticleCount-1 do
  begin
    if ParticleList[i].TimeRelevantInMilliSec > 0 then
    begin
      Result := false;
      Break;
    end;
  end;
end;

function TGSCustomParticleEngine2D.GetParticle(
  Index: Cardinal): TGSParticle2D;
begin
  Assert(Index<High(ParticleList)+1);
  Result := ParticleList[Index];
end;

function TGSCustomParticleEngine2D.GetParticleCount: Cardinal;
begin
  Result := High(ParticleList)+1;
end;

procedure TGSCustomParticleEngine2D.initialize;
begin
  ResetTimeEval;
  ResetPositionAndVelocity;
  ResetCollisitionStatus;
end;

procedure TGSCustomParticleEngine2D.ResetCollisitionStatus;
var i : integer;
begin
  for i := 0 to ParticleCount-1 do
    ParticleList[i].Collided:=false;
end;

procedure TGSCustomParticleEngine2D.ResetPositionAndVelocity;
var i : integer;
begin
  for i := 0 to ParticleCount-1 do
  begin
    ParticleList[i].VelX:=0;
    ParticleList[i].VelY:=0;
    ParticleList[i].X:=0;
    ParticleList[i].Y:=0;
  end;
end;

procedure TGSCustomParticleEngine2D.ResetTimeEval;
var i : integer;
begin
  for i := 0 to ParticleCount-1 do
    ParticleList[i].TimeStart:= TheCadencer.Tic;
end;

procedure TGSCustomParticleEngine2D.SetParticle(Index: Cardinal;
  const Value: TGSParticle2D);
begin
  Assert(Index<=High(ParticleList));
  ParticleList[Index] := Value;
end;

procedure TGSCustomParticleEngine2D.SetParticleCount(const Value: Cardinal);
begin
  SetLength(ParticleList,Value);
end;

procedure TGSCustomParticleEngine2D.Update;
var i,j : integer;
    m : Cardinal;
begin
  if FForceManageEnabled then
  begin
    for i := 0 to ParticleCount-1 do
    begin
      For j:=0 to Object_ForceList.Count-1 do
      begin
        //ParticleList[i].VelX:=(ParticleList[i].VelX+Object_ForceList[j].ForceVector.X);
        //ParticleList[i].VelY:=(ParticleList[i].VelY+Object_ForceList[j].ForceVector.Y);

        ParticleList[i].VelX:=(ParticleList[i].VelX+ Object_ForceList[j].ForceVector.X * TheCadencer.TimeScaler);
        ParticleList[i].VelY:=(ParticleList[i].VelY+ Object_ForceList[j].ForceVector.Y * TheCadencer.TimeScaler);

        if ParticleList[i].VelX>FMaximalVelocity then
          ParticleList[i].VelX := FMaximalVelocity;
        if ParticleList[i].VelY>FMaximalVelocity then
          ParticleList[i].VelY := FMaximalVelocity;

      end;
    end;
  end;

  if FCollisionManage then
  begin
    for i := 0 to ParticleCount-1 do
    begin
      for j:= 0 to Object_CollideList.Count-1 do
      begin
        particleList[i].Collided:=False;
        Object_CollideList[j].Resolve(particleList[i]); //It the shape which is in charge of the particle behaviour.
        if particleList[i].Collided then
          Break;
      end;
    end;
  end;

  if FTimeManage then
  begin
    for i := 0 to ParticleCount-1 do
    begin
      If ParticleList[i].TimeRelevantInMilliSec>0 then
      begin
        ParticleList[i].X:=ParticleList[i].X+TheCadencer.TimeSliceValue(ParticleList[i].VelX);
        ParticleList[i].Y:=ParticleList[i].Y+TheCadencer.TimeSliceValue(ParticleList[i].VelY);

        ParticleList[i].TimeRelevantInMilliSec := ParticleList[i].TimeRelevantInMilliSec -  TheCadencer.DeltaTime; //  (m-ParticleList[i].TimeStart);
      end;
    end;
  end
  else
  begin
    for i := 0 to ParticleCount-1 do
    begin
        ParticleList[i].X:=ParticleList[i].X+TheCadencer.TimeSliceValue(ParticleList[i].VelX);
        ParticleList[i].Y:=ParticleList[i].Y+TheCadencer.TimeSliceValue(ParticleList[i].VelY);
    end;
  end;

end;

{ TGSParticleEngine2D_StarField }

constructor TGSParticleEngine2D_StarField.Create(aParticleCount: Cardinal);
begin
  inherited;
  FMaxVelocity := 6;
end;

procedure TGSParticleEngine2D_StarField.Initialize;
var i,rx,ry : integer;
begin
  Inherited;
  TimeManageEnabled:=False;
  Randomize;
  for i := 0 to ParticleCount-1 do
  begin
    rx := Random(Round(Offset.X));
    ry := Random(Round(Offset.Y));
    ParticleList[i].X:=Position.X+rx;
    ParticleList[i].Y:=Position.Y+ry;
    ParticleList[i].VelX:=Random(FMaxVelocity)+2; //velocity : 8  unity secs. with a minimum of 2
    ParticleList[i].VelY:=0;
  end;
end;


procedure TGSParticleEngine2D_StarField.Update;
var i : integer;
begin
  Inherited Update;
  for i := 0 to ParticleCount-1 do
  begin
    if ParticleList[i].X>Offset.X then
      ParticleList[i].X:=Position.x;
    if ParticleList[i].Y>Offset.Y then
      ParticleList[i].Y:=Position.Y;

    if ParticleList[i].X<Position.X then
      ParticleList[i].X:=Position.X;
    if ParticleList[i].Y<Position.Y then
      ParticleList[i].Y:=Position.Y;
  end;
end;

{ TGSParticleEngine2D_DiscretEmiter }

constructor TGSCustomParticleEngine2D_DiscretEmiter.Create(
  aParticleCount: Cardinal);
begin
  inherited;
  ParticleDefaultDurationMilliSec := 1000; //1 sec.
end;

procedure TGSCustomParticleEngine2D_DiscretEmiter.Initialize;
var i : integer;
begin
  Inherited;
  For i:=0 to ParticleCount-1 do
  begin
    ParticleList[i].TimeToLiveInMilliSec:=ParticleDefaultDurationMilliSec;
    ParticleList[i].TimeRelevantInMilliSec:=ParticleDefaultDurationMilliSec;
    ParticleList[i].TimeStart := TheCadencer.Tic;
  end;
end;

{ TGSParticleEngine2D_Explode }

constructor TGSParticleEngine2D_Explode.Create(aParticleCount: Cardinal);
begin
  inherited Create(aParticleCount);
  FMaxVelocity := 5;
  FMaxDurationInMS := 2000; //2 Sec.
  FQQ := False; //Quicker is the paticle, Faster it die.
  FAD := 360;
  FADD := 0;
end;

procedure TGSParticleEngine2D_Explode.Initialize;
var i : integer;
    v : TofVector;
    pp : single;

begin
  Inherited;
  Randomize;
  TimeManageEnabled:=True;
  v.X:=10;
  v.Y:=0;
  pp := Pi/180;
  For i:=0 to ParticleCount-1 do
  begin
    ParticleList[i].X:=Position.X;
    ParticleList[i].Y:=Position.Y;
    vNorm(v,Random(FMaxVelocity)+10+(Random(1000)/1000));

    //--> implementer here : to see again...
    if i < (ParticleCount div 2) then
      vAngle(v,FADD + Random(FAD) * pp + (Random(1000)/1000))
    else
      vAngle(v,FADD - Random(FAD) * pp + (Random(1000)/1000));

    ParticleList[i].VelX:= v.X;
    ParticleList[i].VelY:= v.Y;

    if FQQ Then
    begin
      ParticleList[i].TimeToLiveInMilliSec := (Random(Round(FMaxDurationInMS)) /  (vNorm(v)/10)) + 100; //100ms min. indexed on speed : More it speed, quicker it's die.
    end
    else
      ParticleList[i].TimeToLiveInMilliSec := Random(Round(FMaxDurationInMS))+100; //100ms min
    ParticleList[i].TimeRelevantInMilliSec := ParticleList[i].TimeToLiveInMilliSec;

  end;
end;


{ TGSParticleEngine2D_ExplodeEx }

constructor TGSParticleEngine2D_ExplodeAdLib.Create(
  aParticleCount: Cardinal);
begin
  inherited Create(aParticleCount);
  BehaviourMaxDurationInMS := 1000;
  BehaviourQuickSpeedMeansQuickDie := False;
end;

procedure TGSParticleEngine2D_ExplodeAdLib.Update;
var i : integer;
    v : TofVector;
    pp : single;
begin
  inherited;
  v.X:=1;
  v.Y:=0;
  pp := PI/180;
  For i:=0 to ParticleCount-1 do
  begin
    if ParticleList[i].TimeRelevantInMilliSec<=0 then //Reemit if terminated.
    begin
      ParticleList[i].X:=Position.X;
      ParticleList[i].Y:=Position.Y;
      ParticleList[i].TimeRelevantInMilliSec := ParticleList[i].TimeToLiveInMilliSec;
      ParticleList[i].TimeStart:=TheCadencer.Tic;

      vNorm(v,Random(BehaviourMaxVelocity)+1+(Random(1000)/1000));

      if i< (ParticleCount div 2) then
        vAngle(v,FADD + Random(FAD)* pp +(Random(100)/1000))
      else
        vAngle(v,FADD - Random(FAD)* pp +(Random(100)/1000));

      //vAngle(v,Random(360)+(Random(1000)/1000));
      //vAngle(v,45+(Random(1000)/1000)); //<-- Cool effect (motor burst ?)
      //vAngle(v,FADD + (Random(100)/1000));
      ParticleList[i].VelX:=v.X;
      ParticleList[i].VelY:=v.Y;
    end;
  end;
end;


{ TGSParticleEngine2D_DiscretEmiter }

constructor TGSParticleEngine2D_Emiter.Create(aParticleCount: Cardinal);
begin
  inherited;
  EmitParticleQuantityByLoop:=5;
end;

procedure TGSParticleEngine2D_Emiter.Initialize;
var i : Integer;
begin
  inherited;
  For i:=0 to ParticleCount-1 do
  begin
    ParticleList[i].TimeToLiveInMilliSec:=0;
    ParticleList[i].TimeRelevantInMilliSec:=0;
  end;
  TimeManageEnabled:=True;
  ResetTimeEval;
  FinternalCount := 0;
end;

procedure TGSParticleEngine2D_Emiter.Update;
var i : Integer;
    ic : integer;
begin
  inherited;
  if FinternalCount=ParticleCount-1 then
    FinternalCount:=0;

  i:=FinternalCount;
  ic:= 0;

  While (i<ParticleCount) And (ic<EmitParticleQuantityByLoop) do
  begin
    if ParticleList[i].TimeRelevantInMilliSec<=0 then
    begin
      Inc(ic);
      ParticleList[i].TimeToLiveInMilliSec:=ParticleDefaultDurationMilliSec;
      ParticleList[i].TimeRelevantInMilliSec:=ParticleDefaultDurationMilliSec;
      ParticleList[i].TimeStart:=GetTickCount;
      ParticleList[i].X:=Position.X;
      ParticleList[i].Y:=Position.Y;
      FinternalCount:=i;
    end;
    Inc(i);
  end;
end;


{ TGSForce2d }

constructor TGSForce2d.Create;
begin
  vInit(forceVector);
  ForceVector.X := 1;
end;

{ TGSForceList }

procedure TGSForceList.AddForce(Fx, Fy: Double);
var v : TGSForce2d;
begin
  v:=TGSForce2d.Create;
  v.ForceVector.X:=fx;
  v.ForceVector.Y:= fy;
  Add(v);
end;

function TGSForceList.GetForce(index: cardinal): TGSForce2d;
begin
  Result := TGSForce2D(Get(Index));
end;

procedure TGSForceList.SetForce(index: cardinal; const Value: TGSForce2d);
begin
  Put(Index,Value);
end;

{ TGSCollidableList }

procedure TGSCollidableList.AddCollideLine(x, y, x2, y2: Double; EnergyRestitution : Double = 0.70);
begin
  AddCollideShape(TGSCollidable2d_StaticLine.Create(x,y,x2,y2, EnergyRestitution));
end;

procedure TGSCollidableList.AddCollideShape(aCollidable: TGSCollidable2d);
begin
  Assert(assigned(aCollidable));
  Add(aCollidable);
end;

function TGSCollidableList.GetCollidable(Index: Cardinal): TGSCollidable2d;
begin
  result := TGSCollidable2d(get(index));
end;

procedure TGSCollidableList.SetCollidable(Index: Cardinal;
  const Value: TGSCollidable2d);
begin
  Put(Index,Value);
end;

{ TGSCollidable2d_Line }

constructor TGSCollidable2d_StaticLine.Create(ax, ay, ax1, ay1: Double; EnergyRestitution : Double = 0.70);
begin
  inherited Create;
  x1 := ax;
  y1 := ay;
  x2 := ax1;
  y2 := ay1;
  FEnergyRestitut := EnergyRestitution;
end;

{
procedure TGSCollidable2d_StaticLine.Draw(re: ofRenderer);
begin
  re.Line(X1,Y1,X2,Y2);
end;
}

procedure TGSCollidable2d_StaticLine.Resolve(var aParticle: TGSParticle2D);
var ix,iy : double;
    n1,n2 : Double;
    tvx,tvy : Double;
begin
  //We firstly solve velocities vector parameter : Particule and line.
  tvx := TheCadencer.TimeSliceValue(aParticle.VelX);
  tvy := TheCadencer.TimeSliceValue(aParticle.VelY);

  if (tvx<>0) or (tvy<>0) then
  begin
    If vIntersect(x1,y1,x2,y2,aParticle.X,aParticle.Y,aParticle.X+tvx,aParticle.Y+tvy,ix,iy) then
    begin
      //Particle has no mass : then, it will bounce again a line, with a EnergyRestitute parameters.
      //and the line will not be affected.
      vMirror(ix+aParticle.VelX,iy+aParticle.VelY,X1,Y1,X2,Y2,n1,n2);

      aParticle.VelX:=(n1-ix)*FEnergyRestitut;
      aParticle.VelY:=(n2-iy)*FEnergyRestitut;
      aParticle.X:=ix;
      aParticle.Y:=iy;
      aParticle.ColX:=ix;
      aParticle.ColY:=iy;
      aParticle.Collided:=true;
    end;
  end;

end;


end.
