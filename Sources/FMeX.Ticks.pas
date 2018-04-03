unit FMeX.Ticks;

interface

Uses
  System.SysUtils,
  FMX.Types,
  FMX.Platform;

Function GetTickCount : Cardinal;

Type
  ofCadencer = class(TObject)
  private
    FLastCall : Cardinal;
    FTimeScaler: Double;
    FDeltaTime : Cardinal;
    FTic : Cardinal;
    FCallBySec : Cardinal;
    FInternalCallBySec : Cardinal;
    FMCSec : Cardinal;
    FSecFromStart : Cardinal;
    function GetSystemTick: Cardinal;
  Public
    constructor Create;

    Procedure Update;
    Procedure Reset;

    Function TimeSliceValue(aValueToSlice : Double) : Double;

    property TimeScaler : Double read FTimeScaler Write FTimeScaler;
    Property Tic : Cardinal read FTic;
    Property DeltaTime : Cardinal read FDeltaTime;

    Property CallBySecond : Cardinal read FCallBySec;

  end;


var
  TheCadencer : ofCadencer;

Implementation

var PlatformTimer : IFMXTimerService;

Function GetTickCount : Cardinal;
begin
  Result := Round(PlatformTimer.GetTick * 1000);
end;

 { ofCadencer }

  constructor ofCadencer.Create;
  begin
    Inherited;
    Reset;
    FSecFromStart := 0;
  end;

  function ofCadencer.TimeSliceValue(aValueToSlice: Double): Double;
  begin
    Result := aValueToSlice * FDeltaTime / 1000;  //div by 1000, cause we assume that value is by seconds.
  end;


  procedure ofCadencer.Update;
  var h : cardinal;
  begin
    FTic := GetTickCount;
    FDeltaTime := Trunc((FTic - FLastCall) * FTimeScaler);
    FLastCall := FTic;

    h := Round(Ftic/1000);
    if h<>FMCSec then
    begin
      FMCSec := h;
      FCallBySec := FInternalCallBySec;
      FInternalCallBySec:=0;
      Inc(FSecFromStart);
    end
    else
    begin
      Inc(FInternalCallBySec);
    end;
  end;

 procedure ofCadencer.Reset;
begin
  TimeScaler := 1; //Realtime by Default. 1 sec real world = 1 sec in game.
  FLastCall := GetTickCount;
  FInternalCallBySec := 0;
  FMCSec := 0;
end;

function ofCadencer.GetSystemTick: Cardinal;
begin
  result := Ftic;
end;


Initialization;
  if Not(TPlatformServices.Current.SupportsPlatformService(IFMXTimerService,IInterface(PlatformTimer)) ) then
  begin
    Raise  Exception.Create('Timer not supported on this plateform. Abort.');
  end;

  TheCadencer := ofCadencer.Create;
  TheCadencer.Reset;

finalization

  TheCadencer.Free;

end.
