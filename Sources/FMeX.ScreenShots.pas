unit FMeX.ScreenShots;

interface

uses SysUtils, FMX.Surfaces;

Type
TCustomScreenShots = Class
public
  class function SystemScreenShot : FMX.TBitmapSurface; virtual; abstract;
End;

var ScreenShots : TCustomScreenShots;

implementation

{$IF defined(MSWINDOWS)}
{$ENDIF}
{$IF defined(OSX)}
{$ENDIF}
{$IF defined(Linux)}
{$ENDIF}
{$IF Defined(IOS)}
{$ENDIF}
{$IF Defined(ANDROID)}
uses FMeX.ScreenShots.Android;
{$ENDIF}


Initialization

{$IF defined(MSWINDOWS)}
{$ENDIF}
{$IF defined(OSX)}
{$ENDIF}
{$IF defined(Linux)}
{$ENDIF}
{$IF Defined(IOS)}
{$ENDIF}
{$IF Defined(ANDROID)}
ScreenShots := TScreenShotAndroid.Create;
{$ENDIF}


Finalization;

FreeAndNil(ScreenShots);

end.
