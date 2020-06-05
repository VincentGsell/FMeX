unit FMeX.ScreenShots.Android;

interface

uses SysUtils,
     Classes,
     System.UITypes,
     FMX.Graphics,
     FMX.Types,
     FMX.Surfaces,
     FMX.Presentation.Android,
     FMX.Helpers.Android,
     Androidapi.JNI.GraphicsContentViewText,
     AndroidAPI.Helpers,
     FMeX.ScreenShots,
     FMeX.Utilities.Android;

type

TScreenShotAndroid = class(TcustomScreenShots)
protected
public
  class function SystemScreenShot : FMX.TBitmapSurface; override;
end;


implementation

procedure SurfaceAlphaCorrector(surface : TBitmapSurface);
var
  Index: Integer;
  Dest: Pointer;
  col : TAlphaColorRec;
begin
  if (surface.Width < 1) or (surface.Height < 1) or (surface.Bits = nil) or (surface.BytesPerPixel = 4) then
    Exit;
  Dest := surface.Bits;
  for Index := 0 to (surface.Width * surface.Height) - 1 do
  begin
    PAlphaColorRec(Dest)^.A := 255;
    Inc(NativeInt(Dest), surface.BytesPerPixel);
  end;
end;

{ TScreenShotAndroid }


{ TScreenShotAndroid }

class function TScreenShotAndroid.SystemScreenShot: FMX.TBitmapSurface;
var v : JView;
    rid : integer;
begin
  rid := TAndroidUtility.getIdentifier('android.id.content');
  if rid=0 then
    rid := TAndroidUtility.getIdentifier('android.R.id.content');

  if rid>0 then
  begin
    TAndroidHelper.Activity
    v := sharedActivity.getWindow.getDecorView.findViewById(rid).getRootView;
    result := TBitmapSurface.Create;
    NativeViewToSurface(v,Result);
    SurfaceAlphaCorrector(Result);
  end
  else
    raise Exception.Create('System Screen Shot : No screen access');
end;

end.
