unit FMeX.Utilities.Android;

interface

uses SysUtils,
     Classes,
     System.UITypes,
     FMX.Presentation.Android,
     FMX.Helpers.Android,
     Androidapi.JNI.GraphicsContentViewText,
     AndroidAPI.Helpers;

Type

TAndroidUtility = class
protected
public
  class function getIdentifier(id : string) : integer;
  class function getIdentifierAsString(id : integer) : string;
end;


implementation

{ TAndroidUtility }

//Put string as "normal" android format i.e. "android.R.id.Content", and it will try to get the Id.
{ TODO -oVGS -cAverage : See if TAndroidHelper.GetRessourceId can help better instead of that. }
class function TAndroidUtility.getIdentifier(id: string): integer;
var l : TStringList;
    lname,ldef,ldomain : string;
    i : integer;
begin
  result := TAndroidHelper.GetResourceID(id);
  if result>0 then
    Exit;

  //Decoding.
  l := TStringList.Create;
  l.Delimiter := '.';
  l.DelimitedText := id;
  if l.Count>2 then
  begin
    lname := l[l.Count-1];
    ldef := l[l.Count-2];
    if l.Count=3 then
    begin
      ldomain := l[0];
    end
    else
    begin
      ldomain := '';
      for I := l.Count-3 downto 0 do
        ldomain := l[i] + '.' + ldomain;
      ldomain := Copy(ldomain,1,length(ldomain)-1);
    end;
    result := SharedActivity.getResources.getIdentifier(StringToJString(lname),StringToJString(ldef),StringToJString(ldomain));
  end
  else
    raise Exception.Create('Android Utility / getIdentifier : id format failure (too short) "'+id+'"');
end;

class function TAndroidUtility.getIdentifierAsString(id: integer): string;
begin
  result := TAndroidHelper.GetResourceString(id);
end;

end.
