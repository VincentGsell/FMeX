unit FMeX.Types3D.GeometryCommon;

Interface

uses
  System.SysUtils, System.Types, System.Classes,
  FMX.Types, FMX.Types3d, FMX.GRaphics, System.Math.Vectors,
  FMeX.Types, FMeX.Vectors, FMeX.Types2D.GeometryCommon;

Type

TFMeXCustom3DGeometryGenerator = Class(TFMeXGeometryGenerator)
Protected
  Procedure Process(aData : TMeshData); Virtual; Abstract;
End;



TFMeXCubeGenerator = Class(TFMeXCustom3DGeometryGenerator)
Protected
//  Procedure Process(aData : TMeshData); Override;
End;

TFMeXParalelepipedeGenerator = Class(TFMeXCustom3DGeometryGenerator)

End;

TFMeXSphereGenerator = Class(TFMeXCustom3DGeometryGenerator)
Protected
//  Procedure Process(aData : TMeshData); Override;
End;




implementation



end.
