unit FMeX.Gx.Types;


interface

Uses
  System.Types,
  System.Classes,
  System.UITypes,
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  System.Math.Vectors,
  FMX.Types3D,
  FMX.Objects3D,
  FMX.Materials,
  FMX.Graphics,
  FMX.Controls3D,
  FMX.MaterialSources,
  Gx.Types,
  Gx.Graph.Types;

Type
TFMeXGraph3D = class;

TFMeXGraph3D = class(TeGraphElement3d)
protected
public
  procedure Render(acontext : TContext3D; const aFXMControl : TControl3d = nil); virtual;
end;

TeMesh3D = class(TFMeXGraph3D)
private
protected
  FMaterialSource: TMaterialSource;
  FData : TMeshData;

  procedure BuildMesh; virtual;
public
  constructor Create; override;
  destructor destroy; Override;
  //For ressource change, asset modification notification.
  procedure MeshNotification(Sender : TObject); virtual; abstract;

  procedure Render(acontext : TContext3D; const aFXMControl : TControl3d = nil); Override;

  property MaterialSource : TMaterialSource read FMaterialSource write FMaterialSource;
end;


implementation


{ TFMeXeControl3D }

procedure TFMeXGraph3D.Render(acontext: TContext3D; const aFXMControl : TControl3d = nil);
begin
  assert(assigned(aContext));

  if Assigned(aFXMControl) then
    acontext.SetMatrix(localMatrix*aFXMControl.AbsoluteMatrix)
  else
    acontext.SetMatrix(localMatrix);

  acontext.DrawCube(Position,Point3D(1,1,1),1.0,TAlphaColorRec.Blue);
end;


{ TeMesh }

procedure TeMesh3D.BuildMesh;
begin
  FData.BoundingBoxNeedsUpdate;
end;

constructor TeMesh3D.Create;
begin
  inherited;
  FData := TMeshData.Create;
end;

destructor TeMesh3D.destroy;
begin
  FreeAndNil(FData);
  inherited;
end;


procedure TeMesh3D.Render(acontext: TContext3D; const aFXMControl: TControl3d);
var MeshMatrix : TMatrix3D;
begin
  if FData.VertexBuffer.Length>0 then
  begin
    acontext.SetMatrix(localMatrix*aFXMControl.AbsoluteMatrix);
    acontext.DrawTriangles(FData.VertexBuffer,FData.IndexBuffer,MaterialSource.Material,1.0);

    //Draw bounding box.
    aContext.DrawCube( NullPoint3D,
                      Vector3D( Width,
                                Height,
                                Depth),
                      1.0,
                      TAlphaColorRec.Blue);
  end
  else
  begin
    inherited;
  end;
end;

end.
