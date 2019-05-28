unit FMeX.HUD;

interface

uses Classes, System.Math.Vectors, Sysutils,
     System.Generics.Collections,
     System.Types,
     System.UITypes,
     FMX.Types3D,
     FMX.Controls3D,
     FMX.Controls,
     FMX.Objects,
     FMeX.Images;


Type

THudShape = Class;
THudlink = Class;
THudControler = Class;

//Class to Keep mouse and Input status for each HudShape in the controler.
THudChildInfo = Class
  ChildInfo : Boolean;
  HoldData_MouseLeftButton : Boolean;
  HoldData_DragAndDropInProgress : Boolean;
  MouseLeftButtonFromOutside : Boolean;
  Constructor Create;
End;


//This one manage all the hud, dispatch the input event and make the rendering.
//As an HUD system, all the rendering is over all the other stuff : You have to call
//manually the render in OnRender Event of the form : This is because the internal render system of FMX.
THudControler = Class
private
  FContext : TContext3D;

  FOnClick: TNotifyEvent;
  FOnMouseMove: TNotifyEvent;

  //saMouseAbility variable
  FHoldData_MouseLeftButton : Boolean;

  //saDragAndDropAbility variables.
  FDragDropHoldData_MouseLeftButton : Boolean;
  FDragDropHoldData_MouseLeftButtonDownCoord : TPointf;
  FDragDropHoldData_MouseCaptured : THudShape;
  FDragDropHoldData_TargetMouseDownPos : TPoint3d;
  FDragDropHoldData_TargetIntersectionPos : TPoint3d;



  FOnMouseEnter: TNotifyEvent;
  FOnMouseLeft: TNotifyEvent;
  FOnMouseDown: TNotifyEvent;
  FOnMouseUp: TNotifyEvent;

  FChildrenInfo : TList<THudChildInfo>;
  Children : TList<THudShape>;
  FOnDragAndDrop: TNotifyEvent;
  FEnabled: Boolean;
  FWorkInProgress: Boolean;
  FReference: TControl3d;

  function GetHud(Index: Integer): THudShape;

  Procedure Internal_MouseManagement(a : THudShape; Index : Integer);
  Procedure Internal_DragAndDropManagement(a : THudShape; Index : Integer);
    function GetWorkInProgress: Boolean;

Public

  InData_MousePos : TPointf;
  InData_MouseLeftButton : Boolean;

  Procedure Process; Virtual;

  Procedure Render; Virtual;

  Procedure AddHud(aHud : THudShape);
  Procedure RemoveHud(aHud : ThudShape);

  Constructor Create(Acontext : TContext3D); Reintroduce; Virtual;
  Destructor Destroy; Override;

  Property Hud[Index : Integer] : THudShape read GetHud;
Published

  Property OnMouseDown : TNotifyEvent read FOnMouseDown Write FOnMouseDown;
  Property OnMouseMove : TNotifyEvent read FOnMouseMove Write FOnMouseMove;
  Property OnMouseUp : TNotifyEvent read FOnMouseUp Write FOnMouseUp;
  Property OnMouseEnter : TNotifyEvent read FOnMouseEnter Write FOnMouseEnter;
  Property OnMouseLeft : TNotifyEvent read FOnMouseLeft Write FOnMouseLeft;
  Property OnClick : TNotifyEvent read FOnClick Write FOnClick;
  Property OnDragAndDrop : TNotifyEvent read FOnDragAndDrop Write FOnDragAndDrop;

  Property Enabled : Boolean read FEnabled;
  Property WorkInProgress : Boolean read GetWorkInProgress;

  //Reference object for DragAndDrop (DragandDrop reference Plane);
  Property ReferenceObject : TControl3d read FReference Write FReference;
End;

THudControlerComponent = Class(TControl3D)
Public
  Hud : THudControler;
  Procedure Render; Override;
End;

THudShapeAbility = (saMouseAbility, saDragAndDropAbility);
THudShapeAbilities = Set of THudShapeAbility;

THudShape = Class
private
  FLeft: Single;
  FWidth: Single;
  FTop: Single;
  FHeight: Single;
  FAColor: TAlphaColor;
  FAbility: THudShapeAbilities;
  FTarget: TObject;
  FTagObject: TObject;
  FTag: NativeInt;
  FTargetAutoClave: Boolean;
  FAutoclaveMinimuSize: Single;
  FSlave: Tcontrol3D;
  FVisible: Boolean;
  FParent: THudShape;
  FOffset: TPointf;
  FParentHud: ThudShape;
  FDrawAutoclave: Boolean;
  FOnAfterRender: TNotifyEvent;
Public
  HudLinks : TList<THudLink>;
  Function IsOver(aPoint : TPointf) : Boolean; Virtual;
  Procedure Render(aContext : TContext3D); Virtual;
  Procedure DoAfterRender; Virtual;

  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Procedure AddLinksWith(aHudShape : THudShape);

Published
  Property Left : Single read FLeft Write FLeft;
  Property Top : Single read FTop Write FTop;
  Property Width : Single read FWidth Write FWidth;
  Property Height : Single read FHeight Write FHeight;
  Property BaseColor : TAlphaColor read FAColor Write FAColor;
  Property Ability : THudShapeAbilities read FAbility Write FAbility;
  Property Visible : Boolean read FVisible write FVisible;
  Property DrawAutoclave : Boolean read FDrawAutoclave Write FDrawAutoclave;

  //If you assign a control to Target; the shape will fellow this target.
  //--> DragAndDrop Ability : The Target Control will fellow the HUD control with the standart plan (origin : Target position,  .
  Property Target : TObject read FTarget Write FTarget;

  //If true, the size of shape will match the screen bound of the control...
  Property TargetAutoclave : Boolean read FTargetAutoclave Write FTargetAutoclave;
  //...with a minimumsize of TargetMinimumSize.
  Property TargetAutoclaveMinimumSize : Single read FAutoclaveMinimuSize Write FAutoclaveMinimuSize;

  Property TagObject : TObject read FTagObject Write FTagObject;
  Property Tag : NativeInt read FTag Write FTag;

  //By default, HUDShape appear on 0,0 above the target object : This will shift your hud by givent offset
  //In the case of a Parent HUD, this will be applyed too.
  Property Offset : TPointf read FOffset write FOffset;

  Property ParentHud : ThudShape read FParentHud Write FPArentHud;

  Property OnAfterRender : TNotifyEvent read FOnAfterRender Write FOnAfterRender;

End;

//Todo : relocalize this stuff : Just a simple class to dray a ime on a 3d context, without hassle.
//use perhaps 2d design stuff,
T2dImage = Class
Public
  Data : TMeshData;
  Configuration : TeCustomMeshTextureConfiguration;

  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Procedure Paint(aContext : TContext3D; X,Y,Width,Height : Double; Const Opacity : Double = 1.0);
End;

//Display image always front of the camera, render in 2d.
THudImage = Class(ThudShape)
private
protected
  FOpacity: Single;
Public
  Image : T2dImage;

  Constructor Create; Override;
  Destructor Destroy; Override;

  Procedure Render(aContext : TContext3D); Override;

  Property Opacity : Single read FOpacity Write FOpacity;
  { TODO -oVGS -cDisplay : PRoportinal display autoclave. }
End;

//Display image form FMX 2d control (TImage) alway front of the camera, same as before but for Viwport use.
THudImageControl = class(THudShape)
private
  FReference: TImage; //Warning : Is is here a FMX 2dControl ! (for Hud) - THudImage not render corretly (on Tokyo) (transparence) in standart use.
Public
  Procedure DoAfterRender; Override;
  Property Reference :  TImage read FReference Write FReference;
end;


//This class manage virtual link between shape.
//in FMX, or other grah system, it is often hierarchical view : Here, it is just "link".
//The nature af the link will determine the nature of behaviour (i.e. movement) of the shape.
//For example, a static link will keep the same distance and ayimut between to given shape, whatever you move the first or the second.
THudlink = Class
private
  FB: ThudShape;
  FA: THudShape;
Public
  Procedure Process(From : THudShape); Virtual; Abstract;

  Property A : THudShape read FA write FA;
  property B  : ThudShape read FB write FB;
End;

THudStaticLink = Class(THudLink)
private
  FX: Single;
  FY: Single;
Public
  Constructor Create; Virtual;

  Procedure Process(From : THudShape); Override;

  property X_OffSetBetweenAnB : Single read FX Write FX;
  property Y_OffSetBetweenAnB : Single read FY Write FY;
End;



implementation

{ THudControler }

procedure THudControler.RemoveHud(aHud: ThudShape);
var i : integer;
    aHInfo : THudChildInfo;
begin
  i := Children.IndexOf(aHud);
  if i>-1 then
  begin
    Children.Delete(i);
    aHInfo := FChildrenInfo[i];
    FChildrenInfo.Delete(i);
    //FreeAndNil(aHud); //Hud must be kept : App responsability.
    FreeAndNil(aHInfo);
  end;
end;

procedure THudControler.Render;
var a : THudShape;
begin
//    FContext.SetContextState(TContextState.csZWriteOn);
//    FContext.SetContextState(TContextState.csZTestOn);
//    end
//    else
//    begin
//      Context.SetContextState(TContextState.csZWriteOff);
//      Context.SetContextState(TContextState.csZTestOff);
//    end;
//    if Projection = TProjection.Camera then
//      Context.SetContextState(TContextState.cs3DScene)
//    else
     FContext.SetContextState(TContextState.cs2DScene);
//    if TwoSide then
//      Context.SetContextState(TContextState.csAllFace)
//    else
//     FContext.SetContextState(TContextState.csFrontFace);

//    if Opaque then
//      Context.SetContextState(TContextState.csAlphaBlendOff)
//    else
//      Context.SetContextState(TContextState.csAlphaBlendOn);
//    Render;

//  FContext.ResetStates;
//  FContext.SetContextState(TContextState.cs2DScene);
//  FContext.SetContextState(TContextState.csZTestOff);
//  FContext.SetContextState(TContextState.csZWriteOff);
//  FContext.SetContextState(TContextState.csStencilOff);
//  FContext.SetContextState(TContextState.csColorWriteOff);
//  FContext.SetContextState(TContextState.csScissorOff);
//  FContext.SetContextState(TContextState.csAlphaBlendOn);
  for a in Children do
  begin
    if Not(a.Visible) then
      Continue;
    a.Render(FContext);
    a.DoAfterRender;
  end;
end;

procedure THudControler.AddHud(aHud: THudShape);
begin
  Children.Add(aHud);
  FChildrenInfo.Add(THudChildInfo.Create);
end;

constructor THudControler.Create(aContext : TContext3D);
begin
  Inherited Create;
  Assert(Assigned(aContext));
  FContext := aContext;

  FHoldData_MouseLeftButton := False;
  FDragDropHoldData_MouseLeftButton := False;
  FDragDropHoldData_MouseCaptured := Nil;

  InData_MouseLeftButton := False;
  Children := TList<THudShape>.Create;
  FChildrenInfo := TList<THudChildInfo>.Create;
  FEnabled := True;
  FWorkInProgress := False;
  FReference := Nil;
end;

destructor THudControler.Destroy;
var a : THudShape;
begin
  for a in Children do
  begin
    a.Free;
  end;
  FreeAndNil(Children);
  inherited;
end;

function THudControler.GetHud(Index: Integer): THudShape;
begin
  Result := Children[Index];
end;

function THudControler.GetWorkInProgress: Boolean;
begin
  Result := FWorkInProgress Or Assigned(FDragDropHoldData_MouseCaptured);
end;

procedure THudControler.Internal_DragAndDropManagement(a: THudShape;
  Index: Integer);
var P : TPointf;
    I : TPoint3D;
    RayPos, RayDir : TVector3d;

    ab : TControl3D;
//    ac : TFAObject;

    cc : THudlink;
begin
  //Basically, DragAndDrop is the same event of mouses ones : With some change for mouse capture and so on.
  if (FDragDropHoldData_MouseCaptured<>Nil) and
     (FDragDropHoldData_MouseCaptured<>a) then
    Exit;

  if FChildrenInfo[index].MouseLeftButtonFromOutside then
    Exit;

  If FDragDropHoldData_MouseCaptured=a then
  begin
    FWorkInProgress := True;
    FChildrenInfo[index].HoldData_DragAndDropInProgress := true;
    a.Left := InData_MousePos.X-FDragDropHoldData_MouseLeftButtonDownCoord.X;
    a.top := InData_MousePos.Y-FDragDropHoldData_MouseLeftButtonDownCoord.Y;

    for cc in a.HudLinks do
    begin
      if cc.B = a then
      begin
        if cc is THudStaticLink then
        begin
          THudStaticLink(cc).X_OffSetBetweenAnB := a.Left - cc.A.Left;
          THudStaticLink(cc).Y_OffSetBetweenAnB := a.Top - cc.A.Top;
        end;
      end;


      cc.Process(a);
    end;

    if Assigned(FOnDragAndDrop) then
    begin
      FOnDragAndDrop(a);
    end;


      //Slave : Follow the HUD durring dragAnddrop. It has the target behaviours else.
      if Assigned(a.Target) then
      begin

        if a.Target is TControl3D then
        begin
          ab := TControl3D(a.Target);

          P := TPointF.Create(a.Left+a.Width/2, a.Top+a.Height/2);
          FContext.Pick(P.X, P.Y, TProjection.Camera, RayPos, RayDir);

          if Assigned(FReference) then
          begin


            //Drag and drop on an objet reference (Plane, grid whatever)
            RayPos := TPoint3D(FReference.AbsoluteToLocalVector(RayPos));
            RayDir := FReference.AbsoluteToLocalDirection(RayDir);

            if FReference.RayCastIntersect(RayPos,RayDir,I) then
            begin
              I := TPoint3D(FReference.LocalToAbsoluteVector(I));
              //I.Z := ab.Position.Z; //Save Z.
              ab.Position.Point := I; //FDragDropHoldData_TargetMouseDownPos + (I-FDragDropHoldData_TargetIntersectionPos);
            end
          end
          else
          begin
            //Drag and drop on the objet itself : It will follow its "direction" (if you rotate the object, "direction" will apparently differs)
            //this on is cool when you not rotate the object
            RayPos := TPoint3D(ab.AbsoluteToLocalVector(RayPos));
            RayDir := ab.AbsoluteToLocalDirection(RayDir);
            if RayCastPlaneIntersect(RayPos,RayDir,TPoint3D.Create(0, 0, 0), TPoint3D.Create(0,0, -1), I) then
            begin
              I := TPoint3D(ab.LocalToAbsoluteVector(I));
              //I.Z := a.Target.Position.Z; //Save Z.
              ab.Position.Point := I; //FDragDropHoldData_TargetMouseDownPos + (I-FDragDropHoldData_TargetIntersectionPos);
            end;
          end;
        end
        else
        begin
{
          ac := TFAObject(a.Target);

          P := TPointF.Create(a.Left+a.Width/2, a.Top+a.Height/2);
          FContext.Pick(P.X, P.Y, TProjection.Camera, RayPos, RayDir);

          RayPos := TPoint3D(ac.AbsoluteToLocalVector(RayPos));
          RayDir := ac.AbsoluteToLocalDirection(RayDir);

          if RayCastPlaneIntersect(RayPos,RayDir,TPoint3D.Create(0, 0, 0), TPoint3D.Create(0, 0, -1), I) then
          begin
            I := TPoint3D(ac.LocalToAbsoluteVector(I));
            //I.Z := a.Target.Position.Z; //Save Z.
            ac.X := I.X; //FDragDropHoldData_TargetMouseDownPos + (I-FDragDropHoldData_TargetIntersectionPos);
            ac.Y := I.Y; //FDragDropHoldData_TargetMouseDownPos + (I-FDragDropHoldData_TargetIntersectionPos);
            ac.Z := I.Z; //FDragDropHoldData_TargetMouseDownPos + (I-FDragDropHoldData_TargetIntersectionPos);

          end;
}
        end;
      end;

  end;


  if InData_MouseLeftButton then
  begin

    if FDragDropHoldData_MouseLeftButton<>InData_MouseLeftButton then
    begin
      if (a.IsOver(InData_MousePos)) And (FDragDropHoldData_MouseCaptured = Nil) then
      begin
        //Get mousedelta
        FDragDropHoldData_MouseLeftButtonDownCoord := InData_MousePos - Pointf(a.Left,a.Top);
        FDragDropHoldData_MouseLeftButton := InData_MouseLeftButton;
        FDragDropHoldData_MouseCaptured := a;
      end;
    end
    else
    begin
      //Mouse move.
    end;
  end
  else
  begin
    FDragDropHoldData_MouseLeftButton := False;
    FChildrenInfo[index].HoldData_DragAndDropInProgress := False;
    FDragDropHoldData_MouseCaptured := Nil;
  end;

end;

procedure THudControler.Internal_MouseManagement(a: THudShape; Index : Integer);
begin
  //Hoovering.
  if a.IsOver(InData_MousePos) then
  begin
    if FChildrenInfo[index].MouseLeftButtonFromOutside then
      Exit;

    if Not(FChildrenInfo[index].ChildInfo) then
    begin
      FChildrenInfo[index].ChildInfo := True;
      if Assigned(FOnMouseEnter) then
      begin
        FOnMouseEnter(a);
      end;
    end;

    if FChildrenInfo[index].HoldData_MouseLeftButton<>InData_MouseLeftButton then
    begin
      if InData_MouseLeftButton then
      begin
        if Assigned(FOnMouseDown) then
        begin
          FOnMouseDown(a);
        end;
      end
      Else
      begin

        if FChildrenInfo[index].HoldData_MouseLeftButton and not(InData_MouseLeftButton) and not(FChildrenInfo[index].HoldData_DragAndDropInProgress)  then //State change.
        begin
          if Assigned(FOnMouseUp) then
          begin
            FOnMouseUp(a);
          end;

          if Assigned(FOnClick) then
          begin
            FOnClick(a);
          end;
        end;

      end;

      FChildrenInfo[index].HoldData_MouseLeftButton := InData_MouseLeftButton;
    end;

    if Assigned(FOnMouseMove) then
    begin
      FOnMouseMove(a);
    end;

  end
  else
  begin
    if FChildrenInfo[Index].ChildInfo then
    begin
      if Assigned(FOnMouseLeft) then
      begin
        FOnMouseLeft(a);
      end;
      FChildrenInfo[Index].ChildInfo := False;
    end;
    FChildrenInfo[index].HoldData_MouseLeftButton := False;
    if Not Assigned(FDragDropHoldData_MouseCaptured) then
      FChildrenInfo[index].MouseLeftButtonFromOutside := InData_MouseLeftButton;
  end;

end;

procedure THudControler.Process;
var a : THudShape;
    index : Integer;

    p : TPoint3D;
    r : TRectF;
    sCorrect : Single;

    RayPos, RayDir : TVector3d;
    ab : TControl3d;
    I : TPoint3D;
    pm : TPointf;

    cc : THudlink;
begin
  FWorkInProgress := False;
  if not(FEnabled) then
    Exit;


  //*****************************
  //Mouse management (Ability)
  //*****************************
  index := 0;
  for a in Children do
  begin
    if a.Visible then
    begin
      if InData_MousePos.X >-1 then //-1 seems to be mouse outside main form.
      begin

        if saMouseAbility in a.Ability  then
        begin
            Internal_MouseManagement(a,Index);
        end;

        if saDragAndDropAbility in a.Ability then
        begin
          Internal_DragAndDropManagement(a,Index);
        end;

      end;
    end;

    inc(index);

  end;


  //*****************************
  //Visual correction (Autoclave or not)
  //*****************************
  for a in Children do
  begin
    if Not(a.Visible) then
      Continue;

    if Assigned(a.Target) then
    begin
      if a.TargetAutoclave then
      begin
        if a.Target is TControl3D then
          r := TControl3D(a.Target).ScreenBounds;
//        else
//          r := TFAObject(a.Target).ScreenBounds;
        a.Left := r.Left;
        a.Top := r.Top;
        a.Width := r.Width;
        a.Height := r.Height;


        //Autoclave is lower than limit, we calculate the direct projection of object on screen...
        //...And make correction of size and center. Here for X axis...
        if (a.Width<a.TargetAutoclaveMinimumSize) then
        begin
          sCorrect := Abs((a.Width-a.TargetAutoclaveMinimumSize)/2);
          a.Left := a.Left - sCorrect;
          a.Width := a.Width + sCorrect;
        end;

        //... And here for Y axis.
        if (a.Height<a.TargetAutoclaveMinimumSize) then
        begin
          sCorrect := Abs((a.Height-a.TargetAutoclaveMinimumSize)/2);
          a.Top := a.Top - sCorrect;;
          a.Height := a.Height + sCorrect;
        end;

        //aContext.DrawRect(point3d(r.TopLeft.X,r.TopLeft.Y,0),point3d(r.BottomRight.X,r.BottomRight.Y,0),1.0,TAlphaColorRec.Green);
      end
      else
      begin
        if a.Target is TControl3D then
          p := FContext.WorldToScreen(TProjection.Camera,TControl3d(a.Target).Position.Point);
//        else
//          p := FContext.WorldToScreen(TProjection.Camera,Point3d(TFAObject(a.Target).X,TFAObject(a.Target).Y,TFAObject(a.Target).Z));

        a.Left := p.X-a.Width/2 + a.Offset.X;
        a.Top := p.Y-a.Height/2 + a.Offset.Y;
        //aContext.DrawRect(point3d(p.X-5,p.Y-5,0),point3d(p.X+5,p.Y+5,0),1.0,TAlphaColorRec.Green);
      end;
    end;
  end;

  //*****************************
  //ParentHUD management.
  //*****************************
  for a in Children do
  begin
    if Assigned(a.ParentHUD) then
    begin
      a.Left := a.ParentHUD.Left + a.Offset.X;
      a.Top := a.ParentHUD.Top + a.Offset.Y;

      if Assigned(a.Target) then
      begin

        if a.Target is TControl3d then
        begin
          a.Top := a.ParentHUD.Top + a.ParentHUD.Height; //StuckUnder ParentHUD
          ab := TControl3d(a.Target);
          Pm := TPointF.Create(a.Left+a.Width/2, a.Top+a.Height/2);
          FContext.Pick(Pm.X, Pm.Y, TProjection.Camera, RayPos, RayDir);

          RayPos := TPoint3D(ab.AbsoluteToLocalVector(RayPos));
          RayDir := ab.AbsoluteToLocalDirection(RayDir);

          if RayCastPlaneIntersect(RayPos,RayDir,TPoint3D.Create(0, 0, 0), TPoint3D.Create(0,0, -1), I) then
          begin
            I := TPoint3D(ab.LocalToAbsoluteVector(I));
            //I.Z := a.Target.Position.Z; //Save Z.
            ab.Position.Point := I; //FDragDropHoldData_TargetMouseDownPos + (I-FDragDropHoldData_TargetIntersectionPos);
          end;
        end;
      end;
    end;
  end;
end;

{ THudShape }

procedure THudShape.AddLinksWith(aHudShape: THudShape);
var a : THudStaticLink;
begin
  Assert(Assigned(aHudShape));
  Assert(aHudShape<>self);
  a := THudStaticLink.Create;
  a.A := Self;
  a.B := aHudShape;
  HudLinks.Add(a);
  aHudShape.HudLinks.Add(a);

  a.Process(Self);
end;

constructor THudShape.Create;
begin
  Inherited;
  HudLinks := TList<THudLink>.Create;

  FAColor := TAlphaColorRec.Red;
  Top := 0;
  Left := 0;
  Width := 100;
  Height := 100;
  FAbility := [saMouseAbility];
  FTargetAutoclave := True;
  FAutoclaveMinimuSize := 10;
  FVisible := True;
  FTarget := Nil;
  FTag := -1;
  FTagObject := Nil;
  FOffset := pointf(0,0);
  FParent := Nil;
  FDrawAutoclave := true;
end;

destructor THudShape.Destroy;
var a : THudlink;
begin
  for a in HudLinks do
  begin
    a.Free;
  end;

  HUDLinks.Clear;
  FreeAndNil(HUDLinks);

  inherited;
end;

function THudShape.IsOver(aPoint: TPointf): Boolean;
begin
  Result := (aPoint.X>=Left) And (aPoint.Y>=Top) And
            (aPoint.X<=Left+Width) And (aPoint.Y<=Top+Height);
end;

procedure THudShape.DoAfterRender;
begin
  //Use this if you need to make operation on shape level after rendering (such as control dependancy, or external display
  If Assigned(FOnAfterRender) then
  begin
    FOnAfterRender(Self);
  end;
end;

procedure THudShape.Render(aContext: TContext3D);
begin
  if FDrawAutoclave then
    aContext.DrawRect(point3d(Left,Top,0),point3d(left+width,top+height,0),1.0,FAColor);
end;


{ THudChildInfo }

constructor THudChildInfo.Create;
begin
  Inherited Create;
  HoldData_MouseLeftButton := False;
  HoldData_DragAndDropInProgress := False;
  ChildInfo := False;
  MouseLeftButtonFromOutside := False;
end;


{ THudStatic }

constructor THudStaticLink.Create;
begin
  Inherited;
  FX := 50;
  FY := 50;
end;

procedure THudStaticLink.Process(From : THudShape);
var cc : THudlink;
begin
  //A is self.
  if From = A then
  begin
    b.Left := a.Left + FX;
    b.Top := a.Top + FY;

    for cc in b.HudLinks do
    begin
      if (cc.A <> From) And (cc.B <> From) then
      begin
        cc.Process(b);
      end;
    end;

  end
  else
  begin
//    a.Left := b.Left - FX;
//    a.Top := b.Top - FY;

//    for cc in a.HudLinks do
//    begin
//      if (cc.A <> From) And (cc.B <> From) then
//      begin
//        cc.Process(a);
//      end;
//    end;

  end;
end;

{ THudImage }

constructor THudImage.Create;
begin
  inherited;
  Image := T2dImage.Create;
  FOpacity := 1.0;
end;

destructor THudImage.Destroy;
begin
  FreeAndNil(Image);
  inherited;
end;

procedure THudImage.Render(aContext: TContext3D);
begin
  inherited;
  Image.Paint(aContext,Left,Top,Width,Height,FOpacity);
end;

{ T2dImage }

constructor T2dImage.Create;
begin
  Configuration := TeCustomMeshTextureConfiguration.Create;
  Configuration.TextureData := TeTextureAtlas.Create;
  Configuration.TextureData.AddItem('Full',0,0,1,1);
  Configuration.TextureItemIndex := 0;

  Data := TMeshData.Create;
  Data.VertexBuffer.Length := 4;
  Data.IndexBuffer.Length := 6;

  Data.VertexBuffer.Vertices[0] := Point3D(-0.5, -0.5, 0);
  Data.VertexBuffer.Vertices[1] := Point3D(0.5, -0.5, 0);
  Data.VertexBuffer.Vertices[2] := Point3D(0.5, 0.5, 0);
  Data.VertexBuffer.Vertices[3] := Point3D(-0.5, 0.5, 0);

  Data.VertexBuffer.TexCoord0[0] := PointF(0, 0);
  Data.VertexBuffer.TexCoord0[1] := PointF(1, 0);
  Data.VertexBuffer.TexCoord0[2] := PointF(1, 1);
  Data.VertexBuffer.TexCoord0[3] := PointF(0, 1);

  Data.IndexBuffer[0] := 0;
  Data.IndexBuffer[1] := 1;
  Data.IndexBuffer[2] := 2;
  Data.IndexBuffer[3] := 2;
  Data.IndexBuffer[4] := 3;
  Data.IndexBuffer[5] := 0;
end;

destructor T2dImage.Destroy;
begin
  FreeAndNil(Configuration);
  FreeAndNil(Data);
  inherited;
end;

procedure T2dImage.Paint(aContext : TContext3D; X,Y,Width,Height : Double; Const Opacity : Double);
begin
  if Assigned(Configuration.TextureData.MaterialSource) then
  begin
    Data.VertexBuffer.Vertices[0] := Point3D(x,y, 0);
    Data.VertexBuffer.Vertices[1] := Point3D(x+Width,y, 0);
    Data.VertexBuffer.Vertices[2] := Point3D(x+Width,y+Height, 0);
    Data.VertexBuffer.Vertices[3] := Point3D(x,y+Height, 0);

    aContext.SetContextState(TContextState.csAlphaBlendOn);

    aContext.DrawTriangles(Data.VertexBuffer, Data.IndexBuffer, Configuration.TextureData.MaterialSource.Material, Opacity);
  end;

end;

{ THudControlerComponent }

procedure THudControlerComponent.Render;
begin
  inherited;
  if Assigned(Hud) then
  begin
    Hud.Process;
    Hud.Render;
  end;
end;

{ THudImage3D }

procedure THudImageControl.DoAfterRender;
begin
  //resize image on autoclave.
  if Assigned(Target) And (Target is TControl3D) And (assigned(Reference)) then
  begin
    Reference.Position.X := Left;
    Reference.Position.Y := Top;
    Reference.Width := Width;
    Reference.Height := Height;
  end;
  Inherited DoAfterRender; //Event propagation
end;

end.
