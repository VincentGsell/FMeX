# FMeX

FMeX (aka Delphi FMX Extended) is a collection of class which aimd to leverage performance and usability of 3D FMX library.

## Goal and target 

For information, Existing Delphi's 3D FMX is a collection of native 3d Control components. This lib is mainly targeting GUI : 3D control have an hierarchic organisation and control rendering pipeline is form-centric. 

FMeX introdude a Proxy Component, which is a standart FMX 3D Control. As Child, it accepts only FMeX components.
This components rendering behaviour is driving by a graph, which rules the global rendering sequence.

As a result :
- Common task are more easy to control and to implement.
- FMeX introduction into a 3D FMX app could leverage somme usual caveat such as descripted below. 

### Rendering performance
FMeX introduce Mesh Merging in order to build a global mesh, which will demand only one (1) draw call to render. This speed up a lot the rendering time.
### Rendering sequence
FMeX introduce Graph method : This permits to control precisely the rendering sequence and solve partially the blending hell in FMX app.
### Opening achitecture
FMeX, with its single ProxyControl and Graph component, open a door on a "masterisable" rendering process. (see Particle demo)

## Main features
- Fully FMX compatible (Entry point : TFMeXProxy, which is FMX TControl3D)
- Introduces TeCustomMesh, with "MergeFrom" method, for merging FMeX Mesh together into on and single Mesh. (Draw Call optimization)
- Show techniques to achieve good performance within FMX, with merging Ticks, and animation
- Introduce Candender, to control precise animation cadence (slow down motion, speed up, back to normal) (see Particle demo)
- Introduce advanced 3D mesh, such has "System light" Image (With Atlas), MetaBalls generation and more to come
- Actualy, as Particle demo show, it simplify drasticaly 2d sequence building and rendering setup.
- [Coming] : Hud system.
- [Coming] : Spine integration.
- [Coming] : Rod and pipe 3d Mesh
- [Coming] : [Angus Johnson's](http://www.angusj.com) clipper, for base 2d extruder.

## Exemple

### Merge Mesh
As a TeCustom.MergeFrom(...) capabilities, see Stress Cube demo. With a less than a middle class usual SoHo desktop PC computer, thousands of cube limits could be reached. It could be a good base for a Cube Voxel Engine. Note that is is a raw rendering, there are no geometric optimization at all. And it need it. :)

![Alt text](/img/StressCubeDemo.png?raw=true "MergeFrom API in action")

### Merge Mesh in a real case
this example implements a 2d particle rendering : Logic side is processed on CPU, and rendering side prepares all Particles in large Merged Mesh, which is manipulated runtime by CPU, and drawing in one "low level" call. (see FMeX.Particles2D.pas)

![Alt text](/img/ParticleDemo.png?raw=true "Particlein action")
