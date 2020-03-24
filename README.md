# FMeX

FMeX (or Delphi FMX Extended) is a set of classes designed to optimize the performance and usability of the FMX library.

## History
- 20200304 - New Perlin 3D voxel Demo
- 202003xx - MASSIVE refacto : Many demo do not work anymore : Temporay. ;)

## Main features
- Introduces TeCustomMesh, with "MergeFrom" method, for merging FMeX Mesh together into on and single Mesh. (Draw Call optimization)
- FMX compatible
- Show techniques to achieve good performance within FMX, with merging Ticks, and animation
- Cadender, to control precise animation cadence (slow down motion, speed up, back to normal) (see Particle demo)

## Dependancy : 

- GS.Core
- Gx (TEMPORARY)

## Exemples

### 3D Perlin's noise Demo.

![Alt text](/img/VoxelsPerlin.png?raw=true "3d Perlin's noise")


### Obj Loader (Work in progress)
Attempt to replace the OBJ FMX loader which has some problem for loading large file (perf(!) and materials behaviour)

![Alt text](/img/Chalet.png?raw=true "Photogrammetry file")

![Alt text](/img/Ramses.png?raw=true "huge model")

### Spine Loader (Work in progress)
Attempt to adapt an existing Spine loader. (esotericsoftware.com)
-> Used cool unit form Dan (dan.soft.studio "_a_t_" gmail.com): 
-> After compile, please put goblins.xxx and goblins-[anim].xxx into binary dir.
This is a test only, and certainly spine had no futur application in FMeX.

![Alt text](/img/spineexpl.png?raw=true "spine goblin model")

### Merged Mesh
As a TeCustom.MergeFrom(...) capabilities, see Stress Cube demo. With a less than a middle class usual SoHo desktop PC computer, thousands of cube limits could be reached. It could be a good base for a Cube Voxel Engine. Note that is is a raw rendering, there are no geometric optimization at all. And it need it. :)

![Alt text](/img/StressCubeDemo.png?raw=true "MergeFrom API in action")

### Merged Mesh in a real case
this example implements a 2d particle rendering : Logic side is processed on CPU, and rendering side prepares all Particles in large Merged Mesh, which is manipulated runtime by CPU, and drawing in one "low level" call. (see FMeX.Particles2D.pas)

![Alt text](/img/ParticleDemo.png?raw=true "Particle in action")
