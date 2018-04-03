//
//MetaBall : implementation found in Carad 2004 - Martin Waldegger.

unit FMeX.Types3D.MetaBall;

interface

uses
  System.SysUtils, System.Types, System.Variants, System.UITypes,
  System.Classes, FMX.Types, FMX.Dialogs, FMX.Types3D, FMX.Forms,
  FMX.Materials, FMX.Objects3d, FMeX.Types3d, System.Math.Vectors;

Const
  MAXMETABALLS = 1024;
Type
  // Colors.
  TColor4f = record
    r: single;
    g: Single;
    b: Single;
    a: Single;
  end;
  PColor4f = ^TColor4f;
  TColors4f = array[0..9999] of TColor4f;
  PColors4f = ^TColors4f;

  TVecs = array[0..9999] of TVector3D;
  PVecs = ^TVecs;


  // spherical metaball elements
  TMetaSphere = record
    Center: TVector3D;         // center of sphere
    Radius: Single;            // radius
    SquareRad: Single;         // radius^2
    Color: TColor4f;           // color (or other) values
  end;
  PMetaSphere = ^TMetaSphere;
  TMetaSpheres = array[0..MAXMETABALLS-1] of TMetaSphere;
  PMetaSpheres = ^TMetaSpheres;

  // grid points for marching cube algorithm
  TGridPoint = record
    Position: TVector3D;            // grid position
    Value: Single;              // result of metaball equation at this position
  end;
  PGridPoint = ^TGridPoint;
  TGridPoints = array[0..9999] of TGridPoint;
  PGridPoints = ^TGridPoints;

  // a "marching cube"
  TGridCell = record
    P: array[0..7] of PGridPoint;  // 8 points (pointers into grid points) forming a cube
  end;
  PGridCell = ^TGridCell;
  TGridCells = array[0..9999] of TGridCell;
  PGridCells = ^TGridCells;

  {$REGION Ressources}

{
  There are 256 possible ways for the metaball surface to intersect a cube. The
  EDGETABLE is used to look up edges that are intersected by the metaball,
  starting from the vertices between which the surface is known to pass.
  When edges are known to intersect the surface, TRITABLE is used to form
  triangles from the intersection points.

  These tables were taken from Paul Bourke's site:
    http://www.swin.edu.au/astronomy/pbourke/modelling/
    http://paulbourke.net/geometry/implicitsurf/


}

const
  EDGETABLE: array [0..255] of Integer = (
    $0  , $109, $203, $30a, $406, $50f, $605, $70c,
    $80c, $905, $a0f, $b06, $c0a, $d03, $e09, $f00,
    $190, $99 , $393, $29a, $596, $49f, $795, $69c,
    $99c, $895, $b9f, $a96, $d9a, $c93, $f99, $e90,
    $230, $339, $33 , $13a, $636, $73f, $435, $53c,
    $a3c, $b35, $83f, $936, $e3a, $f33, $c39, $d30,
    $3a0, $2a9, $1a3, $aa , $7a6, $6af, $5a5, $4ac,
    $bac, $aa5, $9af, $8a6, $faa, $ea3, $da9, $ca0,
    $460, $569, $663, $76a, $66 , $16f, $265, $36c,
    $c6c, $d65, $e6f, $f66, $86a, $963, $a69, $b60,
    $5f0, $4f9, $7f3, $6fa, $1f6, $ff , $3f5, $2fc,
    $dfc, $cf5, $fff, $ef6, $9fa, $8f3, $bf9, $af0,
    $650, $759, $453, $55a, $256, $35f, $55 , $15c,
    $e5c, $f55, $c5f, $d56, $a5a, $b53, $859, $950,
    $7c0, $6c9, $5c3, $4ca, $3c6, $2cf, $1c5, $cc ,
    $fcc, $ec5, $dcf, $cc6, $bca, $ac3, $9c9, $8c0,
    $8c0, $9c9, $ac3, $bca, $cc6, $dcf, $ec5, $fcc,
    $cc , $1c5, $2cf, $3c6, $4ca, $5c3, $6c9, $7c0,
    $950, $859, $b53, $a5a, $d56, $c5f, $f55, $e5c,
    $15c, $55 , $35f, $256, $55a, $453, $759, $650,
    $af0, $bf9, $8f3, $9fa, $ef6, $fff, $cf5, $dfc,
    $2fc, $3f5, $ff , $1f6, $6fa, $7f3, $4f9, $5f0,
    $b60, $a69, $963, $86a, $f66, $e6f, $d65, $c6c,
    $36c, $265, $16f, $66 , $76a, $663, $569, $460,
    $ca0, $da9, $ea3, $faa, $8a6, $9af, $aa5, $bac,
    $4ac, $5a5, $6af, $7a6, $aa , $1a3, $2a9, $3a0,
    $d30, $c39, $f33, $e3a, $936, $83f, $b35, $a3c,
    $53c, $435, $73f, $636, $13a, $33 , $339, $230,
    $e90, $f99, $c93, $d9a, $a96, $b9f, $895, $99c,
    $69c, $795, $49f, $596, $29a, $393, $99 , $190,
    $f00, $e09, $d03, $c0a, $b06, $a0f, $905, $80c,
    $70c, $605, $50f, $406, $30a, $203, $109, $0);

  TRITABLE: array [0..255, 0..15] of Integer = (
    (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 8, 3, 9, 8, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 2, 10, 0, 2, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 8, 3, 2, 10, 8, 10, 9, 8, -1, -1, -1, -1, -1, -1, -1),
    (3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 11, 2, 8, 11, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 9, 0, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 11, 2, 1, 9, 11, 9, 8, 11, -1, -1, -1, -1, -1, -1, -1),
    (3, 10, 1, 11, 10, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 10, 1, 0, 8, 10, 8, 11, 10, -1, -1, -1, -1, -1, -1, -1),
    (3, 9, 0, 3, 11, 9, 11, 10, 9, -1, -1, -1, -1, -1, -1, -1),
    (9, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 3, 0, 7, 3, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 9, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 1, 9, 4, 7, 1, 7, 3, 1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 4, 7, 3, 0, 4, 1, 2, 10, -1, -1, -1, -1, -1, -1, -1),
    (9, 2, 10, 9, 0, 2, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1),
    (2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, -1, -1, -1, -1),
    (8, 4, 7, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (11, 4, 7, 11, 2, 4, 2, 0, 4, -1, -1, -1, -1, -1, -1, -1),
    (9, 0, 1, 8, 4, 7, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1),
    (4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1, -1, -1, -1, -1),
    (3, 10, 1, 3, 11, 10, 7, 8, 4, -1, -1, -1, -1, -1, -1, -1),
    (1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4, -1, -1, -1, -1),
    (4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3, -1, -1, -1, -1),
    (4, 7, 11, 4, 11, 9, 9, 11, 10, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 4, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 5, 4, 1, 5, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (8, 5, 4, 8, 3, 5, 3, 1, 5, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 8, 1, 2, 10, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1),
    (5, 2, 10, 5, 4, 2, 4, 0, 2, -1, -1, -1, -1, -1, -1, -1),
    (2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, -1, -1, -1, -1),
    (9, 5, 4, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 11, 2, 0, 8, 11, 4, 9, 5, -1, -1, -1, -1, -1, -1, -1),
    (0, 5, 4, 0, 1, 5, 2, 3, 11, -1, -1, -1, -1, -1, -1, -1),
    (2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5, -1, -1, -1, -1),
    (10, 3, 11, 10, 1, 3, 9, 5, 4, -1, -1, -1, -1, -1, -1, -1),
    (4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10, -1, -1, -1, -1),
    (5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3, -1, -1, -1, -1),
    (5, 4, 8, 5, 8, 10, 10, 8, 11, -1, -1, -1, -1, -1, -1, -1),
    (9, 7, 8, 5, 7, 9, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 3, 0, 9, 5, 3, 5, 7, 3, -1, -1, -1, -1, -1, -1, -1),
    (0, 7, 8, 0, 1, 7, 1, 5, 7, -1, -1, -1, -1, -1, -1, -1),
    (1, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 7, 8, 9, 5, 7, 10, 1, 2, -1, -1, -1, -1, -1, -1, -1),
    (10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, -1, -1, -1, -1),
    (8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2, -1, -1, -1, -1),
    (2, 10, 5, 2, 5, 3, 3, 5, 7, -1, -1, -1, -1, -1, -1, -1),
    (7, 9, 5, 7, 8, 9, 3, 11, 2, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11, -1, -1, -1, -1),
    (2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7, -1, -1, -1, -1),
    (11, 2, 1, 11, 1, 7, 7, 1, 5, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11, -1, -1, -1, -1),
    (5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0, -1),
    (11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0, -1),
    (11, 10, 5, 7, 11, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 0, 1, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 8, 3, 1, 9, 8, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1),
    (1, 6, 5, 2, 6, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 6, 5, 1, 2, 6, 3, 0, 8, -1, -1, -1, -1, -1, -1, -1),
    (9, 6, 5, 9, 0, 6, 0, 2, 6, -1, -1, -1, -1, -1, -1, -1),
    (5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, -1, -1, -1, -1),
    (2, 3, 11, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (11, 0, 8, 11, 2, 0, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 9, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1, -1, -1, -1),
    (5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11, -1, -1, -1, -1),
    (6, 3, 11, 6, 5, 3, 5, 1, 3, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6, -1, -1, -1, -1),
    (3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, -1, -1, -1, -1),
    (6, 5, 9, 6, 9, 11, 11, 9, 8, -1, -1, -1, -1, -1, -1, -1),
    (5, 10, 6, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 3, 0, 4, 7, 3, 6, 5, 10, -1, -1, -1, -1, -1, -1, -1),
    (1, 9, 0, 5, 10, 6, 8, 4, 7, -1, -1, -1, -1, -1, -1, -1),
    (10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, -1, -1, -1, -1),
    (6, 1, 2, 6, 5, 1, 4, 7, 8, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, -1, -1, -1, -1),
    (8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, -1, -1, -1, -1),
    (7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9, -1),
    (3, 11, 2, 7, 8, 4, 10, 6, 5, -1, -1, -1, -1, -1, -1, -1),
    (5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11, -1, -1, -1, -1),
    (0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6, -1, -1, -1, -1),
    (9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6, -1),
    (8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6, -1, -1, -1, -1),
    (5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11, -1),
    (0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7, -1),
    (6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9, -1, -1, -1, -1),
    (10, 4, 9, 6, 4, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 10, 6, 4, 9, 10, 0, 8, 3, -1, -1, -1, -1, -1, -1, -1),
    (10, 0, 1, 10, 6, 0, 6, 4, 0, -1, -1, -1, -1, -1, -1, -1),
    (8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10, -1, -1, -1, -1),
    (1, 4, 9, 1, 2, 4, 2, 6, 4, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, -1, -1, -1, -1),
    (0, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (8, 3, 2, 8, 2, 4, 4, 2, 6, -1, -1, -1, -1, -1, -1, -1),
    (10, 4, 9, 10, 6, 4, 11, 2, 3, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6, -1, -1, -1, -1),
    (3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10, -1, -1, -1, -1),
    (6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1, -1),
    (9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3, -1, -1, -1, -1),
    (8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1, -1),
    (3, 11, 6, 3, 6, 0, 0, 6, 4, -1, -1, -1, -1, -1, -1, -1),
    (6, 4, 8, 11, 6, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (7, 10, 6, 7, 8, 10, 8, 9, 10, -1, -1, -1, -1, -1, -1, -1),
    (0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10, -1, -1, -1, -1),
    (10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0, -1, -1, -1, -1),
    (10, 6, 7, 10, 7, 1, 1, 7, 3, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, -1, -1, -1, -1),
    (2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9, -1),
    (7, 8, 0, 7, 0, 6, 6, 0, 2, -1, -1, -1, -1, -1, -1, -1),
    (7, 3, 2, 6, 7, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7, -1, -1, -1, -1),
    (2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7, -1),
    (1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11, -1),
    (11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1, -1, -1, -1, -1),
    (8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6, -1),
    (0, 9, 1, 11, 6, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0, -1, -1, -1, -1),
    (7, 11, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 8, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 9, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (8, 1, 9, 8, 3, 1, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1),
    (10, 1, 2, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 3, 0, 8, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1),
    (2, 9, 0, 2, 10, 9, 6, 11, 7, -1, -1, -1, -1, -1, -1, -1),
    (6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8, -1, -1, -1, -1),
    (7, 2, 3, 6, 2, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (7, 0, 8, 7, 6, 0, 6, 2, 0, -1, -1, -1, -1, -1, -1, -1),
    (2, 7, 6, 2, 3, 7, 0, 1, 9, -1, -1, -1, -1, -1, -1, -1),
    (1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, -1, -1, -1, -1),
    (10, 7, 6, 10, 1, 7, 1, 3, 7, -1, -1, -1, -1, -1, -1, -1),
    (10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8, -1, -1, -1, -1),
    (0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7, -1, -1, -1, -1),
    (7, 6, 10, 7, 10, 8, 8, 10, 9, -1, -1, -1, -1, -1, -1, -1),
    (6, 8, 4, 11, 8, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 6, 11, 3, 0, 6, 0, 4, 6, -1, -1, -1, -1, -1, -1, -1),
    (8, 6, 11, 8, 4, 6, 9, 0, 1, -1, -1, -1, -1, -1, -1, -1),
    (9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6, -1, -1, -1, -1),
    (6, 8, 4, 6, 11, 8, 2, 10, 1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6, -1, -1, -1, -1),
    (4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9, -1, -1, -1, -1),
    (10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3, -1),
    (8, 2, 3, 8, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1),
    (0, 4, 2, 4, 6, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8, -1, -1, -1, -1),
    (1, 9, 4, 1, 4, 2, 2, 4, 6, -1, -1, -1, -1, -1, -1, -1),
    (8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1, -1, -1, -1, -1),
    (10, 1, 0, 10, 0, 6, 6, 0, 4, -1, -1, -1, -1, -1, -1, -1),
    (4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3, -1),
    (10, 9, 4, 6, 10, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 9, 5, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 4, 9, 5, 11, 7, 6, -1, -1, -1, -1, -1, -1, -1),
    (5, 0, 1, 5, 4, 0, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1),
    (11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5, -1, -1, -1, -1),
    (9, 5, 4, 10, 1, 2, 7, 6, 11, -1, -1, -1, -1, -1, -1, -1),
    (6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5, -1, -1, -1, -1),
    (7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2, -1, -1, -1, -1),
    (3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6, -1),
    (7, 2, 3, 7, 6, 2, 5, 4, 9, -1, -1, -1, -1, -1, -1, -1),
    (9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7, -1, -1, -1, -1),
    (3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0, -1, -1, -1, -1),
    (6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8, -1),
    (9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7, -1, -1, -1, -1),
    (1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4, -1),
    (4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10, -1),
    (7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10, -1, -1, -1, -1),
    (6, 9, 5, 6, 11, 9, 11, 8, 9, -1, -1, -1, -1, -1, -1, -1),
    (3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5, -1, -1, -1, -1),
    (0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11, -1, -1, -1, -1),
    (6, 11, 3, 6, 3, 5, 5, 3, 1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6, -1, -1, -1, -1),
    (0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10, -1),
    (11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5, -1),
    (6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3, -1, -1, -1, -1),
    (5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, -1, -1, -1, -1),
    (9, 5, 6, 9, 6, 0, 0, 6, 2, -1, -1, -1, -1, -1, -1, -1),
    (1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8, -1),
    (1, 5, 6, 2, 1, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6, -1),
    (10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0, -1, -1, -1, -1),
    (0, 3, 8, 5, 6, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (10, 5, 6, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (11, 5, 10, 7, 5, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (11, 5, 10, 11, 7, 5, 8, 3, 0, -1, -1, -1, -1, -1, -1, -1),
    (5, 11, 7, 5, 10, 11, 1, 9, 0, -1, -1, -1, -1, -1, -1, -1),
    (10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1, -1, -1, -1, -1),
    (11, 1, 2, 11, 7, 1, 7, 5, 1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11, -1, -1, -1, -1),
    (9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7, -1, -1, -1, -1),
    (7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2, -1),
    (2, 5, 10, 2, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1),
    (8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5, -1, -1, -1, -1),
    (9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2, -1, -1, -1, -1),
    (9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2, -1),
    (1, 3, 5, 3, 7, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 7, 0, 7, 1, 1, 7, 5, -1, -1, -1, -1, -1, -1, -1),
    (9, 0, 3, 9, 3, 5, 5, 3, 7, -1, -1, -1, -1, -1, -1, -1),
    (9, 8, 7, 5, 9, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (5, 8, 4, 5, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1),
    (5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0, -1, -1, -1, -1),
    (0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5, -1, -1, -1, -1),
    (10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4, -1),
    (2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8, -1, -1, -1, -1),
    (0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11, -1),
    (0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5, -1),
    (9, 4, 5, 2, 11, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4, -1, -1, -1, -1),
    (5, 10, 2, 5, 2, 4, 4, 2, 0, -1, -1, -1, -1, -1, -1, -1),
    (3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9, -1),
    (5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2, -1, -1, -1, -1),
    (8, 4, 5, 8, 5, 3, 3, 5, 1, -1, -1, -1, -1, -1, -1, -1),
    (0, 4, 5, 1, 0, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5, -1, -1, -1, -1),
    (9, 4, 5, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 11, 7, 4, 9, 11, 9, 10, 11, -1, -1, -1, -1, -1, -1, -1),
    (0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11, -1, -1, -1, -1),
    (1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11, -1, -1, -1, -1),
    (3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4, -1),
    (4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2, -1, -1, -1, -1),
    (9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3, -1),
    (11, 7, 4, 11, 4, 2, 2, 4, 0, -1, -1, -1, -1, -1, -1, -1),
    (11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4, -1, -1, -1, -1),
    (2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9, -1, -1, -1, -1),
    (9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7, -1),
    (3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10, -1),
    (1, 10, 2, 8, 7, 4, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 9, 1, 4, 1, 7, 7, 1, 3, -1, -1, -1, -1, -1, -1, -1),
    (4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1, -1, -1, -1, -1),
    (4, 0, 3, 7, 4, 3, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (4, 8, 7, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (9, 10, 8, 10, 11, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 9, 3, 9, 11, 11, 9, 10, -1, -1, -1, -1, -1, -1, -1),
    (0, 1, 10, 0, 10, 8, 8, 10, 11, -1, -1, -1, -1, -1, -1, -1),
    (3, 1, 10, 11, 3, 10, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 2, 11, 1, 11, 9, 9, 11, 8, -1, -1, -1, -1, -1, -1, -1),
    (3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9, -1, -1, -1, -1),
    (0, 2, 11, 8, 0, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (3, 2, 11, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 3, 8, 2, 8, 10, 10, 8, 9, -1, -1, -1, -1, -1, -1, -1),
    (9, 10, 2, 0, 9, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8, -1, -1, -1, -1),
    (1, 10, 2, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (1, 3, 8, 9, 1, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 9, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (0, 3, 8, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1),
    (-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1) );



  {$ENDREGION}

Type
  TeMetaBall = class(TeCustomMesh)
  private
    Procedure RebuildMesh(Const HighDefNormal : Boolean = False);
  public
    mbMin: TVector3D;
    mbMax: TVector3D;        // min and max of metaball grid space

    mbXRes: integer;
    mbYRes: integer;
    mbZRes: integer;    // grid resolution in x,y,z

    mbXR: integer;
    mbYR: integer;
    mbZR: integer; // grid cell resolutions in x,y,z

    mbGrid: PGridPoints;       // meta ball grid
    mbCell: PGridCells;        // meta ball grid cells

    mbTriVert: PVecs;          // meta ball computed triangles
    mbTriNorm: PVecs;          // meta ball computed normals
    mbTriColor: PColors4f;     // meta ball computed colors

    mbSphere: TMetaSpheres;    // spheres in meta ball object
    mbSpheres: integer;        // number of spheres
    mbTriVertComputed: integer;    // computed triangle vertices

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetResolution(XMin,YMin,ZMin, XMax,YMax,ZMax: Single; XRes,YRes,ZRes: integer);
    procedure ComputeComponents(Normal, Color: boolean);
    procedure AddSphere(x,y,z, r,g,b,a, Radius: Single);

    function GetSpheres: integer;
    function GetSphereCenter(SphereIndex: integer): TVector3D;
    function GetSphereRad(SphereIndex: integer): Single;
    function GetSphereColor(SphereIndex: integer): TColor4f;
    procedure SetSphereCenter(SphereIndex: integer; Center: TVector3D);
    procedure SetSphereRad(SphereIndex: integer; Radius: single);
    procedure SetSphereColor(SphereIndex: integer; Color: TColor4f);

    procedure Evaluate(var GridPoint: TGridPoint);
    function EvalNormal(Pos: TVector3D): TVector3D;
    function EvalColor(Pos: TVector3D): TColor4f;
    procedure EvalNormalAndColor(Pos: TVector3D; var Normal: TVector3D; var Color: TColor4f);
    procedure TriangulateCell(Cell: TGridCell);
    procedure ComputeBalls(Const HighDefNormal : Boolean = False);

  end;




implementation

function Color4f(r, g, b, a : Single) : TColor4f;
begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;
end;

{ Estimate the point where the metaballs intersect the given edge of a cube
  (the line segment [p1 p2]), using linear interpolation. valp1 and valp2 are
  the results of the metaball equations at the two edge vertices. }
procedure Interpolate(var p1, p2: TGridPoint; var v: TVector3d);
var
  mu: Single;
begin
  if Abs(1 - p1.Value) < 0.0001 then
  begin
    // p1 is on the metaball surface
    v := p1.Position;
    Exit;
  end;
  if Abs(1 - p2.Value) < 0.0001 then
  begin
    // p2 is on the metaball surface
    v := p2.Position;
    Exit;
  end;
  if Abs(p1.Value - p2.Value) < 0.0001 then
  begin
    // The given edge is entirely on the metaball surface
    v := p1.Position;
    Exit;
  end;

  // Interpolate for all other cases:
  mu := (1.0 - p1.Value) / (p2.Value - p1.Value);
  v.x := p1.Position.x + mu * (p2.Position.x - p1.Position.x);
  v.y := p1.Position.y + mu * (p2.Position.y - p1.Position.y);
  v.z := p1.Position.z + mu * (p2.Position.z - p1.Position.z);
end;




{ TeMetaBall }

constructor TeMetaBall.Create(AOwner: TComponent);
begin
  Inherited;
  mbXRes := 0;
  mbYRes := 0;
  mbZRes := 0;
  mbGrid := NIL;
  mbCell := NIL;
  FillChar(mbSphere, sizeof(mbSphere), 0);
  mbSpheres := 0;
  mbTriVert := NIL;
  mbTriNorm := NIL;
  mbTriColor := NIL;
  mbMin := NullVector3D;
  mbMax := NullVector3D;

  HitTest := False;
  ZWrite := true;
  TwoSide := true;
  Height := 1;
  Width := 1;
  Depth := 1;
end;

destructor TeMetaBall.Destroy;
begin
  FreeMem(mbGrid);
  FreeMem(mbCell);
  FreeMem(mbTriVert);
  inherited;
end;

procedure TeMetaBall.AddSphere(x, y, z, r, g, b, a, Radius: Single);
begin
  mbSphere[mbSpheres].Center := Vector3d(x,y,z);
  mbSphere[mbSpheres].Color := Color4f(r,g,b,a);
  mbSphere[mbSpheres].Radius := Radius;
  mbSphere[mbSpheres].SquareRad := Sqr(Radius);
  inc(mbSpheres);
end;

procedure TeMetaBall.ComputeComponents(Normal, Color: boolean);
begin
  if Normal then
    ReallocMem(mbTriNorm, 8 * mbXRes * mbYRes * mbZRes * sizeof(TVector3d));
  if Color then
    ReallocMem(mbTriColor, 8 * mbXRes * mbYRes * mbZRes * sizeof(TAlphaColor));
end;


procedure TeMetaBall.SetResolution(XMin, YMin, ZMin, XMax, YMax, ZMax: Single;
  XRes, YRes, ZRes: integer);
var
  n: integer;
  fx,fy,fz: Double;
  x,y,z,i: integer;
  px, py, pz: Double;

begin
  mbXRes := XRes + 1;
  mbYRes := YRes + 1;
  mbZRes := ZRes + 1;
  mbXR := XRes;
  mbYR := YRes;
  mbZR := ZRes;

  n := mbXRes * mbYRes * mbZRes;
  ReallocMem(mbGrid, n * sizeof(TGridPoint));
  n := n * 8;
  ReallocMem(mbTriVert, n * sizeof(TVector3d));
  if mbTriNorm<>NIL then ReallocMem(mbTriNorm, n * sizeof(TVector3d));
  if mbTriColor<>NIL then ReallocMem(mbTriColor, n * sizeof(TVector3d));
  n := mbXR * mbYR * mbZR;
  ReallocMem(mbCell, n * sizeof(TGridCell));

  fx := (XMax - XMin) / mbXR;
  fy := (YMax - YMin) / mbYR;
  fz := (ZMax - ZMin) / mbZR;
  // Set grid positions:
  for x := 0 to mbXR do
  begin
    px := XMin + x*fx;
    for y := 0 to mbYR do
    begin
      py := YMin + y*fy;
      for z := 0 to mbZR do
      begin
        pz := ZMax - z*fz;
        mbGrid^[x*mbYRes*mbZRes + y*mbZRes + z].Position := Vector3d(px, py, pz);
      end;
    end;
  end;

  // Create cubes:
  for x := 0 to mbXR-1 do
  begin
    for y := 0 to mbYR-1 do
    begin
      for z := 0 to mbZR-1 do
      begin
        i := x*mbYR*mbZR + y*mbZR + z;
        mbCell^[i].P[0] := @mbGrid^[x*mbYRes*mbZRes + y*mbZRes + z];
        mbCell^[i].P[1] := @mbGrid^[(x+1)*mbYRes*mbZRes + y*mbZRes + z];
        mbCell^[i].P[2] := @mbGrid^[(x+1)*mbYRes*mbZRes + y*mbZRes + z+1];
        mbCell^[i].P[3] := @mbGrid^[x*mbYRes*mbZRes + y*mbZRes + z+1];
        mbCell^[i].P[4] := @mbGrid^[x*mbYRes*mbZRes + (y+1)*mbZRes + z];
        mbCell^[i].P[5] := @mbGrid^[(x+1)*mbYRes*mbZRes + (y+1)*mbZRes + z];
        mbCell^[i].P[6] := @mbGrid^[(x+1)*mbYRes*mbZRes + (y+1)*mbZRes + z+1];
        mbCell^[i].P[7] := @mbGrid^[x*mbYRes*mbZRes + (y+1)*mbZRes + z+1];
      end;
    end;
  end;
  mbMin := Vector3d(XMin, YMin, ZMin);
  mbMax := Vector3d(XMax, YMax, ZMax);

//    objMin := mbMin;
//    objMax := mbMax;
//    objCent := VectorAdd(objMin, VectorScale(VectorSub(objMax, objMin), 0.5));
//    objBSRad := VectorLength(VectorScale(VectorSub(objMax, objMin), 0.5));
end;


function TeMetaBall.GetSpheres: integer;
begin
  result := mbSpheres;
end;



procedure TeMetaBall.RebuildMesh(Const HighDefNormal : Boolean = False);
var i : integer;
begin
  Data.VertexBuffer.Length := mbTriVertComputed;
  Data.IndexBuffer.Length := mbTriVertComputed;
  For i := 0 to mbTriVertComputed-1 do
  begin
    Data.VertexBuffer.Vertices[i] := Point3D(mbTriVert^[ i  ].X,mbTriVert^[ i  ].Y, mbTriVert^[ i  ].Z);
    Data.IndexBuffer.Indices[i] := i;
    //vb.Normals[i] := Point3D(mbTriNorm^[ i  ].X,mbTriNorm^[ i  ].Y, mbTriNorm^[ i  ].Z);
    //vb.Normals[i].Scale(3);
    //vb.Normals[i] := Point3D(mbTriVert^[ i  ].X,mbTriVert^[ i  ].Y, mbTriVert^[ i  ].Z);
    //vb.Normals[i].Normalize;
  end;

  if HighDefNormal then
    data.CalcSmoothNormals
  else
    Data.CalcFaceNormals;

  ProcessBoundingBox;
end;

{
procedure TeMetaBall.Render;
var Mat : TMaterial;
begin
  //Context.SetMatrix(Matrix3DMultiply(CreateScaleMatrix3D(Vector3D(Width, Height, Depth)), AbsoluteMatrix));
  Mat := nil;
  try
    if not Assigned(MaterialSource) then
    begin
      Mat := TColorMaterial.Create;
      TColorMaterial(Mat).Color := TAlphaColorRec.Blue;
      Context.DrawTriangles(Data.VertexBuffer, Data.IndexBuffer, Mat, AbsoluteOpacity);
    end
    else
    begin
      Context.DrawTriangles(Data.VertexBuffer, Data.IndexBuffer, MaterialSource.Material, AbsoluteOpacity);
    end;

  finally
    if Assigned(Mat) then
      FreeAndNil(Mat);
  end;
end;
}

function TeMetaBall.GetSphereCenter(SphereIndex: integer): TVector3D;
begin
  result := mbSphere[SphereIndex].Center;
end;



function TeMetaBall.GetSphereRad(SphereIndex: integer): Single;
begin
  result := mbSphere[SphereIndex].Radius;
end;



function TeMetaBall.GetSphereColor(SphereIndex: integer): TColor4f;
begin
  result := mbSphere[SphereIndex].Color;
end;



procedure TeMetaBall.SetSphereCenter(SphereIndex: integer; Center: TVector3D);
begin
  mbSphere[SphereIndex].Center := Center;
end;

procedure TeMetaBall.SetSphereRad(SphereIndex: integer; Radius: Single);
begin
  mbSphere[SphereIndex].Radius := Radius;
  mbSphere[SphereIndex].SquareRad := Sqr(Radius);
end;


procedure TeMetaBall.SetSphereColor(SphereIndex: integer; Color: TColor4f);
begin
  mbSphere[SphereIndex].Color := Color;
end;



// add the equations for all meta spheres.
procedure TeMetaBall.Evaluate(var GridPoint: TGridPoint);
var
  i: integer;
  dx,dy,dz: Single;
  Sphere: PMetaSphere;

begin
  GridPoint.Value := 0;
  for i := 0 to mbSpheres-1 do begin
    Sphere := @mbSphere[i];
    // add the result of the metaball's equation for the point [x,y,z]
    dx := GridPoint.Position.x - Sphere^.Center.x;
    dy := GridPoint.Position.y - Sphere^.Center.y;
    dz := GridPoint.Position.z - Sphere^.Center.z;

    if (dx*dx + dy*dy + dz*dz)<>0.0 then
    GridPoint.Value := GridPoint.Value + (Sphere.SquareRad / (dx*dx + dy*dy + dz*dz));
  end;
end;



function TeMetaBall.EvalNormal(Pos: TVector3D): TVector3D;
var
  i: integer;
  dx, dy, dz, f: Single;
  Sphere: PMetaSphere;

begin
  result.x := 0;
  result.y := 0;
  result.z := 0;
  if mbSpheres>0 then begin
    for i := 0 to mbSpheres-1 do begin
      Sphere := @mbSphere[i];
      dx := Pos.x - Sphere^.Center.x;
      dy := Pos.y - Sphere^.Center.y;
      dz := Pos.z - Sphere^.Center.z;
      f := Sphere^.SquareRad / (dx*dx + dy*dy + dz*dz);
      result.x := result.x + dx*f;
      result.y := result.y + dy*f;
      result.z := result.z + dz*f;
    end;
  end;
end;



function TeMetaBall.EvalColor(Pos: TVector3D): TColor4f;
var
  i: integer;
  dx, dy, dz, f: Single;
  Sphere: PMetaSphere;

begin
  result.r := 0.0;
  result.g := 0.0;
  result.b := 0.0;
  result.a := 0.0;
  for i := 0 to mbSpheres-1 do begin
    Sphere := @mbSphere[i];
    dx := Pos.x - Sphere^.Center.x;
    dy := Pos.y - Sphere^.Center.y;
    dz := Pos.z - Sphere^.Center.z;
    f := Sphere^.SquareRad / (dx*dx + dy*dy + dz*dz);
    result.r := result.r + Sphere^.Color.r * f;
    result.g := result.g + Sphere^.Color.g * f;
    result.b := result.b + Sphere^.Color.b * f;
    result.a := result.a + Sphere^.Color.a * f;
  end;
end;



procedure TeMetaBall.EvalNormalAndColor(Pos: TVector3D; var Normal: TVector3D; var Color: TColor4f);
var
  i: integer;
  dx, dy, dz, f: Single;
  Sphere: PMetaSphere;

begin
  Normal.x := 0.0;
  Normal.y := 0.0;
  Normal.z := 0.0;
  Color.r := 0.0;
  Color.g := 0.0;
  Color.b := 0.0;
  Color.a := 0.0;

  for i := 0 to mbSpheres-1 do
  begin
    Sphere := @mbSphere[i];
    dx := Pos.x - Sphere^.Center.x;
    dy := Pos.y - Sphere^.Center.y;
    dz := Pos.z - Sphere^.Center.z;
    f := Sphere.SquareRad / (dx*dx + dy*dy + dz*dz);
    Normal.x := Normal.x + dx*f;
    Normal.y := Normal.y + dy*f;
    Normal.z := Normal.z + dz*f;
    Color.r := Color.r + f * Sphere^.Color.r;
    Color.g := Color.g + f * Sphere^.Color.g;
    Color.b := Color.b + f * Sphere^.Color.b;
    Color.a := Color.a + f * Sphere^.Color.a;
  end;
end;



{ Create triangles for the given grid cell. The triangles will be rendered
  immediately. The cell's corner vertices need to have been initialized with
  the correct values, of course. }
procedure TeMetaBall.TriangulateCell(Cell: TGridCell);
var
  i: integer;
  cubeidx: integer;
  verts: array [0..11] of TVector3D;
begin
  // Determine the index into the edge table:
  cubeidx := 0;
  if Cell.P[0]^.Value < 1 then cubeidx := cubeidx or 1;
  if Cell.P[1]^.Value < 1 then cubeidx := cubeidx or 2;
  if Cell.P[2]^.Value < 1 then cubeidx := cubeidx or 4;
  if Cell.P[3]^.Value < 1 then cubeidx := cubeidx or 8;
  if Cell.P[4]^.Value < 1 then cubeidx := cubeidx or 16;
  if Cell.P[5]^.Value < 1 then cubeidx := cubeidx or 32;
  if Cell.P[6]^.Value < 1 then cubeidx := cubeidx or 64;
  if Cell.P[7]^.Value < 1 then cubeidx := cubeidx or 128;

  // The edge table tells us which vertices are inside/outside the metaballs.
  if edgeTable[cubeidx] = 0 then
  begin
    // The cube is entirely inside/outside of the metaballs:
    Exit;
  end;

  // Find the vertices where the surface intersects the cube, using interpolation.
  if (edgeTable[cubeidx] and 1) <> 0 then
    Interpolate(Cell.P[0]^, Cell.P[1]^, verts[0]);
  if (edgeTable[cubeidx] and 2) <> 0 then
    Interpolate(Cell.P[1]^, Cell.P[2]^, verts[1]);
  if (edgeTable[cubeidx] and 4) <> 0 then
    Interpolate(Cell.P[2]^, Cell.P[3]^, verts[2]);
  if (edgeTable[cubeidx] and 8) <> 0 then
    Interpolate(Cell.P[3]^, Cell.P[0]^, verts[3]);
  if (edgeTable[cubeidx] and 16) <> 0 then
    Interpolate(Cell.P[4]^, Cell.P[5]^, verts[4]);
  if (edgeTable[cubeidx] and 32) <> 0 then
    Interpolate(Cell.P[5]^, Cell.P[6]^, verts[5]);
  if (edgeTable[cubeidx] and 64) <> 0 then
    Interpolate(Cell.P[6]^, Cell.P[7]^, verts[6]);
  if (edgeTable[cubeidx] and 128) <> 0 then
    Interpolate(Cell.P[7]^, Cell.P[4]^, verts[7]);
  if (edgeTable[cubeidx] and 256) <> 0 then
    Interpolate(Cell.P[0]^, Cell.P[4]^, verts[8]);
  if (edgeTable[cubeidx] and 512) <> 0 then
    Interpolate(Cell.P[1]^, Cell.P[5]^, verts[9]);
  if (edgeTable[cubeidx] and 1024) <> 0 then
    Interpolate(Cell.P[2]^, Cell.P[6]^, verts[10]);
  if (edgeTable[cubeidx] and 2048) <> 0 then
    Interpolate(Cell.P[3]^, Cell.P[7]^, verts[11]);

  // Create the triangle(s):
  i := 0;
  while TRITABLE[cubeidx][i] <> -1 do
  begin
    mbTriVert^[mbTriVertComputed+i  ] := verts[TRITABLE[cubeidx][i  ]];
    mbTriVert^[mbTriVertComputed+i+1] := verts[TRITABLE[cubeidx][i+1]];
    mbTriVert^[mbTriVertComputed+i+2] := verts[TRITABLE[cubeidx][i+2]];
    inc(i, 3);
  end;
  inc(mbTriVertComputed, i);
end;


// Evaluate and compute all meta spheres.
procedure TeMetaBall.ComputeBalls(Const HighDefNormal : Boolean = False);
var
  i, n : integer;

begin
  // First initialize the grid:
  n := mbXRes*mbYRes*mbZRes - 1;
  for i := 0 to n do
    Evaluate(mbGrid^[i]);

  // Then triangulate it cell by cell:
  n := mbXR*mbYR*mbZR - 1;
  mbTriVertComputed := 0;
  for i := 0 to n do
    TriangulateCell(mbCell^[i]);

  RebuildMesh(HighDefNormal);


{ Useless in FMX Scope (Normal build from vertex calculation.)
  if mbTriNorm<>NIL then begin
    if mbTriColor<>NIL then begin
      // norm + color
      for i:=0 to mbTriVertComputed-1 do
        EvalNormalAndColor(mbTriVert^[i], mbTriNorm^[i], mbTriColor^[i]);
    end
    else begin
      // norm
      for i:=0 to mbTriVertComputed-1 do
        mbTriNorm^[i] := EvalNormal(mbTriVert^[i]);
    end;
  end
  else if mbTriColor<>NIL then begin
    // color
    for i:=0 to mbTriVertComputed-1 do
      mbTriColor^[i] := EvalColor(mbTriVert^[i]);
  end;
}

end;




end.
