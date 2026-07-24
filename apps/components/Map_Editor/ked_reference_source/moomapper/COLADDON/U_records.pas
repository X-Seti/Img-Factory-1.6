unit U_records;

interface

uses graphics, math, opengl;

var
  TriStrip: gluint = GL_TRIANGLE_STRIP;

type

TMatrix4f = array[0..3, 0..3] of GLfloat;

TQuaternion = packed record
w, x, y, z: Single;
end;

Tvertex2D = packed record // used in colclass.pas for translation
x, y: single;
end;

Tvertex3D = packed record   // 12 bytes
x, y, z: single;           // world coordinates for this vertex
end;

TAxisAngle = packed record
angle: single;
axis: Tvertex3d;
end;

Tbounding_obj = packed record // 40 bytes
// collision sphere
sphere_radius: single;
sphere_pos: Tvertex3D;
// collision box
boxstart, boxend: Tvertex3D
end;

Tcollsphere = packed record // 20 bytes
radius: single;            // sphere radius
position: Tvertex3D;
surface: byte;
attachment: byte;
align: word;
end;

Tcube3D = packed record     // 28 bytes
boxstart, boxend: Tvertex3D;
surface: byte;
attachment: byte;
align: word; 
end;

Tface3D = packed record     // 16 bytes
a: longword;               // vertex a
b: longword;               // vertex b
c: longword;               // vertex c
surface: byte;
attachment: byte;
align: word;
end;

COLL = packed record
collident: array[0..3] of char;   // COLL 
size: longword;                   // coll chunk size without collident text and this longword
object_name: array[0..19] of char; // model name, terminated with a #0
generator: array[0..3] of char;   // part of model name, Abused by Steve M. where his editors 
                                  // sets this to STMU, col-io sets this to DCOL
bounding_obj: Tbounding_obj;      // collision sphere and box 

// 48 bytes

spherescount: longword;           // number of collision spheres
spheres: array of Tcollsphere;     // array of collision spheres

unknowndata: longword;            // unknown data, alaways 0

cubescount: longword;         // number of collision boxes
cubes: array of Tcube3D;           // array of collision boxes

verticescount: longword;          // number of vertexes
vertices: array of Tvertex3D;       // vertex array

facescount: longword;             // number of faces
faces: array of Tface3D;           // faces array
end;

var
colors: array[0..34] of Tcolor; 

procedure rendercube(const start, stop: Tvertex3D);
function QuaternionToMatrix(const q: TQuaternion): Tmatrix4f;
function QuaternionToAxisAngle(const q: TQuaternion): TAxisAngle;

implementation

procedure rendercube(const start, stop: Tvertex3D);
begin
glBegin(gl_quads);
glTexCoord2f(0, 0); glVertex3f(start.x, start.y, start.z );
glTexCoord2f(0, 1); glVertex3f(stop.x,  start.y, start.z );
glTexCoord2f(1, 1); glVertex3f(stop.x,  start.y, stop.z  );
glTexCoord2f(1, 0); glVertex3f(start.x, start.y, stop.z  );

glTexCoord2f(0, 1);  glVertex3f(start.x, stop.y, start.z );
glTexCoord2f(0, 0);  glVertex3f(start.x, stop.y, stop.z  );
glTexCoord2f(1, 0);  glVertex3f(stop.x,  stop.y, stop.z  );
glTexCoord2f(1, 1);  glVertex3f(stop.x,  stop.y, start.z );

glTexCoord2f(0, 0); glVertex3f(start.x, start.y, start.z );
glTexCoord2f(0, 1); glVertex3f(start.x, start.y, stop.z  );
glTexCoord2f(1, 1); glVertex3f(start.x,  stop.y, stop.z  );
glTexCoord2f(1, 0); glVertex3f(start.x,  stop.y, start.z );

glTexCoord2f(0, 0); glVertex3f(stop.x, start.y, start.z );
glTexCoord2f(0, 1); glVertex3f(stop.x,  stop.y, start.z );
glTexCoord2f(1, 1); glVertex3f(stop.x,  stop.y, stop.z  );
glTexCoord2f(1, 0); glVertex3f(stop.x, start.y, stop.z  );

glTexCoord2f(0, 0); glVertex3f(start.x , start.y, start.z);
glTexCoord2f(1, 0); glVertex3f(start.x ,  stop.y, start.z);
glTexCoord2f(1, 1); glVertex3f(stop.x  ,  stop.y, start.z);
glTexCoord2f(0, 1); glVertex3f(stop.x  , start.y, start.z);

glTexCoord2f(0, 0); glVertex3f(start.x , start.y, stop.z);
glTexCoord2f(0, 1); glVertex3f(stop.x  , start.y, stop.z);
glTexCoord2f(1, 1); glVertex3f(stop.x  ,  stop.y, stop.z);
glTexCoord2f(1, 0); glVertex3f(start.x ,  stop.y, stop.z);
glEnd;
end;

// delphi3d@gamedeveloper.org
function QuaternionToMatrix(const q: TQuaternion): Tmatrix4f;
begin

  // Convert a quaternion to a matrix:
  with q do
  begin
    Result[0,0] := 1 - 2*y*y - 2*z*z;
    Result[1,0] := 2*x*y - 2*w*z;
    Result[2,0] := 2*x*z + 2*w*y;
    Result[3,0] := 0;

    Result[0,1] := 2*x*y + 2*w*z;
    Result[1,1] := 1 - 2*x*x - 2*z*z;
    Result[2,1] := 2*y*z - 2*w*x;
    Result[3,1] := 0;

    Result[0,2] := 2*x*z - 2*w*y;
    Result[1,2] := 2*y*z + 2*w*x;
    Result[2,2] := 1 - 2*x*x - 2*y*y;
    Result[3,2] := 0;

    Result[0,3] := 0;
    Result[1,3] := 0;
    Result[2,3] := 0;
    Result[3,3] := 1;
  end;

end;

function QuaternionToAxisAngle(const q: TQuaternion): TAxisAngle;
var
  s: Single;
begin

  // Convert a quaternion to an axis/angle representation:
  with q do
  begin
    s := sqrt(x*x + y*y + z*z);
    if s <> 0 then
    begin
      Result.Angle := 2 * ArcCos(w);
      Result.Axis.x := x / s;
      Result.Axis.y := y / s;
      Result.Axis.z := z / s;
    end
    else begin
      Result.Angle := 0;
      Result.Axis.x := 1;
      Result.Axis.y := 0;
      Result.Axis.z := 0;
    end;
  end;

end;

end.
