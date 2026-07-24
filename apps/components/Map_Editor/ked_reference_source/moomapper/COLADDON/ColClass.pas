unit ColClass;

interface

uses windows, sysutils, classes, dialogs,
  Opengl, U_records, FastTextCRC;

type
Tcolchunk = class(Tcomponent)
public
namecrc: integer;  // faster search using CRC calculations of names
modelname: string; // if crc matches, check the name to get 100% match, otherwise continue searching
data: COLL;
PreList: integer;  // not used?
procedure render3D; // renders into active opengl context
end;

Tcolfile = class(Tcomponent)
public
filename: string;
// col chunks are child objects of this.
function find(const what: string): integer;
procedure loaddata(stream: Tstream; filen: string; maxpos: integer); // loads coll stream (like from .img file)
end;

implementation

// Artem: implementation of glutSolidSphere:
// http://www.xmission.com/~nate/glut.html

procedure glutSolidSphere(radius: GLdouble; slices: GLint; stacks: GLint);
var
  quadObj: GLUquadricObj;
begin
  quadObj := gluNewQuadric();
  gluQuadricDrawStyle(quadObj, GLU_FILL);
  gluQuadricNormals(quadObj, GLU_SMOOTH);
  { If we ever changed/used the texture or orientation state
     of quadObj, we'd need to change it to the defaults here
     with gluQuadricTexture and/or gluQuadricOrientation. }
  gluSphere(quadObj, radius, slices, stacks);
  gluDeleteQuadric(quadObj);
end;

{ Tcolchunk }

procedure Tcolchunk.render3D;
var
  i: integer;
  tex: GLboolean;
begin
  glColor4f(1, 1, 1, 1);
  glDisable(gl_blend);
  // Artem: Store old texture status
  glGetBooleanv(gl_texture_2d, @tex);
  if tex then
    glDisable(gl_texture_2d);

  glEnable(GL_NORMALIZE);

  // PreList: gluint;

  //if PreList = -1 then begin
  //outputdebugstring('pre-instancing..');
  //PreList:= glGenLists(1);
  //glNewList(prelist, GL_COMPILE);

  glmatrixmode(GL_MODELVIEW); // who knows..

  glFrontFace(GL_CW);

  for i:= 0 to data.cubescount-1 do begin
    glColor3ubv(@U_records.colors[data.cubes[i].surface]);
    rendercube(data.cubes[i].boxstart, data.cubes[i].boxend);
  end;

  glFrontFace(GL_CCW);

  for i:= 0 to data.spherescount-1 do begin
    glColor3ubv(@U_records.colors[data.spheres[i].surface]);
    glPushMatrix;
    glTranslatef(data.spheres[i].position.x, data.spheres[i].position.y, data.spheres[i].position.z);
    glutSolidSphere(data.spheres[i].radius, 5, 5);
    glPopMatrix;
  end;

  gldisable(GL_CULL_FACE); // collision faces are both-sided

  glBegin(gl_triangles);

  for i:= 0 to data.facescount-1 do begin
    glColor3ubv(@U_records.colors[data.faces[i].surface]);
    glTexCoord2f(0, 0); glvertex3fv(@data.vertices[data.faces[i].a]);
    glTexCoord2f(0, 1); glvertex3fv(@data.vertices[data.faces[i].b]);
    glTexCoord2f(1, 0); glvertex3fv(@data.vertices[data.faces[i].c]);
  end;

  glend;

  glenable(GL_CULL_FACE);

  // Artem: Restore old texture status
  if tex then
    glEnable(gl_texture_2d);

  //glEndList;
  //end else begin
  //glCallList(prelist);
  //end;

end;

{ Tcolfile }

function Tcolfile.find(const what: string): integer;
var
i: integer;
tmpcrc: integer;
begin
result:= maxint;

tmpcrc:= makecrc(lowercase(what));

for i:= 0 to ComponentCount -1 do begin
if tmpcrc = makecrc(lowercase((Components[i] as Tcolchunk).data.object_name)) then
if what = (Components[i] as Tcolchunk).data.object_name then begin result:= i; exit; end;
end;

end;

procedure Tcolfile.loaddata(stream: Tstream; filen: string; maxpos: integer);
var
lw: longword;
newchunk: Tcolchunk;
begin
filename:= filen;

if stream.position <> 0 then outputdebugstring(pchar('Tcolfile.create #' + inttostr(stream.position)))
else outputdebugstring('disk file');

repeat

newchunk:= Tcolchunk.create(self);
newchunk.namecrc:= makecrc(newchunk.modelname);
newchunk.PreList:= -1;

stream.Read(newchunk.data, 32);

newchunk.modelname:= newchunk.data.object_name;

if trim(newchunk.data.collident) = 'COLL' then begin
//outputdebugstring(newchunk.data.object_name);

stream.read(newchunk.data.bounding_obj, sizeof(newchunk.data.bounding_obj)); // bounding objects

// read spheres
stream.Read(newchunk.data.spherescount, 4);
setlength(newchunk.data.spheres, newchunk.data.spherescount);
stream.Read(Pointer(newchunk.data.spheres)^, newchunk.data.spherescount * 20);

// read unknown stuff
stream.read(lw, 4); if lw <> 0 then begin outputdebugstring(pchar('UNKNOWN DATA - ' + inttostr(lw))); exit; end;

// read cubes
stream.Read(newchunk.data.cubescount, 4);
setlength(newchunk.data.cubes, newchunk.data.cubescount);
stream.Read(Pointer(newchunk.data.cubes)^, newchunk.data.cubescount * 28);

// read vetices
stream.Read(newchunk.data.verticescount, 4);
setlength(newchunk.data.vertices, newchunk.data.verticescount);
stream.Read(Pointer(newchunk.data.vertices)^, newchunk.data.verticescount * 12);

// read faces
stream.Read(newchunk.data.facescount, 4);
setlength(newchunk.data.faces, newchunk.data.facescount);
stream.Read(Pointer(newchunk.data.faces)^, newchunk.data.facescount * 16);

end else stream.position:= maxpos; // hit alingment data, exit

until stream.position >= maxpos; 

//inherited;
end;

end.
