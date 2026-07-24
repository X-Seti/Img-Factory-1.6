//-----------------------------------------------------------------------------
//
//  Translation of the GLUT header ver. 3.6 file for Delphi 3.0+
//  Get the latest translation of the Glut headers at my homepage:
//    http://home.get2net.dk/mithrandir (case-sensitive address)
//  Get the latest compilled version of the Glut-DLLs at Nate Robins homepage:
//    http://www.cs.utah.edu/~narobins/glut.html
//
//-----------------------------------------------------------------------------
//
//  Usage: Include this unit in the uses clause of the units, which need to call GLUT.
//         The GLUT library is dynamicly loaded when GlutInit is called but calling
//         GlutLoadLibrary will take care of this as well. The library will automaticly
//         be released but calling GlutFreeLibrary will do the trick.
//
//         To initialize the dll call GlutInit with an argument specifying which library
//         to use (SGI_LIB or MS_LIB) The commandline arguments will automaticly be passed
//         along.
//
//         If the Glut library used is not the latest version (3.6) remember to set
//         the version define: GLUT_API_VERSION_x (where x = 1, 2 or 3) and
//         GLUT_XLIB_IMPLEMENTATION_xx in 'Glut_Api_ver.inc'
//
//-----------------------------------------------------------------------------
//
//  Assumptions:
//   -The unit asumes that you use a translation of the OpenGL headers which
//    includes GLU headers and that the unit is named OpenGL.pas
//    Otherwise, define {$DEFINE GLU_Exlcuded} in you project if you OpenGL units
//    are named GL.pas and GLU.pas
//
//   -The unit assumes that you do whatever necessary to use the OpenGL DLLs. That is,
//    if the OpenGl unit dynamicly loads the OpenGL DLL then you are still responsible
//    for that.
//
//  function GlutInit(Lib: TOpenGLLib): boolean;
//    Should allways be the first call to the Glut Library. It will load the library if
//    necessary and the parameter passed to the function should reflect the OpenGL library
//    used, ie. the MS or SGI version (OpenGL32.dll or OpenGL.dll).
//    TOpenGLLib = (MS_LIB, SGI_LIB)
//    The function returns true on success.
//
//  function GlutLoadLibrary(Lib: TOpenGLLib): boolean;
//    Load the library specified, ie. MS (MS_LIB) or SGI (SGI_LIB) version of GLUT
//
//  procedure GlutFreeLibrary;
//    Frees the loaded library. Normally this call isn't necessary as it happens
//    automaticly when the application terminates.
//
//-----------------------------------------------------------------------------
//
//  This translation is based on:
//
//  - Mark Kilgard's GLUT.h
//
//  Translator: Laurids Kirkeby (LKHK@get2net.dk)
//
//  Modified: 26. March 1998
//  Version:  0.8.0.0
//
//  Problems to be solved:
//    - None at the moment
//
//-----------------------------------------------------------------------------

{ --- Mark Kilgard's original copyright message ---

   Copyright (c) Mark J. Kilgard, 1994, 1995, 1996.

   This program is freely distributable without licensing fees  and is
   provided without guarantee or warrantee expressed or  implied. This
   program is -not- in the public domain. }

unit Glut;

interface

{$IFNDEF GLU_Included}    // Is the GLU defs part of the OpenGL.pas
{$IFNDEF GLU_Excluded}
  {$DEFINE GLU_Included}  // Assume yes
{$ENDIF}
{$ENDIF}

uses
  windows, SysUtils,
{$IFDEF GLU_Included}
  OpenGL
{$ELSE}
  GL, GLU
{$ENDIF};

{
 GLUT API revision history:

 GLUT_API_VERSION is updated to reflect incompatible GLUT
 API changes (interface changes, semantic changes, deletions,
 or additions).

 GLUT_API_VERSION=1  First public release of GLUT.  11/29/94

 GLUT_API_VERSION=2  Added support for OpenGL/GLX multisampling,
 extension.  Supports new input devices like tablet, dial and button
 box, and Spaceball.  Easy to query OpenGL extensions.

 GLUT_API_VERSION=3  glutMenuStatus added.

 GLUT_API_VERSION=4  glutInitDisplayString, glutWarpPointer,
 glutBitmapLength, glutStrokeLength, glutWindowStatusFunc, dynamic
 video resize subAPI, glutPostWindowRedisplay (NOT FINALIZED!).
}

{
 GLUT implementation revision history:

 GLUT_XLIB_IMPLEMENTATION is updated to reflect both GLUT
 API revisions and implementation revisions (ie, bug fixes).

 GLUT_XLIB_IMPLEMENTATION=1  mjk's first public release of
 GLUT Xlib-based implementation.  11/29/94

 GLUT_XLIB_IMPLEMENTATION=2  mjk's second public release of
 GLUT Xlib-based implementation providing GLUT version 2
 interfaces.

 GLUT_XLIB_IMPLEMENTATION=3  mjk's GLUT 2.2 images. 4/17/95

 GLUT_XLIB_IMPLEMENTATION=4  mjk's GLUT 2.3 images. 6/?/95

 GLUT_XLIB_IMPLEMENTATION=5  mjk's GLUT 3.0 images. 10/?/95

 GLUT_XLIB_IMPLEMENTATION=7  mjk's GLUT 3.1+ with glutWarpPoitner.  7/24/96

 GLUT_XLIB_IMPLEMENTATION=8  mjk's GLUT 3.1+ with glutWarpPoitner
 and video resize.  1/3/97

 GLUT_XLIB_IMPLEMENTATION=9 mjk's GLUT 3.4 release with early GLUT 4 routines.

 GLUT_XLIB_IMPLEMENTATION=11 Mesa 2.5's GLUT 3.6 release.

 GLUT_XLIB_IMPLEMENTATION=12 mjk's GLUT 3.6 release with early GLUT 4 routines + signal handling.
}

{$I 'Glut_Api_ver.inc'}

// ----------------------------------------------------------------------------
// ********** GLUT generic constants **********

// Display mode bit masks.
const
  GLUT_RGB          = 0;
  GLUT_RGBA         = GLUT_RGB;
  GLUT_INDEX        = 1;
  GLUT_SINGLE       = 0;
  GLUT_DOUBLE       = 2;
  GLUT_ACCUM        = 4;
  GLUT_ALPHA        = 8;
  GLUT_DEPTH        = 16;
  GLUT_STENCIL      = 32;
{$IFDEF GLUT_API_VERSION_2_ORGREATER}
  GLUT_MULTISAMPLE  = 128;
  GLUT_STEREO       = 256;
{$ENDIF}
{$IFDEF GLUT_API_VERSION_3_ORGREATER}
  GLUT_LUMINANCE    = 512;
{$ENDIF}

// Mouse buttons.
  GLUT_LEFT_BUTTON    = 0;
  GLUT_MIDDLE_BUTTON  = 1;
  GLUT_RIGHT_BUTTON   = 2;

// Mouse button  state.
  GLUT_DOWN = 0;
  GLUT_UP   = 1;

{$IFDEF GLUT_API_VERSION_2_ORGREATER}
// function keys
  GLUT_KEY_F1         = 1;
  GLUT_KEY_F2         = 2;
  GLUT_KEY_F3         = 3;
  GLUT_KEY_F4         = 4;
  GLUT_KEY_F5         = 5;
  GLUT_KEY_F6         = 6;
  GLUT_KEY_F7         = 7;
  GLUT_KEY_F8         = 8;
  GLUT_KEY_F9         = 9;
  GLUT_KEY_F10        = 10;
  GLUT_KEY_F11        = 11;
  GLUT_KEY_F12        = 12;
// directional keys */
  GLUT_KEY_LEFT       = 100;
  GLUT_KEY_UP         = 101;
  GLUT_KEY_RIGHT      = 102;
  GLUT_KEY_DOWN       = 103;
  GLUT_KEY_PAGE_UP    = 104;
  GLUT_KEY_PAGE_DOWN  = 105;
  GLUT_KEY_HOME       = 106;
  GLUT_KEY_END        = 107;
  GLUT_KEY_INSERT     = 108;
{$ENDIF}

// Entry/exit  state.
  GLUT_LEFT     = 0;
  GLUT_ENTERED  = 1;

// Menu usage  state.
  GLUT_MENU_NOT_IN_USE  = 0;
  GLUT_MENU_IN_USE      = 1;

// Visibility  state.
  GLUT_NOT_VISIBLE  = 0;
  GLUT_VISIBLE      = 1;

// Window status  state.
  GLUT_HIDDEN             = 0;
  GLUT_FULLY_RETAINED     = 1;
  GLUT_PARTIALLY_RETAINED = 2;
  GLUT_FULLY_COVERED      = 3;

// Color index component selection values.
  GLUT_RED    = 0;
  GLUT_GREEN  = 1;
  GLUT_BLUE   = 2;

// Layers for use.
  GLUT_NORMAL   = 0;
  GLUT_OVERLAY  = 1;

// Stroke font constants (use these in GLUT program).
  GLUT_STROKE_ROMAN           = pointer(0);
  GLUT_STROKE_MONO_ROMAN      = pointer(1);

// Bitmap font constants (use these in GLUT program).
  GLUT_BITMAP_9_BY_15         = pointer(2);
  GLUT_BITMAP_8_BY_13         = pointer(3);
  GLUT_BITMAP_TIMES_ROMAN_10  = pointer(4);
  GLUT_BITMAP_TIMES_ROMAN_24  = pointer(5);
{$IFDEF GLUT_API_VERSION_3_ORGREATER}
  GLUT_BITMAP_HELVETICA_10    = pointer(6);
  GLUT_BITMAP_HELVETICA_12    = pointer(7);
  GLUT_BITMAP_HELVETICA_18    = pointer(8);
{$ENDIF}

// glutGet parameters.
  GLUT_WINDOW_X                 = 100;
  GLUT_WINDOW_Y                 = 101;
  GLUT_WINDOW_WIDTH             = 102;
  GLUT_WINDOW_HEIGHT            = 103;
  GLUT_WINDOW_BUFFER_SIZE       = 104;
  GLUT_WINDOW_STENCIL_SIZE      = 105;
  GLUT_WINDOW_DEPTH_SIZE        = 106;
  GLUT_WINDOW_RED_SIZE          = 107;
  GLUT_WINDOW_GREEN_SIZE        = 108;
  GLUT_WINDOW_BLUE_SIZE         = 109;
  GLUT_WINDOW_ALPHA_SIZE        = 110;
  GLUT_WINDOW_ACCUM_RED_SIZE    = 111;
  GLUT_WINDOW_ACCUM_GREEN_SIZE  = 112;
  GLUT_WINDOW_ACCUM_BLUE_SIZE   = 113;
  GLUT_WINDOW_ACCUM_ALPHA_SIZE  = 114;
  GLUT_WINDOW_DOUBLEBUFFER      = 115;
  GLUT_WINDOW_RGBA              = 116;
  GLUT_WINDOW_PARENT            = 117;
  GLUT_WINDOW_NUM_CHILDREN      = 118;
  GLUT_WINDOW_COLORMAP_SIZE     = 119;
{$IFDEF GLUT_API_VERSION_2_ORGREATER}
  GLUT_WINDOW_NUM_SAMPLES       = 120;
  GLUT_WINDOW_STEREO            = 121;
{$ENDIF}
{$IFDEF GLUT_API_VERSION_3_ORGREATER}
  GLUT_WINDOW_CURSOR            = 122;
{$ENDIF}
  GLUT_SCREEN_WIDTH             = 200;
  GLUT_SCREEN_HEIGHT            = 201;
  GLUT_SCREEN_WIDTH_MM          = 202;
  GLUT_SCREEN_HEIGHT_MM         = 203;
  GLUT_MENU_NUM_ITEMS           = 300;
  GLUT_DISPLAY_MODE_POSSIBLE    = 400;
  GLUT_INIT_WINDOW_X            = 500;
  GLUT_INIT_WINDOW_Y            = 501;
  GLUT_INIT_WINDOW_WIDTH        = 502;
  GLUT_INIT_WINDOW_HEIGHT       = 503;
  GLUT_INIT_DISPLAY_MODE        = 504;
{$IFDEF GLUT_API_VERSION_2_ORGREATER}
  GLUT_ELAPSED_TIME             = 700;
{$ENDIF}

{$IFDEF GLUT_API_VERSION_2_ORGREATER}
// glutDeviceGet parameters.
  GLUT_HAS_KEYBOARD             = 600;
  GLUT_HAS_MOUSE                = 601;
  GLUT_HAS_SPACEBALL            = 602;
  GLUT_HAS_DIAL_AND_BUTTON_BOX  = 603;
  GLUT_HAS_TABLET               = 604;
  GLUT_NUM_MOUSE_BUTTONS        = 605;
  GLUT_NUM_SPACEBALL_BUTTONS    = 606;
  GLUT_NUM_BUTTON_BOX_BUTTONS   = 607;
  GLUT_NUM_DIALS                = 608;
  GLUT_NUM_TABLET_BUTTONS       = 609;
{$ENDIF}

{$IFDEF GLUT_API_VERSION_3_ORGREATER}
// glutLayerGet parameters.
  GLUT_OVERLAY_POSSIBLE   = 800;
  GLUT_LAYER_IN_USE       = 801;
  GLUT_HAS_OVERLAY        = 802;
  GLUT_TRANSPARENT_INDEX  = 803;
  GLUT_NORMAL_DAMAGED     = 804;
  GLUT_OVERLAY_DAMAGED    = 805;

{$IFDEF GLUT_API_VERSION_4_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF GLUT_XLIB_IMPLEMENTATION_9_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}

{$IFDEF COMPILE_THIS}
// glutVideoResizeGet parameters.
  GLUT_VIDEO_RESIZE_POSSIBLE      = 900;
  GLUT_VIDEO_RESIZE_IN_USE        = 901;
  GLUT_VIDEO_RESIZE_X_DELTA       = 902;
  GLUT_VIDEO_RESIZE_Y_DELTA       = 903;
  GLUT_VIDEO_RESIZE_WIDTH_DELTA   = 904;
  GLUT_VIDEO_RESIZE_HEIGHT_DELTA  = 905;
  GLUT_VIDEO_RESIZE_X             = 906;
  GLUT_VIDEO_RESIZE_Y             = 907;
  GLUT_VIDEO_RESIZE_WIDTH         = 908;
  GLUT_VIDEO_RESIZE_HEIGHT        = 909;
{$ENDIF}
{$UNDEF COMPILE_THIS}

// glutUseLayer parameters. (Prev. defined)
//  GLUT_NORMAL   = 0;
//  GLUT_OVERLAY  = 1;

// glutGetModifiers return mask.
  GLUT_ACTIVE_SHIFT = 1;
  GLUT_ACTIVE_CTRL  = 2;
  GLUT_ACTIVE_ALT   = 4;

// glutSetCursor parameters.
// Basic arrows.
  GLUT_CURSOR_RIGHT_ARROW         = 0;
  GLUT_CURSOR_LEFT_ARROW          = 1;
// Symbolic cursor shapes.
  GLUT_CURSOR_INFO                = 2;
  GLUT_CURSOR_DESTROY             = 3;
  GLUT_CURSOR_HELP                = 4;
  GLUT_CURSOR_CYCLE               = 5;
  GLUT_CURSOR_SPRAY               = 6;
  GLUT_CURSOR_WAIT                = 7;
  GLUT_CURSOR_TEXT                = 8;
  GLUT_CURSOR_CROSSHAIR           = 9;
// Directional cursors.
  GLUT_CURSOR_UP_DOWN             = 10;
  GLUT_CURSOR_LEFT_RIGHT          = 11;
// Sizing cursors.
  GLUT_CURSOR_TOP_SIDE            = 12;
  GLUT_CURSOR_BOTTOM_SIDE         = 13;
  GLUT_CURSOR_LEFT_SIDE           = 14;
  GLUT_CURSOR_RIGHT_SIDE          = 15;
  GLUT_CURSOR_TOP_LEFT_CORNER     = 16;
  GLUT_CURSOR_TOP_RIGHT_CORNER    = 17;
  GLUT_CURSOR_BOTTOM_RIGHT_CORNER = 18;
  GLUT_CURSOR_BOTTOM_LEFT_CORNER  = 19;
// Inherit from parent window.
  GLUT_CURSOR_INHERIT             = 100;
// Blank cursor.
  GLUT_CURSOR_NONE                = 101;
// Fullscreen crosshair (if available).
  GLUT_CURSOR_FULL_CROSSHAIR      = 102;
{$ENDIF}

// ----------------------------------------------------------------------------
// ********** Generic types **********

type
  PPChar    = ^PChar;
  TOpenGLLib = (MS_LIB, SGI_LIB);

// ********** GLUT types **********

type
// GLUT menu callback
  TGLUTMenuProc                   = procedure(MenuEntry: integer); cdecl;

// GLUT sub-API callback
  TGLUTDisplayProc                = procedure; cdecl;
  TGLUTReshapeProc                = procedure(width, height: integer); cdecl;
  TGLUTKeyboardProc               = procedure(key: byte; x, y: integer); cdecl;
  TGLUTMouseProc                  = procedure(button, state, x, y: integer); cdecl;
  TGLUTMotionProc                 = procedure(x, y: integer); cdecl;
  TGLUTPassiveMotionProc          = procedure(x, y: integer); cdecl;
  TGLUTEntryProc                  = procedure(state: integer); cdecl;
  TGLUTVisibilityProc             = procedure(state: integer); cdecl;
  TGLUTIdleProc                   = procedure; cdecl;
  TGLUTTimerProc                  = procedure(value: integer); cdecl;
  TGLUTMenuStateProc              = procedure(state: integer); cdecl;
  TGLUTSpecialProc                = procedure(key, x, y: integer); cdecl;
  TGLUTSpaceballMotionProc        = procedure(x, y, z: integer); cdecl;
  TGLUTSpaceballRotateProc        = procedure(x, y, z: integer); cdecl;
  TGLUTSpaceballButtonProc        = procedure(button, state: integer); cdecl;
  TGLUTButtonBoxProc              = procedure(button, state: integer); cdecl;
  TGLUTDialsProc                  = procedure(dial, value: integer); cdecl;
  TGLUTTabletMotionProc           = procedure(x, y: integer); cdecl;
  TGLUTTabletButtonProc           = procedure(button, state, x, y: integer); cdecl;
  TGLUTMenuStatusProc             = procedure(status, x, y: integer); cdecl;
  TGLUTOverlayDisplayProc         = procedure; cdecl;
  TGLUTWindowStatusProc           = procedure(state: integer); cdecl;


// ----------------------------------------------------------------------------
// ********** GLUT functions and procedures


// GLUT initialization sub-API.
function GlutInit(Lib: TOpenGLLib): boolean;

var
  glutInitDisplayMode: procedure(mode: DWORD); stdcall;
  glutInitDisplayString: procedure(const astring: PChar); stdcall; // GLUT 3.4+
  glutInitWindowPosition: procedure(x, y: integer); stdcall;
  glutInitWindowSize: procedure(width, height: integer); stdcall;
  glutMainLoop: procedure; stdcall;

// GLUT window sub-API.
  glutCreateWindow: function(const title: PChar): integer; stdcall;
  glutCreateSubWindow: function(win, x, y, width, height: integer): integer; stdcall;
  glutDestroyWindow: procedure(win: integer); stdcall;
  glutPostRedisplay: procedure; stdcall;
  glutPostWindowRedisplay: procedure(win: integer); stdcall; // GLUT 3.6+
  glutSwapBuffers: procedure; stdcall;
  glutGetWindow: function: integer; stdcall;
  glutSetWindow: procedure(win: integer); stdcall;
  glutSetWindowTitle: procedure(const title: PChar); stdcall;
  glutSetIconTitle: procedure(const title: PChar); stdcall;
  glutPositionWindow: procedure(x, y: integer); stdcall;
  glutReshapeWindow: procedure(width, height: integer); stdcall;
  glutPopWindow: procedure; stdcall;
  glutPushWindow: procedure; stdcall;
  glutIconifyWindow: procedure; stdcall;
  glutShowWindow: procedure; stdcall;
  glutHideWindow: procedure; stdcall;
  glutFullScreen: procedure; stdcall;                             // GLUT 3.0+
  glutSetCursor: procedure(cursor: integer); stdcall;             // GLUT 3.0+
  glutWarpPointer: procedure(x, y: integer); stdcall;               // GLUT 3.4+

// GLUT overlay sub-API.
  glutEstablishOverlay: procedure; stdcall;                       // GLUT 3.0+
  glutRemoveOverlay: procedure; stdcall;                          // GLUT 3.0+
  glutUseLayer: procedure(layer: GLenum); stdcall;                // GLUT 3.0+
  glutPostOverlayRedisplay: procedure; stdcall;                   // GLUT 3.0+
  glutPostWindowOverlayRedisplay: procedure(win: integer); stdcall; // GLUT 3.6+
  glutShowOverlay: procedure; stdcall;                            // GLUT 3.0+
  glutHideOverlay: procedure; stdcall;                            // GLUT 3.0+

// GLUT menu sub-API.
  glutCreateMenu: function(MenuProc: TGLUTMenuProc): integer; stdcall;
  glutDestroyMenu: procedure(menu: integer); stdcall;
  glutGetMenu: function: integer; stdcall;
  glutSetMenu: procedure(menu:integer); stdcall;
  glutAddMenuEntry: procedure(const alabel: PChar; value: integer); stdcall;
  glutAddSubMenu: procedure(const alabel: PChar; submenu: integer); stdcall;
  glutChangeToMenuEntry: procedure(item: integer; const alabel: PChar; value: integer); stdcall;
  glutChangeToSubMenu: procedure(item: integer; const alabel: PCHar; submenu: integer); stdcall;
  glutRemoveMenuItem: procedure(item: integer); stdcall;
  glutAttachMenu: procedure(button: integer); stdcall;
  glutDetachMenu: procedure(button: integer); stdcall;

// GLUT  sub-API. */
  glutDisplayFunc: procedure(DisplayProc: TGLUTDisplayProc); stdcall;
  glutReshapeFunc: procedure(ReshapeProc: TGLUTReshapeProc); stdcall;
  glutKeyboardFunc: procedure(KeyboardProc: TGLUTKeyboardProc); stdcall;
  glutMouseFunc: procedure(MouseProc: TGLUTMouseProc); stdcall;
  glutMotionFunc: procedure(MotionProc: TGLUTMotionProc); stdcall;
  glutPassiveMotionFunc: procedure(PassiveMotionProc: TGLUTPassiveMotionProc); stdcall;
  glutEntryFunc: procedure(EntryProc: TGLUTEntryProc); stdcall;
  glutVisibilityFunc: procedure(VisibilityProc: TGLUTVisibilityProc); stdcall;
  glutIdleFunc: procedure(IdleProc: TGLUTIdleProc); stdcall;
  glutTimerFunc: procedure(millis: DWORD; TimerProc: TGLUTTimerProc; value: integer); stdcall;
  glutMenuStateFunc: procedure(MenuStateProc: TGLUTMenuStateProc); stdcall;

  glutSpecialFunc: procedure(SpecialProc: TGLUTSpecialProc); stdcall;       // GLUT 2.0+
  glutSpaceballMotionFunc: procedure(SpaceBallMotionProc:
                                        TGLUTSpaceballMotionProc); stdcall; // GLUT 2.0+
  glutSpaceballRotateFunc: procedure(SpaceBallRotateProc:
                                        TGLUTSpaceballRotateProc); stdcall; // GLUT 2.0+
  glutSpaceballButtonFunc: procedure(SpaceBallButtonProc:
                                        TGLUTSpaceballButtonProc); stdcall; // GLUT 2.0+
  glutButtonBoxFunc: procedure(ButtonBoxProc: TGLUTButtonBoxProc); stdcall; // GLUT 2.0+
  glutDialsFunc: procedure(DialsProc: TGLUTDialsProc); stdcall;             // GLUT 2.0+
  glutTabletMotionFunc: procedure(TabletMotionProc:
                                        TGLUTTabletMotionProc); stdcall;    // GLUT 2.0+
  glutTabletButtonFunc: procedure(TabletButtonProc:
                                        TGLUTTabletButtonProc); stdcall;    // GLUT 2.0+

  glutMenuStatusFunc: procedure(MenuStatusProc:
                                        TGLUTMenuStatusProc); stdcall;      // GLUT 3.0+
  glutOverlayDisplayFunc: procedure(OverlayDispalyProc:
                                        TGLUTOverlayDisplayProc); stdcall;  // GLUT 3.0+

  glutWindowStatusFunc: procedure(WindowStatusProc:
                                        TGLUTWindowStatusProc); stdcall;    // GLUT 3.4+

// GLUT color index sub-API.
  glutSetColor: procedure(cell: integer; red, green, blue: GLfloat); stdcall;
  glutGetColor: function(cell, component: integer): GLfloat; stdcall;
  glutCopyColormap: procedure(win: integer); stdcall;

// GLUT state retrieval sub-API.
  glutGet: function(atype: GLenum): integer; stdcall;
  glutDeviceGet: function(atype: GLenum): integer; stdcall;

// GLUT extension support sub-API
  glutExtensionSupported: function(const name: PChar): integer; stdcall;    // GLUT 2.0+
  glutGetModifiers: function: integer; stdcall;                             // GLUT 3.0+
  glutLayerGet: function(atype: GLenum): integer; stdcall;                  // GLUT 3.0+

// GLUT font sub-API
  glutBitmapCharacter: procedure(font: pointer; character: integer); stdcall;
  glutBitmapWidth: function(font: pointer; character: integer): integer; stdcall;
  glutStrokeCharacter: procedure(font: pointer; character: integer); stdcall;
  glutStrokeWidth: function(font: pointer; character: integer): integer; stdcall;

  glutBitmapLength: function(font: pointer;
                                  const astring: PUChar): integer; stdcall; // GLUT 3.4+
  glutStrokeLength: function(font: pointer;
                                  const astring: PUChar): integer; stdcall; // GLUT 3.4+

// GLUT pre-built models sub-API
  glutWireSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidSphere: procedure(radius: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutSolidCone: procedure(base, height: GLdouble; slices, stacks: GLint); stdcall;
  glutWireCube: procedure(size: GLdouble); stdcall;
  glutSolidCube: procedure(size: GLdouble); stdcall;
  glutWireTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutSolidTorus: procedure(innerRadius, outerRadius: GLdouble; sides, rings: GLint); stdcall;
  glutWireDodecahedron: procedure; stdcall;
  glutSolidDodecahedron: procedure; stdcall;
  glutWireTeapot: procedure(size: GLdouble); stdcall;
  glutSolidTeapot: procedure(size: GLdouble); stdcall;
  glutWireOctahedron: procedure; stdcall;
  glutSolidOctahedron: procedure; stdcall;
  glutWireTetrahedron: procedure; stdcall;
  glutSolidTetrahedron: procedure; stdcall;
  glutWireIcosahedron: procedure; stdcall;
  glutSolidIcosahedron: procedure; stdcall;

// GLUT video resize sub-API.
  glutVideoResizeGet: function(param: GLenum): integer; stdcall;      // GLUT 3.4+
  glutSetupVideoResizing: procedure; stdcall;                         // GLUT 3.4+
  glutStopVideoResizing: procedure; stdcall;                          // GLUT 3.4+
  glutVideoResize: procedure(x, y, width, height: integer); stdcall;  // GLUT 3.4+
  glutVideoPan: procedure(x, y, width, height: integer); stdcall;     // GLUT 3.4+

// GLUT debugging sub-API.
  glutReportErrors: procedure; stdcall;                               // GLUT 3.4+

//-----------------------------------------------------------------------------

type
  PArgArray   = ^TArgArray;
  TArgArray   = array[0..1000] of PChar;

function GlutLoadLibrary(Lib: TOpenGLLib): boolean;
procedure GlutFreeLibrary;

//-----------------------------------------------------------------------------

implementation

var
  GLUTHandle: HINST = 0;
  LibraryInit: boolean = False;
  _glutInit: procedure(argcp: PInteger; argv: PPChar); stdcall;

const
  SGI_OGL = 'OpenGL.dll';
  SGI_GLU = 'GLU.dll';
  SGI_GLUT = 'Glut.dll';
  MS_OGL = 'OpenGL32.dll';
  MS_GLU = 'GLU32.dll';
  MS_GLUT = 'Glut32.dll';

procedure ClearProcs;
begin
  _glutInit:= nil;
  glutInitDisplayMode:= nil;
  glutInitDisplayString:= nil;
  glutInitWindowPosition:= nil;
  glutInitWindowSize:= nil;
  glutMainLoop:= nil;
  glutCreateWindow:= nil;
  glutCreateSubWindow:= nil;
  glutDestroyWindow:= nil;
  glutPostRedisplay:= nil;
  glutPostWindowRedisplay:= nil;
  glutSwapBuffers:= nil;
  glutGetWindow:= nil;
  glutSetWindow:= nil;
  glutSetWindowTitle:= nil;
  glutSetIconTitle:= nil;
  glutPositionWindow:= nil;
  glutReshapeWindow:= nil;
  glutPopWindow:= nil;
  glutPushWindow:= nil;
  glutIconifyWindow:= nil;
  glutShowWindow:= nil;
  glutHideWindow:= nil;
  glutFullScreen:= nil;
  glutSetCursor:= nil;
  glutWarpPointer:= nil;
  glutEstablishOverlay:= nil;
  glutRemoveOverlay:= nil;
  glutUseLayer:= nil;
  glutPostOverlayRedisplay:= nil;
  glutPostWindowOverlayRedisplay:= nil;
  glutShowOverlay:= nil;
  glutHideOverlay:= nil;
  glutCreateMenu:= nil;
  glutDestroyMenu:= nil;
  glutGetMenu:= nil;
  glutSetMenu:= nil;
  glutAddMenuEntry:= nil;
  glutAddSubMenu:= nil;
  glutChangeToMenuEntry:= nil;
  glutChangeToSubMenu:= nil;
  glutRemoveMenuItem:= nil;
  glutAttachMenu:= nil;
  glutDetachMenu:= nil;
  glutDisplayFunc:= nil;
  glutReshapeFunc:= nil;
  glutKeyboardFunc:= nil;
  glutMouseFunc:= nil;
  glutMotionFunc:= nil;
  glutPassiveMotionFunc:= nil;
  glutEntryFunc:= nil;
  glutVisibilityFunc:= nil;
  glutIdleFunc:= nil;
  glutTimerFunc:= nil;
  glutMenuStateFunc:= nil;
  glutSpecialFunc:= nil;
  glutSpaceballMotionFunc:= nil;
  glutSpaceballRotateFunc:= nil;
  glutSpaceballButtonFunc:= nil;
  glutButtonBoxFunc:= nil;
  glutDialsFunc:= nil;
  glutTabletMotionFunc:= nil;
  glutTabletButtonFunc:= nil;
  glutMenuStatusFunc:= nil;
  glutOverlayDisplayFunc:= nil;
  glutWindowStatusFunc:= nil;
  glutSetColor:= nil;
  glutGetColor:= nil;
  glutCopyColormap:= nil;
  glutGet:= nil;
  glutDeviceGet:= nil;
  glutExtensionSupported:= nil;
  glutGetModifiers:= nil;
  glutLayerGet:= nil;
  glutBitmapCharacter:= nil;
  glutBitmapWidth:= nil;
  glutStrokeCharacter:= nil;
  glutStrokeWidth:= nil;
  glutBitmapLength:= nil;
  glutStrokeLength:= nil;
  glutWireSphere:= nil;
  glutSolidSphere:= nil;
  glutWireCone:= nil;
  glutSolidCone:= nil;
  glutWireCube:= nil;
  glutSolidCube:= nil;
  glutWireTorus:= nil;
  glutSolidTorus:= nil;
  glutWireDodecahedron:= nil;
  glutSolidDodecahedron:= nil;
  glutWireTeapot:= nil;
  glutSolidTeapot:= nil;
  glutWireOctahedron:= nil;
  glutSolidOctahedron:= nil;
  glutWireTetrahedron:= nil;
  glutSolidTetrahedron:= nil;
  glutWireIcosahedron:= nil;
  glutSolidIcosahedron:= nil;
  glutVideoResizeGet:= nil;
  glutSetupVideoResizing:= nil;
  glutStopVideoResizing:= nil;
  glutVideoResize:= nil;
  glutVideoPan:= nil;
  glutReportErrors:= nil;
end;

procedure LoadProcs;
begin
  _glutInit:= GetProcAddress(GlutHandle, 'glutInit');
  glutInitDisplayMode:= GetProcAddress(GlutHandle, 'glutInitDisplayMode');

{$IFDEF GLUT_API_VERSION_4_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF GLUT_XLIB_IMPLEMENTATION_9_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF COMPILE_THIS}
  glutInitDisplayString:= GetProcAddress(GlutHandle, 'glutInitDisplayString');
{$ENDIF}
{$UNDEF COMPILE_THIS}

  glutInitWindowPosition:= GetProcAddress(GlutHandle, 'glutInitWindowPosition');
  glutInitWindowSize:= GetProcAddress(GlutHandle, 'glutInitWindowSize');
  glutMainLoop:= GetProcAddress(GlutHandle, 'glutMainLoop');
  glutCreateWindow:= GetProcAddress(GlutHandle, 'glutCreateWindow');
  glutCreateSubWindow:= GetProcAddress(GlutHandle, 'glutCreateSubWindow');
  glutDestroyWindow:= GetProcAddress(GlutHandle, 'glutDestroyWindow');
  glutPostRedisplay:= GetProcAddress(GlutHandle, 'glutPostRedisplay');

{$IFDEF GLUT_API_VERSION_4_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF GLUT_XLIB_IMPLEMENTATION_11_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF COMPILE_THIS}
  glutPostWindowRedisplay:= GetProcAddress(GlutHandle, 'glutPostWindowRedisplay');
{$ENDIF}
{$UNDEF COMPILE_THIS}

  glutSwapBuffers:= GetProcAddress(GlutHandle, 'glutSwapBuffers');
  glutGetWindow:= GetProcAddress(GlutHandle, 'glutGetWindow');
  glutSetWindow:= GetProcAddress(GlutHandle, 'glutSetWindow');
  glutSetWindowTitle:= GetProcAddress(GlutHandle, 'glutSetWindowTitle');
  glutSetIconTitle:= GetProcAddress(GlutHandle, 'glutSetIconTitle');
  glutPositionWindow:= GetProcAddress(GlutHandle, 'glutPositionWindow');
  glutReshapeWindow:= GetProcAddress(GlutHandle, 'glutReshapeWindow');
  glutPopWindow:= GetProcAddress(GlutHandle, 'glutPopWindow');
  glutPushWindow:= GetProcAddress(GlutHandle, 'glutPushWindow');
  glutIconifyWindow:= GetProcAddress(GlutHandle, 'glutIconifyWindow');
  glutShowWindow:= GetProcAddress(GlutHandle, 'glutShowWindow');
  glutHideWindow:= GetProcAddress(GlutHandle, 'glutHideWindow');

{$IFDEF GLUT_API_VERSION_3_ORGREATER}
  glutFullScreen:= GetProcAddress(GlutHandle, 'glutFullScreen');
  glutSetCursor:= GetProcAddress(GlutHandle, 'glutSetCursor');

{$IFDEF GLUT_API_VERSION_4_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF GLUT_XLIB_IMPLEMENTATION_9_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF COMPILE_THIS}
  glutWarpPointer:= GetProcAddress(GlutHandle, 'glutWarpPointer');
{$ENDIF}
{$UNDEF COMPILE_THIS}

  glutEstablishOverlay:= GetProcAddress(GlutHandle, 'glutEstablishOverlay');
  glutRemoveOverlay:= GetProcAddress(GlutHandle, 'glutRemoveOverlay');
  glutUseLayer:= GetProcAddress(GlutHandle, 'glutUseLayer');
  glutPostOverlayRedisplay:= GetProcAddress(GlutHandle, 'glutPostOverlayRedisplay');

{$IFDEF GLUT_API_VERSION_4_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF GLUT_XLIB_IMPLEMENTATION_11_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF COMPILE_THIS}
  glutPostWindowOverlayRedisplay:= GetProcAddress(GlutHandle, 'glutPostWindowOverlayRedisplay');
{$ENDIF}
{$UNDEF COMPILE_THIS}

  glutShowOverlay:= GetProcAddress(GlutHandle, 'glutShowOverlay');
  glutHideOverlay:= GetProcAddress(GlutHandle, 'glutHideOverlay');
{$ENDIF}

  glutCreateMenu:= GetProcAddress(GlutHandle, 'glutCreateMenu');
  glutDestroyMenu:= GetProcAddress(GlutHandle, 'glutDestroyMenu');
  glutGetMenu:= GetProcAddress(GlutHandle, 'glutGetMenu');
  glutSetMenu:= GetProcAddress(GlutHandle, 'glutSetMenu');
  glutAddMenuEntry:= GetProcAddress(GlutHandle, 'glutAddMenuEntry');
  glutAddSubMenu:= GetProcAddress(GlutHandle, 'glutAddSubMenu');
  glutChangeToMenuEntry:= GetProcAddress(GlutHandle, 'glutChangeToMenuEntry');
  glutChangeToSubMenu:= GetProcAddress(GlutHandle, 'glutChangeToSubMenu');
  glutRemoveMenuItem:= GetProcAddress(GlutHandle, 'glutRemoveMenuItem');
  glutAttachMenu:= GetProcAddress(GlutHandle, 'glutAttachMenu');
  glutDetachMenu:= GetProcAddress(GlutHandle, 'glutDetachMenu');
  glutDisplayFunc:= GetProcAddress(GlutHandle, 'glutDisplayFunc');
  glutReshapeFunc:= GetProcAddress(GlutHandle, 'glutReshapeFunc');
  glutKeyboardFunc:= GetProcAddress(GlutHandle, 'glutKeyboardFunc');
  glutMouseFunc:= GetProcAddress(GlutHandle, 'glutMouseFunc');
  glutMotionFunc:= GetProcAddress(GlutHandle, 'glutMotionFunc');
  glutPassiveMotionFunc:= GetProcAddress(GlutHandle, 'glutPassiveMotionFunc');
  glutEntryFunc:= GetProcAddress(GlutHandle, 'glutEntryFunc');
  glutVisibilityFunc:= GetProcAddress(GlutHandle, 'glutVisibilityFunc');
  glutIdleFunc:= GetProcAddress(GlutHandle, 'glutIdleFunc');
  glutTimerFunc:= GetProcAddress(GlutHandle, 'glutTimerFunc');
  glutMenuStateFunc:= GetProcAddress(GlutHandle, 'glutMenuStateFunc');

{$IFDEF GLUT_API_VERSION_2_ORGREATER}
  glutSpecialFunc:= GetProcAddress(GlutHandle, 'glutSpecialFunc');
  glutSpaceballMotionFunc:= GetProcAddress(GlutHandle, 'glutSpaceballMotionFunc');
  glutSpaceballRotateFunc:= GetProcAddress(GlutHandle, 'glutSpaceballRotateFunc');
  glutSpaceballButtonFunc:= GetProcAddress(GlutHandle, 'glutSpaceballButtonFunc');
  glutButtonBoxFunc:= GetProcAddress(GlutHandle, 'glutButtonBoxFunc');
  glutDialsFunc:= GetProcAddress(GlutHandle, 'glutDialsFunc');
  glutTabletMotionFunc:= GetProcAddress(GlutHandle, 'glutTabletMotionFunc');
  glutTabletButtonFunc:= GetProcAddress(GlutHandle, 'glutTabletButtonFunc');

{$IFDEF GLUT_API_VERSION_3_ORGREATER}
  glutMenuStatusFunc:= GetProcAddress(GlutHandle, 'glutMenuStatusFunc');
  glutOverlayDisplayFunc:= GetProcAddress(GlutHandle, 'glutOverlayDisplayFunc');

{$IFDEF GLUT_API_VERSION_4_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF GLUT_XLIB_IMPLEMENTATION_9_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF COMPILE_THIS}
  glutWindowStatusFunc:= GetProcAddress(GlutHandle, 'glutWindowStatusFunc');
{$ENDIF}
{$UNDEF COMPILE_THIS}

{$ENDIF}
{$ENDIF}

  glutSetColor:= GetProcAddress(GlutHandle, 'glutSetColor');
  glutGetColor:= GetProcAddress(GlutHandle, 'glutGetColor');
  glutCopyColormap:= GetProcAddress(GlutHandle, 'glutCopyColormap');
  glutGet:= GetProcAddress(GlutHandle, 'glutGet');
  glutDeviceGet:= GetProcAddress(GlutHandle, 'glutDeviceGet');

{$IFDEF GLUT_API_VERSION_2_ORGREATER}
  glutExtensionSupported:= GetProcAddress(GlutHandle, 'glutExtensionSupported');
{$ENDIF}

{$IFDEF GLUT_API_VERSION_3_ORGREATER}
  glutGetModifiers:= GetProcAddress(GlutHandle, 'glutGetModifiers');
  glutLayerGet:= GetProcAddress(GlutHandle, 'glutLayerGet');
{$ENDIF}

  glutBitmapCharacter:= GetProcAddress(GlutHandle, 'glutBitmapCharacter');
  glutBitmapWidth:= GetProcAddress(GlutHandle, 'glutBitmapWidth');
  glutStrokeCharacter:= GetProcAddress(GlutHandle, 'glutStrokeCharacter');
  glutStrokeWidth:= GetProcAddress(GlutHandle, 'glutStrokeWidth');

{$IFDEF GLUT_API_VERSION_4_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF GLUT_XLIB_IMPLEMENTATION_9_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF COMPILE_THIS}
  glutBitmapLength:= GetProcAddress(GlutHandle, 'glutBitmapLength');
  glutStrokeLength:= GetProcAddress(GlutHandle, 'glutStrokeLength');
{$ENDIF}
{$UNDEF COMPILE_THIS}

  glutWireSphere:= GetProcAddress(GlutHandle, 'glutWireSphere');
  glutSolidSphere:= GetProcAddress(GlutHandle, 'glutSolidSphere');
  glutWireCone:= GetProcAddress(GlutHandle, 'glutWireCone)');
  glutSolidCone:= GetProcAddress(GlutHandle, 'glutSolidCone');
  glutWireCube:= GetProcAddress(GlutHandle, 'glutWireCube');
  glutSolidCube:= GetProcAddress(GlutHandle, 'glutSolidCube');
  glutWireTorus:= GetProcAddress(GlutHandle, 'glutWireTorus');
  glutSolidTorus:= GetProcAddress(GlutHandle, 'glutSolidTorus');
  glutWireDodecahedron:= GetProcAddress(GlutHandle, 'glutWireDodecahedron');
  glutSolidDodecahedron:= GetProcAddress(GlutHandle, 'glutSolidDodecahedron');
  glutWireTeapot:= GetProcAddress(GlutHandle, 'glutWireTeapot');
  glutSolidTeapot:= GetProcAddress(GlutHandle, 'glutSolidTeapot');
  glutWireOctahedron:= GetProcAddress(GlutHandle, 'glutWireOctahedron');
  glutSolidOctahedron:= GetProcAddress(GlutHandle, 'glutSolidOctahedron');
  glutWireTetrahedron:= GetProcAddress(GlutHandle, 'glutWireTetrahedron');
  glutSolidTetrahedron:= GetProcAddress(GlutHandle, 'glutSolidTetrahedron');
  glutWireIcosahedron:= GetProcAddress(GlutHandle, 'glutWireIcosahedron');
  glutSolidIcosahedron:= GetProcAddress(GlutHandle, 'glutSolidIcosahedron');

{$IFDEF GLUT_API_VERSION_4_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF GLUT_XLIB_IMPLEMENTATION_9_ORGREATER}
  {$DEFINE COMPILE_THIS}
{$ENDIF}
{$IFDEF COMPILE_THIS}
  glutVideoResizeGet:= GetProcAddress(GlutHandle, 'glutVideoResizeGet');
  glutSetupVideoResizing:= GetProcAddress(GlutHandle, 'glutSetupVideoResizing');
  glutStopVideoResizing:= GetProcAddress(GlutHandle, 'glutStopVideoResizing');
  glutVideoResize:= GetProcAddress(GlutHandle, 'glutVideoResize');
  glutVideoPan:= GetProcAddress(GlutHandle, 'glutVideoPan');
  glutReportErrors:= GetProcAddress(GlutHandle, 'glutReportErrors');
{$ENDIF}
{$UNDEF COMPILE_THIS}
end;

function GlutInit(Lib: TOpenGLLib): boolean;
var
  argc: integer;
  argvTemp,
  argv: PPChar;
  i: integer;
begin
  Result:= False;
  if LibraryInit then
    exit;
  if (GLUTHandle = 0) then begin
    if not(GlutLoadLibrary(Lib)) then
      exit;
  end;
  argc:= ParamCount + 1;
  GetMem(argv, SizeOf(PChar)*(argc+1));
  argvTemp:= argv;
  try
    for i := 0 to ParamCount do begin
      argvTemp^ := PChar(ParamStr(i));
      inc(argvTemp);
    end;
    argvTemp^ := nil;
    _glutInit(@argc, argv);
  finally
    FreeMem(argv);
  end;
  LibraryInit:= True;
  Result:= True;
end;

function GlutLoadLibrary(Lib: TOpenGLLib): boolean;
var
  OldEMode: LongInt;
begin
  Result:= False;
  GlutFreeLibrary;
  OldEMode:= SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    if (Lib = SGI_LIB) then
      GLUTHandle:= LoadLibrary(SGI_GLUT)
    else
      GLUTHandle:= LoadLibrary(MS_GLUT);
    if (GLUTHandle > 0) then begin
      LoadProcs;
      Result:= True;
    end;
  finally
    SetErrorMode(OldEMode);
  end;
end;

procedure GlutFreeLibrary;
begin
  if (GLUTHandle > 0) then begin
    FreeLibrary(GLUTHandle);
    GLUTHandle:= 0;
  end;
  ClearProcs;
  LibraryInit:= False;
end;

initialization
finalization
  GlutFreeLibrary;
end.
