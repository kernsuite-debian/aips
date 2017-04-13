/*--------------------------------------------------------------------*/
/*! XAS header file                                                   */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-2003, 2008, 2011-2012                         */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;                                                                   */
/*;  This program is free software; you can redistribute it and/or    */
/*;  modify it under the terms of the GNU General Public License as   */
/*;  published by the Free Software Foundation; either version 2 of   */
/*;  the License, or (at your option) any later version.              */
/*;                                                                   */
/*;  This program is distributed in the hope that it will be useful,  */
/*;  but WITHOUT ANY WARRANTY; without even the implied warranty of   */
/*;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
/*;  GNU General Public License for more details.                     */
/*;                                                                   */
/*;  You should have received a copy of the GNU General Public        */
/*;  License along with this program; if not, write to the Free       */
/*;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     */
/*;  MA 02139, USA.                                                   */
/*;                                                                   */
/*;  Correspondence concerning AIPS should be addressed as follows:   */
/*;         Internet email: aipsmail@nrao.edu.                        */
/*;         Postal address: AIPS Project Office                       */
/*;                         National Radio Astronomy Observatory      */
/*;                         520 Edgemont Road                         */
/*;                         Charlottesville, VA 22903-2475 USA        */
/*--------------------------------------------------------------------*/
/* #define BSD 1  */                    /* Select operating system    */
/* #define VMS 0  */                    /* set desired system to 1    */
#ifdef _BSD                             /* Unix needs BSD = 1         */
#define BSD 1
#undef VMS                              /* Do not us VMS specific code*/
#endif
#ifdef _VMS                             /* DEC VMS                    */
#define VMS 1
#undef BSD                              /* No Unix-specific code      */
#endif
#ifdef _AIX                             /* IBM AIX needs BSD = 1 also */
#define AIX 1                           /* use AIX specific code      */
#undef VMS                              /* Do not us VMS specific code*/
#endif

#include <stdio.h>
                                        /* We must override MIT error */
                                        /* in prototype               */
#if __STDC__
#define XIOErrorHandler XFuckedUp1
#define XSetIOErrorHandler XFuckedUp2
#endif
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#if __STDC__
#undef  XIOErrorHandler
#undef  XSetIOErrorHandler
#endif

#if BSD

/* #include <X11/bitmaps/xlogo64> */

/* Header info needed for socket routines (including i/o) */

#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <ctype.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netdb.h>
#include <netinet/in.h>

/* Header for shared memory extension */

#ifdef USE_SHM
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>
#endif

struct sockaddr_un server_un;
struct sockaddr_in server_in;
struct servent *sp_in;

Bool connected;
int AipsSocket, AipsLink;
int domain_type;
#define UNIX_DOMAIN 0
#define INET_DOMAIN 1

#endif /* BSD */


#if VMS

/* ##include "xlogo64." */
#include <ssdef.h>
#include <iodef.h>
#include <descrip.h>

#define XAS_SOCKET "XasSocket"     /* mailbox name AIPS --> XAS */
#define AIPS_SOCKET "AipsSocket"   /* mailbox name XAS --> AIPS */

#define emask ExposureMask|KeyPressMask|StructureNotifyMask|PointerMotionMask

#define maxmsg 16400
#define bufquo 16400

typedef struct {
   short int ioresult;
   short int iolength;
   int unused;
} IOSB;

int XasLink, AipsLink;
IOSB read_status, write_status;
int read_in_progress, write_in_progress;
static  (dtime, "0 00:00:00.25");
int delta[2];

#endif /* VMS */

                                        /* Screen parameters          */
int Ngrey, NgreyUser;
#define NGREYMAX 16
#define NGRAPH   8
#define NGRPHCOL 19
                                        /* total number of planes     */
                                        /* (grey-scale + graphics)    */
#define NGRTOT   (NGREYMAX+NGRAPH)
#define MAXZOOM  16
#define MAXCAT   (MAXZOOM*MAXZOOM)
                                        /* Border allocations         */
/* #define TEXT_SPACE      0 */         /* empty space @ bottom screen*/
#define TEXT_SPACE      69              /* preferred at NRAO?         */
                                        /* I can find no way to ask   */
                                        /* the window manager the size*/
                                        /* of its top banner.         */
                                        /* They need to be visible to */
                                        /* allow window resize, move..*/
#ifdef AIX                              /* Numbers for IBM Motif      */
#define SCREEN_LEFT    11
#define SCREEN_RIGHT   11
#define SCREEN_TOP     34
#define SCREEN_BOTTOM  (11 + TEXT_SPACE)
#else                                   /* Numbers for SUN OpenLook   */
#define SCREEN_LEFT     5
#define SCREEN_RIGHT    5
#define SCREEN_TOP     26
#define SCREEN_BOTTOM   (5 + TEXT_SPACE)
#endif
                                        /* total screen size          */
                                        /* used in positioning        */
int twidth, theight, bwid;
                                        /* size of logical screen     */
                                        /* must be EVEN numbers !     */
int Screen_Height, Screen_Width, visnum;
int Cur_Xsize, Cur_Ysize, Cur_Xzero, Cur_Yzero;
                                        /* number grey levels in:     */
                                        /* total levels: NColour+1-19 */
int NColour, NValue, OColour;           /* for graphics, and cursor   */
                                        /* number of grey-scale (OFM) */
                                        /* intensities                */
#define NINTENS       2046
#define FNINTENS     (NGREYMAX*NINTENS)
#define COLORSHIFT      8               /* 255 max from ofm then shift*/
                                        /* 8 bits left for col table  */

                                        /* cursor [0], graphics [1-8] */
int rgrfx[9], ggrfx[9], bgrfx[9], rgcol[20], ggcol[20], bgcol[20];
int grfvc[256];
float Gamma ;

#define OK 0

#define NPARMS 4
typedef struct {
   short int
      opcode,
      parms[NPARMS],
      data_length;
   union {
      int ldata[8192];
      short int idata[16384];
      unsigned char data[32768];
      } u;
} XASinput;
typedef struct {
   short int
      return_data_length,
      status;
   union {
      int ldata[8192];
      short int idata[16384];
      unsigned char data[32768];
      } u;
} XASoutput;

                                        /* Global variables           */
XASinput xbuf;                          /* I/O buffer for AIPS        */
XASoutput ybuf;                         /* I/O buffer for AIPS        */
int Z_sndbuf, Z_rcvbuf, Z_sndini, Z_rcvini; /* I/O buffer sizes       */
Bool XasDebug;                          /* Toggle with F8             */
Bool XDebug;                            /* Toggle with F9             */
Bool ByteSwapped;                       /* Local byte swapped machine */
Bool ByteSwapDisp;                      /* Display byte swapped       */
Bool RedBigger;                         /* Color mask value R>B       */
Display *display;
int screen_num;
Cursor cursor;
Window win;
GC ImageGC;                             /* X11 graphics contexts for  */
                                        /* drawing images & graphics  */
                                        /* Image data structures:     */
XImage *plane[NGREYMAX];                /* grey-scale planes          */
XImage *line;                           /* buffer for zoomed img line */
XImage *gline;                          /* buffer for zoomed graphics */
                                        /* line                       */
/* All graphs are kept in one plane via a binary trick                */
/* a pixel value of 1 means only graph 1 on, 2 means only graph 2 on  */
/* a pixel value of 4 means only graph 3 on, 8 means only graph 4 on  */
/* a pixel value of 3 means both graph 1 and 2 on, 15 means all 4 on, */
/* etc. */
XImage *graph;                          /* graphics overlay           */
short int *plane_data[NGREYMAX];        /* data storage               */
unsigned char *plane_short[NGREYMAX];   /* data storage               */
unsigned char *line_data;
unsigned char *graph_data;
int *line_idata;

                                        /* Shared memory              */
int using_shm;                          /* set True if using SHM      */
#ifdef USE_SHM
XShmSegmentInfo plane_info[NGREYMAX];   /* shm info for image planes  */
XShmSegmentInfo graph_info;             /* shm info for graphics plane*/
XShmSegmentInfo line_info;              /* shm info for zoom buffers  */
#endif

int rwgraph, depth;
unsigned char gph_mask;

int using_24b, BitsPerPixel ;
int rofm[FNINTENS], gofm[FNINTENS],     /* red, green and blue OFM    */
    bofm[FNINTENS];                     /* registers                  */
                                        /* red, green and blue LUT    */
int *rlut[NGREYMAX], *glut[NGREYMAX], *blut[NGREYMAX];
int *pix2int;                           /* input int <- assigned pixv */
unsigned long *int2pix;                 /* input int -> assigned pixv */

int TvStatus[NGREYMAX+NGRAPH];          /* TV Image + Graphics status */

int params[256];                        /* DTVC.INC                   */

char imgcat[NGREYMAX][MAXCAT][1024] ;
char grpcat[NGRAPH][1024];
int  dircat[NGREYMAX][MAXCAT][5] ;
char typcat[NGREYMAX][MAXCAT][2] ;
int  dirgcat[NGRAPH][5] ;
char typgcat[NGRAPH][2] ;
int  lastcat[NGREYMAX];

int cur_chan[NGREYMAX], on_chan[NGREYMAX][3][4], x_split, y_split, Q_same;
int cursor_x, cursor_y;
int size_i2;
int char_mult;

int big_screen;                         /* True if screen is at max.  */
                                        /* size, FALSE otherwise      */
int cur_xcorn, cur_ycorn;               /* current location of top-   */
                                        /* left corner of smaller     */
                                        /* window                     */
int cur_xsize, cur_ysize;               /* current size of smaller    */
                                        /* window                     */

Colormap TV_colour;
XColor *colour_table;
XColor fg_curs, bg_curs;
int cursor_shape;                      /* standard cursor shape number*/
int start_icon;                        /* > 0 -> start in icon */
unsigned long curs_pixel[2];
unsigned char image_offset;
unsigned char graphics_offset;

int button_a, button_b, button_c, button_d;
int upleft_x[NGREYMAX], upleft_y[NGREYMAX], upleft_mag;
int sc_centre_x, sc_centre_y, sc_zoom_mag;
int sc_width, sc_height, sc_width2, sc_height2;
int ic_width, ic_height, ic_xcorn, ic_ycorn;
int scrhold, numhold, Maxhold, uxs, uxe, uys, uye;

char *ProgName;

                                        /* Useful macros              */
#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

#define intswap(a,b) { int tmp; tmp=a; a=b; b=tmp; }

#define Memory_x(aips_x)  ((aips_x) - 1)
#define Memory_y(aips_y)  (Screen_Height - (aips_y))
#define Aips_x(memory_x)  ((memory_x) + 1)
#define Aips_y(memory_y)  (Screen_Height - (memory_y))
#define chg_s(var,val,msk) ((var) = ((var)&(~(msk)))|((val) ? (msk) : 0))
#define chg_g(var,msk) (((var)&(msk)) ? 1 : 0)

int buffered;                           /* True if socket is          */
                                        /* buffered                   */
/* Defined opcodes */
#define NUMOP   83                      /* Largest opcode             */

int bufferop[NUMOP+1];                  /* bufferop[OP] is True if    */
                                        /* there is no status return  */
                                        /* in buffered mode           */
#define INTGT   10     /* Interrogate: get XAS major parameters       */
#define OPEN    11     /* Opens the XAS connection                    */
#define CLOSE   12     /* Close the XAS, allows new connections       */
#define INTGTO  13     /* Interrogate: get XAS old major parameters   */
#define WINDO   14     /* Read, write the X window size               */
#define INITO   15     /* init the TV old form                        */
#define VIEW    16     /* Do/hold screen updates                      */
#define PSAVEO  17     /* save DTVC.INC parameters old                */
#define XDIE    18     /* close down XAS and lock servers             */
#define IMWRT   19     /* Write image line to some channel            */
#define IMRD    20     /* Read image line from some channel           */
#define OIMWRT  21     /* Write image line to some channel OLD        */
#define OIMRD   22     /* Read image line from some channel OLD       */
#define OFILL   23     /* fill area in some or all channels           */
#define CLEAR   24     /* Clear some or all channels                  */
#define OVECT   25     /* connect two points with line                */
#define CHARS   26     /* write character string                      */
#define FILL    27     /* fill area in some or all channels           */
#define VECT    28     /* connect two points with line                */
#define INIT    29     /* init the TV                                 */
#define CATIN   31     /* Init image catalog                          */
#define CATRD   32     /* Read image catalog                          */
#define CATWR   33     /* Write image catalog                         */
#define CATOV   34     /* Use image catalog - covered?                */
#define CATFI   35     /* Find from image catalog                     */
#define PSAVE   36     /* save DTVC.INC parameters                    */
#define WOFM    37     /* Write OFM.                                  */
#define ROFM    38     /* Read OFM.                                   */
#define WLUT    39     /* Write LUT to a channel.                     */
#define RLUT    40     /* Read LUT to a channel.                      */
#define WLOT    41     /* Write LUT to a channel old                  */
#define RLOT    42     /* Read LUT to a channel old                   */
#define WOFMO   43     /* Write OFM old form                          */
#define ROFMO   44     /* Read OFM old form                           */
#define GRAPH   45     /* On/off graphics channel(s)                  */
#define SPLOT   46     /* On/off image channels(s) - obsolete         */
#define SPLAT   47     /* On/off image channels(s) - replace 2/08     */
#define SPLIT   48     /* On/off image channels(s)                    */
#define WGRFX   51     /* Write graphics/cursor colours               */
#define RGRFX   52     /* Read  graphics/cursor colours               */
#define RCURS   61     /* Read the cursor position.                   */
#define RBUTT   62     /* Read the status of the buttons              */
#define WCURS   63     /* Write the cursor position.                  */
#define RCURB   64     /* Read the cursor position and buttons        */
#define CHMULT  82     /* Force character size multiply factor        */
#define WZSCR   83     /* Write zoom/scroll to XAS using ULC          */

static char *opcodes[NUMOP+1] = {

"CODE0 ","CODE1 ","CODE2 ","CODE3 ","CODE4 ",
"CODE5 ","CODE6 ","CODE7 ","CODE8 ","CODE9 ",
"INTGT ","OPEN  ","CLOSE ","INTGTO","WINDO ",
"INITO ","VIEW  ","PSAVEO","XDIE  ","IMWRT ",
"IMRD  ","OIMWRT","OIMRD ","OFILL ","CLEAR ",
"OVECT ","CHARS ","FILL  ","VECT  ","INIT  ",
"CODE30","CATIN ","CATRD ","CATWR ","CATOV ",
"CATFI ","PSAVE ","WOFM  ","ROFM  ","WLUT  ",
"RLUT  ","WLOT  ","RLOT  ","WOFMO ","ROFMO ",
"GRAPH ","SPLOT ","SPLAT ","SPLIT ","CODE49",
"CODE50","WGRFX ","RGRFX ","CODE53","CODE54",
"CODE55","CODE56","CODE57","CODE58","CODE59",
"CODE60","RCURS ","RBUTT ","WCURS ","RCURB ",
"CODE65","CODE66","CODE67","CODE68","CODE69",
"CODE70","CODE71","CODE72","CODE73","CODE74",
"CODE75","CODE76","CODE77","CODE78","CODE79",
"CODE80","CODE81","CHMULT","WZSCR "

   };

static char  *event_names[] = {
        "",
        "",
        "KeyPress",
        "KeyRelease",
        "ButtonPress",
        "ButtonRelease",
        "MotionNotify",
        "EnterNotify",
        "LeaveNotify",
        "FocusIn",
        "FocusOut",
        "KeymapNotify",
        "Expose",
        "GraphicsExpose",
        "NoExpose",
        "VisibilityNotify",
        "CreateNotify",
        "DestroyNotify",
        "UnmapNotify",
        "MapNotify",
        "MapRequest",
        "ReparentNotify",
        "ConfigureNotify",
        "ConfigureRequest",
        "GravityNotify",
        "ResizeRequest",
        "CirculateNotify",
        "CirculateRequest",
        "PropertyNotify",
        "SelectionClear",
        "SelectionRequest",
        "SelectionNotify",
        "ColormapNotify",
        "ClientMessage",
        "MappingNotify"
   };

/* Prototyp section */

/* Override the MIT Standard prototype declarations which are wrong */
/* on all machines                                                  */
#if __STDC__
   typedef int (*XIOErrorHandler) ( Display*, XErrorEvent * );

   extern XIOErrorHandler XSetIOErrorHandler ( XIOErrorHandler );
#endif

#if __STDC__
   void  ProcessAipsRequest ( void );
   int   scrwrt  ( int, int, int, int );
   void  scrdo   ( int, int, int, int, int );
   void  scrdoit ( int, int, int, int, int, int, int, int, int, int,
            int );
   void  scrdo8  ( int, int, int, int, int, int, int, int, int, int,
            int );
   void  scrdo24 ( int, int, int, int, int, int, int, int, int, int,
            int );
#ifdef USE_SHM
   Bool  is_complete ( Display *, XEvent *, char * );
#endif
   int XAS_IOHandler ( Display *, XErrorEvent * );
   void  init ( void );
   void  InitXAS ( void );
   void  InitOldXAS ( void );
   void  SetupWindow ( int, char ** );
   void  user_options ( int, char **, char *, char * );
   int   imwrt ( void );
   int   oimwrt ( void );
   int   imrd ( short int * );
   int   oimrd ( short int * );
   void  resize_canvas ( int, int, int, int );
   void  resize_pressed ( void );
   int   windo_status ( void );
   int   Interogate ( short int * );
   int   InterogateO ( short int * );
   int   PSave ( void );
   int   PSaveO ( void );
   int   ClearChan ( void );
   int   FillChan ( void );
   int   VectChan ( void );
   int   FillChon ( void );
   int   VectChon ( void );
   int   CharChan ( void );
   int   CatInit ( short int );
   int   CatRead ( void );
   int   CatWrite ( void );
   int   CatOver ( void );
   int   CatFind ( void );
   int   ViewData ( void );
   int   zoom ( void );
   void  RecordCursor ( int, int );
   void  CheckKey ( KeySym, int );
   int   GetCursor ( void ) ;
   int   movecursor ( void );
   int   readbuttons ( void );
   int   cursor_button ( void );
   short int swapbytes ( short int );
   int   MakeLink ( void );
#if BSD
   int   ReadLink ( int, XASinput *, XASoutput * );
   int   WriteLink ( int, XASinput *, XASoutput * );
#endif
   void  zssslk ( int, int * ) ;
#if VMS
   void  XasAipsReadAST ( void );
   int   ReadLink ( void );
   void  XasAipsWriteAST ( void );
   int   WriteLink ( void );
#endif
   void  closedown ( void );
   void  printbufin ( void );
   void  printbufout ( void );
   int   cmap_wlot ( void );
   int   cmap_rlot ( void );
   int   cmap_wlut ( void );
   int   cmap_rlut ( void );
   int   cmap_wofm ( void );
   int   cmap_rofm ( void );
   int   cmap_wofmo ( void );
   int   cmap_rofmo ( void );
   int   cmap_change ( void );
   int   cmap_graph ( void );
   int   cmap_splot ( void );
   int   cmap_splat ( void );
   int   cmap_split ( void );
   int   cmap_wgrfx ( void );
   int   cmap_rgrfx ( void );
   void  crscol ( int *, int * );
   int   active ( int );

#else
   void  ProcessAipsRequest (/* void */);
   int   scrwrt  (/* int, int, int, int */);
   void  scrdo   (/* int, int, int, int, int */);
   void  scrdoit (/* int, int, int, int, int, int, int, int, int, int,
            int */);
   void  scrdo8  (/* int, int, int, int, int, int, int, int, int, int,
            int */);
   void  scrdo24 (/* int, int, int, int, int, int, int, int, int, int,
            int */);
#ifdef USE_SHM
   Bool  is_complete (/* Display *, XEvent *, char * */);
#endif
   int   XAS_IOHandler (/* Display *, XErrorEvent * */);
   void  init (/* void */);
   void  InitXAS (/* void */);
   void  InitOldXAS (/* void */);
   void  SetupWindow (/* int, char ** */);
   void  user_options (/* int, char **, char *, char * */);
   int   imwrt (/* void */);
   int   imrd (/* short int * */);
   int   oimwrt (/* void */);
   int   oimrd (/* short int * */);
   void  resize_canvas (/* int, int, int, int */);
   void  resize_pressed (/* void */);
   int   windo_status (/* void */);
   int   Interogate (/* short int * */);
   int   InterogateO (/* short int * */);
   int   PSave (/* void */);
   int   PSaveO (/* void */);
   int   ClearChan (/* void */);
   int   FillChan (/* void */);
   int   VectChan (/* void */);
   int   FillChon (/* void */);
   int   VectChon (/* void */);
   int   CharChan (/* void */);
   int   CatInit (/* short int */);
   int   CatRead (/* void */);
   int   CatWrite (/* void */);
   int   CatOver (/* void */);
   int   CatFind (/* void */);
   int   ViewData (/* void */);
   int   zoom (/* void */);
   void  RecordCursor (/* int, int */);
   void  CheckKey (/* KeySym, int */);
   int   GetCursor (/* void */) ;
   int   movecursor (/* void */);
   int   readbuttons (/* void */);
   int   cursor_button (/* void */);
   short int swapbytes (/* short int */);
   int   MakeLink (/* void */);
#if BSD
   int   ReadLink (/* int, XASinput *, XASoutput * */);
   int   WriteLink (/* int, XASinput *, XASoutput * */);
#endif
   void  zssslk (/* int, int * */) ;
#if VMS
   void  XasAipsReadAST (/* void */);
   int   ReadLink (/* void */);
   void  XasAipsWriteAST (/* void */);
   int   WriteLink (/* void */);
#endif
   void  closedown (/* void */);
   void  printbufin (/* void */);
   void  printbufout (/* void */);
   int   cmap_wlot (/* void */);
   int   cmap_rlot (/* void */);
   int   cmap_wlut (/* void */);
   int   cmap_rlut (/* void */);
   int   cmap_wofm (/* void */);
   int   cmap_rofm (/* void */);
   int   cmap_wofmo (/* void */);
   int   cmap_rofmo (/* void */);
   int   cmap_change (/* void */);
   int   cmap_graph (/* void */);
   int   cmap_splot (/* void */);
   int   cmap_splat (/* void */);
   int   cmap_split (/* void */);
   int   cmap_wgrfx (/* void */);
   int   cmap_rgrfx (/* void */);
   void  crscol (/* int *, int * */);
   int   active (/* int */);

#endif
