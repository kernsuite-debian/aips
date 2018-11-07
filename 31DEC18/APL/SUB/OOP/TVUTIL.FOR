C   TV-Device Class utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "TVDEVICE" utility module.
C# Ext-util TV-Util Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999-2000, 2002, 2005-2009, 2011-2012, 2014
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Public functions:
C
C   OTVPRM (name, ngray, ngraph, tvmax, tvwind, csize, ierr)
C      return TV device parameters
C
C   Public call back functions: all have call sequence (name, ierr)
C      [..] re names of object data used
C
C      OTVINI : initialize all functions of the TV device
C      OTVCLC : clear gray memories      [TVCHANS]
C      OTVCLG : clear graphics memories  [GRCHANS]
C      OTVON  : on gray memories         [TVCHANS]
C      OTVONG : on graphics memories     [GRCHANS]
C      OTVOFF : off gray memories        [TVCHANS]
C      OTVOFG : off graphics memories    [GRCHANS]
C
C      OTVFID : TVFIDDLE combination of operations
C      OTVPSU : TVPSEUDO coloring
C      OTVFLA : TVPHLAME coloring
C      OTVZOM : interactive zoom
C      OTVOFC : init color transfer function
C      OTVOFZ : init zoom
C      OTVTRA : interactive transfer functions of 1 or more channels
C               [TVCHANS]
C      OTVOFT : init transfer functions of 1 or more channels
C               [TVCHANS]
C
C      OTVWIN : set TV window into a displayed image  [TVOBJECT] and
C               save in it [TBLC], [TTRC]
C      OTVBOX : init/reset windows into displayed image [TVOBJECT],
C               [TVPARENT] get from and reset in [NBOXES], [WINDOW]
C      OTVUBX : init/reset windows into displayed image [TVOBJECT],
C               [TVPARENT] get from and reset in [UNBOXES], [UNWINDOW]
C
C      OTVLOD : load an image to the TV
C               [TVCHANS], [TVOBJECT], [TVPARENT] and in them [TBLC],
C               [TTRC], [TXINC], [TYINC], [TVCORNER], [PIXRANGE],
C               [FUNCTYPE], [DISK], [CNO]
C      OTVROM : load an image to the TV and roam
C               [TVCHANS], [TVOBJECT], [TVPARENT] and in them [TBLC],
C               [TTRC], [TXINC], [TYINC], [TVCORNER], [PIXRANGE],
C               [FUNCTYPE], [DISK], [CNO]
C      OTVRRM : re-roam an already loaded roamable image
C               [ROAMMODE] has info about previous load
C      OTVROF : capture a roamed image in non-roamed form
C      OTVWED : load a step wedge to TV [WWIDTH], [WSIDE], [WPIXR]
C      OTVLAB : label a TV image [LTYPE], [DOACROSS], [GRCHANS]
C      OTVWEL : label a TV wedge [GRCHANS]
C      OTVIER : erase an image
C      OTVWER : erase a wedge
C
C      OTVOGA : play with gamma correction [GRCHANS]
C      OTVOCO : modify OFM with contours [GRCHANS}, [OFMCONT]
C      OTVOWE : modify OFM with wedge contours [GRCHANS}, [OFMCONT]
C      OTVOAJ : modify OFM piecewise linear [GRCHANS]
C      OTVOTW : modify OFM by tweaking slope and intercept
C      OTVODI : list OFM files available in public area
C      OTVOPU : save OFM as text file [OFMFILE]
C      OTVOGE : get OFM from text file [OFMFILE]
C      OTVOZP : destroy OFM text file [OFMFILE]
C
C   Public call back functions with other call sequences
C      OTVSCR (name, grtoo, ierr)
C         interactive scroll in channels TVCHANS + graphics
C      OTVOFS (name, grtoo, ierr)
C         init scroll in channels TVCHANS in object NAME + graphics
C
C-----------------------------------------------------------------------
LOCAL INCLUDE 'TVUTIL.INC'
      INTEGER   STMAX
      PARAMETER (STMAX = 100)
C
      CHARACTER NAME*(*)
      INTEGER   IERR
C
      INTEGER   MSGSAV, TYPE, DIM(7), CHANS(16), I, DUMMY, WINLOD,
     *   SCRB(256), TVCATB(256), XWINLD, UWINLD, XLTYPE, NSTARS
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PAOOF.INC'
      COMMON /TVUTIL/ TVCATB, SCRB, MSGSAV, TYPE, DIM, CHANS, I, DUMMY,
     *   WINLOD, XWINLD, UWINLD, XLTYPE, NSTARS
      COMMON /TVUTIC/ CDUMMY
LOCAL END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   Public functions:
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE OTVPRM (NAME, NGRY, NGRPH, MAXX, TVWIND, CSIZE, IERR)
C-----------------------------------------------------------------------
C   Return the parameters of the current TV device object.  Must be open
C   Inputs:
C      NAME     C*?    Object name
C   Outputs:
C      NGRY     I      Number grey planes
C      NGRPH    I      Number graphics overlay planes
C      MAXX     I(2)   Absolute size of TV - x,y
C      TVWIND   I(4)   Current visible TV positions: blc x,y, trc x,y
C      CSIZE    I(2)   Character size in x,y
C      IERR     I      0 okay, else not open
C-----------------------------------------------------------------------
      INTEGER   NGRY, NGRPH, MAXX(2), TVWIND(4), CSIZE(2)
      INCLUDE 'TVUTIL.INC'
C
      INTEGER   TVSTAT(16), GRSTAT(8)
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       OPen?
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CALL TVDOKA (NAME, TVSTAT, GRSTAT, IERR)
      IF (IERR.EQ.0) CALL YWINDO ('READ', WINDTV, IERR)
      IF (IERR.EQ.0) THEN
         NGRY = NGRAY
         NGRPH = NGRAPH
         MAXX(1) = MAXXTV(1)
         MAXX(2) = MAXXTV(2)
         CSIZE(1) = CSIZTV(1)
         CSIZE(2) = CSIZTV(2)
         CALL COPY (4, WINDTV, TVWIND)
         END IF
C
 999  RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C   Public call-back functions
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE OTVINI (NAME, IERR)
C-----------------------------------------------------------------------
C   Initialize the TV
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CALL TVDOPR (NAME, 'INIT', 1, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVCLC (NAME, IERR)
C-----------------------------------------------------------------------
C   clear gray memories in channels TVCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         DIM(1) = 1
         CALL FILL (15, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      DO 10 I = 1,DIM(1)
         IF (CHANS(I).GT.0) THEN
            CALL TVDOPR (NAME, 'TVCL', CHANS(I), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 10      CONTINUE
      GO TO 999
C
 990  MSGTXT = 'OTVCLC: ERROR CLEARING TV GREY MEMORIES'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTVCLG (NAME, IERR)
C-----------------------------------------------------------------------
C   clear graphics memories in channels GRCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         DIM(1) = 1
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      DO 10 I = 1,DIM(1)
         IF (CHANS(I).GT.0) THEN
            CALL TVDOPR (NAME, 'GRCL', CHANS(I), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 10      CONTINUE
      GO TO 999
C
 990  MSGTXT = 'OTVCLG: ERROR CLEARING TV GRAPHICS MEMORIES'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTVON (NAME, IERR)
C-----------------------------------------------------------------------
C   turn on gray memories in channels TVCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         DIM(1) = 1
         CALL FILL (15, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      DO 10 I = 1,DIM(1)
         IF (CHANS(I).GT.0) THEN
            CALL TVDOPR (NAME, 'TVON', CHANS(I), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 10      CONTINUE
      GO TO 999
C
 990  MSGTXT = 'OTVON: ERROR TURNING ON TV GREY MEMORIES'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTVONG (NAME, IERR)
C-----------------------------------------------------------------------
C   turn on graphics memories in channels GRCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         DIM(1) = 1
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      DO 10 I = 1,DIM(1)
         IF (CHANS(I).GT.0) THEN
            CALL TVDOPR (NAME, 'GRON', CHANS(I), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 10      CONTINUE
      GO TO 999
C
 990  MSGTXT = 'OTVONG: ERROR TUNING ON GRAPHICS MEMORIES'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTVOFF (NAME, IERR)
C-----------------------------------------------------------------------
C   turn off gray memories in channels TVCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         DIM(1) = 1
         CALL FILL (15, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      DO 10 I = 1,DIM(1)
         IF (CHANS(I).GT.0) THEN
            CALL TVDOPR (NAME, 'TVOFF', CHANS(I), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 10      CONTINUE
      GO TO 999
C
 990  MSGTXT = 'OTVOFF: ERROR TURNING OFF TV GREY MEMORIES'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTVOFG (NAME, IERR)
C-----------------------------------------------------------------------
C   turn off graphics memories in channels GRCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         DIM(1) = 1
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      DO 10 I = 1,DIM(1)
         IF (CHANS(I).GT.0) THEN
            CALL TVDOPR (NAME, 'GROFF', CHANS(I), IERR)
            IF (IERR.NE.0) GO TO 990
            END IF
 10      CONTINUE
      GO TO 999
C
 990  MSGTXT = 'OTVOFG: ERROR TURNING OFF GRAPHICS MEMORIES'
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE OTVFID (NAME, IERR)
C-----------------------------------------------------------------------
C   TVFIDDLE
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         DIM(1) = 1
         CALL FILL (15, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      CALL TVDFUN (NAME, 'FIDDLE', CHANS, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVPSU (NAME, IERR)
C-----------------------------------------------------------------------
C   TVPSEUDO
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CALL TVDFUN (NAME, 'PSEUDO', 1, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVFLA (NAME, IERR)
C-----------------------------------------------------------------------
C   TVPHLAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CALL TVDFUN (NAME, 'FLAME', 1, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVZOM (NAME, IERR)
C-----------------------------------------------------------------------
C   TVZOOM
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CALL TVDFUN (NAME, 'ZOOM', 1, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOFC (NAME, IERR)
C-----------------------------------------------------------------------
C   Off color
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CALL TVDFUN (NAME, 'OFFCOLOR', 1, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOFZ (NAME, IERR)
C-----------------------------------------------------------------------
C   off zoom
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CALL TVDFUN (NAME, 'OFFZOOM', 1, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVTRA (NAME, IERR)
C-----------------------------------------------------------------------
C   TVTRANSF in channels TVCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         CALL FILL (15, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.EQ.0) CALL TVDFUN (NAME, 'TRANSFER', CHANS, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVSCR (NAME, GRTOO, IERR)
C-----------------------------------------------------------------------
C   interactive scroll in channels TVCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C      GRTOO    L     Include graphics channels?
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      LOGICAL   GRTOO
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         CALL FILL (15, 0, CHANS(2))
         IERR = 0
         END IF
      IF (GRTOO) CHANS(16) = 99
      IF (IERR.EQ.0) CALL TVDFUN (NAME, 'SCROLL', CHANS, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOFS (NAME, GRTOO, IERR)
C-----------------------------------------------------------------------
C   init scroll in channels TVCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C      GRTOO    L     Include graphics channels?
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      LOGICAL   GRTOO
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         CALL FILL (15, 0, CHANS(2))
         IERR = 0
         END IF
      IF (GRTOO) CHANS(16) = 99
      IF (IERR.EQ.0) CALL TVDFUN (NAME, 'OFFSCROLL', CHANS, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOFT (NAME, IERR)
C-----------------------------------------------------------------------
C   init transfer function in channels TVCHANS in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         CALL FILL (15, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.EQ.0) CALL TVDFUN (NAME, 'OFFTRAN', CHANS, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVWIN (NAME, IERR)
C-----------------------------------------------------------------------
C   Set a TV display window into a displayed image.
C   Parameters TBLC, TTRC will be be in the TVOBJECT object on
C   output.
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IM, IC, IBLC(7), ITRC(7)
      REAL      BLC(7), TRC(7)
      CHARACTER TVOBJ*32
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      CALL TVDGET (NAME, 'TVOBJECT', TYPE, DIM, DUMMY, TVOBJ, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       get window - try object
      IF (TVOBJ.EQ.' ') THEN
         IERR = 2
         MSGTXT = 'OTVWIN: A TVOBJECT NAME IS NEEDED'
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       okay do it
      IM = 3
      IC = 0
      CALL TVDWIN (NAME, IM, IC, BLC, TRC, IERR)
      IF (IERR.EQ.0) THEN
         DO 10 IC = 1,7
            IBLC(IC) = BLC(IC) + 0.5
            ITRC(IC) = TRC(IC) + 0.5
 10         CONTINUE
         DIM(1) = 7
         DIM(2) = 1
         CALL OPUT (TVOBJ, 'TBLC', OOAINT, DIM, IBLC, CDUMMY, IERR)
         IF (IERR.EQ.0) CALL OPUT (TVOBJ, 'TTRC', OOAINT, DIM, ITRC,
     *      CDUMMY, IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OTVBOX (NAME, IERR)
C-----------------------------------------------------------------------
C   Set windows into a displayed image.
C   Parameters NBOXES and WINDOW must be in the TVOBJECT or TVPARENT
C   object and, if the latter, a TVFIELD parameter is also needed.
C   Limit: MXNBOX boxes / field, MXNBOX * MAXFLD total boxes.
C   If NBOXES(TVFIELD) < 0 on input then return a value >= 0 and do the
C   delete boxes function.
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   WIN(2), WINW, IOBJ, NTYPE, NDIM(7), TVFLD, LIMIT,
     *   IROUND, NBOXES(MAXFLD), WTYPE, WDIM(7), IC, IM, MXF, MSL,
     *   CATSAV(256), TVSCRB(256)
      LONGINT   WINP, JP
      REAL      BLC(7,MXNBOX), TRC(7,MXNBOX)
      CHARACTER TVOBJ*32, TVPAR*32
      LOGICAL   UNIQUE, BXONLY
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IOBJ = 0
      MSGSAV = MSGSUP
      CALL TVDGET (NAME, 'TVOBJECT', TYPE, DIM, DUMMY, TVOBJ, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TVDGET (NAME, 'TVPARENT', TYPE, DIM, DUMMY, TVPAR, IERR)
      IF (IERR.NE.0) GO TO 999
      MSGSUP = 32000
      CALL OGET (NAME, 'BOXONLY', TYPE, DIM, BXONLY, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         BXONLY = .FALSE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       get window - try object
      IF (TVOBJ.NE.' ') THEN
         MSGSUP = 32000
         CALL OGET (TVOBJ, 'NBOXES', NTYPE, NDIM, NBOXES, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            MXF = MIN (MXNBOX, MXNBFL/NDIM(1))
            WINW = 4 * MXF * NDIM(1)
            WINW = (WINW - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', 'OTVBOX', WINW, WIN(1), WINP, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'OTVBOX: UNABLE TO GET DYNAMIC MEMORY'
               CALL MSGWRT (6)
               GO TO 999
               END IF
            CALL OGET (TVOBJ, 'WINDOW', WTYPE, WDIM, WIN(1+WINP),
     *         CDUMMY, IERR)
            IF (IERR.EQ.0) THEN
               IOBJ = 1
               MSGSUP = 32000
               CALL OGET (TVOBJ, 'BOXMSGL', NTYPE, NDIM, MSL, CDUMMY,
     *            IERR)
               MSGSUP = MSGSAV
               IF (IERR.EQ.1) MSL = 3
               IERR = 0
            ELSE
               CALL ZMEMRY ('FREE', 'OTVBOX', WINW, WIN(1), WINP, IERR)
               END IF
            TVFLD = 1
            END IF
         END IF
C                                       get window - try object
      IF ((TVPAR.NE.' ') .AND. (IOBJ.EQ.0)) THEN
         MSGSUP = 32000
         CALL OGET (TVPAR, 'TVFIELD', NTYPE, NDIM, TVFLD, CDUMMY, IERR)
         IF (IERR.NE.0) TVFLD = 1
         CALL OGET (TVPAR, 'NBOXES', NTYPE, NDIM, NBOXES, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            MXF = MIN (MXNBOX, MXNBFL/NDIM(1))
            WINW = 4 * MXF * NDIM(1)
            WINW = (WINW - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', 'OTVBOX', WINW, WIN(1), WINP, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'OTVBOX: UNABLE TO GET DYNAMIC MEMORY'
               CALL MSGWRT (6)
               GO TO 999
               END IF
            CALL OGET (TVPAR, 'WINDOW', WTYPE, WDIM, WIN(1+WINP),
     *         CDUMMY, IERR)
            IF (IERR.EQ.0) THEN
               IOBJ = 2
               MSGSUP = 32000
               CALL OGET (TVPAR, 'BOXMSGL', NTYPE, NDIM, MSL, CDUMMY,
     *            IERR)
               MSGSUP = MSGSAV
               IF (IERR.EQ.1) MSL = 3
               IERR = 0
            ELSE
               CALL ZMEMRY ('FREE', 'OTVBOX', WINW, WIN(1), WINP, IERR)
               END IF
            END IF
         END IF
      MSGSUP = MSGSAV
      IF (IOBJ.EQ.0) THEN
         MSGTXT = 'UNABLE TO GET WINDOW PARAMETERS FROM ' // TVOBJ
         CALL MSGWRT (6)
         MSGTXT = 'UNABLE TO GET WINDOW PARAMETERS FROM ' // TVPAR
         CALL MSGWRT (6)
         IERR = 2
         GO TO 999
         END IF
C                                       outer limit
      IF ((WINLOD.GT.0) .AND. (XWINLD.GT.0)) THEN
         CALL COPY (256, CATBLK, CATSAV)
         CALL TVFIND (NGRAY, 'MA', I, UNIQUE, CATBLK, TVSCRB, IERR)
         IF (IERR.EQ.0) THEN
            IM = 0
            IC = MAX (0, XWINLD)
            LIMIT = -1
C                                       make an ellipse
            IF (CATBLK(KINAX).NE.CATBLK(KINAX+1)) THEN
               BLC(1,1) = -(CATBLK(KINAX)/2 - 5)
               BLC(2,1) = -(CATBLK(KINAX+1)/2 - 5)
            ELSE
               BLC(1,1) = -1
               BLC(2,1) = CATBLK(KINAX)/2 - 5
               END IF
            TRC(1,1) = CATBLK(KINAX) / 2
            TRC(2,1) = CATBLK(KINAX+1) / 2 + 1
            I = 1
            MSGSUP = 32000
            CALL TVDBOX (NAME, IM, IC, LIMIT, I, BLC, TRC, IERR)
            MSGSUP = 0
            END IF
         CALL COPY (256, CATSAV, CATBLK)
         IERR = 0
         END IF
C                                       okay do it
      MXF = WDIM(1) / 4
      MXF = MAX (1, MIN (MXF, MAXFLD))
      LIMIT = MIN (MXNBOX, MXNBFL/MXF)
      DO 20 I = 1,LIMIT
         JP = ((I - 1) * MXF + TVFLD - 1) * 4 + WINP
         BLC(1,I) = WIN(1+JP)
         BLC(2,I) = WIN(2+JP)
         TRC(1,I) = WIN(3+JP)
         TRC(2,I) = WIN(4+JP)
 20      CONTINUE
      IM = MSL
      IC = MAX (0, WINLOD)
      IF ((WINLOD.GT.0) .OR. (BXONLY)) LIMIT = -LIMIT
      CALL TVDBOX (NAME, IM, IC, LIMIT, NBOXES(TVFLD), BLC, TRC, IERR)
      IF ((IERR.EQ.0) .AND. (LIMIT.GT.0)) THEN
         NBOXES(TVFLD) = MIN (NBOXES(TVFLD), MXNBOX)
         DO 30 I = 1,NBOXES(TVFLD)
            JP = ((I - 1) * MXF + TVFLD - 1) * 4 + WINP
            WIN(1+JP) = IROUND (BLC(1,I))
            WIN(2+JP) = IROUND (BLC(2,I))
            WIN(3+JP) = IROUND (TRC(1,I))
            WIN(4+JP) = IROUND (TRC(2,I))
 30         CONTINUE
         IF (IOBJ.EQ.1) THEN
            CALL OPUT (TVOBJ, 'NBOXES', NTYPE, NDIM, NBOXES, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (TVOBJ, 'WINDOW', WTYPE, WDIM, WIN(1+WINP),
     *         CDUMMY, IERR)
         ELSE
            CALL OPUT (TVPAR, 'NBOXES', NTYPE, NDIM, NBOXES, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (TVPAR, 'WINDOW', WTYPE, WDIM, WIN(1+WINP),
     *         CDUMMY, IERR)
            END IF
         END IF
      CALL ZMEMRY ('FREE', 'OTVBOX', WINW, WIN(1), WINP, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVUBX (NAME, IERR)
C-----------------------------------------------------------------------
C   Set windows into a displayed image.
C   Parameters UNBOXES and UNWINDOW must be in the TVOBJECT or TVPARENT
C   object and, if the latter, a TVFIELD parameter is also needed.
C   Limit: MXNBOX boxes / field, MXNBOX * MAXFLD total boxes.
C   If NBOXES(TVFIELD) < 0 on input then return a value >= 0 and do the
C   delete boxes function.
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   WIN(2), WINW, IOBJ, NTYPE, NDIM(7), TVFLD, LIMIT,
     *   IROUND, NBOXES(MAXFLD), WTYPE, WDIM(7), IC, IM, MXF, MSL,
     *   CATSAV(256), TVSCRB(256)
      LONGINT   WINP, JP
      REAL      BLC(7,MXNBOX), TRC(7,MXNBOX)
      CHARACTER TVOBJ*32, TVPAR*32
      LOGICAL   UNIQUE, BXONLY
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IOBJ = 0
      MSGSAV = MSGSUP
      CALL TVDGET (NAME, 'TVOBJECT', TYPE, DIM, DUMMY, TVOBJ, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TVDGET (NAME, 'TVPARENT', TYPE, DIM, DUMMY, TVPAR, IERR)
      IF (IERR.NE.0) GO TO 999
      MSGSUP = 32000
      CALL OGET (NAME, 'BOXONLY', TYPE, DIM, BXONLY, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         BXONLY = .FALSE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       get window - try object
      IF (TVOBJ.NE.' ') THEN
         MSGSUP = 32000
         CALL OGET (TVOBJ, 'UNBOXES', NTYPE, NDIM, NBOXES, CDUMMY, IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            MXF = MIN (MXNBOX, MXNBFL/NDIM(1))
            WINW = 4 * MXF * NDIM(1)
            WINW = (WINW - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', 'OTVUBX', WINW, WIN(1), WINP, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'OTVUBX: UNABLE TO GET DYNAMIC MEMORY'
               CALL MSGWRT (6)
               GO TO 999
               END IF
            CALL OGET (TVOBJ, 'UNWINDOW', WTYPE, WDIM, WIN(1+WINP),
     *         CDUMMY, IERR)
            IF (IERR.EQ.0) THEN
               IOBJ = 1
               MSGSUP = 32000
               CALL OGET (TVOBJ, 'BOXMSGL', NTYPE, NDIM, MSL, CDUMMY,
     *            IERR)
               MSGSUP = MSGSAV
               IF (IERR.EQ.1) MSL = 3
               IERR = 0
            ELSE
               CALL ZMEMRY ('FREE', 'OTVUBX', WINW, WIN(1), WINP, IERR)
               END IF
            TVFLD = 1
            END IF
         END IF
C                                       get window - try object
      IF ((TVPAR.NE.' ') .AND. (IOBJ.EQ.0)) THEN
         MSGSUP = 32000
         CALL OGET (TVPAR, 'TVFIELD', NTYPE, NDIM, TVFLD, CDUMMY, IERR)
         IF (IERR.NE.0) TVFLD = 1
         CALL OGET (TVPAR, 'UNBOXES', NTYPE, NDIM, NBOXES, CDUMMY,
     *      IERR)
         MSGSUP = MSGSAV
         IF (IERR.EQ.0) THEN
            MXF = MIN (MXNBOX, MXNBFL/NDIM(1))
            WINW = 4 * MXF * NDIM(1)
            WINW = (WINW - 1) / 1024 + 1
            CALL ZMEMRY ('GET ', 'OTVUBX', WINW, WIN(1), WINP, IERR)
            IF (IERR.NE.0) THEN
               MSGTXT = 'OTVUBX: UNABLE TO GET DYNAMIC MEMORY'
               CALL MSGWRT (6)
               GO TO 999
               END IF
            CALL OGET (TVPAR, 'UNWINDOW', WTYPE, WDIM, WIN(1+WINP),
     *         CDUMMY, IERR)
            IF (IERR.EQ.0) THEN
               IOBJ = 2
               MSGSUP = 32000
               CALL OGET (TVPAR, 'BOXMSGL', NTYPE, NDIM, MSL, CDUMMY,
     *            IERR)
               MSGSUP = MSGSAV
               IF (IERR.EQ.1) MSL = 3
               IERR = 0
            ELSE
               CALL ZMEMRY ('FREE', 'OTVUBX', WINW, WIN(1), WINP, IERR)
               END IF
            END IF
         END IF
      MSGSUP = MSGSAV
      IF (IOBJ.EQ.0) THEN
         MSGTXT = 'UNABLE TO GET WINDOW PARAMETERS FROM ' // TVOBJ
         CALL MSGWRT (6)
         MSGTXT = 'UNABLE TO GET WINDOW PARAMETERS FROM ' // TVPAR
         CALL MSGWRT (6)
         IERR = 2
         GO TO 999
         END IF
C                                       outer limit
      IF ((UWINLD.GT.0) .AND. (XWINLD.GT.0)) THEN
         CALL COPY (256, CATBLK, CATSAV)
         CALL TVFIND (NGRAY, 'MA', I, UNIQUE, CATBLK, TVSCRB, IERR)
         IF (IERR.EQ.0) THEN
            IM = 0
            IC = MAX (0, XWINLD)
            LIMIT = -1
C                                       make an ellipse
            IF (CATBLK(KINAX).NE.CATBLK(KINAX+1)) THEN
               BLC(1,1) = -(CATBLK(KINAX)/2 - 5)
               BLC(2,1) = -(CATBLK(KINAX+1)/2 - 5)
            ELSE
               BLC(1,1) = -1
               BLC(2,1) = CATBLK(KINAX)/2 - 5
               END IF
            TRC(1,1) = CATBLK(KINAX) / 2
            TRC(2,1) = CATBLK(KINAX+1) / 2 + 1
            I = 1
            MSGSUP = 32000
            CALL TVDBOX (NAME, IM, IC, LIMIT, I, BLC, TRC, IERR)
            MSGSUP = 0
            END IF
         CALL COPY (256, CATSAV, CATBLK)
         IERR = 0
         END IF
C                                       okay do it
      MXF = WDIM(1) / 4
      MXF = MAX (1, MIN (MXF, MAXFLD))
      LIMIT = MIN (MXNBOX, MXNBFL/MXF)
      DO 20 I = 1,LIMIT
         JP = ((I - 1) * MXF + TVFLD - 1) * 4 + WINP
         BLC(1,I) = WIN(1+JP)
         BLC(2,I) = WIN(2+JP)
         TRC(1,I) = WIN(3+JP)
         TRC(2,I) = WIN(4+JP)
 20      CONTINUE
      IM = MSL
      IC = MAX (0, UWINLD)
      IF ((UWINLD.GT.0) .OR. (BXONLY)) LIMIT = -LIMIT
      CALL TVDBOX (NAME, IM, IC, LIMIT, NBOXES(TVFLD), BLC, TRC, IERR)
      IF ((IERR.EQ.0) .AND. (LIMIT.GT.0)) THEN
         NBOXES(TVFLD) = MIN (NBOXES(TVFLD), MXNBOX)
         DO 30 I = 1,NBOXES(TVFLD)
            JP = ((I - 1) * MXF + TVFLD - 1) * 4 + WINP
            WIN(1+JP) = IROUND (BLC(1,I))
            WIN(2+JP) = IROUND (BLC(2,I))
            WIN(3+JP) = IROUND (TRC(1,I))
            WIN(4+JP) = IROUND (TRC(2,I))
 30         CONTINUE
         IF (IOBJ.EQ.1) THEN
            CALL OPUT (TVOBJ, 'UNBOXES', NTYPE, NDIM, NBOXES, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (TVOBJ, 'UNWINDOW', WTYPE, WDIM, WIN(1+WINP),
     *         CDUMMY, IERR)
         ELSE
            CALL OPUT (TVPAR, 'UNBOXES', NTYPE, NDIM, NBOXES, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
            CALL OPUT (TVPAR, 'UNWINDOW', WTYPE, WDIM, WIN(1+WINP),
     *         CDUMMY, IERR)
            END IF
         END IF
 100  CALL ZMEMRY ('FREE', 'OTVUBX', WINW, WIN(1), WINP, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVLOD (NAME, IERR)
C-----------------------------------------------------------------------
C   Displays an object in NAME's [TVCHANS](1).
C   Parameters sought in the TVOBJECT or TVPARENT include [TBLC],
C   [TTRC], [TXINC], [TYINC], [TVCORNER], [PIXRANGE], [FUNCTYPE] plus
C   [DISK] and [CNO] in TVOBJECT only
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C
      INTEGER   CNO, DISK, BLC(7), TRC(7), PINC(2), CHAN, TVCORN(2)
      REAL      PIXRNG(2)
      CHARACTER TVOBJ*32, TVPAR*32, FUNTYP*2
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      CALL TVDGET (NAME, 'TVOBJECT', TYPE, DIM, DUMMY, TVOBJ, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDGET (NAME, 'TVPARENT', TYPE, DIM, DUMMY, TVPAR, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDGET (NAME, 'WINLOAD', TYPE, DIM, WINLOD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (TVPAR, 'NSTARS', TYPE, DIM, NSTARS, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         NSTARS = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CALL OGET (TVPAR, 'LTYPE', TYPE, DIM, XLTYPE, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         XLTYPE = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CALL OGET (NAME, 'XWINLOAD', TYPE, DIM, XWINLD, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         XWINLD = 7
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (NAME, 'UWINLOAD', TYPE, DIM, UWINLD, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         UWINLD = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       get file parameters
      CALL FNAGET (TVOBJ, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL FNAGET (TVOBJ, 'CNO', TYPE, DIM, CNO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IERR = 2
      IF ((DISK.LE.0) .OR. (CNO.LE.0)) GO TO 999
C                                       TV channel
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CHAN = CHANS(1)
C                                       Windows
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TBLC', TYPE, DIM, BLC, CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR, 'TBLC',
     *   TYPE, DIM, BLC, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (7, 0, BLC)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TTRC', TYPE, DIM, TRC, CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR, 'TTRC',
     *   TYPE, DIM, TRC, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (7, 0, TRC)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       increments
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TXINC', TYPE, DIM, PINC(1), CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR, 'TXINC',
     *   TYPE, DIM, PINC(1), CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         PINC(1) = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TYINC', TYPE, DIM, PINC(2), CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR, 'TYINC',
     *   TYPE, DIM, PINC(2), CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         PINC(2) = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       display intensity range
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'PIXRANGE', TYPE, DIM, PIXRNG, CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR,
     *   'PIXRANGE', TYPE, DIM, PIXRNG, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         PIXRNG(1) = 0.0
         PIXRNG(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       forced corner
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TVCORNER', TYPE, DIM, TVCORN, CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR,
     *   'TVCORNER', TYPE, DIM, TVCORN, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         TVCORN(1) = 0
         TVCORN(2) = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       display intensity range
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'FUNCTYPE', TYPE, DIM, DUMMY, FUNTYP, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR,
     *   'FUNCTYPE', TYPE, DIM, DUMMY, FUNTYP, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         FUNTYP = ' '
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       So do it already
      CALL TVDLOD (NAME, DISK, CNO, CHAN, PIXRNG, BLC, TRC, PINC,
     *   FUNTYP, TVCORN, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       windows on top
      IF (WINLOD.GT.0) CALL OTVBOX (NAME, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (UWINLD.GT.0) CALL OTVUBX (NAME, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (MOD(ABS(XLTYPE),100).GT.2) CALL OTVLAB (NAME, IERR)
      IF (IERR.NE.0) GO TO 990
      IF (NSTARS.GT.0) CALL OTVSTR (NAME, IERR)
      IF (IERR.NE.0) GO TO 990
C
 990  WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      IF (IERR.NE.0) THEN
         MSGTXT = 'OTVLOD: FAILS TO LOAD IMAGE TO TV'
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
      END
      SUBROUTINE OTVROM (NAME, IERR)
C-----------------------------------------------------------------------
C   Displays an object in NAME's [TVCHANS](2-16).
C   Parameters sought in the TVOBJECT or TVPARENT include [TBLC],
C   [TTRC], [TXINC], [TYINC], [TVCORNER], [PIXRANGE], [FUNCTYPE] plus
C   [DISK] and [CNO] in TVOBJECT only
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C
      INTEGER   CNO, DISK, BLC(7), TRC(7), PINC(2), CHAN, TVCORN(2),
     *   ITVC(4), IWIN(4), TVC(4), INX, INY, IX, IY, INN, LBLC(7),
     *   LTRC(7), ICS(16), RTYPE
      REAL      PIXRNG(2), RBLC(7), RTRC(7)
      CHARACTER TVOBJ*32, TVPAR*32, FUNTYP*2
      INCLUDE 'INCS:PTVC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INTEGER   INBUF(TVMLUT)
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      CALL TVDGET (NAME, 'TVOBJECT', TYPE, DIM, DUMMY, TVOBJ, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDGET (NAME, 'TVPARENT', TYPE, DIM, DUMMY, TVPAR, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDGET (NAME, 'WINLOAD', TYPE, DIM, WINLOD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (TVPAR, 'NSTARS', TYPE, DIM, NSTARS, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         NSTARS = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CALL OGET (TVPAR, 'LTYPE', TYPE, DIM, XLTYPE, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         XLTYPE = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CALL TVDGET (NAME, 'XWINLOAD', TYPE, DIM, XWINLD, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         XWINLD = 7
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL TVDGET (NAME, 'UWINLOAD', TYPE, DIM, UWINLD, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         UWINLD = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       get file parameters
      CALL FNAGET (TVOBJ, 'DISK', TYPE, DIM, DISK, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL FNAGET (TVOBJ, 'CNO', TYPE, DIM, CNO, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      IERR = 2
      IF ((DISK.LE.0) .OR. (CNO.LE.0)) GO TO 999
C                                       TV channel
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         DO 10 I = 1,16
            CHANS(I) = I
 10         CONTINUE
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       Windows
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TBLC', TYPE, DIM, BLC, CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR, 'TBLC',
     *   TYPE, DIM, BLC, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (7, 0, BLC)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TTRC', TYPE, DIM, TRC, CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR, 'TTRC',
     *   TYPE, DIM, TRC, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CALL FILL (7, 0, TRC)
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       increments
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TXINC', TYPE, DIM, PINC(1), CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR, 'TXINC',
     *   TYPE, DIM, PINC(1), CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         PINC(1) = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'TYINC', TYPE, DIM, PINC(2), CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR, 'TYINC',
     *   TYPE, DIM, PINC(2), CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         PINC(2) = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       display intensity range
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'PIXRANGE', TYPE, DIM, PIXRNG, CDUMMY, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR,
     *   'PIXRANGE', TYPE, DIM, PIXRNG, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         PIXRNG(1) = 0.0
         PIXRNG(2) = 0.0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       display intensity range
      MSGSUP = 32000
      CALL OGET (TVOBJ, 'FUNCTYPE', TYPE, DIM, DUMMY, FUNTYP, IERR)
      IF ((IERR.EQ.1) .AND. (TVPAR.NE.' ')) CALL OGET (TVPAR,
     *   'FUNCTYPE', TYPE, DIM, DUMMY, FUNTYP, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         FUNTYP = ' '
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       find type, windows
      CHAN = CHANS(1)
      RTYPE = 0
C                                       use NGRAY-1 chans for zoom
      INN = NGRAY
      NGRAY = MAX (1, NGRAY-1)
      DO 15 I = 1,7
         RBLC(I) = BLC(I)
         RTRC(I) = TRC(I)
 15      CONTINUE
      CALL TVWIND (RTYPE, PINC, RBLC, RTRC, CHAN, ITVC, IWIN, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 20 I = 1,7
         BLC(I) = RBLC(I) + 0.1
         TRC(I) = RTRC(I) + 0.1
 20   CONTINUE
      NGRAY = INN
      CALL TVDOPR (NAME, 'HOLD', 0, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDFUN (NAME, 'OFFZ', 0, IERR)
      IF (IERR.NE.0) GO TO 990
      DO 30 I = 1,NGRAY
         CALL TVDOPR (NAME, 'TVOF', I, IERR)
         IF (IERR.NE.0) GO TO 990
 30      CONTINUE
      DO 35 I = 1,NGRAPH
         CALL TVDOPR (NAME, 'GROF', I, IERR)
         IF (IERR.NE.0) GO TO 990
 35      CONTINUE
      TVCORN(1) = 0
      TVCORN(2) = 0
C                                       So do it already: 1 chan only
      IF (RTYPE.EQ.0) THEN
         CHAN = CHANS(1)
         CALL TVDOPR (NAME, 'TVON', CHAN, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TVDOPR (NAME, 'TVCL', CHAN, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TVDLOD (NAME, DISK, CNO, CHAN, PIXRNG, BLC, TRC, PINC,
     *      FUNTYP, TVCORN, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       windows on top
         IF (WINLOD.GT.0) CALL OTVBOX (NAME, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (UWINLD.GT.0) CALL OTVUBX (NAME, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (MOD(ABS(XLTYPE),100).GT.2) CALL OTVLAB (NAME, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (NSTARS.GT.0) CALL OTVSTR (NAME, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       roam instead
      ELSE
         CALL COPY (7, BLC, LBLC)
         CALL COPY (7, TRC, LTRC)
         WINLOD = 0
         UWINLD = 0
         XWINLD = 0
         XLTYPE = 0
         NSTARS = 0
         INX = RTYPE / 100
         INY = MOD (RTYPE, 100)
         I = 1
C                                       LUTs
         CHAN = CHANS(1)
         CHAN = 2 ** (CHAN-1)
         IY = INY * INX
         IY = (2 ** IY) - 1
         CALL YLUT ('READ', CHAN, 4, .FALSE., INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YLUT ('WRIT', IY, 4, .FALSE., INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YLUT ('READ', CHAN, 2, .FALSE., INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YLUT ('WRIT', IY, 2, .FALSE., INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YLUT ('READ', CHAN, 1, .FALSE., INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL YLUT ('WRIT', IY, 1, .FALSE., INBUF, IERR)
         IF (IERR.NE.0) GO TO 990
C                                       do loads
         DO 90 IY = 1,INY
C                                       Y windows: top
            IF (IY.EQ.1) THEN
               TVC(4) = ITVC(4) - (INY-1) * MAXXTV(2)
               TVC(2) = 1
               LTRC(2) = IWIN(4)
            ELSE
               TVC(4) = MAXXTV(2)
               TVC(2) = 1
               IF (IY.EQ.INY) TVC(2) = ITVC(2)
               LTRC(2) = IWIN(4) - (ITVC(4) - (INY+1-IY)*MAXXTV(2))
     *            * PINC(2)
               END IF
            LBLC(2) = LTRC(2) - (TVC(4)-TVC(2)) * PINC(2)
            IF (LBLC(2).LT.IWIN(2)) THEN
               TVC(2) = TVC(2) + (IWIN(2) - LBLC(2)) / PINC(2)
               LBLC(2) = IWIN(2)
               END IF
C                                       x loop
            DO 80 IX = 1,INX
               I = I + 1
               CALL TVDOPR (NAME, 'TVON', CHANS(I), IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TVDOPR (NAME, 'TVCL', CHANS(I), IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TVDOPR (NAME, 'HOFF', CHANS(I), IERR)
               IF (IERR.NE.0) GO TO 990
C                                       x windows: right
               IF (IX.EQ.1) THEN
                  TVC(3) = ITVC(3) - (INX-1) * MAXXTV(1)
                  TVC(1) = 1
                  LTRC(1) = IWIN(3)
               ELSE
                  TVC(3) = MAXXTV(1)
                  TVC(1) = 1
                  IF (IX.EQ.INX) TVC(1) = ITVC(1)
                  LTRC(1) = IWIN(3) - (ITVC(3) - (INX+1-IX)*MAXXTV(1))
     *               * PINC(1)
                  END IF
               LBLC(1) = LTRC(1) - (TVC(3)-TVC(1)) * PINC(1)
               IF (LBLC(1).LT.IWIN(1)) THEN
                  TVC(1) = TVC(1) + (IWIN(1) - LBLC(1)) / PINC(1)
                  LBLC(1) = IWIN(1)
                  END IF
               TVCORN(1) = TVC(1)
               TVCORN(2) = TVC(2)
               CHAN = CHANS(I)
               CALL TVDLOD (NAME, DISK, CNO, CHAN, PIXRNG, LBLC, LTRC,
     *            PINC, FUNTYP, TVCORN, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TVDOPR (NAME, 'HOLD', 0, IERR)
               IF (IERR.NE.0) GO TO 990
               CALL TVDOPR (NAME, 'TVOF', CHAN, IERR)
               IF (IERR.NE.0) GO TO 990
 80            CONTINUE
 90         CONTINUE
C                                       now roam
         DIM(1) = 16
         CALL COPY (15, CHANS(2), ICS(2))
         ICS(1) = RTYPE
         CALL OPUT (NAME, 'ROAMMODE', OOAINT, DIM, ICS, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TVDROM (NAME, RTYPE, CHANS(2), IERR)
         END IF
C
 990  WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      IF (IERR.NE.0) THEN
         MSGTXT = 'OTVROM: FAILS TO LOAD IMAGE TO TV'
         CALL MSGWRT (7)
         END IF
      CALL TVDOPR (NAME, 'HOFF', 0, I)
C
 999  RETURN
      END
      SUBROUTINE OTVRRM (NAME, IERR)
C-----------------------------------------------------------------------
C   re-does already loaded roam mode
C   Parameters sought in NAME: [ROAMMODE] - type, up to 15 chans
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C
      INTEGER   ICS(16)
C-----------------------------------------------------------------------
      CALL OGET (NAME, 'ROAMMODE', TYPE, DIM, ICS, CDUMMY, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'UNABLE TO FIND PREVIOUS ROAM MODE AND CHANNELS'
      ELSE
         CALL TVDROM (NAME, ICS(1), ICS(2), IERR)
         IF (IERR.NE.0) MSGTXT = 'ERROR WHILE ROAMING'
         END IF
C
      IF (IERR.NE.0) CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE OTVROF (NAME, IERR)
C-----------------------------------------------------------------------
C   captures current roamed display in single channel [TVCHANS](1)
C   Parameters sought in NAME: [TVCHANS] - (1) to load
C   Parameters sought from TV include TXINC, TYINC, TRANSFUN, PIXRANGE
C   and windows
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C
      INTEGER   IXY(2), QUAD, QCHAN(4), QIMC(4,4), QTVC(4,4), QVOL(4),
     *   QCNO(4), NDIFF, J, ZAND, INC(2), IWIN(4), IX, IY, LBLC(7),
     *   LTRC(7), ITVC(4), ITEMP, II
      LOGICAL   ODD
      CHARACTER TVOBJ*32, TVPAR*32, FUNTYP*2
      REAL      PIXR(2)
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      CALL TVDGET (NAME, 'TVOBJECT', TYPE, DIM, DUMMY, TVOBJ, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDGET (NAME, 'TVPARENT', TYPE, DIM, DUMMY, TVPAR, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDGET (NAME, 'WINLOAD', TYPE, DIM, WINLOD, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL OGET (TVPAR, 'NSTARS', TYPE, DIM, NSTARS, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         NSTARS = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CALL OGET (TVPAR, 'LTYPE', TYPE, DIM, XLTYPE, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         XLTYPE = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      CALL TVDGET (NAME, 'XWINLOAD', TYPE, DIM, XWINLD, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         XWINLD = 7
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
      MSGSUP = 32000
      CALL TVDGET (NAME, 'UWINLOAD', TYPE, DIM, UWINLD, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         UWINLD = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       TV channel
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 990
C                                       analyze TV pix
      ODD = .FALSE.
      NDIFF = 0
      IXY(1) = MAXXTV(1) / 2
      IXY(2) = MAXXTV(2) / 2
      DO 25 I = 1,4
         QUAD = 5 - I
         DO 20 J = 1,NGRAY
            ITEMP = 2 ** (J-1)
            IF (ZAND(ITEMP,TVLIMG(QUAD)).NE.0) THEN
               QCHAN(QUAD) = J
               CALL TVDCAT (NAME, 'READ', J, IXY, CATBLK, IERR)
               IF (IERR.NE.0) THEN
                  MSGTXT = 'TVDCAT RETURNS ERROR'
                  GO TO 990
                  END IF
               CALL COPY (4, CATBLK(IIWIN), QIMC(1,QUAD))
               CALL COPY (4, CATBLK(IICOR), QTVC(1,QUAD))
               QVOL(QUAD) = CATBLK(IIVOL)
               QCNO(QUAD) = CATBLK(IICNO)
               IF (QVOL(QUAD).NE.QVOL(4)) ODD = .TRUE.
               IF (QCNO(QUAD).NE.QCNO(4)) ODD = .TRUE.
               DO 15 II = QUAD+1,4
                  IF (QCHAN(QUAD).NE.QCHAN(II)) NDIFF = NDIFF + 1
 15               CONTINUE
               GO TO 25
               END IF
 20         CONTINUE
 25      CONTINUE
C                                       not ROAM probably
      IF ((ODD) .OR. ((NDIFF.NE.6) .AND. (NDIFF.NE.4))) THEN
         MSGTXT = 'TV DOES NOT SEEM TO BE IN A ROAM MODE'
         IERR = 1
         GO TO 990
         END IF
C                                       figure out image
      INC(1) = 1
      INC(2) = 1
      IF (QTVC(3,3).NE.QTVC(1,3)) INC(1) = (QIMC(3,3)-QIMC(1,3)) /
     *   (QTVC(3,3)-QTVC(1,3))
      IF (QTVC(4,3).NE.QTVC(2,3)) INC(2) = (QIMC(4,3)-QIMC(2,3)) /
     *   (QTVC(4,3)-QTVC(2,3))
C                                       2x2
      IF (NDIFF.EQ.6) THEN
         IWIN(1) = QIMC(3,3) - (TVSPLT(1) - 2) * INC(1)
         IWIN(2) = QIMC(4,3) - TVSPLT(2) * INC(2)
         IWIN(3) = IWIN(1) + (MAXXTV(1) - 1) * INC(1)
         IWIN(4) = IWIN(2) + (MAXXTV(2) - 1) * INC(2)
C                                       1x2
      ELSE IF (QCHAN(1).EQ.QCHAN(2)) THEN
         IWIN(1) = QIMC(1,1)
         IWIN(3) = QIMC(3,1)
         IWIN(2) = QIMC(4,3) - TVSPLT(2) * INC(2)
         IWIN(4) = IWIN(2) + (MAXXTV(2) - 1) * INC(2)
C                                       2x1
      ELSE
         IWIN(2) = QIMC(2,1)
         IWIN(4) = QIMC(4,1)
         IWIN(1) = QIMC(3,3) - (TVSPLT(1) - 2) * INC(1)
         IWIN(3) = IWIN(1) + (MAXXTV(1) - 1) * INC(1)
         END IF
      IF (IWIN(1).LE.0) THEN
         IWIN(3) = MIN (IWIN(3)-IWIN(1)+1, CATBLK(KINAX))
         IWIN(1) = 1
         END IF
      IF (IWIN(3).GT.CATBLK(KINAX)) THEN
         IWIN(1) = MAX (1, IWIN(1) - IWIN(3) + CATBLK(KINAX))
         IWIN(3) = CATBLK(KINAX)
         END IF
      IF (IWIN(2).LE.0) THEN
         IWIN(4) = MIN (IWIN(4)-IWIN(2)+1, CATBLK(KINAX+1))
         IWIN(2) = 1
         END IF
      IF (IWIN(4).GT.CATBLK(KINAX+1)) THEN
         IWIN(2) = MAX (1, IWIN(2) - IWIN(4) + CATBLK(KINAX+1))
         IWIN(4) = CATBLK(KINAX+1)
         END IF
      IX = (IWIN(3) - IWIN(1)) / INC(1) + 1
      IY = (IWIN(4) - IWIN(2)) / INC(2) + 1
      I = (MAXXTV(1) - IX) / 2
      I = MAX (0, I)
      ITVC(3) = MAXXTV(1) - I
      ITVC(1) = ITVC(3) - IX + 1
      I = (MAXXTV(2) - IY) / 2
      I = MAX (0, I)
      ITVC(4) = MAXXTV(2) - I
      ITVC(2) = ITVC(4) - IY + 1
      CALL TVDOPR (NAME, 'HOLD', 0, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL TVDFUN (NAME, 'OFFS', CHANS, IERR)
      IF (IERR.NE.0) GO TO 980
      DO 50 J = 1,NGRAY
         IF (J.NE.CHANS(1)) THEN
            CALL TVDOPR (NAME, 'TVOF', J, IERR)
         ELSE
            CALL TVDOPR (NAME, 'TVON', J, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDOPR (NAME, 'TVCL', J, IERR)
            END IF
         IF (IERR.NE.0) GO TO 980
 50      CONTINUE
      CALL COPY (5, CATBLK(IIDEP), LBLC(3))
      CALL COPY (5, CATBLK(IIDEP), LTRC(3))
      LBLC(1) = IWIN(1)
      LBLC(2) = IWIN(2)
      LTRC(1) = IWIN(3)
      LTRC(2) = IWIN(4)
      CALL H2CHR (2, 1, CATH(IITRA), FUNTYP)
      PIXR(1) = CATR(IRRAN)
      PIXR(2) = CATR(IRRAN+1)
      CALL TVDLOD (NAME, QVOL(1), QCNO(1), CHANS(1), PIXR, LBLC, LTRC,
     *   INC, FUNTYP, ITVC, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'TVDLOD RETURNS ERROR'
C                                       windows on top
      ELSE
         IF (WINLOD.GT.0) CALL OTVBOX (NAME, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (UWINLD.GT.0) CALL OTVUBX (NAME, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (MOD(ABS(XLTYPE),100).GT.2) CALL OTVLAB (NAME, IERR)
         IF (IERR.NE.0) GO TO 990
         IF (NSTARS.GT.0) CALL OTVSTR (NAME, IERR)
         IF (IERR.NE.0) GO TO 990
         END IF
      GO TO 990
C
 980  MSGTXT = 'TV FUNCTION ERROR REPORTED'
C
 990  IF (IERR.NE.0) CALL MSGWRT (8)
      CALL TVDOPR (NAME, 'HOFF', 0, I)
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
C
 999  RETURN
      END
      SUBROUTINE OTVBLK (NAME, BTYPE, IERR)
C-----------------------------------------------------------------------
C   TVBLINK or TVMBLINK in channels TVCHANS(1 and 2) in object NAME
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C      BTYPE    I     type 1 - TVBLINK, 2 manual blink
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INTEGER   BTYPE
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'TVCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 1
         CHANS(2) = 2
         CALL FILL (14, 0, CHANS(3))
         IERR = 0
         END IF
      IF (IERR.EQ.0) CALL TVDBLK (NAME, BTYPE, CHANS, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVALU (NAME, IERR)
C-----------------------------------------------------------------------
C   CURVALU using graphics channel GRCHANS(1)
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 2
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.EQ.0) CALL TVDVAL (NAME, CHANS, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVWED (NAME, IERR)
C-----------------------------------------------------------------------
C   load a step wedge to TV [WWIDTH], [WSIDE], [WPIXR]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   WIDTH, LENGTH, BLC(2), WSIDE, CHAN, IBUT, IX0, IY0, IPL,
     *   ITYPE, IROUND
      REAL      RPOS(2)
      LOGICAL   WPIXR, UNIQUE, ASKPOS
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
C                                       width
      MSGSUP = 32000
      CALL TVDGET (NAME, 'WWIDTH', TYPE, DIM, WIDTH, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         WIDTH = 16
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      ASKPOS = WIDTH.LT.0
      WIDTH = ABS (WIDTH)
      IF (WIDTH.LE.5) WIDTH = 16
C                                       side: LBRT = 0,1,2,3
      MSGSUP = 32000
      CALL TVDGET (NAME, 'WSIDE', TYPE, DIM, WSIDE, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         WSIDE = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      WSIDE = MOD (WSIDE+400, 4)
C                                       clipped as TV or full image
      MSGSUP = 32000
      CALL TVDGET (NAME, 'WPIXR', TYPE, DIM, WPIXR, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         WPIXR = .TRUE.
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       find image
      CALL TVFIND (NGRAY, 'MA', CHAN, UNIQUE, TVCATB, SCRB, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       ask for cursor position
      IF (ASKPOS) THEN
         IF ((WSIDE.EQ.1) .OR. (WSIDE.EQ.3)) THEN
            MSGTXT = 'Please point out approx position for bottom edge'
     *         // ' of wedge'
         ELSE
            MSGTXT = 'Please point out approx position for left edge'
     *         // ' of wedge'
            END IF
         CALL MSGWRT (1)
         CALL TVWHER (IPL, RPOS, IBUT, IERR)
         IF (IERR.NE.0) GO TO 999
         IF ((WSIDE.EQ.1) .OR. (WSIDE.EQ.3)) THEN
            IY0 = IROUND (RPOS(2))
            IF ((IY0.LT.TVCATB(IICOR+1)) .AND. (IY0.GE.TVCATB(IICOR+1)
     *         -WIDTH-2)) IY0 = MAX (1, TVCATB(IICOR+1)-WIDTH)
            IF ((IY0.GT.TVCATB(IICOR+3)) .AND. (IY0.LT.TVCATB(IICOR+3)
     *         +WIDTH)) IY0 = MIN (TVCATB(IICOR+3)+1, MAXXTV(2)+1-WIDTH)
            IX0 = TVCATB(IICOR)
            LENGTH = TVCATB(IICOR+2) - TVCATB(IICOR) + 1
            IF (IY0.LE.TVCATB(IICOR+1)) WSIDE = 1
            IF (IY0.GE.TVCATB(IICOR+3)) WSIDE = 3
         ELSE
            IX0 = IROUND (RPOS(1))
            IF ((IX0.LT.TVCATB(IICOR)) .AND. (IX0.GE.TVCATB(IICOR)
     *         -WIDTH-2)) IX0 = MAX (1, TVCATB(IICOR)-WIDTH)
            IF ((IX0.GT.TVCATB(IICOR+2)) .AND. (IX0.LT.TVCATB(IICOR+2)
     *         +WIDTH)) IX0 = MIN (TVCATB(IICOR+2)+1, MAXXTV(1)+1-WIDTH)
            IY0 = TVCATB(IICOR+1)
            LENGTH = TVCATB(IICOR+3) - TVCATB(IICOR+1) + 1
            IF (IX0.LE.TVCATB(IICOR)) WSIDE = 0
            IF (IX0.GE.TVCATB(IICOR+2)) WSIDE = 2
            END IF
C                                       Set position
      ELSE
         IF (WSIDE.EQ.3) THEN
            IY0 = MIN (TVCATB(IICOR+3)+1+2*CSIZTV(2), MAXXTV(2)+1-WIDTH)
            IX0 = TVCATB(IICOR)
            LENGTH = TVCATB(IICOR+2) - TVCATB(IICOR) + 1
         ELSE IF (WSIDE.EQ.2) THEN
            IX0 = MIN (TVCATB(IICOR+2)+1, MAXXTV(1)+1-WIDTH)
            IY0 = TVCATB(IICOR+1)
            LENGTH = TVCATB(IICOR+3) - TVCATB(IICOR+1) + 1
         ELSE IF (WSIDE.EQ.1) THEN
            IY0 = 0
            IF (UNIQUE) IY0 = TVCATB(IICOR+1) - 6.833*CSIZTV(2) - 4.5 -
     *         WIDTH
            IF (IY0.LT.3*CSIZTV(2)) IY0 = TVCATB(IICOR+1) - WIDTH
            IY0 = MAX (1, IY0)
            IX0 = TVCATB(IICOR)
            LENGTH = TVCATB(IICOR+2) - TVCATB(IICOR) + 1
         ELSE
            IX0 = 0
            IF (UNIQUE) IX0 = TVCATB(IICOR) - 8.333*CSIZTV(1) - 4.5 -
     *         WIDTH
            IF (IX0.LT.3*CSIZTV(1)) IX0 = TVCATB(IICOR) - WIDTH
            IX0 = MAX (1, IX0)
            IY0 = TVCATB(IICOR+1)
            LENGTH = TVCATB(IICOR+3) - TVCATB(IICOR+1) + 1
            END IF
         END IF
      ITYPE = 10 + WSIDE
      IF (WPIXR) ITYPE = ITYPE + 10
C                                       do it
      BLC(1) = IX0
      BLC(2) = IY0
      CALL TVDWED (NAME, ITYPE, CHAN, BLC, LENGTH, WIDTH, TVCATB, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVLAB (NAME, IERR)
C-----------------------------------------------------------------------
C   label a TV image [LTYPE], [DOACROSS], [GRCHANS]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   ILTYPE, JLTYPE
      LOGICAL   DOACRO
      CHARACTER TVPAR*32
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      CALL TVDGET (NAME, 'TVPARENT', TYPE, DIM, DUMMY, TVPAR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       graphics channel
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 2
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       label type
      MSGSUP = 32000
      CALL TVDGET (NAME, 'LTYPE', TYPE, DIM, XLTYPE, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         CALL OGET (TVPAR, 'LTYPE', TYPE, DIM, XLTYPE, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            XLTYPE = 3
            IERR = 0
            END IF
         END IF
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 999
      JLTYPE = ABS(XLTYPE) / 100
      ILTYPE = MOD (ABS (XLTYPE), 100)
      IF ((ILTYPE.LE.0) .OR. (ILTYPE.GT.10)) THEN
         ILTYPE = 3
         IF (XLTYPE.LT.0) THEN
            XLTYPE = -ILTYPE - 100*JLTYPE
         ELSE
            XLTYPE = ILTYPE + 100*JLTYPE
            END IF
         END IF
C                                       tick all way across
      MSGSUP = 32000
      CALL TVDGET (NAME, 'DOACROSS', TYPE, DIM, DOACRO, CDUMMY, IERR)
      IF (IERR.EQ.1) THEN
         CALL OGET (TVPAR, 'DOACROSS', TYPE, DIM, DOACRO, CDUMMY, IERR)
         IF (IERR.EQ.1) THEN
            IERR = 0
            DOACRO = .FALSE.
            END IF
         END IF
      MSGSUP = MSGSAV
      IF (IERR.NE.0) GO TO 999
C                                       do it
      CALL TVDLAB (NAME, 'MA', CHANS, XLTYPE, DOACRO, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVSTR (NAME, IERR)
C-----------------------------------------------------------------------
C   put stars on image [LTYPE], [DOACROSS], [GRCHANS] uses GRCHANS(2)
C   not 1
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      DOUBLE PRECISION STPOS(2,STMAX)
      REAL      STPARM(4,STMAX)
      CHARACTER TVPAR*32
C-----------------------------------------------------------------------
      MSGSAV = MSGSUP
      CALL TVDGET (NAME, 'TVPARENT', TYPE, DIM, DUMMY, TVPAR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       graphics channel
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(2) = 3
         CALL FILL (6, 0, CHANS(3))
         CHANS(1) = 0
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       label type
      MSGSUP = 32000
      CALL OGET (TVPAR, 'NSTARS', TYPE, DIM, NSTARS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         NSTARS = 0
         IERR = 0
         END IF
      IF ((NSTARS.LE.0) .OR. (IERR.NE.0)) GO TO 999
      NSTARS = MIN (NSTARS, STMAX)
C                                       get stars data
      CALL OGET (TVPAR, 'STARPOSN', TYPE, DIM, STPOS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET (TVPAR, 'STARPARM', TYPE, DIM, STPARM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       do it
      CALL TVDSTR (NAME, 'MA', CHANS(2), NSTARS, STPOS, STPARM, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVWEL (NAME, IERR)
C-----------------------------------------------------------------------
C   label a TV wedge [GRCHANS]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   LTYPE
      LOGICAL   DOACRO
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 2
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       do it
      LTYPE = 107
      DOACRO = .FALSE.
      CALL TVDLAB (NAME, 'WE', CHANS, LTYPE, DOACRO, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOGA (NAME, IERR)
C-----------------------------------------------------------------------
C   play with gamma correction [GRCHANS]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 2
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      NCONT = 1
      OFMFIL = ' '
      CALL TVDOFM (NAME, 'GAMM', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOCO (NAME, IERR)
C-----------------------------------------------------------------------
C   modify OFM with contours [GRCHANS}, [OFMCONT]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
C                                       graphics channel
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 2
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       number of contours
      MSGSUP = 32000
      CALL TVDGET (NAME, 'OFMCONT', TYPE, DIM, NCONT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         NCONT = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      OFMFIL = ' '
      CALL TVDOFM (NAME, 'CONT', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOWE (NAME, IERR)
C-----------------------------------------------------------------------
C   modify OFM with wedge contours [GRCHANS}, [OFMCONT]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
C                                       graphics channel
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 2
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
C                                       number of contours
      MSGSUP = 32000
      CALL TVDGET (NAME, 'OFMCONT', TYPE, DIM, NCONT, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         NCONT = 1
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      OFMFIL = ' '
      CALL TVDOFM (NAME, 'WEDG', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOAJ (NAME, IERR)
C-----------------------------------------------------------------------
C   modify OFM piecewise linear [GRCHANS]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'GRCHANS', TYPE, DIM, CHANS, CDUMMY, IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.1) THEN
         CHANS(1) = 2
         CALL FILL (7, 0, CHANS(2))
         IERR = 0
         END IF
      IF (IERR.NE.0) GO TO 999
      NCONT = 1
      OFMFIL = ' '
      CALL TVDOFM (NAME, 'ADJU', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOTW (NAME, IERR)
C-----------------------------------------------------------------------
C   modify OFM by tweaking slope and intercept
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CHANS(1) = 2
      NCONT = 1
      OFMFIL = ' '
      CALL TVDOFM (NAME, 'TWEK', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVODI (NAME, IERR)
C-----------------------------------------------------------------------
C   list OFM files available in public area
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      CHANS(1) = 2
      NCONT = 1
      OFMFIL = ' '
      CALL TVDOFM (NAME, 'DIR ', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOPU (NAME, IERR)
C-----------------------------------------------------------------------
C   save OFM as text file [OFMFILE]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'OFMFILE', TYPE, DIM, DUMMY, OFMFIL, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.1) .OR. (OFMFIL.EQ.' ')) THEN
         MSGTXT = 'OTVOPU: OFMFILE MUST BE SET'
         CALL MSGWRT (7)
         IERR = 1
         END IF
      IF (IERR.NE.0) GO TO 999
      CHANS(1) = 2
      NCONT = 1
      CALL TVDOFM (NAME, 'SAVE', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOGE (NAME, IERR)
C-----------------------------------------------------------------------
C   Get OFM from text file [OFMFILE]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'OFMFILE', TYPE, DIM, DUMMY, OFMFIL, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.1) .OR. (OFMFIL.EQ.' ')) THEN
         MSGTXT = 'OTVOGE: OFMFILE MUST BE SET'
         CALL MSGWRT (7)
         IERR = 1
         END IF
      IF (IERR.NE.0) GO TO 999
      CHANS(1) = 2
      NCONT = 1
      CALL TVDOFM (NAME, 'GET ', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVOZP (NAME, IERR)
C-----------------------------------------------------------------------
C   destroy OFM text file [OFMFILE]
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   NCONT
      CHARACTER OFMFIL*48
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL TVDGET (NAME, 'OFMFILE', TYPE, DIM, DUMMY, OFMFIL, IERR)
      MSGSUP = MSGSAV
      IF ((IERR.EQ.1) .OR. (OFMFIL.EQ.' ')) THEN
         MSGTXT = 'OTVOZP: OFMFILE MUST BE SET'
         CALL MSGWRT (7)
         IERR = 1
         END IF
      IF (IERR.NE.0) GO TO 999
      CHANS(1) = 2
      NCONT = 1
      CALL TVDOFM (NAME, 'ZAP ', CHANS, NCONT, OFMFIL, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVIER (NAME, IERR)
C-----------------------------------------------------------------------
C   erase an image on the TV
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   WIDTH, LENGTH, CHAN, BLC(2)
      LOGICAL   UNIQUE
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
C                                       find image
      CALL TVFIND (NGRAY, 'MA', CHAN, UNIQUE, TVCATB, SCRB, IERR)
      IF (IERR.NE.0) GO TO 999
C
      BLC(1) = TVCATB(IICOR)
      BLC(2) = TVCATB(IICOR+1)
      LENGTH = TVCATB(IICOR+2) - BLC(1) + 1
      WIDTH = TVCATB(IICOR+3) - BLC(2) + 1
      CALL TVDWED (NAME, 1, CHAN, BLC, LENGTH, WIDTH, TVCATB, IERR)
C
 999  RETURN
      END
      SUBROUTINE OTVWER (NAME, IERR)
C-----------------------------------------------------------------------
C   erase a wedge on the TV
C   Inputs:
C      NAME     C*?   TVDEVICE object name - must be open
C   Output
C      IERR     I     Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'TVUTIL.INC'
      INTEGER   WIDTH, LENGTH, CHAN, BLC(2)
      LOGICAL   UNIQUE
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      WINLOD = 0
      UWINLD = 0
      XWINLD = 0
      XLTYPE = 0
      NSTARS = 0
C                                       find image
      CALL TVFIND (NGRAY, 'WE', CHAN, UNIQUE, TVCATB, SCRB, IERR)
      IF (IERR.NE.0) GO TO 999
C
      BLC(1) = TVCATB(IICOR)
      BLC(2) = TVCATB(IICOR+1)
      LENGTH = TVCATB(IICOR+2) - BLC(1) + 1
      WIDTH = TVCATB(IICOR+3) - BLC(2) + 1
      CALL TVDWED (NAME, 1, CHAN, BLC, LENGTH, WIDTH, TVCATB, IERR)
C
 999  RETURN
      END
