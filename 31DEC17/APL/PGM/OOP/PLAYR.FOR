LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER   NPARMS
      PARAMETER (NPARMS=25)
      INTEGER   AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
C                     1         2          3        4         5
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'IN2NAME',
C           6           7         8          9       10      11
     *   'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'TBLC', 'TTRC', 'TXINC',
C           12       13        14         15        16          17
     *   'TYINC', 'TVCHAN', 'TV2CHAN', 'TVCORN', 'PIXRANGE', 'PIXRANG2',
C           18          19          20        21
     *   'FUNCTYPE', 'FUNCTYP2', 'DETIME', 'LTYPE',
C           22          23         24         25
     *   'DOACROSS', 'WEDWIDTH', 'DOWEDGE', 'OFMFILE'/
C                     1       2      3       4       5       6
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOACAR, OOACAR,
C           7       8      9       10      11      12      13
     *   OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT, OOAINT,
C          14       15     16     17      18      19      20
     *   OOAINT, OOAINT,  OOARE, OOARE, OOACAR, OOACAR, OOAINT,
C          21      22      23      24      25
     *   OOAINT, OOALOG, OOAINT, OOAINT, OOACAR/
C                   1    2    3    4    5    6    7     8    9
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 12,1, 6,1, 1,1, 1,1, 7,1,
C         10   11   12   13   14   15   16   17   18   19   20
     *   7,1, 1,1, 1,1, 1,1, 1,1, 2,1, 2,1, 2,1, 2,1, 2,1, 1,1,
C         21   22   23   24    25
     *   1,1, 1,1, 1,1, 1,1, 48,1/
LOCAL END
      PROGRAM PLAYR
C-----------------------------------------------------------------------
C! Pastes a selected subimage of one image into another.
C# Map-util Utility Object-Oriented OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999-2000, 2008-2009, 2013-2014
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
C   Pastes a selected subimage of one image into another.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, IMAGE(2)*32
      INTEGER  IRET, BUFF1(256)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'PLAYR'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL PLAYIN (PRGM, IMAGE, IRET)
C                                       Paste
      IF (IRET.EQ.0) CALL PLAYIT (IMAGE, IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE PLAYIN (PRGN, IMAGE, IRET)
C-----------------------------------------------------------------------
C   PLAYIN gets input parameters for PLAYR and creates the relevant
C   objects.
C   Inputs:
C      PRGN    C*6        Program name
C   Output:
C      IMAGE   C(2)*32    Names of input images
C      IRET    I          Error code: 0 => ok
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6, IMAGE(2)*(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DFIL.INC'
      INTEGER   NKEY
C                                       NKEY no. adverbs to copy to
C                                       image
      PARAMETER (NKEY=11)
      CHARACTER INK1(NKEY)*8, OUTK(NKEY)*32, INK2(NKEY)*8
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs to copy to 2nd image
C                                       object
C                    1          1           2         4        5
      DATA INK2 /'IN2NAME', 'IN2CLASS', 'IN2SEQ', 'IN2DISK', 'TBLC',
C          6       7        8       9           10         11
     *   'TTRC', 'TXINC', 'TYINC', 'PIXRANG2', 'TVCORN', 'FUNCTYP2'/
C                                       Rename
C                   1       2        3        4       5
      DATA OUTK /'NAME', 'CLASS', 'IMSEQ', 'DISK', 'TBLC',
C          6       7         8        9           10        11
     *   'TTRC', 'TXINC', 'TYINC', 'PIXRANGE', 'TVCORNER', 'FUNCTYPE'/
C                                       Adverbs for ist image object
C                    1         2         3         4        5
      DATA INK1 /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'TBLC',
C          6       7        8       9           10         11
     *   'TTRC', 'TXINC', 'TYINC', 'PIXRANGE', 'TVCORN', 'FUNCTYPE'/
C-----------------------------------------------------------------------
C                                       Startup
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
      RQUICK = .FALSE.
C                                       Create input object 1
      IMAGE(1) = 'First image to play'
      CALL CREATE (IMAGE(1), 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY, INK1, OUTK, IMAGE(1), IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create input object 2
      IMAGE(2) = 'Second image to play'
      CALL CREATE (IMAGE(2), 'IMAGE', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
      CALL IN2OBJ ('Input', NKEY, INK2, OUTK, IMAGE(2), IRET)
      IF (IRET.NE.0) GO TO 999
C
 999  RETURN
      END
      SUBROUTINE PLAYIT (IMAGE, IERR)
C-----------------------------------------------------------------------
C   Play with images
C   Inputs:
C      IMAGE   C(2)*?  Name of Images to play
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE(2)*(*)
      INTEGER   IERR
C
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   TVCS(16,3), TYPE, DIM(7), DUMMY, GRCS(8), NCOL, I,
     *   NROWS(5), IMA, WIN(4,MXNBOX), NBOX, TIMLIM, TVBUTT, CHS, JERR,
     *   TBLC(7), TTRC(7), NCONTR, TTY(2), LTYPE, WWIDTH, ITEMP, NGRY,
     *   TVMAX(2), TVWND(4), CSIZ(2), NGRPH, NTITLE, TOPSEP, SIDSEP
      CHARACTER STATUS*4, CDUMMY*1, TVNAME*32, CHOICS(41)*16, ISHELP*6,
     *   OFMFIL*48, MSGBUF*72, TITLE*8
      LOGICAL   DOACRO, LTEMP, LEAVE(41)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA TOPSEP, SIDSEP /3, 10/
      DATA CHOICS /'TVFIDDLE', 'TVPSEUDO', 'TVFLAME', 'TVZOOM',
     *   'TVTRAN', 'OFFCOLOR', 'OFFTRAN', 'OFFZOOM', 'CURVALUE',
     *   'GAMMASET', 'OFMCONTOUR', 'OFMWEDGE', 'OFMADJUST', 'OFM TWEAK',
     *   'OFM DIR', 'OFM GET', 'OFM PUT', 'OFM ZAP', 'SET OFM NAME',
     *   'SET # CONT', 'WEDGE WIDTH',
     *   'EXIT', 'SET WINDOW', 'RESET WINDOW', 'TVBOX', 'REBOX',
     *   'TV WEDGE', 'IMAGE WEDGE', 'TV VERT WEDGE', 'IM VERT WEDGE',
     *   'ERASE WEDGE', 'LABEL IMAGE', 'LABEL WEDGE', 'SHOW LABEL',
     *   'HIDE LABEL', 'CLEAR LABEL', 'SELECT IMAGE 1','SELECT IMAGE 2',
     *   'TVTRAN BOTH', 'BLINK', 'MANUAL BLINK'/
      DATA LEAVE /5*.FALSE., 3*.TRUE., 6*.FALSE., 7*.TRUE., 2*.FALSE.,
     *   .TRUE., 9*.FALSE., 5*.TRUE., 3*.FALSE./
C-----------------------------------------------------------------------
      TTY(1) = 5
      TTY(2) = 0
      LOCNUM = 1
C                                       Get TV control
      CALL FILL (48, 0, TVCS)
      CALL OGET ('Input', 'DETIME', TYPE, DIM, TIMLIM, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'TVCHAN', TYPE, DIM, TVCS(1,1), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'TV2CHAN', TYPE, DIM, TVCS(1,2), CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'DOWEDGE', TYPE, DIM, NCONTR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'OFMFILE', TYPE, DIM, DUMMY, OFMFIL, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'WEDWIDTH', TYPE, DIM, WWIDTH, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'DOACROSS', TYPE, DIM, DOACRO, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL OGET ('Input', 'LTYPE', TYPE, DIM, LTYPE, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      IF (NCONTR.LT.1) NCONTR = 8
      IF (WWIDTH.EQ.0) WWIDTH = -16
      IF (TVCS(1,1).LE.0) TVCS(1,1) = 1
      IF (TVCS(1,2).EQ.TVCS(1,1)) TVCS(1,2) = TVCS(1,1)+1
      TVCS(1,3) = 1
      TVCS(2,3) = 2
      TVCS(3,3) = 3
      TVCS(4,3) = 4
      CALL FILL (7, 0, TBLC)
      CALL FILL (7, 0, TTRC)
C                                       graphics channels
      CALL FILL (8, 0, GRCS)
      GRCS(1) = 4
      GRCS(2) = 1
      GRCS(3) = 3
      GRCS(4) = 2
      GRCS(5) = 5
C                                       open images
      CALL IMGOPN (IMAGE(1), 'READ', IERR)
      IF (IERR.NE.0) GO TO 999
      IF (TVCS(1,2).GT.0) THEN
         CALL IMGOPN (IMAGE(2), 'READ', IERR)
         IF (IERR.NE.0) GO TO 995
         END IF
C                                       put in window parm
      NBOX = 0
      DIM(1) = 1
      DIM(2) = 1
      CALL OPUT (IMAGE(1), 'NBOXES', OOAINT, DIM, NBOX, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (TVCS(1,2).GT.0) THEN
         CALL OPUT (IMAGE(2), 'NBOXES', OOAINT, DIM, NBOX, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      DIM(1) = 4*MXNBOX
      CALL FILL (DIM(1), 0, WIN)
      DIM(1) = 4
      DIM(2) = MXNBOX
      CALL OPUT (IMAGE(1), 'WINDOW', OOAINT, DIM, WIN, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      IF (TVCS(1,2).GT.0) THEN
         CALL OPUT (IMAGE(2), 'WINDOW', OOAINT, DIM, WIN, CDUMMY, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       open TV device
      TVNAME = 'Test the OOPs TV'
      CALL TVDCRE (TVNAME, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TVDOPN (TVNAME, STATUS, IERR)
      IF (IERR.NE.0) GO TO 985
C                                       Init the TV
      CALL OTVINI (TVNAME, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       get TV parms
      CALL OTVPRM (TVNAME, NGRY, NGRPH, TVMAX, TVWND, CSIZ, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       open the TTY
      CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *   IERR)
      IF (IERR.NE.0) THEN
         TTY(2) = 0
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         GO TO 980
         END IF
      TTY(2) = MAX (1, TTY(2))
C                                       install parms
      DIM(1) = 4
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      DIM(1) = 1
      CALL TVDPUT (TVNAME, 'OFMCONT', OOAINT, DIM, NCONTR, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL TVDPUT (TVNAME, 'WWIDTH', OOAINT, DIM, WWIDTH, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TVDPUT (TVNAME, 'DOACROSS', OOALOG, DIM, DOACRO, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL TVDPUT (TVNAME, 'LTYPE', OOAINT, DIM, LTYPE, DUMMY, IERR)
      IF (IERR.NE.0) GO TO 999
      DIM(1) = 48
      CALL TVDPUT (TVNAME, 'OFMFILE', OOACAR, DIM, DUMMY, OFMFIL, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       all off
      IF (TVCS(1,1).NE.1) THEN
         CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,3), CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL OTVOFF (TVNAME, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
      CALL FILL (4, 0, TVCS(1,3))
      TVCS(1,3) = GRCS(3)
C                                       select image 2
      IF (TVCS(1,2).GT.0) THEN
         DIM(1) = LEN (IMAGE(2))
         DIM(2) = 1
         CALL TVDPUT (TVNAME, 'TVOBJECT', OOACAR, DIM, DUMMY, IMAGE(2),
     *      IERR)
         IF (IERR.NE.0) GO TO 980
         CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,2), CDUMMY,
     *      IERR)
         IF (IERR.NE.0) GO TO 980
C                                       load the image
         CALL OTVLOD (TVNAME, IERR)
         IF (IERR.NE.0) GO TO 980
         END IF
C                                       select image 1
      DIM(1) = LEN (IMAGE(1))
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'TVOBJECT', OOACAR, DIM, DUMMY, IMAGE(1),
     *   IERR)
      IF (IERR.NE.0) GO TO 980
      CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,1), CDUMMY,
     *   IERR)
      IF (IERR.NE.0) GO TO 980
C                                       load the image
      CALL OTVON (TVNAME, IERR)
      IF (IERR.NE.0) GO TO 980
      CALL OTVLOD (TVNAME, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Lets play
      IMA = 1
      NCOL = 2
      NROWS(1) = 21
      NROWS(2) = 15
      IF (TVCS(1,2).GT.0) NROWS(2) = 20
      TIMLIM = MIN (3600, MAX (120, TIMLIM))
      TYPE = -1
      ISHELP = 'PLAYR'
      TITLE = ' '
      NTITLE = 0
C                                       loop to menu
 100  CALL TVDMEN (TVNAME, TYPE, NCOL, NROWS, GRCS, TOPSEP, SIDSEP,
     *   ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE, CHS, TVBUTT,
     *   IERR)
      IF (IERR.EQ.0) THEN
         IF (TVBUTT.LE.0) THEN
            MSGTXT = 'Menu read timed out'
            CALL MSGWRT (3)
            GO TO 980
C                                       Call back: fiddle LUTs
C                                       TVFIDDLE
         ELSE IF (CHOICS(CHS).EQ.'TVFIDDLE') THEN
            CALL OTVFID (TVNAME, IERR)
C                                       TVPSEUDO
         ELSE IF (CHOICS(CHS).EQ.'TVPSEUDO') THEN
            CALL OTVPSU (TVNAME, IERR)
C                                       TVPHLAME
         ELSE IF (CHOICS(CHS).EQ.'TVFLAME') THEN
            CALL OTVFLA (TVNAME, IERR)
C                                       TVZOOM
         ELSE IF (CHOICS(CHS).EQ.'TVZOOM') THEN
            CALL OTVZOM (TVNAME, IERR)
C                                       TVTRAN
         ELSE IF (CHOICS(CHS).EQ.'TVTRAN') THEN
            CALL OTVTRA (TVNAME, IERR)
C                                       OFFCOLOR
         ELSE IF (CHOICS(CHS).EQ.'OFFCOLOR') THEN
            CALL OTVOFC (TVNAME, IERR)
C                                       OFFTRAN
         ELSE IF (CHOICS(CHS).EQ.'OFFTRAN') THEN
            CALL OTVOFT (TVNAME, IERR)
C                                       OFFZOOM
         ELSE IF (CHOICS(CHS).EQ.'OFFZOOM') THEN
            CALL OTVOFZ (TVNAME, IERR)
C                                       CURVALUE
         ELSE IF (CHOICS(CHS).EQ.'CURVALUE') THEN
            DIM(1) = 4
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVALU (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       Gamma Set
         ELSE IF (CHOICS(CHS).EQ.'GAMMASET') THEN
            DIM(1) = 4
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVOGA (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       OFM contours
         ELSE IF (CHOICS(CHS).EQ.'OFMCONTOUR') THEN
            DIM(1) = 4
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVOCO (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       OFM wedge contours
         ELSE IF (CHOICS(CHS).EQ.'OFMWEDGE') THEN
            DIM(1) = 4
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVOWE (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       OFM adjust
         ELSE IF (CHOICS(CHS).EQ.'OFMADJUST') THEN
            DIM(1) = 4
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVOAJ (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       OFM tweak
         ELSE IF (CHOICS(CHS).EQ.'OFM TWEAK') THEN
            CALL OTVOTW (TVNAME, IERR)
C                                       OFM dir
         ELSE IF (CHOICS(CHS).EQ.'OFM DIR') THEN
            CALL OTVODI (TVNAME, IERR)
C                                       OFM get
         ELSE IF (CHOICS(CHS).EQ.'OFM GET') THEN
            IF (OFMFIL.EQ.' ') THEN
               MSGTXT = 'SET OFM NAME FIRST'
               CALL MSGWRT (6)
            ELSE
               CALL OTVOGE (TVNAME, IERR)
               END IF
C                                       OFM put
         ELSE IF (CHOICS(CHS).EQ.'OFM PUT') THEN
            IF (OFMFIL.EQ.' ') THEN
               MSGTXT = 'SET OFM NAME FIRST'
               CALL MSGWRT (6)
            ELSE
               CALL OTVOPU (TVNAME, IERR)
               END IF
C                                       OFM get
         ELSE IF (CHOICS(CHS).EQ.'OFM ZAP') THEN
            IF (OFMFIL.EQ.' ') THEN
               MSGTXT = 'SET OFM NAME FIRST'
               CALL MSGWRT (6)
            ELSE
               CALL OTVOZP (TVNAME, IERR)
               END IF
C                                       OFM name set
         ELSE IF (CHOICS(CHS).EQ.'SET OFM NAME') THEN
            MSGBUF = 'Enter file name (LEFT JUSTIFIED)'
            CALL INQSTR (TTY, MSGBUF, 48, OFMFIL, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL CHLTOU (48, OFMFIL)
            DIM(1) = 48
            CALL TVDPUT (TVNAME, 'OFMFILE', OOACAR, DIM, DUMMY, OFMFIL,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       ncont set
         ELSE IF (CHOICS(CHS).EQ.'SET # CONT') THEN
            MSGBUF = 'Enter number of contours/wedges <= 16'
            CALL INQINT (TTY, MSGBUF, 1, NCONTR, IERR)
            IF (IERR.NE.0) NCONTR = 8
            IF (NCONTR.LE.0) NCONTR = 8
            IF (NCONTR.GT.16) NCONTR = 8
            DIM(1) = 1
            CALL TVDPUT (TVNAME, 'OFMCONT', OOAINT, DIM, NCONTR, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       wwidth set
         ELSE IF (CHOICS(CHS).EQ.'WEDGE WIDTH') THEN
            MSGBUF = 'Enter width of step wedges, < 0 -> cursor sets'
     *         // ' wedge position'
            CALL INQINT (TTY, MSGBUF, 1, WWIDTH, IERR)
            IF (IERR.NE.0) WWIDTH = -16
            IF (NCONTR.EQ.0) WWIDTH = -16
            DIM(1) = 1
            CALL TVDPUT (TVNAME, 'WWIDTH', OOAINT, DIM, WWIDTH, DUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 999
C                                       EXIT
         ELSE IF (CHOICS(CHS).EQ.'EXIT') THEN
            MSGTXT = 'EXIT was selected --- bye!'
            CALL MSGWRT (2)
            GO TO 980
C                                       set window
         ELSE IF (CHOICS(CHS).EQ.'SET WINDOW') THEN
            DIM(1) = 4
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVWIN (TVNAME, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVOFG (TVNAME, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVCLC (TVNAME, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVLOD (TVNAME, IERR)
C                                       reset window
         ELSE IF (CHOICS(CHS).EQ.'RESET WINDOW') THEN
            DIM(1) = 7
            DIM(2) = 1
            CALL OPUT (IMAGE(IMA), 'TBLC', OOAINT, DIM, TBLC, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OPUT (IMAGE(IMA), 'TTRC', OOAINT, DIM, TTRC, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVLOD (TVNAME, IERR)
C                                       TVBOX
         ELSE IF (CHOICS(CHS).EQ.'TVBOX') THEN
            NBOX = 0
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (IMAGE(IMA), 'NBOXES', OOAINT, DIM, NBOX, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            DIM(1) = 4
            TVCS(1,3) = GRCS(5)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            TVCS(1,3) = GRCS(3)
C                                       ignore IERR deliberately
            CALL OTVBOX (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       REBOX
         ELSE IF (CHOICS(CHS).EQ.'REBOX') THEN
            DIM(1) = 4
            TVCS(1,3) = GRCS(5)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            TVCS(1,3) = GRCS(3)
C                                       ignore IERR deliberately
            CALL OTVBOX (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
C                                       TV horiz wedge
         ELSE IF (CHOICS(CHS).EQ.'TV WEDGE') THEN
            DIM(1) = 1
            LTEMP = .TRUE.
            CALL TVDPUT (TVNAME, 'WPIXR', OOALOG, DIM, LTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            ITEMP = 3
            CALL TVDPUT (TVNAME, 'WSIDE', OOAINT, DIM, ITEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVWED (TVNAME, IERR)
C                                       IM horiz wedge
         ELSE IF (CHOICS(CHS).EQ.'IMAGE WEDGE') THEN
            DIM(1) = 1
            LTEMP = .FALSE.
            CALL TVDPUT (TVNAME, 'WPIXR', OOALOG, DIM, LTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            ITEMP = 3
            CALL TVDPUT (TVNAME, 'WSIDE', OOAINT, DIM, ITEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVWED (TVNAME, IERR)
C                                       TV vertical wedge
         ELSE IF (CHOICS(CHS).EQ.'TV VERT WEDGE') THEN
            DIM(1) = 1
            LTEMP = .TRUE.
            CALL TVDPUT (TVNAME, 'WPIXR', OOALOG, DIM, LTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            ITEMP = 2
            CALL TVDPUT (TVNAME, 'WSIDE', OOAINT, DIM, ITEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVWED (TVNAME, IERR)
C                                       IM vertical wedge
         ELSE IF (CHOICS(CHS).EQ.'IM VERT WEDGE') THEN
            DIM(1) = 1
            LTEMP = .FALSE.
            CALL TVDPUT (TVNAME, 'WPIXR', OOALOG, DIM, LTEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            ITEMP = 2
            CALL TVDPUT (TVNAME, 'WSIDE', OOAINT, DIM, ITEMP, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVWED (TVNAME, IERR)
C                                       Erase wedge
         ELSE IF (CHOICS(CHS).EQ.'ERASE WEDGE') THEN
            CALL OTVWER (TVNAME, IERR)
C                                       Image label
         ELSE IF (CHOICS(CHS).EQ.'LABEL IMAGE') THEN
            DIM(1) = 4
            TVCS(1,3) = GRCS(4)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVLAB (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS,
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            TVCS(1,3) = GRCS(3)
C                                       Wedge label
         ELSE IF (CHOICS(CHS).EQ.'LABEL WEDGE') THEN
            DIM(1) = 4
            TVCS(1,3) = GRCS(4)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVWEL (TVNAME, IERR)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IERR)
            IF (IERR.NE.0) GO TO 980
            TVCS(1,3) = GRCS(3)
C                                       On label
         ELSE IF (CHOICS(CHS).EQ.'SHOW LABEL') THEN
            CALL TVDOPR (TVNAME, 'GRON', GRCS(4), IERR)
            CALL TVDOPR (TVNAME, 'GRON', GRCS(5), IERR)
            CALL TVDOPR (TVNAME, 'GRON', NGRPH, IERR)
C                                       Off label
         ELSE IF (CHOICS(CHS).EQ.'HIDE LABEL') THEN
            CALL TVDOPR (TVNAME, 'GROF', GRCS(4), IERR)
            CALL TVDOPR (TVNAME, 'GROF', GRCS(5), IERR)
            CALL TVDOPR (TVNAME, 'GROF', NGRPH, IERR)
C                                       Zero label
         ELSE IF (CHOICS(CHS).EQ.'CLEAR LABEL') THEN
            CALL TVDOPR (TVNAME, 'GRCL', GRCS(4), IERR)
            CALL TVDOPR (TVNAME, 'GRCL', GRCS(5), IERR)
            CALL TVDOPR (TVNAME, 'GRCL', NGRPH, IERR)
            CALL TVDOPR (TVNAME, 'GROF', GRCS(4), IERR)
            CALL TVDOPR (TVNAME, 'GROF', GRCS(5), IERR)
            CALL TVDOPR (TVNAME, 'GROF', NGRPH, IERR)
C                                       SELECT IMAGE 1 or 2
         ELSE IF (CHOICS(CHS)(:12).EQ.'SELECT IMAGE') THEN
            READ (CHOICS(CHS),1100) I
            IF (IMA.NE.I) THEN
               CALL OTVOFF (TVNAME, IERR)
               IF (IERR.NE.0) GO TO 980
               IMA = I
               DIM(1) = LEN (IMAGE(IMA))
               DIM(2) = 1
               CALL TVDPUT (TVNAME, 'TVOBJECT', OOACAR, DIM, DUMMY,
     *            IMAGE(IMA), IERR)
               IF (IERR.NE.0) GO TO 980
               CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM,
     *            TVCS(1,IMA), CDUMMY, IERR)
               IF (IERR.NE.0) GO TO 980
               CALL OTVON (TVNAME, IERR)
               END IF
C                                       TVTRA both
         ELSE IF (CHOICS(CHS).EQ.'TVTRAN BOTH') THEN
            TVCS(1,3) = TVCS(1,IMA)
            TVCS(2,3) = TVCS(1,3-IMA)
            CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVTRA (TVNAME, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,IMA),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            TVCS(1,3) = GRCS(3)
            TVCS(2,3) = 0
C                                       regular blink
         ELSE IF (CHOICS(CHS).EQ.'BLINK') THEN
            TVCS(1,3) = TVCS(1,IMA)
            TVCS(2,3) = TVCS(1,3-IMA)
            CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVBLK (TVNAME, 1, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,IMA),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            TVCS(1,3) = GRCS(3)
            TVCS(2,3) = 0
C                                       manual blink
         ELSE IF (CHOICS(CHS).EQ.'MANUAL BLINK') THEN
            TVCS(1,3) = TVCS(1,IMA)
            TVCS(2,3) = TVCS(1,3-IMA)
            CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,3),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL OTVBLK (TVNAME, 2, IERR)
            IF (IERR.NE.0) GO TO 980
            CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS(1,IMA),
     *         CDUMMY, IERR)
            IF (IERR.NE.0) GO TO 980
            TVCS(1,3) = GRCS(3)
            TVCS(2,3) = 0
            END IF
         GO TO 100
         END IF
C                                       close downs
 980  CALL TVDCLO (TVNAME, JERR)
 985  CALL TVDDES (TVNAME, JERR)
 990  IF (TVCS(1,2).GT.0) CALL IMGCLO (IMAGE(2), JERR)
 995  CALL IMGCLO (IMAGE(1), JERR)
      IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I6,' OPENING THE TERMINAL')
 1100 FORMAT (12X,I2)
      END
