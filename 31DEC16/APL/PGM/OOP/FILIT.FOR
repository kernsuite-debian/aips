LOCAL INCLUDE 'INPUT.INC'
C                                       Declarations for inputs
      INTEGER NPARMS
      PARAMETER (NPARMS=16)
      INTEGER AVTYPE(NPARMS), AVDIM(2,NPARMS)
      CHARACTER AVNAME(NPARMS)*8
LOCAL END
LOCAL INCLUDE 'INPUTDATA.INC'
C                                       DATA statments defining input
C                                       parameters.
      INCLUDE 'INCS:PAOOF.INC'
      DATA AVNAME /'INNAME', 'INCLASS', 'INSEQ', 'INDISK', 'NFIELD',
     *   'NGAUSS', 'BOXFILE', 'OBOXFILE', 'TXINC', 'TYINC', 'TBLC',
     *   'TTRC', 'PIXRANGE', 'FUNCTYPE', 'PRTLEV', 'IM2PARM'/
      DATA AVTYPE /OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOACAR, OOACAR, OOAINT, OOAINT, OOAINT,
     *   OOAINT, OOARE, OOACAR, OOAINT, OOARE/
      DATA AVDIM /12,1, 6,1, 1,1, 1,1, 1,1,
     *   1,1, 48,1, 48,1, 1,1,  1,1, 7,1,
     *   7,1,  2,1,  2,1, 1,1, 40,1/
LOCAL END
      PROGRAM FILIT
C-----------------------------------------------------------------------
C! find boxes to clean existing images
C# Task IMAGING OOP
C-----------------------------------------------------------------------
C;  Copyright (C) 2009, 2011-2014
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
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER PRGM*6, IN(MAXFLD)*32
      INTEGER   NIMAGE, IRET, BUFF1(256), FNUM(MAXFLD), MAXDIM(2), I,
     *   OIND, FFIELD
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'FILIT '/
C-----------------------------------------------------------------------
C                                       Startup
      I = MAXFLD
      CALL FILITI (PRGM, I, IN, NIMAGE, FFIELD, FNUM, MAXDIM, OIND,
     *   IRET)
      RQUICK = .FALSE.
      IF (IRET.NE.0) GO TO 990
C                                       find boxes
      CALL FILITD (IN, NIMAGE, FFIELD, FNUM, MAXDIM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       write them out
      CALL FILITO (IN, NIMAGE, FNUM, OIND, IRET)
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FILITI (PRGN, NMMAX, IN, NIMAGE, FFIELD, FNUM, MAXDIM,
     *   OIND, IRET)
C-----------------------------------------------------------------------
C   FILITI gets input parameters for FILIT.
C   Inputs:
C      PRGN     C*6      Program name
C      NMMAX    I        Max dimension of IN
C   Output:
C      IN       C(*)*?   Input object
C      NIMAGE   I        Number of images in IN
C      FFIELD   I        First facet number
C      FNUM     I(*)     Facet numbers of images in IN
C      MAXDIM   I        Maximum X,Y dimensions of window
C      OIND     I        FTAB pointer for open LUN=10 OBOXFILE
C      IRET     I        Error code: 0 => ok
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NMMAX, NIMAGE, FFIELD, FNUM(*), MAXDIM(2), OIND, IRET
      CHARACTER PRGN*6, IN(*)*(*)
C
      INTEGER   NKEY1
C                                       NKEY1=no. adverbs for inname
      PARAMETER (NKEY1=9)
      INTEGER   DIM(7), TYPE, BLC(7), TRC(7), NAXIS(7), DUMMY, NFIELD,
     *   LFIELD, IDUMMY, MSGSAV, KFIELD, ILUN, OLUN, IIND, I, J, J0,
     *   KBP, IPARM(5), NBOX(MAXFLD), BOXES(4,MXNBOX,MAXFLD), TXINC,
     *   TYINC, PRTLEV, IROUND, NGAUSS, NMAPS, JTRIM, MFIELD, K, CNO
      CHARACTER INK1(NKEY1)*8, OUTK1(NKEY1)*32, CLASS*6, CDUMMY*1,
     *   CFIELD*11, TEST*32, ICLASS*6, IBXFIL*48, OBXFIL*48, LINE*132
      REAL      IM2PRM(40), AUTOBX(6)
      LOGICAL   OLDNAM
      DOUBLE PRECISION X
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INPUT.INC'
      INCLUDE 'INPUTDATA.INC'
C                                       Adverbs for IN
      DATA INK1 /'INNAME', 'INDISK', 'INSEQ', 'TBLC', 'TTRC', 'TXINC',
     *   'TYINC', 'PIXRANGE', 'FUNCTYPE'/
C                                       Rename
      DATA OUTK1 /'NAME', 'DISK', 'IMSEQ', 'TBLC', 'TTRC', 'TXINC',
     *   'TYINC', 'PIXRANGE', 'FUNCTYPE'/
      DATA ILUN, OLUN /11,10/
C-----------------------------------------------------------------------
      NIMAGE = 0
      MAXDIM(1) = 0
      MAXDIM(2) = 0
      CALL FILL (MAXFLD, 0, NBOX)
C                                       Startup as interactive
      CALL AV2INT (PRGN, NPARMS, AVNAME, AVTYPE, AVDIM, 'Input', IRET)
      IF (IRET.NE.0) GO TO 999
C                                       other inputs
      CALL OGET ('Input', 'NFIELD', TYPE, DIM, NFIELD, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'NGAUSS', TYPE, DIM, NGAUSS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NMAPS = MAX (1, NFIELD) * MAX (1, NGAUSS)
      NMAPS = MAX (1, MIN (MAXFLD, NMAPS))
      CALL OGET ('Input', 'TXINC', TYPE, DIM, TXINC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'TYINC', TYPE, DIM, TYINC, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      TXINC = MAX (1, TXINC)
      TYINC = MAX (1, TYINC)
      CALL OGET ('Input', 'PRTLEV', TYPE, DIM, PRTLEV, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'INCLASS', TYPE, DIM, DUMMY, CLASS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       auto-box defaults
      CALL OGET ('Input', 'IM2PARM', TYPE, DIM, IM2PRM, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      AUTOBX(1) = IROUND (IM2PRM(1))
      AUTOBX(2) = IM2PRM(2)
      AUTOBX(3) = IM2PRM(3)
      AUTOBX(4) = IM2PRM(4)
      AUTOBX(5) = IROUND (IM2PRM(5))
      AUTOBX(6) = IROUND (IM2PRM(6))
      AUTOBX(1) = MAX (1.0, MIN (200.0, AUTOBX(1)))
      IF (AUTOBX(2).LE.1.5) AUTOBX(2) = 3.
      IF (AUTOBX(3).LT.AUTOBX(2)) AUTOBX(3) = AUTOBX(2) + 2.
      IF (AUTOBX(4).LT.0.01) AUTOBX(4) = 0.1
      IF (AUTOBX(4).GT.0.90) AUTOBX(4) = 0.1
      IF (AUTOBX(5).LT.-1.0) AUTOBX(5) = 1.0
      IF (AUTOBX(5).GT.6.0) AUTOBX(5) = 6.0
      IF (AUTOBX(6).EQ.0.0) AUTOBX(6) = 5.0
      IF (AUTOBX(6).LT.0.0) AUTOBX(6) = -1.0
      IF (AUTOBX(6).GT.50.) AUTOBX(6) = 5.0
C                                       box file names
      CALL OGET ('Input', 'BOXFILE', TYPE, DIM, IDUMMY, IBXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET ('Input', 'OBOXFILE', TYPE, DIM, IDUMMY, OBXFIL, IRET)
      IF (IRET.NE.0) GO TO 999
      IF (OBXFIL.EQ.' ') OBXFIL = IBXFIL
      IF (OBXFIL.EQ.IBXFIL) THEN
         J = JTRIM (IBXFIL)
         OBXFIL = IBXFIL(:J) // 'O'
         MSGTXT = 'OBOXFILE CHANGED TO NOT EQUAL BOXFILE'
         CALL MSGWRT (7)
         END IF
      IF ((OBXFIL.EQ.' ') .OR. (OBXFIL.EQ.IBXFIL)) THEN
         IRET = 8
         MSGTXT = 'OBOXFILE MUST BE SPECIFIED AND NOT BOXFILE'
         GO TO 990
         END IF
C                                       open output text file
      CALL ZTXOPN ('WRIT', OLUN, OIND, OBXFIL, .FALSE., IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'OPEN OF OBOXFILE FAILS'
         GO TO 990
         END IF
C                                       copy input boxfile if diff
      J = MAXFLD
      CALL FILL (J, 0, NBOX)
      J = J * 4 * MXNBOX
      CALL FILL (J, 0, BOXES)
      MFIELD = NMAPS
      IIND = -1
      IF (IBXFIL.NE.' ') THEN
         CALL ZTXOPN ('READ', ILUN, IIND, IBXFIL, .FALSE., IRET)
         IF (IRET.NE.0) THEN
            MSGTXT = 'OPEN OF INPUT BOXFILE FAILS'
            GO TO 990
            END IF
C                                       read loop
         I = 0
 10      CALL ZTXIO ('READ', ILUN, IIND, LINE, IRET)
         IF (IRET.EQ.0) THEN
            I = I + 1
            CALL CHTRIM (LINE, 132, LINE, J)
C                                       copy non-box lines
            IF ((LINE(:1).LT.'0') .OR. (LINE(:1).GT.'9')) THEN
               CALL ZTXIO ('WRIT', OLUN, OIND, LINE(:J), IRET)
               IF (IRET.NE.0) THEN
                  MSGTXT = 'WRITE ERROR TO OBOXFILE'
                  GO TO 990
                  END IF
C                                       parse clean box
            ELSE
C                                       field, blc, trc
               KBP = 1
               CALL FILL (5, 0, IPARM)
               DO 20 J = 1,5
                  CALL GETNUM (LINE, 132, KBP, X)
                  IF (X.EQ.DBLANK) THEN
                     IF (J.EQ.1) GO TO 20
                     WRITE (MSGTXT,1040) I, J
                     CALL MSGWRT (6)
                     IRET = 1
                     GO TO 999
                  ELSE
                     IF (X.GE.0.0D0) THEN
                        IPARM(J) = X + 0.50D0
                     ELSE
                        IPARM(J) = X - 0.50D0
                        END IF
                     END IF
 20               CONTINUE
               KFIELD = IPARM(1)
               MFIELD = MAX (MFIELD, KFIELD)
               IF (KFIELD.GT.0) THEN
                  IF ((IPARM(2).NE.0) .AND. (IPARM(3).GT.0) .AND.
     *               (IPARM(4).GT.0) .AND. (IPARM(5).GT.0)) THEN
                     NBOX(KFIELD) = NBOX(KFIELD) + 1
                     CALL COPY (4, IPARM(2),
     *                  BOXES(1,NBOX(KFIELD),KFIELD))
                     END IF
                  END IF
               END IF
            GO TO 10
         ELSE IF (IRET.NE.2) THEN
            MSGTXT = 'READ ERROR IN BOXFILE'
            GO TO 990
            END IF
         END IF
C                                       allowed class types
      IF (CLASS.EQ.' ') CLASS = 'ICL001'
      OLDNAM = (CLASS(4:4).LT.'0') .OR. (CLASS(4:4).GT.'9') .OR.
     *   (CLASS(5:5).LT.'0') .OR. (CLASS(5:5).GT.'9') .OR.
     *   (CLASS(6:6).LT.'0') .OR. (CLASS(6:6).GT.'9')
      FFIELD = 1
      J0 = 1
      IF (.NOT.OLDNAM) READ (CLASS(4:6),1001) J0
      ICLASS = CLASS
      DO 40 KFIELD = 1,MFIELD
         LFIELD = KFIELD
         WRITE (CFIELD,1000) LFIELD
         TEST = 'Input image object ' // CFIELD
         CALL CREATE (TEST, 'IMAGE', IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Copy adverbs to object
         CALL IN2OBJ ('Input', NKEY1, INK1, OUTK1, TEST, IRET)
         IF (IRET.NE.0) GO TO 999
         DIM(1) = 6
         DIM(2) = 1
         IF (OLDNAM) THEN
            CALL ZEHEX (LFIELD-1, 2, CFIELD(:2))
            IF (LFIELD.GT.1) ICLASS(5:6) = CFIELD(:2)
         ELSE IF (LFIELD.LT.1000) THEN
            ICLASS(4:6) = CFIELD(2:4)
         ELSE
            ICLASS(3:6) = CFIELD(1:4)
            END IF
         CALL OPUT (TEST, 'CLASS', OOACAR, DIM, IDUMMY, ICLASS, IRET)
         IF (IRET.NE.0) GO TO 999
C                                       Open and close to check
         MSGSAV = MSGSUP
c         MSGSUP = 32000
         CALL OOPEN (TEST, 'READ', IRET)
         MSGSUP = MSGSAV
         CALL FNAGET (TEST, 'CNO', TYPE, DIM, CNO, CDUMMY, IRET)
C                                       skip missing ones
         IF (IRET.NE.0) THEN
            IRET = 0
            WRITE (MSGTXT,1020) LFIELD
            CALL MSGWRT (7)
            IF (NBOX(LFIELD).GT.0) THEN
               WRITE (MSGTXT,1021) NBOX(LFIELD)
               CALL MSGWRT (7)
               DO 25 J = 1,NBOX(LFIELD)
                  WRITE (LINE,1025) LFIELD, (BOXES(K,J,LFIELD), K = 1,4)
                  CALL ZTXIO ('WRIT', OLUN, OIND, LINE(:40), IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1026) 'WRITE OBOXFILE', LFIELD
                     GO TO 990
                     END IF
 25               CONTINUE
               END IF
C                                       got it - continue
         ELSE
            IF (NIMAGE.EQ.NMMAX) THEN
               MSGTXT = 'TOO MANY FIELDS*POINTINGS !!!!'
               CALL MSGWRT (8)
               IRET = 8
               GO TO 999
               END IF
            NIMAGE = NIMAGE + 1
            IN(NIMAGE) = TEST
            FNUM(NIMAGE) = LFIELD
            IF (LFIELD.EQ.J0) FFIELD = NIMAGE
C                                       Input subimage dimension
            CALL ARRWIN (TEST, BLC, TRC, NAXIS, IRET)
            IF (IRET.NE.0) GO TO 999
            MAXDIM(1) = MAX (MAXDIM(1), (TRC(1)-BLC(1))/TXINC+1)
            MAXDIM(2) = MAX (MAXDIM(2), (TRC(2)-BLC(2))/TYINC+1)
            CALL COPY (5, BLC(3), TRC(3))
            DIM(1) = 7
            DIM(2) = 1
            CALL OPUT (TEST, 'TRC', OOAINT, DIM, TRC, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = 1
            CALL OPUT (TEST, 'NBOXES', OOAINT, DIM, NBOX(LFIELD),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = 4
            DIM(2) = MXNBOX
            CALL OPUT (TEST, 'WINDOW', OOAINT, DIM, BOXES(1,1,LFIELD),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (TEST, 'BOXMSGL', OOAINT, DIM, PRTLEV, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            DIM(1) = 6
            DIM(2) = 1
            CALL OPUT (TEST, 'AUTOBOX', OOARE, DIM, AUTOBX, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
            CALL OCLOSE (TEST, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 40      CONTINUE
C                                       end of file: close input
      IF (IIND.GT.0) CALL ZTXCLS (ILUN, IIND, IRET)
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4.4)
 1001 FORMAT (I3)
 1020 FORMAT ('WARNING: FIELD',I5,' APPEARS TO BE MISSING')
 1021 FORMAT ('WRITING',I5,' BOXES OUT ANYWAY')
 1025 FORMAT (I6.4,2X,4I8)
 1026 FORMAT ('ERROR ON ',A,' IMAGE',I5)
 1040 FORMAT ('FORMAT ERROR BOXFILE LINE',I6,' PARAMETER',I2)
      END
      SUBROUTINE FILITO (IN, NIMAGE, FNUM, OIND, IRET)
C-----------------------------------------------------------------------
C   FILITO writes out the Clean boxes
C   Inputs:
C      IN      C*(*)     Class names
C      NIMAGE  I         Number fields
C      FNUM    I(*)      Facet number for list
C      OIND    I         FTAB pointer to LUN 10
C   Output:
C      IRET    I         Error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER IN(*)*(*)
      INTEGER   NIMAGE, FNUM(*), OIND, IRET
C
      CHARACTER LINE*40, CDUMMY*1
      INTEGER   OLUN, I, J, K, L, TYPE, DIM(7), NBOX, BOXES(4,MXNBOX)
      INCLUDE 'INCS:DMSG.INC'
      DATA OLUN /10/
C-----------------------------------------------------------------------
      DO 20 L = 1,NIMAGE
         CALL OGET (IN(L), 'NBOXES', TYPE, DIM, NBOX, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET (IN(L), 'WINDOW', TYPE, DIM, BOXES, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         J = FNUM(L)
         IF (NBOX.LE.0) THEN
            NBOX = 1
            CALL FILL (4, 0, BOXES(1,1))
            END IF
         DO 10 I = 1,NBOX
            WRITE (LINE,1000) J, (BOXES(K,I), K = 1,4)
            CALL ZTXIO ('WRIT', OLUN, OIND, LINE, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1001) 'WRITE OBOXFILE', FNUM(I)
               GO TO 990
               END IF
 10         CONTINUE
 20      CONTINUE
C
 990  IF (IRET.NE.0) CALL MSGWRT (8)
C
      CALL ZTXCLS (OLUN, OIND, I)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I6.4,2X,4I8)
 1001 FORMAT ('ERROR ON ',A,' IMAGE',I5)
      END
      SUBROUTINE FILITD (IN, NMAPS, FFIELD, FNUM, MAXDIM, IRET)
C-----------------------------------------------------------------------
C   Loops over input fields: load memory, find rms, find islands,
C   write boxes.
C   Inputs:
C      IN       C(*)*?   Input image(s)
C      NMAPS    I        Number images in IN
C      FNUM     I(*)     Facet number of each image
C      MAXDIM   I(2)     X, Y max number pixels
C   Output:
C      IRET     I        Error code: 0 okay
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER IN(*)*(*)
      INTEGER   NMAPS, FFIELD, FNUM(*), MAXDIM(2), IRET
C
      INCLUDE 'INCS:PAOOF.INC'
      INTEGER NCHOIC
      PARAMETER (NCHOIC = 22)
C
      INTEGER   TYPE, DIM(7), I, TVCS(16), GRCS(8,2), NBOX, NCOL,
     *   NROWS(2), IMA, TIMLIM, TVBUTT, CHS, JERR, NGRY, TVMAX(2),
     *   TVWND(4), CSIZ(2), NGRPH, DUMMY, PNBOX, PBOXES(4,MXNBOX),
     *   TTY(2), IMSIZE(2,MAXFLD), WIN(4,MXNBFL), NGAUSS, NTITLE,
     *   TOPSEP, SIDSEP
      CHARACTER STATUS*4, CDUMMY*1, TVNAME*32, CHOICS(NCHOIC)*16,
     *   ISHELP*6, TITLE*10, MSGBUF*72
      LOGICAL   DOROAM, LEAVE(NCHOIC), BXONLY, FIRST
      REAL      AUTOBX(6)
      DOUBLE PRECISION D
      INCLUDE 'INCS:DMSG.INC'
      DATA CHOICS /'EXIT', ' ', 'OFFZOOM', 'OFFTRAN', 'OFFCOLOR',
     *   'TVFIDDLE', 'TVZOOM', 'TVTRAN', 'TVPSEUDO', 'TVFLAME',
     *   'CURVALUE', 'REBOX', 'DELBOX', 'AUTOBOX', 'AUTOBOX PARMS',
     *   'DEL ALL BOXES', 'RESET BOXES', 'CHECK BOXES', 'NEXT FACET',
     *   'LAST FACET', 'ENTER FACET', 'REROAM'/
      DATA LEAVE /2*.TRUE., 16*.FALSE., 4*.TRUE./
C-----------------------------------------------------------------------
      CALL OGET ('Input', 'NGAUSS', TYPE, DIM, NGAUSS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NGAUSS = MAX (1, NGAUSS)
C                                       interactive terminal
      TTY(1) = 5
      TTY(2) = 0
      CALL ZOPEN (TTY(1), TTY(2), 1, MSGBUF, .FALSE., .TRUE., .TRUE.,
     *   IRET)
      IF (IRET.NE.0) THEN
         TTY(2) = 0
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Get TV control
C                                       graphics channels
      CALL FILL (16, 0, GRCS)
      GRCS(1,1) = 4
      GRCS(2,1) = 1
      GRCS(3,1) = 3
      GRCS(4,1) = 2
      GRCS(5,1) = 5
      GRCS(1,2) = 1
      GRCS(2,2) = 2
      GRCS(3,2) = 3
      GRCS(4,2) = 4
      IMSIZE(1,1) = 0
C                                       open TV device
      TVNAME = 'TV for boxing'
      CALL TVDCRE (TVNAME, IRET)
      IF (IRET.NE.0) GO TO 990
      CALL TVDOPN (TVNAME, STATUS, IRET)
      IF (IRET.NE.0) GO TO 985
C                                       Init the TV
      CALL OTVINI (TVNAME, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       get TV parms
      CALL OTVPRM (TVNAME, NGRY, NGRPH, TVMAX, TVWND, CSIZ, IRET)
      IF (IRET.NE.0) GO TO 980
      DOROAM = (MAXDIM(1).GT.TVMAX(1)) .OR. (MAXDIM(2).GT.TVMAX(2))
      CALL FILL (16, 0, TVCS)
      DO 10 I = 1,NGRY
         TVCS(I) = I
 10      CONTINUE
C                                       install parms
      DIM(1) = 8
      DIM(2) = 1
      CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       all off
      DIM(1) = 16
      CALL TVDPUT (TVNAME, 'TVCHANS', OOAINT, DIM, TVCS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 980
      CALL OTVOFF (TVNAME, IRET)
      IF (IRET.NE.0) GO TO 980
      DIM(1) = 1
      CALL OPUT (TVNAME, 'WINLOAD', OOAINT, DIM, GRCS(5,1), CDUMMY,
     *   IRET)
      IF (IRET.NE.0) GO TO 980
      CALL OPUT (TVNAME, 'XWINLOAD', OOAINT, DIM, 6, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       Lets play
      IMA = MAX (0, FFIELD - 1)
      NCOL = 1
      NROWS(1) = NCHOIC
      IF (.NOT.DOROAM) THEN
         NROWS(1) = NCHOIC - 1
         IF (NMAPS.LE.1) NROWS(1) = NROWS(1) - 4
      ELSE IF (NMAPS.LE.1) THEN
         NROWS(1) = NCHOIC - 4
         CHOICS(NROWS(1)) = CHOICS(NCHOIC)
         LEAVE(NROWS(1)) = LEAVE(NCHOIC)
         END IF
      NROWS(2) = 0
      TIMLIM = 0
      TYPE = -1
      ISHELP = 'FILIT'
      TITLE = ' '
      CHS = 19
      TVBUTT = 1
      CALL OGET (IN(1), 'AUTOBOX', TYPE, DIM, AUTOBX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 980
C                                       skip read 1st time
      FIRST = .TRUE.
C                                       loop to menu
 100  WRITE (TITLE,1100) IMA
      CALL REFRMT (TITLE, ' ', I)
      NTITLE = 1
      TOPSEP = 5
      SIDSEP = 5
      IF (.NOT.FIRST) CALL TVDMEN (TVNAME, TYPE, NCOL, NROWS, GRCS,
     *   TOPSEP, SIDSEP,ISHELP, CHOICS, TIMLIM, LEAVE, NTITLE, TITLE,
     *   CHS, TVBUTT,IRET)
      IF (IRET.EQ.0) THEN
         IF (TVBUTT.LE.0) THEN
            MSGTXT = 'Menu read timed out'
            CALL MSGWRT (3)
            GO TO 980
C                                       Call back: fiddle LUTs
C                                       TVFIDDLE
         ELSE IF (CHOICS(CHS).EQ.'TVFIDDLE') THEN
            CALL OTVFID (TVNAME, IRET)
C                                       TVZOOM
         ELSE IF (CHOICS(CHS).EQ.'TVZOOM') THEN
            CALL OTVZOM (TVNAME, IRET)
C                                       TVTRAN
         ELSE IF (CHOICS(CHS).EQ.'TVTRAN') THEN
            CALL OTVTRA (TVNAME, IRET)
C                                       TVPSEUDO
         ELSE IF (CHOICS(CHS).EQ.'TVPSEUDO') THEN
            CALL OTVPSU (TVNAME, IRET)
C                                       TVPHLAME
         ELSE IF (CHOICS(CHS).EQ.'TVFLAME') THEN
            CALL OTVFLA (TVNAME, IRET)
C                                       OFFCOLOR
         ELSE IF (CHOICS(CHS).EQ.'OFFCOLOR') THEN
            CALL OTVOFC (TVNAME, IRET)
C                                       OFFTRAN
         ELSE IF (CHOICS(CHS).EQ.'OFFTRAN') THEN
            CALL OTVOFT (TVNAME, IRET)
C                                       OFFZOOM
         ELSE IF (CHOICS(CHS).EQ.'OFFZOOM') THEN
            CALL OTVOFZ (TVNAME, IRET)
C                                       CURVALUE
         ELSE IF (CHOICS(CHS).EQ.'CURVALUE') THEN
            DIM(1) = 8
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL OTVALU (TVNAME, IRET)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,1),
     *         CDUMMY, IRET)
C                                       NEXT FACET or LAST FACET
         ELSE IF ((CHOICS(CHS).EQ.'NEXT FACET') .OR.
     *      (CHOICS(CHS).EQ.'LAST FACET') .OR.
     *      (CHOICS(CHS).EQ.'ENTER FACET')) THEN
            IF (CHOICS(CHS).EQ.'NEXT FACET') THEN
               IMA = IMA + 1
               IF (IMA.GT.NMAPS) IMA = 1
            ELSE IF (CHOICS(CHS).EQ.'LAST FACET') THEN
               IMA = IMA - 1
               IF (IMA.LT.1) IMA = NMAPS
            ELSE
               WRITE (MSGBUF,1105) NMAPS
               CALL INQINT (TTY, MSGBUF, 1, I, JERR)
               IF (JERR.LE.0) IMA = MAX (1, MIN (NMAPS, I))
               END IF
C                                       select image IMA
            DIM(1) = LEN (IN(IMA))
            DIM(2) = 1
            CALL TVDPUT (TVNAME, 'TVOBJECT', OOACAR, DIM, DUMMY,
     *         IN(IMA), IRET)
            IF (IRET.NE.0) GO TO 980
            CALL TVDOPR (TVNAME, 'GRCL', GRCS(5,1), IRET)
            IF (IRET.NE.0) GO TO 980
            CALL TVDOPR (TVNAME, 'GROF', GRCS(5,1), IRET)
            IF (IRET.NE.0) GO TO 980
            CALL OTVOFZ (TVNAME, IRET)
            IF (IRET.NE.0) GO TO 980
C                                       load the image
            IF (.NOT.DOROAM) THEN
               CALL TVDOPR (TVNAME, 'TVON', TVCS(1), IRET)
               IF (IRET.NE.0) GO TO 980
               CALL OTVLOD (TVNAME, IRET)
            ELSE
               CALL OTVROM (TVNAME, IRET)
               IF (IRET.NE.0) GO TO 980
               CALL OTVROF (TVNAME, IRET)
               END IF
            CALL OGET (IN(IMA), 'NBOXES', TYPE, DIM, PNBOX, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            CALL OGET (IN(IMA), 'WINDOW', TYPE, DIM, PBOXES, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
C                                       RESET BOXES
         ELSE IF ((CHOICS(CHS).EQ.'RESET BOXES') .OR.
     *      (CHOICS(CHS).EQ.'DEL ALL BOXES')) THEN
            I = PNBOX
            IF (CHOICS(CHS).EQ.'DEL ALL BOXES') I = 0
            TYPE = OOAINT
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (IN(IMA), 'NBOXES', TYPE, DIM, I, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            DIM(1) = 4
            DIM(2) = MXNBOX
            CALL OPUT (IN(IMA), 'WINDOW', TYPE, DIM, PBOXES, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
C                                       redisplay
            BXONLY = .TRUE.
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (TVNAME, 'BOXONLY', OOALOG, DIM, BXONLY, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            DIM(1) = 4
            GRCS(1,2) = GRCS(5,1)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            GRCS(1,2) = GRCS(2,1)
C                                       ignore IRET deliberately
            CALL OTVBOX (TVNAME, IRET)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            BXONLY = .FALSE.
            DIM(1) = 1
            CALL OPUT (TVNAME, 'BOXONLY', OOALOG, DIM, BXONLY, CDUMMY,
     *         IRET)
C                                       REROAM
         ELSE IF (CHOICS(CHS).EQ.'REROAM') THEN
            CALL TVDOPR (TVNAME, 'GRCL', GRCS(5,1), IRET)
            IF (IRET.NE.0) GO TO 980
            CALL TVDOPR (TVNAME, 'GROF', GRCS(5,1), IRET)
            IF (IRET.NE.0) GO TO 980
            CALL OTVOFZ (TVNAME, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL OTVRRM (TVNAME, IRET)
            IF (IRET.NE.0) GO TO 980
            CALL OTVROF (TVNAME, IRET)
C                                       EXIT
         ELSE IF (CHOICS(CHS).EQ.'EXIT') THEN
            MSGTXT = 'EXIT was selected --- bye!'
            CALL MSGWRT (2)
            GO TO 980
C                                       DELBOX
         ELSE IF (CHOICS(CHS).EQ.'DELBOX') THEN
            CALL OGET (IN(IMA), 'NBOXES', TYPE, DIM, NBOX, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            NBOX = -NBOX
            CALL OPUT (IN(IMA), 'NBOXES', TYPE, DIM, NBOX, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            DIM(1) = 4
            GRCS(1,2) = GRCS(5,1)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            GRCS(1,2) = GRCS(2,1)
C                                       ignore IRET deliberately
            CALL OTVBOX (TVNAME, IRET)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IRET)
C                                       REBOX
         ELSE IF (CHOICS(CHS).EQ.'REBOX') THEN
            DIM(1) = 4
            GRCS(1,2) = GRCS(5,1)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            GRCS(1,2) = GRCS(2,1)
C                                       ignore IRET deliberately
            CALL OTVBOX (TVNAME, IRET)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IRET)
C                                       AUTOBOX PARMS
         ELSE IF (CHOICS(CHS).EQ.'AUTOBOX PARMS') THEN
            I = AUTOBX(1) + 0.01
            WRITE (MSGBUF,1111) I
            CALL INQINT (TTY, MSGBUF, 1, I, JERR)
            IF (JERR.LE.0) THEN
               IF (I.GT.0) AUTOBX(1) = I
               WRITE (MSGBUF,1112) AUTOBX(2)
               CALL INQFLT (TTY, MSGBUF, 1, D, JERR)
               END IF
            IF (JERR.LE.0) THEN
               IF (D.GT.1.5) AUTOBX(2) = D
               WRITE (MSGBUF,1113) AUTOBX(3)
               CALL INQFLT (TTY, MSGBUF, 1, D, JERR)
               END IF
            IF (JERR.LE.0) THEN
               IF (D.GT.AUTOBX(2)) AUTOBX(3) = D
               WRITE (MSGBUF,1114) AUTOBX(4)
               CALL INQFLT (TTY, MSGBUF, 1, D, JERR)
               END IF
            IF (JERR.LE.0) THEN
               IF (D.GT.0.0) AUTOBX(4) = MAX (0.01D0, MIN (0.9D0, D))
               I = AUTOBX(5) + 0.01
               WRITE (MSGBUF,1115)
               CALL INQINT (TTY, MSGBUF, 1, I, JERR)
               END IF
            IF (JERR.LE.0) THEN
               IF ((I.GE.-1) .AND. (I.LE.10)) AUTOBX(5) = I
               I = AUTOBX(6) + 0.01
               WRITE (MSGBUF,1116) I
               CALL INQINT (TTY, MSGBUF, 1, I, JERR)
               END IF
            IF (JERR.LE.0) THEN
               IF (I.NE.0) AUTOBX(6) = I
               WRITE (MSGTXT,1120) AUTOBX(1), AUTOBX(2), AUTOBX(3),
     *            AUTOBX(5), AUTOBX(6)
               CALL MSGWRT (3)
               END IF
C                                       AUTOBOX
         ELSE IF (CHOICS(CHS).EQ.'AUTOBOX') THEN
C                                       set current autobox parms
            DIM(1) = 6
            DIM(2) = 1
            CALL OPUT (IN(IMA), 'AUTOBOX', OOARE, DIM, AUTOBX, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
C                                       do auto-box function
            CALL IMABOX (IN(IMA), IRET)
            IF (IRET.GT.0) GO TO 980
C                                       redisplay
            BXONLY = .TRUE.
            DIM(1) = 1
            CALL OPUT (TVNAME, 'BOXONLY', OOALOG, DIM, BXONLY, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            DIM(1) = 4
            GRCS(1,2) = GRCS(5,1)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            GRCS(1,2) = GRCS(2,1)
C                                       ignore IRET deliberately
            CALL OTVBOX (TVNAME, IRET)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            BXONLY = .FALSE.
            DIM(1) = 1
            CALL OPUT (TVNAME, 'BOXONLY', OOALOG, DIM, BXONLY, CDUMMY,
     *         IRET)
C                                       CHECK BOXES
         ELSE IF (CHOICS(CHS).EQ.'CHECK BOXES') THEN
            CALL FILITB (NMAPS, NGAUSS, IN, IMSIZE, WIN, IRET)
            IF (IRET.NE.0) GO TO 980
C                                       redisplay
            BXONLY = .TRUE.
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (TVNAME, 'BOXONLY', OOALOG, DIM, BXONLY, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            DIM(1) = 4
            GRCS(1,2) = GRCS(5,1)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS(1,2),
     *         CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 980
            GRCS(1,2) = GRCS(2,1)
C                                       ignore IRET deliberately
            CALL OTVBOX (TVNAME, IRET)
            CALL TVDPUT (TVNAME, 'GRCHANS', OOAINT, DIM, GRCS, CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 980
            BXONLY = .FALSE.
            DIM(1) = 1
            CALL OPUT (TVNAME, 'BOXONLY', OOALOG, DIM, BXONLY, CDUMMY,
     *         IRET)
            END IF
         END IF
      IF (IRET.LE.0) GO TO 100
C                                       close downs
 980  CALL TVDCLO (TVNAME, JERR)
 985  CALL TVDDES (TVNAME, JERR)
 990  IF (TTY(2).GT.0) CALL ZCLOSE (TTY(1), TTY(2), JERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR',I6,' OPENING THE TERMINAL')
 1100 FORMAT ('FACET',I5)
 1105 FORMAT ('Enter desired facet number from 1 -',I5)
 1111 FORMAT ('Enter max # boxes in any one autobox [',I4,']')
 1112 FORMAT ('Enter min brightness to be in island [',F6.2,'] sigma')
 1113 FORMAT ('Enter min peak brightness in new box [',F6.2,'] sigma')
 1114 FORMAT ('Enter fraction of peak unboxed as limit [',F6.2,'] ')
 1115 FORMAT ('Enter number pixels to expand new boxes: no default')
 1116 FORMAT ('Enter number pixels to skip at image edge [',I4,']')
 1120 FORMAT ('Using: ',F4.0,2F6.2,2F4.0)
      END
      SUBROUTINE IMABOX (NAME, IRET)
C-----------------------------------------------------------------------
C   IMABOX does autoboxing on the image object
C   Inputs:
C      NAME   C*(*)    Image object
C   Outputs:
C      IRET   I        Error
C   Class parameters In:
C      AUTOBOX    R(6)      Autobox parameters
C   Class parameters In/Out:
C      NBOXES     I         Number Clean boxes used
C      WINDOW     I(4,*)    Clean boxes
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   IRET
C
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   NBOX, WINS(4,MXNBOX), NX, NY, TYPE, DIM(7), NAXIS(7),
     *   BLC(7), TRC(7), NEED, PNBOX, I, J, CNO
      LONGINT   PIMAGE
      REAL      AUTOBX(6), IMAGE(2)
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       open object
      CALL OOPEN (NAME, 'READ', IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'OPEN'
         GO TO 990
         END IF
      CALL FNAGET (NAME, 'CNO', TYPE, DIM, CNO, CDUMMY, IRET)
C                                       Input subimage dimension
      CALL ARRWIN (NAME, BLC, TRC, NAXIS, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) 'WINDOW'
         GO TO 990
         END IF
      NX = TRC(1) - BLC(1) + 1
      NY = TRC(2) - BLC(2) + 1
      CALL OGET (NAME, 'NBOXES', TYPE, DIM, NBOX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      CALL OGET (NAME, 'WINDOW', TYPE, DIM, WINS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      NEED = 4 * (MXNBOX - NBOX)
      CALL FILL (NEED, 0, WINS(1,NBOX+1))
      CALL OGET (NAME, 'AUTOBOX', TYPE, DIM, AUTOBX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       allocate memory
      NEED = (NX * NY - 1) / 1024 + 2
      CALL ZMEMRY ('GET ', 'IMABOX', NEED, IMAGE, PIMAGE, IRET)
      IF (IRET.NE.0) THEN
         MSGTXT = 'DYNAMIC MEMORY INADEQUATE: GET A BIGGER COMPUTER'
         GO TO 990
         END IF
      PNBOX = NBOX
      CALL ABOXIT (NAME, NX, NY, AUTOBX, IMAGE(1+PIMAGE), NBOX, WINS,
     *   IRET)
      IF (IRET.NE.0) GO TO 980
      CALL OCLOSE (NAME, IRET)
C                                       report
      DO 20 I = PNBOX+1,NBOX
         WRITE (MSGTXT,1010) I, (WINS(J,I), J = 1,4)
         CALL MSGWRT (3)
 20      CONTINUE
      DIM(1) = 1
      DIM(2) = 1
      TYPE = OOAINT
      CALL OPUT (NAME, 'NBOXES', TYPE, DIM, NBOX, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
      DIM(1) = 4
      DIM(2) = MXNBOX
      CALL OPUT (NAME, 'WINDOW', TYPE, DIM, WINS, CDUMMY, IRET)
      IF (IRET.NE.0) GO TO 999
C
 980  CALL ZMEMRY ('FREE', 'IMABOX', NEED, IMAGE, PIMAGE, I)
      GO TO 995
 990  CALL MSGWRT (8)
 995  IF (IRET.NE.0) THEN
         MSGTXT = 'ERROR IN ' // NAME
         CALL MSGWRT (8)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IMABOX: ERROR AT ',A)
 1010 FORMAT ('Added box',I5,' : ',4I6)
      END
      SUBROUTINE ABOXIT (NAME, NX, NY, AUTOBX, IMAGE, NBOX, WINS, IRET)
C-----------------------------------------------------------------------
C   ABOXIT does the heavy lifting finding the boxes
C   Inputs:
C      NAME     C*(*)   Image object name already open
C      NX       I       X dimension
C      NY       I       Y dimension
C      AUTOBX   R(6)    autobox parameters
C   In/out:
C      IMAGE    R(*,*)  memory for image
C      NBOX     I       number of boxes
C      WINS     I(4,*)  boxes
C   Output:
C      IRET     I       error code
C-----------------------------------------------------------------------
      CHARACTER NAME*(*)
      INTEGER   NX, NY, NBOX, WINS(4,*), IRET
      REAL      AUTOBX(6), IMAGE(NX,NY)
C
      INTEGER   MAXISL
      PARAMETER (MAXISL = 5000)
      INTEGER   IX, IY, DIM(7), NPASS, NPK, PKWIN(4,MAXISL), NISLND,
     *   IPK, II, IROUND, JJ, EDGSKP, I, IBOX, SN
      REAL      ACTN, T, X, Y, R, TM, CX, CY, FMAX(MAXISL)
      DOUBLE PRECISION SS, SQ, TT, RM, RS, RSP, RSM
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IBOX = NBOX
      II = 4 * MAXISL
      CALL FILL (II, 0, PKWIN)
C                                       read the image
      DO 10 IY = 1,NY
         CALL ARREAD (NAME, DIM, IMAGE(1,IY), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1000) 'READ', IY
            GO TO 990
            END IF
 10      CONTINUE
      CALL ARRCLO (NAME, IRET)
C                                       blank outside ellipse
      IF (AUTOBX(6).GT.0.0) THEN
         EDGSKP = AUTOBX(6) + 0.5
         R = (NX - 2*EDGSKP) * (NY - 2*EDGSKP) / 2.0
         R = R * R
         DO 30 IY = 1,NY
            Y = (IY - NY/2.0) * NX
            Y = Y * Y
            DO 20 IX = 1,NX
               X = (IX - NX/2.0) * NY
               X = X * X
               IF (X+Y.GT.R) IMAGE(IX,IY) = FBLANK
 20            CONTINUE
 30         CONTINUE
         END IF
C                                       blank existing boxes
      DO 60 I = 1,NBOX
C                                       rectangles
         IF (WINS(1,I).GT.0) THEN
            DO 40 IY = WINS(2,I),WINS(4,I)
               DO 35 IX = WINS(1,I),WINS(3,I)
                  IMAGE(IX,IY) = FBLANK
 35               CONTINUE
 40            CONTINUE
C                                       circle
         ELSE
            DO 50 IY = WINS(4,I)-WINS(2,I),WINS(4,I)+WINS(2,I)
               DO 45 IX = WINS(3,I)-WINS(2,I),WINS(3,I)+WINS(2,I)
                  R = (IX - WINS(3,I)) ** 2 + (IY - WINS(4,I)) ** 2
                  R = SQRT (R)
                  IF (R.LE.WINS(2,I)) IMAGE(IX,IY) = FBLANK
 45               CONTINUE
 50            CONTINUE
            END IF
 60      CONTINUE
C                                       find rms
      RSP = 1.D10
      RSM = -1.D10
      DO 80 NPASS = 1,10
         SS = 0.0D0
         SQ = 0.0D0
         SN = 0
         TM = 0.0
         DO 70 IY = 1,NY
            DO 65 IX = 1,NX
               T = IMAGE(IX,IY)
               IF (T.NE.FBLANK) THEN
                  TM = MAX (ABS(T), TM)
                  TT = T
                  IF ((TT.LT.RSP) .AND. (TT.GT.RSM)) THEN
                     SS = SS + TT
                     SQ = SQ + TT * TT
                     SN = SN + 1.0
                     END IF
                  END IF
 65            CONTINUE
 70         CONTINUE
         IF (SN.LE.0) THEN
            RSP = RSP + 3.0D0 * RS
            RSM = RSP - 3.0D0 * RS
         ELSE
            RM = SS / SN
            SQ = SQ / SN
            RS = SQ - RM * RM
            RS = SQRT (MAX (0.0D0, RS))
            RS = MAX (RS, 0.01D0*RM)
            RSP = RM + 3.0D0 * RS
            RSM = RM - 4.0D0 * RS
            END IF
 80      CONTINUE
      ACTN = RS
      IF (TM.GE.ACTN*AUTOBX(3)) THEN
         WRITE (MSGTXT,1080) ACTN
         CALL MSGWRT (3)
      ELSE
         WRITE (MSGTXT,1081) TM, AUTOBX(3), ACTN
         CALL MSGWRT (5)
         GO TO 999
         END IF
C                                       find islands a la SAD
      ACTN = ACTN * AUTOBX(2)
      NISLND = MAXISL
      II = 0
 85   CALL ISLAND (NISLND, NX, NY, IMAGE, ACTN, PKWIN, NPK)
      IF (NPK.GT.NISLND-10) THEN
         II = II + 1
         ACTN = ACTN * AUTOBX(3) / AUTOBX(2)
         IF (II.LT.4) GO TO 85
         END IF
C                                       find maxima
      ACTN = ACTN * AUTOBX(3) / AUTOBX(2)
      TM = TM * AUTOBX(4)
      TM = MAX (TM, ACTN)
      DO 120 IPK = 1,NPK
         RM = 0.0
         IF ((PKWIN(1,IPK).LE.0) .OR. (PKWIN(2,IPK).LE.0) .OR.
     *      (PKWIN(3,IPK).LT.PKWIN(1,IPK)) .OR. (PKWIN(3,IPK).GT.NX)
     *      .OR. (PKWIN(3,IPK).LT.PKWIN(1,IPK)) .OR.
     *      (PKWIN(3,IPK).GT.NY)) GO TO 120
         DO 110 IY = PKWIN(2,IPK),PKWIN(4,IPK)
            DO 105 IX = PKWIN(1,IPK),PKWIN(3,IPK)
               IF ((IMAGE(IX,IY).NE.FBLANK) .AND.
     *            (ABS(IMAGE(IX,IY)).GT.RM)) THEN
                  RM = ABS (IMAGE(IX,IY))
                  CX = IX
                  CY = IY
                  END IF
 105           CONTINUE
 110        CONTINUE
C                                       peak > peak cutoff
         IF (RM.GT.TM) THEN
C                                       is it in a box already?
            DO 115 I = 1,IBOX
               IF (WINS(1,I).EQ.-1) THEN
                  RS = SQRT ((CX-WINS(3,I))**2 + (CY-WINS(4,I))**2)
                  IF (RS.LE.WINS(2,I)) GO TO 120
               ELSE
                  IF ((CX.GE.WINS(1,I)) .AND. (CX.LE.WINS(3,I)) .AND.
     *               (CY.GE.WINS(2,I)) .AND. (CY.LE.WINS(4,I)))
     *               GO TO 120
                  END IF
 115           CONTINUE
C                                       count it
            II = II + 1
            CALL COPY (4, PKWIN(1,IPK), PKWIN(1,II))
            FMAX(II) = RM
            END IF
 120     CONTINUE
C                                       make boxes
      NPK = II
      JJ = IROUND (AUTOBX(5))
      DO 140 IPK = 1,NPK
C                                       find strongest
         RM = 0
         DO 130 I = 1,NPK
            IF (FMAX(I).GT.RM) THEN
               II = I
               RM = FMAX(I)
               END IF
 130        CONTINUE
         IX = PKWIN(3,II) - PKWIN(1,II) + 1
         IY = PKWIN(4,II) - PKWIN(2,II) + 1
         CX = (PKWIN(3,II) + PKWIN(1,II)) / 2.0
         CY = (PKWIN(4,II) + PKWIN(2,II)) / 2.0
C                                       drop single points unless strong
C                                       parameter MINWIDTH=2
         IF (((IX.LT.2) .OR. (IY.LT.2)) .AND. (RM.LT.2.5*ACTN))
     *      GO TO 135
C                                       circle
         IF ((ABS(IX-IY).LE.1) .AND. (MIN(IX,IY).LE.8)) THEN
             IX = (MAX (IX,IY) + 1) / 2
             NBOX = NBOX + 1
             WINS(1,NBOX) = -1
             WINS(2,NBOX) = IX + JJ
             WINS(3,NBOX) = IROUND (CX)
             WINS(4,NBOX) = IROUND (CY)
C                                       rectangle
         ELSE
            NBOX = NBOX + 1
            WINS(1,NBOX) = MAX (1, PKWIN(1,II) - JJ)
            WINS(2,NBOX) = MAX (1, PKWIN(2,II) - JJ)
            WINS(3,NBOX) = MIN (NX, PKWIN(3,II) + JJ)
            WINS(4,NBOX) = MIN (NY, PKWIN(4,II) + JJ)
            END IF
C                                       bail if reached upper limit
         IX = NBOX - IBOX
         IF (IX-IROUND(AUTOBX(1)).GE.0) GO TO 999
 135     FMAX(II) = 0
 140     CONTINUE
      IF (NBOX.EQ.IBOX) THEN
         MSGTXT = 'No islands found both strong and large enough'
         CALL MSGWRT (5)
         END IF
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ABOXIT: ERROR DOING ',A,' AT ROW',I7)
 1080 FORMAT ('Using rms',F10.6)
 1081 FORMAT ('Peak',F12.7,' less than',F6.2,' times rms',F11.7)
      END
      SUBROUTINE FILITB (NMAPS, NGAUSS, IN, IMSIZE, WIN, IRET)
C-----------------------------------------------------------------------
C   FILITB replaces BOXFIX in that fetching the parameters is changed.
C   Calls BOXCHK and then deletes windows
C   Inputs:
C      NMAPS    I          Total number factes
C      NGAUSS   I          Separated into Gaussian widths groups
C      IN       C(*)*(*)   Names of image objects
C   In/Out:
C      IMSIZE   I(2,*)     Image dimensions - found then used
C   Outputs:
C      WIN      I(4,*)     Work space for all facets boxes
C      IRET     I          Error code
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NMAPS, NGAUSS, IMSIZE(2,MAXFLD), WIN(4,NMAPS,*), IRET
      CHARACTER IN(*)*(*)
C
      INCLUDE 'INCS:PCLN.INC'
      INTEGER   I, J, K, L, NBOXES(MAXFLD), IBOXES(MAXFLD), NOV, LOV,
     *   BOXES(4,MXNBOX), BLC(7), TRC(7), NAXIS(7), MSGSAV, FOV(5,2000),
     *   IB, JB, MBOX, TYPE, DIM(7)
      REAL      XI, XJ, CXI, CXJ, CYI, CYJ
      LOGICAL   CHANGD
      CHARACTER CDUMMY*1
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PAOOF.INC'
C-----------------------------------------------------------------------
      MBOX = MXNBFL / NMAPS
      MBOX = MIN (MBOX, MXNBOX)
      MSGSAV = MSGSUP
      CHANGD = .FALSE.
      LOV = 2000
C                                       get info from objects
      DO 20 L = 1,NMAPS
         CALL OGET (IN(L), 'NBOXES', TYPE, DIM, NBOXES(L), CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         CALL OGET (IN(L), 'WINDOW', TYPE, DIM, BOXES, CDUMMY, IRET)
         IF (IRET.NE.0) GO TO 999
         IBOXES(L) = NBOXES(L)
         IF (NBOXES(L).GT.MBOX) THEN
            WRITE (MSGTXT,1000) L, NBOXES(L), MBOX
            CALL MSGWRT (7)
            NBOXES(L) = MBOX
            END IF
C                                       put boxes in big array
         DO 10 I = 1,NBOXES(L)
            WIN(1,L,I) = BOXES(1,I)
            WIN(2,L,I) = BOXES(2,I)
            WIN(3,L,I) = BOXES(3,I)
            WIN(4,L,I) = BOXES(4,I)
 10         CONTINUE
C                                       get imsize if not known
         IF ((IMSIZE(1,L).LE.0) .OR. (IMSIZE(2,L).LE.0)) THEN
            CALL ARRWIN (IN(L), BLC, TRC, NAXIS, IRET)
            IF (IRET.NE.0) GO TO 999
            IMSIZE(1,L) = NAXIS(1)
            IMSIZE(2,L) = NAXIS(2)
            END IF
 20      CONTINUE
C                                       now know enough
 30   MSGSUP = 0
      MSGSUP = 1
      IF (.NOT.CHANGD) MSGSUP = 0
      NOV = LOV
      CALL BOXCHK (NMAPS, NGAUSS, IN, IMSIZE, NBOXES, WIN, NOV, FOV,
     *   IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
C                                       do fixing
      IF (NOV.GT.0) THEN
         DO 40 K = 1,NOV
            IB = FOV(1,K)
            I  = FOV(2,K)
            JB = FOV(3,K)
            J  = FOV(4,K)
            IF ((WIN(1,I,IB).GT.-99) .AND. (WIN(1,J,JB).GT.-99) .AND.
     *         (FOV(5,K).GT.1)) THEN
               IF (WIN(1,I,IB).LT.0) THEN
                  XI = PI * WIN(2,I,IB) * WIN(2,I,IB)
                  CXI = WIN(3,I,IB)
                  CYI = WIN(4,I,IB)
               ELSE
                  XI = (WIN(3,I,IB) - WIN(1,I,IB) + 1.) *
     *               (WIN(4,I,IB) - WIN(2,I,IB) + 1.)
                  CXI = (WIN(1,I,IB) + WIN(3,I,IB)) / 2.0
                  CYI = (WIN(2,I,IB) + WIN(4,I,IB)) / 2.0
                  END IF
               IF (WIN(1,J,JB).LT.0) THEN
                  XJ = PI * WIN(2,J,JB) * WIN(2,J,JB)
                  CXJ = WIN(3,J,JB)
                  CYJ = WIN(4,J,JB)
               ELSE
                  XJ = (WIN(3,J,JB) - WIN(1,J,JB) + 1.) *
     *               (WIN(4,J,JB) - WIN(2,J,JB) + 1.)
                  CXJ = (WIN(1,J,JB) + WIN(3,J,JB)) / 2.0
                  CYJ = (WIN(2,J,JB) + WIN(4,J,JB)) / 2.0
                  END IF
               IF (XJ.GT.1.1*XI) THEN
                  WIN(1,I,IB) = -999
               ELSE IF (XI.GT.1.1*XJ) THEN
                  WIN(1,J,JB) = -999
               ELSE
                  XI = (IMSIZE(1,I)/2.0-CXI)**2 +
     *               (IMSIZE(2,I)/2.0-CYI)**2
                  XJ = (IMSIZE(1,J)/2.0-CXJ)**2 +
     *               (IMSIZE(2,J)/2.0-CYJ)**2
                  IF (XJ.LT.0.90*XI) THEN
                     WIN(1,I,IB) = -999
                  ELSE IF (XI.LT.0.9*XJ) THEN
                     WIN(1,J,JB) = -999
                  ELSE IF (I.GT.J) THEN
                     WIN(1,I,IB) = -999
                  ELSE
                     WIN(1,J,JB) = -999
                     END IF
                  END IF
               END IF
 40         CONTINUE
         DO 60 I = 1,NMAPS
            JB = 0
            DO 50 IB = 1,NBOXES(I)
               IF (WIN(1,I,IB).GT.-99) THEN
                  JB = JB + 1
                  IF (JB.LT.IB) THEN
                     WIN(1,I,JB) = WIN(1,I,IB)
                     WIN(2,I,JB) = WIN(2,I,IB)
                     WIN(3,I,JB) = WIN(3,I,IB)
                     WIN(4,I,JB) = WIN(4,I,IB)
                     END IF
                  END IF
 50            CONTINUE
            WRITE (MSGTXT,1050) I, NBOXES(I), JB
            IF (NBOXES(I).NE.JB) THEN
               CHANGD = .TRUE.
               CALL MSGWRT (4)
               END IF
            NBOXES(I) = JB
 60         CONTINUE
         IF (NOV.EQ.LOV) GO TO 30
         END IF
C                                       are any left?
      MSGSUP = 1
      NOV = LOV
      CALL BOXCHK (NMAPS, NGAUSS, IN, IMSIZE, NBOXES, WIN, NOV, FOV,
     *   IRET)
      MSGSUP = MSGSAV
      IF (IRET.NE.0) GO TO 999
      DO 70 K = 1,NOV
         IB = FOV(1,K)
         I  = FOV(2,K)
         JB = FOV(3,K)
         J  = FOV(4,K)
         IF ((WIN(1,I,IB).GT.-99) .AND. (WIN(1,J,JB).GT.-99) .AND.
     *      (FOV(5,K).GE.1)) THEN
            WRITE (MSGTXT,1060) IB, I, JB, J, FOV(5,K)
            IF ((I.NE.J) .OR. (FOV(5,K).GT.1)) CALL MSGWRT (4)
            END IF
 70      CONTINUE
C                                       put info back in objects
      DO 90 L = 1,NMAPS
         IF (NBOXES(L).NE.IBOXES(L)) THEN
            DIM(1) = 1
            DIM(2) = 1
            CALL OPUT (IN(L), 'NBOXES', OOAINT, DIM, NBOXES(L), CDUMMY,
     *         IRET)
            IF (IRET.NE.0) GO TO 999
C                                       get boxes in big array
            DO 80 I = 1,NBOXES(L)
               BOXES(1,I) = WIN(1,L,I)
               BOXES(2,I) = WIN(2,L,I)
               BOXES(3,I) = WIN(3,L,I)
               BOXES(4,I) = WIN(4,L,I)
 80            CONTINUE
            DIM(1) = 4
            DIM(2) = MXNBOX
            CALL OPUT (IN(L), 'WINDOW', TYPE, DIM, BOXES, CDUMMY, IRET)
            IF (IRET.NE.0) GO TO 999
            END IF
 90      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIELD',I5,' NUMBER BOXES',I6,' EXCEEDS MAX ALLOWED',I5)
 1050 FORMAT ('BOXFIX: Field',I5,' number boxes reduced from',I5,' to',
     *   I5)
 1060 FORMAT ('Box',I5,' facet',I5,' overlaps box',I5,' facet',I5,' at',
     *   I2,' points')
      END
