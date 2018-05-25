      SUBROUTINE AU5C (BRANCH)
C-----------------------------------------------------------------------
C! verbs to draw wedges on TV, erase images, set corners with TV cursor
C# POPS-appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-2002, 2004-2005, 2011, 2013
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
C   AU5C performs additional TV image formation functions:
C   BRANCH = 1   TVWEDGE   load wedge like load of image (clip mn:mx)
C          = 2   IMWEDGE   load wedge like load of an image (map mn:mx)
C          = 3   WEDERASE  fill a wedge area with zeros
C          = 4   IMERASE   fill an image area with zeros
C          = 5   TVWIN     set BLC, TRC with graphics display
C          = 6   TVBOX     set NBOXES boxes with graphics display
C          = 7   TVSLICE   set BLC, TRC with diag. graphics line
C          = 8   REBOX     start w NBOXES boxes, reset w graphics
C          = 9   FILEBOX   REBOX to the TV.
C          = 10  DRAWBOX   Draw the boxes and quit
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      INCLUDE 'INCS:PCLN.INC'
      CHARACTER LINTYP*2, PRGNAM*6, CDUM*1, BOXFIL*48, TXLINE*132,
     *   ATIME*8, ADATE*12, SCNAME*256, CLASS*6, ULOG*24, UNAME*48,
     *   TMLINE*132
      LOGICAL   ASKPOS, UNIQUE, SAVEBU
      INTEGER   NROW, NPIX, J, POTERR, IERR, IPLAN, IPL, I, IX0, IY0,
     *   IROUND, IBUT, IBUF(1280), NBO, IGR, IDUM, INBO, WETYPE, WESIDE,
     *   IFIELD, TXLUN, TXIND, TXLUN2, TXIND2, IBX(4), ITIME(3),
     *   IDATE(3), MSGSAV, KBP, ITRIM, IOFF, COLP, MBOX
      REAL      RPOS(2), BB(7,MXNBOX), TT(7,MXNBOX), BXX(100), BYY(100),
     *   RDUM, RIBUF(4096), BLCO(7), TRCO(7), BOX(4,MXCLBX)
      DOUBLE PRECISION X
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      COMMON /AIPSCR/ RIBUF
      EQUIVALENCE (BXX, BB),  (BYY, TT)
      EQUIVALENCE (IBUF, RIBUF)
      DATA PRGNAM /'AU5C  '/
C-----------------------------------------------------------------------
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.10)) GO TO 999
C                                        Wedges have possible immed arg
      IF (BRANCH.GT.2) GO TO 20
         NROW = 16
         ASKPOS = .FALSE.
         IF ((SP.LT.1) .OR. (STACK(SP).EQ.2)) GO TO 20
            I = IROUND (V(SP))
            J = ABS(I)
            IF ((J.LT.1) .OR. (J.GT.MAXXTV(2)/4)) GO TO 20
               NROW = J
               ASKPOS = I.LT.0
               SP = SP - 1
C                                        Open the TV
 20   POTERR = 101
      CALL TVOPEN (IBUF, IERR)
      IF (IERR.NE.0) GO TO 980
C                                       Get a plane and image
 40   LINTYP = 'MA'
      IF (BRANCH.EQ.3) LINTYP = 'WE'
      CALL TVFIND (NGRAY, LINTYP, IPLAN, UNIQUE, CATBLK, IBUF, IERR)
      IF (IERR.NE.0) GO TO 975
C                                        Branch to OP
 50   GO TO (100, 100, 400, 400, 500, 600, 700, 800, 900, 800), BRANCH
C-----------------------------------------------------------------------
C                                        TVWEDGE
C                                        wedge of image clip mn:mx
C                                        IMWEDGE
C                                        wedge of image intensities
C-----------------------------------------------------------------------
C                                        default position
 100  IF (.NOT.ASKPOS) THEN
C                                       sole image: leave room labels
         IF (UNIQUE) THEN
            IY0 = CATBLK(IICOR+1) - 6.833*CSIZTV(2) - 4.5 - NROW
            IF (IY0-2.7*CSIZTV(2).GT.0.5) GO TO 120
            IY0 = CATBLK(IICOR+3) + 5.2*CSIZTV(2) + 1.5
            IF (IY0+NROW.LE.MAXXTV(2)) GO TO 120
            END IF
C                                       put next to, or on, img
         IY0 = CATBLK(IICOR+1) - NROW
         IF (IY0.LT.1) THEN
            IY0 = CATBLK(IICOR+3) + 1
            IF (IY0+NROW-1.GT.MAXXTV(2)) IY0 = CATBLK(IICOR+1)
            END IF
C                                        ask location
      ELSE
         WRITE (MSGTXT,1110)
         CALL MSGWRT (1)
         CALL TVWHER (IPL, RPOS, IBUT, IERR)
         IF (IERR.NE.0) GO TO 975
         IY0 = IROUND (RPOS(2))
         IF ((IY0.LT.CATBLK(IICOR+1)) .AND. (IY0.GE.CATBLK(IICOR+1)
     *      -NROW-2)) IY0 = MAX (1, CATBLK(IICOR+1)-NROW)
         IF ((IY0.GT.CATBLK(IICOR+3)) .AND. (IY0.LT.CATBLK(IICOR+3)
     *      +NROW)) IY0 = MIN (CATBLK(IICOR+3)+1, MAXXTV(2)+1-NROW)
         END IF
C                                        Fill buffer
 120  WETYPE = 3 - BRANCH
      WESIDE = 1
      IF (IY0.GE.CATBLK(IICOR+3)) WESIDE = 3
      NPIX = CATBLK(IICOR+2) - CATBLK(IICOR) + 1
      IF (NPIX.LE.1) NPIX = 2
      IX0 = CATBLK(IICOR)
      RPOS(1) = 0.0
      RPOS(2) = 0.0
      CALL IWEDGE (WETYPE, WESIDE, IPLAN, IX0, IY0, NPIX, NROW, RPOS,
     *   RIBUF, IBUF, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 975
C-----------------------------------------------------------------------
C                                        IMERASE
C                                        zero part of plane
C-----------------------------------------------------------------------
 400  WETYPE = 0
      WESIDE = 1
      IY0 = CATBLK(IICOR+1)
      NROW = CATBLK(IICOR+3) - IY0 + 1
      IX0 = CATBLK(IICOR)
      NPIX = CATBLK(IICOR+2) - IX0 + 1
      RPOS(1) = 0.0
      RPOS(2) = 0.0
      CALL IWEDGE (WETYPE, WESIDE, IPLAN, IX0, IY0, NPIX, NROW, RPOS,
     *   RIBUF, IBUF, IERR)
      IF (IERR.EQ.0) POTERR = 0
      GO TO 975
C-----------------------------------------------------------------------
C                                       TVWIN
C                                       set BLC, TRC with graphics
C-----------------------------------------------------------------------
 500  POTERR = 31
      IGR = MIN (3, NGRAPH)
      IF (IGR.LE.0) GO TO 975
      IPL = IGR + NGRAY
      CALL YCINIT (IPL, IBUF)
      CALL ADVERB ('BLC', 'R', 7, 0, IDUM, BLCO, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('BLC', 'R', 7, 0, IDUM, TRCO, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      NBO = 0
      MBOX = 1
      CALL GRBOXS (IGR, MBOX, NBO, BLCO, TRCO, IBUF, IERR)
      POTERR = 49
      IF (IERR.EQ.0) THEN
         POTERR = 0
         WRITE (MSGTXT,1500) BLCO
         CALL MSGWRT (2)
         WRITE (MSGTXT,1501) TRCO
         CALL MSGWRT (2)
         CALL ADVRBS ('BLC', 'R', 7, 0, IDUM, BLCO, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         CALL ADVRBS ('TRC', 'R', 7, 0, IDUM, TRCO, CDUM)
         END IF
      GO TO 975
C-----------------------------------------------------------------------
C                                       TVBOX
C                                       set NBOXES boxes with graphics
C-----------------------------------------------------------------------
 600  POTERR = 31
      IGR = MIN (3, NGRAPH)
      IF (IGR.LE.0) GO TO 975
      IPL = IGR + NGRAY
      CALL YCINIT (IPL, IBUF)
      CALL ADVERB ('NBOXES',  'I', 1, 0, INBO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      INBO = MAX (0, MIN (MXCLBX, INBO))
      NBO = 0
      MBOX = MXCLBX
      CALL GRBOXS (IGR, MBOX, NBO, BB, TT, IBUF, IERR)
      POTERR = 49
      IF (IERR.NE.0) GO TO 975
      POTERR = 0
      IF (NBO.NE.INBO) THEN
         WRITE (MSGTXT,1600) INBO, NBO
         CALL MSGWRT (6)
         CALL ADVRBS ('NBOXES',  'I', 1, 0, NBO, RDUM, CDUM)
         END IF
      CALL RFILL (4*MXCLBX, 0.0, BOX)
      DO 610 I = 1,NBO
         BOX(1,I) = BB(1,I)
         BOX(2,I) = BB(2,I)
         BOX(3,I) = TT(1,I)
         BOX(4,I) = TT(2,I)
         WRITE (MSGTXT,1601) I, (BOX(J,I), J = 1,4)
         CALL MSGWRT (2)
 610     CONTINUE
      CALL ADVRBS ('CLBOX', 'R', 200, 0, IDUM, BOX, CDUM)
      GO TO 975
C-----------------------------------------------------------------------
C                                       TVSLICE
C                                       set BLC, TRC with graphics
C-----------------------------------------------------------------------
 700  POTERR = 31
      IGR = MIN (3, NGRAPH)
      IF (IGR.LE.0) GO TO 975
      IPL = IGR + NGRAY
      CALL YCINIT (IPL, IBUF)
      CALL ADVERB ('BLC', 'R', 7, 0, IDUM, BLCO, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL ADVERB ('BLC', 'R', 7, 0, IDUM, TRCO, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      CALL GRSLIC (IGR, BLCO, TRCO, IBUF, IERR)
      POTERR = 49
      IF (IERR.EQ.0) THEN
         POTERR = 0
         WRITE (MSGTXT,1500) BLCO
         CALL MSGWRT (2)
         WRITE (MSGTXT,1501) TRCO
         CALL MSGWRT (2)
         CALL ADVRBS ('BLC', 'R', 7, 0, IDUM, BLCO, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         CALL ADVRBS ('TRC', 'R', 7, 0, IDUM, TRCO, CDUM)
         END IF
      GO TO 975
C-----------------------------------------------------------------------
C                                       REBOX
C                                       reset NBOXES boxes with graphics
C-----------------------------------------------------------------------
 800  POTERR = 31
      IGR = 0
      IF (BRANCH.EQ.10) THEN
         CALL ADVERB ('BOXFILE', 'C', 1, 48, IDUM, RDUM, BOXFIL)
         IF (ERRNUM.NE.0) GO TO 975
         IF (BOXFIL.NE.' ') GO TO 900
         CALL ADVERB ('GRCHAN',  'I', 1, 0, IGR, RDUM, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         IPL = IGR / 10
         IGR = IGR - 10*IPL
         END IF
      IF (IGR.EQ.0) IGR = 3
      IGR = MIN (IGR, NGRAPH)
      IF (IGR.LE.0) GO TO 975
      IPL = NGRAY + IGR
      CALL YCINIT (IPL, IBUF)
      CALL ADVERB ('NBOXES',  'I', 1, 0, INBO, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      NBO = INBO
C                                       pick up old Boxes & convert
      IF (NBO.GT.0) THEN
         CALL ADVERB ('CLBOX', 'R', 200, 0, IDUM, BOX, CDUM)
         IF (ERRNUM.NE.0) GO TO 975
         DO 810 I = 1,NBO
            BB(1,I) = BOX(1,I)
            BB(2,I) = BOX(2,I)
            TT(1,I) = BOX(3,I)
            TT(2,I) = BOX(4,I)
 810        CONTINUE
         END IF
      MBOX = MXCLBX
      IF (BRANCH.EQ.10) MBOX = -MBOX
      CALL GRBOXS (IGR, MBOX, NBO, BB, TT, IBUF, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 49
         GO TO 975
         END IF
      IF (BRANCH.EQ.8) THEN
         IF (NBO.NE.INBO) THEN
            WRITE (MSGTXT,1600) INBO, I
            CALL MSGWRT (6)
            CALL ADVRBS ('NBOXES',  'I', 1, 0, NBO, RDUM, CDUM)
            END IF
         DO 820 I = 1,NBO
            BOX(1,I) = BB(1,I)
            BOX(2,I) = BB(2,I)
            BOX(3,I) = TT(1,I)
            BOX(4,I) = TT(2,I)
            WRITE (MSGTXT,1601) I, (BOX(J,I), J = 1,4)
            CALL MSGWRT (2)
 820        CONTINUE
         CALL ADVRBS ('CLBOX', 'R', 200, 0, IDUM, BOX, CDUM)
         END IF
      POTERR = 0
      GO TO 975
C-----------------------------------------------------------------------
C                                       FILEBOX
C                                       reset NBOXES boxes with graphics
C                                       write to text file
C-----------------------------------------------------------------------
C                                       open file in FILEBOX
 900  CALL ADVERB ('NFIELD', 'I', 1, 0, IFIELD, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IF ((IFIELD.LT.1) .OR. (IFIELD.GT.MAXFLD)) THEN
         CALL H2CHR (6, KHIMCO, CATH(KHIMC), CLASS)
         J = 3
         DO 905 I = 6,3,-1
            IF ((CLASS(I:I).LT.'0') .OR. (CLASS(I:I).GT.'9'))
     *         J = MAX (J, I+1)
 905        CONTINUE
         IF (J.EQ.3) THEN
            READ (CLASS(3:6),1900) IFIELD
         ELSE IF (J.EQ.4) THEN
            READ (CLASS(4:6),1901) IFIELD
         ELSE IF (CLASS(5:6).EQ.'  ') THEN
            IFIELD = 1
         ELSE IF ((CLASS(4:4).GE.'0') .AND. (CLASS(4:4).LE.'9') .AND.
     *      (J.GT.4)) THEN
            IFIELD = 0
         ELSE
            CALL ZREHEX (2, CLASS(5:6), IFIELD)
            IFIELD = IFIELD + 1
            END IF
         IF ((IFIELD.GE.1) .AND. (IFIELD.LE.MAXFLD)) THEN
            WRITE (MSGTXT,1902) CLASS, IFIELD
         ELSE
            IFIELD = 1
            MSGTXT = 'Class no help: Field 1 assumed'
            END IF
         CALL MSGWRT (2)
         END IF
      CALL ADVERB ('BOXFILE', 'C', 1, 48, IDUM, RDUM, BOXFIL)
      IF (ERRNUM.NE.0) GO TO 975
      TXLUN = 3
      TXLUN2 = 11
      NBO = 0
      IOFF = 0
      SAVEBU = .FALSE.
      SCNAME = ' '
C                                       Read in pre-existing
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL ZTXOPN ('QRED', TXLUN, TXIND, BOXFIL, .TRUE., IERR)
      MSGSUP = MSGSAV
      IF (IERR.EQ.0) THEN
C                                       do a backup file
         COLP = INDEX (BOXFIL, ':')
         IF (COLP.LE.1) THEN
            MSGTXT = 'WARNING: NO LOGICAL: BACKUP FILE IN $FITS'
            CALL MSGWRT (6)
            CALL ZFULLN (' ', 'FITS', 'FILEBOX', SCNAME, IERR)
         ELSE
            ULOG = BOXFIL(:COLP-1)
            UNAME = BOXFIL(COLP+1:)
            CALL ZFULLN (' ', ULOG, UNAME, SCNAME, IERR)
            END IF
         POTERR = 55
         IF (IERR.NE.0) GO TO 970
         CALL ZTXOPN ('QWRT', TXLUN2, TXIND2, SCNAME, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 970
         DO 910 I = 1,50000
            CALL ZTXIO ('READ', TXLUN, TXIND, TXLINE, IERR)
            IF (IERR.EQ.2) GO TO 915
            IF (IERR.NE.0) GO TO 965
            J = ITRIM (TXLINE)
            J = MAX (1, J)
            CALL ZTXIO ('WRIT', TXLUN2, TXIND2, TXLINE(:J), IERR)
            IF (IERR.NE.0) GO TO 965
 910        CONTINUE
 915     SAVEBU = .TRUE.
C                                       close both
         CALL ZTXCLS (TXLUN, TXIND, IERR)
         CALL ZTXCLS (TXLUN2, TXIND2, IERR)
         POTERR = 55
C                                       this is less dangerous!
         CALL ZTXZAP (TXLUN, BOXFIL, IERR)
         CALL ZTXOPN ('WRIT', TXLUN, TXIND, BOXFIL, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 971
         CALL ZTXOPN ('QRED', TXLUN2, TXIND2, SCNAME, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 970
         POTERR = 50
C                                       read back backup to new orig.
         DO 930 I = 1,50000
            CALL ZTXIO ('READ', TXLUN2, TXIND2, TXLINE, IERR)
            IF (IERR.EQ.2) GO TO 935
            IF (IERR.NE.0) GO TO 965
            CALL CHTRIM (TXLINE, 132, TMLINE, J)
            KBP = 1
            IF (TMLINE(:1).EQ.' ') GO TO 920
            IF (TMLINE(:1).LT.'0') GO TO 920
            IF (TMLINE(:1).GT.'9') GO TO 920
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            J = IROUND (RDUM)
            IF (J.NE.IFIELD) GO TO 920
C                                       window to read in
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            IBX(1) = IROUND (RDUM)
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            IBX(2) = IROUND (RDUM)
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            IBX(3) = IROUND (RDUM)
            CALL GETNUM (TMLINE, 132, KBP, X)
            IF ((ERRNUM.NE.0) .OR. (X.EQ.DBLANK)) GO TO 965
            RDUM = X
            IBX(4) = IROUND (RDUM)
C                                       keep this
            NBO = NBO + 1
            BB(1,NBO) = IBX(1)
            BB(2,NBO) = IBX(2)
            TT(1,NBO) = IBX(3)
            TT(2,NBO) = IBX(4)
            GO TO 930
C                                       otherwise write to scratch
 920        J = ITRIM (TXLINE)
            J = MAX (1, J)
            CALL ZTXIO ('WRIT', TXLUN, TXIND, TXLINE(:J), IERR)
            IF (IERR.NE.0) GO TO 965
 930        CONTINUE
 935     CALL ZTXCLS (TXLUN2, TXIND2, IERR)
C                                       create a new BOXFILE
      ELSE
         POTERR = 55
         CALL ZTXOPN ('WRIT', TXLUN, TXIND, BOXFIL, .FALSE., IERR)
         IF (IERR.NE.0) GO TO 975
         END IF
C                                       add the time
      CALL ZTIME (ITIME)
      CALL ZDATE (IDATE)
      CALL TIMDAT (ITIME, IDATE, ATIME, ADATE)
      TXLINE = '#  FILEBOX: ' // ADATE // ATIME
      J = ITRIM (TXLINE)
      POTERR = 50
      CALL ZTXIO ('WRIT', TXLUN, TXIND, TXLINE(:J), IERR)
      IF (IERR.NE.0) GO TO 970
      POTERR = 31
      CALL ADVERB ('GRCHAN',  'I', 1, 0, IGR, RDUM, CDUM)
      IF (ERRNUM.NE.0) GO TO 975
      IPL = IGR / 10
      IGR = IGR - 10*IPL
      IF (IGR.EQ.0) IGR = 3
      IGR = MIN (IGR, NGRAPH)
      IF (IGR.LE.0) GO TO 970
      IPL = NGRAY + IGR
      CALL YCINIT (IPL, IBUF)
      INBO = NBO
      IF (NBO.GT.0) THEN
         WRITE (MSGTXT,1940) NBO
         IF (BRANCH.EQ.10) WRITE (MSGTXT,1941) NBO
         CALL MSGWRT (3)
         END IF
      MBOX = MXNBOX
      IF (BRANCH.EQ.10) MBOX = -MBOX
      CALL GRBOXS (IGR, MBOX, NBO, BB, TT, IBUF, IERR)
      IF (IERR.NE.0) THEN
         POTERR = 49
         IF (INBO.LE.0) GO TO 970
         MSGTXT = 'TRYING TO SAVE BOXES ANYWAY - YOU MAY HAVE TO EDIT'
     *      // 'THE FILE'
         CALL MSGWRT (6)
         END IF
      NBO = MIN (NBO, ABS(MBOX))
      IF (NBO.NE.INBO) THEN
         WRITE (MSGTXT,1600) INBO, NBO
         CALL MSGWRT (6)
         END IF
      POTERR = 50
      DO 950 I = 1,NBO
C                                       round
         IF (BB(1,I).LT.-0.5) THEN
            IBX(1) = IROUND (BB(1,I))
            IBX(2) = IROUND (BB(2,I))
            IBX(3) = IROUND (TT(1,I))
            IBX(4) = IROUND (TT(2,I))
         ELSE
            IBX(1) = BB(1,I) + 0.01
            IBX(2) = BB(2,I) + 0.01
            IBX(3) = TT(1,I) + 0.99
            IBX(4) = TT(2,I) + 0.99
            END IF
         WRITE (MSGTXT,1945) IFIELD, I, IBX
         CALL MSGWRT (2)
         WRITE (TXLINE,1946) IFIELD, IBX
         CALL CHTRIM (TXLINE, 132, TXLINE, J)
         CALL ZTXIO ('WRIT', TXLUN, TXIND, TXLINE(:J), IERR)
         IF (IERR.NE.0) GO TO 970
 950     CONTINUE
      SAVEBU = .FALSE.
      POTERR = 0
      GO TO 970
C-----------------------------------------------------------------------
C                                        Close downs
 965  CALL ZTXCLS (TXLUN2, TXIND2, IERR)
 970  CALL ZTXCLS (TXLUN, TXIND, IERR)
 971  IF (SCNAME.NE.' ') THEN
         IF (SAVEBU) THEN
            MSGTXT = 'BOXFILE backup saved in file:'
            CALL MSGWRT (8)
            MSGTXT = SCNAME
            CALL MSGWRT (8)
         ELSE
            CALL ZTXZAP (TXLUN2, SCNAME, IERR)
            END IF
         END IF
 975  CALL TVCLOS (IBUF, IERR)
C
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.EQ.0) GO TO 999
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1110 FORMAT ('Please point out approx position for bottom edge',
     *   ' of wedge')
 1500 FORMAT ('BLC = ',7F8.2)
 1501 FORMAT ('TRC = ',7F8.2)
 1600 FORMAT ('NBOXES reset from',I4,' to',I4)
 1601 FORMAT ('BOX(',I3,') = ',4F8.2)
 1900 FORMAT (I4)
 1901 FORMAT (I3)
 1902 FORMAT ('Class ',A,' implies field number',I5)
 1940 FORMAT (I4,' boxes to be reset - some may not be visible')
 1941 FORMAT (I4,' boxes to be displayed - some may not be visible')
 1945 FORMAT ('FIELD',I5.2,' BOX(',I4.3,') = ',4I7.4)
 1946 FORMAT (I4,4I7)
      END
