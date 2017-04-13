C   IMAGE Class utility module
C-----------------------------------------------------------------------
C! Object Oriented AIPS Fortran "IMAGE" utility module.
C# Ext-util Utility Object-Oriented
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2009-2010, 2013
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
C   IMTCOP (in, out, tbtype, tbver, ierr)
C      Copy table between images. tbver=0 => copy all.
C   IM2TAB (image, table, tbtype, tbver, ierr)
C      Makes table object associated with a image object.
C   BOXCHK (mfield, ngauss, fname, imsize, nboxes, win, nov, fov, iret)
C      Make list of overlapping Clean boxes
C   IMCALT (imin, imout, ierr)
C      Copies all tables from one imdata object to another except those
C      listed in optional keyword DROPTABS (<= 20 char*2 values)
C-----------------------------------------------------------------------
      SUBROUTINE IMTCOP (IN, OUT, TBTYPE, TBVER, IERR)
C-----------------------------------------------------------------------
C   IMAGE class utility routine
C   Copy selected table(s) from one image to another.
C   Inputs:
C      IN      C*?  Name of input image object.
C      OUT     C*?  Name of output image object.
C      TBTYPE  C*2  Table type.
C      TBVER   I    Table version 0=> all.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IN*(*), OUT*(*), TBTYPE*2
      INTEGER   TBVER, IERR
C
      INTEGER   TCAT(256), MCAT(256)
      CHARACTER TAB1*32, TAB2*32
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      TAB1 = 'Temporary Table 1 for IMTCOP'
      TAB2 = 'Temporary Table 2 for IMTCOP'
      CALL IM2TAB (IN, TAB1, TBTYPE, TBVER, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IM2TAB (OUT, TAB2, TBTYPE, TBVER, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy
      CALL TBLCOP (TAB1, TAB2, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Update CATBLK
      CALL OBHGET (OUT, MCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHGET (TAB2, TCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL IMBTNF (TCAT, MCAT, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBHPUT (OUT, MCAT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Destroy temp. table objects
      CALL TABDES (TAB1,  IERR)
      IF (IERR.NE.0) GO TO 990
      CALL TABDES (TAB2,  IERR)
      IF (IERR.NE.0) GO TO 990
      IERR = 0
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMTCOP: ERROR COPYING ' // TBTYPE // ' TABLE FROM '
     *   // IN
      CALL MSGWRT (7)
      MSGTXT = '   TO ' // OUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
      SUBROUTINE IM2TAB (IMAGE, TABLE, TBTYPE, TBVER, IERR)
C-----------------------------------------------------------------------
C   IMAGE class utility routine
C   Makes table object associated with a image object.
C   Inputs:
C      IMAGE   C*?  Name of image object.
C      TABLE   C*?  Name of table object.
C      TBTYPE  C*2  Table type.
C      TBVER   I    Table version
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMAGE*(*), TABLE*(*), TBTYPE*2
      INTEGER   TBVER, IERR
C
      INTEGER NKEY
C                                       NKEY = number of keywords to
C                                       copy from IMAGE to TABLE
      PARAMETER (NKEY = 4)
      INTEGER   LOOP, IVAL(20), TYPE, DIM(7), DUMMY
      CHARACTER KEYWDI(NKEY)*8, KEYWDO(NKEY)*8, CVAL*20, CDUMMY*1
      INCLUDE 'INCS:PAOOF.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA KEYWDI /'NAME','CLASS','IMSEQ','DISK'/
      DATA KEYWDO /'NAME','CLASS','IMSEQ','DISK'/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Make Table object
      CALL TABCRE (TABLE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Copy keywords
      DO 100 LOOP = 1,NKEY
         CALL IMGET (IMAGE, KEYWDI(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 990
         CALL TABPUT (TABLE, KEYWDO(LOOP), TYPE, DIM, IVAL, CVAL, IERR)
         IF (IERR.NE.0) GO TO 990
 100     CONTINUE
C                                       Table type
      DIM(1) = 2
      DIM(2) = 1
      DIM(3) = 0
      CALL TABPUT (TABLE, 'TBLTYPE', OOACAR, DIM, DUMMY, TBTYPE, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Table version
      DIM(1) = 1
      CALL TABPUT (TABLE, 'VER', OOAINT, DIM, TBVER, CDUMMY, IERR)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IM2TAB: ERROR MAKING TABLE FROM ' // IMAGE
      CALL MSGWRT (8)
C
 999  RETURN
      END
      SUBROUTINE BOXCHK (MFIELD, NGAUSS, FNAME, IMSIZE, NBOXES, WIN,
     *   NOV, FOV, IRET)
C-----------------------------------------------------------------------
C   Checks for Clean boxes that overlap between different facets of the
C   same Gaussian width.  The assumptions are that the facets are
C   numbered 1-NFACET for the 1st Gaussian width, NFACET+1-2*NFACET for
C   the second Gaussian width, etc with NFACET = MFIELD / NGAUSS.
C   Inputs:
C      MFIELD   I               Total number facets = NFACET*NGAUSS
C      NGAUSS   I               Number of Gaussian widths
C      FNAME    C(MFIELD)*(*)   Facet image names
C      IMSIZE   I(2,MFIELD)
C      NBOXES   I(*)            Number boxes in each facet
C      WIN      I(4,MFIELD,*)   Clean boxes
C   In/out:
C      NOV      I               In: upper limit to list FOV
c                               Out: Number overlapped pairs
C   Output:
C      FOV      I(5,*)          Box #, facet #, box# facet # overlaps
C      IRET     I               Error somehow
C-----------------------------------------------------------------------
      INTEGER   MFIELD, NGAUSS, IMSIZE(2,MFIELD), NBOXES(MFIELD),
     *   WIN(4,MFIELD,*), NOV, FOV(5,*), IRET
      CHARACTER FNAME(*)*(*)
C
      INTEGER   NFACET, F1, F2, I, J, K, IB, JB, NG, NP, MSGSAV, IERR,
     *   IN, LOV, XYC(2,9), IROUND
      REAL      XYI(7), XYL(7), ARG
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
      NFACET = MFIELD / MAX (1, NGAUSS)
      MSGSAV = MSGSUP
      LOV = NOV
      NOV = 0
      DO 100 NG = 1,NGAUSS
         F2 = NG * NFACET
         F1 = F2 - NFACET + 1
         DO 90 I = F1,F2
            DO 80 J = F1,F2
               IF ((NBOXES(I).GT.0) .AND. (NBOXES(J).GT.0) .AND.
     *            (I.NE.J)) THEN
C                                       do facets overlap?
                  XYI(1) = 1.0
                  XYI(2) = 1.0
                  MSGSUP = 32000
                  CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                  MSGSUP = MSGSAV
                  IF (IERR.NE.0) GO TO 80
                  XYC(1,1) = IROUND (XYL(1))
                  XYC(2,1) = IROUND (XYL(2))
                  XYI(1) = IMSIZE(1,I)
                  MSGSUP = 32000
                  CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                  MSGSUP = MSGSAV
                  IF (IERR.NE.0) GO TO 80
                  XYC(1,2) = IROUND (XYL(1))
                  XYC(2,2) = IROUND (XYL(2))
                  XYI(2) = IMSIZE(2,I)
                  MSGSUP = 32000
                  CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                  MSGSUP = MSGSAV
                  IF (IERR.NE.0) GO TO 80
                  XYC(1,3) = IROUND (XYL(1))
                  XYC(2,3) = IROUND (XYL(2))
                  XYI(1) = 1.0
                  MSGSUP = 32000
                  CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                  MSGSUP = MSGSAV
                  IF (IERR.NE.0) GO TO 80
                  XYC(1,4) = IROUND (XYL(1))
                  XYC(2,4) = IROUND (XYL(2))
                  IF ((XYC(1,1).LT.1) .AND. (XYC(1,2).LT.1) .AND.
     *               (XYC(1,3).LT.1) .AND. (XYC(1,4).LT.1))
     *               GO TO 80
                  IF ((XYC(1,1).GT.IMSIZE(1,J)) .AND.
     *               (XYC(1,2).GT.IMSIZE(1,J)) .AND.
     *               (XYC(1,3).GT.IMSIZE(1,J)) .AND.
     *               (XYC(1,4).GT.IMSIZE(1,J))) GO TO 80
                  IF ((XYC(2,1).LT.1) .AND. (XYC(2,2).LT.1) .AND.
     *               (XYC(2,3).LT.1) .AND. (XYC(2,4).LT.1))
     *               GO TO 80
                  IF ((XYC(2,1).GT.IMSIZE(2,J)) .AND.
     *               (XYC(2,2).GT.IMSIZE(2,J)) .AND.
     *               (XYC(2,3).GT.IMSIZE(2,J)) .AND.
     *               (XYC(2,4).GT.IMSIZE(2,J))) GO TO 80
C                                       They do actually
                  DO 60 IB = 1,NBOXES(I)
C                                       rectangle in I
                     IF (WIN(1,I,IB).GT.0) THEN
                        NP = 5
                        XYI(1) = (WIN(1,I,IB) + WIN(3,I,IB)) / 2.0
                        XYI(2) = (WIN(2,I,IB) + WIN(4,I,IB)) / 2.0
                        MSGSUP = 32000
                        CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                        MSGSUP = MSGSAV
                        IF (IERR.NE.0) GO TO 60
                        XYC(1,1) = IROUND (XYL(1))
                        XYC(2,1) = IROUND (XYL(2))
                        XYI(1) = WIN(1,I,IB)
                        XYI(2) = WIN(2,I,IB)
                        MSGSUP = 32000
                        CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                        MSGSUP = MSGSAV
                        IF (IERR.NE.0) GO TO 60
                        XYC(1,2) = IROUND (XYL(1))
                        XYC(2,2) = IROUND (XYL(2))
                        XYI(1) = WIN(3,I,IB)
                        MSGSUP = 32000
                        CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                        MSGSUP = MSGSAV
                        IF (IERR.NE.0) GO TO 60
                        XYC(1,3) = IROUND (XYL(1))
                        XYC(2,3) = IROUND (XYL(2))
                        XYI(2) = WIN(4,I,IB)
                        MSGSUP = 32000
                        CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                        MSGSUP = MSGSAV
                        IF (IERR.NE.0) GO TO 60
                        XYC(1,4) = IROUND (XYL(1))
                        XYC(2,4) = IROUND (XYL(2))
                        XYI(1) = WIN(1,I,IB)
                        MSGSUP = 32000
                        CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                        MSGSUP = MSGSAV
                        IF (IERR.NE.0) GO TO 60
                        XYC(1,5) = IROUND (XYL(1))
                        XYC(2,5) = IROUND (XYL(2))
C                                       circle in I
                     ELSE
                        NP = 9
                        XYI(1) = WIN(3,I,IB)
                        XYI(2) = WIN(4,I,IB)
                        MSGSUP = 32000
                        CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL, IERR)
                        MSGSUP = MSGSAV
                        IF (IERR.NE.0) GO TO 60
                        XYC(1,1) = IROUND (XYL(1))
                        XYC(2,1) = IROUND (XYL(2))
                        DO 20 K = 2,9
                           ARG = (K-2) * PI / 4.0
                           XYI(1) = WIN(3,I,IB) + WIN(2,I,IB)*COS(ARG)
                           XYI(2) = WIN(4,I,IB) + WIN(2,I,IB)*SIN(ARG)
                           MSGSUP = 32000
                           CALL PSNCVT (FNAME(I), XYI, FNAME(J), XYL,
     *                        IERR)
                           MSGSUP = MSGSAV
                           IF (IERR.NE.0) GO TO 60
                           XYC(1,K) = IROUND (XYL(1))
                           XYC(2,K) = IROUND (XYL(2))
 20                        CONTINUE
                        END IF
                     DO 50 JB = 1,NBOXES(J)
                        IF ((JB.NE.IB) .OR. (I.NE.J)) THEN
C                                       Box in J rectangle
                           IN = 0
                           IF (WIN(1,J,JB).GT.0) THEN
                              DO 30 K = 1,NP
                                 IF ((XYC(1,K).GE.WIN(1,J,JB)) .AND.
     *                              (XYC(1,K).LE.WIN(3,J,JB)) .AND.
     *                              (XYC(2,K).GE.WIN(2,J,JB)) .AND.
     *                              (XYC(2,K).LE.WIN(4,J,JB))) IN = IN+1
 30                              CONTINUE
C                                       Box in J circle
                           ELSE
                              DO 40 K = 1,NP
                                 ARG = (XYC(1,K)-WIN(3,J,JB))**2 +
     *                              (XYC(2,K)-WIN(4,J,JB))**2
                                 ARG = SQRT (ARG)
                                 IF (ARG.LE.WIN(2,J,JB)) IN = IN+1
 40                              CONTINUE
                              END IF
C                                       What do we do now???
                           IF ((I.EQ.J) .AND. (IN.LT.NP)) IN = 0
                           IF (IN.GT.0) THEN
                              IF ((IN.GT.1) .AND. (MSGSAV.EQ.0)) THEN
                                 WRITE (MSGTXT,1040) IB, I, JB, J, IN
                                 CALL MSGWRT (4)
                                 END IF
                              IF (NOV.LT.LOV) THEN
                                 NOV = NOV + 1
                                 FOV(1,NOV) = IB
                                 FOV(2,NOV) = I
                                 FOV(3,NOV) = JB
                                 FOV(4,NOV) = J
                                 FOV(5,NOV) = IN
                                 END IF
                              END IF
                           END IF
 50                     CONTINUE
 60                  CONTINUE
                  END IF
 80            CONTINUE
 90         CONTINUE
 100     CONTINUE
      IRET = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1040 FORMAT ('Box',I5,' facet',I5,' overlaps box',I5,' facet',I5,' at',
     *   I2,' points')
      END
      SUBROUTINE IMCALT (IMIN, IMOUT, IERR)
C-----------------------------------------------------------------------
C   IMdata class utility routine
C   Copies all tables from one imdata object to another except those
C   listed in optional keyword DROPTABS (<= 20 char*2 values)
C   Inputs:
C      IMIN    C*?  Name of input uvdata object.
C      IMOUT   C*?  Name of output uvdata object.
C   Output:
C      IERR    I    Error code: 0 => ok
C-----------------------------------------------------------------------
      CHARACTER IMIN*(*), IMOUT*(*)
      INTEGER   IERR
C
      INTEGER   LUNI, LUNO, DISKI, DISKO, CNOI, CNOO, BUFIN, BUFOUT,
     *   NDRPTB, MDRPTB, TYPE, DIM(7), IDUMMY, MSGSAV, I
      PARAMETER (MDRPTB=20)
      CHARACTER DRPTBS(MDRPTB)*2
      INCLUDE 'INCS:OBJPARM.INC'
      INCLUDE 'INCS:CLASSIO.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Open object for buffer
      CALL OBOPEN (IMIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBOPEN (IMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get buffer number
      CALL OBINFO (IMIN, BUFIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBINFO (IMOUT, BUFOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get other info
      CALL OBDSKC (IMIN, DISKI, CNOI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBDSKC (IMOUT, DISKO, CNOO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Catalog header
      CALL OBHGET (IMOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Assign LUNs
      CALL OBLUN (LUNI, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUN (LUNO, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Get table list not to copy
      MSGSAV = MSGSUP
      MSGSUP = 32000
      CALL IMGET (IMIN, 'DROPTABS', TYPE, DIM, IDUMMY, DRPTBS, IERR)
      MSGSUP = MSGSAV
      IF (IERR.NE.0) THEN
         IERR = 0
         NDRPTB = 0
         DRPTBS(1) = '  '
      ELSE
         NDRPTB = 0
         I = MAX (1, MIN (MDRPTB, DIM(2)))
 10      CONTINUE
         IF ((DRPTBS(NDRPTB+1).NE.'  ') .AND. (NDRPTB.LT.I)) THEN
            NDRPTB = NDRPTB + 1
            GO TO 10
            END IF
         END IF
C                                       Copy table
      CALL ALLTAB (NDRPTB, DRPTBS, LUNI, LUNO, DISKI, DISKO, CNOI,
     *   CNOO, CATBLK, OBUFFR(1,BUFIN), OBUFFR(1,BUFOUT), IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Save catalog header
      CALL OBHPUT (IMOUT, CATBLK, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Close object
      CALL OBCLOS (IMIN, IERR)
      IF (IERR.NE.0) GO TO 990
      CALL OBCLOS (IMOUT, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Free LUN
      CALL OBLUFR (LUNI)
      IF (IERR.NE.0) GO TO 990
      CALL OBLUFR (LUNO)
      IF (IERR.NE.0) GO TO 990
      GO TO 999
C                                       Error
 990  MSGTXT = 'IMCALT: ERROR COPYING TABLES FOR ' // IMIN
      CALL MSGWRT (7)
      MSGTXT = '   TO ' // IMOUT
      CALL MSGWRT (7)
C
 999  RETURN
      END
