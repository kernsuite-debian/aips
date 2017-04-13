      SUBROUTINE GRSLIC (IG, BBLC, BTRC, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! sets diagonal line with TV cursor and graphics
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2008-2009, 2015
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
C   GRSLIC uses a graphics plane to show the user a diagonal line and
C   let hime set the end points.
C   Inputs:
C      IG      I      graphics plane to use
C   In/Out:
C      BBLC    R(7)   Bottom left corners
C      BTRC    R(7)   Top right corners
C   Output:
C      SCRTCH  I(*)   Scratch buffer: > X dimension (>1280)
C      IERR    I      Error code
C   Input in common:
C      CATBLK  I(256)   Image catalog header for image being used
C   For circular: BBLC(1,i) = -1, BBLC(2,i) = radius, BTRC(,i) center
C-----------------------------------------------------------------------
      INTEGER   IG, SCRTCH(*), IERR
      REAL      BBLC(7), BTRC(7)
C
      INTEGER   ICH, ITW(3), IL, QUAD, IBUT, I, JERR, IPOS, LTVSC(2),
     *   ITEMP, SCROLX, SCROLY, IX(2), IY(2), LBUT
      REAL      PPOS(2), RPOS(2), IMWIND(4), TVAREA(4), RDEP(5), XPOS(2)
      LOGICAL   T, F, DOIT, ONGR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Check inputs
      IERR = 2
      IF ((IG.LT.1) .OR. (IG.GT.NGRAPH)) GO TO 999
C                                       windows
      IF ((CATBLK(IIWIN+2).LE.CATBLK(IIWIN)) .OR.
     *   (CATBLK(IIWIN+3).LE.CATBLK(IIWIN+1))) GO TO 999
      IF ((CATBLK(IICOR+2).LE.CATBLK(IICOR)) .OR.
     *   (CATBLK(IICOR+3).LE.CATBLK(IICOR+1))) GO TO 999
      IMWIND(1) = CATBLK(IIWIN)
      IMWIND(2) = CATBLK(IIWIN+1)
      IMWIND(3) = CATBLK(IIWIN+2)
      IMWIND(4) = CATBLK(IIWIN+3)
      TVAREA(1) = CATBLK(IICOR)
      TVAREA(2) = CATBLK(IICOR+1)
      TVAREA(3) = CATBLK(IICOR+2)
      TVAREA(4) = CATBLK(IICOR+3)
      RDEP(1) = CATBLK(IIDEP+0)
      RDEP(2) = CATBLK(IIDEP+1)
      RDEP(3) = CATBLK(IIDEP+2)
      RDEP(4) = CATBLK(IIDEP+3)
      RDEP(5) = CATBLK(IIDEP+4)
      IL = 0
C                                       Init
      ICH = NGRAY + IG
      CALL YHOLD ('ONNN', IERR)
      CALL YZERO (ICH, IERR)
      IF (IERR.NE.0) GO TO 999
      CALL ZTIME (ITW)
C                                       turn on graphics to be certain
      LTVSC(1) = TVSCGX
      LTVSC(2) = TVSCGY
      I = 2 ** (NGRAY+IG-1)
      ONGR = MOD (TVLIMG(1)/I, 2) .EQ. 1
      CALL YSLECT ('ONNN', ICH, 0, SCRTCH, IERR)
      IPOS = 1
      IF (IERR.NE.0) GO TO 980
C                                       no scroll on graphics now
      ITEMP = 2 ** NGRAY
      SCROLX = TVSCRX(1)
      SCROLY = TVSCRY(1)
      CALL YSCROL (ITEMP, SCROLX, SCROLY, T, IERR)
      IPOS = 2
      IF (IERR.NE.0) GO TO 980
      CALL YHOLD ('OFFF', IERR)
C                                       Init BLC of new box
      IL = 1
      MSGTXT = 'Set B.L.C. : button A, B, or C to change to T.R.C.'
      CALL MSGWRT (1)
      MSGTXT = 'Button D to kill and exit'
      CALL MSGWRT (1)
      IX(1) = (WINDTV(1) + WINDTV(3)) / 2
      IX(2) = IX(1) + 10
      IY(1) = (WINDTV(2) + WINDTV(4)) / 2
      IY(2) = IY(1) + 10
      RPOS(1) = IX(1)
      RPOS(2) = IY(1)
C                                       No scroll correction
C      QUAD = -1
C                                       ON cursor at desired position
 40   PPOS(1) = 0.0
      PPOS(2) = 0.0
      CALL YCURSE ('ONNN', F, T, RPOS, QUAD, IBUT, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
      IF (IERR.EQ.2) CALL YCURSE ('ONNN', F, F, RPOS, QUAD, IBUT, IERR)
      IPOS = 3
      IF (IERR.NE.0) GO TO 980
C                                       Cursor read loop
 50   CALL YCURSE ('READ', F, T, RPOS, QUAD, IBUT, IERR)
         IPOS = 4
         IF (IERR.NE.0) GO TO 980
C                                       keep in bounds
         XPOS(1) = RPOS(1)
         XPOS(2) = RPOS(2)
         RPOS(1) = MAX (TVAREA(1), MIN (TVAREA(3), RPOS(1)))
         RPOS(2) = MAX (TVAREA(2), MIN (TVAREA(4), RPOS(2)))
         IF ((ABS(RPOS(1)-XPOS(1)).GE.1.) .OR.
     *      (ABS(RPOS(2)-XPOS(2)).GE.1.)) THEN
            CALL YCURSE ('ONNN', F, T, RPOS, QUAD, LBUT, IERR)
            IF (IERR.NE.0) GO TO 980
            END IF
         IF (IBUT.EQ.0) IBUT = LBUT
         CALL DLINTR (RPOS, IBUT, PPOS, ITW, DOIT)
         IF (.NOT.DOIT) GO TO 50
C                                       Erase current box
         CALL YHOLD ('ONNN', IERR)
         CALL IMVECT ('OFFF', ICH, 2, IX, IY, SCRTCH, IERR)
         IPOS = 5
         IF (IERR.NE.0) GO TO 980
         CALL YHOLD ('OFFF', IERR)
C                                       New corners: BLC
         IF (IL.NE.2) THEN
            IX(1) = RPOS(1) + 0.5
            IY(1) = RPOS(2) + 0.5
C                                       top: regular boxes TRC
         ELSE
            IX(2) = RPOS(1) + 0.5
            IY(2) = RPOS(2) + 0.5
            END IF
C                                       Respond to buttons
         IF (IBUT.GT.0) THEN
            IF ((IL.EQ.1) .OR. (IBUT.EQ.1) .OR. (IBUT.EQ.2)) THEN
C                                       switch to TRC all buttons
               IF (IL.EQ.1) THEN
                  IF (IBUT.GT.7) GO TO 70
                  IL = 2
                  MSGTXT = 'Set T.R.C. : button A or B to repeat ' //
     *               'B.L.C.'
                  CALL MSGWRT (1)
                  MSGTXT = 'Button C or D to exit'
                  RPOS(1) = RPOS(1) + 10.0
                  RPOS(2) = RPOS(2) + 10.0
                  RPOS(1) = MIN (TVAREA(3), RPOS(1))
                  RPOS(2) = MIN (TVAREA(4), RPOS(2))
C                                       switch to other corn, but A
               ELSE
                  IF (IL.EQ.2) THEN
                     IL = 3
                     RPOS(1) = IX(1)
                     RPOS(2) = IY(1)
                     MSGTXT = 'Reset B.L.C. : buttons as for T.R.C.'
                  ELSE
                     IL = 2
                     RPOS(1) = IX(2)
                     RPOS(2) = IY(2)
                     MSGTXT = 'Reset T.R.C.'
                     END IF
                  END IF
               CALL MSGWRT (1)
               END IF
            END IF
C                                       draw all boxes
 70      IERR = 0
         CALL YHOLD ('ONNN', IERR)
         CALL IMVECT ('ONNN', ICH, 2, IX, IY, SCRTCH, IERR)
         IPOS = 6 + 100
         IF (IERR.NE.0) GO TO 980
         CALL YHOLD ('OFFF', IERR)
         IF (IBUT.GT.7) GO TO 800
C                                       same box
         IF (IBUT.EQ.0) GO TO 50
         GO TO 40
C                                       DONE: fill in real boxes
 800  BBLC(1) = (IX(1) - TVAREA(1)) * (IMWIND(3) - IMWIND(1))
     *   / (TVAREA(3) - TVAREA(1)) + IMWIND(1)
      BBLC(2) = (IY(1) - TVAREA(2)) * (IMWIND(4) - IMWIND(2))
     *   / (TVAREA(4) - TVAREA(2)) + IMWIND(2)
C                                       TRC
      BTRC(1) = (IX(2) - TVAREA(1)) * (IMWIND(3) - IMWIND(1))
     *   / (TVAREA(3) - TVAREA(1)) + IMWIND(1)
      BTRC(2) = (IY(2) - TVAREA(2)) * (IMWIND(4) - IMWIND(2))
     *   / (TVAREA(4) - TVAREA(2)) + IMWIND(2)
      CALL RCOPY (5, RDEP, BBLC(3))
      CALL RCOPY (5, RDEP, BTRC(3))
      IERR = 0
C                                       Off cursor, graphics, scroll
C                                       leave graphics on
 980  CALL YCURSE ('OFFF', F, T, RPOS, QUAD, IBUT, JERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1980) IERR, IPOS
         CALL MSGWRT (7)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('GRSLIC: ERROR CODE',I7,' AT',I5)
      END
