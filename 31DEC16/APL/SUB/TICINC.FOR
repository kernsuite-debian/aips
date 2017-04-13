      SUBROUTINE TICINC (LAXIS, BLC, TRC, XYRATO, YX, DEGC, DEG, INOI,
     *   TICX, TICY, TICL, PT5SEC, ITRY, IERR)
C-----------------------------------------------------------------------
C! determines tick mark lengths and increments for CTICS, ...
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1999-2003, 2012-2013
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
C   TICINC figures out the tick mark lengths and increments.
C   Inputs:
C      LAXIS   I        1 => horizontal,  2 => vertical full plots
C                       3 => horiz subplot 4 => vertical subplot
C                       5 => top horizontal 6 => right vertical
C      BLC     R(2)     X and Y pixels to form bottom left hand
C                       corner of the graph.
C      TRC     R(2)     X and Y pixels to form the top right hand
C                       corner of the graph.
C      XYRATO  R        X to Y scaling factor
C   In/Out:
C      YX      D        3: plot x axis at y = YX; 4: plot y axis
C                       at x = YX - out range => BLC(1,2) value
C      DEG     D        Tick increment: input if LAXIS>4, else out
C   Outputs:
C      DEGC    D        Value at first tick
C      INOI    I        Number of ticks
C      TICX    D        Tick length in x units (if LAXIS < 5)
C      TICY    D        Tick length in y units (if LAXIS < 5)
C      TICL    R        Tick length in pixels. (if LAXIS < 5)
C      PT5SEC  D        value used in rounding coordinate values
C      ITRY    I        subscript in coordinate incr array
C      IERR    I        error code: 0 => ok
C                                  -1 => can't find an increment
C                                   1 => bad LAXIS
C                                   3 => tic algorithm fails
C-----------------------------------------------------------------------
      DOUBLE PRECISION YX, DEGC, DEG, TICX, TICY, PT5SEC
      REAL      BLC(2), TRC(2), XYRATO, TICL
      INTEGER   LAXIS, INOI, ITRY, IERR
C
      INTEGER   FINE
      PARAMETER (FINE=24)
C
      DOUBLE PRECISION DEGL, DEGU, DBX, DIV, DIFF, DTX, DX, DBY, DTY,
     *   DY, DZ, XINTER(35), DTZ, XINTR1(35), XINTR2(35), XINTR3(35),
     *   XINTR(35,3), XMAX, XMIN, YMAX, YMIN
      REAL      FRACT, PXT, PYT, X, Y
      INTEGER   IAXIS, AXISTP, IXT, IXTNO, I, ISGN, COOTYP, JERR, JJ, J
      LOGICAL   TRUBLE
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DGPH.INC'
      EQUIVALENCE (XINTR(1,1), XINTR1(1))
      EQUIVALENCE (XINTR(1,2), XINTR2(1))
      EQUIVALENCE (XINTR(1,3), XINTR3(1))
C
      SAVE ISGN, AXISTP
C
      DATA XINTR1 /0.000001D0, 0.000002D0, 0.000005D0, 0.00001D0,
     *   0.00002D0, 0.00005D0, 0.0001D0, 0.0002D0, 0.0005D0, 0.001D0,
     *   0.002D0, 0.005D0, 0.01D0, 0.02D0, 0.05D0, 0.1D0, 0.2D0, 0.5D0,
     *   1.D0, 2.D0, 5.D0, 10.D0, 20.D0, 50.D0, 100.D0, 200.D0, 500.D0,
     *   1000.D0, 2000.D0, 5000.D0, 10000.D0, 20000.D0, 50000.D0,
     *   100000.D0, 100000.D0/
      DATA XINTR2 /0.000001D0, 0.000002D0, 0.000005D0, 0.00001D0,
     *   0.00002D0, 0.00005D0, 0.0001D0, 0.0002D0, 0.0005D0, 0.001D0,
     *   0.002D0, 0.005D0, 0.01D0, 0.02D0, 0.05D0, 0.1D0, 0.2D0, 0.5D0,
     *   1.D0, 2.D0, 5.D0, 15.D0, 30.D0, 60.D0, 120.D0, 300.D0, 600.D0,
     *   1800.D0, 3600.D0, 7200.D0, 18000.D0, 36000.D0, 72000.D0,
     *   180000.D0, 360000.D0/
      DATA XINTR3 /0.000001D0, 0.000002D0, 0.000005D0, 0.00001D0,
     *   0.00002D0, 0.00005D0, 0.0001D0, 0.0002D0, 0.0005D0, 0.001D0,
     *   0.002D0, 0.005D0, 0.01D0, 0.02D0, 0.05D0, 0.1D0, 0.2D0, 0.5D0,
     *   1.D0, 2.D0, 5.D0, 15.D0, 30.D0, 60.D0, 120.D0, 300.D0, 600.D0,
     *   1800.D0, 3600.D0, 7200.D0, 14400.D0, 28800.D0, 86400.D0,
     *   172800.D0, 345600.0D0/
C-----------------------------------------------------------------------
      CALL CHECKL ('TICINC')
      IAXIS = LAXIS
      TRUBLE = .FALSE.
      IF (LAXIS.GT.2) IAXIS = IAXIS - 2
      IF (LAXIS.GT.4) GO TO 200
C                                       Find value at BLC.
C                                       Determine axis type.
      IF (IAXIS.EQ.2) THEN
C                                       vertical
         YMAX = -1.E10
         YMIN = 1.E10
         FRACT = 1.0
         IF (GPHY1.NE.GPHY2) FRACT = ABS ((TRC(2) - BLC(2)) /
     *      (GPHY2 - GPHY1))
         FRACT = MAX (0.05, MIN (1.0, FRACT))
         TICY = 0.0
         DO 10 I = 1,FINE
            PXT = BLC(1) + (I-1.0)*(TRC(1)-BLC(1))/(FINE - 1.0)
            DO 5 J = 1,FINE
               PYT = BLC(2) + (J-1.0)*(TRC(2)-BLC(2))/(FINE - 1.0)
               CALL XYVAL (PXT, PYT, DX, DY, DZ, IERR)
               IF (IERR.EQ.0) THEN
                  YMAX = MAX (YMAX, DY)
                  YMIN = MIN (YMIN, DY)
                  END IF
 5             CONTINUE
 10         CONTINUE
         IF (YMAX.LT.YMIN) THEN
            IERR = 4
            GO TO 970
            END IF
         TICX = ABS ((TRC(1)-BLC(1) + (TRC(2)-BLC(2))/XYRATO) / 80.0)
         IF (TICX.LE.0.05) TICX = 3 * TICX
         TICL = TICX * 1.2
         TICX = ABS (AXINC(1,LOCNUM) * TICX)
         IF (AXINC(2,LOCNUM).GT.0) THEN
            ISGN = 1
            DEGU = YMAX
            DEGL = YMIN
         ELSE
            ISGN = -1
            DEGU = YMIN
            DEGL = YMAX
            END IF
         IXTNO = 28.0 * FRACT + 0.5
         IXTNO = MIN (11, MAX (4, IXTNO))
         AXISTP = LABTYP(LOCNUM) / 10
         IF ((MOD(AXISTP,2).EQ.1) .AND. (ABS(DEGU-DEGL).GT.340.)) IXTNO
     *      = 2 * IXTNO
         IF (LAXIS.GE.3) THEN
            CALL XYVAL (TRC(1), BLC(2), DX, DY, DZ, JERR)
            IF (JERR.NE.0) GO TO 30
            CALL XYVAL (BLC(1), BLC(2), DBX, DBY, DZ, JERR)
            IF (JERR.NE.0) GO TO 30
            IF ((DBX.GT.DX) .AND. ((YX.GT.DBX) .OR. (YX.LT.DX)))
     *         YX = DBX
            IF ((DBX.LT.DX) .AND. ((YX.LT.DBX) .OR. (YX.GT.DX)))
     *         YX = DBX
            END IF
C                                       horizontal
      ELSE
         XMAX = -1.E10
         XMIN = 1.E10
         TICX = 0.0
         DO 20 I = 1,FINE
            PXT = BLC(1) + (I-1.0)*(TRC(1)-BLC(1))/(FINE - 1.0)
            DO 15 J = 1,FINE
               PYT = BLC(2) + (J-1.0)*(TRC(2)-BLC(2))/(FINE - 1.0)
               CALL XYVAL (PXT, PYT, DX, DY, DZ, IERR)
               IF (IERR.EQ.0) THEN
                  XMAX = MAX (XMAX, DX)
                  XMIN = MIN (XMIN, DX)
                  END IF
 15             CONTINUE
 20         CONTINUE
         IF (XMAX.LT.XMIN) THEN
            IERR = 4
            GO TO 970
            END IF
         FRACT = 1.0
         IF (GPHX1.NE.GPHX2) FRACT = ABS ((TRC(1) - BLC(1)) /
     *      (GPHX2 - GPHX1))
         FRACT = MAX (0.05, MIN (1.0, FRACT))
         TICY = ABS (((TRC(1)-BLC(1))*XYRATO + TRC(2)-BLC(2)) / 80.0)
         IF (TICY.LE.0.05) TICY = 3.0 * TICY
         TICL = TICY * 1.2
         TICY = ABS (AXINC(2,LOCNUM) * TICY)
         IF (AXINC(1,LOCNUM).GT.0.0) THEN
            ISGN = -1
            DEGU = XMIN
            DEGL = XMAX
         ELSE
            ISGN = 1
            DEGU = XMAX
            DEGL = XMIN
            END IF
         IXTNO = 17.0 * FRACT + 0.5
         IXTNO = MIN (10, MAX (4, IXTNO))
         AXISTP = MOD (LABTYP(LOCNUM), 10)
         IF ((MOD(AXISTP,2).EQ.1) .AND. (ABS(DEGU-DEGL).GT.340.)) IXTNO
     *      = 2 * IXTNO
         IF (LAXIS.GE.3) THEN
            CALL XYVAL (BLC(1), TRC(2), DX, DY, DZ, JERR)
            IF (JERR.NE.0) GO TO 30
            CALL XYVAL (BLC(1), BLC(2), DBX, DBY, DZ, JERR)
            IF (JERR.NE.0) GO TO 30
            IF ((DBY.GT.DY) .AND. ((YX.GT.DBY) .OR. (YX.LT.DY)))
     *         YX = DBY
            IF ((DBY.LT.DY) .AND. ((YX.LT.DBY) .OR. (YX.GT.DY)))
     *         YX = DBY
            END IF
         END IF
C                                       Determine possible tic intervls
 30   CALL TICCOR (TICX, TICY)
      COOTYP = 2
      IF ((AXISTP.EQ.5) .OR. (AXISTP.GE.7)) COOTYP = 1
      DIV = 240.0D0
      IF (COOTYP.EQ.2) DIV = 3600.0D0
C                                       not RA or DEC types
      IF (AXISTP.EQ.0) THEN
         DIV = 1.0D0
         JJ = 1
      ELSE IF (COOTYP.EQ.2) THEN
         DIV = 3600.0D0
         JJ = 2
      ELSE
         DIV = 240.0D0
         JJ = 3
         END IF
      PT5SEC = .5D0 / DIV
      DO 100 I = 1,35
         XINTER(I) = XINTR(I,JJ) / DIV
 100     CONTINUE
C                                       Try different values.
 120  IXT = 2
 125  DO 130 ITRY = 1,35
         DEG = ISGN * XINTER(ITRY)
         DEGC = DINT(DEGU/DEG) * DEG
         IF (ISGN * DEGU.LT.0.0) DEGC = DEGC - DEG
         DIFF = ISGN * (DEGC - DEGL)
         IF (ABS(DIFF / XINTER(ITRY)).GT.32000.0) GO TO 130
            INOI = (DIFF / XINTER(ITRY)) + 1
            IF ((INOI.GT.IXT) .AND. (INOI.LE.IXTNO)) GO TO 140
 130     CONTINUE
      IF (IXT.NE.0) THEN
         IXT = 0
         IXTNO = IXTNO + 2
         GO TO 125
C                                       Tic mark algorithm failed.
      ELSE
         WRITE (MSGTXT,1130)
         CALL MSGWRT (6)
         IERR = -1
         GO TO 999
         END IF
C                                       check number of actual ticks
 140  JJ = ABS (MIN (DEGL,DEGU)) / XINTER(ITRY)
      IXT = ABS (MAX (DEGL,DEGU)) / XINTER(ITRY)
      IF (MIN(DEGL,DEGU).GT.0) THEN
         JJ = JJ + 1
      ELSE
         JJ = -JJ
         END IF
      IF (MAX(DEGL,DEGU).LT.0) IXT = -IXT - 1
      INOI = IXT - JJ + 1
      IF ((INOI.LE.1) .AND. (ITRY.GT.1)) THEN
         ITRY = ITRY - 1
         GO TO 140
         END IF
      DEG = ISGN * XINTER(ITRY)
      DEGC = DINT(DEGU/DEG) * DEG
      IF (ISGN * DEGU.LT.0.0) DEGC = DEGC - DEG
C                                       extend ? when bail out comp.
      IF (.NOT.TRUBLE) GO TO 190
      IF (LAXIS.GT.2) GO TO 190
 150  DY = DEGC + DEG
         DX = DY
         IF (LAXIS.EQ.2) CALL FNDX (BLC(1), DY, DX, JERR)
         IF (LAXIS.EQ.1) CALL FNDY (BLC(2), DX, DY, JERR)
         IF (JERR.NE.0) GO TO 160
         CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 160
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 160
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 160
            DEGC = DEGC + DEG
            INOI = INOI + 1
            GO TO 150
 160  DY = DEGC - INOI * DEG
         DX = DY
         IF (LAXIS.EQ.2) CALL FNDX (BLC(1), DY, DX, JERR)
         IF (LAXIS.EQ.1) CALL FNDY (BLC(2), DX, DY, JERR)
         IF (JERR.NE.0) GO TO 170
         CALL XYPIX (DX, DY, X, Y, JERR)
         IF (JERR.NE.0) GO TO 170
         IF ((X.LT.BLC(1)-0.01) .OR. (X.GT.TRC(1)+0.01)) GO TO 170
         IF ((Y.LT.BLC(2)-0.01) .OR. (Y.GT.TRC(2)+0.01)) GO TO 170
            INOI = INOI + 1
            GO TO 160
C                                       change increment?
 170  IF (INOI.LE.IXTNO) GO TO 190
         DEGU = DEGC
         DEGL = DEGC - DEG * (INOI-1)
         ITRY = ITRY + 1
         DEG = ISGN * XINTER(ITRY)
         DEGC = DINT(DEGU/DEG) * DEG
         IF (ISGN * DEGU.LT.0.0) DEGC = DEGC - DEG
         DIFF = ISGN * (DEGC - DEGL)
         INOI = (DIFF / XINTER(ITRY)) + 1
C                                       round off
 190  IF (AXISTP.GT.0) THEN
         IF (ITRY.LT.19) THEN
            PT5SEC = 0.0D0
         ELSE IF (ITRY.GT.28) THEN
            PT5SEC = XINTER(28)
         ELSE IF (ITRY.GT.23) THEN
            PT5SEC = XINTER(23)
         ELSE
            PT5SEC = XINTER(18)
            END IF
         END IF
      IERR = 0
      GO TO 999
C                                       Draw tics for other side.
C                                       Same intervals but not
C                                       necessarily same values.
 200  IF (LAXIS.EQ.5) GO TO 210
         IF (LAXIS.NE.6) GO TO 990
         CALL XYVAL (TRC(1), TRC(2), DBX, DBY, DZ, IERR)
         IF (IERR.EQ.0) CALL XYVAL (TRC(1), BLC(2), DX, DY, DZ, IERR)
         IF (IERR.EQ.0) GO TO 205
            PXT = (BLC(1) + TRC(1)) / 2.0
            CALL XYVAL (PXT, TRC(2), DBX, DBY, DZ, IERR)
            IF (IERR.EQ.0) CALL XYVAL (PXT, BLC(2), DX, DY, DZ, IERR)
            IF (IERR.EQ.0) GO TO 205
               PYT = (BLC(2) + TRC(2)) / 2.0
               CALL XYVAL (PXT, PYT, DX, DY, DZ, IERR)
               IF (IERR.NE.0) GO TO 970
               PYT = PYT + (TRC(2) - BLC(2)) / 10.0
               CALL XYVAL (PXT, PYT, DTX, DTY, DTZ, IERR)
               IF (IERR.NE.0) GO TO 970
               DBY = DY + 5.0D0 * (DTY - DY)
               DY = DY - 5.0D0 * (DTY - DY)
               TRUBLE = .TRUE.
 205     DEGL = DY
         DEGU = DBY
         GO TO 220
C                                       horizontal
 210  CONTINUE
         CALL XYVAL (TRC(1), TRC(2), DBX, DBY, DZ, IERR)
         IF (IERR.EQ.0) CALL XYVAL (BLC(1), TRC(2), DX, DY, DZ, IERR)
         IF (IERR.EQ.0) GO TO 215
            PYT = (BLC(2) + TRC(2)) / 2.0
            CALL XYVAL (TRC(1), PYT, DBX, DBY, DZ, IERR)
            IF (IERR.EQ.0) CALL XYVAL (BLC(1), PYT, DX, DY, DZ, IERR)
            IF (IERR.EQ.0) GO TO 215
               PXT = (BLC(1) + TRC(1)) / 2.0
               CALL XYVAL (PXT, PYT, DX, DY, DZ, IERR)
               IF (IERR.NE.0) GO TO 970
               PXT = PXT + (TRC(1) - BLC(1)) / 10.0
               CALL XYVAL (PXT, PYT, DTX, DTY, DTZ, IERR)
               IF (IERR.NE.0) GO TO 970
               DBX = DX + 5.0D0 * (DTX - DX)
               DX = DX - 5.0D0 * (DTX - DX)
               TRUBLE = .TRUE.
 215     DEGU = DX
         DEGL = DBX
 220  DEGC = DINT(DEGU/DEG) * DEG
      IF (ISGN * DEGU.LT.0.0) DEGC = DEGC - DEG
      DIFF = ISGN * (DEGC - DEGL)
      IF ((DIFF.LT.0.0D0) .AND. (AXISTP.GT.0)) DIFF = DIFF + 360.0
      INOI = (DIFF/ABS(DEG)) + 1
      IERR =0
      GO TO 999
C                                       Total position failure
 970  IERR = 3
      WRITE (MSGTXT,1970)
      CALL MSGWRT (7)
      GO TO 999
C                                       Invalid axis type.
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
      IERR = 1
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1130 FORMAT ('TICINC: TIC MARK ALGORITHM FAILED! CONTINUING.')
 1970 FORMAT ('TICINC: POSITION ROUTINES FAIL ON THIS IMAGE')
 1990 FORMAT ('TICINC: INVALID AXIS TYPE.')
      END
