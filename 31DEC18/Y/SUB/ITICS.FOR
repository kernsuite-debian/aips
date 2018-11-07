      SUBROUTINE ITICS (IAXIS, IGR, INSIDE, RX0, RY0, XSCAL, YSCAL, BLC,
     *   TRC, DOACRS, IBUFF, IERR)
C-----------------------------------------------------------------------
C! draws tick marks and labels on TV
C# TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2003, 2005-2006, 2012-2015
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
C   ITICS writes tick marks and tick labels to the TV directly
C   Inputs:
C      IAXIS    I     1 => horizontal label below,  2 => vertical label
C                     to left,  5 horizontal above, 6 vertical to right
C      IGR      I     Graphics channel to use
C      INSIDE   L     T => labels inside borders, not outside
C      RX0      R     corner position on screen in map pixels: X
C      RY0      R     corner position on screen in map pixels: Y
C      XSCAL    R     X ratio map pixel / TV pixel
C      YSCAL    R     Y ratio map pixel / TV pixel
C      BLC      R(2)  X AND Y pixels to form bottom left hand
C                     corner of the graph.
C      TRC      R(2)  X and Y pixels to form the top right hand
C                     corner of the graph.
C      DOACRS   L     Do coordinate grid (T) or just ticks (F)
C   Outputs:
C      IBUFF    I(*)  scratch buffer.
C      IERR     I     error code: 0 => ok
C                                 1 => bad IAXIS
C                                 2 => graph drawing error
C                                 3 => tic algorithm fails
C-----------------------------------------------------------------------
      INTEGER   IAXIS, IGR, IBUFF(*), IERR
      LOGICAL   INSIDE, DOACRS
      REAL      RX0, RY0, XSCAL, YSCAL, BLC(2), TRC(2)
C
      CHARACTER SPRTXT*24, CHDL*4
      DOUBLE PRECISION DEG, DEGC, DTX, DX, DTY, DY, PT5SEC, TICX,
     *   TICY, AYX, DEGC0, DEG0, XDT, YDT, DEGC1, DEGU, DEGL, LDX, LDY,
     *   LLDX, LLDY, UPLIM, LOLIM, XUPLIM, XLOLIM, DZ
      REAL      DCX, DCY, X, Y, D, XT, YT, TICL, XYRATO, TICT, BX(2),
     *   BY(2), PRX, PRY, DXLIM, DYLIM, LBLC(2), LTRC(2)
      INTEGER   HML(2), AXISTP, LX, LY, IX(2), IY(2), GRCHAN, IG1, IG2,
     *   ILL, IX2L, XCH(35,2), I, IANGL, ILEN, ITRY, INCHAR, INOI,
     *   COOTYP, JERR, I1, I2, J1, J2, J
      LOGICAL   NONUM, FIRST, VERTCL, UP, SPOLE, NPOLE
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA ILL /0/
      DATA XCH /3*16, 3*15, 3*14, 3*13, 3*12, 3*11, 5*9, 5*6, 7*3,
     *   3*7, 3*6, 3*5, 3*4, 3*3, 3*2, 17*0/
C-----------------------------------------------------------------------
      XYRATO = XSCAL / YSCAL
      VERTCL = (IAXIS/2)*2.EQ.IAXIS
      LBLC(1) = BLC(1) - 0.5
      LBLC(2) = BLC(2) - 0.5
      LTRC(1) = TRC(1) + 0.5
      LTRC(2) = TRC(2) + 0.5
C                                       Assign initial values.
      INCHAR = 10
      IERR = 0
      IANGL = 0
      IX2L = -1000
      IG1 = IGR
      IF (IG1.EQ.0) IG1 = MIN (2, NGRAPH)
      IG2 = 0
      IF (NGRAPH.GE.4) IG2 = NGRAPH
      GRCHAN = NGRAY + IG1
C                                       vertical
      IF (VERTCL) THEN
         AYX = 0.0D0
         IF (IAXIS.EQ.6) THEN
            CALL TICINC (2, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI,
     *         TICX, TICY, TICL, PT5SEC, ITRY, IERR)
            IF (IERR.NE.0) THEN
               IERR = MAX (0, IERR)
               GO TO 999
               END IF
            END IF
         CALL TICINC (IAXIS, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, IERR)
         IF (IERR.NE.0) THEN
            IERR = MAX (0, IERR)
            GO TO 999
            END IF
         IF (IERR.NE.0) GO TO 999
         DCX = -1.0 * CSIZTV(1)
         DCY = -0.5 * CSIZTV(2)
         TICL = (MAXXTV(1) + MAXXTV(2)) / 67.0 * XSCAL
         TICL = MIN (TICL, 0.08 * (LTRC(1)-LBLC(1)))
         TICX = ABS (AXINC(1,LOCNUM) * TICL) / 1.2
         TICX = TICX * XSCAL
         CALL TICCOR (TICX, TICY)
         IF (INSIDE) DCX = 1.5 * CSIZTV(1) + 0.5
         IF (IAXIS.EQ.6) DCX = -DCX
         AXISTP = LABTYP(LOCNUM) / 10
         IF (IAXIS.EQ.6) TICX = -TICX
C                                       horizontal
      ELSE
         AYX = 0.0D0
         IF (IAXIS.EQ.5) THEN
            CALL TICINC (1, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI,
     *         TICX, TICY, TICL, PT5SEC, ITRY, IERR)
            IF (IERR.NE.0) THEN
               IERR = MAX (0, IERR)
               GO TO 999
               END IF
            END IF
         CALL TICINC (IAXIS, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, IERR)
         IF (IERR.NE.0) THEN
            IERR = MAX (0, IERR)
            GO TO 999
            END IF
         IF (IERR.NE.0) GO TO 999
         DCX = 0.5 * CSIZTV(1)
         DCY = -1.5 * CSIZTV(2)
         TICL = (MAXXTV(1) + MAXXTV(2)) / 67.0 * YSCAL
         TICL = MIN (TICL, 0.08 * (LTRC(2)-LBLC(2)))
         TICY = ABS (AXINC(2,LOCNUM) * TICL) / 1.2
         TICY = TICY * YSCAL
         CALL TICCOR (TICX, TICY)
         IF (INSIDE) DCY = 1.5 * CSIZTV(2) + 0.5
         IF (IAXIS.EQ.5) THEN
            DCY = CSIZTV(2) / 2.0
            IF (INSIDE) DCY = - CSIZTV(2)
            END IF
         AXISTP = MOD (LABTYP(LOCNUM), 10)
         IF (IAXIS.EQ.6) TICY = -TICY
         END IF
C                                       Determine possible tic intervls
      IF (VERTCL) THEN
         NONUM = (CTYP(2,LOCNUM).EQ.' ')
      ELSE
         NONUM = (CTYP(1,LOCNUM).EQ.' ')
         END IF
      LDX = RPVAL(1,LOCNUM)
      LLDX = RPVAL(1,LOCNUM)
      LDY = RPVAL(2,LOCNUM)
      LLDY = RPVAL(2,LOCNUM)
      COOTYP = 2
      IF ((AXISTP.EQ.5) .OR. (AXISTP.GE.7)) COOTYP = 1
C                                       Get # chars for INSIDE vertical
      IF ((IAXIS.EQ.2) .AND. (INSIDE)) THEN
C                                       RA, Dec axes
         IF (AXISTP.NE.0) THEN
            I = XCH(ITRY,1)
            DCX = DCX + I*CSIZTV(1)
C                                       Other axis types
         ELSE
            DEGU = DEGC
            DEGL = DEGC - (INOI-1)*DEG
            D = DEGU
            IF (ABS(DEGU).LT.DABS(DEGC)) D = DEGC
            IF (ABS(DEGL).LT.ABS(D)) D = DEGL
            I = LOG10 (ABS(D)) + 1.00
            I = MAX (I, 1)
            IF ((DEGU.LT.0.0) .OR. (DEGC.LE.0.0) .OR. (DEGL.LT.0))
     *         I = I + 1
            I = I + XCH(ITRY,2)
            DCX = DCX + (I + 0.5) * CSIZTV(1)
            END IF
         END IF
C                                       Draw tic marks and values.
      HML(1) = -32000
      HML(2) = HML(1)
      CHDL = '$$'
      DEGC0 = DEGC
      FIRST = .TRUE.
      DEG0 = DEG
      DEGC1 = DEGC0 - DEG
      DO 50 I = 1,INOI
         IF (VERTCL) THEN
            DY = DEGC
            IF (IAXIS.EQ.6) THEN
               CALL FNDX (TRC(1), DY, DX, JERR)
               IF (JERR.NE.0) GO TO 40
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (X.LT.TRC(1)-0.1) GO TO 40
               X = MAX (X, TRC(1))
            ELSE
               CALL FNDX (BLC(1), DY, DX, JERR)
               IF (JERR.NE.0) GO TO 40
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (X.GT.BLC(1)+0.1) GO TO 40
               X = MIN (X, BLC(1))
               END IF
         ELSE
            DX = DEGC
            IF (IAXIS.EQ.5) THEN
               CALL FNDY (TRC(2), DX, DY, JERR)
               IF (JERR.NE.0) GO TO 40
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (Y.LT.TRC(2)-0.1) GO TO 40
               Y = MAX (Y, TRC(2))
            ELSE
               CALL FNDY (BLC(2), DX, DY, JERR)
               IF (JERR.NE.0) GO TO 40
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (Y.GT.BLC(2)+0.1) GO TO 40
               Y = MIN (Y, BLC(2))
               END IF
            END IF
         IF (JERR.NE.0) GO TO 40
C                                       Convert degrees to pixels.
         IF (JERR.NE.0) GO TO 40
         IF ((X.LT.LBLC(1)-0.01) .OR. (X.GT.LTRC(1)+0.01)) GO TO 40
         IF ((Y.LT.LBLC(2)-0.01) .OR. (Y.GT.LTRC(2)+0.01)) GO TO 40
         LX = (X + RX0)/XSCAL + 0.5
         LY = (Y + RY0)/YSCAL + 0.5
         IF (DX.NE.LDX) LLDX = LDX
         IF (DY.NE.LDY) LLDY = LDY
         LDX = DX
         LDY = DY
C                                       Find end of tic.
         DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 20
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF (TICL.LE.0.) TICL = 1.
         IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
            IF (TICT.LE.0.0) GO TO 20
            DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL / TICT
            DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL / TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 20
            END IF
         IF ((XT.LT.LBLC(1)-0.01) .OR. (XT.GT.LTRC(1)+0.01)) GO TO 20
         IF ((YT.LT.LBLC(2)-0.01) .OR. (YT.GT.LTRC(2)+0.01)) GO TO 20
C                                       Simple tick
         IX(2) = (XT + RX0)/XSCAL + 0.5
         IY(2) = (YT + RY0)/YSCAL + 0.5
         IX(1) = LX
         IY(1) = LY
         CALL IMVECT ('ONNN', GRCHAN, 2, IX, IY, IBUFF, IERR)
         IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
         DEGC1 = DEGC - DEG
         IF (FIRST) DEGC0 = DEGC
         FIRST = .FALSE.
C                                       Convert degrees to DEC/RA.
 20      IF (.NOT.NONUM) THEN
            CALL TICSTR (ITRY, DEGC, PT5SEC, AXISTP, COOTYP, CHDL, HML,
     *         SPRTXT, ILEN)
            IF (IAXIS.EQ.6) THEN
               IX(1) = LX + DCX + 0.5
            ELSE
               IX(1) = LX + DCX - ILEN*CSIZTV(1) + 0.5
               END IF
            IY(1) = LY + DCY + 0.5
            IX(2) = IX(1) + ILEN*CSIZTV(1) - 1
            IY(2) = IY(1) + CSIZTV(2) - 1
            IF ((IX(1).LT.1) .OR. (IY(1).LT.1) .OR. (IX(2).GT.MAXXTV(1))
     *         .OR. (IY(2).GT.MAXXTV(2))) GO TO 40
            IF ((VERTCL) .OR. (IX(1).GT.IX2L)) THEN
               IX2L = IX(2)
               CALL IMANOT ('WRIT', IG1, IX, IY, IANGL, ILL,
     *            SPRTXT(1:ILEN), IBUFF, IERR)
               IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
               END IF
            END IF
 40      DEGC = DEGC - DEG
 50      CONTINUE
C                                       Draw tics for other side.
C                                       Same intervals but not
C                                       necessarily same values.
      IF (.NOT.DOACRS) THEN
         IF (VERTCL) THEN
            I = 6
            IF (IAXIS.EQ.6) I = 2
         ELSE
            I = 5
            IF (IAXIS.EQ.5) I = 1
            END IF
         CALL TICINC (I, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI, TICX,
     *      TICY, TICL, PT5SEC, ITRY, IERR)
         IF (IERR.NE.0) THEN
            IERR = MAX (0, IERR)
            GO TO 999
            END IF
C                                       Loop for other border.
         DO 90 I = 1,INOI
            IF (VERTCL) THEN
               DY = DEGC
               IF (IAXIS.EQ.6) THEN
                  CALL FNDX (BLC(1), DY, DX, JERR)
                  IF (JERR.NE.0) GO TO 80
                  CALL XYPIX (DX, DY, X, Y, JERR)
                  IF (X.GT.BLC(1)+0.1) GO TO 80
                  X = MIN (X, BLC(1))
               ELSE
                  CALL FNDX (TRC(1), DY, DX, JERR)
                  IF (JERR.NE.0) GO TO 80
                  CALL XYPIX (DX, DY, X, Y, JERR)
                  IF (X.LT.TRC(1)-0.1) GO TO 80
                  X = MAX (X, TRC(1))
                  END IF
            ELSE
               DX = DEGC
               IF (IAXIS.EQ.5) THEN
                  CALL FNDY (BLC(2), DX, DY, JERR)
                  IF (JERR.NE.0) GO TO 80
                  CALL XYPIX (DX, DY, X, Y, JERR)
                  IF (Y.GT.BLC(2)+0.1) GO TO 80
                  Y = MIN (Y, BLC(2))
               ELSE
                  CALL FNDY (TRC(2), DX, DY, JERR)
                  IF (JERR.NE.0) GO TO 80
                  CALL XYPIX (DX, DY, X, Y, JERR)
                  IF (Y.LT.TRC(2)-0.1) GO TO 80
                  Y = MAX (Y, TRC(2))
                  END IF
               END IF
            IF (JERR.NE.0) GO TO 80
C                                       Convert degrees to pixels.
            IF ((X.LT.LBLC(1)-0.01) .OR. (X.GT.LTRC(1)+0.01)) GO TO 80
            IF ((Y.LT.LBLC(2)-0.01) .OR. (Y.GT.LTRC(2)+0.01)) GO TO 80
            LX = (X + RX0)/XSCAL + 0.5
            LY = (Y + RY0)/YSCAL + 0.5
C                                       Find end of tic.
            DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX
            DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 80
            TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
            IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
               IF (TICT.LE.0.0) GO TO 80
               DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL/TICT
               DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL/TICT
               CALL XYPIX (DTX, DTY, XT, YT, JERR)
               IF (JERR.NE.0) GO TO 80
               END IF
            IF ((XT.LT.LBLC(1)-0.01) .OR. (XT.GT.LTRC(1)+0.01)) GO TO 80
            IF ((YT.LT.LBLC(2)-0.01) .OR. (YT.GT.LTRC(2)+0.01)) GO TO 80
C                                       Simple ticks
            IX(2) = (XT + RX0)/XSCAL + 0.5
            IY(2) = (YT + RY0)/YSCAL + 0.5
            IX(1) = LX
            IY(1) = LY
            CALL IMVECT ('ONNN', GRCHAN, 2, IX, IY, IBUFF, IERR)
            IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
 80         DEGC = DEGC - DEG
 90         CONTINUE
         END IF
      IERR = 0
C                                       Full curves from top?
      IF (DOACRS) THEN
         PRX = -1.E10
         PRY = -1.E10
C                                       Simple coordinates
         IF (AXTYP(LOCNUM).NE.1) THEN
            DEGC = DEGC0 + 5 * DEG0
            LOLIM = DEGC1 - 5 * DEG0
 100        DEGC = DEGC - DEG0
            IF ((DEGC.GT.LOLIM) .AND. (DEG0.LT.0.0D0)) GO TO 999
            IF ((DEGC.LT.LOLIM) .AND. (DEG0.GT.0.0D0)) GO TO 999
C                                       Find start
C                                       vertical full plot
            IF (IAXIS.EQ.2) THEN
               DY = DEGC
               CALL FNDX (BLC(1), DY, DX, JERR)
               IF (JERR.NE.0) GO TO 100
C                                       horizontal full plot
            ELSE
               DX = DEGC
               CALL FNDY (BLC(2), DX, DY, JERR)
               IF (JERR.NE.0) GO TO 100
               END IF
C                                       Convert to pixels.
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (JERR.NE.0) GO TO 100
            BX(1) = X
            BY(1) = Y
C                                       Find end
C                                       vertical full plot
            IF (IAXIS.EQ.2) THEN
               DY = DEGC
               CALL FNDX (TRC(1), DY, DX, JERR)
               IF (JERR.NE.0) GO TO 100
C                                       horizontal full plot
            ELSE
               DX = DEGC
               CALL FNDY (TRC(2), DX, DY, JERR)
               IF (JERR.NE.0) GO TO 100
               END IF
C                                       Convert to pixels.
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (JERR.NE.0) GO TO 100
            BX(2) = X
            BY(2) = Y
            CALL LINLIM (LBLC, LTRC, BX, BY, JERR)
            IF (JERR.EQ.0) THEN
               IX(1) = (BX(1) + RX0)/XSCAL + 0.5
               IY(1) = (BY(1) + RY0)/YSCAL + 0.5
               IX(2) = (BX(2) + RX0)/XSCAL + 0.5
               IY(2) = (BY(2) + RY0)/YSCAL + 0.5
               CALL IMVECT ('ONNN', GRCHAN, 2, IX, IY, IBUFF, IERR)
               IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
               END IF
            GO TO 100
C                                       celestial coordinates
         ELSE
            DEG0 = ABS(DEG0)
C                                       Find range of parameters
            NPOLE = .FALSE.
            IF (CORTYP(LOCNUM).EQ.1) THEN
               DY = 90.0D0
               DX = 0.0D0
            ELSE
               DY = 0.0D0
               DX = 90.0D0
               END IF
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (JERR.EQ.0) THEN
               NPOLE = (X.GE.LBLC(1)) .AND. (X.LE.LTRC(1)) .AND.
     *            (Y.GE.LBLC(2)) .AND. (Y.LE.LTRC(2))
               END IF
            SPOLE = .FALSE.
            IF (CORTYP(LOCNUM).EQ.1) THEN
               DY = -90.0D0
               DX = 0.0D0
            ELSE
               DY = 0.0D0
               DX = -90.0D0
               END IF
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (JERR.EQ.0) THEN
               SPOLE = (X.GE.LBLC(1)) .AND. (X.LE.LTRC(1)) .AND.
     *            (Y.GE.LBLC(2)) .AND. (Y.LE.LTRC(2))
               END IF
            XUPLIM = -400.
            XLOLIM = 400.
            UPLIM = -400.
            LOLIM = 400.
            I1 = LBLC(1) + 0.01
            I2 = LTRC(1) + 0.01
            J1 = LBLC(2) + 0.01
            J2 = LTRC(2) + 0.01
            X = I2
            Y = J1
            CALL XYVAL (X, Y, DX, DY, DZ, JERR)
            IF (JERR.EQ.0) THEN
               XUPLIM = MAX (XUPLIM, DX)
               XLOLIM = MIN (XLOLIM, DX)
               UPLIM = MAX (UPLIM, DY)
               LOLIM = MIN (LOLIM, DY)
               END IF
            Y = J2
            CALL XYVAL (X, Y, DX, DY, DZ, JERR)
            IF (JERR.EQ.0) THEN
               XUPLIM = MAX (XUPLIM, DX)
               XLOLIM = MIN (XLOLIM, DX)
               UPLIM = MAX (UPLIM, DY)
               LOLIM = MIN (LOLIM, DY)
               END IF
            X = I1
            CALL XYVAL (X, Y, DX, DY, DZ, JERR)
            IF (JERR.EQ.0) THEN
               XUPLIM = MAX (XUPLIM, DX)
               XLOLIM = MIN (XLOLIM, DX)
               UPLIM = MAX (UPLIM, DY)
               LOLIM = MIN (LOLIM, DY)
               END IF
            DO 210 J = J1,J2,5
               DO 200 I = I1,I2,5
                  X = I
                  Y = J
                  CALL XYVAL (X, Y, DX, DY, DZ, JERR)
                  IF (JERR.EQ.0) THEN
                     XUPLIM = MAX (XUPLIM, DX)
                     XLOLIM = MIN (XLOLIM, DX)
                     UPLIM = MAX (UPLIM, DY)
                     LOLIM = MIN (LOLIM, DY)
                     END IF
 200              CONTINUE
 210           CONTINUE
            IF (NPOLE) THEN
               IF (CORTYP(LOCNUM).EQ.1) THEN
                  UPLIM = 90.0D0
               ELSE
                  XUPLIM = 90.0D0
                  END IF
               END IF
            IF (SPOLE) THEN
               IF (CORTYP(LOCNUM).EQ.1) THEN
                  LOLIM = -90.0D0
               ELSE
                  XLOLIM = -90.0D0
                  END IF
               END IF
C                                       nearly full circle -> full
            IF (CORTYP(LOCNUM).EQ.1) THEN
               IF (XUPLIM-XLOLIM.GT.358.) THEN
                  XUPLIM = XUPLIM + 1.0
                  XLOLIM = XLOLIM - 1.0
                  END IF
            ELSE
               IF (UPLIM-LOLIM.GT.358.) THEN
                  UPLIM = UPLIM + 1.0
                  LOLIM = LOLIM - 1.0
                  END IF
               END IF
            DXLIM = MAX (1.2, 0.05*(LTRC(1)-LBLC(1)))
            DYLIM = MAX (1.2, 0.05*(LTRC(2)-LBLC(2)))
C                                       plot X
            IF (IAXIS.EQ.1) THEN
               XDT = DEG0
               YDT = MIN (1.0, 0.01*(LTRC(2)-LBLC(2))) *
     *            ABS (AXINC(2,LOCNUM))
               I = (XUPLIM + DEG0) / DEG0
               XUPLIM = I * DEG0
               I = (XLOLIM - DEG0) / DEG0
               XLOLIM = I * DEG0
               IF (CORTYP(LOCNUM).EQ.2) THEN
                  IF (XUPLIM.GT.90.0D0) XUPLIM = XUPLIM - DEG0
                  IF (XUPLIM.GT.90.0D0) XUPLIM = XUPLIM - DEG0
                  IF (XLOLIM.LT.-90.0D0) XLOLIM = XLOLIM + DEG0
                  IF (XLOLIM.LT.-90.0D0) XLOLIM = XLOLIM + DEG0
                  END IF
               UPLIM = UPLIM + 5*YDT
               LOLIM = LOLIM - 5*YDT
C                                       plot loop main axis
               DX = XLOLIM - XDT
 220           UP = .TRUE.
               DX = DX + XDT
               IF (DX.LE.XUPLIM) THEN
                  DY = LOLIM - YDT
 230              DY = DY + YDT
                  IF (DY.LE.UPLIM) THEN
                     CALL XYPIX (DX, DY, X, Y, JERR)
                     IF (JERR.NE.0) THEN
                        UP = .TRUE.
                     ELSE IF ((X.GE.LBLC(1)) .AND. (X.LE.LTRC(1)) .AND.
     *                  (Y.GE.LBLC(2)) .AND. (Y.LE.LTRC(2))) THEN
                        IF ((ABS(X-PRX).GT.0.05*(LTRC(1)-LBLC(1))) .OR.
     *                     (ABS(Y-PRY).GT.0.05*(LTRC(2)-LBLC(2)))) UP =
     *                     .TRUE.
                        IF (UP) THEN
                           IX(2) = (X + RX0)/XSCAL + 0.5
                           IY(2) = (Y + RY0)/YSCAL + 0.5
                           UP = .FALSE.
                        ELSE
                           IX(1) = IX(2)
                           IY(1) = IY(2)
                           IX(2) = (X + RX0)/XSCAL + 0.5
                           IY(2) = (Y + RY0)/YSCAL + 0.5
                           CALL IMVECT ('ONNN', GRCHAN, 2, IX, IY,
     *                        IBUFF, IERR)
                           IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                           END IF
                        PRX = X
                        PRY = Y
                     ELSE
                        UP = .TRUE.
                        END IF
                     GO TO 230
                     END IF
                  GO TO 220
                  END IF
C                                       plot Y
            ELSE
               I = (UPLIM + DEG0) / DEG0
               UPLIM = I * DEG0
               I = (LOLIM - DEG0) / DEG0
               LOLIM = I * DEG0
               IF (CORTYP(LOCNUM).EQ.1) THEN
                  IF (UPLIM.GT.90.0D0) UPLIM = UPLIM - DEG0
                  IF (UPLIM.GT.90.0D0) UPLIM = UPLIM - DEG0
                  IF (LOLIM.LT.-90.0D0) LOLIM = LOLIM + DEG0
                  IF (LOLIM.LT.-90.0D0) LOLIM = LOLIM + DEG0
                  END IF
               XDT = MIN (1.0, 0.01*(LTRC(1)-LBLC(1))) *
     *            ABS (AXINC(1,LOCNUM))
               YDT = DEG0
               XUPLIM = XUPLIM + 5*XDT
               XLOLIM = XLOLIM - 5*XDT
C                                       plot loop main axis
               DY = LOLIM - YDT
 240           UP = .TRUE.
               DY = DY + YDT
               IF (DY.LE.UPLIM) THEN
                  DX = XLOLIM - XDT
 250              DX = DX + XDT
                  IF (DX.LE.XUPLIM) THEN
                     CALL XYPIX (DX, DY, X, Y, JERR)
                     IF (JERR.NE.0) THEN
                        UP = .TRUE.
                     ELSE IF ((X.GE.LBLC(1)) .AND. (X.LE.LTRC(1)) .AND.
     *                     (Y.GE.LBLC(2)) .AND. (Y.LE.LTRC(2))) THEN
                        IF ((ABS(X-PRX).GT.0.05*(LTRC(1)-LBLC(1))) .OR.
     *                     (ABS(Y-PRY).GT.0.05*(LTRC(2)-LBLC(2)))) UP =
     *                     .TRUE.
                        IF (UP) THEN
                           IX(2) = (X + RX0)/XSCAL + 0.5
                           IY(2) = (Y + RY0)/YSCAL + 0.5
                           UP = .FALSE.
                        ELSE
                           IX(1) = IX(2)
                           IY(1) = IY(2)
                           IX(2) = (X + RX0)/XSCAL + 0.5
                           IY(2) = (Y + RY0)/YSCAL + 0.5
                           CALL IMVECT ('ONNN', GRCHAN, 2, IX, IY,
     *                        IBUFF, IERR)
                           IF ((IERR.NE.0) .AND. (IERR.NE.2)) GO TO 980
                           END IF
                        PRX = X
                        PRY = Y
                     ELSE
                        UP = .TRUE.
                        END IF
                     GO TO 250
                     END IF
                  GO TO 240
                  END IF
               END IF
            END IF
         END IF
      GO TO 999
C                                       Graph drawing error.
 980  WRITE (MSGTXT,1980) IERR
      CALL MSGWRT (7)
      IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('ITICS: GRAPH LABEL WRITING ERROR. IERR =',I5)
      END
