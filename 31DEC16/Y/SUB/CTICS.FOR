      SUBROUTINE CTICS (LAXIS, BLC, TRC, XYRATO, YX, DOACRS, IBUFF,
     *   IERR)
C-----------------------------------------------------------------------
C! writes tick marks and tick labels to a plot file
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998-2003, 2013-2014
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
C   CTICS writes tick marks and tick labels to a plot file.
C   If CPREF(IAX) and CTYP(IAX) are all blank, no tick labels are done.
C   Inputs:
C      LAXIS   I        1 => horizontal,  2 => vertical full plots
C                       3 => horiz subplot 4 => vertical subplot
C      BLC     R(2)     X and Y pixels to form bottom left hand
C                       corner of the graph.
C      TRC     R(2)     X and Y pixels to form the top right hand
C                       corner of the graph.
C      XYRATO  R        X to Y scaling factor
C      YX      D        LAXIS=3: plot x axis at y = YX; 4: plot y axis
C                       at x = YX - out range => BLC(1,2) value
C      DOACRS  L        Do coordinate grid (T) or just ticks (F)
C   In/out:
C      IBUFF   I(256)   buffer being used for output to
C                       the graphics file.
C   Outputs:
C      IERR    I        error code: 0 => ok
C                                   1 => bad IAXIS
C                                   2 => graph drawing error
C                                   3 => tic algorithm fails
C-----------------------------------------------------------------------
      INTEGER   LAXIS, IBUFF(256), IERR
      REAL      BLC(2), TRC(2), XYRATO
      DOUBLE PRECISION YX
      LOGICAL   DOACRS
C
      CHARACTER SPRTXT*80, SUBR*6, CHDL*4
      DOUBLE PRECISION DEG, DEGC, DTX, DX, DTY, DY, PT5SEC, TICX, TICY,
     *   DEGC0, DEG0, XDT, YDT, DEGC1, LDX, LDY, LLDX, LLDY, UPLIM,
     *   LOLIM, AYX, XUPLIM, XLOLIM, DZ
      REAL      DCX, DSP, X, Y, TICT, DCY, XT, YT, TICL, BX(2), BY(2),
     *   PRX, PRY, DXLIM, DYLIM, LBLC(2), LTRC(2)
      INTEGER   HML(2), IAXIS, AXISTP, I, IANGL, ILEN, ITRY, COOTYP,
     *   INOI, JERR, KERR, I1,I2, J1, J2, J
      LOGICAL   NONUM, FIRST, UP, NPOLE, SPOLE
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IAXIS = LAXIS
      IF (LAXIS.GT.2) IAXIS = IAXIS - 2
      LBLC(1) = BLC(1) - 0.5
      LBLC(2) = BLC(2) - 0.5
      LTRC(1) = TRC(1) + 0.5
      LTRC(2) = TRC(2) + 0.5
C                                       Assign initial values.
      IANGL = 0
C                                       Find value at BLC.
C                                       vertical
      IF (IAXIS.EQ.2) THEN
         AYX = YX
         CALL TICINC (LAXIS, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, KERR)
         IF (KERR.NE.0) THEN
            IF (KERR.GT.0) IERR = 3
            GO TO 999
            END IF
         DCX = -1.0
         DCY = -0.5
         AXISTP = LABTYP(LOCNUM) / 10
C                                       horizontal
      ELSE IF (IAXIS.EQ.1) THEN
         DCX = 0.5
         DCY = -1.5
         AYX = YX
         CALL TICINC (LAXIS, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI,
     *      TICX, TICY, TICL, PT5SEC, ITRY, KERR)
         IF (KERR.NE.0) THEN
            IF (KERR.GT.0) IERR = 3
            GO TO 999
            END IF
         AXISTP = MOD (LABTYP(LOCNUM), 10)
      ELSE
         GO TO 990
         END IF
C                                       Determine possible tic intervls
      NONUM = (CPREF(IAXIS,LOCNUM).EQ.' ') .AND.
     *   (CTYP(IAXIS,LOCNUM).EQ.' ')
      COOTYP = 2
      IF ((AXISTP.EQ.5) .OR. (AXISTP.GE.7)) COOTYP = 1
      LDX = RPVAL(1,LOCNUM)
      LLDX = RPVAL(1,LOCNUM)
      LDY = RPVAL(2,LOCNUM)
      LLDY = RPVAL(2,LOCNUM)
C                                       Draw tic marks and values.
      HML(1) = -32000
      HML(2) = -32000
      CHDL = '$$$$'
      DEGC0 = DEGC
      FIRST = .TRUE.
      DEG0 = DEG
      DEGC1 = DEGC0 - DEG
      DO 50 I = 1,INOI
C                                       vertical full plot
         IF (LAXIS.EQ.2) THEN
            DY = DEGC
            CALL FNDX (BLC(1), DY, DX, JERR)
            IF (JERR.NE.0) GO TO 40
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (X.GT.BLC(1)+0.1) GO TO 40
            X = MIN (X, BLC(1))
C                                       horizontal full plot
         ELSE IF (LAXIS.EQ.1) THEN
            DX = DEGC
            CALL FNDY (BLC(2), DX, DY, JERR)
            IF (JERR.NE.0) GO TO 40
            CALL XYPIX (DX, DY, X, Y, JERR)
            IF (Y.GT.BLC(2)+0.1) GO TO 40
            Y = MIN (Y, BLC(2))
C                                       vertical subplot
         ELSE IF (LAXIS.EQ.4) THEN
            DY = DEGC
            DX = AYX
            CALL XYPIX (DX, DY, X, Y, JERR)
C                                       horizontal subplot
         ELSE
            DX = DEGC
            DY = AYX
            CALL XYPIX (DX, DY, X, Y, JERR)
            END IF
         IF (JERR.NE.0) GO TO 40
C                                       Convert degrees to pixels.
         IF ((X.LT.LBLC(1)-0.01) .OR. (X.GT.LTRC(1)+0.01)) GO TO 40
         IF ((Y.LT.LBLC(2)-0.01) .OR. (Y.GT.LTRC(2)+0.01)) GO TO 40
         IF (DX.NE.LDX) LLDX = LDX
         IF (DY.NE.LDY) LLDY = LDY
         LDX = DX
         LDY = DY
         CALL GPOS (X, Y, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
C                                       Find end of tic.
         DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX
         DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY
         CALL XYPIX (DTX, DTY, XT, YT, JERR)
         IF (JERR.NE.0) GO TO 20
         TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
         IF (TICL.LE.0.) TICL = 1.
         IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
            IF (TICT.EQ.0.0) GO TO 20
            DTX = DX + SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL / TICT
            DTY = DY + SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL / TICT
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 20
            END IF
         IF ((XT.LT.LBLC(1)-0.01) .OR. (XT.GT.LTRC(1)+0.01)) GO TO 20
         IF ((YT.LT.LBLC(2)-0.01) .OR. (YT.GT.LTRC(2)+0.01)) GO TO 20
C                                       Simple tick: even with DOACROS
C                                       Position at end of tic.
         CALL GPOS (XT, YT, IBUFF, IERR)
         SUBR = 'GPOS'
         IF (IERR.NE.0) GO TO 980
C                                       Draw back to border.
         CALL GVEC (X, Y, IBUFF, IERR)
         SUBR = 'GVEC'
         IF (IERR.NE.0) GO TO 980
         DEGC1 = DEGC - DEG
         IF (FIRST) DEGC0 = DEGC
         FIRST = .FALSE.
C                                       Convert degrees to DEC/RA.
 20      IF (.NOT.NONUM) THEN
C                                       labels offset
            IF ((LAXIS.EQ.3) .OR. (LAXIS.EQ.4)) THEN
               IF (LAXIS.EQ.3) CALL FNDY (BLC(2), DX, DY, JERR)
               IF (LAXIS.EQ.4) CALL FNDX (BLC(1), DY, DX, JERR)
               IF (JERR.NE.0) GO TO 40
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (JERR.NE.0) GO TO 40
               END IF
C                                       Position for labels
            CALL GPOS (X, Y, IBUFF, IERR)
            SUBR = 'GPOS'
            IF (IERR.NE.0) GO TO 980
C                                       label string
            CALL TICSTR (ITRY, DEGC, PT5SEC, AXISTP, COOTYP, CHDL, HML,
     *         SPRTXT, ILEN)
            DSP = DCX - ILEN
            CALL GCHAR (ILEN, IANGL, DSP, DCY, SPRTXT, IBUFF, IERR)
            SUBR = 'GCHAR'
            IF (IERR.NE.0) GO TO 980
            END IF
 40      DEGC = DEGC - DEG
 50      CONTINUE
C                                       No top/right in subplots
      IF (((LAXIS.EQ.1) .OR. (LAXIS.EQ.2)) .AND. (.NOT.DOACRS)) THEN
C                                       Draw tics for other side.
C                                       Same intervals but not
C                                       necessarily same values.
         IF (IAXIS.NE.1) THEN
            CALL TICINC (6, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI,
     *         TICX, TICY, TICL, PT5SEC, ITRY, KERR)
C                                       horizontal
         ELSE
            CALL TICINC (5, LBLC, LTRC, XYRATO, AYX, DEGC, DEG, INOI,
     *         TICX, TICY, TICL, PT5SEC, ITRY, KERR)
            END IF
         IF (KERR.NE.0) INOI = 0
C                                       Loop for other border.
         DO 90 I= 1,INOI
            IF (IAXIS.NE.1) THEN
               DY = DEGC
               CALL FNDX (TRC(1), DY, DX, JERR)
               IF (JERR.NE.0) GO TO 80
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (X.LT.TRC(1)-0.1) GO TO 80
               X = MAX (X, TRC(1))
            ELSE
               DX = DEGC
               CALL FNDY (TRC(2), DX, DY, JERR)
               IF (JERR.NE.0) GO TO 80
               CALL XYPIX (DX, DY, X, Y, JERR)
               IF (Y.LT.TRC(2)-0.1) GO TO 80
               Y = MAX (Y, TRC(2))
               END IF
            IF (JERR.NE.0) GO TO 80
C                                       Convert degrees to pixels.
            IF ((X.LT.LBLC(1)-0.01) .OR. (X.GT.LTRC(1)+0.01)) GO TO 80
            IF ((Y.LT.LBLC(2)-0.01) .OR. (Y.GT.LTRC(2)+0.01)) GO TO 80
C                                       Find end of tic.
            DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX
            DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY
            CALL XYPIX (DTX, DTY, XT, YT, JERR)
            IF (JERR.NE.0) GO TO 80
            TICT = SQRT ((XT-X)**2 + (YT-Y)**2)
            IF ((TICT.GT.TICL) .OR. (TICT.LT.0.1*TICL)) THEN
               IF (TICT.EQ.0.0) GO TO 80
               DTX = DX - SIGN (1.0, AXINC(1,LOCNUM)) * TICX * TICL/TICT
               DTY = DY - SIGN (1.0, AXINC(2,LOCNUM)) * TICY * TICL/TICT
               CALL XYPIX (DTX, DTY, XT, YT, JERR)
               IF (JERR.NE.0) GO TO 80
               END IF
            IF ((XT.LT.LBLC(1)-0.01) .OR. (XT.GT.LTRC(1)+0.01)) GO TO 80
            IF ((YT.LT.LBLC(2)-0.01) .OR. (YT.GT.LTRC(2)+0.01)) GO TO 80
C                                       Simple ticks only
C                                       Position at end of tic.
            CALL GPOS (XT, YT, IBUFF, IERR)
            SUBR = 'GPOS'
            IF (IERR.NE.0) GO TO 980
C                                       Draw back to border.
            CALL GVEC (X, Y, IBUFF, IERR)
            SUBR = 'GVEC'
            IF (IERR.NE.0) GO TO 980
 80         DEGC = DEGC - DEG
 90         CONTINUE
         END IF
C                                       Full curves from top?
      IF ((DOACRS) .AND. (LAXIS.LE.2)) THEN
C                                       Simple coordinates
         PRX = -1.E10
         PRY = -1.E10
         IF (AXTYP(LOCNUM).NE.1) THEN
            DEGC = DEGC0 + 5 * DEG0
            LOLIM = DEGC1 - 5 * DEG0
 100        DEGC = DEGC - DEG0
            IF ((DEGC.GT.LOLIM) .AND. (DEG0.LT.0.0D0)) GO TO 999
            IF ((DEGC.LT.LOLIM) .AND. (DEG0.GT.0.0D0)) GO TO 999
C                                       Find start
C                                       vertical full plot
            IF (LAXIS.EQ.2) THEN
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
            IF (LAXIS.EQ.2) THEN
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
            CALL PLOTWO (LBLC, LTRC, BX, BY, PRX, PRY, .FALSE., .FALSE.,
     *         IBUFF, JERR)
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
                        IF ((ABS(X-PRX).GT.DXLIM) .OR.
     *                     (ABS(Y-PRY).GT.DYLIM)) UP = .TRUE.
                        IF (UP) THEN
                           CALL GPOS (X, Y, IBUFF, JERR)
                           UP = .FALSE.
                        ELSE
                           CALL GVEC (X, Y, IBUFF, JERR)
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
                        IF ((ABS(X-PRX).GT.DXLIM) .OR.
     *                     (ABS(Y-PRY).GT.DYLIM)) UP = .TRUE.
                        IF (UP) THEN
                           CALL GPOS (X, Y, IBUFF, JERR)
                           UP = .FALSE.
                        ELSE
                           CALL GVEC (X, Y, IBUFF, JERR)
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
 980  WRITE (MSGTXT,1980) IERR, SUBR
      CALL MSGWRT (7)
      IERR = 2
      GO TO 999
C                                       Invalid axis type.
 990  WRITE (MSGTXT,1990)
      CALL MSGWRT (8)
      IERR = 1
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('CTICS: GRAPH LABEL WRITING ERROR',I5,' FROM ',A)
 1990 FORMAT ('CTICS: INVALID AXIS TYPE.')
      END
