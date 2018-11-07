      SUBROUTINE PLTBSZ (BMCORN, BLC, TRC, CATR, BBLC, BTRC)
C-----------------------------------------------------------------------
C! Find size of box for beam plot
C# Map Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 2004
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
C   PLTBEM plots the half-power beam if available and if it will fit.
C   Inputs:
C      BMCORN   I        Code indicating where to put the plot
C                           0 or 5: in the middle of the pane
C                                   10    darker
C                           1: lower left-hand corner  6 darker
C                           2: lower right-hand corner 7 darker
C                           3: upper left-hand corner  8 darker
C                           4: upper right-hand corner 9 darker
C      CATR     R(256)   Map header.
C   Output:
C      BBLC     I(2)     Beam box BLC
C      BTRC     I(2)     Beam box TRC
C      IERR     I        Error code. 0: success, > 0 from plot routines
C                            -1: HPBW not defined
C                            -2: plot too big
C-----------------------------------------------------------------------
      INTEGER   BBLC(2), BTRC(2), BMCORN
      REAL      CATR(256), BLC(7), TRC(7)
C
      INTEGER   I, LCORN, JERR
      REAL      CP, CPA, D2R, DM,  PHI, SP, SPA, X, X0, XM, Y, DX, DY,
     *   Y0, YM, XC, YC, BMAJ, BMIN, BEAMPA, DMX, DMY
      DOUBLE PRECISION RA, DEC, DRA, RAC, DECC
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DLOC.INC'
      PARAMETER (D2R = 3.14159265/180.0)
C-----------------------------------------------------------------------
      BBLC(1) = 0.0
      BTRC(1) = 0.0
      BBLC(2) = 0.0
      BTRC(2) = 0.0
      IF (AXTYP(LOCNUM).NE.1) GO TO 999
      IF ((LABTYP(LOCNUM).NE.56) .AND. (LABTYP(LOCNUM).NE.65)) GO TO 999
      LCORN = MOD (BMCORN, 5)
C
      BMAJ = ABS (CATR(KRBMJ)) / 2.0
      BMIN = ABS (CATR(KRBMN)) / 2.0
      BEAMPA = CATR(KRBPA) * D2R
      IF ((BMAJ.LT.1.E-9) .OR. (BMIN.LT.1.E-9)) GO TO 999
C                                       find coordinate info
      XC = (BLC(1) + TRC(1)) / 2.0
      YC = (BLC(2) + TRC(2)) / 2.0
      IF (CORTYP(LOCNUM).EQ.1) THEN
         CALL XYVAL (XC, YC, RAC, DECC, DRA, JERR)
      ELSE
         CALL XYVAL (XC, YC, DECC, RAC, DRA, JERR)
         END IF
      IF (JERR.NE.0) GO TO 999
C                                       Compute the extrema of X and Y
      CPA = COS (BEAMPA)
      SPA = SIN (BEAMPA)
      DRA = COS (DECC * D2R)
      IF (DRA.EQ.0.0) DRA = 1.0
      XM = 0.
      YM = 0.
      DO 10 I = 0,360
         PHI = I * D2R
         CP = BMIN*COS(PHI)
         SP = BMAJ*SIN(PHI)
         X  = CP*CPA - SP*SPA
         Y  = CP*SPA + SP*CPA
         RA = RAC - X / DRA
         DEC = DECC + Y
         IF (CORTYP(LOCNUM).EQ.1) THEN
            CALL XYPIX (RA, DEC, X, Y, JERR)
         ELSE
            CALL XYPIX (DEC, RA, X, Y, JERR)
            END IF
         IF (JERR.NE.0) GO TO 999
         XM = MAX (XM, ABS (X-XC))
         YM = MAX (YM, ABS (Y-YC))
 10      CONTINUE
C                                       Compute scaling
      DX = 1.0
      DY = 1.0
      IF (CATR(KRCIC).NE.0.0) DX = 1.0 / ABS(CATR(KRCIC))
      IF (CATR(KRCIC+1).NE.0.0) DY = 1.0 / ABS(CATR(KRCIC+1))
C                                       Define the origin
      DM = (MAX (XM, YM) + XM + YM) * 0.1
      IF (DX.LT.DY) THEN
         DMX = DM * DX/DY
         DMY = DM
      ELSE
         DMX = DM
         DMY = DM * DY/DX
         END IF
      IF (XM.GT.YM) THEN
         XM = XM * 1.2 + 0.4 * YM
         YM = YM * 1.6
      ELSE
         YM = YM * 1.2 + 0.4 * XM
         XM = XM * 1.6
         END IF
      IF (LCORN.EQ.0) THEN
         X0 = BLC(1) + (TRC(1) - BLC(1))/2.0
         Y0 = BLC(2) + (TRC(2) - BLC(2))/2.0
      ELSE IF (LCORN.EQ.1) THEN
         X0 = BLC(1) + (XM + DMX)
         Y0 = BLC(2) + (YM + DMY)
      ELSE IF (LCORN.EQ.2) THEN
         X0 = TRC(1) - (XM + DMX)
         Y0 = BLC(2) + (YM + DMY)
      ELSE IF (LCORN.EQ.3) THEN
         X0 = TRC(1) - (XM + DMX)
         Y0 = TRC(2) - (YM + DMY)
      ELSE IF (LCORN.EQ.4) THEN
         X0 = BLC(1) + (XM + DMX)
         Y0 = TRC(2) - (YM + DMY)
         END IF
      IF ((X0-XM.LT.BLC(1)) .OR. (X0+XM.GT.TRC(1)) .OR.
     *   (Y0-YM.LT.BLC(2)) .OR. (Y0+YM.GT.TRC(2))) GO TO 999
C                                       Draw a box around the beam
      BBLC(1) = X0 - XM + 0.02
      BTRC(1) = X0 + XM + 0.98
      BBLC(2) = Y0 - YM + 0.02
      BTRC(2) = Y0 + YM + 0.98
C
 999  RETURN
      END
