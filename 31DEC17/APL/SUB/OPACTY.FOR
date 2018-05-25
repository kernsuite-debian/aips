      SUBROUTINE OPACTY (IBAND, TSURF, TDEW, PRESS, WT, MJD, AVGIAT,
     *   NH2O, OPAC)
C-----------------------------------------------------------------------
C! Calculate opacity averaging surface weather opacity and seasonal
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 2002, 2007
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
C   Routine to calculate estimates of opacity
C   Input:
C      IBAND    I   VLA band, 1->4, 2->P, ..., 8->Q
C      TSURF    R   Surface ambient temp (C)
C      TDEW     R   Surface dewpoint temp (C)
C      PRESS    R   Surface pressure in millibars
C      WT       R   Weight to give weather (0 -> 1), 1-WT given seasonal
C      MJD      I   Current Modified Julian day
C      AVGIAT   D   Average IAT after MJD
C   Output:
C      NH2O     R   H2O column density  (requires WT > 0)
C      OPAC     R   Zenith opacity
C   Function to calculate day of year from month, day, year from Meeus'
C   book on Astronomical Algorithms.
C-----------------------------------------------------------------------
      INTEGER   IBAND, MJD
      REAL      TSURF, TDEW, PRESS, WT, NH2O, OPAC
      DOUBLE PRECISION AVGIAT
C
      REAL      KK, MVAP, WATDEN, VAPH, CONST, WOPAC, SOPAC
      PARAMETER (KK = 1.380658E-23)
      PARAMETER (MVAP = 18 * 1.6749286E-27)
      PARAMETER (WATDEN = 1000.0)
C                                       VAPH is the scale height of H2O
C                                       vapor
      PARAMETER (VAPH = 1500.0)
      PARAMETER (CONST = 1.0E5*MVAP*VAPH/(WATDEN*KK))
      REAL      PVAP, C(3,10), MDOY, D(3,10)
      INTEGER   IDATE(6), DOY, MONTH, DAY, YEAR, K2
      LOGICAL   ISLEAP, FIRST
      EQUIVALENCE (YEAR, IDATE(1)), (MONTH, IDATE(2)), (DAY, IDATE(3))
      INCLUDE 'INCS:DMSG.INC'
      SAVE FIRST
      DATA FIRST /.TRUE./
C                                       4-band
      DATA C /  0.0,  0.0,  0.0,
C                                       P-band
     *          0.0,  0.0,  0.0,
C                                       L-band
     *          1.0,  0.0,  0.0,
C                                       S-band
     *          1.0,  0.0,  0.0,
C                                       C-band
     *          1.0,  0.0,  0.0,
C                                       X-band
     *          1.0,  0.0,  0.0,
C                                       U-band
     *          1.19, 0.0578, -2.486E-4,
C                                       K-band
     *          1.77, 0.9060, -1.385E-4,
C                                       A-band   copy U for now
     *          1.19, 0.0578, -2.486E-4,
C                                       Q-band
     *          5.86, 0.1712,  9.201E-4 /
C                                       4-band
      DATA D /  0.0,  0.0,  0.0,
C                                       P-band
     *          0.0,  0.0,  0.0,
C                                       L-band
     *          1.0,  0.0,  0.0,
C                                       S-band
     *          1.0,  0.0,  0.0,
C                                       C-band
     *          1.0,  0.0,  0.0,
C                                       X-band
     *          1.0,  0.0,  0.0,
C                                       U-band
     *          2.19, -0.0085, 2.010E-5,
C                                       K-band
     *         22.12, -0.1783, 4.402E-4,
C                                       A-band   copy U for now
     *          2.19, -0.0085, 2.010E-5,
C                                       Q-band
     *          9.70, -0.0357, 9.402E-5 /
C-----------------------------------------------------------------------
      IF (IBAND.EQ.9) THEN
         MSGTXT = 'OPACTY: HAS A BAND BEEN MEASURED YET?  USING U BAND'
         IF (FIRST) CALL MSGWRT (7)
         FIRST = .FALSE.
      ELSE
         FIRST = .TRUE.
         END IF
C                                       weather opacity
      IF (WT.GT.0.0) THEN
         CALL SATPRS (TDEW, PRESS, PVAP)
         NH2O = CONST * PVAP / (TSURF + 273.15)
         WOPAC = (C(1,IBAND) + C(2,IBAND)*NH2O + C(3,IBAND)*NH2O*NH2O) /
     *      100.0
      ELSE
         WOPAC = 0.0
         END IF
C                                       seasonal opacity
      IF (WT.LT.1.0) THEN
         CALL JD2DAT (MJD+2400000.5D0, IDATE)
         ISLEAP = (MOD(YEAR,4).EQ.0) .AND. ((MOD(YEAR,100).NE.0) .OR.
     *      (MOD(YEAR,400).EQ.0))
         IF (ISLEAP) THEN
            K2 = 1
         ELSE
            K2 = 2
            END IF
         DOY = ((275 * MONTH) / 9) - K2 * ((MONTH + 9) / 12) + DAY - 30
         MDOY = DOY
         IF (MDOY.GE.200.0) MDOY = MDOY - 365.0
         MDOY = MDOY + 165.0
         MDOY = MDOY + AVGIAT/24.0
         SOPAC = (D(1,IBAND) + D(2,IBAND)*MDOY + D(3,IBAND)*MDOY*MDOY) /
     *      100.0
      ELSE
         SOPAC = 0.0
         END IF
      OPAC = MAX (0.0, MIN (1.0, WT)) * WOPAC +
     *   MAX (0.0, MIN (1.0, 1.0-WT)) * SOPAC
C
 999  RETURN
      END
