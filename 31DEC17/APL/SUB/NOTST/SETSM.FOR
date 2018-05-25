      SUBROUTINE SETSM (IRET)
C-----------------------------------------------------------------------
C! Determines type of spectral smoothing and sets up look up table.
C# UV Spectral Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004
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
C   SETSM determines the type of spectral smoothing to be applied and
C   sets up the look up table to do it. The actual smoothing is done in
C   routine SMOSP
C   Inputs: (via common)
C      SMOOTH   R(3)   Array containing smoothing parms
C                         SMOOTH(1) = type of function
C                               (2) = width of function in channels
C                               (3) = support of function in channels
C                       Type of function supported are:
C                          0 => no smoothing
C                          1 => hanning
C                          2 => gaussian
C                          3 => boxcar
C                          4 => sin(x)/x
C   Output:
C     IRET           I     Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   I, N, LSPECT, IROUND, IT, SUPRAD, LT
      REAL      FX, X, W, WIDTHS(4), SUPS(4)
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA WIDTHS /4.0, 2.0, 2.0, 3.0/
      DATA SUPS /1.0, 3.0, 1.0, 4.0/
C-----------------------------------------------------------------------
      IRET = 0
      DOSMTH = 0
      IT = IROUND (SMOOTH(1))
      IF (IT.LE.0) GO TO 999
      IF (IT.GT.8) IT = 1
      DOSMTH = (IT + 3) / 4
      LT = MOD (IT-1, 4) + 1
C                                       Convolution: parms & tables
      SMOOTH(1) = IT
      LSPECT = MAX (12, CATUV(KINAX+JLOCF))
      IF ((SMOOTH(2).LT.0.5) .OR. (SMOOTH(2).GT.LSPECT/3.))
     *   SMOOTH(2) = WIDTHS(LT)
      IF ((SMOOTH(3).GT.4.*SUPS(LT)*SMOOTH(2)) .OR.
     *   (SMOOTH(3).LT.SMOOTH(2)))SMOOTH(3) = SUPS(LT) * SMOOTH(2)
      SUPRAD = SMOOTH(3) / 2.0 + 0.1
      IF (SUPRAD+1.GT.MAXSMO) THEN
         SUPRAD = MAXSMO - 1
         SMOOTH(2) = (2. * SUPRAD) / SUPS(LT)
         END IF
      SMOOTH(3) = 2.0 * SUPRAD + 1.0
C                                       Channel ranges for smoothing
      BCHANS = MAX (1, BCHAN - SUPRAD)
      ECHANS = MIN (CATUV(KINAX+JLOCF), ECHAN + SUPRAD)
      CALL RFILL (MAXSMO, 0.0, SMTAB)
      N = 1 + SUPRAD
      FX = 2.0 / SMOOTH(2)
      SMTAB(1) = 1.0
C                                       Compute look-up tables
      W = SMTAB(1)
C                                       Hanning smooth
      IF (LT.EQ.1) THEN
         DO 20 I = 2,N
            X = I - 1.0
            SMTAB(I) = MAX (0.0, 1.0-FX*X)
            W = W + 2 * SMTAB(I)
 20         CONTINUE
C                                       Gaussian smooth
      ELSE IF (LT.EQ.2) THEN
         FX = -LOG(2.0) * FX * FX
         DO 30 I = 2,N
            X = I - 1.0
            SMTAB(I) = EXP (FX * X * X)
            W = W + 2 * SMTAB(I)
 30         CONTINUE
C                                       Boxcar smooth
      ELSE IF (LT.EQ.3) THEN
         FX = 1.0 / FX
         DO 40 I = 2,N
            X = I - 1.0
            IF (X.LT.FX) THEN
               SMTAB(I) = 1.0
            ELSE IF (X.EQ.FX) THEN
               SMTAB(I) = 0.5
               END IF
            W = W + 2 * SMTAB(I)
 40         CONTINUE
C                                      Sinc smooth
      ELSE IF (LT.EQ.4) THEN
         FX = 3.14159 * FX
         DO 50 I = 2,N
            X = (I - 1.0) * FX
            SMTAB(I) = SIN(X) / X
            W = W + 2 * SMTAB(I)
 50         CONTINUE
         END IF
C                                       Normalize integral
      IF (W.LE.0.0) W = 1.0
      DO 70 I = 1,N
         SMTAB(I) = SMTAB(I) / W
 70      CONTINUE
C
 999  RETURN
      END
