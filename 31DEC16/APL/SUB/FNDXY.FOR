      SUBROUTINE FNDXY (RANDP, BUFR, IAD, TYPCOR, TYPEAX, FRQSCL, XY)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   FNDXY extracts the desired X and Y values from the Vis record.
C   Inputs:  RANDP  R(*)     Random parameters
C            BUFR   R(*)     Visibility record
C            IAD    I        Address in BUFR for first desired corr.
C            TYPCOR I        Type of Correlation record
C            TYPEAX I(*)     Array of Axis types (valid values 1-12)
C            FRQSCL R        Frequency Scale
C   Outputs: XY     R(2)     X, Y values
C-----------------------------------------------------------------------
      REAL      RANDP(*), BUFR(*), XY(2), FRQSCL
      INTEGER   IAD, TYPCOR, TYPEAX(*)
C                                      Epsilon is 1/5 second (in days)
      REAL      EPSLN
      PARAMETER (EPSLN=2.0E-6)
      REAL      TR, TI, R2D, LASTT, T
      INTEGER   NV, I, J, LASTSC
      INCLUDE 'INCS:DUVH.INC'
      SAVE LASTT, LASTSC
      DATA R2D /57.29578/, LASTT/0.0/, LASTSC/0/
C-----------------------------------------------------------------------
C                                       Loop over axes
      DO 900 I = 1,2
         NV = 0
         TR = 0.0
         TI = 0.0
         XY(I) = 0.0
         J = TYPEAX(I)
         GO TO (10, 10, 30, 40, 50, 60, 70, 80, 10, 10, 50, 120) J
C                                       amplitudes and phases
 10     CONTINUE
C                                       calc amplitude based on
C                                       Correlator Type
            IF (TYPCOR.LE.4) THEN
               TR = BUFR(IAD)
               TI = BUFR(IAD+1)
               END IF
            IF (TYPCOR.EQ.5) THEN
               IF (BUFR(IAD+2).GT.0.0) THEN
                  TR = BUFR(IAD)
                  TI = BUFR(IAD+1)
                  NV = NV + 1
                  END IF
               IF (BUFR(IAD+2+INCS).GT.0.0) THEN
                  TR = TR + BUFR(IAD+INCS)
                  TI = TI + BUFR(IAD+1+INCS)
                  NV = NV + 1
                  END IF
               TR = TR / NV
               TI = TI / NV
               END IF
            IF (TYPCOR.EQ.6) THEN
               TR = (BUFR(IAD) - BUFR(IAD+INCS)) / 2.0
               TI = (BUFR(IAD+1) - BUFR(IAD+INCS+1)) / 2.0
               END IF
            IF (TYPCOR.EQ.7) THEN
               TR = (BUFR(IAD) + BUFR(IAD+INCS)) / 2.0
               TI = (BUFR(IAD+1) + BUFR(IAD+INCS+1)) / 2.0
               END IF
            IF (TYPCOR.EQ.8) THEN
               TR = (BUFR(IAD+1) - BUFR(IAD+INCS+1)) / 2.0
               TI = (BUFR(IAD+INCS) - BUFR(IAD)) / 2.0
               END IF
            IF (TYPCOR.EQ.9) THEN
               TR = BUFR(IAD) + BUFR(IAD+3*INCS)
               TI = BUFR(IAD+1) + BUFR(IAD+1+3*INCS)
               END IF
            IF (TYPCOR.EQ.10) THEN
               TR = BUFR(IAD) - BUFR(IAD+3*INCS)
               TI = BUFR(IAD+1) - BUFR(IAD+1+3*INCS)
               END IF
            IF (TYPCOR.EQ.11) THEN
               TR = BUFR(IAD) - BUFR(IAD+1+INCS)
               TI = BUFR(IAD+1) + BUFR(IAD+INCS)
               END IF
            IF (TYPCOR.GE.12) THEN
               TR = BUFR(IAD) + BUFR(IAD+1+INCS)
               TI = BUFR(IAD+1) - BUFR(IAD+INCS)
               END IF
C                                       amplitude
            IF (J.LE.1) XY(I) = SQRT (TR*TR + TI*TI)
C                                       phase
            IF (J.EQ.2) THEN
               IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XY(I) = R2D *
     *           ATAN2 (TI, TR)
               END IF
C                                       Real , Imag parts
            IF (J.EQ.9)  XY(I) = TR
            IF (J.EQ.10) XY(I) = TI
            GO TO 900
C                                       U, V distance
 30     CONTINUE
            XY(I) = SQRT (RANDP(1+ILOCU)**2 + RANDP(1+ILOCV)**2)
            XY(I) = XY(I) * FRQSCL
            GO TO 900
C                                       U, V position angle
 40     CONTINUE
            TR = RANDP(1+ILOCU)
            TI = RANDP(1+ILOCV)
            IF ((TI.NE.0.0) .OR. (TR.NE.0.0)) XY(I) = R2D *
     *         ATAN2 (TI, TR)
            GO TO 900
C                                       Time
 50     CONTINUE
            XY(I) = RANDP(1+ILOCT)
            GO TO 900
C                                       U projected spacing
 60     CONTINUE
            XY(I) = RANDP(1+ILOCU) * FRQSCL
            GO TO 900
C                                       V projected spacing
 70     CONTINUE
            XY(I) = RANDP(1+ILOCV) * FRQSCL
            GO TO 900
C                                       W projected spacing
 80     CONTINUE
            XY(I) = RANDP(1+ILOCW) * FRQSCL
            GO TO 900
C                                       Scan Number
 120    CONTINUE
C                                       Get current time
            T = RANDP(1+ILOCT)
C                                       If Time is not last time
            IF (T-LASTT .GT. EPSLN) THEN
C                                       increment integration number
               LASTSC = LASTSC + 1
C                                       save last time
               LASTT  = T
               END IF
C                                       If Time is before last time
            IF (LASTT-T .GT. EPSLN) THEN
C                                       reset integration number
               LASTSC = 1
C                                       save last time
               LASTT  = T
               END IF
C                                       return last integration number
            XY(I) = LASTSC
 
 900     CONTINUE
C
 999  RETURN
      END
