      SUBROUTINE FUNBSM (FUNTYP, CUT, WIDTH, X, Y, S, BLANK, N, YS)
C-----------------------------------------------------------------------
C! smoothing with wt function of irregularly spaced array with blanking
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   Does a weight-function smoothing of irregularly spaced points,
C   replacing blanked and good values with the smoothed values.  Only
C   returns blanked values if no valid data found within +-WIDTH/2
C   and/or sum of function weights < CUT.  Does not smooth different
C   source numbers together.  Therefore, make S(*) constant if you wish
C   to smooth all samples.
C   Inputs:
C      FUNTYP   C*4    Function type: 'GAUS', 'EXP ', 'LINE'
C      CUT      R      Sum of function < CUT => output blanked
C      WIDTH    R(2)   (1) Full width of window in same units as X
C                      (2) FWHM of function in same units as X
C      X        R(N)   Absissae of points to be smoothed - monotonically
C                      increasing
C      Y        R(N)   Values to be smoothed.
C      S        I(N)   Source numbers - do not smooth across numbers
C                      If S(1) < 0, then S(I) is not otherwise examined
C      BLANK    R      Value used for blanking.
C      N        I      Number of points to smooth.
C    Output:
C      YS       R(N)   Smoothed values.
C-----------------------------------------------------------------------
      CHARACTER FUNTYP*(*)
      INTEGER   N, S(N)
      REAL      CUT, WIDTH(2), X(N), Y(N), YS(N), BLANK
C
      INTEGER   I, J, J1, J2
      REAL      HWIDTH, FWIDTH, WT, WTS, V
      LOGICAL   NOSORC
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       copy if width <= 0
      CALL RCOPY (N, Y, YS)
      IF ((FUNTYP(:4).NE.'LINE') .AND. (FUNTYP(:4).NE.'GAUS') .AND.
     *   (FUNTYP(:4).NE.'EXP ')) THEN
         WRITE (MSGTXT,1000) FUNTYP, WIDTH
         CALL MSGWRT (6)
      ELSE IF ((WIDTH(1).GT.0.0) .AND. (WIDTH(2).GT.0.0)) THEN
         NOSORC = S(1).LT.0
         CUT = MAX (CUT, 1.E-6)
         HWIDTH = WIDTH(1) / 2.0
         FWIDTH = WIDTH(2) / 2.0
         IF (FUNTYP(:4).EQ.'LINE') FWIDTH = 2.0 * FWIDTH
         IF (FUNTYP(:4).EQ.'EXP ') FWIDTH = FWIDTH / LOG (2.0)
         IF (FUNTYP(:4).EQ.'GAUS') FWIDTH = FWIDTH / SQRT (LOG (2.0))
         J2 = 1
         DO 100 I = 1,N
            J1 = J2
            J2 = N + 1
            WTS = 0.0
            V   = 0.0
C                                       source not examined
            IF (NOSORC) THEN
               DO 10 J = J1,N
                  IF (X(J)-X(I).GT.HWIDTH) GO TO 90
                  IF (ABS(X(I)-X(J)).LE.HWIDTH) THEN
                     J2 = MIN (J2, J)
                     IF (Y(J).NE.BLANK) THEN
                        WT = ABS (X(I)-X(J)) / FWIDTH
                        IF (FUNTYP(:4).EQ.'GAUS') THEN
                           WT = EXP (-(WT**2))
                        ELSE IF (FUNTYP(:4).EQ.'EXP ') THEN
                           WT = EXP (-WT)
                        ELSE
                           WT = 1.0 - WT
                           END IF
                        WT = MAX (0.0, WT)
                        WTS = WTS + WT
                        V = V + Y(J) * WT
                        END IF
                     END IF
 10               CONTINUE
C                                       source examined
            ELSE
               DO 20 J = J1,N
                  IF (X(J)-X(I).GT.HWIDTH) GO TO 90
                  IF (ABS(X(I)-X(J)).LE.HWIDTH) THEN
                     J2 = MIN (J2, J)
                     IF ((Y(J).NE.BLANK) .AND. (S(I).EQ.S(J))) THEN
                        WT = ABS (X(I)-X(J)) / FWIDTH
                        IF (FUNTYP(:4).EQ.'GAUS') THEN
                           WT = EXP (-(WT**2))
                        ELSE IF (FUNTYP(:4).EQ.'EXP ') THEN
                           WT = EXP (-WT)
                        ELSE
                           WT = 1.0 - WT
                           END IF
                        WT = MAX (0.0, WT)
                        WTS = WTS + WT
                        V = V + Y(J) * WT
                        END IF
                     END IF
 20               CONTINUE
               END IF
 90         IF (WTS.GT.CUT) THEN
               YS(I) = V / WTS
            ELSE
               YS(I) = BLANK
               END IF
 100        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FUNBSM: FUNCTION ',A,' TIMES',2(1PE11.3),' ERROR')
      END
