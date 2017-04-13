      SUBROUTINE TPTBSM (WIDTH, X, Y, S, BLANK, N, DOHAN, YS)
C-----------------------------------------------------------------------
C! 2-point smoothing of an irregularly spaced array with blanking
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003, 2013
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
C   TPTBSM does a 2-point smoothing of irregularly spaced points,
C   replacing blanked values with the smoothed values if they are within
C   +-WIDTH/2 of good values.  If DOHAN, it then modifies good points
C   too, averaging them with the 2-point value.  Does not smooth
C   different source numbers together.  Therefore, make S(*) constant if
C   you wish to smooth all samples.
C   Inputs:
C      WIDTH   R      Width of boxcar in same units as X
C      X       R(N)   Absissae of points to be smoothed - monotonically
C                     increasing
C      Y       R(N)   Values to be smoothed.
C      S       I(N)   Source numbers - do not smooth across numbers
C                     If S(1) < 0, the S array is not otherwise used
C      BLANK   R      Value used for blanking.
C      N       I      Number of points to smooth.
C      DOHAN   L      "Hanning" smooth all data too
C    Output:
C      YS      R(N)   Smoothed values.
C-----------------------------------------------------------------------
      INTEGER   N, S(N)
      REAL      WIDTH, X(N), Y(N), YS(N), BLANK
      LOGICAL   DOHAN
C
      INTEGER   I, J, J1, J2, K
      REAL      HWIDTH, DIFF
      LOGICAL   NOSORC
C-----------------------------------------------------------------------
C                                       copy if width <= 0
      CALL RCOPY (N, Y, YS)
      IF (WIDTH.GT.0.0) THEN
         NOSORC = S(1).LT.0
         HWIDTH = WIDTH / 2.0
         DO 100 I = 1,N
            IF ((Y(I).EQ.BLANK) .OR. (DOHAN)) THEN
               J2 = 0
               J1 = 0
               IF (NOSORC) THEN
                  K = I + 1
                  DO 10 J = K,N
                     IF (X(J)-X(I).GT.HWIDTH) GO TO 20
                     IF (Y(J).NE.BLANK) THEN
                        J2 = J
                        GO TO 20
                        END IF
 10                  CONTINUE
 20               K = I - 1
                  DO 30 J = 1,K
                     IF (X(I)-X(I-J).GT.HWIDTH) GO TO 90
                     IF (Y(I-J).NE.BLANK) THEN
                        J1 = I - J
                        GO TO 90
                        END IF
 30                  CONTINUE
               ELSE
                  K = I + 1
                  DO 50 J = K,N
                     IF (X(J)-X(I).GT.HWIDTH) GO TO 60
                     IF ((Y(J).NE.BLANK) .AND. (S(I).EQ.S(J))) THEN
                        J2 = J
                        GO TO 60
                        END IF
 50                  CONTINUE
 60               K = I - 1
                  DO 70 J = 1,K
                     IF (X(I)-X(I-J).GT.HWIDTH) GO TO 90
                     IF ((Y(I-J).NE.BLANK) .AND. (S(I).EQ.S(I-J))) THEN
                        J1 = I - J
                        GO TO 90
                        END IF
 70                  CONTINUE
                  END IF
C                                       average
 90            IF ((J1.GT.0) .AND. (J2.GT.0)) THEN
                  DIFF = X(J2) - X(J1)
                  IF (DIFF.GT.0.0) THEN
                     YS(I) = Y(J1) + (X(I) - X(J1)) * (Y(J2) - Y(J1)) /
     *                  DIFF
                  ELSE
                     YS(I) = (Y(J2) + Y(J1)) / 2.0
                     END IF
                  IF ((DOHAN) .AND. (Y(I).NE.BLANK)) YS(I) = (YS(I) +
     *               Y(I)) / 2.0
               ELSE IF (J1.GT.0) THEN
                  YS(I) = Y(J1)
                  IF ((DOHAN) .AND. (Y(I).NE.BLANK)) YS(I) = (YS(I) +
     *               2.* Y(I)) / 3.0
               ELSE IF (J2.GT.0) THEN
                  YS(I) = Y(J2)
                  IF ((DOHAN) .AND. (Y(I).NE.BLANK)) YS(I) = (YS(I) +
     *               2.* Y(I)) / 3.0
                  END IF
               END IF
 100        CONTINUE
         END IF
C
 999  RETURN
      END
