      SUBROUTINE TFILTR (FILTER, RATE, TMINT, RFACT, IERR)
C-----------------------------------------------------------------------
C! Compute rate decorrelation corrections as a function of filter type
C# Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2007
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
C  Compute rate decorrelation corrections as a function of filter type
C   Inputs:
C      FILTER   I    Filter type: 4 => 64-8; 3 => 32-4; 0 => BOX
C      RATE     R    Fringe rate (radians/day)
C      TMINT    R    Averaging time (days)
C   Outputs:
C      RFACT    R    Amplitude correction factor
C      IERR     I    Return code (0=> ok; else error)
C-----------------------------------------------------------------------
      REAL      RATE, TMINT, RFACT
      INTEGER   FILTER, IERR
C
      INCLUDE 'INCS:PSTD.INC'
C                                       Polynomial order for
C                                       VLBA filter corrections
      INTEGER N32D4, N64D8
      PARAMETER (N32D4 = 14)
      PARAMETER (N64D8 = 14)
      DOUBLE PRECISION DP
      REAL C32D4(N32D4), C64D8(N64D8), ARG
      INTEGER J
      INCLUDE 'INCS:DMSG.INC'
C                                       Polynomial coefficients for
C                                       VLBA filter corrections,
C                                       derived by L. Kogan (96):
C                                       32-tap; decimation 4
      DATA C32D4 /0.9998181, 0.05842116, -0.315723E+1,
     *   0.2541054E+2, -0.4032384E+2, -0.8400728E+2, -0.1156864E+3,
     *   0.7344348E+3, 0.1153372E+4, 0.7487309E+3, -0.1181970E+5,
     *   -0.5113351E+4, 0.4165397E+5, -0.2703458E+5/
C                                       64-TAP; decimation 8
      DATA C64D8 /0.1000438E+1, -0.1965840E-1, -0.8578708,
     *   0.4649189E+1, 0.1179239E+2, -0.2645647E+2, -0.2537850E+3,
     *   0.4259856E+3, 0.5437562E+3, -0.1127070E+4, 0.4009456E+4,
     *   -0.8898893E+4, -0.1362069E+4, 0.9950514E+4/
C-----------------------------------------------------------------------
C                                       Initialization
      IERR = 0
      RFACT = 1.0
C                                       RATE is given in radians/day
      ARG = ABS (TMINT * RATE / TWOPI)
C                                       Case filter_type of:
      IF (FILTER .EQ. 3) THEN
C                                       32-tap; decimation factor 4
         IF (ARG .LE. 0.5 .AND. ARG .GT. 0.01) THEN
            DP = C32D4(N32D4)
            DO 50 J = (N32D4-1), 1, -1
               DP = DP * ARG + C32D4(J)
50             CONTINUE
            RFACT = 1.0 / DP
         ELSE
            RFACT = 1.0
            END IF
      ELSE IF (FILTER .EQ. 4) THEN
C                                       64-tap; decimation factor 8
         IF (ARG .LE. 0.5 .AND. ARG .GT. 0.01) THEN
            DP = C64D8(N64D8)
            DO 100 J = (N64D8-1), 1, -1
               DP = DP * ARG + C64D8(J)
100            CONTINUE
            RFACT = 1.0 / RFACT
         ELSE
            RFACT = 1.0
            END IF
      ELSE IF (FILTER .EQ. 0) THEN
C                                       Boxcar filter
         ARG = 0.5 * ARG * TWOPI
         IF (ARG .GT. 1.0E-5) THEN
            RFACT = ARG / SIN (ARG)
         ELSE
            RFACT = 1.0
            END IF
      ELSE
C                                       Unidentified filter
         IERR = 1
         WRITE (MSGTXT,1200) FILTER
         GO TO 990
         END IF
C                                       Endcase
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1200  FORMAT ('TFILTR: UNIDENTIFIED CORRELATOR FILTER TYPE:',I4)
      END
