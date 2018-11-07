      SUBROUTINE DMACH (JOB, XDMACH)
C-----------------------------------------------------------------------
C! Linpack? routine: Sets machine precision parameters. (DOUBLE)
C# Math
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
C   DMACH computes machine parameters of floating point
C   arithmetic for use in testing ONLY.
C   If there is trouble with automatic computation of these quantities,
C   they can be set by direct assignment statements.
C   assume the computer has:
C        B = Base of arithmetic
C        T = Number of base  B  digits
C        L = smallest possible exponent
C        U = largest possible exponent
C     then
C        EPS = B**(1-T)
C        TINY = 100.0*B**(-L+T)
C        HUGE = 0.01*B**(U-T)
C     Input:
C       JOB     I    1 => EPSILON, 2 => TINY, 3 => HUGE.
C     Output:
C       XDMACH  D    Result indicated by JOB
C-----------------------------------------------------------------------
      INTEGER   JOB
      DOUBLE PRECISION EPS, TINY, HUGE, S, XDMACH
C-----------------------------------------------------------------------
      EPS = 1.0D0
   10    EPS = EPS/2.0D0
         S = 1.0D0 + EPS
         IF (S .GT. 1.0D0) GO TO 10
      EPS = 2.0D0*EPS
C
      S = 1.0D0
   20    TINY = S
         S = S/16.0D0
         IF (S*1.0 .NE. 0.0D0) GO TO 20
      TINY = (TINY/EPS)*100.0
      HUGE = 1.0D0/TINY
C
      IF (JOB .EQ. 1) XDMACH = EPS
      IF (JOB .EQ. 2) XDMACH = TINY
      IF (JOB .EQ. 3) XDMACH = HUGE
C
 999  RETURN
      END
