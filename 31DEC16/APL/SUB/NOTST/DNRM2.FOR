      SUBROUTINE DNRM2 (N, DX, INCX, XDNRM2)
C-----------------------------------------------------------------------
C! Compute Euclidean norm of N-Vector
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
C   Euclidean norm of the N-vector stored in DX() with storage
C   increment INCX .
C    Input:
C     N      I    Size of vector DX with increment INCX
C     DX(*)  D    Input vector
C     INCX   I    Increment of DX
C    Output:
C     XDNRM2 D    Euclidian norm
C  Useage notes:
C   IF    N .LE. 0 return with RESULT = 0.
C   IF N .GE. 1 then INCX must be .GE. 1
C        C. L. Lawson, 1978 JAN 08
C   Four phase method  using two built-in constants that are
C   hopefully applicable to all machines.
C       CUTLO = Maximum of  DSQRT(U/EPS)  over all known machines.
C       CUTHI = Minimum of  DSQRT(V)      over all known machines.
C   where
C       EPS = smallest no. such that EPS + 1. .GT. 1.
C       U   = smallest positive no.   (underflow limit)
C       V   = largest  no.            (overflow  limit)
C   brief outline of algorithm..
C   Phase 1    scans zero components.
C   move to phase 2 when a component is nonzero and .LE. CUTLO
C   move to phase 3 when a component is .GT. CUTLO
C   move to phase 4 when a component is .GE. CUTHI/M
C   where M = N for X() real and M = 2*N for COMPLEX.
C
C   values for CUTLO and CUTHI..
C   from the environmental parameters listed in the IMSL converter
C   document the limiting values are as follows..
C   CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
C                 UNIVAC and DEC at 2**(-103)
C                 Thus CUTLO = 2**(-51) = 4.44089E-16
C   CUTHI, S.P.   V = 2**127 for UNIVAC, HONEYWELL, and DEC.
C                 THUS CUTHI = 2**(63.5) = 1.30438E19
C   CUTLO, D.P.   U/EPS = 2**(-67) for HONEYWELL and DEC.
C                 thus CUTLO = 2**(-33.5) = 8.23181D-11
C   CUTHI, D.P.   same AS S.P.  CUTHI = 1.30438D19
C   DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C   DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
C-----------------------------------------------------------------------
      INTEGER   N, INCX, NEXT, NN, I, J
      DOUBLE PRECISION DX(*), XDNRM2, CUTLO, CUTHI, HITEST, SUM, XMAX
      DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
C-----------------------------------------------------------------------
      IF (N.GT.0) GO TO 10
         XDNRM2 = 0.0D0
         GO TO 300
C
 10   NEXT = 1
      SUM = 0.0D0
      NN = N * INCX
C                                                 Begin main loop
      I = 1
   20    GO TO (30, 50, 70, 110), NEXT
   30 IF( ABS (DX(I)).GT.CUTLO) GO TO 85
      NEXT = 2
      XMAX = 0.0D0
C                                       Phase 1.  sum is zero
   50 IF (DX(I).EQ.0.0D0) GO TO 200
      IF (ABS (DX(I)).GT.CUTLO) GO TO 85
C                                       Prepare for phase 2.
      NEXT = 3
      GO TO 105
C                                       Prepare for phase 4.
  100 I = J
      NEXT = 4
      SUM = (SUM / DX(I)) / DX(I)
  105 XMAX = ABS (DX(I))
      GO TO 115
C                                       phase 2.  SUM is small.
C                                       scale to avoid destructive
C                                       underflow.
   70 IF( ABS (DX(I)).GT.CUTLO ) GO TO 75
C                                       Common code for phases 2 and 4.
C                                       In phase 4 SUM is large.
C                                       Scale to avoid overflow.
  110 IF (ABS(DX(I)).LE.XMAX) GO TO 115
         SUM = 1.0D0 + SUM * (XMAX / DX(I))**2
         XMAX = ABS (DX(I))
         GO TO 200
  115 SUM = SUM + (DX(I)/XMAX)**2
      GO TO 200
C                                       Prepare for phase 3.
   75 SUM = (SUM * XMAX) * XMAX
C                                       For real or D.P.
C                                       set HITEST = CUTHI/N
C                                       For complex
C                                       set HITEST = CUTHI/(2*N)
   85 HITEST = CUTHI / REAL (N)
C                                       phase 3.
C                                       Sum is mid-range.  No scaling.
      DO 95 J =I, NN,INCX
      IF (ABS (DX(J)).GE.HITEST) GO TO 100
   95    SUM = SUM + DX(J)**2
      XDNRM2 = SQRT (SUM)
      GO TO 300
  200 CONTINUE
      I = I + INCX
      IF (I.LE.NN) GO TO 20
C                                       End of main loop.
C                                       Compute square root and
C                                       adjust for scaling.
      XDNRM2 = XMAX * SQRT (SUM)
  300 CONTINUE
C
 999  RETURN
      END
