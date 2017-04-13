      SUBROUTINE CONVFN (APLOC, TYPE, PARM, BUFFER)
C-----------------------------------------------------------------------
C! Computes convolving fn. kernels and stores them in "AP memory"
C# AP-util Math
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
C   CONVFN computes the convolving functions and stores them in the
C   array processor.  Values are tabulated every 1/XPARM(5) cell.
C   Inputs:
C      APLOC    I       AP base address of convolving function.
C      TYPE     I       Convolving function type:
C                          1 - 6  x, y separable functions
C                          11 - 16  radial functions
C      PARM     R(10)   Convolving function parameters.
C                       PARM(1) = radius of support, PARM(5) increment
C   Output:
C      BUFFER   R(*)    Work buffer - 1 full row = (2*PARM(1)+1)*100
C                       plus second (size XPARM(5) rather than 100)
C                       for circular grid types
C-----------------------------------------------------------------------
      INTEGER   APLOC, TYPE
      REAL      PARM(10), BUFFER(*)
C
      INTEGER   LIM, IM, NMAX, NROW, I, IALF, IER, LIMIT, IRAD,
     *   LLIM, LINC, J, APL1, APL2, LTYPE, BIAS, BBIAS
      REAL      P1, P2, U, UMAX, ABSU, ETA, PSI, XINC, RADIUS, V, PI,
     *   BESSJ1
      INCLUDE 'INCS:DMSG.INC'
      DATA PI /3.1415926536/
C-----------------------------------------------------------------------
C                                       Determine number of rows necessa
      LTYPE = ABS (TYPE)
      IF (LTYPE.GT.10) LTYPE = LTYPE - 10
C                                       Check function types.
      IF ((LTYPE.LT.1) .OR. (LTYPE.GT.6)) THEN
         TYPE = 4
         PARM(1) = 3.0
         PARM(2) = 1.55
         PARM(3) = 2.52
         PARM(4) = 2.00
         PARM(5) = 100.0
         WRITE (MSGTXT,1000) TYPE, PARM(1), PARM(2), PARM(3)
         CALL MSGWRT (1)
         LTYPE = 4
         END IF
C                                       Set parameters
      NROW = MAX (PARM(1) , 1.0)
      NROW = NROW * 2 + 1
      LINC = 100
      LINC = (LINC/2) * 2
      LIM = NROW * LINC + 1
      BIAS = (LINC/2) * NROW + 1
      UMAX = PARM(1)
      XINC = 1.0 / LINC
C                                       Pill box function
      IF (LTYPE.EQ.1) THEN
         DO 10 I = 1,LIM
            U = (I-BIAS) * XINC
            ABSU = ABS (U)
            BUFFER(I) = 1.0
            IF (ABSU.EQ.UMAX) THEN
               BUFFER(I) = 0.5
            ELSE IF (ABSU.GT.UMAX) THEN
               BUFFER(I) = 0.0
               END IF
 10         CONTINUE
C                                       Exponential function.
      ELSE IF (LTYPE.EQ.2) THEN
         P1 = 1.0 / PARM(2)
         DO 20 I = 1,LIM
            U = (I-BIAS) * XINC
            ABSU = ABS (U)
            BUFFER(I) = 0.0
            IF (ABSU.LE.UMAX) BUFFER(I) = EXP (-((P1*ABSU) ** PARM(3)))
 20         CONTINUE
C                                       Sinc function.
      ELSE IF (LTYPE.EQ.3) THEN
         P1 = PI / PARM(2)
         DO 30 I = 1,LIM
            U = (I-BIAS) * XINC
            ABSU = ABS (U)
            BUFFER(I) = 0.0
            IF (ABSU.EQ.0) THEN
               BUFFER(I) = 1.0
            ELSE IF (ABSU.LE.UMAX) THEN
               BUFFER(I) = SIN (P1*ABSU) / (P1*ABSU)
               END IF
 30         CONTINUE
C                                       EXP * SINC convolving fn.
      ELSE IF (LTYPE.EQ.4) THEN
         P1 = PI / PARM(2)
         P2 = 1.0 / PARM(3)
         DO 40 I = 1,LIM
            U = (I - LIM/2 - 1) * XINC
            ABSU = ABS (U)
            BUFFER(I) = 0.0
C                                       Check for central point.
            IF (ABSU.LT.XINC) THEN
               BUFFER(I) = 1.0
            ELSE IF (ABSU.LE.UMAX) THEN
               BUFFER(I) = SIN(U*P1) / (U*P1) *
     *            EXP (-((ABSU * P2) ** PARM(4)))
               END IF
 40         CONTINUE
C                                       Spherodial wave function
      ELSE IF (LTYPE.EQ.5) THEN
         NMAX = PARM(1)/XINC + 0.1
C                                       Zero table
         DO 50 I = 1,LIM
            BUFFER(I) = 0.0
 50         CONTINUE
C                                       Compute function
         IALF = 2.0 * PARM(2) + 1.1
         IM = 2.0 * PARM(1) + 0.1
         IALF = MAX (1, MIN (5, IALF))
         IM = MAX (4, MIN (8, IM))
         DO 51 I = 1,NMAX
            ETA = REAL (I-1) / REAL (NMAX-1)
            CALL SPHFN (IALF, IM, 0, ETA, PSI, IER)
            BUFFER(BIAS+I-1) = PSI
 51         CONTINUE
C                                       Fill in other half
         LIMIT = BIAS-1
         DO 52 I = 1,LIMIT
            BUFFER(BIAS-I) = BUFFER(BIAS+I)
 52         CONTINUE
C                                       EXP * SINC convolving fn.
      ELSE IF (LTYPE.EQ.6) THEN
         P1 = PI / PARM(2)
         P2 = 1.0 / PARM(3)
         DO 60 I = 1,LIM
            U = (I - LIM/2 - 1) * XINC
            ABSU = ABS (U)
            BUFFER(I) = 0.0
C                                       Check for central point.
            IF (ABSU.LT.XINC) THEN
               BUFFER(I) = 1.0
            ELSE IF (ABSU.LE.UMAX) THEN
               BUFFER(I) = 2.0 * BESSJ1(U*P1) / (U*P1) *
     *            EXP (-((ABSU * P2) ** PARM(4)))
               END IF
 60         CONTINUE
C
C                                       Put further functions here.
C
         END IF
C                                       Load function into AP.
      IF (ABS(TYPE).LE.10) THEN
         CALL QWR
         LLIM = LIM
         CALL QPUT (BUFFER, APLOC, LLIM, 2)
         CALL QWD
C                                       Circular: compute & load rows
       ELSE
         LINC = PARM(5)
         LINC = (LINC/2) * 2
         LLIM = NROW * LINC + 1
         BBIAS = (LINC/2) * NROW + 1
         XINC = 100.0 / LINC
         APL1 = APLOC
         APL2 = APLOC + (LLIM - 1) * LLIM
         LIMIT = LLIM / 2 + 1
         CALL QWR
         DO 120 J = 1,LIMIT
            V = BBIAS - J
            DO 110 I = 1,LIMIT
               U = BBIAS - I
               RADIUS = SQRT (U*U + V*V)
               IRAD = RADIUS * XINC + 0.5 + BIAS
               BUFFER(I+LIM) = 0.0
               IF (IRAD.LE.LIM) BUFFER(I+LIM) = BUFFER(IRAD)
               BUFFER(LIM+LLIM+1-I) = BUFFER(I+LIM)
 110           CONTINUE
            CALL QPUT (BUFFER(LIM+1), APL1, LLIM, 2)
            IF (APL2.GT.APL1) CALL QPUT (BUFFER(LIM+1), APL2, LLIM, 2)
            CALL QWD
            APL1 = APL1 + LLIM
            APL2 = APL2 - LLIM
 120        CONTINUE
         END IF
C
 999   RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CONVFN: Convolving fn =',I3,' parm =',5F8.5)
      END
