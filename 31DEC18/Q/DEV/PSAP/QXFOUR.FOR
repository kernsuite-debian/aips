      SUBROUTINE QXFOUR (X, M, ISIG, WORK)
C-----------------------------------------------------------------------
C! Pseudo AP routine: FFT routine.
C# AP-FFT
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2005-2006, 2010-2012, 2015
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
C   This program will produce a fast Fourier transform of a complex data
C   set using quarter-length tables by based on an algorithm devised by
C   William Newman, Cornell September 1, 1973.
C   Currently will handle up to M=32768
C      The file INCLUDEd should contain the compiler No-dependency
C   directive on vectorizing compilers.
C       On scalar systems the code can be simplified by
C   removing the branch to 400 and lines from label 400 to 490.
C   Inputs:
C      X     D(2,M)  Complex array to be transformed.
C      M     I       Length of X (must be a power of 2)
C      ISIG  I       Direction of the transform - same convention as FPS
C   Outputs:
C      X     D(2,M)  Resulting transformed array.
C      WORK  D(2,M)  Complex work array.
C   Modified for vector machines by W. D. Cotton, NRAO, Feb. 1986
C   Modified to perform addtion for cos(theta=0) and sin(theta=0)
C-----------------------------------------------------------------------
      INTEGER   M, ISIG
      DOUBLE PRECISION X(2,M), WORK(2,M)
C
      INTEGER   OLDM, N, I, J, L, M4, NM2, I1, K, MSTEP, ISEP,
     *   ITIMES, ISEPI1, JI, JI1, II, ISEP4
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IBITRV(2*MAXIMG)
      DOUBLE PRECISION C(MAXIMG/2), S(MAXIMG/2), XT, ARG, AR, AI, DARG,
     *   TPI
      SAVE OLDM, C, S, IBITRV, N
      DATA OLDM /0/
C-----------------------------------------------------------------------
C                                       Init if input not same as last
      IF (M.NE.OLDM) THEN
C                                       Initialize tables
C                                       Find power of 2
         ARG = M
         N = (LOG (ARG) / LOG(2.0D0)) + 0.1D0
C                                       Fill sine & cosine tables
         M4 = M / 4
         NM2 = N - 2
         TPI = 8.0D0 * ATAN (1.0D0)
         TPI = TPI / M
C                                       Set Cos and Sin of zero
         C(1) = 1.
         S(1) = 0.
C                                       init rest of table
         DO 10 I = 2,M4
            DARG = TPI * (I-1)
            C(I) = COS (DARG)
            S(I) = SIN (DARG)
 10         CONTINUE
C                                       Reverse bit order
         DO 50 I = 1,M4
            I1 = I - 1
            J = 0
            DO 20 K = 1,NM2
               L = I1 / 2
               J = I1 + 2 * (J-L)
               I1 = L
 20            CONTINUE
            J = J + 1
            IF (I.LT.J) THEN
               XT = C(I)
               C(I) = C(J)
               C(J) = XT
               XT = S(I)
               S(I) = S(J)
               S(J) = XT
               END IF
 50            CONTINUE
C                                       Bit reversal index array
         DO 100 I = 1,M
            I1 = I - 1
            J = 0
            DO 80 K = 1,N
               L = I1 / 2
               J = I1 + 2 * (J-L)
               I1 = L
 80            CONTINUE
            J = J + 1
            IBITRV(I) = J
 100        CONTINUE
C                                       Record fft size
         OLDM = M
C                                       End if time to init.
         END IF
C                                       Do transform
C                                       Copy to work vector (w/ conj.)
C                                       assume forward or backward trans
      IF (ISIG .EQ. -1) THEN
         DO 210 I = 1,M
            WORK(1,I) = X(1,I)
            WORK(2,I) = X(2,I)
 210        CONTINUE
C                                       else going in other direction
      ELSE
         DO 220 I = 1,M
            WORK(1,I) = X(1,I)
            WORK(2,I) = -X(2,I)
 220        CONTINUE
         END IF
C                                       Butterfly loop
      DO 500 MSTEP = 1,N
         ISEP = 2 ** (N-MSTEP)
         ISEP4 = ISEP * 4
         ITIMES = 2 ** (MSTEP-1)
C                                       Move longer loop to inner
         IF (ITIMES.LE.ISEP) THEN
            DO 390 I = 1,ITIMES
               ISEPI1 = 2 * (I-1) * ISEP
               II = (I+1) / 2
               JI1 = ISEPI1 + 1
               JI = JI1 + ISEP
C                                       process one loop of FFT
               CALL QXFOR2 (I, ISEP, JI, JI1, C(II), S(II), WORK)
 390           CONTINUE
C                                       else other loop is longer
         ELSE
            DO 490 J = 1,ISEP
C                                       Odd passes
               II = 1
               JI1 = J
               JI = JI1 + ISEP
      INCLUDE 'INCS:ZVND.INC'
               DO 450 I = 1,ITIMES,2
                  AR = WORK(1,JI) * C(II) - WORK(2,JI) * S(II)
                  AI = WORK(1,JI) * S(II) + WORK(2,JI) * C(II)
                  WORK(1,JI) = WORK(1,JI1) - AR
                  WORK(2,JI) = WORK(2,JI1) - AI
                  WORK(1,JI1) = WORK(1,JI1) + AR
                  WORK(2,JI1) = WORK(2,JI1) + AI
                  II = II + 1
                  JI1 = JI1 + ISEP4
                  JI = JI + ISEP4
 450              CONTINUE
C                                       Even passes
               II = 1
               JI1 = J + 2 * ISEP
               JI = JI1 + ISEP
      INCLUDE 'INCS:ZVND.INC'
               DO 480 I = 2,ITIMES,2
                  AR = WORK(1,JI) * S(II) + WORK(2,JI) * C(II)
                  AI = WORK(1,JI) * C(II) - WORK(2,JI) * S(II)
                  WORK(1,JI) = WORK(1,JI1) + AR
                  WORK(2,JI) = WORK(2,JI1) - AI
                  WORK(1,JI1) = WORK(1,JI1) - AR
                  WORK(2,JI1) = WORK(2,JI1) + AI
                  II = II + 1
                  JI1 = JI1 + ISEP4
                  JI = JI + ISEP4
 480           CONTINUE
C                                       end do all elements
 490        CONTINUE
C                                       end if I pass longer than M
         END IF
C                                       end do all butterfly loops
 500     CONTINUE
C                                       Bit reverse and conjugate back
C                                       to X
C                                       assume forward or backward trans
      IF (ISIG .EQ. -1) THEN
         DO 600 I = 1, M
            II = IBITRV(I)
            X(1,I) =  WORK(1,II)
            X(2,I) =  WORK(2,II)
 600        CONTINUE
      ELSE
         DO 610 I = 1, M
            II = IBITRV(I)
            X(1,I) =  WORK(1,II)
            X(2,I) = -WORK(2,II)
 610        CONTINUE
         END IF
C
 999  RETURN
      END



