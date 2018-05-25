      SUBROUTINE R8TX (NXTLT, NTHPO, LENGT, CR0, CR1, CR2, CR3, CR4,
     *   CR5, CR6, CR7, CI0, CI1, CI2, CI3, CI4, CI5, CI6, CI7)
C-----------------------------------------------------------------------
C! VMS Pseudo AP routine: Radix 8 FFT butterfly
C# AP-FFT
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
C    Radix 8 Butterfly called by FFT842
C-----------------------------------------------------------------------
      REAL      CR0(*), CR1(*), CR2(*), CR3(*), CR4(*), CR5(*), CR6(*),
     *   CR7(*), CI0(*), CI1(*), CI2(*), CI3(*), CI4(*), CI5(*),
     *   CI6(*), CI7(*), C1, C2, C3, C4, C5, C6, C7, S1, S2, S3, S4, S5,
     *   S6, S7,
     *   AR0, AR1, AR2, AR3, AR4, AR5, AR6, AR7,
     *   AI0, AI1, AI2, AI3, AI4, AI5, AI6, AI7,
     *   BR0, BR1, BR2, BR3, BR4, BR5, BR6, BR7,
     *   BI0, BI1, BI2, BI3, BI4, BI5, BI6, BI7,
     *   TR, TI, SCALE, ARG, PI2, P7
      INTEGER   NXTLT, NTHPO, LENGT, J, K
      COMMON /CON2/ PI2, P7
C-----------------------------------------------------------------------
      SCALE = PI2 / LENGT
      DO 30 J = 1,NXTLT,2
         ARG = (J - 1.0) * SCALE
         C1 = COS (ARG)
         S1 = SIN (ARG)
         C2 = C1 * C1 - S1 * S1
         S2 = C1 * S1 * 2.0
         C3 = C1 * C2 - S1 * S2
         S3 = C2 * S1 + S2 * C1
         C4 = C2 * C2 - S2 * S2
         S4 = C2 * S2 * 2.0
         C5 = C2 * C3 - S2 * S3
         S5 = C3 * S2 + S3 * C2
         C6 = C3 * C3 - S3 * S3
         S6 = C3 * S3 * 2.0
         C7 = C3 * C4 - S3 * S4
         S7 = C4 * S3 + S4 * C3
         DO 20 K = J,NTHPO,LENGT
            AR0 = CR0(K) + CR4(K)
            AR1 = CR1(K) + CR5(K)
            AR2 = CR2(K) + CR6(K)
            AR3 = CR3(K) + CR7(K)
            AR4 = CR0(K) - CR4(K)
            AR5 = CR1(K) - CR5(K)
            AR6 = CR2(K) - CR6(K)
            AR7 = CR3(K) - CR7(K)
            AI0 = CI0(K) + CI4(K)
            AI1 = CI1(K) + CI5(K)
            AI2 = CI2(K) + CI6(K)
            AI3 = CI3(K) + CI7(K)
            AI4 = CI0(K) - CI4(K)
            AI5 = CI1(K) - CI5(K)
            AI6 = CI2(K) - CI6(K)
            AI7 = CI3(K) - CI7(K)
            BR0 = AR0 + AR2
            BR1 = AR1 + AR3
            BR2 = AR0 - AR2
            BR3 = AR1 - AR3
            BR4 = AR4 - AI6
            BR5 = AR5 - AI7
            BR6 = AR4 + AI6
            BR7 = AR5 + AI7
            BI0 = AI0 + AI2
            BI1 = AI1 + AI3
            BI2 = AI0 - AI2
            BI3 = AI1 - AI3
            BI4 = AI4 + AR6
            BI5 = AI5 + AR7
            BI6 = AI4 - AR6
            BI7 = AI5 - AR7
            CR0(K) = BR0 + BR1
            CI0(K) = BI0 + BI1
            IF (J.LE.0) GO TO 10
               CR1(K) = C4 * (BR0 - BR1) - S4 * (BI0 - BI1)
               CI1(K) = C4 * (BI0 - BI1) + S4 * (BR0 - BR1)
               CR2(K) = C2 * (BR2 - BI3) - S2 * (BI2 + BR3)
               CI2(K) = C2 * (BI2 + BR3) + S2 * (BR2 - BI3)
               CR3(K) = C6 * (BR2 + BI3) - S6 * (BI2 - BR3)
               CI3(K) = C6 * (BI2 - BR3) + S6 * (BR2 + BI3)
               TR = P7 * (BR5 - BI5)
               TI = P7 * (BR5 + BI5)
               CR4(K) = C1 * (BR4 + TR) - S1 * (BI4 + TI)
               CI4(K) = C1 * (BI4 + TI) + S1 * (BR4 + TR)
               CR5(K) = C5 * (BR4 - TR) - S5 * (BI4 - TI)
               CI5(K) = C5 * (BI4 - TI) + S5 * (BR4 - TR)
               TR = -P7 * (BR7 + BI7)
               TI = P7 * (BR7 - BI7)
               CR6(K) = C3 * (BR6 + TR) - S3 * (BI6 + TI)
               CI6(K) = C3 * (BI6 + TI) + S3 * (BR6 + TR)
               CR7(K) = C7 * (BR6 - TR) - S7 * (BI6 - TI)
               CI7(K) = C7 * (BI6 - TI) + S7 * (BR6 - TR)
               GO TO 20
 10         CR1(K) = BR0 - BR1
            CI1(K) = BI0 - BI1
            CR2(K) = BR2 - BI3
            CI2(K) = BI2 + BR3
            CR3(K) = BR2 + BI3
            CI3(K) = BI2 - BI3
            TR = P7 * (BR5 - BI5)
            TI = P7 * (BR5 + BI5)
            CR4(K) = BR4 + TR
            CI4(K) = BI4 + TI
            CR5(K) = BR4 - TR
            CI5(K) = BI4 - TI
            TR = -P7 * (BR7 + BI7)
            TI = P7 * (BR7 - BI7)
            CR6(K) = BR6 + TR
            CI6(K) = BI6 + TI
            CR7(K) = BR6 - TR
            CI7(K) = BI6 - TI
 20         CONTINUE
 30      CONTINUE
 999  RETURN
      END
