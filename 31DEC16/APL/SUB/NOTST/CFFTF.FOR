      SUBROUTINE CFFTF(N, C, WSAVE)
C-----------------------------------------------------------------------
C! Complex-to-complex FFT
C# FFT
C-----------------------------------------------------------------------
C;  Copyright (C) 1997
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
C   Replace the first N elements of C by their unnormalized FFT.
C   N must be positive and WSAVE must have at least 4*N+15 elements and
C   must have been initialized by calling CFFTI.  WSAVE may not be
C   modified between the call to CFFTI and CFFTB.
C
C   This routine should not be called directly: use QCFT1D instead.
C   QCFT1D may take advantage of FFT routines that have been
C   specifically optimized for the machine that you are working on.
C
C   Inputs:
C      N      I       Length of FFT
C      WSAVE  R(*)    Workspace
C
C   Input/output
C      C      C(*)    Complex data; replaced by its transform
C
C   Adapted from the FFTPACK routine by Paul N. Schwartztrauber.
C   This file also contains subroutines CFFTF1, PASSF, PASSF2, PASSF3,
C   PASSF4, and PASSF5
C-----------------------------------------------------------------------
      INTEGER   N
      COMPLEX   C(*)
      REAL      WSAVE(*)
C
      INTEGER IW1, IW2
C-----------------------------------------------------------------------
      IF (N .GT. 1) THEN
         IW1 = N + N + 1
         IW2 = IW1 + N + N
C                                       This call may generate compiler
C                                       warnings since WSAVE(IW2) is
C                                       associated with an INTEGER
C                                       array and C with a REAL array
C                                       in this call.
         CALL CFFTF1(N, C, WSAVE, WSAVE(IW1), WSAVE(IW2))
      END IF
C
      END
      SUBROUTINE CFFTF1(N, C, CH, WA, IFAC)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   N, IFAC(*)
      REAL      C(*), CH(*), WA(*)
C
      INTEGER   I, IDL1, IDO, IDOT, IP, IW, IX2, IX3, IX4, K1, L1,
     *   L2, N2, NA, NAC, NF
C-----------------------------------------------------------------------
      NF = IFAC(2)
      NA = 0
      L1 = 1
      IW = 1
      DO 100 K1 = 1, NF
         IP = IFAC(K1+2)
         L2 = IP*L1
         IDO = N/L2
         IDOT = IDO + IDO
         IDL1 = IDOT*L1
         IF ( IP.EQ.4 ) THEN
            IX2 = IW + IDOT
            IX3 = IX2 + IDOT
            IF ( NA.NE.0 ) THEN
               CALL PASSF4(IDOT, L1, CH, C, WA(IW), WA(IX2), WA(IX3))
            ELSE
               CALL PASSF4(IDOT, L1, C, CH, WA(IW), WA(IX2), WA(IX3))
            END IF
            NA = 1 - NA
         ELSE IF ( IP.EQ.2 ) THEN
            IF ( NA.NE.0 ) THEN
               CALL PASSF2(IDOT, L1, CH, C, WA(IW))
            ELSE
               CALL PASSF2(IDOT, L1, C, CH, WA(IW))
            END IF
            NA = 1 - NA
         ELSE IF ( IP.EQ.3 ) THEN
            IX2 = IW + IDOT
            IF ( NA.NE.0 ) THEN
               CALL PASSF3(IDOT, L1, CH, C, WA(IW), WA(IX2))
            ELSE
               CALL PASSF3(IDOT, L1, C, CH, WA(IW), WA(IX2))
            END IF
            NA = 1 - NA
         ELSE IF ( IP.NE.5 ) THEN
            IF ( NA.NE.0 ) THEN
               CALL PASSF(NAC, IDOT, IP, L1, IDL1, CH, CH, CH, C, C,
     *                    WA(IW))
            ELSE
               CALL PASSF(NAC, IDOT, IP, L1, IDL1, C, C, C, CH, CH,
     *                    WA(IW))
            END IF
            IF ( NAC.NE.0 ) NA = 1 - NA
         ELSE
            IX2 = IW + IDOT
            IX3 = IX2 + IDOT
            IX4 = IX3 + IDOT
            IF ( NA.NE.0 ) THEN
               CALL PASSF5(IDOT, L1, CH, C, WA(IW), WA(IX2), WA(IX3),
     *                     WA(IX4))
            ELSE
               CALL PASSF5(IDOT, L1, C, CH, WA(IW), WA(IX2), WA(IX3),
     *                     WA(IX4))
            END IF
            NA = 1 - NA
         END IF
         L1 = L2
         IW = IW + (IP-1)*IDOT
 100  CONTINUE
      IF ( NA.EQ.0 ) RETURN
      N2 = N + N
      DO 200 I = 1, N2
         C(I) = CH(I)
 200  CONTINUE
      RETURN
      END
      SUBROUTINE PASSF(NAC, IDO, IP, L1, IDL1, CC, C1, C2, CH, CH2, WA)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   NAC, IDO, IP, L1, IDL1
      REAL      CC(IDO, IP, L1), C1(IDO, L1, IP), C2(IDL1, IP),
     *   CH(IDO, L1, IP), CH2(IDL1, IP), WA(*)
C
      REAL      WAI, WAR
      INTEGER I, IDIJ, IDJ, IDL, IDLJ, IDOT, IDP, IK, INC,
     *        IPP2, IPPH, J, JC, K, L, LC
      INTEGER NT
C-----------------------------------------------------------------------
      IDOT = IDO/2
      NT = IP*IDL1
      IPP2 = IP + 2
      IPPH = (IP+1)/2
      IDP = IP*IDO
C
      IF ( IDO.LT.L1 ) THEN
         DO 50 J = 2, IPPH
            JC = IPP2 - J
            DO 20 I = 1, IDO
               DO 10 K = 1, L1
                  CH(I, K, J) = CC(I, J, K) + CC(I, JC, K)
                  CH(I, K, JC) = CC(I, J, K) - CC(I, JC, K)
 10            CONTINUE
 20         CONTINUE
 50      CONTINUE
         DO 100 I = 1, IDO
            DO 60 K = 1, L1
               CH(I, K, 1) = CC(I, 1, K)
 60         CONTINUE
 100     CONTINUE
      ELSE
         DO 150 J = 2, IPPH
            JC = IPP2 - J
            DO 120 K = 1, L1
               DO 110 I = 1, IDO
                  CH(I, K, J) = CC(I, J, K) + CC(I, JC, K)
                  CH(I, K, JC) = CC(I, J, K) - CC(I, JC, K)
 110           CONTINUE
 120        CONTINUE
 150     CONTINUE
         DO 200 K = 1, L1
            DO 160 I = 1, IDO
               CH(I, K, 1) = CC(I, 1, K)
 160        CONTINUE
 200     CONTINUE
      END IF
      IDL = 2 - IDO
      INC = 0
      DO 400 L = 2, IPPH
         LC = IPP2 - L
         IDL = IDL + IDO
         DO 250 IK = 1, IDL1
            C2(IK, L) = CH2(IK, 1) + WA(IDL-1)*CH2(IK, 2)
            C2(IK, LC) = -WA(IDL)*CH2(IK, IP)
 250     CONTINUE
         IDLJ = IDL
         INC = INC + IDO
         DO 300 J = 3, IPPH
            JC = IPP2 - J
            IDLJ = IDLJ + INC
            IF ( IDLJ.GT.IDP ) IDLJ = IDLJ - IDP
            WAR = WA(IDLJ-1)
            WAI = WA(IDLJ)
            DO 260 IK = 1, IDL1
               C2(IK, L) = C2(IK, L) + WAR*CH2(IK, J)
               C2(IK, LC) = C2(IK, LC) - WAI*CH2(IK, JC)
 260        CONTINUE
 300     CONTINUE
 400  CONTINUE
      DO 500 J = 2, IPPH
         DO 450 IK = 1, IDL1
            CH2(IK, 1) = CH2(IK, 1) + CH2(IK, J)
 450     CONTINUE
 500  CONTINUE
      DO 600 J = 2, IPPH
         JC = IPP2 - J
         DO 550 IK = 2, IDL1, 2
            CH2(IK-1, J) = C2(IK-1, J) - C2(IK, JC)
            CH2(IK-1, JC) = C2(IK-1, J) + C2(IK, JC)
            CH2(IK, J) = C2(IK, J) + C2(IK-1, JC)
            CH2(IK, JC) = C2(IK, J) - C2(IK-1, JC)
 550     CONTINUE
 600  CONTINUE
      NAC = 1
      IF ( IDO.EQ.2 ) RETURN
      NAC = 0
      DO 700 IK = 1, IDL1
         C2(IK, 1) = CH2(IK, 1)
 700  CONTINUE
      DO 800 J = 2, IP
         DO 750 K = 1, L1
            C1(1, K, J) = CH(1, K, J)
            C1(2, K, J) = CH(2, K, J)
 750     CONTINUE
 800  CONTINUE
      IF ( IDOT.LE.L1 ) THEN
         IDIJ = 0
         DO 850 J = 2, IP
            IDIJ = IDIJ + 2
            DO 820 I = 4, IDO, 2
               IDIJ = IDIJ + 2
               DO 810 K = 1, L1
                  C1(I-1, K, J) = WA(IDIJ-1)*CH(I-1, K, J) + WA(IDIJ)
     *                            *CH(I, K, J)
                  C1(I, K, J) = WA(IDIJ-1)*CH(I, K, J) - WA(IDIJ)
     *                          *CH(I-1, K, J)
 810           CONTINUE
 820        CONTINUE
 850     CONTINUE
         RETURN
      END IF
      IDJ = 2 - IDO
      DO 1000 J = 2, IP
         IDJ = IDJ + IDO
         DO 900 K = 1, L1
            IDIJ = IDJ
            DO 860 I = 4, IDO, 2
               IDIJ = IDIJ + 2
               C1(I-1, K, J) = WA(IDIJ-1)*CH(I-1, K, J) + WA(IDIJ)
     *                         *CH(I, K, J)
               C1(I, K, J) = WA(IDIJ-1)*CH(I, K, J) - WA(IDIJ)
     *                       *CH(I-1, K, J)
 860        CONTINUE
 900     CONTINUE
 1000 CONTINUE
      RETURN
C
      END
      SUBROUTINE PASSF2(IDO, L1, CC, CH, WA1)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   IDO, L1
      REAL      CC(IDO, 2, L1), CH(IDO, L1, 2), WA1(*)
C
      REAL      TI2, TR2
      INTEGER   I, K
C-----------------------------------------------------------------------
      IF ( IDO.LE.2 ) THEN
         DO 50 K = 1, L1
            CH(1, K, 1) = CC(1, 1, K) + CC(1, 2, K)
            CH(1, K, 2) = CC(1, 1, K) - CC(1, 2, K)
            CH(2, K, 1) = CC(2, 1, K) + CC(2, 2, K)
            CH(2, K, 2) = CC(2, 1, K) - CC(2, 2, K)
 50      CONTINUE
         RETURN
      END IF
      DO 200 K = 1, L1
         DO 100 I = 2, IDO, 2
            CH(I-1, K, 1) = CC(I-1, 1, K) + CC(I-1, 2, K)
            TR2 = CC(I-1, 1, K) - CC(I-1, 2, K)
            CH(I, K, 1) = CC(I, 1, K) + CC(I, 2, K)
            TI2 = CC(I, 1, K) - CC(I, 2, K)
            CH(I, K, 2) = WA1(I-1)*TI2 - WA1(I)*TR2
            CH(I-1, K, 2) = WA1(I-1)*TR2 + WA1(I)*TI2
 100     CONTINUE
 200  CONTINUE
      RETURN
      END
      SUBROUTINE PASSF3(IDO, L1, CC, CH, WA1, WA2)
C------------------------------------------------------------------------
C------------------------------------------------------------------------
      INTEGER   IDO, L1
      REAL      CC(IDO, 3, L1), CH(IDO, L1, 3), WA1(*), WA2(*)
C
      REAL CI2, CI3, CR2, CR3, DI2, DI3, DR2, DR3, TAUI, TAUR,
     *     TI2, TR2
      PARAMETER (TAUR = -0.5)
      PARAMETER (TAUI = -0.866025403784439)
      INTEGER I, K
C-----------------------------------------------------------------------
      IF ( IDO.EQ.2 ) THEN
         DO 50 K = 1, L1
            TR2 = CC(1, 2, K) + CC(1, 3, K)
            CR2 = CC(1, 1, K) + TAUR*TR2
            CH(1, K, 1) = CC(1, 1, K) + TR2
            TI2 = CC(2, 2, K) + CC(2, 3, K)
            CI2 = CC(2, 1, K) + TAUR*TI2
            CH(2, K, 1) = CC(2, 1, K) + TI2
            CR3 = TAUI*(CC(1, 2, K)-CC(1, 3, K))
            CI3 = TAUI*(CC(2, 2, K)-CC(2, 3, K))
            CH(1, K, 2) = CR2 - CI3
            CH(1, K, 3) = CR2 + CI3
            CH(2, K, 2) = CI2 + CR3
            CH(2, K, 3) = CI2 - CR3
 50      CONTINUE
         RETURN
      END IF
      DO 200 K = 1, L1
         DO 100 I = 2, IDO, 2
            TR2 = CC(I-1, 2, K) + CC(I-1, 3, K)
            CR2 = CC(I-1, 1, K) + TAUR*TR2
            CH(I-1, K, 1) = CC(I-1, 1, K) + TR2
            TI2 = CC(I, 2, K) + CC(I, 3, K)
            CI2 = CC(I, 1, K) + TAUR*TI2
            CH(I, K, 1) = CC(I, 1, K) + TI2
            CR3 = TAUI*(CC(I-1, 2, K)-CC(I-1, 3, K))
            CI3 = TAUI*(CC(I, 2, K)-CC(I, 3, K))
            DR2 = CR2 - CI3
            DR3 = CR2 + CI3
            DI2 = CI2 + CR3
            DI3 = CI2 - CR3
            CH(I, K, 2) = WA1(I-1)*DI2 - WA1(I)*DR2
            CH(I-1, K, 2) = WA1(I-1)*DR2 + WA1(I)*DI2
            CH(I, K, 3) = WA2(I-1)*DI3 - WA2(I)*DR3
            CH(I-1, K, 3) = WA2(I-1)*DR3 + WA2(I)*DI3
 100     CONTINUE
 200  CONTINUE
      RETURN
      END
      SUBROUTINE PASSF4(IDO, L1, CC, CH, WA1, WA2, WA3)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   IDO, L1
      REAL      CC(IDO, 4, L1), CH(IDO, L1, 4), WA1(*), WA2(*), WA3(*)
C
      REAL      CI2, CI3, CI4, CR2, CR3, CR4, TI1, TI2, TI3, TI4,
     *   TR1, TR2, TR3, TR4
      INTEGER   I, K
C-----------------------------------------------------------------------
      IF ( IDO.EQ.2 ) THEN
         DO 50 K = 1, L1
            TI1 = CC(2, 1, K) - CC(2, 3, K)
            TI2 = CC(2, 1, K) + CC(2, 3, K)
            TR4 = CC(2, 2, K) - CC(2, 4, K)
            TI3 = CC(2, 2, K) + CC(2, 4, K)
            TR1 = CC(1, 1, K) - CC(1, 3, K)
            TR2 = CC(1, 1, K) + CC(1, 3, K)
            TI4 = CC(1, 4, K) - CC(1, 2, K)
            TR3 = CC(1, 2, K) + CC(1, 4, K)
            CH(1, K, 1) = TR2 + TR3
            CH(1, K, 3) = TR2 - TR3
            CH(2, K, 1) = TI2 + TI3
            CH(2, K, 3) = TI2 - TI3
            CH(1, K, 2) = TR1 + TR4
            CH(1, K, 4) = TR1 - TR4
            CH(2, K, 2) = TI1 + TI4
            CH(2, K, 4) = TI1 - TI4
 50      CONTINUE
         RETURN
      END IF
      DO 200 K = 1, L1
         DO 100 I = 2, IDO, 2
            TI1 = CC(I, 1, K) - CC(I, 3, K)
            TI2 = CC(I, 1, K) + CC(I, 3, K)
            TI3 = CC(I, 2, K) + CC(I, 4, K)
            TR4 = CC(I, 2, K) - CC(I, 4, K)
            TR1 = CC(I-1, 1, K) - CC(I-1, 3, K)
            TR2 = CC(I-1, 1, K) + CC(I-1, 3, K)
            TI4 = CC(I-1, 4, K) - CC(I-1, 2, K)
            TR3 = CC(I-1, 2, K) + CC(I-1, 4, K)
            CH(I-1, K, 1) = TR2 + TR3
            CR3 = TR2 - TR3
            CH(I, K, 1) = TI2 + TI3
            CI3 = TI2 - TI3
            CR2 = TR1 + TR4
            CR4 = TR1 - TR4
            CI2 = TI1 + TI4
            CI4 = TI1 - TI4
            CH(I-1, K, 2) = WA1(I-1)*CR2 + WA1(I)*CI2
            CH(I, K, 2) = WA1(I-1)*CI2 - WA1(I)*CR2
            CH(I-1, K, 3) = WA2(I-1)*CR3 + WA2(I)*CI3
            CH(I, K, 3) = WA2(I-1)*CI3 - WA2(I)*CR3
            CH(I-1, K, 4) = WA3(I-1)*CR4 + WA3(I)*CI4
            CH(I, K, 4) = WA3(I-1)*CI4 - WA3(I)*CR4
 100     CONTINUE
 200  CONTINUE
      RETURN
      END
      SUBROUTINE PASSF5(IDO, L1, CC, CH, WA1, WA2, WA3, WA4)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER   IDO, L1
      REAL      CC(IDO, 5, L1), CH(IDO, L1, 5), WA1(*), WA2(*), WA3(*),
     *   WA4(*)
      REAL      CI2, CI3, CI4, CI5, CR2, CR3, CR4, CR5, DI2, DI3,
     *   DI4, DI5, DR2, DR3, DR4, DR5, TI11, TI12
      REAL      TI2, TI3, TI4, TI5, TR11, TR12, TR2, TR3, TR4, TR5
      PARAMETER (TR11 =  0.309016994374947)
      PARAMETER (TI11 = -0.951056516295154)
      PARAMETER (TR12 = -0.809016994374947)
      PARAMETER (TI12 = -0.587785252292473)
      INTEGER   I, K
C-----------------------------------------------------------------------
      IF ( IDO.EQ.2 ) THEN
         DO 50 K = 1, L1
            TI5 = CC(2, 2, K) - CC(2, 5, K)
            TI2 = CC(2, 2, K) + CC(2, 5, K)
            TI4 = CC(2, 3, K) - CC(2, 4, K)
            TI3 = CC(2, 3, K) + CC(2, 4, K)
            TR5 = CC(1, 2, K) - CC(1, 5, K)
            TR2 = CC(1, 2, K) + CC(1, 5, K)
            TR4 = CC(1, 3, K) - CC(1, 4, K)
            TR3 = CC(1, 3, K) + CC(1, 4, K)
            CH(1, K, 1) = CC(1, 1, K) + TR2 + TR3
            CH(2, K, 1) = CC(2, 1, K) + TI2 + TI3
            CR2 = CC(1, 1, K) + TR11*TR2 + TR12*TR3
            CI2 = CC(2, 1, K) + TR11*TI2 + TR12*TI3
            CR3 = CC(1, 1, K) + TR12*TR2 + TR11*TR3
            CI3 = CC(2, 1, K) + TR12*TI2 + TR11*TI3
            CR5 = TI11*TR5 + TI12*TR4
            CI5 = TI11*TI5 + TI12*TI4
            CR4 = TI12*TR5 - TI11*TR4
            CI4 = TI12*TI5 - TI11*TI4
            CH(1, K, 2) = CR2 - CI5
            CH(1, K, 5) = CR2 + CI5
            CH(2, K, 2) = CI2 + CR5
            CH(2, K, 3) = CI3 + CR4
            CH(1, K, 3) = CR3 - CI4
            CH(1, K, 4) = CR3 + CI4
            CH(2, K, 4) = CI3 - CR4
            CH(2, K, 5) = CI2 - CR5
 50      CONTINUE
         RETURN
      END IF
      DO 200 K = 1, L1
         DO 100 I = 2, IDO, 2
            TI5 = CC(I, 2, K) - CC(I, 5, K)
            TI2 = CC(I, 2, K) + CC(I, 5, K)
            TI4 = CC(I, 3, K) - CC(I, 4, K)
            TI3 = CC(I, 3, K) + CC(I, 4, K)
            TR5 = CC(I-1, 2, K) - CC(I-1, 5, K)
            TR2 = CC(I-1, 2, K) + CC(I-1, 5, K)
            TR4 = CC(I-1, 3, K) - CC(I-1, 4, K)
            TR3 = CC(I-1, 3, K) + CC(I-1, 4, K)
            CH(I-1, K, 1) = CC(I-1, 1, K) + TR2 + TR3
            CH(I, K, 1) = CC(I, 1, K) + TI2 + TI3
            CR2 = CC(I-1, 1, K) + TR11*TR2 + TR12*TR3
            CI2 = CC(I, 1, K) + TR11*TI2 + TR12*TI3
            CR3 = CC(I-1, 1, K) + TR12*TR2 + TR11*TR3
            CI3 = CC(I, 1, K) + TR12*TI2 + TR11*TI3
            CR5 = TI11*TR5 + TI12*TR4
            CI5 = TI11*TI5 + TI12*TI4
            CR4 = TI12*TR5 - TI11*TR4
            CI4 = TI12*TI5 - TI11*TI4
            DR3 = CR3 - CI4
            DR4 = CR3 + CI4
            DI3 = CI3 + CR4
            DI4 = CI3 - CR4
            DR5 = CR2 + CI5
            DR2 = CR2 - CI5
            DI5 = CI2 - CR5
            DI2 = CI2 + CR5
            CH(I-1, K, 2) = WA1(I-1)*DR2 + WA1(I)*DI2
            CH(I, K, 2) = WA1(I-1)*DI2 - WA1(I)*DR2
            CH(I-1, K, 3) = WA2(I-1)*DR3 + WA2(I)*DI3
            CH(I, K, 3) = WA2(I-1)*DI3 - WA2(I)*DR3
            CH(I-1, K, 4) = WA3(I-1)*DR4 + WA3(I)*DI4
            CH(I, K, 4) = WA3(I-1)*DI4 - WA3(I)*DR4
            CH(I-1, K, 5) = WA4(I-1)*DR5 + WA4(I)*DI5
            CH(I, K, 5) = WA4(I-1)*DI5 - WA4(I)*DR5
 100     CONTINUE
 200  CONTINUE
      RETURN
      END
