      SUBROUTINE CFFTI(N, WSAVE)
C-----------------------------------------------------------------------
C! Initialize FFT work array.
C# AP-FFT
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
C   Initialize a workspace for a complex-to-complex FFT of length N.
C   The workspace should be an array of at least 4*N+15 elements and
C   N should be a positive integer.
C
C   This routine should not be called directly: use QCFT1I instead
C   which may call an FFT routine that has been optimized for the
C   machine you are working on.  QCFT1I will call this routine if
C   there is no specific optimized routine available.
C
C   Input:
C      N       I        Length of Fourier transform.
C
C   Output:
C      WSAVE   R(*)     Workspace.
C
C   Adapted from the FFTPACK routine by Paul N. Schwartztrauber.
C   This file also contains subroutine CFFTI1.
C-----------------------------------------------------------------------
      INTEGER   N
      REAL      WSAVE(*)
C
      INTEGER IW1, IW2
C-----------------------------------------------------------------------
      IF (N .GT. 1) THEN
C                                       Set indices for real and
C                                       integer workspaces; the integer
C                                       workspace is storage-associated
C                                       with WSAVE(IW2:?) in the call
C                                       to CFFTI1.  This may give rise
C                                       to warnings from some compilers.
         IW1 = N + N + 1
         IW2 = IW1 + N + N
         CALL CFFTI1(N, WSAVE(IW1), WSAVE(IW2))
      END IF
C
      END
      SUBROUTINE CFFTI1(N, WA, IFAC)
C-----------------------------------------------------------------------
C   Prepare the workspace for a complex-to-complex FFT of length N.
C   N should be at least 2.
C
C   Input:
C      N       I       Length of Fourier transform
C
C   Output:
C      WA      R(*)    Real workspace
C      IFAC    I(*)    Integer workspace
C-----------------------------------------------------------------------
      INTEGER   N, IFAC(*)
      REAL      WA(*)
C
      REAL ARG, ARGH, ARGLD, FI
      INTEGER I, I1, IB, IDO, IDOT, II, IP, IPM, J, K1, L1, L2,
     *   LD, NF, NL, NQ, NR, NTRY
      INTEGER NTRYH(4)
      SAVE    NTRYH
C
      INCLUDE 'INCS:PSTD.INC'
C
      DATA NTRYH /3, 4, 2, 5/
C-----------------------------------------------------------------------
      NL = N
      NF = 0
      J = 0
 100  J = J + 1
      IF ( J.LE.4 ) THEN
         NTRY = NTRYH(J)
      ELSE
         NTRY = NTRY + 2
      END IF
 200  NQ = NL/NTRY
      NR = NL - NTRY*NQ
      IF ( NR.NE.0 ) GO TO 100
      NF = NF + 1
      IFAC(NF+2) = NTRY
      NL = NQ
      IF ( NTRY.EQ.2 ) THEN
         IF ( NF.NE.1 ) THEN
            DO 220 I = 2, NF
               IB = NF - I + 2
               IFAC(IB+2) = IFAC(IB+1)
 220        CONTINUE
            IFAC(3) = 2
         END IF
      END IF
      IF ( NL.NE.1 ) GO TO 200
      IFAC(1) = N
      IFAC(2) = NF
      ARGH = TWOPI/FLOAT(N)
      I = 2
      L1 = 1
      DO 300 K1 = 1, NF
         IP = IFAC(K1+2)
         LD = 0
         L2 = L1*IP
         IDO = N/L2
         IDOT = IDO + IDO + 2
         IPM = IP - 1
         DO 250 J = 1, IPM
            I1 = I
            WA(I-1) = 1.
            WA(I) = 0.
            LD = LD + L1
            FI = 0.
            ARGLD = FLOAT(LD)*ARGH
            DO 240 II = 4, IDOT, 2
               I = I + 2
               FI = FI + 1.
               ARG = FI*ARGLD
               WA(I-1) = COS(ARG)
               WA(I) = SIN(ARG)
 240        CONTINUE
            IF ( IP.GT.5 ) THEN
               WA(I1-1) = WA(I-1)
               WA(I1) = WA(I)
            END IF
 250     CONTINUE
         L1 = L2
 300  CONTINUE
      RETURN
      END
