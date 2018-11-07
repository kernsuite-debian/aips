      SUBROUTINE QPHSRO (A, I, B, J, PHAS0, DELPHS, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Add phase gradient to a complex array.
C# AP-appl
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
C   Adds a phase gradient to a complex array:
C        B(j) = A(j)*EXP(-i*(PHAS0+j*DELPHS)) for j = 0,N-1
C    Inputs:
C     A       I    Source vector base address.
C     I       I    Increment of A
C     B       I    Destination base address.
C     J       I    Increment of B
C     PHAS0   I    Address of complex unit vector with
C                  phase PHAS0
C     DELPHS  I    Address of complex unit vector with
C                  phase DELPHS
C     N       I    Element count
C   Useage notes:
C      This routine uses work vector, WKVECA
C   CRI version.
C-----------------------------------------------------------------------
      INTEGER   A, I, B, J, PHAS0, DELPHS, N, LOOP, JA, JB, JC
      REAL     TEMP, TEMPR, TEMPI, PHASE, DPHASE, TWOPI
      INCLUDE 'INCS:DAPC.INC'
      DATA TWOPI /6.283185308/
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Get phase and delta phase
      PHASE = ATAN2 (APCORE(PHAS0+2), APCORE(PHAS0+1)+1.0E-30)
      DPHASE = ATAN2 (APCORE(DELPHS+2), APCORE(DELPHS+1)+1.0E-30)
C                                       Make phase ramp
      JA = 1
      DO 20 LOOP = 1,N
         TEMP = MOD ((PHASE + (LOOP-1) * DPHASE), TWOPI)
         WKVECA(JA+1) = SIN (TEMP)
         WKVECA(JA) = COS (TEMP)
         JA = JA + 2
 20      CONTINUE
C                                       Addresses 1-rel
      JA = A + 1
      JB = B + 1
      JC = 1
C                                        (complex) multiply WKVECA*JA
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,N
         TEMPR = APCORE(JA)*WKVECA(JC) - APCORE(JA+1)*WKVECA(JC+1)
         TEMPI = APCORE(JA)*WKVECA(JC+1) + APCORE(JA+1)*WKVECA(JC)
         APCORE(JB) = TEMPR
         APCORE(JB+1) = TEMPI
C                                        Update indices.
         JA = JA + I
         JB = JB + J
         JC = JC + 2
 100     CONTINUE
C
 999  RETURN
      END
