      SUBROUTINE QPHSRO (A, I, B, J, PHAS0, DELPHS, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Add phase gradient to a complex array.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   Pseudo-AP version
C   Adds a phase gradient to a complex array:
C      B(j) = A(j)*EXP(-i*(PHAS0+j*DELPHS)) for j = 0,N-1
C                              or
C      B(mJ)   = A(mI) * cos(P0+mDP) - A(mI+1) * sin(P0+mDP)
C      B(mJ+1) = A(mI) * sin(P0+mDP) + A(mI+1) * cos(P0+mDP)
C                                for m = 0 to N-1
C          where cos(P0) = PHAS0(0),   sin(P0) = PHAS0(0+1)
C                cos(DP) = DELPHS(0),  sin(DP) = DELPHS(0+1)
C   Inputs:
C      A       I  Source vector base address.
C      I       I  Increment of A (normally 2 * integer)
C      B       I  Destination base address.
C      J       I  Increment of B (normally 2 * integer)
C      PHAS0   I  Address of complex unit vector with phase PHAS0
C      DELPHS  I  Address of complex unit vector with phase DELPHS
C      N       I  Element count
C-----------------------------------------------------------------------
      INTEGER   A, I, B, J, PHAS0, DELPHS, N
C
      INTEGER   LOOP
      LONGINT   JA, JB
      DOUBLE PRECISION SPHS, CPHS, SDEL, CDEL, TEMPR, PHASE, DPHASE
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IF (N.LE.0) GO TO 999
C                                       Addresses 1-rel
      JA = A + PSAPOF
      JB = B + PSAPOF
C                                        Get sin and cos of PHAS0
C                                        and DELPHS
      SPHS = APCORE(PHAS0+1+PSAPOF)
      CPHS = APCORE(PHAS0+PSAPOF) + 1.0D-30
C                                       Get angles from sin and cos
      PHASE  = DATAN2 (SPHS, CPHS)
      SDEL = APCORE(DELPHS+1+PSAPOF)
      CDEL = APCORE(DELPHS+PSAPOF) + 1.0D-30
C                                       Get delta phase from sin and cos
      DPHASE = DATAN2 (SDEL, CDEL)
C                                       Loop thru vector.
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,N
C                                       Cos and Sin Theta
         CPHS = DCOS(PHASE)
         SPHS = DSIN(PHASE)
C                                       Add Phase to APCORE(JB)
         TEMPR        = APCORE(JA)*CPHS - APCORE(JA+1)*SPHS
         APCORE(JB+1) = APCORE(JA)*SPHS + APCORE(JA+1)*CPHS
         APCORE(JB)   = TEMPR
C                                       Update indices.
         PHASE = PHASE + DPHASE
         JA = JA + I
         JB = JB + J
 100     CONTINUE
C
 999  RETURN
      END
