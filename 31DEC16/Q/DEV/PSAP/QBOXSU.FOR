      SUBROUTINE QBOXSU (A, I, NB, C, J, N)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Boxcar sum of a vector.
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
C   Pseudo AP version.
C   QBOXSU does a boxcar sum of a vector, returns constant values in
C   first and last half boxcar width.
C   Inputs:
C      A   I  input vector base address
C      I   I  input vector increment
C      NB  I  boxcar width
C      C   I  output vector base address; output vector should not
C             overlap input.
C      J   I  output increment
C      N   I  element count
C-----------------------------------------------------------------------
      INTEGER   A, I, NB, C, J, N
C
      INTEGER   II, IJ, IODD, ITEMP
      LONGINT   JA, JC
      DOUBLE PRECISION SUM
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Convert to 1-rel. addresses
      JA = A + PSAPOF
      JC = C + PSAPOF
C                                       Sum first section.
      IODD = MOD (NB, 2)
      SUM = 0.0
      INCLUDE 'INCS:ZVND.INC'
      DO 10 II = 1,NB
         SUM = SUM + APCORE(JA)
         JA = JA + I
 10      CONTINUE
C                                       Set output for first section.
      ITEMP = NB / 2
      IJ = MAX (1, ITEMP)
      DO 20 II = 1,IJ
         APCORE(JC) = SUM
         JC = JC + J
 20      CONTINUE
C                                       Loop through middle.
      IJ = N - NB
      DO 30 II = 1, IJ
         SUM = SUM + APCORE(JA) - APCORE(JA-NB*I)
         APCORE(JC) = SUM
         JA = JA + I
         JC = JC + J
 30      CONTINUE
C                                       Save constant value for last
C                                       section.
      IF (NB .LE. 1) GO TO 999
      IJ = IODD + NB/2
      DO 40 II = 1,IJ
         APCORE(JC) = SUM
         JC = JC + J
 40      CONTINUE
C
 999  RETURN
      END
