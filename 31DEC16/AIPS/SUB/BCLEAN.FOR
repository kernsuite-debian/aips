      SUBROUTINE BCLEAN
C-----------------------------------------------------------------------
C! Pops items from B-stack to A-stack until BPR-stack precedence < NEXTP
C# POPS-lang
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
C   BCLEAN pops items from B-stack to A-stack until the precedence
C   in the BPR-stack is less than the parameter NEXTP.  Used in
C   generating Polish postfix string of opcodes and operands.
C   Changes STACK, AP, BP in /POPS/
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   BPR(100), A(100), B(100), M
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DSMS.INC'
      EQUIVALENCE (STACK(1), A(1)), (CSTACK(1), B(1)), (BPR(1), V(1))
      DATA PRGNAM /'BCLEAN'/
C-----------------------------------------------------------------------
 10   IF (BP.LE.0) GO TO 999
         M = B(BP)
         IF (BPR(BP).LT.NEXTP) GO TO 999
            CALL PUSH (A, AP, M)
            IF (ERRNUM.NE.0) GO TO 980
            BP = BP - 1
            GO TO 10
C                                         Error return.
 980  ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
