      SUBROUTINE POP (STACK, SP, ITEM)
C-----------------------------------------------------------------------
C! pops item from stack
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
C   POP takes the last item on the stack STACK and places it in ITEM.
C   The stack pointer SP is decremented.
C   Inputs:
C      STACK   I(100)   stack (upper size not checked)
C      SP      I        current stack pointer
C   Outputs:
C      ITEM    I        last item on current stack
C      SP      I        revised stack pointer
C-----------------------------------------------------------------------
      INTEGER   STACK(*), SP, ITEM
C
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DERR.INC'
      DATA PRGNAM /'POP   '/
C-----------------------------------------------------------------------
      IF (SP.LE.0) GO TO 980
         ITEM = STACK(SP)
         SP = SP - 1
         GO TO 999
C                                      Error: stack pointer < 0
 980  ERRNUM = 7
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
