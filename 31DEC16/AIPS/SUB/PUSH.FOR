      SUBROUTINE PUSH (LSTACK, LSP, ITEM)
C-----------------------------------------------------------------------
C! pushes item onto stack advancing the stack pointer
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
C   PUSH pushes ITEM onto the pushdown stack LSTACK and advances the
C   stack pointer.
C   Inputs:
C      ITEM    I       datum to be placed on stack
C   In/out:
C      LSP     I       in: current stack pointer
C                      out: modified stack pointer
C      LSTACK  I(100)  stack
C   Common:
C      /POPS/    SLIM   I   size of stack
C      /ERRORS/  ...        POPS error return 7, traceback possible
C-----------------------------------------------------------------------
      INTEGER   LSTACK(*), LSP, ITEM
C
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      DATA PRGNAM /'PUSH '/
C-----------------------------------------------------------------------
      LSP = LSP + 1
      IF (LSP.GT.SLIM) GO TO 980
         LSTACK(LSP) = ITEM
         GO TO 999
C                                 Error routine. LSP too big
 980  ERRNUM = 7
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
      END
