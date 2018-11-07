      SUBROUTINE ZCPU (TIME, IOCNT)
C-----------------------------------------------------------------------
C! return current process CPU time and IO count
C# Z System
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
C   ZCPU determines cumulative cpu usage in seconds for this process:
C   i.e. each time a process calls ZCPU during an execution, TIME is
C   larger.
C   Output:
C      TIME    R        Current CPU accumulation in seconds
C      IOCNT   I        IO count accumulation
C   VAX version:  accuracy 1 / 100 of a second.
C-----------------------------------------------------------------------
      REAL      TIME
      INTEGER   IOCNT
C
      INTEGER   LISTI(10), JERR, SYS$GETJPIW, ITIME, IOCNT3, IOCNT2
      INTEGER*2 LIST2(20)
      EQUIVALENCE (LISTI, LIST2)
      INCLUDE '($JPIDEF)'
C-----------------------------------------------------------------------
C                                       Set control list
      CALL FILL (10, 0, LISTI)
      LIST2(1) = 4
      LIST2(2) = JPI$_CPUTIM
      LISTI(2) = %LOC(ITIME)
      LIST2(7) = 4
      LIST2(8) = JPI$_DIRIO
      LISTI(5) = %LOC(IOCNT2)
      LIST2(13) = 4
      LIST2(14) = JPI$_BUFIO
      LISTI(8) = %LOC(IOCNT3)
C                                       Get time, iocnt
      JERR = SYS$GETJPIW ( , , , LISTI, , , )
      TIME = 0.01 * ITIME
      IOCNT = IOCNT2 + IOCNT3
C
 999  RETURN
      END
