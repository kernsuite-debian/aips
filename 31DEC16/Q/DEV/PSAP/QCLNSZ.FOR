      SUBROUTINE QCLNSZ (LAPSIZ)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Determine AP size for CLEAN
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006-2008
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
C   Determine the amount of memory available for CLEAN.
C   Generic version: allows both primary and secondary AP memory.
C   Outputs:
C      LAPSIZ  I  Numbers of words of memory available.
C-----------------------------------------------------------------------
      INTEGER   LAPSIZ
C
      INTEGER   I1, I2, I3
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DAPM.INC'
C-----------------------------------------------------------------------
C                                       New method: alocate minimum
      IF (PSAPNW.LE.0) THEN
         I1 = 0
         CALL QINIT (I1, I2, I3)
         END IF
      LAPSIZ = 1024 * PSAPNW
C
 999  RETURN
      END


