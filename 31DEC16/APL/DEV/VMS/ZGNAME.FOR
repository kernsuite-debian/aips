      SUBROUTINE ZGNAME (NAME, IERR)
C-----------------------------------------------------------------------
C! get name of current process
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
C   Get the name of the current process.
C   Output:
C      NAME   C*6   Current process name
C      IERR   I     Error return code: 0 => no error
C                      1 => error
C   VMS version
C-----------------------------------------------------------------------
      CHARACTER NAME*6
      INTEGER   IERR
C
      INTEGER   SYS$GETJPIW, I, J1, J2, IT, ITEM(4)
      INTEGER*2 ITEM2(8)
      CHARACTER UNAME*15
      INCLUDE '($JPIDEF)'
      EQUIVALENCE (ITEM2, ITEM)
C-----------------------------------------------------------------------
C                                       Get name
      ITEM2(1) = 15
      ITEM2(2) = JPI$_PRCNAM
      ITEM(2) = %LOC (UNAME)
      ITEM(3) = 0
      ITEM(4) = 0
      IERR = SYS$GETJPIW ( , , , ITEM, , , )
      IERR = IERR - 1
C                                       Move chars to output
      NAME = ' '
      DO 10 I = 1,6
         IT = ICHAR (UNAME(I:I))
         IF (IT.EQ.0) GO TO 999
         NAME(I:I) = UNAME(I:I)
 10      CONTINUE
C
 999  RETURN
      END
