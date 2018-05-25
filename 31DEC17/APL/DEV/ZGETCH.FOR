      SUBROUTINE ZGETCH (ICHAR, WORD, NCHAR)
C-----------------------------------------------------------------------
C! get a character from a REAL word
C# Z Binary
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
C   Extracts the character in position NCHAR of the REAL argument WORD
C   and inserts it in the least significant bits of the INTEGER argument
C   ICHAR with zero in the rest.  It should also work for INTEGER WORD
C   as long as NCHAR is valid.  Characters are numbered from 1 in the
C   order in which they would be printed by a Fortran "A" format
C   specifier.  NOTE - we actually get 8 bits here - so this routine
C   works for bytes too.
C   Inputs:
C      WORD   R   Word from which the character is to be extracted
C      NCHAR  I   Position of character to extract
C   Output:
C      ICHAR  I   Extracted character in LS bits, zero in the rest
C   Generic version - stub
C-----------------------------------------------------------------------
      INTEGER   ICHAR, NCHAR
      HOLLERITH WORD
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      MSGTXT = 'ZGETCH REQUIRES LOCAL DEVELOPMENT'
      CALL MSGWRT (10)
      IF (NLUSER.GT.0) STOP 'ZGETCH missing'
C
 999  RETURN
      END
