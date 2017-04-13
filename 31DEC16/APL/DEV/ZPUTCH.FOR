      SUBROUTINE ZPUTCH (ICHAR, WORD, NCHAR)
C----------------------------------------------------------------------
C! inserts 8-bit "character" into a word
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
C----------------------------------------------------------------------
C   Inserts the character contained in the least significant bits of
C   the INTEGER argument ICHAR into the NCHAR position of the HOLLERITH
C   argument WORD.  It should also work for INTEGER WORD as long as
C   NCHAR is valid.  Characters are numbered from 1 in the order in
C   which they would be printed by a Fortran "A" format specifier.
C   Inputs:
C      ICHAR   I   Character to insert in LS bits
C      NCHAR   I   Position in WORD to store character
C   Output:
C      WORD    H   Word into which character is to be inserted
C   Generic version - stub
C----------------------------------------------------------------------
      INTEGER   ICHAR, NCHAR
      HOLLERITH WORD
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      WRITE (6,1000)
      IF (NLUSER.GT.0) STOP 'ZPUTCH missing'
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ZPUTCH REQUIRES LOCAL DEVELOPMENT')
      END
