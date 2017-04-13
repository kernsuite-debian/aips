      SUBROUTINE H2CHR (NCH, INPNT, ISTR, OSTR)
C-----------------------------------------------------------------------
C! convert AIPS Hollerith string to Fortran CHARACTER variable
C# Character Utility
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
C   Convert an AIPS HOLLERITH string to a Fortran CHARACTER variable.
C   Blank fills the full ISTR variable.
C   Inputs:
C      NCH    I       Number of characters
C      INPNT  I       Start position in input string
C      ISTR   H(*)    Input AIPS string
C   Output:
C      OSTR   C*(*)   Output CHARACTER string
C-----------------------------------------------------------------------
      INTEGER   NCH, INPNT
      HOLLERITH ISTR(*)
      CHARACTER OSTR*(*)
C
      INTEGER   NA, I, J1, J2, IT, NLEN
C-----------------------------------------------------------------------
      NLEN = LEN (OSTR)
      OSTR = ' '
      IF (NCH.LE.0) GO TO 999
         NA = MAX (1, INPNT)
         NLEN = MIN (NLEN, NCH)
         DO 10 I = 1,NLEN
            J1 = (NA-1) / 4 + 1
            J2 = MOD (NA-1, 4) + 1
            CALL ZGETCH (IT, ISTR(J1), J2)
            OSTR(I:I) = CHAR (IT)
            NA = NA + 1
 10         CONTINUE
C
 999  RETURN
      END
