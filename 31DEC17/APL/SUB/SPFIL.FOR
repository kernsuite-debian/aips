      SUBROUTINE SPFIL (STRING, NT, NA)
C-----------------------------------------------------------------------
C! fills HOLLERITH string with blanks beginning at first null
C# Character
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
C   SPFIL locates the first null character in a string and replaces it
C   and the remainder of the string with ASCII blanks.
C   Inputs:
C      NT      I      # characters in full string
C   In/out:
C      STRING  H(*)   string to be modified
C   Outputs:
C      NA      I      # valid characters in string
C-----------------------------------------------------------------------
      HOLLERITH STRING(*)
      INTEGER   NT, NA
      INTEGER   IT, J1, J2, IIB, I, J
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NA = 0
      IF (NT.LE.0) GO TO 999
         J = 1
         IT = 100
         CALL ZGETCH (IIB, HBLANK, 1)
         DO 20 I = 1,NT
            J1 = (I-1) / 4 + 1
            J2 = MOD (I-1, 4) + 1
            IF (IT.NE.0) CALL ZGETCH (IT, STRING(J1), J2)
            IF (IT.EQ.0) CALL ZPUTCH (IIB, STRING(J1), J2)
            IF ((IT.NE.0) .AND. (IT.NE.IIB)) NA = I
 20         CONTINUE
C
 999  RETURN
      END
