      SUBROUTINE UNPACK (N, KF, KT)
C-----------------------------------------------------------------------
C! converts a packed character buffer into one with 1 character/integer
C# Obsolete Character
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
C   UNPACK unpacks a tight to a loose character buffer.  The operation
C   is defined as X/Y = X/b, Y/b.
C   Inputs:
C      N   I         # characters in input buffer
C      KF  R(*)      input tight buffer
C   Outputs:
C      KT  I(N)      output loose buffer
C-----------------------------------------------------------------------
      HOLLERITH KF(*)
      INTEGER   N, KT(*)
C
      INTEGER   J1, J2, IT, KTP, I, IBLANK
      HOLLERITH RBLANK
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (IBLANK, RBLANK)
C-----------------------------------------------------------------------
      RBLANK = HBLANK
      IF (N.LE.0) GO TO 999
         KTP = 1
         DO 10 I = 1, N
            KT(KTP) = IBLANK
            J1 = (I-1) / 4 + 1
            J2 = MOD (I-1, 4) + 1
            CALL ZGETCH (IT, KF(J1), J2)
            CALL ZPUTCH (IT, KT(KTP), 1)
            KTP = KTP + 1
 10         CONTINUE
C
 999  RETURN
      END
