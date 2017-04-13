      SUBROUTINE HUNT (K, L, NT, ISTART, KHUNT)
C-----------------------------------------------------------------------
C! searches a linked list for words to be matched
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
C   HUNT searches a linked list by words - usually the literal values
C   list, since CHUNT is used to do min-match on the SYMBOL names.
C   Inputs:
C      K      I(*)   contains linked tables
C      NT     I      number of words to match
C      ISTART I      offset within each table entry for matching
C      KHUNT  I(NT)  words to be matched
C   In/out:
C      L      I      in:  start address in K for tables
C                    out: pointer to entry found (0 if none)
C-----------------------------------------------------------------------
      INTEGER   K(*), L, NT, ISTART, KHUNT(*)
C
      INTEGER   I, J, LENGTH
C-----------------------------------------------------------------------
C                                        zero link -> none
 10   IF (L.EQ.0) GO TO 999
C                                        compare
         LENGTH = K(L+1) / 16
         IF (LENGTH.NE.NT) GO TO 30
            J = L + ISTART
            DO 20 I = 1,NT
               IF (K(J).NE.KHUNT(I)) GO TO 30
               J = J + 1
 20            CONTINUE
C                                        found it!
            GO TO 999
C                                        link to next
 30      L = K(L)
         GO TO 10
C
 999  RETURN
      END
