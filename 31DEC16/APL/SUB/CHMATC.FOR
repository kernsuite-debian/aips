      SUBROUTINE CHMATC (NA, JA, CA, NB, JB, CB, NP)
C-----------------------------------------------------------------------
C! searches one HOLLERITH string for the occurrence of another
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
C   searches one HOLLERITH string for the occurrence of another string.
C   Inputs: NA   I        Number of characters in CA (start at JA)
C           JA   I        Start at char position JA in CA
C           CA   H(*)     Packed substring to be found in CB
C           NB   I        Number of characters in CB (n.b. TOTAL)
C           JB   I        Start search at offset in CB
C           CB   H(*)     Packed string.
C   Output: NP   I        start position in CB of CA, 0 if none.
C                         w.r.t. start of string
C-----------------------------------------------------------------------
      HOLLERITH CA(*), CB(*)
      INTEGER   NA, NB, NP, JA, JB
C
      INTEGER   I, NT, J1, J2, IT, ITA, ITB
C-----------------------------------------------------------------------
      NP = 0
      NT = NB - JB - NA + 2
      IF (NT.LE.0) GO TO 999
      J1 = (JA - 1) / 4 + 1
      J2 = JA - (J1-1) * 4
      CALL ZGETCH (ITA, CA(J1), J2)
      DO 20 NP = 1,NT
         J1 = (NP + JB - 2) / 4 + 1
         J2 = NP + JB - 1 - (J1-1) * 4
         CALL ZGETCH (IT, CB(J1), J2)
         IF (IT.NE.ITA) GO TO 20
            IF (NA.EQ.1) GO TO 30
            DO 10 I = 2,NA
               J1 = (I+JA-2) / 4 + 1
               J2 = I + JA - 1 - (J1-1) * 4
               CALL ZGETCH (ITB, CA(J1), J2)
               J1= (NP+JB+I-3) / 4 + 1
               J2 = NP + JB + I - 2 - (J1-1) * 4
               CALL ZGETCH (IT, CB(J1), J2)
               IF (ITB.NE.IT) GO TO 20
 10            CONTINUE
            GO TO 30
 20      CONTINUE
      NP = 0
      GO TO 999
C                                       answer wrt start CB
 30   NP = NP + JB - 1
C
 999  RETURN
      END
