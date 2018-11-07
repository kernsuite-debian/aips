      SUBROUTINE CITCPR (S1, S2, N, WILD, EQUAL)
C-----------------------------------------------------------------------
C! KEYIN routine: character compare with wild cards.
C# Parsing
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
C   CITCPR compares character strings S1 and S2 up to N places;
C   WILD contains the "wildcard" character.
C   Inputs:
C      S1    C    First input character string.
C      S2    C    Second character string.
C      N     I    Number of characters to compare.
C      WILD  C*1  Wild card character.
C   Outputs:
C      EQUAL L    .TRUE. iff match.
C   TJP 1977 DECEMBER 12  CIT 370/158
C   MODIFIED 1979 FEB 6   VAX/VMS FORTRAN 4+
C   Rewritten for AIPS 1983 May, WDC
C-----------------------------------------------------------------------
      CHARACTER S1*(*), S2*(*), WILD*1, CHS1*1, CHS2*1
      INTEGER   I, N, INDEX
      LOGICAL   EQUAL, T, F
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      EQUAL = F
      IF (S1(1:N).NE.S2(1:N)) THEN
C                                       Any wild cards?
         IF ((INDEX (S1(1:N), WILD).LE.0) .AND.
     *      (INDEX (S2(1:N), WILD).LE.0)) GO TO 999
C                                       Loop checking
         DO 10 I = 1,N
C                                       Get characters.
            CHS1 = S1(I:I)
            CHS2 = S2(I:I)
C                                       Check for wildcard.
            IF ((CHS1.NE.WILD) .AND. (CHS2.NE.WILD)) THEN
C                                       Compare
               IF (CHS1.NE.CHS2) GO TO 999
               END IF
 10         CONTINUE
         END IF
C                                       No differences.
      EQUAL = T
C
 999  RETURN
      END
