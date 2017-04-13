      SUBROUTINE PSFORM (NC, PS, IPT)
C-----------------------------------------------------------------------
C! analyses a wild-card string, preparing an array for pattern matching
C# Character Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   PSFORM prepares a string pattern array for use by CHWMAT (the wild
C   card matching subroutine).
C   Inputs:
C      NC   I       Number characters in pattern possible
C      PS   C*(*)   Pattern string
C   Output:
C      IPT  I(NC)   Coded array:
C                       value = -2 => position is *
C                       value = -1 => position is ?
C                       value = 0  => position is a blank
C                       value > 0  => there are IPT(i) real chars
C                                     including the present following
C-----------------------------------------------------------------------
      INTEGER   NC, IPT(*)
      CHARACTER PS*(*)
C
      CHARACTER IT*1
      INTEGER   J1, J2, I, J, N
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IF (NC.LE.0) GO TO 999
      N = 0
C                                       loop through pattern
      DO 30 I = 1,NC
         IT = PS(I:I)
         IPT(I) = 1
         IF (IT.EQ.'*') IPT(I) = -2
         IF (IT.EQ.'?') IPT(I) = -1
         IF (IT.EQ.' ') IPT(I) = 0
C                                       a real character
         IF (IPT(I).EQ.1) THEN
            N = N + 1
            IPT(I) = N
C                                       reset previous real ones
         ELSE
            IF (N.GT.1) THEN
               J1 = I - N
               J2 = I - 1
               DO 15 J = J1,J2
                  IPT(J) = N + 1 - IPT(J)
 15               CONTINUE
               END IF
            N = 0
            END IF
 30      CONTINUE
C                                      reset previous real ones
      IF ((IPT(NC).GT.0) .AND. (N.GT.1)) THEN
         J1 = NC + 1 - N
         J2 = NC
         DO 40 J = J1,J2
            IPT(J) = N + 1 - IPT(J)
 40         CONTINUE
         END IF
C
 999  RETURN
      END
