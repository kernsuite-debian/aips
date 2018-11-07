      SUBROUTINE GETI (STR, I)
C-----------------------------------------------------------------------
C! determines integer part from string and returns it
C# FITS Parsing
C-----------------------------------------------------------------------
C;  Copyright (C) 2001
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
C   GETI parses the numeric integer field from a string and returns its
C   value.  Up to 12 numeric characters are supported.
C   Inputs:
C      STR   C(*)   Character string
C   Output:
C      I     I      Value or 1st numeric portion of I
C-----------------------------------------------------------------------
      CHARACTER STR*(*)
      INTEGER   I
C
      INTEGER   J, K, L, N
      CHARACTER LSTR*12
C-----------------------------------------------------------------------
      N = LEN (STR)
      I = 0
      DO 10 J = 1,N
         IF ((STR(J:J).GE.'0') .AND. (STR(J:J).LE.'9')) GO TO 20
 10      CONTINUE
      GO TO 999
C                                       numeric found
 20   DO 30 K = J,N
         IF ((STR(K:K).LT.'0') .OR. (STR(K:K).GT.'9')) GO TO 50
 30      CONTINUE
      K = N + 1
 50   K = K - 1
      LSTR = ' '
      L = 12 - (K-J)
      LSTR(L:) = STR(J:K)
      READ (LSTR,1000) I
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I12)
      END
