      SUBROUTINE CHLORU (OPER, N, STRING)
C-----------------------------------------------------------------------
C! converts a CHARACTER string to all upper- or all lower-case letters
C# Character Parsing
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
C   CHLORU converts any lower-case characters in a CHARACTER string to
C   upper case (OPER = 'UP') or all upper-case letters to lower case
C   (OPER = 'DOWN').
C   Inputs:
C      OPER    C*4     'UP', "DOWN'
C      N       I       Number of characters
C   In/out:
C      STRING  C*(*)   String to be converted.
C-----------------------------------------------------------------------
      CHARACTER OPER*(*), STRING*(*)
      INTEGER   N
C
      INTEGER   I, J, J1, J2, M, K1
      CHARACTER ALPH(2)*26, IT*1
      DATA ALPH /'ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     *   'abcdefghijklmnopqrstuvwxyz'/
C-----------------------------------------------------------------------
C                                       set direction
      IF (OPER.EQ.'DOWN') THEN
         J1 = 1
         J2 = 2
      ELSE
         J1 = 2
         J2 = 1
         END IF
C                                       loop through string
      M = MIN (N, LEN (STRING))
      IF (M.GT.0) THEN
         DO 20 I = 1,M
            IT = STRING(I:I)
            IF ((IT.GE.ALPH(J1)(1:1)) .AND. (IT.LE.ALPH(J1)(26:26)))
     *         THEN
C                                       small hash to speed search
               K1 = 1
               IF (IT.LT.ALPH(J1)(14:14)) THEN
                  IF (IT.GE.ALPH(J1)(7:7)) K1 = 7
               ELSE
                  K1 = 14
                  IF (IT.GE.ALPH(J1)(20:20)) K1 = 20
                  END IF
               DO 10 J = K1,26
                  IF (IT.EQ.ALPH(J1)(J:J)) THEN
                     STRING(I:I) = ALPH(J2)(J:J)
                     GO TO 20
                     END IF
 10               CONTINUE
               END IF
 20         CONTINUE
         END IF
C
 999  RETURN
      END
