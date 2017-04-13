      SUBROUTINE NAMEST (NAME, SEQ, STRING, NCHAR)
C-----------------------------------------------------------------------
C! packs image name in string with leading and trailing blanks removed
C# Header Utility Character
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   NAMEST packs the image name as name.class.seq with leading and
C   trailing blanks removed.
C   Inputs:
C      NAME    C*18     Name & class need 18 chars
C      SEQ     I        Image sequence number
C   Output:
C      STRING  C(*)     Output string
C      NCHAR   I        Total # chars in STRING
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), STRING*(*)
      INTEGER   SEQ, NCHAR
C
      INTEGER   OUP, INC, J1, J2, JM
      CHARACTER SUBSTR*12
C-----------------------------------------------------------------------
      OUP = 1
      JM = LEN (STRING)
      J1 = LEN (NAME)
C                                       Name
      J2 = MIN (J1, 12)
      SUBSTR = NAME(1:J2)
      CALL CHTRIM (SUBSTR, J2, SUBSTR, INC)
      STRING = SUBSTR(1:INC)
      OUP = OUP + INC
      IF (OUP.GT.JM) GO TO 999
      STRING(OUP:OUP) = '.'
      OUP = OUP + 1
      IF (OUP.GT.JM) GO TO 999
C                                       Class
      J2 = J2 + 1
      J1 = MIN (J1, 18)
      IF (J1.GT.J2) THEN
         SUBSTR = NAME(J2:J1)
         J2 = J1 - J2 + 1
         CALL CHTRIM (SUBSTR, J2, SUBSTR, INC)
         STRING(OUP:) = SUBSTR(1:INC)
         OUP = OUP + INC
         IF (OUP.GT.JM) GO TO 999
         END IF
      STRING(OUP:OUP) = '.'
      OUP = OUP + 1
      IF (OUP.GT.JM) GO TO 999
C                                       Sequence
      WRITE (SUBSTR,1000) SEQ
      CALL CHTRIM (SUBSTR, 6, SUBSTR, INC)
      NCHAR = MIN (OUP+INC-1, JM)
      STRING(OUP:) = SUBSTR(1:INC)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I6)
      END
