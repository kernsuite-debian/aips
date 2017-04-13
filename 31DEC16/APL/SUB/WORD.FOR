      SUBROUTINE WORD (LINE, AWORD)
C-----------------------------------------------------------------------
C! Gets the first word from input line
C# Printer Character
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
C   gets the first word (delimited by blanks or , . : ; ' ")
C   Input:
C      LINE    C*(*)   Input text - no special characters
C   Output:
C      AWORD   C*(*)   Output word
C-----------------------------------------------------------------------
      CHARACTER LINE*(*), AWORD*(*)
C
      INTEGER   NDELIM
      PARAMETER (NDELIM = 7)
      INTEGER   JTRIM, IL, IW, I1, I2, I, N
      CHARACTER DELIMI(7)*1
      DATA DELIMI /' ', '.', ',', ':', ';', '"', ''''/
C-----------------------------------------------------------------------
      AWORD = ' '
      IL = JTRIM (LINE)
      IW = LEN (AWORD)
C                                       something to check
      I1 = 0
      IF (IL.GT.0) THEN
         DO 20 I = 1,IL
            DO 15 N = 1,NDELIM
               IF (LINE(I:I).EQ.DELIMI(N)) GO TO 20
 15            CONTINUE
            I1 = I
            GO TO 25
 20         CONTINUE
C                                       not all delimiters
 25      I2 = 0
         IF (I1.GT.0) THEN
            IL = MIN (IL, I1-1+IW)
            DO 40 I = I1,IL
               DO 35 N = 1,NDELIM
                  IF (LINE(I:I).EQ.DELIMI(N)) GO TO 45
 35               CONTINUE
               I2 = I
 40            CONTINUE
C                                       return word
 45         AWORD = LINE(I1:I2)
            END IF
         END IF
C
 999  RETURN
      END
