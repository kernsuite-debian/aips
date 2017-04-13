      SUBROUTINE FPTRIM (INSTR, OUTST, NCHAR)
C-----------------------------------------------------------------------
C! removes leading blanks and trailing 0's from floating format string
C# Character
C-----------------------------------------------------------------------
C;  Copyright (C) 2011
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
C   Removes leading blanks and trailing zeros from an Fa.b formatted
C   string
C   Input:
C      INSTR   C*(*)    Input string
C   Outputs:
C      OUTST   C*(*)    Output string
C      NCHAR   I        Number of useful characters in OUTST
C-----------------------------------------------------------------------
      INTEGER   NCHAR
      CHARACTER INSTR*(*), OUTST*(*)
C
      INTEGER   I, JTRIM, N, I1, I2, IP
C-----------------------------------------------------------------------
      N = JTRIM (INSTR)
C                                       find 1st char, check format
      I2 = N
      I1 = 0
      IP = 0
      DO 10 I = 1,N
         IF (INSTR(I:I).EQ.'.') THEN
            IP = I
         ELSE IF (INSTR(I:I).EQ.' ') THEN
            I1 = I
         ELSE IF ((INSTR(I:I).EQ.'-') .OR. (INSTR(I:I).EQ.'+')) THEN
            CONTINUE
C                                       bail on error
         ELSE IF ((INSTR(I:I).LT.'0') .OR. (INSTR(I:I).GT.'9')) THEN
            GO TO 30
            END IF
 10      CONTINUE
C                                       trailing zeros
      IF (IP.GT.0) THEN
         DO 20 I = N,IP+2,-1
            IF (INSTR(I:I).EQ.'0') I2 = I - 1
 20         CONTINUE
         END IF
C                                       copy to out: allows out to be in
 30   NCHAR = I2 - I1
      DO 40 I = 1,NCHAR
         OUTST(I:I) = INSTR(I+I1:I+I1)
 40      CONTINUE
      OUTST(NCHAR+1:) = ' '
C
 999  RETURN
      END

