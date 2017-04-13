      SUBROUTINE CH2NUM (ISTART, IEND, STRING, INUM, IERR)
C-----------------------------------------------------------------------
C! converts string containing an integer in ASCII form into the integer
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
C  This routine will convert a substring containing an ASCII character
C  representation of an integer into an integer.
C  Input:
C     ISTART    I      Starting position in the string.
C     IEND      I      Ending position in the string.
C     STRING    C(*)   Character string.
C  Output:
C     INUM      I      Number.
C     IERR      I      Error code. 0=ok.  A message will be printed on
C                      error.  3 => Non-numeric character in string
C-----------------------------------------------------------------------
      CHARACTER STRING*(*)
      INTEGER   ISTART, IEND, INUM, IERR
C
      INTEGER   IT, III, IEXP
      CHARACTER OUTSTR*1
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      INUM = 0
      IERR = 0
      DO 10 III = ISTART,IEND
         OUTSTR = STRING(III:III)
         IF (OUTSTR.NE.' ') THEN
            IT = ICHAR(OUTSTR) - ICHAR('0')
            IF ((IT.LT.0) .OR. (IT.GT.9)) GO TO 980
            IEXP = IEND - III
            INUM = INUM + (IT * (10 ** IEXP))
            END IF
 10      CONTINUE
      GO TO 999
C                                       Not numeric.
 980  WRITE (MSGTXT,1980) STRING(ISTART:IEND)
      CALL MSGWRT (6)
      IERR = 3
C
 999  RETURN
C-----------------------------------------------------------------------
 1980 FORMAT ('CH2NUM: STRING CONTAINS NON-NUMERIC CHARACTER: ''',A,
     *   '''')
      END
