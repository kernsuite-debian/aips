      SUBROUTINE GETINT (KB, KBPLIM, KBP, IX)
C-----------------------------------------------------------------------
C! returns numeric field from character buffer
C# Parsing FITS POPS-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2007
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
C   GETNUM converts ASCII numeric field into integer
C   Inputs:
C      KB      C*(*)   character buffer
C      KBPLIM  I       max character position in buffer to examine
C      KBP     I       start of numeric field
C   Outputs:
C      KBP     I       start of next field (incl blanks)
C      IX      I       numerical value: sets to -2147483648
C                         when no numeric characters found
C   Common:
C      DERR.INC        Sets ERRNUM to 27 on failures
C-----------------------------------------------------------------------
      CHARACTER KB*(*)
      INTEGER   KBP, KBPLIM, IX
C
      INTEGER   IT, NC, SIGN
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IX = 0
      IF (KBP.GT.KBPLIM) GO TO 980
      SIGN = 1
      NC = 0
C                                       skip leading blank,comma,+
 10   IF ((KB(KBP:KBP).EQ.' ') .OR. (KB(KBP:KBP).EQ.',') .OR.
     *   (KB(KBP:KBP).EQ.'+')) THEN
         KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 999
         GO TO 10
         END IF
      KBP = KBP - 1
C                                       get chars
 20   KBP = KBP + 1
      IF (KBP.LE.KBPLIM) THEN
C                                       unary minus
         IF ((KB(KBP:KBP).EQ.'-') .AND. (NC.EQ.0)) THEN
            SIGN = -SIGN
C                                       other: leave if not num
         ELSE
            IT = ICHAR(KB(KBP:KBP)) - ICHAR('0')
            IF ((IT.LT.0) .OR. (IT.GT.9)) THEN
               GO TO 40
            ELSE
               NC = NC + 1
               IX = IX * 10 + IT
               END IF
            END IF
         GO TO 20
         END IF
C                                       got it: apply sign, decimal pl
 40   IF (NC.GT.0) THEN
         IX = IX * SIGN
         GO TO 999
         END IF
C                                       error
 980  ERRNUM = 27
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.10) PNAME(ERRLEV) = 'GETNUM'
C                                       return blank value: can't
C                                       compile -2147483648
      IX = -2147483647 - 1
C
 999  RETURN
      END
