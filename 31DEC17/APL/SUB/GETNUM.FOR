      SUBROUTINE GETNUM (KB, KBPLIM, KBP, X)
C-----------------------------------------------------------------------
C! returns numeric field from character buffer
C# Parsing FITS POPS-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 2007
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
C   GETNUM converts ASCII numeric field into double precision number.
C   Inputs:
C      KB      C*(*)   character buffer
C      KBPLIM  I       max character position in buffer to examine
C      KBP     I       start of numeric field
C   Outputs:
C      KBP     I       start of next field (incl blanks)
C      X       D       numerical value: sets to magic indef = DBLANK
C                      from DDCH.INC when overflow exponent or value or
C                      when there are no numeric characters found
C   Common:
C      DERR.INC        Sets ERRNUM to 27 on failures
C   Note that a value of 'INDE' returns X = DBLANK, ERRNUM = 0
C   Other missing numeric values, excessive exponents return X = DBLANK,
C   ERRNUM = 27.
C-----------------------------------------------------------------------
      CHARACTER KB*(*)
      INTEGER   KBP, KBPLIM
      DOUBLE PRECISION X
C
      INTEGER   EXSIGN, IEXP, IT, LEADER, IDEC, NC
      DOUBLE PRECISION    SIGN, TEN
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      X = 0.0D0
      IF (KBP.GT.KBPLIM) GO TO 980
      LEADER = 0
      IDEC = 0
      SIGN = 1.0D0
      EXSIGN = 1
      TEN = 10.0D0
      IEXP = 0
      NC = 0
C                                        skip leading blank,comma,+
 10   IF ((KB(KBP:KBP).NE.' ') .AND. (KB(KBP:KBP).NE.',') .AND.
     *   (KB(KBP:KBP).NE.'+')) GO TO 15
         KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 999
         GO TO 10
C                                        allow blank
 15   IF ((KBP+9.LE.KBPLIM) .AND. (KB(KBP:KBP+9).EQ.'''INDEFINT'''))
     *   THEN
         KBP = KBP + 10
         GO TO 990
         END IF
      IF ((KBP+5.LE.KBPLIM) .AND. (KB(KBP:KBP+5).EQ.'''INDE''')) THEN
         KBP = KBP + 6
         GO TO 990
         END IF
      KBP = KBP - 1
C                                        get chars
 20   KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 90
         IF (KB(KBP:KBP).NE.'.') GO TO 25
            IDEC = 1
            GO TO 20
C                                        unary minus
 25      IF (KB(KBP:KBP).NE.'-') GO TO 30
            IF (LEADER.NE.0) GO TO 40
            SIGN = -SIGN
            GO TO 20
C                                        other: leave if not num
 30      CONTINUE
            LEADER = 1
            IT = ICHAR(KB(KBP:KBP)) - ICHAR('0')
            IF ((IT.LT.0) .OR. (IT.GT.9)) GO TO 40
            NC = NC + 1
            X = X * TEN + IT
            IF (IDEC.NE.0) IDEC = IDEC + 1
            GO TO 20
C                                        exponential notation E or D
C                                        then (optional) blank,+,-
C                                        then integer number
 40   IF ((KB(KBP:KBP).NE.'E') .AND. (KB(KBP:KBP).NE.'D') .AND.
     *   (KB(KBP:KBP).NE.'e') .AND. (KB(KBP:KBP).NE.'d')) GO TO 90
         KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 90
         IF ((KB(KBP:KBP).NE.'-') .AND. (KB(KBP:KBP).NE.'+') .AND.
     *      (KB(KBP:KBP).NE.' ')) GO TO 50
            IF (KB(KBP:KBP).EQ.'-') EXSIGN = -1
 45         KBP = KBP + 1
            IF (KBP.GT.KBPLIM) GO TO 90
 50            IT = ICHAR(KB(KBP:KBP)) - ICHAR('0')
               IF ((IT.LT.0) .OR. (IT.GT.9)) GO TO 90
                  IEXP = 10*IEXP + IT
                  NC = NC + 1
                  GO TO 45
C                                        got it: apply sign, decimal pl
 90   IF (NC.LE.0) GO TO 980
         IDEC = MAX (0, IDEC-1)
         IDEC = EXSIGN*IEXP - IDEC
         IF (IDEC.GT.38) GO TO 980
            IF (IDEC.GT.-34) THEN
               X = X * SIGN / (TEN**(-IDEC))
C                                       actual IEEE limit around 2.0e38
C                                       for single precision storage
               IF (ABS(X).GT.1.E35) GO TO 980
            ELSE
               X = 0.0
               END IF
            GO TO 999
C                                        error
 980  ERRNUM = 27
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.10) PNAME(ERRLEV) = 'GETNUM'
C                                        allow blank
 990  X = DBLANK
C
 999  RETURN
      END
