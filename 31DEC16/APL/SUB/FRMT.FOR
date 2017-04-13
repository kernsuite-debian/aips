      INTEGER FUNCTION FRMT (FLDLEN, DECLEN, NUMBER, STRING)
C-----------------------------------------------------------------------
C! encode floating number removing trailing zeros, alter accuracy if nec
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001, 2015
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
C   FRMT encodes a floating point number into a string, then removes
C   trailing zeros (including decimal, if appropriate).  If the
C   requested format is inappropriate for the value, FRMT will alter
C   (increase or decrease) the actual DECLEN used.  Zero is handled as
C   a special case.
C   Inputs:
C      FLDLEN  I      total number of characters to use
C      DECLEN  I      number of characters after decimal point
C      NUMBER  R      number to be printed
C   Outputs:
C      STRING  C*(*)  string with number encoded
C      FRMT    I      number actual chars in string including leading
C                     blanks but not trailing
C-----------------------------------------------------------------------
      INTEGER   FLDLEN, DECLEN
      REAL      NUMBER
      CHARACTER STRING*(*)
C
      CHARACTER FORM2*12, IT*1
      REAL      XNUM, TENS
      INTEGER   IDMAX, IDEC, J, NUM, ID
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IDMAX = FLDLEN - 2 - DECLEN
      IF (NUMBER.LT.0.0) IDMAX = IDMAX - 1
      IDEC = DECLEN
      XNUM = ABS (NUMBER)
C                                       handle zero as special case
      IF (NUMBER.EQ.0.0) THEN
         J = FLDLEN - IDEC - 1
         WRITE (FORM2,1001) J
         J = 0
         WRITE (STRING,FORM2) J
         NUM = IDEC + 1
         FRMT = J
         GO TO 999
         END IF
C                                       more decimal places?
      IF (IDMAX.GT.0) THEN
         TENS = 10.0 ** MIN (0, 3-DECLEN)
         DO 10 ID = 1,IDMAX
            IF (XNUM.GE.TENS) THEN
               GO TO 20
            ELSE
               TENS = TENS / 10.0
               IDEC = IDEC + 1
               END IF
 10         CONTINUE
         IF (FLDLEN.GE.8) GO TO 800
         IF ((FLDLEN.GE.7) .AND. (NUMBER.GT.0.0)) GO TO 800
         END IF
C                                       fewer decimal places?
 20   ID = FLDLEN - 1 - IDEC
      IF (NUMBER.LT.0) ID = ID - 1
      TENS = 10.0**ID
 25   IF (XNUM.GE.TENS) THEN
         IF (IDEC.LE.0) GO TO 800
         IDEC = IDEC - 1
         TENS = TENS * 10.0
         GO TO 25
         END IF
C                                       encode format, then number
      IDEC = MIN (9, IDEC)
      WRITE (FORM2,1000) FLDLEN, IDEC
      WRITE (STRING,FORM2) NUMBER
C                                       remove trailing zeros
      J = FLDLEN
      NUM = 0
 50      IT = STRING(J:J)
         IF ((IT.NE.'0') .AND. (IT.NE.'.')) GO TO 60
            STRING(J:J) = ' '
            IF (IT.EQ.'.') GO TO 60
               NUM = NUM + 1
               J = J - 1
               GO TO 50
 60   FRMT = FLDLEN - NUM
      GO TO 999
C                                       number too big: put *'s
 800  IF ((FLDLEN.LT.7) .OR. ((FLDLEN.LT.8) .AND. (NUMBER.LT.0.0)))
     *   THEN
         DO 810 J = 1,FLDLEN
            STRING(J:J) = '*'
 810        CONTINUE
C                                       Exponential notation
      ELSE
         IDEC = FLDLEN - 6
         IF (NUMBER.LT.0.0) IDEC = IDEC - 1
         IDEC = MIN (9, IDEC)
         WRITE (FORM2,1800) FLDLEN, IDEC
         WRITE (STRING,FORM2) NUMBER
         END IF
      FRMT = FLDLEN
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('(F',I2.2,'.',I1,')')
 1001 FORMAT ('(I',I2.2,')')
 1800 FORMAT ('(1PE',I2.2,'.',I1,')')
      END
