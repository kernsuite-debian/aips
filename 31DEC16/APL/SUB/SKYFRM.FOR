      SUBROUTINE SKYFRM (DPOS, PREFIX, CTYPE, IACC, STRING, ILEN, ITYP)
C-----------------------------------------------------------------------
C! returns string with character representation of a corrdinate
C# Coordinates
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
C   SKYFRM will figure out the type (RA, DEC, OTHER) of a sky coordinate
C   and format, zero fill, and interpret the prefix to provide a string
C   representation suitable for printing.
C   Inputs:
C      DPOS    D     coordinate value in degrees.
C      PREFIX  L     T means include prefix in sting, F means no.
C      CTYPE   C*8   axis type
C      IACC    I     accuracy of seconds (types 1 and 2 only). 0
C                    means no decimal fraction. > 0 => # decimal places
C                    for RA (one fewer for DEC)
C   Outputs:
C      STRING  C*80  string containing packed character representation
C                    of the coordinate.
C      ILEN    I     length in characters of STRING.
C      ITYP    I     1 = RA; 2 = DEC; 3 = other.
C-----------------------------------------------------------------------
      DOUBLE PRECISION DPOS
      LOGICAL   PREFIX
      CHARACTER CTYPE*8, STRING*(*)
C
      CHARACTER FORM2*20, SPTXT*20, CTEMP*4, RA(2)*4, DEC(2)*4,
     *   CHSIGN*1
      REAL      SEC, TEMP
      INTEGER   IACC, ILEN, ITYP, HM(3), IACC2, IFLEN, IFLDLN, ITRY,
     *   FRMT
      DATA RA, DEC /'RA  ','RA--', 'DEC ','DEC-'/
C-----------------------------------------------------------------------
C                                       Figure out which coordinate.
      ITYP = 3
      CTEMP = CTYPE(1:4)
      IF ((CTEMP.EQ.RA(1)) .OR. (CTEMP.EQ.RA(2))) ITYP = 1
      IF (CTEMP.EQ.'LL') ITYP = 1
      IF ((CTEMP.EQ.DEC(1)) .OR. (CTEMP.EQ.DEC(2))) ITYP = 2
      IF (CTEMP.EQ.'MM') ITYP = 2
C                                       Generate prefix.
      ILEN = 1
      IF (.NOT.PREFIX) GO TO 20
         ILEN = ITYP + 1
         IF (ITYP.EQ.1) STRING = RA(1)
         IF (ITYP.EQ.2) STRING = DEC(1)
         IF (ITYP.EQ.3) STRING = CTYPE
         IF (ITYP.EQ.3) CALL CHTRIM (STRING, 8, STRING, ILEN)
C                                       Put blank after prefix.
         ILEN = ILEN + 1
         STRING(ILEN:ILEN) = ' '
         ILEN = ILEN + 1
C                                       Convert degrees.
C                                       RA or DEC type.
 20   IF (ITYP.EQ.3) GO TO 40
         CALL COORDD (ITYP, DPOS, CHSIGN, HM, SEC)
C                                       No decimal.
         IACC2 = MIN (IACC + 1 - ITYP, 9)
         IF (IACC2.LE.0) THEN
            HM(3) = SEC + .5
            WRITE (SPTXT,1020) CHSIGN, HM
C                                       Decimal point.
         ELSE
            IFLDLN = IACC2 + 4
            WRITE (FORM2,1025) IFLDLN, IACC2
            IFLDLN = IFLDLN + 6
            WRITE (SPTXT,FORM2) CHSIGN, HM(1), HM(2), SEC
            END IF
         GO TO 50
C                                       Not ra, dec.
 40   CONTINUE
         IFLDLN = 16
         TEMP = DPOS
         ITRY = FRMT (IFLDLN, 4, TEMP, SPTXT)
C                                       Put pieces together.
 50   CALL CHTRIM (SPTXT, IFLDLN, SPTXT, IFLEN)
      STRING(ILEN:ILEN+IFLEN-1) = SPTXT(1:IFLEN)
      ILEN = ILEN + IFLEN - 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT (A1,I2.2,2I3.2)
 1025 FORMAT ('(A1,I2.2,I3.2,F',I2,'.',I1,')')
      END
