      SUBROUTINE SELSTR (INP, OPTYPE, TWOOF, ABSEM, TYPEAX, ISUBAX, XC,
     *   XO, XP, COLTYP, OUTP, TEXT)
C-----------------------------------------------------------------------
C! builds string displaying the functions applied to columns of table
C# EXT-util
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
C   SELSTR builds a selection string displaying the function applied
C   to one or two columns of a table
C   Inputs:
C      INP     I       Start position in string
C      OPTYPE  I       Function type
C      TWOOF   L       T => 2 columns involved
C      ABSEM   L(2)    Was the absolute value of the cols taken
C      TYPEAX  I(2)    Column number(s)
C      ISUBAX  I(2)    Column array subscript(s)
C      XC      R(4)    Col A factor, offset; Col B factor, offset
C      XO      R(2)    Overall factor, offset
C      XP      R       Power to take result to
C      COLTYP  C(2)*8  Column alpha labels
C   Output:
C      OUTP    I       Next available character after this op.
C                         (Can be same as INP)
C      TEXT    C*(*)   Packed string chars INP to OUTP-1 changed
C-----------------------------------------------------------------------
      INTEGER   INP, OPTYPE, TYPEAX(2), ISUBAX(2), OUTP
      LOGICAL   TWOOF, ABSEM(2)
      REAL      XO(2), XC(4), XP
      CHARACTER COLTYP(2)*8, TEXT*(*)
C
      CHARACTER TTXT*80, BRAKS*4, CHFUN(19)*4
      INTEGER   IP, NCH, FLDLEN, DECLEN, I, FRMT
      LOGICAL   DOBRAK
      DATA BRAKS /'{[]}'/
      DATA CHFUN /'SUM ','ASIN','LOG ','LN  ','EXP ','SIN ','COS ',
     *   'TAN ','ATAN','+   ','-   ','*   ','/   ','**  ','MOD ',
     *   'MODU','ATN2','MAX ','MIN '/
C-----------------------------------------------------------------------
      FLDLEN = 20
      DECLEN = 4
      IP = INP
C                                       Overall multiplier and constant
      DOBRAK = .FALSE.
      IF ((XO(1).EQ.1.0) .AND. (XO(2).EQ.0.0)) GO TO 10
         DOBRAK = .TRUE.
         IF (XO(1).EQ.1.0) GO TO 20
            I = FRMT (FLDLEN, DECLEN, XO(1), TTXT)
            CALL CHTRIM (TTXT, I, TTXT, NCH)
            TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
            IP = IP + NCH
            TEXT(IP:IP) = '*'
            IP = IP + 1
C                                       Open bracket for power
 10   IF (XP.EQ.1.0) GO TO 20
         TEXT(IP:IP) = BRAKS(1:1)
         IP = IP + 1
C                                       single column function
 20   IF ((OPTYPE.LE.0) .OR. (TWOOF)) GO TO 30
         TEXT(IP:IP+3) = CHFUN(OPTYPE)
         IP = IP + 3
         CALL CHTRIM (TEXT, IP, TEXT, NCH)
         IP = NCH + 2
 30   IF (.NOT.DOBRAK) GO TO 40
         TEXT(IP:IP) = BRAKS(1:1)
         IP = IP + 1
C                                       col A multiplier
 40   IF (XC(1).EQ.1.0) GO TO 50
         I = FRMT (FLDLEN, DECLEN, XC(1), TTXT)
         CALL CHTRIM (TTXT, I, TTXT, NCH)
         TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
         IP = IP + NCH
         TEXT(IP:IP) = '*'
         IP = IP + 1
 50   IF (.NOT.ABSEM(1)) GO TO 60
         TEXT(IP:IP+3) = 'ABS '
         IP = IP + 4
 60   TEXT(IP:IP+7) = COLTYP(1)
      IP = IP + 7
      CALL CHTRIM (TEXT, IP, TEXT, NCH)
      IP = NCH + 1
      IF (TYPEAX(1).LE.0) GO TO 65
         TEXT(IP:IP) = ' '
         IP = IP + 1
         TEXT(IP:IP) = BRAKS(2:2)
         IP = IP + 1
         WRITE (TTXT,1060) TYPEAX(1)
         CALL CHTRIM (TTXT, 4, TTXT, NCH)
         TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
         IP = IP + NCH
         WRITE (TTXT,1061) ISUBAX(1)
         CALL CHTRIM (TTXT, 4, TTXT, NCH)
         TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
         IP = IP + NCH
         TEXT(IP:IP) = BRAKS(3:3)
         IP = IP + 1
 65   IF (XC(2).EQ.0.0) GO TO 70
         IF (XC(2).LT.0.0) GO TO 66
            TEXT(IP:IP) = '+'
            IP = IP + 1
 66      I = FRMT (FLDLEN, DECLEN, XC(2), TTXT)
         CALL CHTRIM (TTXT, I, TTXT, NCH)
         TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
         IP = IP + NCH
C                                       2-col functions
 70   IF (.NOT.TWOOF) GO TO 100
         TEXT(IP:IP) = ' '
         IP = IP + 1
         TEXT(IP:IP+3) = CHFUN(OPTYPE)
         IP = IP + 3
         CALL CHTRIM (TEXT, IP, TEXT, NCH)
         IP = NCH + 2
         TEXT(IP:IP) = ' '
         IP = IP + 1
C                                       col B multiplier
         IF (XC(3).EQ.1.0) GO TO 80
            I = FRMT (FLDLEN, DECLEN, XC(3), TTXT)
            CALL CHTRIM (TTXT, I, TTXT, NCH)
            TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
            IP = IP + NCH
            TEXT(IP:IP) = '*'
            IP = IP + 1
 80      IF (.NOT.ABSEM(2)) GO TO 90
            TEXT(IP:IP+3) = 'ABS '
            IP = IP + 4
 90      TEXT(IP:IP+7) = COLTYP(2)
         IP = IP + 7
         CALL CHTRIM (TEXT, IP, TEXT, NCH)
         IP = NCH + 1
         IF (TYPEAX(2).LE.0) GO TO 95
            TEXT(IP:IP) = ' '
            IP = IP + 1
            TEXT(IP:IP) = BRAKS(2:2)
            IP = IP + 1
            WRITE (TTXT,1060) TYPEAX(2)
            CALL CHTRIM (TTXT, 4, TTXT, NCH)
            TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
            IP = IP + NCH
            WRITE (TTXT,1061) ISUBAX(2)
            CALL CHTRIM (TTXT, 4, TTXT, NCH)
            TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
            IP = IP + NCH
            TEXT(IP:IP) = BRAKS(3:3)
            IP = IP + 1
 95      IF (XC(4).EQ.0.0) GO TO 100
            IF (XC(2).LT.0.0) GO TO 96
               TEXT(IP:IP) = '+'
               IP = IP + 1
 96         I = FRMT (FLDLEN, DECLEN, XC(4), TTXT)
            CALL CHTRIM (TTXT, I, TTXT, NCH)
            TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
            IP = IP + NCH
C                                       col to power function
 100  IF (XP.EQ.1.0) GO TO 110
         TEXT(IP:IP+2) = '}**'
         IP = IP + 3
         I = FRMT (FLDLEN, DECLEN, XP, TTXT)
         CALL CHTRIM (TTXT, I, TTXT, NCH)
         TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
         IP = IP + NCH
C                                       Close overall brackets
 110  IF (.NOT.DOBRAK) GO TO 120
         TEXT(IP:IP) = BRAKS(4:4)
         IP = IP + 1
C                                       Overall offset
 120  IF (XO(2).EQ.0.0) GO TO 130
         IF (XO(2).LT.0.0) GO TO 125
            TEXT(IP:IP) = '+'
            IP = IP + 1
 125     I = FRMT (FLDLEN, DECLEN, XO(2), TTXT)
         CALL CHTRIM (TTXT, I, TTXT, NCH)
         TEXT(IP:IP+NCH-1) = TTXT(1:NCH)
         IP = IP + NCH
C                                       return length
 130  OUTP = IP
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT (I3,'(')
 1061 FORMAT (I3,')')
      END
