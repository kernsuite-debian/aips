      SUBROUTINE CITEXP (S, IMAX, I, ZVALUE, IERR)
C-----------------------------------------------------------------------
C! KEYIN routine: evaluate an expression in a character string
C# Parsing
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
C   Function to evaluate arithmetic expressions.
C   Inputs:
C      S(IMAX)   C     Expression in char.
C      IMAX      I     Max. number of characters
C      I         I     Pointer in S,  updated by CITEXP
C   Outputs:
C      ZVALUE    D     Result of expression evaluation.
C      IERR      I     = 0  : success
C                        11 : too many parentheses
C                        12 : expecting ')'
C                        13 : missing value after operator
C                        14 : divide by zero
C-----------------------------------------------------------------------
      CHARACTER S*(*), PLUS*1, MINUS*1, STAR*1, SLASH*1, BRA*1, KET*1,
     *   OP(10)*1
      INTEGER   SIGN(10), IERR, IMAX, I, LEVEL, IP
      DOUBLE PRECISION    ZVALUE, RESULT(10), TERM(10), FACTOR(10),
     *   XVALUE
      DATA      PLUS, MINUS, STAR, SLASH, BRA, KET
     *         / '+',  '-',   '*',  '/',  '(', ')'/
C-----------------------------------------------------------------------
      IERR = 0
      LEVEL = 1
C                                       Evaluate 'expression'
  100 RESULT(LEVEL) = 0.0
  110 SIGN(LEVEL) = +1
      IF (S(I:I).NE.PLUS) GO TO 120
          I = I + 1
  120 IF (S(I:I).NE.MINUS) GO TO 130
          I = I + 1
          SIGN(LEVEL) = -1
  130 CONTINUE
C                                       Evaluate 'term'
      TERM(LEVEL) = 1.0
      OP(LEVEL) = STAR
  200 CONTINUE
C                                       Evaluate 'factor'
C                                       Parenthesized expression  =
C                                        'expression' at next level
      IF (S(I:I).NE.BRA) GO TO 330
         I = I + 1
         LEVEL = LEVEL + 1
         IF (LEVEL.LE.10) GO TO 100
             IERR = 11
             GO TO 999
  310    FACTOR(LEVEL-1) = RESULT(LEVEL)
         LEVEL = LEVEL - 1
         IF (S(I:I).EQ.KET) GO TO 320
              IERR = 12
              GO TO 999
  320     I = I + 1
          GO TO 340
  330  IP = I
      CALL CITC2R (S, IMAX, I, XVALUE)
      FACTOR(LEVEL) = XVALUE
       IF (I.NE.IP) GO TO 340
          IERR = 13
          GO TO 999
 340  CONTINUE
C                                       End 'factor'
      IF (OP(LEVEL).EQ.STAR)
     *   TERM(LEVEL) = TERM(LEVEL) * FACTOR(LEVEL)
      IF (OP(LEVEL).EQ.SLASH .AND. FACTOR(LEVEL).EQ.0.0) THEN
         IERR = 14
         GO TO 999
         END IF
      IF (OP(LEVEL).EQ.SLASH .AND. FACTOR(LEVEL).NE.0.0)
     *   TERM(LEVEL) = TERM(LEVEL) / FACTOR(LEVEL)
      IF (S(I:I).NE.SLASH.AND.S(I:I).NE.STAR) GO TO 350
          OP(LEVEL) = S(I:I)
          I = I + 1
          GO TO 200
  350 CONTINUE
C                                       End 'term'
      RESULT(LEVEL) = RESULT(LEVEL)+SIGN(LEVEL)*TERM(LEVEL)
      IF (S(I:I).EQ.PLUS.OR.S(I:I).EQ.MINUS) GO TO 110
C                                       End 'expression'
      IF (LEVEL.GT.1) GO TO 310
      ZVALUE = RESULT(LEVEL)
C
 999  RETURN
      END
