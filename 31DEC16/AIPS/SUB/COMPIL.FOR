      SUBROUTINE COMPIL
C-----------------------------------------------------------------------
C! parses line of input with GETFLD, builds stacks for execution
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   COMPIL parses a line of input (using GETFLD) and builds the
C   stacks to be executed.  It exits when a pseudo-verb or the end
C   of the line is encountered.
C-----------------------------------------------------------------------
      INTEGER   PR(20), BPR(100), EQ, APIN, NSYMS, NUSYMS, IXT, A(100),
     *   B(100), I, IT, J, MAXLEV, N, NOTHNG, POTERR
      LOGICAL   WASP, TWIDDL
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      EQUIVALENCE (BPR(1), V(1)), (STACK(1), A(1)), (CSTACK(1), B(1))
      DATA MAXLEV /10/,     NOTHNG /0/
      DATA PR/ 0,0,0,1,5, 5,6,6,7,3, 3,0,8,2,4, 4,3,2,2,0/
C                ( ) = +  - * / **>  <   - ^ TO BY= | & ;
      DATA PRGNAM /'COMPIL'/
C-----------------------------------------------------------------------
      EQ = -2
      NSYMS = -2
      NUSYMS = -2
      WASP = .FALSE.
      TWIDDL = .FALSE.
      APIN = AP + 1
 10   CALL GETFLD
      IF (ERRNUM.NE.0) GO TO 980
      IF ((NKAR.EQ.0) .OR. (TAG.EQ.0) .OR. (SYTYPE.EQ.5)) GO TO 100
      IF (LEVEL.EQ.0) NSYMS = NSYMS + 1
      NUSYMS = NUSYMS + 1
      IF ((SYTYPE.EQ.3) .OR. (SYTYPE.EQ.4)) GO TO 50
C-----------------------------------------------------------------------
C                  O P E R A N D    P R O C E S S O R
C-----------------------------------------------------------------------
C                                       insert comma between adverbs
      IF (LAST.GT.0) GO TO 20
         NEXTP = 1 + LEVEL
         CALL BCLEAN
         IF (ERRNUM.NE.0) GO TO 980
         IF ((NSYMS.LE.0) .OR. (LAST.GE.0) .OR. (LEVEL.GT.0) .OR.
     *      (WASP)) GO TO 20
            EQ = EQ + 1
            IF (EQ.GT.0) GO TO 20
               IF (EQ.EQ.0) AP = AP - 1
               EQ = 2
C                                       Operand is hollerith (no subs)
 20   IF (SYTYPE.NE.14) GO TO 25
         N = K(LOCSYM+1)/16
         GO TO 30
C                                       Operand is an array or string.
 25   IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.7)) GO TO 35
C                                       Check if in array subscipt mode
         N = K(LOCSYM+3)
C                                        stack gets -(2,tag,size,type)
 30   CONTINUE
         IXT = -SYTYPE
         CALL PUSH (A, AP, IXT)
         IF (ERRNUM.NE.0) GO TO 980
         IXT = -N
         CALL PUSH (A, AP, IXT)
         IF (ERRNUM.NE.0) GO TO 980
         IXT = -TAG
         CALL PUSH (A, AP, IXT)
         IF (ERRNUM.NE.0) GO TO 980
         IXT = -2
         CALL PUSH (A, AP, IXT)
         IF (ERRNUM.NE.0) GO TO 980
         GO TO 40
C                                       All other operands.
 35   CONTINUE
C                                       Check if in array subscipt mode
C                                       Check on maximum dimensions.
            POTERR = 6
 37      CONTINUE
         IXT = -TAG
         CALL PUSH (A, AP, IXT)
         IF (ERRNUM.NE.0) GO TO 980
 40   LAST = -TAG
      GO TO 10
C-----------------------------------------------------------------------
C                  O P E R A T O R    P R O C E S S O R
C-----------------------------------------------------------------------
C                                       Comma (,) operator.
 50   IF (TAG.NE.1) GO TO 55
         LAST = NOTHNG
         NEXTP = 1 + LEVEL
         CALL BCLEAN
         IF (ERRNUM.NE.0) GO TO 980
C                                           array assign?
         IF ((LEVEL.GT.0) .OR. (EQ.EQ.0) .OR. (WASP)) GO TO 10
C                                           yes: count
            EQ = EQ + 1
            IF (EQ.GT.0) GO TO 10
               IF (EQ.EQ.0) AP = AP - 1
               EQ = 2
            GO TO 10
C                                       Left Paren "(" operator.
 55   IF (TAG.NE.2) GO TO 65
         IF (LEVEL.EQ.0) NSYMS = NSYMS - 1
         NUSYMS = NUSYMS - 1
         LEVEL = LEVEL + MAXLEV
         IF (LAST.GT.0) GO TO 10
         IF (A(AP).NE.-2) GO TO 10
C                                           array subscript
            J = LEVEL - MAXLEV + 9
            CALL PUSH (B, BP, 98)
            IF (ERRNUM.NE.0) GO TO 980
            BPR(BP) = J
            DO 60 I = 1,4
               CALL POP (A, AP, IT)
               IF (ERRNUM.NE.0) GO TO 980
               CALL PUSH (B, BP, IT)
               IF (ERRNUM.NE.0) GO TO 980
               BPR(BP) = J
 60            CONTINUE
            GO TO 10
C                                       Right paren ")".
 65   IF (TAG.EQ.3) THEN
         LEVEL = LEVEL - MAXLEV
         NUSYMS = NUSYMS - 1
         GO TO 10
         END IF
C                                       Semi colon ";".
 70   IF (TAG.NE.20) GO TO 80
         NEXTP = 0
         IFFLAG = 0
         CALL BCLEAN
         IF (ERRNUM.NE.0) GO TO 980
         LAST = NOTHNG
C                                       push array operator
         IF ((EQ.GT.0) .OR. (TWIDDL)) THEN
            IF (A(AP).EQ.4) AP = AP - 1
            IXT = -MAX (1, EQ)
            CALL PUSH (A, AP, IXT)
            IF (ERRNUM.NE.0) GO TO 980
            IF (TWIDDL) THEN
               CALL PUSH (A, AP, 29)
            ELSE
               CALL PUSH (A, AP, 27)
               END IF
            IF (ERRNUM.NE.0) GO TO 980
            GO TO 75
            END IF
C                                       Do we have an operator?
 72      IF (APIN.GE.AP) GO TO 75
            IF (IFFLAG.EQ.1) GO TO 75
            DO 73 I = APIN,AP
               IF ((A(I).GT.50) .AND. (A(I).NE.98)) GO TO 75
               IF ((A(I).GE.21) .AND. (A(I).LE.30)) GO TO 75
               IF ((A(I).EQ.4) .OR. (A(I).EQ.41)) GO TO 75
 73            CONTINUE
C                                       No: stick on = for dummies
            CALL PUSH (A, AP, 4)
            IF (ERRNUM.NE.0) GO TO 980
 75      EQ = -2
         NSYMS = -2
         NUSYMS = -2
         WASP = .FALSE.
         TWIDDL = .FALSE.
         APIN = AP + 1
         GO TO 10
C                                        unary operator
 80   IF ((LAST.LT.0) .AND. (NUSYMS.GT.0)) GO TO 85
         IF ((LAST.LT.0) .AND. (NUSYMS.EQ.0) .AND. (IFFLAG.EQ.1))
     *      GO TO 85
C                                          Unary plus "+".
         IF (TAG.EQ.5) GO TO 10
C                                          Unary minus "-".
         IF (TAG.EQ.6) TAG = 13
C                                       "=" . . . compare or store?
C                                       Twiddle "="
 85   IF ((TAG.EQ.4) .OR. (TAG.EQ.29)) THEN
         IF ((IFFLAG.NE.1) .AND. (EQ.LT.0)) EQ = -1
         IF (IFFLAG.EQ.1) TAG = 17
         IF (TAG.EQ.29) THEN
            TWIDDL = .TRUE.
            TAG = 4
            END IF
         IFFLAG = 1
         END IF
C                                       All other operators.
90    CONTINUE
         NEXTP = LEVEL
         IF (TAG.EQ.24) IFFLAG = 1
         IF (TAG.LT.21) NEXTP = LEVEL + PR(TAG)
         IF ((TAG.GT.30) .OR. (TAG.EQ.26)) NEXTP = LEVEL + 9
         IF (TAG.EQ.41) NEXTP = LEVEL
         IF ((TAG.GE.42) .AND. (TAG.LE.44)) NEXTP = LEVEL + PR(11)
         IF (TAG.EQ.46) NEXTP = LEVEL + PR(5)
         IF (TAG.EQ.47) NEXTP = LEVEL + PR(15)
         CALL BCLEAN
         IF (ERRNUM.NE.0) GO TO 980
         CALL PUSH (B, BP, TAG)
         IF (ERRNUM.NE.0) GO TO 980
         BPR(BP) = NEXTP
         LAST = TAG
         IF ((TAG.EQ.23) .OR. (TAG.EQ.24)) WASP = .TRUE.
         GO TO 10
C-----------------------------------------------------------------------
C                                        final clean up
C-----------------------------------------------------------------------
 100  NEXTP = 0
      CALL BCLEAN
      IF (ERRNUM.NE.0) GO TO 980
      IF ((EQ.GT.0) .OR. (TWIDDL)) THEN
         IF (A(AP).EQ.4) AP = AP - 1
         IXT = -MAX (1, EQ)
         CALL PUSH (A, AP, IXT)
         IF (ERRNUM.NE.0) GO TO 980
         IF (TWIDDL) THEN
            CALL PUSH (A, AP, 29)
         ELSE
            CALL PUSH (A, AP, 27)
            END IF
         IF (ERRNUM.NE.0) GO TO 980
         GO TO 999
         END IF
C                                       Do we have an operator?
 105  IF (APIN.GE.AP) GO TO 999
         IF (IFFLAG.EQ.1) GO TO 999
         DO 110 I = APIN,AP
            IF ((A(I).GT.50) .AND. (A(I).NE.98)) GO TO 999
            IF ((A(I).GE.21) .AND. (A(I).LE.30)) GO TO 999
            IF ((A(I).EQ.4) .OR. (A(I).EQ.41)) GO TO 999
 110        CONTINUE
C                                       No: stick on = for dummies
         CALL PUSH (A, AP, 4)
         IF (ERRNUM.EQ.0) GO TO 999
C                                        error return
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
  999 RETURN
      END
