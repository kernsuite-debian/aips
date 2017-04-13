      SUBROUTINE POLISH
C-----------------------------------------------------------------------
C! parses the input text buffer, building stacks; executes pseudoverbs
C# POPS-lang
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
C   POLISH parses the character string in KARBUF translating to Polish
C   postfix notation.  Result is a sequence of integers representing
C   code for the POPS interpreter.  Negative tokens are operand pointers
C   while positive tokens are operation codes.  The array A (eqiv. to
C   STACK) holds these; AP points to the last entry.
C   POLISH also causes all pseudo verbs to be executed.
C-----------------------------------------------------------------------
      CHARACTER PRGNAM*6
      INTEGER   A(100), B(100), POTERR, NOTHNG, JTAG, MODIFY, ENDMOD,
     *   BRANCH, I
      LOGICAL   WASPS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      EQUIVALENCE (STACK(1), A(1)), (CSTACK(1), B(1))
      DATA NOTHNG /0/
      DATA PRGNAM /'POLISH'/
      DATA MODIFY, ENDMOD / 139, 2/
C-----------------------------------------------------------------------
      LEVEL = 0
      AP = 0
      BP = 0
      IFFLAG = 0
      LAST = NOTHNG
C                                       Parse tokens.
 10   CALL COMPIL
      IF (ERRNUM.NE.0) GO TO 980
      WASPS = .FALSE.
C                                       Pseudo operators.
      IF (SYTYPE.EQ.5) THEN
         JTAG = TAG
C                                       basic operators
         IF (JTAG.LT.120) THEN
            BRANCH = JTAG - 99
            CALL PSEUDO (BRANCH)
            IF (ERRNUM.NE.0) GO TO 980
            GO TO 10
            END IF
C                                       proc editing
         IF (JTAG.GE.130) GO TO 25
            BRANCH = JTAG - 119
            CALL EDITOR (BRANCH)
            IF (ERRNUM.NE.0) GO TO 980
            IF (MODE.NE.3) THEN
               WASPS = .TRUE.
               GO TO 65
               END IF
            JTAG = MODIFY
C                                       store code
 25      IF (JTAG.LT.150) THEN
            BRANCH = JTAG - 129
            CALL STORES (BRANCH)
            IF (ERRNUM.NE.0) GO TO 980
C                                       RESTORE/GET ... only 1 on line
            IF ((MODE.NE.3) .OR. (BRANCH.GT.5)) THEN
               WASPS = .TRUE.
               GO TO 65
               END IF
C                                       friendly verbs (imm. arg.)
         ELSE
            BRANCH = JTAG - 149
            CALL HELPS (BRANCH)
            IF (ERRNUM.NE.0) GO TO 980
C                                       RUN only one on line
            IF (BRANCH.EQ.1) THEN
               WASPS = .TRUE.
               GO TO 65
               END IF
            END IF
         GO TO 10
         END IF
C                                       Add OMEGA operator.
 50   CALL PUSH (A, AP, 1)
      IF (ERRNUM.NE.0) GO TO 980
      POTERR = 3
      IF (LEVEL.NE.0) GO TO 980
 65   IF ((MODE.NE.0) .AND. (AP.NE.0)) CALL STORES (1)
      IF (ERRNUM.NE.0) GO TO 980
C                                       Finish Modify
      IF (MODE.EQ.3) THEN
         BRANCH = ENDMOD
         CALL EDITOR (BRANCH)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
C                                       block execution after pseudos
 70   IF ((.NOT.WASPS) .OR. (MODE.NE.0)) GO TO 85
         IF (AP.GT.1) GO TO 80
         IF ((KBPTR.LE.1) .OR. (KBPTR.GT.KARLIM)) GO TO 85
            DO 75 I = KBPTR,KARLIM
               IF ((KARBUF(I:I).NE.' ') .AND. (KARBUF(I:I).NE.';'))
     *            GO TO 80
 75            CONTINUE
            GO TO 85
 80      CONTINUE
            WRITE (MSGTXT,1080)
            CALL MSGWRT (6)
 85   IF (WASPS) AP = 0
      IF (AP.EQ.0) CALL PUSH (A, AP, 1)
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C                                       Error return.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1080 FORMAT ('A PSEUDOVERB HAS CAUSED THE VERBS ON THE LINE TO BE ',
     *   'IGNORED')
      END
