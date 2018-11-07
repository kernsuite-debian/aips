      SUBROUTINE EDITOR (BRANCH)
C-----------------------------------------------------------------------
C! does operations needed at start and end of editing existing procedure
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
C   EDITOR performs the operations required to begin and to stop
C   editing an existing procedure.
C   Inputs:
C      BRANCH   I  opcode: 1 - EDIT
C                          2 - ENDEDIT
C                          3 - MODIFY
C                          4 - ERASE
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
      CHARACTER PRGNAM*6
      REAL      EPSILN
      INTEGER   POTERR, TSTACK(100), BPR(100), A(100), B(100), REASON,
     *   I, IT, LL, NT, LL1, NTHEN, NWHILE, NELSE, NEND, NENDR, NFOR
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      EQUIVALENCE (BPR (1), V(1)), (STACK(1), A(1)),  (CSTACK(1), B(1))
      DATA PRGNAM /'EDITOR'/
      DATA EPSILN /1.0E-6/
      DATA NTHEN, NWHILE, NELSE, NEND, NENDR, NFOR
     *   /   107,    109,   106,   22,    96,   21/
C-----------------------------------------------------------------------
      POTERR = 76
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.4)) GO TO 980
      GO TO (100, 200, 100, 100), BRANCH
C-----------------------------------------------------------------------
C                                       E D I T  <name>  <line>
C                                       M O D I F Y  <name>  <line>
C                                       D E L E T E <name> <lin1:lin2>
C-----------------------------------------------------------------------
C                                        get procedure name
 100  POTERR = 71
      IF (MODE.EQ.1) GO TO 980
      POTERR = 0
C                                       MODIFY is interactive only
      IF (BRANCH.EQ.3) THEN
         IF ((TSKNAM(1:5).EQ.'AIPSC') .OR. (NPOPS.GT.NINTRN) .OR.
     *      (ISBTCH.EQ.32000)) POTERR = 60
         IF (IUNIT.NE.1) POTERR = 53
         IF (POTERR.NE.0) GO TO 980
         END IF
      CALL GETFLD
      IF ((ERRNUM.NE.0) .OR. ((SYTYPE.NE.3) .AND. (SYTYPE.NE.4))) THEN
         WRITE (MSGTXT,1100) 'FIRST'
         CALL MSGWRT (6)
         IF (ERRNUM.EQ.0) POTERR = 11
         GO TO 980
         END IF
C                                        protect VLAGEN'd ones
      POTERR = 25
      IF (L.LT.K(7)) GO TO 980
      IP = TAG
      NAMEP = LOCSYM + 2
C                                        get line number
      CALL GETFLD
      IF ((ERRNUM.NE.0) .OR. (SYTYPE.NE.11)) THEN
         WRITE (MSGTXT,1100) 'SECOND'
         CALL MSGWRT (6)
         IF (ERRNUM.EQ.0) POTERR = 23
         GO TO 980
         END IF
      IPT = ';'
      LL = X(1)
      IF (BRANCH.EQ.3) X(1) = LL
      LL1 = LL
C                                       2nd line number on ERASE
      IF (BRANCH.EQ.4) THEN
         CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 980
         IF ((TAG.GT.0) .AND. (TAG.NE.20)) THEN
            POTERR = 8
            IF (TAG.NE.15) GO TO 980
            CALL GETFLD
            IF (ERRNUM.NE.0) GO TO 980
            POTERR = 23
            IF (SYTYPE.NE.11) GO TO 980
            LL = X(1)
            IF (LL.LT.LL1) THEN
               I = LL1
               LL1 = LL
               LL = I
               END IF
            END IF
C                                       Check for line 1
         IF (LL1.LE.1) THEN
            WRITE (MSGTXT,1104)
            CALL MSGWRT (8)
            POTERR = 25
            GO TO 980
            END IF
         END IF
      DO 110 I = 1,LL
         IF (I.LE.LL1) LP = IP
         IP = K(IP)
         IF (IP.EQ.0) GO TO 120
 110     CONTINUE
C
 120  IF (IP.NE.0) IP = K(IP)
      XX = LL
      IF ((ABS(X(1)-XX).GT.EPSILN) .AND. (BRANCH.NE.4)) LP = K(LP)
      MODE = 1
      IF (BRANCH.EQ.3) MODE = 3
      LPGM = LP
      AP = 0
      IF (BRANCH.NE.4) GO TO 999
C-----------------------------------------------------------------------
C                                        ENDEDIT
C-----------------------------------------------------------------------
 200  MODE = 0
      AP = 0
      K(LPGM) = IP
C                                       Just like FINISH in PSEUDO.
      NT = 0
      POTERR = 11
      IF ((NAMEP.LT.200) .OR. (NAMEP.GT.KXORG)) GO TO 980
      L = K(NAMEP)
      POTERR = 11
      IF (L.EQ.0) GO TO 980
      NAMEP = 0
 210  LINK = K(L)
         L = L + 1
 215     L = L + 1
            IF (K(L).EQ.1) GO TO 240
C                                          THEN or WHILE.
            IF ((K(L).NE.NTHEN) .AND. (K(L).NE.NWHILE)) GO TO 225
 220           CALL PUSH (TSTACK, NT, L)
               IF (ERRNUM.NE.0) GO TO 980
               CALL PUSH (TSTACK, NT, K(L))
               IF (ERRNUM.NE.0) GO TO 980
               GO TO 215
C                                          ELSE or END.
 225        IF ((K(L).NE.NELSE) .AND. (K(L).NE.NEND) .AND.
     *         (K(L).NE.NENDR)) GO TO 230
               POTERR = 21
               IF (NT.EQ.0) GO TO 980
               CALL POP (TSTACK, NT, REASON)
               IF (ERRNUM.NE.0) GO TO 980
               IF (((REASON.EQ.NELSE) .OR. (REASON.EQ.NTHEN)) .AND.
     *            (K(L).EQ.NEND)) K(L) = NENDR
               CALL POP (TSTACK, NT, IT)
               IF (ERRNUM.NE.0) GO TO 980
               IF (IT.EQ.0) GO TO 215
                  K(IT-2) = -L-1
                  K(IT-1) = -LINK
                  IF (LINK.LE.0) K(IT-1) = -K(3)
                  IF (K(L).EQ.NELSE) GO TO 220
                  GO TO 215
C                                          FOR.
 230        IF (K(L).NE.NFOR) GO TO 215
               CALL PUSH (TSTACK, NT, 0)
               IF (ERRNUM.NE.0) GO TO 980
               CALL PUSH (TSTACK, NT, K(L))
               IF (ERRNUM.NE.0) GO TO 980
               GO TO 215
C                                        program chunk
 240     L = LINK
         IF (L.NE.0) GO TO 210
      POTERR = 21
      IF (NT.NE.0) GO TO 980
      IPT = '>'
      IF (MODE.NE.0) THEN
         CALL PUSH (A, AP, 1)
         IF (ERRNUM.NE.0) GO TO 980
         END IF
 250  POTERR = 20
      IF (LEVEL.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                          Error return.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('PROBLEM WITH ',A,' ARGUMENT OF EDIT, MODIFY, OR DELETE')
 1104 FORMAT ('ERASE LINE 1 NOT ALLOWED: DO YOU WANT SCRATCH ?')
      END
