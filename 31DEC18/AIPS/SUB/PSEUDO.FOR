      SUBROUTINE PSEUDO (BRANCH)
C-----------------------------------------------------------------------
C! compiles pseudoverbs: PROC, declarations, IF, THEN, WHILE, FINISH, ..
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
C   PSEUDO handles procedure and array declarations, setups for run-
C   time operators IF, THEN, ELSE, WHILE (which require forward
C   references and an additional clean-up pass), and the FINISH
C   operator.
C   Inputs:
C      BRANCH   I   pseudoverb number: specifies desired operation
C                      PROCEDURE     1
C                      PROC          1
C                      SCALAR        2
C                      VERB          3
C                      PSEUDOVRB     4
C                      ARRAY         5
C                      STRING        6
C                      ELSE          7
C                      THEN          8
C                      IF            9
C                      WHILE        10
C                      FINISH       11
C                      DEBUG        12
C                      MSGKILL      13
C                      ISBATCH      14
C-----------------------------------------------------------------------
      INTEGER   BRANCH
C
C                                  push down stack for clean-up
C                                  "FINISH" operation
      INTEGER   BPR(100), TSTACK(100)
C                                  reason why an item was pushed down
C                                  during clean-up
      INTEGER   REASON
      INTEGER   A(100), B(100), TAGS, I, ISGN, ISIZE, IT, K3SAVE,
     *   KXLINK, LL, LLI, LS, MM, N, NARG, NCH, NDIM, NOTHNG, NP, NSIZE,
     *   NT, LMODE, LLOCAT, RLOCAT, RETNUM, POTERR, NTHEN, NWHILE,
     *   NELSE, NEND, NENDR, NFOR
      LOGICAL   PREVDF
      CHARACTER PRGNAM*6
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      EQUIVALENCE (BPR(1), V(1))
      EQUIVALENCE (STACK(1), A(1)),    (CSTACK(1), B(1))
      DATA NOTHNG, RETNUM /0, 25/
      DATA PRGNAM /'PSEUDO'/
      DATA NTHEN, NWHILE, NELSE, NEND, NENDR, NFOR
     *   /   107,    109,   106,   22,    96,   21/
C-----------------------------------------------------------------------
      POTERR = 71
      IF ((MODE.EQ.1) .AND. (BRANCH.EQ.1)) GO TO 980
      POTERR = 70
      IF ((MODE.EQ.0) .AND. (BRANCH.GT.1) .AND. (BRANCH.LT.12))
     *   GO TO 980
      POTERR = 76
      IF ((BRANCH.LT.1) .OR. (BRANCH.GT.14)) GO TO 980
C
      GO TO (100, 150, 160, 160, 200, 250, 300, 310, 320, 350, 400,
     *   500, 500, 500), BRANCH
C-----------------------------------------------------------------------
C                                       PROC name (arg1, arg2, ...)
C-----------------------------------------------------------------------
 100  LMODE = MODE
      MODE = 69
      IPT = ':'
      CALL GETFLD
      MODE = 1
      IF (ERRNUM.NE.0) GO TO 980
      POTERR = 14
      IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.3)) GO TO 980
C                                        protect VLAGEN'd procs
      POTERR = 25
      IF (L.LT.K(7)) GO TO 980
      NAMEP = LOCSYM + 2
      NARG = 0
 105  CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 980
         IF (TAG.EQ.1) GO TO 105
         IF (TAG.EQ.2) GO TO 105
         IF (TAG.EQ.3) GO TO 110
         IF ((TAG.EQ.0) .OR. (TAG.EQ.20)) GO TO 110
            POTERR = 23
C                                        real parameters
            IF (SYTYPE.NE.1) GO TO 106
               CALL PUSH (TSTACK, NARG, TAG)
               IF (ERRNUM.NE.0) GO TO 980
               GO TO 105
C                                        string/array parameters
 106        IF ((SYTYPE.NE.2) .AND. (SYTYPE.NE.7)) GO TO 980
               CALL PUSH (TSTACK, NARG, SYTYPE)
               CALL PUSH (TSTACK, NARG, K(LOCSYM+3))
               CALL PUSH (TSTACK, NARG, TAG)
               CALL PUSH (TSTACK, NARG, 2)
               IF (ERRNUM.NE.0) GO TO 980
               GO TO 105
 110     CONTINUE
            N = NARG + 4
            LL = LLOCAT (N, K, LPGM)
            IF (ERRNUM.NE.0) GO TO 980
            LPGM = LL
            K(NAMEP) = LL
            CALL SETTYP (K, NAMEP, 3)
            LL = LL+2
            IF (NARG.EQ.0) GO TO 120
               DO 115 I = 1,NARG
                  K(LL) = -TSTACK(I)
                  LL = LL + 1
 115              CONTINUE
               K(LL) = 3
               LL = LL + 1
 120  K(LL) = 1
      GO TO 999
C-----------------------------------------------------------------------
C                                       SCALAR <name>, <name>, ...
C-----------------------------------------------------------------------
 150  POTERR = 36
      IF ((MODE.NE.1) .AND. (MODE.NE.3)) GO TO 980
      K3SAVE = K(3)
      LMODE = MODE
      MODE = 69
      CALL GETFLD
      MODE = LMODE
         IF (ERRNUM.NE.0) GO TO 980
         IF (TAG.EQ.1) GO TO 150
         IF ((TAG.EQ.0) .OR. (TAG.EQ.20)) GO TO 999
            POTERR = 14
            IF (SYTYPE.NE.1) GO TO 980
            IF (K3SAVE.LE.L) C(TAG) = 0.0
            GO TO 150
C-----------------------------------------------------------------------
C                                       VERB <name> = #, <name>, ...
C                                       PSEUDOVRB <name> = #, <name>, ..
C-----------------------------------------------------------------------
 160  POTERR = 36
      IF ((MODE.NE.1) .AND. (MODE.NE.3)) GO TO 980
      K3SAVE = K(3)
      LMODE = MODE
      MODE = 69
C                                       get verb name
      CALL GETFLD
      MODE = LMODE
         IF (ERRNUM.NE.0) GO TO 980
         IF (TAG.EQ.1) GO TO 160
         IF ((TAG.EQ.0) .OR. (TAG.EQ.20)) GO TO 999
         POTERR = 8
         IF ((SYTYPE.EQ.4) .AND. (BRANCH.EQ.4)) GO TO 980
         IF ((SYTYPE.EQ.5) .AND. (BRANCH.EQ.3)) GO TO 980
         IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.4) .AND. (SYTYPE.NE.5))
     *      GO TO 980
         PREVDF = (SYTYPE.EQ.4) .OR. (SYTYPE.EQ.5)
         POTERR = 14
         IF ((.NOT.PREVDF) .AND. (K3SAVE.GT.L)) GO TO 980
         POTERR = 25
         IF ((.NOT.PREVDF) .AND. (K(7).GT.L)) GO TO 980
         LS = L + 2
C                                       get = sign
         POTERR = 8
         CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 980
         IF ((TAG.NE.4) .AND. (TAG.NE.17)) GO TO 980
C                                       get value
         CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 980
C                                       allow a new number
         IF (SYTYPE.NE.11) GO TO 980
         IT = X(1) + 0.5
         IF ((IT.LT.1) .OR. (IT.GT.999)) GO TO 980
C                                       put in K array
         I = BRANCH + 1
         CALL SETTYP (K, LS, I)
         K(LS) = IT
         GO TO 160
C-----------------------------------------------------------------------
C                                       A R R A Y <name> <(n)>.
C-----------------------------------------------------------------------
 200  POTERR = 36
      IF ((MODE.NE.1) .AND. (MODE.NE.3)) GO TO 980
      K3SAVE = K(3)
      LMODE = MODE
      MODE = 69
      CALL GETFLD
      MODE = LMODE
      IF (ERRNUM.NE.0) GO TO 980
      IF (TAG.EQ.1) GO TO 200
      IF ((TAG.EQ.0) .OR. (TAG.EQ.20)) GO TO 999
         POTERR = 8
         IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.2)) GO TO 980
         PREVDF = SYTYPE.EQ.2
         POTERR = 14
         IF ((.NOT.PREVDF) .AND. (K3SAVE.GT.L)) GO TO 980
         POTERR = 25
         IF ((.NOT.PREVDF) .AND. (K(7).GT.L)) GO TO 980
         TAGS = TAG
         LS = LOCSYM + 2
         CALL SETTYP (K, LS, 2)
         NDIM = 0
         NP = 0
C                                       Parse Dimensions: open paren
         CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 980
         POTERR = 8
         IF(TAG.NE.2) GO TO 980
C                                       get number
 210     CALL GETFLD
            IF (ERRNUM.NE.0) GO TO 980
            ISGN = 1
C                                       - sign, get number
            IF (TAG.EQ.6) THEN
               ISGN = -1
               CALL GETFLD
               IF (ERRNUM.NE.0) GO TO 980
               END IF
            POTERR = 8
            IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.11)) GO TO 980
            NDIM = NDIM + 1
            LL = C(TAG) * ISGN
            CALL GETFLD
            IF (ERRNUM.NE.0) GO TO 980
C                                       a range of numbers
            IF ((TAG.EQ.15) .AND. (SYTYPE.EQ.4)) THEN
               CALL GETFLD
               IF (ERRNUM.NE.0) GO TO 980
               ISGN = 1
               IF (TAG.EQ.6) THEN
                  ISGN = -1
                  CALL GETFLD
                  IF (ERRNUM.NE.0) GO TO 980
                  END IF
               POTERR = 8
               IF((SYTYPE.NE.1) .AND. (SYTYPE.NE.11)) GO TO 980
               MM = C(TAG)*ISGN - LL + 1
               CALL GETFLD
               IF (ERRNUM.NE.0) GO TO 980
            ELSE
               MM = LL
               LL = 1
               END IF
C                                       put on stack for later
            POTERR = 6
            IF (MM.LE.0) GO TO 980
            CALL PUSH (TSTACK, NP, LL)
            IF (ERRNUM.NE.0) GO TO 980
            CALL PUSH (TSTACK, NP, MM)
            IF (ERRNUM.NE.0) GO TO 980
C                                       if not close paren, loop
            IF (TAG.NE.3) GO TO 210
C                                       Build array dope vector.
         N = 2*NDIM + 2
         LS = LS + 1
         IF (.NOT.PREVDF) LL = LLOCAT (N, K, LS)
         IF (ERRNUM.NE.0) GO TO 980
         IF (PREVDF) LL = K(LS)
         LLI = LL + 2
         N = N - 2
C                                         Allocate remaining space.
         ISIZE = 1
         DO 215 I = 2,N,2
            ISIZE = ISIZE * TSTACK(I)
 215        CONTINUE
         POTERR = 14
         IF ((PREVDF) .AND. (ISIZE.NE.K(LL))) GO TO 980
         IF ((PREVDF) .AND. (NDIM.NE.K(LL+1))) GO TO 980
         K(LL) = ISIZE
         K(LL+1) = NDIM
         KXLINK = 1
         CALL COPY (N, TSTACK, K(LLI))
C                                       make space, zero fill
         IF (.NOT.PREVDF) THEN
            N  =  ISIZE - 1
            LL = RLOCAT (N, K(KXORG), C, KXLINK)
            IF (ERRNUM.NE.0) GO TO 980
            CALL RFILL (ISIZE, 0.0, C(TAGS))
            END IF
         GO TO 200
C-----------------------------------------------------------------------
C                                     S T R I N G * n <name> <(dim)>.
C-----------------------------------------------------------------------
C                                       parse length
 250  POTERR = 36
      IF ((MODE.NE.1) .AND. (MODE.NE.3)) GO TO 980
      LMODE = MODE
      MODE = 69
      K3SAVE = K(3)
      CALL GETFLD
      MODE = LMODE
      IF (ERRNUM.NE.0) GO TO 980
C                                       length at beginning
      IF (TAG.EQ.7) THEN
         CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 980
         POTERR = 8
         IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.11)) GO TO 980
         NCH = C(TAG) + 0.1
         NSIZE = (NCH + 3) / 4
         POTERR = 13
         IF (NCH.GT.132) GO TO 980
      ELSE
         POTERR = 8
         GO TO 980
         END IF
C                                       Parse name of string.
 260  K3SAVE = K(3)
      LMODE = MODE
      MODE = 69
      CALL GETFLD
      MODE = LMODE
         IF (ERRNUM.NE.0) GO TO 980
         IF (TAG.EQ.1) GO TO 260
         IF ((TAG.EQ.0) .OR. (TAG.EQ.20)) GO TO 999
            POTERR = 8
            IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.7)) GO TO 980
            PREVDF = SYTYPE.EQ.7
            POTERR = 14
            IF ((.NOT.PREVDF) .AND. (K3SAVE.GT.L)) GO TO 980
            POTERR = 25
            IF ((.NOT.PREVDF) .AND. (K(7).GT.L)) GO TO 980
            TAGS = TAG
            LS = LOCSYM + 2
            CALL SETTYP (K, LS, 7)
            NDIM = 1
            NP = 0
            CALL PUSH (TSTACK, NP, 1)
            IF (ERRNUM.NE.0) GO TO 980
            CALL PUSH (TSTACK, NP, NSIZE)
            IF (ERRNUM.NE.0) GO TO 980
C                                       Parse dimensions.
            CALL GETFLD
            IF (ERRNUM.NE.0) GO TO 980
            IF ((TAG.GT.1) .AND. (TAG.NE.20)) THEN
               POTERR = 8
C                                       must be open paren
               IF (TAG.NE.2) GO TO 980
C                                       get number
 270           CALL GETFLD
               IF (ERRNUM.NE.0) GO TO 980
               ISGN = 1
               IF (TAG.EQ.6) THEN
                  ISGN = -1
                  CALL GETFLD
                  IF (ERRNUM.NE.0) GO TO 980
                  END IF
               POTERR = 8
               IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.11)) GO TO 980
               NDIM = NDIM + 1
               LL = C(TAG) * ISGN
C                                       get next symbol
               CALL GETFLD
               IF (ERRNUM.NE.0) GO TO 980
C                                       a range ("TO" or ":")
               IF ((TAG.EQ.15) .AND. (SYTYPE.EQ.4)) THEN
                  CALL GETFLD
                  IF (ERRNUM.NE.0) GO TO 980
                  ISGN = 1
                  IF (TAG.EQ.6) THEN
                     ISGN = -1
                     CALL GETFLD
                     IF (ERRNUM.NE.0) GO TO 980
                     END IF
                  POTERR = 8
                  IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.11)) GO TO 980
                  MM = C(TAG)*ISGN - LL + 1
                  CALL GETFLD
                  IF (ERRNUM.NE.0) GO TO 980
               ELSE
                  MM = LL
                  LL = 1
                  END IF
C                                       dimensions to stack
               POTERR = 6
               IF (MM.LE.0) GO TO 980
               CALL PUSH (TSTACK, NP, LL)
               IF (ERRNUM.NE.0) GO TO 980
               CALL PUSH (TSTACK, NP, MM)
               IF (ERRNUM.NE.0) GO TO 980
               IF (TAG.NE.3) GO TO 270
               END IF
C                                          Allocate remaining space.
         N = 2 * NDIM + 2
         LS = LS + 1
         IF (.NOT.PREVDF) LL = LLOCAT (N, K, LS)
         IF (ERRNUM.NE.0) GO TO 980
         IF (PREVDF) LL = K(LS)
         LLI = LL + 2
         N = N - 2
         ISIZE = 1
         DO 275 I = 2,N,2
            ISIZE = ISIZE * TSTACK(I)
 275        CONTINUE
         POTERR = 14
         IF ((PREVDF) .AND. (ISIZE.NE.K(LL))) GO TO 980
         IF ((PREVDF) .AND. (NDIM.NE.K(LL+1))) GO TO 980
         CALL COPY (N, TSTACK, K(LLI))
         K(LL) = ISIZE
         K(LL+1) = NDIM
         K(LL+3) = NCH
C                                       make space
         IF (.NOT.PREVDF) THEN
            KXLINK = 1
C                                       -1 since 1 already allocated
            N = ISIZE - 1
            LL = RLOCAT (N, K(KXORG), C, KXLINK)
            IF (ERRNUM.NE.0) GO TO 980
C                                       Blank fill.
            CALL RFILL (ISIZE, HBLANK, C(TAGS))
            END IF
         IF ((TAG.EQ.0) .OR. (TAG.EQ.20)) GO TO 999
         GO TO 260
C-----------------------------------------------------------------------
C                                       E L S E.
C-----------------------------------------------------------------------
 300  GO TO 310
C-----------------------------------------------------------------------
C                                       T H E N.
C-----------------------------------------------------------------------
 310  CONTINUE
      IFFLAG = 0
      NEXTP = 0
      CALL BCLEAN
      CALL PUSH (A, AP, 0)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (A, AP, 0)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (A, AP, TAG)
      IF (ERRNUM.NE.0) GO TO 980
      LAST = NOTHNG
      GO TO 999
C-----------------------------------------------------------------------
C                                       I F
C-----------------------------------------------------------------------
 320  IFFLAG = 1
      NEXTP = 0
      CALL BCLEAN
      LAST = NOTHNG
      GO TO 999
C-----------------------------------------------------------------------
C                                       W H I L E.
C-----------------------------------------------------------------------
 350  IFFLAG = 1
      IT = 97
      NEXTP = 0
      CALL BCLEAN
      CALL PUSH (A, AP, IT)
      IF (ERRNUM.NE.0) GO TO 980
      CALL PUSH (B, BP, TAG)
      IF (ERRNUM.NE.0) GO TO 980
      BPR(BP) = 0
      CALL PUSH (B, BP, 0)
      IF (ERRNUM.NE.0) GO TO 980
      BPR(BP) = 0
      CALL PUSH (B, BP, 0)
      IF (ERRNUM.NE.0) GO TO 980
      BPR(BP) = 0
      GO TO 999
C-----------------------------------------------------------------------
C                                       F I N I S H.
C-----------------------------------------------------------------------
 400  MODE = 2
      NT = 0
      IF (IDEBUG.LE.0) GO TO 405
         WRITE (MSGTXT,1400) NAMEP, K(NAMEP)
         CALL MSGWRT (4)
 405  POTERR = 11
      IF ((NAMEP.LT.200) .OR. (NAMEP.GT.KXORG)) GO TO 980
      L = K(NAMEP)
      POTERR = 11
      IF (L.EQ.0) GO TO 980
      NAMEP = 0
 410  LINK = K(L)
         L = L+1
 415     L = L+1
            IF (IDEBUG.GT.0) THEN
               WRITE (MSGTXT,1415) L, K(L), NT, TSTACK(NT)
               CALL MSGWRT (4)
               END IF
            IF (K(L).EQ.1) GO TO 450
C                                          THEN or WHILE.
            IF ((K(L).NE.NTHEN) .AND. (K(L).NE.NWHILE)) GO TO 435
 430           CALL PUSH (TSTACK, NT, L)
               IF (ERRNUM.NE.0) GO TO 980
               CALL PUSH (TSTACK, NT, K(L))
               IF (ERRNUM.NE.0) GO TO 980
               GO TO 415
C                                          ELSE or END.
 435        IF ((K(L).NE.NELSE) .AND. (K(L).NE.NEND) .AND.
     *         (K(L).NE.NENDR)) GO TO 440
               POTERR = 21
               IF (NT.EQ.0) GO TO 980
               CALL POP (TSTACK, NT, REASON)
               IF (ERRNUM.NE.0) GO TO 980
C                                     If END operator of an IF stmt
C                                     change to Opcode 96 (a no-op)
C                                     which won't hurt the EDITOR.
               IF (((REASON.EQ.NELSE) .OR. (REASON.EQ.NTHEN)) .AND.
     *            (K(L).EQ.NEND)) K(L) = NENDR
               CALL POP (TSTACK, NT, IT)
               IF (ERRNUM.NE.0) GO TO 980
               IF (IT.EQ.0) GO TO 415
                  K(IT-2) = -L-1
                  K(IT-1) = -LINK
                  IF (LINK.LE.0) K(IT-1) = -K(3)
                  IF (K(L).EQ.NELSE) GO TO 430
                  GO TO 415
C                                          FOR.
 440        IF (K(L).NE.NFOR) GO TO 415
               CALL PUSH (TSTACK, NT, 0)
               IF (ERRNUM.NE.0) GO TO 980
               CALL PUSH (TSTACK, NT, K(L))
               IF (ERRNUM.NE.0) GO TO 980
               GO TO 415
 450     L = LINK
         IF (L.NE.0) GO TO 410
      POTERR = 21
      IF (NT.NE.0) GO TO 980
      IPT = '>'
C                                       Put a RETURN in automatically
      IF (MODE.NE.0) THEN
         CALL PUSH (A, AP, RETNUM)
         CALL PUSH (A, AP, 1)
         END IF
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 999
C-----------------------------------------------------------------------
C                                       M S G K I L L  <true/false>.
C                                       D E B U G  <true/false>.
C                                       I S B A T C H  <true/false>.
C-----------------------------------------------------------------------
 500  CALL GETFLD
      IF (ERRNUM.NE.0) GO TO 980
      ISGN = 1
C                                       - sign, get number
      IF (TAG.EQ.6) THEN
         ISGN = -1
         CALL GETFLD
         IF (ERRNUM.NE.0) GO TO 980
         END IF
      POTERR = 8
      IF ((SYTYPE.NE.1) .AND. (SYTYPE.NE.11) .AND. (SYTYPE.NE.15))
     *    GO TO 980
C                                       DEBUG
      IF (BRANCH.EQ.12) THEN
         IDEBUG = 1
         IF (ISGN*C(TAG).LT.0.0) IDEBUG = -1
C                                       MSGKILL
      ELSE IF (BRANCH.EQ.13) THEN
         MSGKIL = 32000 + ISGN*C(TAG) - 0.99
         IF (ISGN*C(TAG).LT.0.0) MSGKIL = 0
C                                       ISBATCH
      ELSE IF (BRANCH.EQ.14) THEN
         ISBTCH = 32000
         IF (ISGN*C(TAG).LT.0.0) ISBTCH = 0
         IF (NPOPS.GT.NINTRN+1) ISBTCH = 32000
         END IF
      GO TO 999
C-----------------------------------------------------------------------
C                                   Error return.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      IF (ERRNUM.NE.0) THEN
         ERRLEV = ERRLEV + 1
         IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1400 FORMAT ('NAMEP, K(NAMEP)=',2I10)
 1415 FORMAT ('FINISH: L,K(L),NT,TSTACK(NT): ', 4I10)
      END
