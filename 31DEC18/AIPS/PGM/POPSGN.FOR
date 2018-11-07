      PROGRAM POPSGN
C-----------------------------------------------------------------------
C! compiles the initial symbol table for POPS from a text file
C# POPS-lang
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2003, 2005, 2013
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
C   POPSGN is the core initializer for the POPS driver.
C   It initializes the K array with permanent symbols, opcodes,
C   procedures, etc. that are needed to run POPS.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DCON.INC'
      CHARACTER PRGNAM*6, MNAME*8, VERSON*48, PHFILE*48, MSGBUF*80,
     *   KPAC*10, COUT*56
      INTEGER   KKT(KKTSIZ), HIPRO, I, I1, I2, IDT, IEOF, IERR, IFIND,
     *   IL, J, JFIND, JJ, KSIZE, KTEMP, KTLP, KTLPSV, LL, LLI, LSIZE,
     *   LUNCRT, LUNHE, LUNME, N, NPOPS1, NPOPS2, RLOCAT, LLOCAT, IDPI,
     *   JTRIM, TTY(2), LIST(4), IOUT
      REAL      VX(2)
      DOUBLE PRECISION DOUT
      LOGICAL   T, F, WASPRO
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DSMS.INC'
      INCLUDE 'INCS:DIO.INC'
      INCLUDE 'INCS:DBAT.INC'
      INCLUDE 'INCS:DBWT.INC'
      INCLUDE 'INCS:DRUN.INC'
      EQUIVALENCE (KKT(1), K(51))
      EQUIVALENCE (TTY(1), LUNCRT),  (TTY(2), IFIND)
      DATA LIST /-1, 8, 48, 0/
      DATA LUNCRT, LUNME, LUNHE /5, 14, 10/
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGNAM /'POPSGN'/
      DATA IEOF /2/
C-----------------------------------------------------------------------
C                                       init commons
      CALL AIPINI (TTY, PRGNAM, IERR)
      IF (IERR.NE.0) GO TO 995
      WASERR = .TRUE.
      IUNIT = 2
      ERRNUM = 0
      ERRLEV = 0
      PNAME(1) = ' '
      PNAME(2) = ' '
      PNAME(3) = ' '
      PNAME(4) = ' '
      PNAME(5) = ' '
      CALL FILL (5, 0, IERROR)
      CALL RFILL (5, HBLANK, KPAK)
      NUMRUN = 1
      CALL FILL (MAXRUN, 0, LUNRUN)
      LUNRUN(1) = LUNHE
      IPT = '>'
 10   MSGBUF = 'Enter Idebug, Mname, Version (1 I, 2 A''s) (NO COMMAS)'
      CALL INQGEN (TTY, MSGBUF, LIST, IOUT, DOUT, COUT, IERR)
      IF (IERR.LT.0) GO TO 10
      IF (IERR.GT.0) GO TO 995
      IDEBUG = IOUT
      MNAME = COUT(1:8)
      VERSON = COUT(9:56)
      CALL ZCLOSE (LUNCRT, IFIND, IERR)
      CALL CHLTOU (8, MNAME)
      CALL CHLTOU (48, VERSON)
      CALL ZMYVER
      IF (VERSON.EQ.' ') VERSON = VERNAM
C                                       Set sytem-wide version ID
      IF (VERSON(1:3).EQ.'NEW') VERNAM = 'NEW:'
      IF (VERSON(1:3).EQ.'TST') VERNAM = 'TST:'
      IF (VERSON(1:3).EQ.'CVX') VERNAM = 'CVX:'
      IF (VERSON(1:3).EQ.'OLD') VERNAM = 'OLD:'
      NPOPS1 = 1
      NPOPS2 = NINTRN
      IF (NBATQS.GT.0) NPOPS2 = NINTRN + 1 + NBATQS
C                                        Memory structure definitions.
      LPAGE = LBLOCK
      MPAGE = KBLOCK + LPAGE
      KSIZE = (MPAGE - LPAGE) * 256
      CALL RFILL (KSIZE, 0.0, C)
      LSIZE = LPAGE * 256
      CALL FILL (LSIZE, 0, LISTF)
      MODE = 69
      K(3) = 11
      K(5) = KPOINT
      K(8) = K(5) + 1
      K(9) = 1
      K(10) = 4
      K(KXORG+2) = K(8) + 10
      K(KXORG+4) = KSIZE
      K(KXORG+6) = K(KXORG+2) - 1
      HIPRO = K(KXORG+6)
      LISTF(3) = 11
      LISTF(5) = LSIZE
      KTEMP = 6
      KTEMP = LLOCAT (40, K, KTEMP)
      IF (ERRNUM.NE.0) GO TO 980
      KT = 7
      SLIM = 1000
      NEXTP = 0
      LPGM = 2
      KARLIM = NCHLIN
C                                       Note:  KT is the subscript of
C                                              K for immediate exec.
      KT = LLOCAT (KKTSIZ, K, KT)
      IF (ERRNUM.NE.0) GO TO 980
      KKT(3) = 11
      KKT(5) = KKTSIZ
C                                       One.
      LX = 2
      X(1) = 1.
      SYTYPE = 11
      CALL LTSTOR
      IF (ERRNUM.NE.0) GO TO 980
      ONE = TAG
C                                       Zero.
      X(1) = 0
      CALL LTSTOR
      IF (ERRNUM.NE.0) GO TO 980
      ZERO = TAG
C                                       True.
      NKAR = 4
      CALL CHR2H (4, 'TRUE', 1, KPAK)
      CALL SYMBOL (15)
      IF (ERRNUM.NE.0) GO TO 980
      C(TAG) = +1.0
      TRUE = TAG
C                                       False.
      NKAR = 5
      CALL CHR2H (5, 'FALSE', 1, KPAK)
      CALL SYMBOL (15)
      IF (ERRNUM.NE.0) GO TO 980
      C(TAG) = -1.0
      FALSE = TAG
      HIPRO = TAG
C                                       Compile symbols and opcodes.
C                                       open input file
      CALL ZPHFIL ('HE', 1, 0, 0, PHFILE, IERR)
      CALL ZTOPEN (LUNHE, IFIND, 1, PHFILE, MNAME, VERSON, F, IERR)
      IF (IERR.EQ.0) GO TO 45
         ERRNUM = 100
         GO TO 980
C                                       read to first line dashes
 45   CALL ZTREAD (LUNHE, IFIND, JBUFF, IERR)
         ERRNUM = 100
         IF (IERR.NE.0) GO TO 980
         IF (JBUFF(:8).NE.'--------') GO TO 45
         ERRNUM = 0
C                                       compile symbols
 50   CALL ZTREAD (LUNHE, IFIND, JBUFF, IERR)
      IF (IERR.EQ.IEOF) GO TO 160
      IF (IERR.EQ.0) GO TO 55
         ERRNUM = 100
         GO TO 980
 55   IF (JBUFF(1:2).EQ.'C-') GO TO 50
      IDT = JTRIM (JBUFF)
      READ (JBUFF,1055,ERR=100) KPAC, NKAR, IDT, I1, I2, VX
      WASPRO = (IDT.EQ.91) .OR. (IDT.EQ.92) .OR. (IDT.EQ.97)
      IF (WASPRO) IDT = IDT - 90
      IF ((IDT.LT.1) .OR. (IDT.GT.7)) GO TO 100
      CALL CHR2H (10, KPAC, 1, KPAK)
      GO TO (110, 120, 130, 140, 150, 160, 120), IDT
C                                       Error
 100  MSGTXT = JBUFF
      CALL MSGWRT (8)
      WRITE (MSGTXT,1100) KPAC, NKAR, IDT, I1, I2, VX
      CALL MSGWRT (8)
      GO TO 50
C                                       Real symbols.
 110  CALL SYMBOL (IDT)
      IF (ERRNUM.NE.0) GO TO 980
      C(TAG) = VX(1)
      IF (WASPRO) HIPRO = K(KXORG+3-1) - 1
      GO TO 50
C                                       Arrays.
 120  CALL SYMBOL (IDT)
      IF (ERRNUM.NE.0) GO TO 980
      L = LOCSYM + 3
      I = 2*I1 + 2
      LL = LLOCAT (I, K, L)
      IF (ERRNUM.NE.0) GO TO 980
      IF (IDT.EQ.7) VX(1) = MIN (VX(1), 132.0)
      N = 1
      LLI = LL + 2
      DO 125 I = 1,I1
         N = N * VX(I)
         K(LLI) = 1
         K(LLI+1) = VX(I) + .1
         LLI = LLI + 2
 125     CONTINUE
C                                       assign core as if 4 ch/fp
      IF ((IDT.EQ.7) .AND. (K(LL+3).LE.0)) K(LL+3) = 1
      IF (IDT.EQ.7) N = N * ((K(LL+3)+3)/4) / K(LL+3)
      K(LL) = N
      K(LL+1) = I1
      L = 1
      IL = N - 1
      LL = RLOCAT (IL, K(KXORG), C, L) - 1
      IF (ERRNUM.NE.0) GO TO 980
C                                       blank fill strings
      IF (WASPRO) HIPRO = K(KXORG+3-1) - 1
      IF (IDT.EQ.7) CALL RFILL (N, HBLANK, CH(LL))
      GO TO 50
 130  CONTINUE
      GO TO 50
C                                       Operators.
 140  CONTINUE
C                                       Pseudo operators.
 150  NEWCOD = I1
      CALL SYMBOL (IDT)
      IF (ERRNUM.NE.0) GO TO 980
      GO TO 50
C                                       regular compiles
 160  CONTINUE
C                                        init for regular execution
      MODE = 0
 190  KKT(1) = 0
      KKT(2) = 0
      KKT(3) = 11
      KKT(4) = 0
      KKT(5) = KKTSIZ
      KKT(7) = 11
      KKT(10) = 4
      KTLP = 2
C                                       Compile Procedures.
 200  CALL PREAD (KARBUF)
      IF (ERRNUM.NE.0) GO TO 980
      IF (IUNIT.NE.2) GO TO 390
      IF (IDEBUG.LE.0) GO TO 210
         MSGTXT = JBUFF
         CALL MSGWRT (1)
C                                          Comment Card.
 210  IF (KARBUF(1:1).EQ.'*') GO TO 200
      KBPTR = 1
      CALL POLISH
      IF (ERRNUM.NE.0) GO TO 980
      IF ((AP.EQ.0) .OR. (MODE.NE.0)) GO TO 220
C                                          Mode 0 -> Immediate exec.
         KTLPSV = KTLP
         I = AP + 2
         L = LLOCAT (I, KKT, KTLP)
         IF (ERRNUM.EQ.1) ERRNUM = 74
         IF (ERRNUM.NE.0) GO TO 980
         KKT(KTLPSV) = KKT(KTLPSV) + 50
         KKT(7) = KKT(3)
         KKT(6) = KKT(10)
         L = L + 2
         CALL COPY (AP, STACK(1), KKT(L))
         CALL INTERG (KKT(2))
         IF (ERRNUM.NE.0) GO TO 980
         GO TO 190
C                                          Mode 1 -> Compilation.
 220  IF (MODE.EQ.1) GO TO 200
         LPGM = 2
         MODE = 0
         GO TO 200
C
C                                       DONE !!!!!!!
 390  K(7) = K(3)
      K(KXORG+7-1) = HIPRO
C                                       store main copy
      CALL ZPHFIL ('ME', 1, 0, 0, PHFILE, IERR)
      CALL ZOPEN (LUNME, JFIND, 1, PHFILE, F, T, T, IERR)
      IF (IERR.NE.0) THEN
         ERRNUM = 100
         GO TO 980
         END IF
C                                       save K array
      IDPI = 1
      JJ = MPAGE - LPAGE
      J = 1
      IERROR(2) = 30
      DO 405 I = 1,JJ
         CALL ZFIO ('WRIT', LUNME, JFIND, IDPI, K(J), IERROR)
         IF (IERROR(1).NE.0) GO TO 980
         IDPI = IDPI + 1
         J = J + 256
 405     CONTINUE
C                                       save LISTF array
      J = 1
      IERROR(2) = 40
      DO 410 I = 1,LPAGE
         CALL ZFIO ('WRIT', LUNME, JFIND, IDPI, LISTF(J), IERROR)
         IF (IERROR(1).NE.0) GO TO 980
         IDPI = IDPI + 1
         J = J + 256
 410     CONTINUE
      CALL ZCLOSE (LUNME, JFIND, IERROR)
C                                       close down
      MSGTXT = 'Popsgen complete'
      CALL MSGWRT (4)
      GO TO 995
C                                       Error
 980  ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
      CALL ERROR
C
 995  CALL MSGWRT (-1)
      CALL ACOUNT (2)
C
 999  STOP
C-----------------------------------------------------------------------
 1055 FORMAT (A10,1X,I3,1X,I3,1X,I4,1X,I4,2(1X,F7.2))
 1100 FORMAT ('Read as ',A10,4I4,2F7.3)
      END
      SUBROUTINE ERROR
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DCON.INC'
C-----------------------------------------------------------------------
      CALL OERROR
      CALL ZKDUMP (1, 10, K, C)
C
 999  RETURN
      END
      SUBROUTINE INTERG (KENTRY)
C-----------------------------------------------------------------------
C   INTERG causes the POPS code to be executed: placing operands on
C   the V and STACK stacks and calling VERBS and QUICK for verbs.
C   Inputs:
C      KENTRY  I    pointer to first particle of executable code
C   Version for POPSGN:::: does not call VERBS
C-----------------------------------------------------------------------
      INTEGER   KENTRY
C
      CHARACTER PRGNAM*6
      INTEGER   J, J1, KK, LFLAG, POTERR
      INCLUDE 'INCS:DCON.INC'
      INCLUDE 'INCS:DERR.INC'
      INCLUDE 'INCS:DPOP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DIO.INC'
      DATA PRGNAM /'INTERG'/
C-----------------------------------------------------------------------
      IPT = '#'
      SP = 0
      CP = 0
      SP0 = 1
      LINK = K(KENTRY)
      L = KENTRY
C                                       Program chunk begins.
 10   L = L + 1
C                                          Advance program counter.
 20      L = L + 1
 25         J = K(L)
C                                       Debug.
               IF (IDEBUG.GT.0) THEN
                  KK = CSTACK(CP)
                  WRITE (MSGTXT,1025) L, J, SP, SP0, STACK(SP),
     *               V(SP), CP, CSTACK(CP), C(KK)
                  CALL MSGWRT (4)
                  END IF
C                                       Operand - push on stack.
               IF (J.LE.0) THEN
                  SP = SP + 1
                  POTERR = 7
                  IF ((SP.LT.1) .OR. (SP.GT.SLIM)) GO TO 980
                  IF ((CP.LT.0) .OR. (CP.GT.SLIM)) GO TO 980
                  J1 = -J
                  STACK(SP) = J1
                  V(SP) = C(J1)
C                                       Verb linkages.
               ELSE IF ((J.GE.200) .AND. (J.LE.999)) THEN
C                 CALL VERBS (J)
                  IF (ERRNUM.NE.0) GO TO 980
C                                       POPS operator linkages.
               ELSE
                  CALL KWICK (LFLAG, J)
                  IF (ERRNUM.NE.0) GO TO 980
                  IF ((LFLAG.GE.1) .AND. (LFLAG.LE.6)) GO TO (10, 20,
     *               25, 50, 60, 995), LFLAG
                     POTERR = 15
                     GO TO 980
C                                        store intermediate value
 50                  SP = SP - 1
                     STACK(SP) = 0
 60                  V(SP) = XX
                  END IF
               GO TO 20
C                                       Error exit.
 980  IF (ERRNUM.EQ.0) ERRNUM = POTERR
      ERRLEV = ERRLEV + 1
      IF (ERRLEV.LE.5) PNAME(ERRLEV) = PRGNAM
C                                       Normal exit.
 995  IPT = '>'
C
 999  RETURN
C-----------------------------------------------------------------------
 1025 FORMAT ('INTERG:',2I6,2I3,I5,G12.5,2I5,G12.5)
      END
