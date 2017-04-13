      SUBROUTINE ZACTV8 (NAME, INPOPS, RVERSN, PID, IERR)
C-----------------------------------------------------------------------
C! activate the requested program, returning process ID information
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2004
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
C   Activate the specified program and return the activated process
C   identification information.
C   Inputs:
C      NAME     C*6    Program name
C      INPOPS   I      POPS # for the task to use
C      RVERSN   C*(*)  Logical name or fully qualified name of the
C                      directory from which to get the required
C                      executable module
C   In/Out:
C      PID      I(4)   Process identification information used directly
C                      by subsequent calls to ZTACTQ where
C                      In:  PID(1)   = user number for systems that use
C                                      it (= 0 otherwise and on all
C                                      AIPSB invocations)
C                      Out: PID(2-4) = process ID number(s) as needed
C   Output:
C      IERR     I      Error return code: 0 => no error
C                         1 => program name too long
C                         2 => activation error
C   Common: DMSG.INC
C      DBGAIP   I      > 10 => start tasks in DEBUG mode if possible
C   Generic UNIX version (since VERSION is converted to uppercase by
C   AIPS, RVERSN can only be a literal directory name if it is all
C   uppercase).  Relatively generic over all, but uses "/" everywhere to
C   build file and path names.
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), RVERSN*(*)
      INTEGER   INPOPS, PID(4), IERR
C
      INTEGER   ALTX, ELEN, FLEN, I, IALT(3), IDBG, ITRIM, J, LARCHX,
     *   NALT, PLEN, SYSERR, TVALT, VLEN, XLEN, XLNB, JT, JTRIM
      LOGICAL   EXIST
      CHARACTER CARCHX*40, CVERSN*256, EXENAM*9, FULNAM*256, PNAME*6,
     *   XLATED*256
      HOLLERITH HPNAME(2), HFULNM(64)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Build process name = task name
C                                       with POPS # suffix.
      CALL BLDTNM (NAME, INPOPS, PNAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) NAME
         GO TO 995
         END IF
C                                       Get logical name ARCH for
C                                       architecture (SUN4, DEC, etc)
      CALL ZTRLOG (4, 'ARCH', 40, CARCHX, LARCHX, IERR)
      IF (IERR.NE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1990)
         CALL MSGWRT (8)
         WRITE (MSGTXT,1991) 'ARCH'
         CALL MSGWRT (8)
         GO TO 999
         END IF
C                                       Translate TVALT.
      XLEN = LEN (XLATED)
      CALL ZTRLOG (5, 'TVALT', XLEN, XLATED, XLNB, IERR)
      IF (IERR.NE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1990)
         CALL MSGWRT (6)
         WRITE (MSGTXT,1991) 'TVALT'
         CALL MSGWRT (6)
         WRITE (MSGTXT,1010)
         CALL MSGWRT (6)
         TVALT = 0
      ELSE IF ((XLATED(1:5).NE.'TVALT') .OR. (XLNB.NE.7)) THEN
         JT = JTRIM (XLATED(:XLNB))
         WRITE (MSGTXT,1020) XLATED(:JT)
         CALL MSGWRT (7)
         WRITE (MSGTXT,1010)
         CALL MSGWRT (7)
         TVALT = 0
      ELSE
         READ (XLATED(6:6), 1030, IOSTAT=SYSERR) TVALT
         IF (SYSERR.NE.0 .OR. MOD(TVALT,2).EQ.1) THEN
            JT = JTRIM (XLATED(:XLNB))
            WRITE (MSGTXT,1020) XLATED(:JT)
            CALL MSGWRT (7)
            WRITE (MSGTXT,1010)
            CALL MSGWRT (7)
            TVALT = 0
            END IF
         END IF
C                                       Determine full name of .EXE
      ELEN = ITRIM (NAME)
      EXENAM = NAME(1:ELEN) // '.EXE'
C                                       If blank, use VERNAM.
      CVERSN = RVERSN
C     IF (CVERSN.EQ.' ' .OR. CVERSN.EQ.'LOCAL') CVERSN = VERNAM
      IF (CVERSN.EQ.' ') CVERSN = VERNAM
      VLEN = ITRIM (CVERSN)
C                                       Version name: VMSism
      IF (CVERSN(VLEN:VLEN).EQ.':') THEN
         CVERSN(VLEN:VLEN) = ' '
         VLEN = VLEN - 1
         END IF
C                                       Determine fully qualified name
C                                       of executable module.
C                                       Standard logicals
      IF ((CVERSN(1:4).EQ.'TST ') .OR. (CVERSN(1:4).EQ.'NEW ') .OR.
     *   (CVERSN(1:4).EQ.'OLD ') .OR. (CVERSN(1:4).EQ.'CVX ') .OR.
     *   (CVERSN(1:6).EQ.'LOCAL ')) THEN
         IF (TVALT.EQ.0) THEN
            NALT = 1
            IALT(1) = 0
         ELSE
            NALT = 2
            IALT(1) = TVALT
            IALT(2) = 0
            END IF
         CALL ZTRLOG (VLEN, CVERSN, XLEN, XLATED, XLNB, IERR)
         IF (IERR.NE.0) GO TO 990
         FULNAM = XLATED(1:XLNB) // '/' // CARCHX(1:LARCHX) //
     *      '/LOAD/ALTx/' // EXENAM
         ALTX = INDEX (FULNAM, 'ALTx') + 3
C                                       pseudo AP tasks
      ELSE IF ((CVERSN(1:8).EQ.'TSTPSAP ') .OR. (CVERSN(1:8).EQ.
     *   'NEWPSAP ') .OR. (CVERSN(1:8).EQ.'OLDPSAP ') .OR.
     *   (CVERSN(1:8).EQ.'CVXPSAP ')) THEN
         IF (TVALT.EQ.0) THEN
            NALT = 2
            IALT(1) = 1
            IALT(1) = 0
         ELSE
            NALT = 3
            IALT(1) = TVALT + 1
            IALT(2) = TVALT
            IALT(3) = 0
            END IF
         CVERSN(4:) = ' '
         VLEN = 3
         CALL ZTRLOG (VLEN, CVERSN, XLEN, XLATED, XLNB, IERR)
         IF (IERR.NE.0) GO TO 990
         FULNAM = XLATED(1:XLNB) // '/' // CARCHX(1:LARCHX) //
     *      '/LOAD/ALTx/' // EXENAM
         ALTX = INDEX (FULNAM, 'ALTx') + 3
C                                       Some other logical.
      ELSE IF (INDEX (CVERSN, '/').EQ.0) THEN
         NALT = 1
         IALT(1) = 0
         ALTX = 0
         CALL ZTRLOG (VLEN, CVERSN, XLEN, XLATED, XLNB, IERR)
         IF (IERR.NE.0) GO TO 990
         FULNAM = XLATED(1:XLNB) // '/' // EXENAM
C                                       Literal path name.
      ELSE
         NALT = 1
         IALT(1) = 0
         ALTX = 0
         I = INDEX (CVERSN, ' ') - 1
         IF (I.LE.0) I = LEN (CVERSN)
         FULNAM = CVERSN(1:I) // '/' // EXENAM
         END IF
C                                       Cycle through standard areas
C                                       looking for executable module.
      DO 40 I = 1, NALT
         J = IALT(I)
C                                       Fill in 'x' in ALTx or eliminate
C                                       ALTx based on J
         IF (ALTX.GT.0) THEN
            IF (J.GT.0) CALL ZEHEX (J, 1, FULNAM(ALTX:ALTX))
            IF (J.EQ.0) FULNAM = XLATED(1:XLNB) // '/' //
     *         CARCHX(1:LARCHX) // '/LOAD/' // EXENAM
            END IF
         INQUIRE (FILE=FULNAM, EXIST=EXIST)
         IF (EXIST) GO TO 50
 40      CONTINUE
C                                       Could not find executable
      IERR = 1
      WRITE (MSGTXT,1040) NAME
      CALL MSGWRT (7)
      WRITE (MSGTXT,1041) CVERSN(1:VLEN)
      GO TO 995
C                                       Activate the program.
 50   FLEN = ITRIM (FULNAM)
      PLEN = ITRIM (PNAME)
      IF (DBGAIP.LE.10) THEN
         IDBG = 0
      ELSE
         IDBG = 1
         END IF
C                                       call C here
      CALL CHR2H (FLEN, FULNAM, 1, HFULNM)
      CALL CHR2H (PLEN, PNAME, 1, HPNAME)
      CALL ZACTV9 (FLEN, HFULNM, PLEN, HPNAME, IDBG, PID, SYSERR, IERR)
      IF (IERR.EQ.0) THEN
         IF ((J/2)*2.NE.J) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (2)
            END IF
C                                       ZACTV9 now does zerror messages
      ELSE
CCC         CALL ZERROR ('ZACTV9', SYSERR, ' ', -999, F)
         IERR = 2
         END IF
      GO TO 999
C                                       version not found
 990  IERR = 2
      WRITE (MSGTXT,1990)
      CALL MSGWRT (7)
      WRITE (MSGTXT,1991) CVERSN
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZACTV8: TASK NAME',A6,' TOO LONG')
 1010 FORMAT ('ZACTV8: Assuming ALT0.')
 1020 FORMAT ('ZACTV8: Invalid TVALT: ',A)
 1030 FORMAT (I1)
 1040 FORMAT ('ZACTV8: TASK ',A6,' DOES NOT EXIST IN')
 1041 FORMAT ('ZACTV8: VERSION = ',A)
 1050 FORMAT ('Starting up pseudo-AP version')
 1990 FORMAT ('ZACTV8: COULD NOT TRANSLATE LOGICAL =')
 1991 FORMAT ('ZACTV8: ',A)
      END
