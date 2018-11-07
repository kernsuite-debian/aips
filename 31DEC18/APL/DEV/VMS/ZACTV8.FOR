      SUBROUTINE ZACTV8 (NAME, INPOPS, RVERSN, PID, IERR)
C-----------------------------------------------------------------------
C! activate the requested program, returning process ID information
C# Z System
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
C   Activate the specified program and return the activated process
C   identification information.
C   Inputs:
C      NAME     C*6    Program name
C      INPOPS   I      POPS # for the task to use
C      RVERSN   C*48   Logical name or fully qualified name of the
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
C   VAX VERSION: PID null for now
C-----------------------------------------------------------------------
      CHARACTER NAME*(*), RVERSN*(*)
      INTEGER   INPOPS, PID(4), IERR
C
      INTEGER   I, J, J2, J3, ELEN, VLEN, XLEN, XLNB, ALTX, FLEN, PLEN,
     *   SYSERR, ITRIM, IDBG
      LOGICAL   EXIST, F
      CHARACTER EXENAM*9, FULNAM*128, XLATED*128, CVERSN*48, PNAME*6
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Build process name = task name
C                                       with POPS # suffix.
      CALL BLDTNM (NAME, INPOPS, PNAME, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) NAME
         GO TO 995
         END IF
C                                       Determine full name of .EXE
      ELEN = ITRIM (NAME)
      EXENAM = NAME(1:ELEN) // '.EXE'
C                                       If blank, use VERNAM.
      CVERSN = RVERSN
      IF (CVERSN.EQ.' ') CVERSN = VERNAM
      VLEN = ITRIM (CVERSN)
C                                       Version name: VMSism
      IF (CVERSN(VLEN:VLEN).EQ.':') THEN
         CVERSN(VLEN:VLEN) = ' '
         VLEN = VLEN - 1
         END IF
      XLEN = LEN (XLATED)
C                                       Full file name
C                                       Determine fully qualified name
C                                       of executable module.
C                                       Standard logicals
      IF ((CVERSN(1:4).EQ.'TST ') .OR. (CVERSN(1:4).EQ.'NEW ') .OR.
     *   (CVERSN(1:4).EQ.'OLD ') .OR. (CVERSN(1:6).EQ.'LOCAL ')) THEN
         J2 = 2 * NTVDEV - 1
         J3 = 2
         IF (J2.LE.0) J2 = 1
         CALL ZTRLOG (VLEN, CVERSN, XLEN, XLATED, XLNB, IERR)
         IF (IERR.NE.0) GO TO 990
         FULNAM = XLATED(1:XLNB) // '[LOAD.ALTx]' // EXENAM
         ALTX = XLNB + 10
C                                       pseudo AP tasks
      ELSE IF ((CVERSN(1:8).EQ.'TSTPSAP ') .OR. (CVERSN(1:8).EQ.
     *   'NEWPSAP ') .OR. (CVERSN(1:8).EQ.'OLDPSAP ')) THEN
         J2 = 2 * NTVDEV
         J3 = 1
         IF (J2.LE.0) J2 = 2
         CVERSN(4:) = ' '
         VLEN = 3
         CALL ZTRLOG (VLEN, CVERSN, XLEN, XLATED, XLNB, IERR)
         IF (IERR.NE.0) GO TO 990
         FULNAM = XLATED(1:XLNB) // '[LOAD.ALTx]' // EXENAM
         ALTX = XLNB + 10
C                                       Some other logical.
      ELSE IF (INDEX (CVERSN, ']').EQ.0) THEN
         J2 = 1
         J3 = 4
         ALTX = 0
         CALL ZTRLOG (VLEN, CVERSN, XLEN, XLATED, XLNB, IERR)
         IF (IERR.NE.0) GO TO 990
         FULNAM = XLATED(1:XLNB) // EXENAM
C                                       Literal path name.
      ELSE
         J2 = 1
         J3 = 4
         ALTX = 0
         FULNAM = CVERSN(1:VLEN) // EXENAM
         END IF
C                                       Cycle through standard areas
C                                       looking for executable module.
      DO 40 I = 1,J2,J3
         J = J2 - I
C                                       Fill in 'x' in ALTx or eliminate
C                                       ALTx based on J
         IF (ALTX.GT.0) THEN
            IF (J.GT.0) CALL ZEHEX (J, 1, FULNAM(ALTX:ALTX))
            IF (J.EQ.0) FULNAM = XLATED(1:XLNB) // '[LOAD]' // EXENAM
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
 50   FLEN = ITRIM (FULNAM)
      PLEN = ITRIM (PNAME)
      IF (DBGAIP.LE.10) THEN
         IDBG = 0
      ELSE
         IDBG = 1
         END IF
      CALL ZACTV9 (FLEN, FULNAM, PLEN, PNAME, IDBG, PID, SYSERR, IERR)
      IF (IERR.EQ.0) THEN
         IF ((J/2)*2.NE.J) THEN
            WRITE (MSGTXT,1050)
            CALL MSGWRT (2)
            END IF
      ELSE
         CALL ZERROR ('ZACTV9', SYSERR, ' ', -999, F)
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
 1040 FORMAT ('ZACTV8: TASK ',A6,' DOES NOT EXIST IN')
 1041 FORMAT ('ZACTV8: VERSION = ',A)
 1050 FORMAT ('Starting up pseudo-AP version')
 1990 FORMAT ('ZACTV8: COULD NOT TRANSLATE LOGICAL =')
 1991 FORMAT ('ZACTV8: ',A)
      END
