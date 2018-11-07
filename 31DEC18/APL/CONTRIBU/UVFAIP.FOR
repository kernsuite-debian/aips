      SUBROUTINE UVFAIP (XCNAME, DATFIL, GAINFL, FIRST, LAST, IFNUMB,
     *   OUTFIL, IERR)
C-----------------------------------------------------------------------
C! fortran for UVF task as an AIPS task
C# SingleDish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   Start up routine for UVF as an AIPS task
C   Output:
C      IERR             Error code: 0 AIPS started us,
C                          > 0 AIPS started us with bad data
C                          < 0 AIPS did not start us
C-----------------------------------------------------------------------
      INTEGER   FIRST, LAST, IERR
      REAL      IFNUMB
      HOLLERITH DATFIL(64), GAINFL(64), OUTFIL(64), XCNAME(2)
C
      CHARACTER PRGNAM*6, UFIL*48, SCRTCH*256, CZERO*4, CNAME*6
      HOLLERITH XDTFIL(12), XGNFIL(12), XOUFIL(12), XZERO
      REAL      XFIRST, XLAST, XIFNUM
      INTEGER   BUFFER(256), JTRIM, IROUND, IRET, I, NPARM
      LOGICAL   RQUICK
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /INPARM/ XDTFIL, XGNFIL, XFIRST, XLAST, XIFNUM, XOUFIL
      COMMON /UVFCOM/ RQUICK
      DATA PRGNAM /'UVF  '/
C-----------------------------------------------------------------------
C                                       AIPS init
      NPOPS = 1
      MSGCNT = -1
      NLUSER = 1
      TSKNAM = PRGNAM
      CALL ZDCHIN (.TRUE.)
      NPARM = 39
      IRET = 0
      XZERO = 0
      CALL H2CHR (4, 1, XZERO, CZERO)
      CALL H2CHR (6, 1, XCNAME, CNAME)
      I = JTRIM (CNAME)
C                                       Are we a task?
C      MSGSUP = 32000
      CALL UVFGTP (PRGNAM, CNAME, NPARM, RQUICK, XDTFIL, BUFFER, IERR)
      MSGSUP = 0
      IF (IERR.EQ.0) THEN
         FIRST = IROUND (XFIRST)
         FIRST = MAX (1, FIRST)
         LAST = IROUND (XLAST)
         IF (LAST.LE.0) LAST = 1000
         IFNUMB = XIFNUM / 100.0
         IF (IFNUMB.EQ.0.0) IFNUMB = 0.01
         CALL H2CHR (48, 1, XDTFIL, UFIL)
         CALL ZFULLN (UFIL, ' ', ' ', SCRTCH, IRET)
         IF (IRET.EQ.0) THEN
            I = JTRIM (SCRTCH)
            SCRTCH(I+1:I+4) = CZERO
            CALL CHR2H (256, SCRTCH, 1, DATFIL)
            CALL H2CHR (48, 1, XGNFIL, UFIL)
            CALL ZFULLN (UFIL, ' ', ' ', SCRTCH, IRET)
            END IF
         IF (IRET.EQ.0) THEN
            I = JTRIM (SCRTCH)
            SCRTCH(I+1:I+4) = CZERO
            CALL CHR2H (256, SCRTCH, 1, GAINFL)
            CALL H2CHR (48, 1, XOUFIL, UFIL)
            CALL ZFULLN (UFIL, ' ', ' ', SCRTCH, IRET)
            END IF
         IF (IRET.EQ.0) THEN
            I = JTRIM (SCRTCH)
            SCRTCH(I+1:I+4) = CZERO
            CALL CHR2H (256, SCRTCH, 1, OUTFIL)
            END IF
         IERR = IRET
         IF (RQUICK) CALL RELPOP (IRET, BUFFER, I)
      ELSE
         IERR = - ABS (IERR)
         END IF
C
 999  RETURN
      END
      SUBROUTINE UVFGTP (NAME, CNAME, NPARMS, RQUICK, RPARM, SCRTCH,
     *   IERR)
C-----------------------------------------------------------------------
C! starts tasks, getting parameters and task ID number, does accounting
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   GTPARM obtains the activator task number, obtains the transmitted
C   parameters, initializes the message common, and outputs the message
C   'task NAME begins'.  It also handles startup accounting.
C   Inputs:
C      NAME    C*6        Task name
C      CNAME   C*6        Real task name passed through
C      NPARMS  I          Number of real variables wanted
C   Outputs:
C      RQUICK  L          T => release POPs as soon as possible
C                         F => wait until you have finished
C      RPARM   R(NPARMS)  Parameters received
C      SCRTCH  I(256)     Scratch buffer
C      IERR    I          Error code: 0 -> ok
C                                     1 -> initiator not found
C                                     2 -> disk troubles
C                                     3 -> initiator zeroed
C-----------------------------------------------------------------------
      CHARACTER NAME*6, CNAME*6
      INTEGER   NPARMS, SCRTCH(256), IERR
      REAL      RPARM(*)
      LOGICAL   RQUICK
C
      INTEGER   FINDTD, ISCR, I, IER, J, LUNTD, NBL, JBL
      REAL      RSCR
      LOGICAL   T, F
      CHARACTER PHNAME*48, PTYPE*2, TNAME*6
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (RSCR, ISCR)
      DATA LUNTD, PTYPE /13, 'TD'/
      DATA T, F /.TRUE.,.FALSE./
      DATA NBL /3/
C-----------------------------------------------------------------------
C                                       set initial values
      IERR = 0
      MSGCNT = -1
      TSKNAM = NAME
      NPOPS = 1
      NLUSER = 1
      MSGSUP = 0
C                                       open task data file
      CALL ZPHFIL (PTYPE, 1, 0, 0, PHNAME, IER)
      CALL ZOPEN (LUNTD, FINDTD, 1, PHNAME, F, T, T, IER)
      IF (IER.EQ.0) GO TO 10
         IERR = 2
         GO TO 999
C                                       read directory record
 10   CALL ZFIO ('READ', LUNTD, FINDTD, 1, SCRTCH, IER)
      IF (IER.NE.0) GO TO 980
C                                       locate initiator
      CALL UVFWHO (NAME, CNAME, NPOPS, IER)
      J = 5 * (NPOPS-1)
      IF (IER.NE.0) GO TO 20
         CALL H2CHR (6, 1, SCRTCH(J+1), TNAME)
         IF (NAME.EQ.TNAME) GO TO 30
 20   IERR = 1
      IF ((SCRTCH(J+1).EQ.0) .AND. (SCRTCH(J+2).EQ.0)) IERR = 3
      GO TO 990
C                                       Clear task name from file
 30   SCRTCH(J+1) = 0
      SCRTCH(J+2) = 0
      CALL ZFIO ('WRIT', LUNTD, FINDTD, 1, SCRTCH, IER)
C                                       set values, read pars
      JBL = NBL*(NPOPS-1) + 2
      CALL ZFIO ('READ', LUNTD, FINDTD, JBL, SCRTCH, IER)
      IF (IER.NE.0) GO TO 980
C                                       User number
      NLUSER = SCRTCH(1)
C                                       Get TV and TK assignments
      NTVDEV = SCRTCH(2)
      NTKDEV = SCRTCH(3)
C                                       Get Message Kill parameter
      MSGKIL = SCRTCH(4)
C                                       Is this batch job
      ISBTCH = SCRTCH(5)
C                                       Is debug needed?
      DBGAIP = SCRTCH(6)
C                                       quick return
      J = 9
      ISCR = SCRTCH(J)
      RQUICK = RSCR.LE.0.0
      IF (NPOPS.GT.NINTRN+1) ISBTCH = 32000
      RQUICK = RQUICK .AND. (NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)
C                                       Get version ID
      J = J + 1
      CALL H2CHR (4, 1, SCRTCH(J), VERNAM)
C                                       any machine-dependent parms
      CALL ZSETUP
C                                       Get task parms
      IF (NPARMS.LE.0) GO TO 60
         DO 50 I = 1,NPARMS
            J = J + 1
            IF (J.LE.256) GO TO 45
               J = 1
               JBL = JBL + 1
               CALL ZFIO ('READ', LUNTD, FINDTD, JBL, SCRTCH, IER)
               IF (IER.NE.0) GO TO 980
 45         ISCR = SCRTCH(J)
            RPARM(I) = RSCR
 50         CONTINUE
C                                       close file
 60   CALL ZCLOSE (LUNTD, FINDTD, IER)
C                                       Start accounting
      CALL ACOUNT (1)
C                                       message: start
      WRITE (MSGTXT,1060) TSKNAM, RLSNAM
      CALL MSGWRT (5)
      GO TO 999
C                                       Errors
 980  IERR = 2
 990  CALL ZCLOSE (LUNTD, FINDTD, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1060 FORMAT ('Task ',A6,' (release of ',A7,') begins')
      END
      SUBROUTINE UVFWHO (INAME, CNAME, INUM, IERR)
C-----------------------------------------------------------------------
C! given root task name, gets actual task name and finds NPOPS number
C# System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   WHOAMI obtains the actual task name, compares it to the root task
C   name, and determines the POPS number.
C   Inputs:
C      INAME  C*6   Root task name
C      CNAME  C*6   actual name   SPECIAL VERSION IS HANDED THIS
C   Output:
C      INUM   I     POPS number
C      IERR   I     Error code: 0 => ok
C                      1 => illegal root
C                      2 => roots don't match
C                      3 => INUM out of range, illegal
C                      4 => other
C-----------------------------------------------------------------------
      CHARACTER INAME*6
      INTEGER   INUM, IERR
C
      INTEGER   IP
C      INTEGER   IT
      CHARACTER CNAME*6, CTEST*1
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       Legal root?
      DO 10 IP = 1,6
         IF (INAME(IP:IP).EQ.' ') GO TO 20
 10      CONTINUE
      IERR = 1
      GO TO 999
C                                       Get real name
 20   IERR = 4
C      CALL ZGNAME (CNAME, IT)
C      IF (IT.NE.0) GO TO 999
C                                       Roots match?
      IP = IP - 1
      IERR = 2
      IF (IP.LT.1) GO TO 999
      IF (CNAME(1:IP).NE.INAME(1:IP)) GO TO 999
C                                       Get number
      IP = IP + 1
      CTEST = CNAME(IP:IP)
      IP = NINTRN
      IF (NBATQS.GT.0) IP = NINTRN + 1 + NBATQS
      CALL ZHEX10 (CTEST, INUM, IERR)
      IF ((INUM.LT.1) .OR. (INUM.GT.IP) .OR. (IERR.NE.0)) IERR = 3
C
 999  RETURN
      END
      SUBROUTINE UVFEND (IRET)
C-----------------------------------------------------------------------
C   Does the AIPS close down messages
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   BUFFER(256)
      LOGICAL   RQUICK
      COMMON /UVFCOM/ RQUICK
C-----------------------------------------------------------------------
      CALL DIETSK (IRET, RQUICK, BUFFER)
C
 999  RETURN
      END
