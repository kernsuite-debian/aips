      SUBROUTINE ZTQSP2 (DOALL, MSLEV, NPROC)
C-----------------------------------------------------------------------
C! display AIPS account or all processes running on the system
C# Z2 System
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
C   Displays information on the user's terminal regarding AIPS account
C   originated processes or all processes running on the system.
C   Inputs:
C      DOALL   R   > 0.0 => display all processes
C      MSLEV   I   Message level to use in calling MSGWRT.
C   VMS version.  This one doesn't use the info in NPROC to filter
C   down the process list.  If we ever revive the VMS port, it should.
C-----------------------------------------------------------------------
      REAL      DOALL
      INTEGER   MSLEV, NPROC
C
      INTEGER   I, IL, LISTI(28), PIDADR, JERR, AIPGRP, HOURS, MINS,
     *   JTRIM, STATE, STATUS, DIRIO, PRIORI, CPU, GROUP, MEMB, BUFIO,
     *   SYS$GETJPIW
      INTEGER*2 LIST2(56)
      REAL      RCPU, SECS
      CHARACTER UNAME*15, WCH*1, CSTATE(14)*5
      LOGICAL   DOIT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE '($JPIDEF)'
      INCLUDE '($SSDEF)'
      EQUIVALENCE (LISTI, LIST2)
      DATA CSTATE /'CPW  ', 'MWAIT', 'CEF  ', 'PFW  ', 'LEF  ',
     *   'LEFO ', 'HIB  ', 'HIBO ', 'SUSP ', 'SUSPO', 'FPW  ',
     *   'COM  ', 'COMO ', 'CUR  '/
C-----------------------------------------------------------------------
C                                       table entry lengths
      CALL FILL (28, 0, LISTI)
      LIST2(1) = 4
      LIST2(7) = 4
      LIST2(13) = 4
      LIST2(19) = 4
      LIST2(25) = 4
      LIST2(31) = 4
      LIST2(37) = 4
      LIST2(43) = 15
      LIST2(49) = 4
      LIST2(55) = 4
C                                       Operation codes in table
      LIST2(2) = JPI$_STATE
      LIST2(8) = JPI$_STS
      LIST2(14) = JPI$_DIRIO
      LIST2(20) = JPI$_PRI
      LIST2(26) = JPI$_CPUTIM
      LIST2(32) = JPI$_GRP
      LIST2(38) = JPI$_MEM
      LIST2(44) = JPI$_PRCNAM
      LIST2(50) = JPI$_BUFIO
      LIST2(56) = 0
C                                       Addresses in table
      LISTI(2) = %LOC (STATE)
      LISTI(5) = %LOC (STATUS)
      LISTI(8) = %LOC (DIRIO)
      LISTI(11) = %LOC (PRIORI)
      LISTI(14) = %LOC (CPU)
      LISTI(17) = %LOC (GROUP)
      LISTI(20) = %LOC (MEMB)
      LISTI(23) = %LOC (UNAME)
      LISTI(26) = %LOC (BUFIO)
C                                       Header
      WRITE (MSGTXT,1000)
      CALL MSGWRT (MSLEV)
C                                       Find out what my group is.
      PIDADR = 0
      JERR = SYS$GETJPIW ( , PIDADR, , LISTI, , , )
      AIPGRP = GROUP
C                                       Loop for all processes
C                                       PIDADR -1 implies this
      PIDADR = -1
10    JERR = SYS$GETJPIW ( , PIDADR, , LISTI, , , )
C                                       there are more processes
      IF (JERR.NE.SS$_NOMOREPROC) THEN
C                                       error
         IF (JERR.NE.SS$_NORMAL) THEN
            IF (JERR.NE.SS$_NOPRIV) THEN
               WRITE (MSGTXT,1010) JERR
               CALL MSGWRT (MSLEV)
               END IF
C                                       list one?
         ELSE IF ((GROUP.EQ.AIPGRP) .OR. (DOALL.GT.0.0)) THEN
            IL = JTRIM (UNAME)
            DOIT = .TRUE.
C                                       Is it really an AIPS name?
            IF (DOALL.LE.0.0) THEN
               DOIT = IL.LE.6
               DO 20 I = 1,IL
                  WCH = UNAME(I:I)
                  IF ((WCH.LT.'0') .OR. (WCH.GT.'Z')) DOIT = .FALSE.
                  IF ((WCH.GT.'9') .AND. (WCH.LT.'A')) DOIT = .FALSE.
 20               CONTINUE
               WCH = UNAME(IL:IL)
C                                       I have a feeling this isn't
C                                       quite right...
               IF (NPROC.LT.10) THEN
                  CLAST = CHAR(ICHAR('0') + NPROC)
               ELSE
                  CLAST = CHAR(ICHAR('A') + NPROC - 10)
                  END IF
               CLAST =
               IF (WCH.GT.CLAST) DOIT = .FALSE.
               END IF
C                                       list it
            IF (DOIT) THEN
               RCPU = CPU / 360000.
               HOURS = RCPU
               RCPU = (RCPU - HOURS) * 60.
               MINS = RCPU
               SECS = (RCPU - MINS) * 60.
               DIRIO = DIRIO + BUFIO
               WRITE (MSGTXT,1020) UNAME(1:13), HOURS, MINS, SECS,
     *            CSTATE(STATE), PRIORI, DIRIO
               IF (MSGTXT(21:21).EQ.' ') MSGTXT(21:21) = '0'
               CALL MSGWRT (MSLEV)
               END IF
            END IF
         GO TO 10
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('  Name          CPU-time    State  Priority  I/O count')
 1010 FORMAT ('ZTQSP2: GETJPI RETURNS ERROR CODE',I10)
 1020 FORMAT (A13,1X,I2.2,':',I2.2,':',F5.2,4X,A5,I6,3X,I10)
      END
