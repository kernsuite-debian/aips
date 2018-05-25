      PROGRAM ZSTRTD
C-----------------------------------------------------------------------
C! determine parameters under which DAIP is to run
C# Z System
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2002
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
C   ZSTRTD determines what 'DAIPx' we will be (if any)
C-----------------------------------------------------------------------
      CHARACTER VERSIN*256, PHFILE*48, AIPSN*6, MSGBUF*80, PRGNAM*6,
     *   XLATED*256, MYTTY*256, RESTTX*7, RESTTY*256, POPSNO*1
      INTEGER   IBUF(256), IERR, IPOPS, PID(4), IFIND, LUNCRT, MYPOPS,
     *   MYPRIO, XLEN, XLNB
      LOGICAL   ACTIV1, ACTIV2, ACTIVE, RESRVD(15), T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNCRT, T, F /5, .TRUE., .FALSE./
      DATA PRGNAM /'ZSTRTD'/
      DATA RESTTX /'RESSTTX'/
C-----------------------------------------------------------------------
C                                       Set values especially NINTRN.
      CALL ZDCHIN (.TRUE., IBUF)
C                                       Copy PRGNAM into TSKNAM.
      TSKNAM = PRGNAM
C                                       Get default version.
      CALL ZMYVER
      VERSIN = VERNAM(1:3)
C                                       Check for LOCAL version
      XLEN = LEN (XLATED)
      CALL ZTRLOG (9, 'AIPSLOCAL', XLEN, XLATED, XLNB, IERR)
      IF (IERR.EQ.0) THEN
         VERSIN = XLATED(1:XLNB)
C                                       Not local: Determine LOAD area
C                                       based on the value of logical
C                                       TVLOAD as defined by the
C                                       startup procedure.
      ELSE
         XLEN = LEN (XLATED)
         CALL ZTRLOG (6, 'TVLOAD', XLEN, XLATED, XLNB, IERR)
         IF (IERR.EQ.0) VERSIN = XLATED(1:XLNB)
         END IF
C                                       Get name of default task message
C                                       terminal (logical TASKTT0 as
C                                       defined via the "tty" command in
C                                       $AIPS_ROOT/AIPSPATH.SH and .CSH
C                                       Default is your terminal
      XLEN = LEN (XLATED)
      CALL ZTRLOG (7, 'TASKTT0', XLEN, XLATED, XLNB, IERR)
      IF (IERR.NE.0) THEN
C                                       If that failed, just continue on.
         XLATED = 'NOT A TTY'
         XLNB = 9
         IERR = 0
         END IF
      MYTTY = XLATED(1:XLNB)
C                                       See if MYTTY matches any
C                                       RESSTTx.  If so, this is the
C                                       terminal reserved for POPS x.
C                                       If no match, then skip all this.
      DO 25 IPOPS = 1,NINTRN
         RESRVD(IPOPS) = .FALSE.
         CALL ZEHEX (IPOPS, 1, RESTTX(7:7))
         XLEN = LEN (XLATED)
         CALL ZTRLOG (7, RESTTX, XLEN, XLATED, XLNB, IERR)
C                                       Record reserved POPS #'s
         IF (IERR.NE.0) GO TO 25
            RESRVD(IPOPS) = .TRUE.
            RESTTY = XLATED(1:XLNB)
C                                       We are on a terminal reserved
C                                       for IPOPS. Make sure interactive
C                                       session is not already running
C                                       (if so, it's an error).
            IF (RESTTY.EQ.MYTTY) THEN
               MYPOPS = IPOPS
               CALL ZEHEX (IPOPS, 1, POPSNO)
               AIPSN = 'AIPS' // POPSNO
               CALL FILL (4, 0, PID)
               CALL ZTACTQ (AIPSN, PID(2), ACTIV1, IERR)
               AIPSN = 'DAIP' // POPSNO
               CALL FILL (4, 0, PID)
               CALL ZTACTQ (AIPSN, PID(2), ACTIV2, IERR)
               ACTIVE = (ACTIV1.OR.ACTIV2)
               IF (.NOT.ACTIVE) GO TO 50
C                                       Reserved session for this term.
C                                       is already running on some other
C                                       terminal.
                  WRITE (MSGTXT,1020) POPSNO
                  GO TO 990
               END IF
 25      CONTINUE
C                                       Not on a reserved terminal.
C                                       Get lowest inactive POPS #
C                                       not reserved.
      DO 30 IPOPS = 1,NINTRN
         IF (RESRVD(IPOPS)) GO TO 30
            CALL ZEHEX (IPOPS, 1, POPSNO)
            AIPSN = 'AIPS' // POPSNO
            CALL FILL (4, 0, PID)
            CALL ZTACTQ (AIPSN, PID(2), ACTIV1, IERR)
            AIPSN = 'DAIP' // POPSNO
            CALL FILL (4, 0, PID)
            CALL ZTACTQ (AIPSN, PID(2), ACTIV2, IERR)
            ACTIVE = (ACTIV1.OR.ACTIV2)
            IF (.NOT.ACTIVE) THEN
               MYPOPS = IPOPS
               GO TO 50
               END IF
 30      CONTINUE
C                                       All interactive POPS
C                                       numbers are in use.
      WRITE (MSGTXT,1040)
      GO TO 990
C                                       Set execution priority.
 50   CALL ZSTRTP (MYPOPS, MYPRIO, IERR)
C                                       Announce what's about to
C                                       happen.
      CALL ZOPEN (LUNCRT, IFIND, 1, PHFILE, F, T, T, IERR)
      WRITE (MSGBUF,1050) POPSNO, RLSNAM, MYPRIO
      CALL ZTTYIO ('WRIT', LUNCRT, IFIND, 72, MSGBUF, IERR)
C                                       GNU copyleft
      MSGBUF = 'AIPS: Copyright (C) 1995-2002 ' //
     *   'Associated Universities, Inc.'
      CALL ZTTYIO ('WRIT', LUNCRT, IFIND, 72, MSGBUF, IERR)
      MSGBUF = '       *** AIPS comes with ABSOLUTELY NO WARRANTY' //
     *   ' ***'
      CALL ZTTYIO ('WRIT', LUNCRT, IFIND, 72, MSGBUF, IERR)
C                                       Activate DAIPx (x = MYPOPS).
      AIPSN = 'DAIP '
      CALL ZACTV8 (AIPSN, MYPOPS, VERSIN, PID, IERR)
C                                       Never expect to get here, but
C                                       just in case.
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1060) IERR
 990     CALL MSGWRT (8)
C
 999  STOP
C-----------------------------------------------------------------------
 1020 FORMAT ('ZSTRTD: ',A5,' IS ALREADY RUNNING SOMEWHERE!')
 1040 FORMAT ('ZSTRTD: ******  ALL AIPS ARE IN USE.  ******')
 1050 FORMAT ('Start AIPS daemon number ',A1,' (release of ',
     *   A7,') at priority = ',I3)
 1060 FORMAT ('ZSTRTD: ACTIVATION ERROR = ',I6)
      END
