      PROGRAM ZSTRTB
C-----------------------------------------------------------------------
C! starts up BATER with correct process name - standalone AIPS batch
C# System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002
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
C   ZSTRTB starts up BATERx (x = NPOPS = 2 * NINTRN + 1 in hex).
C   UNIX version
C-----------------------------------------------------------------------
      CHARACTER BATER*6, VERSIN*48, PHFILE*48, BATERN*6, MSGBUF*80,
     *   XLATED*256
      INTEGER   IBUF(256), IERR, IPOPS, PID(4), IFIND, LUNCRT, XLEN,
     *   XLNB
      LOGICAL   ACTIVE, T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA LUNCRT /5/, T, F /.TRUE., .FALSE./
      DATA BATER /'BATER '/
C-----------------------------------------------------------------------
C                                       Set values especially NINTRN.
      CALL ZDCHIN (.TRUE., IBUF)
C                                       Copy PRGNAM into TSKNAM.
      TSKNAM = 'ZSTRTB'
C                                       Get default version.
      CALL ZMYVER
      VERSIN = VERNAM(1:3)
C                                       Check for local version
C                                       BATER.EXE request.
      XLEN = LEN (XLATED)
      CALL ZTRLOG (10, 'BATERLOCAL', XLEN, XLATED, XLNB, IERR)
      IF (IERR.EQ.0) VERSIN = XLATED(1:XLNB)
C                                       Get POPS # for BATER.
 20   IPOPS = NINTRN + 1
      BATERN = 'BATER '
      CALL ZEHEX (IPOPS, 1, BATERN(6:6))
      CALL FILL (4, 0, PID)
C                                       Is it already in use?
      CALL ZTACTQ (BATERN, PID, ACTIVE, IERR)
      IF (ACTIVE) THEN
         WRITE (MSGTXT,1040)
         GO TO 990
         END IF
C                                       Announce what's about to
C                                       happen.
 50   CALL ZOPEN (LUNCRT, IFIND, 1, PHFILE, F, T, T, IERR)
      WRITE (MSGBUF,1050) BATERN(6:6), RLSNAM
      CALL ZTTYIO ('WRIT', LUNCRT, IFIND, 72, MSGBUF, IERR)
C                                       Activate BATERn (n = IPOPS).
      CALL ZACTV8 (BATER, IPOPS, VERSIN, PID, IERR)
C                                       Never expect to get here.
C                                       ZACTV8 has special code to
C                                       overlay this with BATER/AIPS
      IF (IERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1060) IERR
 990     CALL MSGWRT (8)
C
 999  STOP
C-----------------------------------------------------------------------
 1040 FORMAT ('ZSTRTB: ******  BATER IS IN USE.  ******')
 1050 FORMAT ('BEGIN THE ONE TRUE BATER NUMBER ',A1,' (RELEASE OF ',
     *   A7,')')
 1060 FORMAT ('ZSTRTB: ACTIVATION ERROR = ',I6)
      END
