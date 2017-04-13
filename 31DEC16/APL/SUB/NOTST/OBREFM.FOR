      SUBROUTINE OBREFM (DISK, CNO, VER, NEWVER, CATBLK, LUN1, LUN2,
     *   IROWF, DJULDY, IRET)
C----------------------------------------------------------------------
C! Converts input VLBA OB table (ver 1) to version 2 used by AIPS
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2007
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
C----------------------------------------------------------------------
C   Converts input VLBA OB table to version 2 used by AIPS
C   Inputs:
C      DISK     I      Volume number
C      CNO      I      Catalog slot number
C      VER      I      Version # of table in old format
C      NEWVER   I      Version # of output table; may equal VER
C      CATBLK   I(256) Catalog header block
C      LUN1     I      LUN for table I/O
C      LUN2     I      LUN for table I/O
C      DJULDY   D      Reference Julian day number (only v.1=>v.2)
C   Output:
C      IROWF    I      Start row in output table
C      IRET     I      Return code (0=>ok; else error)
C----------------------------------------------------------------------
      DOUBLE PRECISION DJULDY
      INTEGER DISK, CNO, VER, NEWVER, CATBLK(256), LUN1, LUN2, IROWF,
     *   IRET
C
      INCLUDE 'INCS:POBV.INC'
      LOGICAL WTABLE, WEXIST, WFITS, WSTAT
      CHARACTER LTYPE*2, LTEMP*12, LSTAT*4
      DOUBLE PRECISION DXYZOB(3), DVELOB(3), DTIMOB, DMJD
      REAL ANGLOB(3), ECLPOB(4), ORIOB
      INTEGER JBUFF1(512), JBUFF2(512), IOUTVR, I, J, IDUMMY, NKEY,
     *   NCOL, NREC, DATP(128,2), NROW, IERR, IREV, IPT, IPOINT,
     *   IOBRNO, OBKOLS(MAXOBC), OBNUMV(MAXOBC), IRNO, K, IANTOB,
     *   ISUBOB
      INCLUDE 'INCS:DMSG.INC'
C                                       Define table format OB ver #1
      INTEGER NOBKEY, MOBAXC, MOBXSP, MOBXDP
C                                       # keywords
      PARAMETER (NOBKEY = 1)
C                                       # columns
      PARAMETER (MOBAXC = 4)
C                                       Logical record length in
C                                       single precision words
      PARAMETER (MOBXSP = 30)
C                                       in double precision words
      PARAMETER (MOBXDP = MOBXSP / 2)
C                                       Parameters specifying the
C                                       position of variables within
C                                       a table row. Third character
C                                       gives variable type (D=double)
      INTEGER OBDTI1, OBDXY1, OBDVE1
C                                       Time
      PARAMETER (OBDTI1 = 2)
C                                       Spacecraft position (x, y, z)
      PARAMETER (OBDXY1 = 3)
C                                       Spacecraft velocity (vx,vy,vz)
      PARAMETER (OBDVE1 = 4)
C
      CHARACTER LKEYW(NOBKEY)*8
      DOUBLE PRECISION RECD(MOBXDP)
      REAL RECORD(MOBXSP)
      INTEGER KLOCS(NOBKEY), KEYVAL(NOBKEY), KEYTYP(NOBKEY),
     *   KOLS(MOBAXC), NUMV(MOBAXC)
      EQUIVALENCE (RECD, RECORD)
      DATA LKEYW /'TABREV  '/
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Compute modified Julian dayno
      DMJD = DJULDY - 2400000.5D0
C                                       Check for valid input table
      CALL ISTAB ('OB', DISK, CNO, VER, LUN1, JBUFF1, WTABLE, WEXIST,
     *   WFITS, IERR)
      IF (.NOT.WEXIST) GO TO 999
C                                       Read input table keywords
      CALL TABINI ('READ', 'OB', DISK, CNO, VER, CATBLK, LUN1,
     *   NKEY, NREC, NCOL, DATP, JBUFF1, IERR)
      IF (IERR.GT.0) THEN
         IRET = 4
         CALL TABERR ('READ', 'TABINI', 'OBREFM', IERR)
         GO TO 990
         END IF
C
      I = MSGSUP
      MSGSUP = 32000
      CALL TABKEY ('READ', LKEYW, NOBKEY, JBUFF1, KLOCS, KEYVAL, KEYTYP,
     *   IERR)
      MSGSUP = I
      IF ((IERR.GE.1).AND.(IERR.LE.20)) THEN
         IRET = 5
         CALL TABERR ('READ', 'TABKEY', 'OBREFM', IERR)
         GO TO 990
         END IF
C                                       Revision number
      IPT = 1
      IPOINT = 1
      IREV = KEYVAL(IPOINT)
C                                       Get table column indices
C                                       and lengths
      DO 40 I = 1, NCOL
         KOLS(I) = DATP(I,1)
         NUMV(I) = DATP(I,2) / 10
40       CONTINUE
C
      CALL TABIO ('CLOS', 0, 0, JBUFF1, JBUFF1, IERR)
C                                       Only convert v.1 -> v.2
C                                       (no other versions exist)
      IF (IREV.NE.1) GO TO 999
C                                       Copy input file
      IOUTVR = 0
      I = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('OB', VER, IOUTVR, LUN1, LUN2, DISK, DISK, CNO,
     *   CNO, CATBLK, JBUFF1, JBUFF2, IERR)
      MSGSUP = I
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Delete old version
      CALL RMEXT (DISK, CNO, 'OB', VER, CATBLK, JBUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 2
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', JBUFF1, IERR)
C                                       Determine file status
      LTYPE = 'UV'
      WSTAT = .FALSE.
      CALL CATDIR ('INFO', DISK, CNO, LTEMP, LTEMP, IDUMMY, LTYPE,
     *   IDUMMY, LSTAT, JBUFF1, IERR)
C                                       Change status
      IF (LSTAT.NE.'WRIT') THEN
         CALL STATCH (LSTAT, 'WRIT', DISK, CNO, LTYPE, JBUFF1, IERR)
         IF (IERR.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT,1080) IERR
            GO TO 990
            END IF
         WSTAT = .TRUE.
         END IF
C                                       Open input table
      CALL TABINI ('READ', 'OB', DISK, CNO, IOUTVR, CATBLK, LUN2,
     *   NKEY, NREC, NCOL, DATP, JBUFF1, IERR)
      IF (IERR.GT.0) THEN
         IRET = 4
         CALL TABERR ('READ', 'TABINI', 'OBREFM', IERR)
         GO TO 990
         END IF
C                                       # rows in output table
      NROW = JBUFF1(5)
C                                       Create output table
      CALL OBINI ('WRIT', JBUFF2, DISK, CNO, NEWVER, CATBLK, LUN1,
     *   IOBRNO, OBKOLS, OBNUMV, IERR)
      IF (IERR.GT.0) THEN
         IRET = 6
         CALL TABERR ('WRIT', 'OBINI', 'OBREFM', IERR)
         GO TO 990
         END IF
      IROWF = IOBRNO
C                                       Loop and translate each row
      DO 300 J = 1, NROW
         IRNO = J
C                                       Read input row
         CALL TABIO ('READ', 0, IRNO, RECORD, JBUFF1, IERR)
         IF (IERR.GT.0) THEN
            IRET = 7
            CALL TABERR ('READ', 'TABIO', 'OBREFM', IERR)
            GO TO 990
            END IF
C                                       Time (subtract MJD)
         DTIMOB = RECD(KOLS(OBDTI1)) - DMJD
C                                       Spacecraft position and
C                                       velocity
         DO 100 K = 1, 3
            DXYZOB(K) = RECD(KOLS(OBDXY1) + K - 1)
            DVELOB(K) = RECD(KOLS(OBDVE1) + K - 1)
100         CONTINUE
C                                       Initialize missing fields
         IANTOB = 0
         ISUBOB = 0
         CALL RFILL (3, 0.0, ANGLOB)
         CALL RFILL (4, 0.0, ECLPOB)
         ORIOB = 0
C                                       Write output row
         CALL TABOB ('WRIT', JBUFF2, IOBRNO, OBKOLS, OBNUMV, IANTOB,
     *      ISUBOB, DTIMOB, DXYZOB, DVELOB, ANGLOB, ECLPOB, ORIOB,
     *      IERR)
         IF (IERR.GT.0) THEN
            IRET = 8
            CALL TABERR ('WRIT', 'TABOB', 'OBREFM', IERR)
            GO TO 990
            END IF
300      CONTINUE
C                                       Close tables
      CALL TABIO ('CLOS', 0, IRNO, JBUFF1, JBUFF1, IERR)
      CALL TABIO ('CLOS', 0, IOBRNO, JBUFF2, JBUFF2, IERR)
C                                       Delete copy
      CALL RMEXT (DISK, CNO, 'OB', IOUTVR, CATBLK, JBUFF1, IERR)
      IF (IERR.NE.0) THEN
         IRET = 9
         WRITE (MSGTXT,1020) IERR
         GO TO 990
         END IF
C                                       Check if changed status
C                                       Change status
      IF (WSTAT) THEN
         CALL STATCH ('WRIT', LSTAT, DISK, CNO, LTYPE, JBUFF1, IERR)
         IF (IERR.NE.0) THEN
            IRET = 10
            WRITE (MSGTXT,1400) IERR
            GO TO 990
            END IF
         END IF
C                                       Update CATBLK
      CALL CATIO ('UPDT', DISK, CNO, CATBLK, 'REST', JBUFF1, IERR)
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C
999   RETURN
C----------------------------------------------------------------------
1010  FORMAT ('OBREFM: ERROR',I3,' COPYING OLD TABLE')
1020  FORMAT ('OBREFM: ERROR',I3,' DELETING OLD TABLE')
1080  FORMAT ('OBREFM: ERROR',I3,' CHANGING CLRD STATUS')
1400  FORMAT ('OBREFM: ERROR',I3,' CHANGING CLWR STATUS')
      END
