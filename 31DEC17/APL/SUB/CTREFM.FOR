      SUBROUTINE CTREFM (DISK, CNO, VER, NEWVER, CATBLK, LUN1, LUN2,
     *   IROWF, TIMER, IRET)
C----------------------------------------------------------------------
C! Converts input VLBA CT table (ver 2) to version 3 used by AIPS
C# EXT-appl
C----------------------------------------------------------------------
C;  Copyright (C) 2005, 2007
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
C   Converts input VLBA CT table to version 3 used by AIPS
C   Inputs:
C      DISK     I      Volume number
C      CNO      I      Catalog slot number
C      VER      I      Version # of table in old format
C      NEWVER   I      Version # of output table; may equal VER
C      CATBLK   I(256) Catalog header block
C      LUN1     I      LUN for table I/O
C      LUN2     I      LUN for table I/O
C      TIMER    D(2)   Obs time range column added
C   Output:
C      IROWF    I      Start row in output table
C      IRET     I      Return code (0=>ok; else error)
C----------------------------------------------------------------------
      DOUBLE PRECISION TIMER(2)
      INTEGER   DISK, CNO, VER, NEWVER, CATBLK(256), LUN1, LUN2, IROWF,
     *   IRET
C
      INCLUDE 'INCS:DCTV.INC'
      CHARACTER UT1TYP*1, WOBTYP*1
      DOUBLE PRECISION TIME, UT1UTC, IATUTC, A1IAT, WOBXY(2), DPSI,
     *   DDPSI, DEPS, DDEPS, TRANGE(2)
C
      LOGICAL   WTABLE, WEXIST, WFITS, WSTAT
      CHARACTER LTYPE*2, LTEMP*12, LSTAT*4
      INTEGER   JBUFF1(512), JBUFF2(512), I, J, IOUTVR, CTKOL1(MAXCTC),
     *   CTKOL2(MAXCTC), CTNUM1(MAXCTC), CTNUM2(MAXCTC), IDUMMY,
     *   NROW, IERR, IREV, CTRNO1, CTRNO2, IRNO
      INCLUDE 'INCS:DMSG.INC'
C----------------------------------------------------------------------
      IRET = 0
C                                       Check for valid input table
      CALL ISTAB ('CT', DISK, CNO, VER, LUN1, JBUFF1, WTABLE, WEXIST,
     *   WFITS, IERR)
      IF (.NOT.WEXIST) GO TO 999
C                                       Read input table keywords
      CALL CTINI ('READ', JBUFF1, DISK, CNO, VER, CATBLK, LUN1, CTRNO1,
     *   CTKOL1, CTNUM1, IERR)
      IF (IERR.GT.0) THEN
         IRET = 4
         CALL TABERR ('READ', 'CTINI', 'CTREFM', IERR)
         GO TO 999
         END IF
C                                       Revision number
      IREV = CTTABV
      CALL TABIO ('CLOS', 0, 0, JBUFF1, JBUFF1, IERR)
C                                       Only convert v.2 -> v.3
      IF (IREV.GT.2) GO TO 999
C                                       Copy input file
      IOUTVR = 0
      I = MSGSUP
      MSGSUP = 31999
      CALL TABCOP ('CT', VER, IOUTVR, LUN1, LUN2, DISK, DISK, CNO,
     *   CNO, CATBLK, JBUFF1, JBUFF2, IERR)
      MSGSUP = I
      IF (IERR.NE.0) THEN
         IRET = 1
         WRITE (MSGTXT,1010) IERR
         GO TO 990
         END IF
C                                       Delete old version
      CALL RMEXT (DISK, CNO, 'CT', VER, CATBLK, JBUFF1, IERR)
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
      CALL CTINI ('READ', JBUFF1, DISK, CNO, IOUTVR, CATBLK, LUN1,
     *   CTRNO1, CTKOL1, CTNUM1, IERR)
      IF (IERR.GT.0) THEN
         IRET = 4
         CALL TABERR ('READ', 'CTINI', 'CTREFM', IERR)
         GO TO 999
         END IF
C                                       # rows in output table
      NROW = JBUFF1(5)
C                                       Create output table
      CALL CTINI ('WRIT', JBUFF2, DISK, CNO, NEWVER, CATBLK, LUN2,
     *   CTRNO2, CTKOL2, CTNUM2, IERR)
      IF (IERR.GT.0) THEN
         IRET = 6
         CALL TABERR ('WRIT', 'CTINI', 'CTREFM', IERR)
         GO TO 999
         END IF
      IROWF = CTRNO2
C                                       Loop and translate each row
      DO 30 J = 1,NROW
         IRNO = J
         CALL TABCT ('READ', JBUFF1, IRNO, CTKOL1, CTNUM1, TIME, UT1UTC,
     *      IATUTC, A1IAT, UT1TYP, WOBXY, WOBTYP, DPSI, DDPSI, DEPS,
     *      DDEPS, TRANGE, IERR)
         IF (IERR.GT.0) THEN
            IRET = 7
            CALL TABERR ('READ', 'TABCT', 'CTREFM', IERR)
            GO TO 999
            END IF
         TRANGE(1) = TIMER(1)
         TRANGE(2) = TIMER(2)
C                                       Write output row
         CALL TABCT ('WRIT', JBUFF2, CTRNO2, CTKOL2, CTNUM2, TIME,
     *      UT1UTC, IATUTC, A1IAT, UT1TYP, WOBXY, WOBTYP, DPSI, DDPSI,
     *      DEPS, DDEPS, TRANGE, IERR)
         IF (IERR.GT.0) THEN
            IRET = 7
            CALL TABERR ('WRIT', 'TABCT', 'CTREFM', IERR)
            GO TO 999
            END IF
30       CONTINUE
C                                       Close tables
      CALL TABIO ('CLOS', 0, IRNO, JBUFF1, JBUFF1, IERR)
      CALL TABIO ('CLOS', 0, CTRNO2, JBUFF2, JBUFF2, IERR)
C                                       Delete copy
      CALL RMEXT (DISK, CNO, 'CT', IOUTVR, CATBLK, JBUFF1, IERR)
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
1010  FORMAT ('CTREFM: ERROR',I3,' COPYING OLD TABLE')
1020  FORMAT ('CTREFM: ERROR',I3,' DELETING OLD TABLE')
1080  FORMAT ('CTREFM: ERROR',I3,' CHANGING CLRD STATUS')
1400  FORMAT ('CTREFM: ERROR',I3,' CHANGING CLWR STATUS')
      END
