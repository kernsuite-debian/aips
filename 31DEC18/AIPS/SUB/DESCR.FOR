      SUBROUTINE DESCR (WRK)
C-----------------------------------------------------------------------
C! destroys all scratch files for tasks which are no longer active
C# Catalog Service
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   DESCR destroys all scratch files associated with inactive tasks.
C   In these days of networked CPU's it will only destroy scratch files
C   associated with the current system since we don't interrogate remote
C   systems for process status.
C   Output:
C      WRK   I(256)   Scratch buffer
C-----------------------------------------------------------------------
      INTEGER   WRK(256)
C
      CHARACTER DTASK*6, DNAME*12, DCLASS*6, STATUS*4, PTYPE*2,
     *   SYSKYS(2)*8, SCRSYS*12
      HOLLERITH NAMESY(4)
      INTEGER  NLOCS(2), NKEYTP(2), PID(4), IERR, IVOL, ISEQ, I, CNO,
     *   IUSER, NDEST, NKEPT, NERR, HDR(256), INDC
      LOGICAL   ACTIVE, IAMOK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA DTASK /'DUMMY '/
      DATA NLOCS /1, 3/, NKEYTP /3, 3/
      DATA SYSKYS /'SYSNAME1', 'SYSNAME2' /
C-----------------------------------------------------------------------
      PTYPE = 'SC'
      NDEST = 0
      NKEPT = 0
      NERR = 0
      WRITE (MSGTXT,1000)
      CALL MSGWRT (2)
C                                       No: Build basic catlg name
      CALL BLDSNM (DTASK, NPOPS, DNAME, DCLASS, IERR)
C                                       Search catalogs
 10   DO 100 IVOL = 1,NVOL
         IF (.NOT.IAMOK(IVOL,'CA')) GO TO 100
         CNO = 0
         DO 90 I = 1,100
            ISEQ = 0
            IUSER = NLUSER
            DCLASS = ' '
            CNO = CNO + 1
            CALL CATDIR ('SRCN', IVOL, CNO, DNAME, DCLASS, ISEQ, PTYPE,
     *          IUSER, STATUS, WRK, IERR)
            IF (IERR.EQ.0) GO TO 15
               IF (IERR.EQ.5) GO TO 100
                  NERR = NERR + 1
                  WRITE (MSGTXT,1010) IERR, IVOL
                  CALL MSGWRT (6)
                  GO TO 100
C                                       Only delete our scratch files
 15         CALL CATKEY ('READ', IVOL, CNO, SYSKYS, 2, NLOCS, NAMESY,
     *          NKEYTP, WRK, IERR)
            IF (IERR.EQ.0) THEN
               CALL H2CHR (12, 1, NAMESY, SCRSYS)
               IF (HSTNAM.NE.SCRSYS) THEN
                  WRITE (MSGTXT,1015) IVOL, CNO, SCRSYS
                  CALL MSGWRT (6)
                  NKEPT = NKEPT + 1
                  GO TO 90
                  END IF
            ELSE IF (IERR.GT.20) THEN
               WRITE (MSGTXT,1016) IVOL, CNO
               CALL MSGWRT (6)
            ELSE
               NERR = NERR + 1
               WRITE (MSGTXT,1017) IERR, IVOL, CNO
               CALL MSGWRT (6)
               GO TO 90
               END IF
C                                       Is task active?
 20         CALL FILL (4, 0, PID)
            PID(1) = NLUSER
            CALL ZTACTQ (DCLASS, PID, ACTIVE, IERR)
            IF (.NOT.ACTIVE) GO TO 30
               NKEPT = NKEPT + 1
               GO TO 90
C                                       Get status: clear if needed
 30         CALL CATDIR ('INFO', IVOL, CNO, DNAME, DCLASS, ISEQ, PTYPE,
     *         IUSER, STATUS, WRK, IERR)
            IF (IERR.EQ.0) GO TO 40
               NERR = NERR + 1
               WRITE (MSGTXT,1030) IERR, IVOL, CNO
               CALL MSGWRT (6)
               GO TO 90
C                                       Clear read
 40         IF (STATUS.NE.'READ') GO TO 50
               CALL CATDIR ('CSTA', IVOL, CNO, DNAME, DCLASS, ISEQ,
     *            PTYPE, IUSER, 'CLRD', WRK, IERR)
               IF (IERR.EQ.0) GO TO 30
                  IF (IERR.EQ.10) GO TO 50
                     NERR = NERR + 1
                     WRITE (MSGTXT,1040) IERR, IVOL, CNO
                     CALL MSGWRT (6)
                     GO TO 90
C                                       Clear write
 50         IF (STATUS.NE.'WRIT') GO TO 60
               CALL CATDIR ('CSTA', IVOL, CNO, DNAME, DCLASS, ISEQ,
     *            PTYPE, IUSER, 'CLWR', WRK, IERR)
               IF (IERR.EQ.0) GO TO 30
                  IF (IERR.EQ.10) GO TO 60
                     NERR = NERR + 1
                     WRITE (MSGTXT,1040) IERR, IVOL, CNO
                     CALL MSGWRT (6)
                     GO TO 90
C                                       Destroy the file
 60         INDC = -32000
            CALL MDESTR (IVOL, CNO, HDR, WRK, INDC, IERR)
            IF (IERR.LE.0) GO TO 80
               NERR = NERR + 1
               WRITE (MSGTXT,1060) IERR, IVOL, CNO
               CALL MSGWRT (6)
               GO TO 90
 80         NDEST = NDEST + 1
 90         CONTINUE
 100     CONTINUE
      WRITE (MSGTXT,1100) NDEST, NKEPT
      CALL MSGWRT (2)
      WRITE (MSGTXT,1101) NERR
      IF (NERR.GT.0) CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Begin check for any ''standard'' scratch files')
 1010 FORMAT ('DESCR: ERROR',I5,' DISK',I3,' SEARCHING CATALOG')
 1015 FORMAT ('DESCR: DISK,CNO',I3,I4,' BELONG TO ', A20, ' - IGNORE')
 1016 FORMAT ('DESCR: DISK,CNO',I3,I4,' ASSUMED LOCAL')
 1017 FORMAT ('DESCR: ERROR',I5,' DISK,CNO',I3,I4,' READING SYSNAME')
 1030 FORMAT ('DESCR: ERROR',I5,' DISK,CNO',I3,I4,' GETTING STATUS')
 1040 FORMAT ('DESCR: ERROR',I5,' DISK,CNO',I3,I4,' CLEARING STATUS')
 1060 FORMAT ('DESCR: ERROR',I5,' DISK,CNO',I3,I4,' DELETING FILE')
 1100 FORMAT ('Scratch files -- destroyed:',I5,'  still active:',I5)
 1101 FORMAT ('DESCR: encountered',I4,' errors in doing this')
      END
