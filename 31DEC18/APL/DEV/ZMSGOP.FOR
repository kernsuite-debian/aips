      SUBROUTINE ZMSGOP (LUN, FIND, IVOL, PNAME, IERR)
C-----------------------------------------------------------------------
C! open a message file or message terminal
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2011
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
C   Open a message file (much like ZOPEN, but does not call MSGWRT to
C   avoid recursion).  Non-map, exclusive, patient assumed on open.
C   Inputs:
C      LUN      I      Logical unit number
C      IVOL     I      Disk volume containing file, 1,2,3,...
C      PNAME    C*48   Physical file name
C   Output:
C      FIND     I      Index in FTAB to file control block for LUN
C      IERR     I      Error return code: 0 => no error
C                         1 => LUN already in use
C                         2 => file not found
C                         3 => volume not found
C                         4 => exclusive use denied
C                         5 => no room for LUN in FTAB
C                         6 => other open errors
C   Generic version - calls ZDAOPN, ZTTOPN, ZMSGER.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IVOL, IERR
      CHARACTER PNAME*48
C
      INTEGER   JERR, ERRLUN, ERRTER, FCBOFF, IMAP, IEXCL, DELMAX,
     *   DELCNT, DLMAX2, DLCNT2, IOSVAL, I, J
      REAL      TDELAY
      HOLLERITH ANAME(12)
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA ERRLUN, ERRTER, TDELAY, DELMAX, DLMAX2 /12, 6, 1.0, 30, 60/
C-----------------------------------------------------------------------
C                                       Set integer map, exclusive use
      IMAP = 0
      IEXCL = 1
C                                       Valid LUN?
      IERR = 6
      IF ((LUN.EQ.ERRLUN) .OR. (LUN.EQ.ERRTER)) GO TO 10
         WRITE (ERRTER,1000) LUN
         GO TO 999
C                                       Allocate area in FTAB for LUN.
 10   IF (LUN.EQ.ERRLUN) THEN
         CALL LSERCH ('OPEN', LUN, FIND, F, JERR)
         IF (JERR.EQ.0) GO TO 20
C                                       LUN already in use.
            IF (JERR.EQ.2) THEN
               IERR = 1
               WRITE (ERRTER,1011) LUN
C                                       No room in FTAB for LUN.
            ELSE IF (JERR.EQ.3) THEN
               IERR = 5
               WRITE (ERRTER,1012) LUN
C                                       Other error
            ELSE
               WRITE (ERRTER,1010) JERR, LUN
               END IF
            GO TO 999
C                                       Okay, so far.
 20      IERR = 0
         DELCNT = 0
         DLCNT2 = 0
         FCBOFF = FIND + NMOFF
C                                       Volume (disk) number in range?
         IF ((IVOL.GT.0) .AND. (IVOL.LE.NVOL)) GO TO 30
            IERR = 3
            WRITE (ERRTER,1020) IVOL
            GO TO 900
C                                       Open disk message file.
 30      CALL CHR2H (48, PNAME, 1, ANAME)
         CALL ZDAOPM (FTAB(FCBOFF), ANAME, IMAP, IEXCL, IERR)
         IF (IERR.NE.0) THEN
C                                       File not found?
            IF (IERR.EQ.2) THEN
               WRITE (ERRTER,1030) PNAME(1:24)
C                                       Volume not found?
            ELSE IF (IERR.EQ.3) THEN
               WRITE(ERRTER,1031) IVOL, PNAME(1:24)
C                                       File busy?
            ELSE IF (IERR.EQ.4) THEN
C                                       Wait and try again every TDELAY
C                                       seconds up to DELMAX*DLMAX2
C                                       times.  Issue a message every
C                                       DLMAX2 wait.
               DELCNT = DELCNT + 1
               IF (DELCNT.LE.DELMAX) THEN
                  CALL ZDELAY (TDELAY, IERR)
               ELSE
                  DLCNT2 = DLCNT2 + 1
                  IF (DLCNT2.GT.DLMAX2) GO TO 33
                     DELCNT = 0
                     WRITE (6,1032) PNAME(1:24)
                  END IF
               GO TO 30
C
 33            WRITE (6,1033) PNAME(1:24)
               GO TO 900
C                                       Other open error.
 34         ELSE
               CALL ZMSGER ('ZDAOPN', FTAB(FCBOFF+FCBERR), PNAME,
     *            FTAB(FCBOFF), F)
               END IF
            GO TO 900
            END IF
C                                       Open message termimal here.
      ELSE
         CALL ZTTOPN (LUN, FIND, IERR)
         IF (IERR.NE.0) THEN
            IF (FTAB(FIND+1+NFCBER).GT.0) CALL ZMSGER ('ZTTOPN',
     *         FTAB(FIND+1+NFCBER), ' ', -999, .FALSE.)
            WRITE (ERRTER,1040,IOSTAT=IOSVAL) IERR, LUN
            IERR = 6
            END IF
         END IF
C                                       Clear FTAB entry for LUN.
 900  IF (IERR.NE.0) THEN
         CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
C                                       or add file name to lists
      ELSE
         CALL FSERCH (FIND, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = PNAME
            IF (I.EQ.2) NONNAM(J) = PNAME
            IF (I.EQ.3) MAPNAM(J) = PNAME
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ZMSGOP: INVALID LUN = ',I2)
 1010 FORMAT (' ZMSGOP: LSERCH ERROR ',I2,' ON OPEN, LUN = ',I2)
 1011 FORMAT (' ZMSGOP: LUN = ',I2,' ALREADY OPENED IN FTAB')
 1012 FORMAT (' ZMSGOP: NO ROOM IN FTAB FOR LUN = ',I2)
 1020 FORMAT (' ZMSGOP: VOLUME NUMBER = ',I6,' OUT OF RANGE')
 1030 FORMAT (' ZMSGOP: FILE ',A24,' NOT FOUND')
 1031 FORMAT (' ZMSGOP: VOLUME ',I2,' FOR FILE ',A24,' NOT FOUND')
 1032 FORMAT (' ZMSGOP: STILL WAITING FOR FILE ',A24)
 1033 FORMAT (' ZMSGOP: FILE ',A24,' NOT AVAILABLE')
 1040 FORMAT (' ZMSGOP: ZTTOPN RETURNS ERROR = ',I3,' FOR LUN = ',I3)
      END
