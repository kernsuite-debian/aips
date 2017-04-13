      SUBROUTINE CATFIX (DISK, CNO, STAT)
C-----------------------------------------------------------------------
C! Changed catalog status to requested with messages but no errors
C# Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 2011
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
C   CATFIX changes the status of a file to the requested status from
C   whatever it was.  Produces messages but no error return.
C   Inputs:
C      DISK   I     Disk number
C      CNO    I     Catalog number
C      STAT   C*4   'READ', 'WRIT', '    ' (none)
C                   'NOTR' (not read), 'NOTW' (not write)
C-----------------------------------------------------------------------
      INTEGER   DISK, CNO
      CHARACTER STAT*4
C
      INTEGER   CIND, CMAX, IERR, BUFF(256), NWPL, NLPR, CMOD, NREC,
     *   NIND, CLUN
      INCLUDE 'INCS:DMSG.INC'
      DATA CLUN /15/
C-----------------------------------------------------------------------
C                                       check
      IF ((DISK.GT.0) .AND. (CNO.GT.0) .AND. ((STAT.EQ.'READ') .OR.
     *   (STAT.EQ.'WRIT') .OR. (STAT.EQ.' ') .OR. (STAT.EQ.'REST') .OR.
     *   (STAT.EQ.'NOTR') .OR. (STAT.EQ.'NOTW'))) THEN
         CALL CATOPN (DISK, CIND, BUFF, CMAX, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'OPEN CATALOG FILE', DISK
            GO TO 990
            END IF
         NWPL = 10
         NLPR = 256 / NWPL
         CMOD = (CNO - 1) / NLPR
         NREC = 2 + CMOD
         NIND = 1 + NWPL * (CNO - NLPR*CMOD -1)
         CALL ZFIO ('READ', CLUN, CIND, NREC, BUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ CAT RECORD', DISK
            GO TO 980
            END IF
         IF (BUFF(NIND).EQ.-1) THEN
            WRITE (MSGTXT,1010) DISK, CNO, 'NO CATALOG ENTRY'
            GO TO 980
         ELSE IF (BUFF(NIND).NE.NLUSER) THEN
            WRITE (MSGTXT,1010) DISK, CNO, 'NOT OWNED BY LOGIN USER'
            GO TO 980
            END IF
         IF (STAT.EQ.'READ') THEN
            IF (BUFF(NIND+1).LT.0) THEN
               WRITE (MSGTXT,1020) DISK, CNO, 'WRIT', 'READ'
               CALL MSGWRT (7)
               END IF
            BUFF(NIND+1) = 1
         ELSE IF (STAT.EQ.'WRIT') THEN
            IF (BUFF(NIND+1).GT.0) THEN
               WRITE (MSGTXT,1020) DISK, CNO, 'READ', 'WRIT'
               CALL MSGWRT (7)
               END IF
            BUFF(NIND+1) = -1
         ELSE IF (STAT.EQ.'NOTW') THEN
            IF (BUFF(NIND+1).LT.0) THEN
               WRITE (MSGTXT,1020) DISK, CNO, 'WRIT', 'none'
               CALL MSGWRT (7)
               BUFF(NIND+1) = 0
               END IF
         ELSE IF (STAT.EQ.'NOTR') THEN
            IF (BUFF(NIND+1).GT.0) THEN
               WRITE (MSGTXT,1020) DISK, CNO, 'READ', 'none'
               CALL MSGWRT (7)
               BUFF(NIND+1) = 0
               END IF
         ELSE
            IF (BUFF(NIND+1).GT.0) THEN
               WRITE (MSGTXT,1020) DISK, CNO, 'READ', 'none'
               CALL MSGWRT (7)
            ELSE IF (BUFF(NIND+1).LT.0) THEN
               WRITE (MSGTXT,1020) DISK, CNO, 'WRIT', 'none'
               CALL MSGWRT (7)
               END IF
            BUFF(NIND+1) = 0
            END IF
         CALL ZFIO ('WRIT', CLUN, CIND, NREC, BUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRITE CAT RECORD', DISK
            GO TO 980
            END IF
         GO TO 985
      ELSE
         WRITE (MSGTXT,1100) DISK, CNO, STAT
         GO TO 990
         END IF
C
 980  CALL MSGWRT (8)
C
 985  CALL ZCLOSE (CLUN, CIND, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CATFIX: ERROR',I4,' ON ',A, ' DISK',I3)
 1010 FORMAT ('CATFIX DISK, CNO=',I3,I7,1X,A)
 1020 FORMAT ('CATFIX DISK, CNO=',I3,I7,' changed from ',A,' to ',A)
 1100 FORMAT ('CATFIX USAGE ERROR DISK, CNO, STAT',I3,I7,' ''',A4,'''')
      END
