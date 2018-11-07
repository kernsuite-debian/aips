      SUBROUTINE ZDEAOP (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! opens DeAnza TV device
C# Z TV-IO
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
C   ZDEAOP opens A TV logical file
C   Inputs:
C      LUN    I   FORTRAN logical file number.
C   Output:
C      FIND   I   Index into FTAB for the file control block.
C      IERR   I   Error return code: 0 = no error
C                    1 = LUN already in use
C                    2 = file not found
C                    3 = volume not found
C                    4 = excl requested but not available
C                    5 = no room for lun
C                    6 = other open errors
C   Generic version calls ZDEAO2, ZDEAXF.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      CHARACTER PNAME*48
      INTEGER   JERR, FCBOFF, TVLUN, TVDEVN, I, J
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA TVLUN, TVDEVN /9, 4/
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 6
C                                       Valid LUN?
      IF (LUN.EQ.TVLUN) GO TO 10
         WRITE (MSGTXT,1000) LUN
         GO TO 995
C                                       Valid device type?
 10   IF (DEVTAB(LUN).EQ.TVDEVN) GO TO 20
         WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
         GO TO 995
C                                       Allocate area in FTAB for LUN.
 20   CALL LSERCH ('OPEN', LUN, FIND, F, JERR)
      IF (JERR.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) JERR, 'OPEN', LUN
C                                       LUN already in use.
         IF (JERR.EQ.2) THEN
            CALL MSGWRT (6)
            IERR = 1
            WRITE (MSGTXT,1021) LUN
C                                       No room in FTAB for LUN.
         ELSE IF (JERR.EQ.3) THEN
            CALL MSGWRT (6)
            IERR = 5
            WRITE (MSGTXT,1022) LUN
            END IF
         GO TO 995
 30   CALL ZPHFIL ('TV', 1, 1, 1, PNAME, IERR)
C                                       TV DeAnza open
      FCBOFF = FIND + NMOFF
      CALL ZDEAO2 (FTAB(FCBOFF), PNAME, IERR)
      IF (IERR.EQ.0) GO TO 50
C                                       No such logical device.
         IF (IERR.EQ.2) THEN
            WRITE (MSGTXT,1030) PNAME(1:24)
            CALL MSGWRT (6)
C                                       No such device.
         ELSE IF (IERR.EQ.3) THEN
            WRITE (MSGTXT,1031) PNAME(1:24)
            CALL MSGWRT (6)
C                                       Other open error.
         ELSE
            CALL ZERROR ('ZDEAO2', FTAB(FCBOFF+FCBERR), PNAME,
     *         FTAB(FCBOFF), F)
            END IF
         GO TO 900
C                                       Allocate Deanza
 50   CALL ZDEAXF ('DAT ', FTAB(FCBOFF), 0, 0, 0, T, JERR)
      IF (JERR.EQ.0) GO TO 100
         IERR = 6
         WRITE (MSGTXT,1050) LUN
         CALL MSGWRT (6)
         GO TO 900
C                                       store file name
 100  CALL FSERCH (FIND, I, J, JERR)
      IF (JERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = PNAME
         IF (I.EQ.2) NONNAM(J) = PNAME
         IF (I.EQ.3) MAPNAM(J) = PNAME
         END IF
      GO TO 999
C                                       Errors:
C                                       Remove FTAB entry for LUN.
 900  CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
      IF (JERR.EQ.0) GO TO 999
         IERR = IERR + 2
         WRITE (MSGTXT,1020) JERR, 'CLOS', LUN
C                                       print msg
 995  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDEAOP: LUN = ',I2,' INVALID FOR TV DEVICE')
 1010 FORMAT ('ZDEAOP: DEVTAB(',I2,') = ',I1,' INVALID FOR TV DEVICE')
 1020 FORMAT ('ZDEAOP: LSERCH ERROR ',I2,' ON ',A4,' LUN = ',I2)
 1021 FORMAT ('ZDEAOP: LUN = ',I2,' ALREADY OPENED IN FTAB')
 1022 FORMAT ('ZDEAOP: NO ROOM IN FTAB FOR LUN = ',I2)
 1030 FORMAT ('ZDEAOP: NO SUCH LOGICAL DEVICE = ',A24)
 1031 FORMAT ('ZDEAOP: TV DEVICE ',A24,' NOT FOUND')
 1050 FORMAT ('ZDEAOP: CHANNEL ASSIGN ERROR',I5)
      END
