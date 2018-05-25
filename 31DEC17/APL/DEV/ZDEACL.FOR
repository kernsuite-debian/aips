      SUBROUTINE ZDEACL (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! close DeAnza TV device
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
C   ZDEACL closes DeAnza TV device.
C   Inputs:
C      LUN    I   logical unit number
C      FIND   I   pointer into FTAB
C   Output:
C      IERR   I   error code: 0 -> no error
C                    1 -> Deaccess or Deassign error
C                    2 -> file already closed in FTAB
C                    3 -> both errors
C                    4 -> erroneous LUN
C   Generic version - calls ZDEAXF (deallocate) and ZDEAC2.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   JERR, TVLUN, TVDEVN, FCBOFF, I, J
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA TVLUN, TVDEVN /9, 4/
C-----------------------------------------------------------------------
      IERR = 4
C                                       Valid LUN?
      IF (LUN.EQ.TVLUN) GO TO 10
         WRITE (MSGTXT,1000) LUN
         GO TO 995
C                                       Valid device type?
 10   IF (DEVTAB(LUN).EQ.TVDEVN) GO TO 20
         WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
         GO TO 995
C                                       Locate LUN in FTAB.
 20   IERR = 0
      CALL LSERCH ('SRCH', LUN, FIND, F, JERR)
      IF (JERR.EQ.0) GO TO 30
         IERR = 2
         WRITE (MSGTXT,1020) LUN
         GO TO 995
C                                       deallocate DEANZA
 30   FCBOFF = FIND + NMOFF
      CALL ZDEAXF ('DET ', FTAB(FCBOFF), 0, 0, 0, T, JERR)
      IF (JERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) JERR
         CALL MSGWRT (7)
C                                       Close the device.
 40   CALL ZDEAC2 (FTAB(FCBOFF), JERR)
      IF (JERR.EQ.0) GO TO 50
         IERR = 1
         WRITE (MSGTXT,1040) JERR
         CALL MSGWRT (7)
         CALL ZERROR ('ZDEAC2', FTAB(FCBOFF+FCBERR), ' ', FTAB(FCBOFF),
     *      F)
C                                       Clear FTAB entry.
 50   CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
      IF (JERR.NE.0) THEN
         IERR = IERR + 2
         WRITE (MSGTXT,1050) LUN
      ELSE
         CALL FSERCH (FIND, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = ' '
            IF (I.EQ.2) NONNAM(J) = ' '
            IF (I.EQ.3) MAPNAM(J) = ' '
            END IF
         GO TO 999
         END IF
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZDEACL: INVALID LUN = ',I4)
 1010 FORMAT ('ZDEACL: DEVTAB(',I2,') = ',I2,' = INVALID DEVICE TYPE')
 1020 FORMAT ('ZDEACL: LUN = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1030 FORMAT ('ZDEACL: ZDEAXF RETURNS ERROR = ',I6)
 1040 FORMAT ('ZDEACL: ZDEAC2 RETURNS ERROR = ',I6)
 1050 FORMAT ('ZDEACL: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END
