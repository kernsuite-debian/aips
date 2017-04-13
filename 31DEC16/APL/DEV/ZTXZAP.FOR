      SUBROUTINE ZTXZAP (LUN, OUTFIL, IERR)
C-----------------------------------------------------------------------
C! delete text file openable via ZTXOPN
C# Z Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C   delete a text file of the ZTXOPN type.
C   Inputs:
C      LUN      I       Logical unit number - used to temporarily open
C                       the file
C      OUTFIL   C*(*)   Full file name
C   Output:
C      IERR     I       Error return code: 0 => no error
C                          1 => error in inputs
C                          2 => LUN already in use
C                          3 => no room for LUN in FTAB
C                          4 => trouble translating logical
C                          5 => close/delete error
C                          6 => open error
C   Generic version: uses ZTXOPN and Fortran CLOSE
C-----------------------------------------------------------------------
      INTEGER   LUN, IERR
      CHARACTER OUTFIL*(*)
C
      INTEGER   JERR, IOSVAL, I, J, FIND
      LOGICAL   T, F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE. , .FALSE./
C-----------------------------------------------------------------------
C                                       Open the file in normal way
      CALL ZTXOPN ('QWRT', LUN, FIND, OUTFIL, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, OUTFIL
         GO TO 995
         END IF
C                                       Locate file in FTAB.
 20   IERR = 0
      CALL LSERCH ('SRCH', LUN, FIND, F, JERR)
      IF (JERR.EQ.0) GO TO 30
         IERR = 3
         WRITE (MSGTXT,1020) LUN
         GO TO 995
C                                       Close the text file.
 30   CLOSE (UNIT=LUN, STATUS='DELETE', IOSTAT=IOSVAL)
      IF (IOSVAL.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IOSVAL, LUN
         CALL MSGWRT (7)
         CALL ZERROR ('ZTXZAP', IOSVAL, ' ', -999, .FALSE.)
         IERR = 5
C                                       Clear FTAB entry.
 40   CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
      IF (JERR.EQ.0) THEN
         CALL FSERCH (FIND, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = ' '
            IF (I.EQ.2) NONNAM(J) = ' '
            IF (I.EQ.3) MAPNAM(J) = ' '
            END IF
         GO TO 999
      ELSE
         WRITE (MSGTXT,1040) LUN
         END IF
C
 995   CALL MSGWRT (7)
C
 999   RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTXZAP: ERROR',I5,' OPENING ',A53)
 1020 FORMAT ('ZTXZAP: LUN = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1030 FORMAT ('ZTXZAP: FORTRAN ERROR ',I6,' ON CLOSE FOR LUN = ',I2)
 1040 FORMAT ('ZTXZAP: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END
