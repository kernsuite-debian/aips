      SUBROUTINE ZTCLOS (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! close text file opened with ZTOPEN
C# Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2013
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
C   Close the text file and clear the FTAB entry associated with LUN.
C   Inputs:
C      LUN      I   Logical unit number
C      FIND     I   Index in FTAB for LUN
C   Output:
C      IERR     I   Error return code: 0 => no error
C                      1 => close error
C                      2 => file already closed in FTAB
C                      3 => both errors
C                      4 => erroneous LUN
C   Generic version.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   JERR, IOSVAL, I, J
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 4
      IF ((LUN.LE.0) .OR. (LUN.GT.99)) THEN
         WRITE (MSGTXT,1000) LUN
         GO TO 995
         END IF
C                                       Valid device type?
      IF (MOD (DEVTAB(LUN), 2).NE.1) THEN
         WRITE (MSGTXT,1010) LUN, DEVTAB(LUN)
         GO TO 995
         END IF
C                                       Locate file in FTAB.
      IERR = 0
      CALL LSERCH ('SRCH', LUN, FIND, F, JERR)
      IF (JERR.NE.0) THEN
         IERR = 2
         WRITE (MSGTXT,1020) LUN
         GO TO 995
         END IF
C                                       Close the text file.
      CLOSE (UNIT=LUN, IOSTAT=IOSVAL)
      IF (IOSVAL.NE.0) THEN
         WRITE (MSGTXT,1030) IOSVAL, LUN
         CALL MSGWRT (7)
         IERR = 1
         END IF
C                                       Clear FTAB entry.any case
      CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
      IF (JERR.EQ.0) THEN
         CALL FSERCH (FIND, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = ' '
            IF (I.EQ.2) NONNAM(J) = ' '
            IF (I.EQ.3) MAPNAM(J) = ' '
            END IF
         GO TO 999
      ELSE
         IERR = IERR + 2
         WRITE (MSGTXT,1040) LUN
         END IF
C
 995  CALL MSGWRT (7)
C
 999   RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTCLOS: INVALID LUN = ',I2)
 1010 FORMAT ('ZTCLOS: INVALID DEVTAB(',I2,') = ',I1)
 1020 FORMAT ('ZTCLOS: LUN = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1030 FORMAT ('ZTCLOS: FORTRAN ERROR ',I6,' ON CLOSE FOR LUN = ',I2)
 1040 FORMAT ('ZTCLOS: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END
