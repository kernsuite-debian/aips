      SUBROUTINE ZTXREN (LUNI, INFIL, LUNO, OUTFIL, IERR)
C-----------------------------------------------------------------------
C! Rename INFILE to OUTFIL
C# Z Text
C-----------------------------------------------------------------------
C;  Copyright (C) 1999-2000, 2012
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
C   Delete a text file of the ZTXOPN type after copying it.
C   Inputs:
C      LUNI     I       LUN to use for INFIL
C      INFIL    C*(*)   Full file name to read and delete
C      LUNI     I       LUN to use for OUTFIL
C      OUTFIL   C*(*)   Full new file name
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
      INTEGER   LUNI, LUNO, IERR
      CHARACTER INFIL*(*), OUTFIL*(*)
C
      INTEGER   JERR, IOSVAL, I, J, INDI, INDO, JTRIM, NC
      CHARACTER LINE*132
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Open the file in normal way
      CALL ZTXOPN ('QRED', LUNI, INDI, INFIL, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN', INFIL
         GO TO 995
         END IF
      CALL ZTXOPN ('QWRT', LUNO, INDO, OUTFIL, F, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'OPEN', OUTFIL
         GO TO 995
         END IF
      DO 20 I = 1,1000000000
         CALL ZTXIO ('READ', LUNI, INDI, LINE, IERR)
         IF (IERR.EQ.2) GO TO 25
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'READ', INFIL
            GO TO 995
            END IF
         NC = JTRIM(LINE)
         CALL ZTXIO ('WRIT', LUNO, INDO, LINE(:NC), IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) IERR, 'WRIT', OUTFIL
            GO TO 995
            END IF
 20      CONTINUE
 25   IERR = 0
C                                       Close output
      CALL ZTXCLS (LUNO, INDO, JERR)
C                                       Close the text file.
 30   CLOSE (UNIT=LUNI, STATUS='DELETE', IOSTAT=IOSVAL)
      IF (IOSVAL.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IOSVAL, LUNI
         CALL MSGWRT (7)
         CALL ZERROR ('ZTXREN', IOSVAL, ' ', -999, .FALSE.)
         IERR = 5
C                                       Clear FTAB entry.
 40   CALL LSERCH ('CLOS', LUNI, INDI, F, JERR)
      IF (JERR.EQ.0) THEN
         CALL FSERCH (INDI, I, J, JERR)
         IF (JERR.EQ.0) THEN
            IF (I.EQ.1) DEVNAM(J) = ' '
            IF (I.EQ.2) NONNAM(J) = ' '
            IF (I.EQ.3) MAPNAM(J) = ' '
            END IF
         GO TO 999
      ELSE
         WRITE (MSGTXT,1040) LUNI
         END IF
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZTXREN: ERROR',I5,' ',A,'ING ',A53)
 1020 FORMAT ('ZTXREN: LUNI = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1030 FORMAT ('ZTXREN: FORTRAN ERROR ',I6,' ON CLOSE FOR LUNI = ',I2)
 1040 FORMAT ('ZTXREN: LUNI = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END
