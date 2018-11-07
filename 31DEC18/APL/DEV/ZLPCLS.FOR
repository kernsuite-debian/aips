      SUBROUTINE ZLPCLS (LUN, IERR)
C-----------------------------------------------------------------------
C! close an open printer device
C# Z Printer
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2004
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
C   Close a line printer file, spool it to a printer and delete it.
C   Inputs:
C      LUN      I   Logical unit number
C   Output:
C      IERR     I   Error return code: 0 => no error
C                      1 => close error
C   Generic version - closes file, calls ZLPCL2
C-----------------------------------------------------------------------
      INTEGER   LUN, IERR
C
      INTEGER   LLEN, DELFIL, SYSERR, IOSVAL, ITRIM
      LOGICAL   F
      CHARACTER LPRFIL*256
      HOLLERITH HPRFIL(64)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 1
C                                       Get the name of the line printer
C                                       file as opened on LUN.
      INQUIRE (UNIT=LUN, NAME=LPRFIL, IOSTAT=IOSVAL)
      IF (IOSVAL.NE.0) THEN
         WRITE (MSGTXT,1000) IOSVAL, LUN
         GO TO 995
         END IF
C                                       Perform the Fortran CLOSE.
      CLOSE (UNIT=LUN, IOSTAT=IOSVAL)
      IF (IOSVAL.NE.0) THEN
         WRITE (MSGTXT,1010) IOSVAL, LUN
         GO TO 995
         END IF
C                                       Print and delete the file.
      LLEN = ITRIM (LPRFIL)
      DELFIL = INDEX (LPRFIL, 'ZLPOPN')
      DELFIL = MIN (DELFIL, 1)
C
      IF (DELFIL.EQ.0) THEN
         IERR = 0
C                                       print and  delete
      ELSE
         DELFIL = PRDELE
         IF (SYSTYP.EQ.'VMS') THEN
            CALL ZLPCL2 (LLEN, LPRFIL, DELFIL, PRTMAX, SYSERR, IERR)
         ELSE
            CALL CHR2H (256, ' ', 1, HPRFIL)
            CALL CHR2H (LLEN, LPRFIL, 1, HPRFIL)
            CALL ZLPCL2 (LLEN, HPRFIL, DELFIL, PRTMAX, SYSERR, IERR)
            END IF
         IF (IERR.NE.0) CALL ZERROR ('ZLPCL2', SYSERR, ' ', -999, F)
         END IF
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZLPCLS: FORTRAN ERROR ',I6,' ON INQUIRE FOR LUN = ',I6)
 1010 FORMAT ('ZLPCLS: FORTRAN ERROR ',I6,' ON CLOSE FOR LUN = ',I6)
      END
