      SUBROUTINE ZLPCLS (LUN, IERR)
C-----------------------------------------------------------------------
C! close an open printer device
C# Z Printer
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
C   Close a line printer file, spool it to a printer and optionally
C   delete it.
C   Inputs:
C      LUN      I   Logical unit number
C   Output:
C      IERR     I   Error return code: 0 => no error
C                      1 => close error
C   VMS version
C-----------------------------------------------------------------------
      INTEGER   LUN, IERR
C
      INTEGER   LLEN, DELFIL, SYSERR, IOSVAL, ITRIM
      LOGICAL   F
      CHARACTER LPRFIL*128
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
      IERR = 0
C                                       Get the name of the line printer
C                                       file as opened on LUN.
      INQUIRE (UNIT=LUN, NAME=LPRFIL, IOSTAT=IOSVAL)
      IF (IOSVAL.NE.0) THEN
         WRITE (MSGTXT,1000) IOSVAL, LUN
         CALL MSGWRT (6)
         LPRFIL = ' '
         END IF
C                                       Perform the Fortran CLOSE.
      DELFIL = INDEX (LPRFIL, 'ZLPOPN')
      IF (DELFIL.GT.0) THEN
         CLOSE (UNIT=LUN, DISP='PRINT/DELETE', IOSTAT=IOSVAL)
      ELSE
         CLOSE (UNIT=LUN, DISP='KEEP', IOSTAT=IOSVAL)
         END IF
      IF (IOSVAL.NE.0) THEN
         WRITE (MSGTXT,1010) IOSVAL, LUN
         CALL MSGWRT (7)
         IERR = 1
         END IF
      IF (IERR.NE.0) CALL ZERROR ('ZLPCL2', IOSVAL, ' ', -999, F)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZLPCLS: FORTRAN ERROR ',I6,' ON INQUIRE FOR LUN = ',I6)
 1010 FORMAT ('ZLPCLS: FORTRAN ERROR ',I6,' ON CLOSE FOR LUN = ',I6)
      END
