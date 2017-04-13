      SUBROUTINE ZLASCL (FILNAM, LUN, DELFIL, SYSERR, IERR)
C-----------------------------------------------------------------------
C! close and spool a laser printer print/plot file
C# Z2 Printer Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   close and spool a file for printing a plot on a laser printer
C   Inputs:
C      FILNAM   C*48   print/plot file name
C      LUN      I      LUN under which file is open
C      DELFIL   I      1 => delete the file after print, 0 => keep it
C                      -1 => close print file only
C   Output
C      SYSERR   I      system error code (on IERR 2)
C      IERR     I      error code: 0 => okay, 1 => error in queue,
C                         2 => Fortran close error
C   Generic version - calls ZLASC2 for any special stuff after a Fortran
C   close.
C-----------------------------------------------------------------------
      CHARACTER FILNAM*(*)
      INTEGER   LUN, DELFIL,SYSERR,  IERR
C
      INTEGER   ITRIM, FLEN, ID
      HOLLERITH HNAM(32)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Fortran close
      CLOSE (UNIT=LUN, IOSTAT=SYSERR)
      IF (SYSERR.NE.0) THEN
         WRITE (MSGTXT,1000) SYSERR
         CALL MSGWRT (6)
         IERR = 2
         GO TO 999
         ENDIF

      IF (DELFIL.GE.0) THEN
         IF (DELFIL.GT.0) THEN
            ID = PRDELE
         ELSE
            ID = 0
            END IF
         FLEN = ITRIM (FILNAM)
         IF (SYSTYP.EQ.'VMS') THEN
            CALL ZLASC2 (FLEN, FILNAM, ID, IERR)
         ELSE
            CALL CHR2H (128, ' ', 1, HNAM)
            CALL CHR2H (FLEN, FILNAM, 1, HNAM)
            CALL ZLASC2 (FLEN, HNAM, ID, IERR)
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZLASCL: FORTRAN CLOSE ERROR =',I7)
      END
