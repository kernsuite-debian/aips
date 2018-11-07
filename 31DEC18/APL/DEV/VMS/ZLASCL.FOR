      SUBROUTINE ZLASCL (FILNAM, LUN, DELFIL, SYSERR, IERR)
C-----------------------------------------------------------------------
C! close and spool a laser printer print/plot file
C# Z2 Printer Plot-util
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
C   close and spool a file for printing a plot on a laser printer
C   Inputs:
C      FILNAM   C*48   print/plot file name
C      LUN      I      LUN under which file is open
C      DELFIL   I      1 => delete the file after print, 0 => keep it
C                      -1 => close file only
C   Output
C      SYSERR   I      system error code (on IERR 2)
C      IERR     I      error code: 0 => okay, 1 => could not queue
C                         2 => Fortran close error
C   VMS version - queues file in the Fortran close.
C-----------------------------------------------------------------------
      CHARACTER FILNAM*48
      INTEGER   LUN, DELFIL, SYSERR, IERR
C
      INTEGER   ITRIM, FLEN, LELFIL, XLNB, SYS$CRELNM, LIST4(4)
      CHARACTER DEVICQ*64
      INTEGER*2 LIST2(8)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE '($LNMDEF)'
      EQUIVALENCE (LIST2, LIST4)
C-----------------------------------------------------------------------
      SYSERR = 0
      IERR = 0
      LELFIL = DELFIL
C                                       assign the printer: find queue
C                                       Find out Queue name for plotter.
      CALL ZTRLOG (6, 'LASQUE', 64, DEVICQ, XLNB, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         IERR = 1
C                                       Assign SYS$PRINT to plotter
C                                       device queue.
      ELSE
         LIST2(1) = XLNB
         LIST2(2) = LNM$_STRING
         LIST4(2) = %LOC (DEVICQ)
         LIST4(3) = 0
         LIST4(4) = 0
         SYSERR = SYS$CRELNM ( , 'LNM$PROCESS_TABLE', 'SYS$PRINT', ,
     *      LIST4)
         IF (SYSERR.NE.1) THEN
            WRITE (MSGTXT,1010) SYSERR
            CALL MSGWRT (6)
            IERR = 1
            END IF
         END IF
      IF (IERR.NE.0) THEN
         LELFIL = 0
      ELSE
         SYSERR = 0
         END IF
C                                       Fortran close to printer
      IF (LELFIL .GT. 0)
     *   CLOSE (UNIT=LUN, DISP='PRINT/DELETE', IOSTAT=SYSERR)
      IF (LELFIL .EQ. 0)
     *   CLOSE (UNIT=LUN, DISP='PRINT', IOSTAT=SYSERR)
      IF (LELFIL .LT. 0)
     *   CLOSE (UNIT=LUN, DISP='SAVE', IOSTAT=SYSERR)
      IF (SYSERR.NE.0) IERR = 2
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ZLASCL: NO GROUP LOGICAL NAME ''LASQUE''. IERR=',I8)
 1010 FORMAT ('ZLASCL: COULD NOT ASSIGN LOGICAL NAME, IERR=',I8)
      END
