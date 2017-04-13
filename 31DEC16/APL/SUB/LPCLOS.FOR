      SUBROUTINE LPCLOS (LUN, FIND, LINES, IERR)
C-----------------------------------------------------------------------
C! closes open line printer operation (actual printer, file, terminal)
C# IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007, 2016
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
C   Close the file associated with LUN removing any exclusive use state
C   and clear the FTAB entry for the LUN.
C   Inputs:
C      LUN      I        Logical unit number
C      FIND     I        Index in FTAB to file control block for LUN
C      LINES    I        Number of lines printed in current page
C                        < 0 -> do NOT print on line printer
C   Output:
C      IERR     I        Error return code: 0 => no error
C                           1 => close error
C                           2 => file already closed in FTAB
C                           3 => both errors
C                           4 => erroneous LUN
C   92.01.15 PPM: MAP never initialized.  Screws up if it happened to be
C                 .true. (on HP's).
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, LINES, IERR
C
      INTEGER   JERR, I, J
      LOGICAL   MAP
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
C                                       Make sure this is false!!!
      MAP = .FALSE.
C                                       Line printer?
      IF (LUN.EQ.1) THEN
         IF (LINES.GE.0) THEN
            CALL ZENDPG (LINES)
            CALL ZLPCLS (LUN, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) 'ZLPCLS', IERR
               CALL MSGWRT (7)
               IERR = 1
               END IF
         ELSE
            IERR = 0
            END IF
C                                       Terminal
      ELSE IF (LUN.EQ.5) THEN
         CALL ZTTCLS (LUN, FIND, IERR)
         IF (IERR.NE.0) THEN
            IF (FTAB(FIND+1+NFCBER).GT.0) THEN
               CALL ZERROR ('ZTTCLS', FTAB(FIND+1+NFCBER), ' ', -999,
     *            .FALSE.)
            ELSE
               WRITE (MSGTXT,1000) 'ZTTCLS', IERR
               CALL MSGWRT (7)
               END IF
            IERR = 1
            END IF
C                                       Text file
      ELSE IF (LUN.EQ.3) THEN
         CALL ZTXCLS (LUN, FIND, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1000) 'ZTXCLS', IERR
            CALL MSGWRT (7)
            END IF
         GO TO 999
C                                       Unknown Fortran device LUN.
      ELSE
         WRITE (MSGTXT,1010) LUN
         IERR = 4
         GO TO 995
         END IF
C                                       Clear FTAB entry for LUN 1, 5.
      CALL LSERCH ('CLOS', LUN, FIND, MAP, JERR)
      IF (JERR.NE.0) THEN
         IERR = IERR + 2
         WRITE (MSGTXT,1100) LUN
         GO TO 995
         END IF
      CALL FSERCH (FIND, I, J, JERR)
      IF (JERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = ' '
         IF (I.EQ.2) NONNAM(J) = ' '
         IF (I.EQ.3) MAPNAM(J) = ' '
         END IF
      GO TO 999
C
 995  CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('LPCLOS: ',A,' RETURNS ERROR = ',I4)
 1010 FORMAT ('LPCLOS: UNKNOWN FORTRAN DEVICE.  LUN = ',I2)
 1100 FORMAT ('LPCLOS: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END
