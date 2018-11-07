      SUBROUTINE ZMSGCL (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! close Message file or terminal
C# Z IO-basic Messages
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
C   Close a message file associated with LUN removing any exclusive use
C   state and clear the FTAB entry for the LUN (much like ZCLOSE but
C   does not call MSGWRT to avoid recursion).
C   Inputs:
C      LUN     I   Logical unit number
C      FIND    I   Index in FTAB to file control block for LUN
C   Output:
C      IERR    I   Error return code: 0 => no error
C                     1 => close error
C                     2 => file already closed in FTAB
C                     3 => both errors
C                     4 => erroneous LUN
C   Generic version - uses ZDACLS and ZTTCLS.
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      INTEGER   ERRLUN, ERRTER, JERR, IMAP, I, J
      LOGICAL   F
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
      DATA F /.FALSE./
      DATA ERRTER, ERRLUN /6, 12/
C-----------------------------------------------------------------------
C                                       Check inputs.
      IERR = 4
      IF ((LUN.EQ.ERRTER) .OR. (LUN.EQ.ERRLUN)) GO TO 10
         WRITE (ERRTER,1000) LUN
         GO TO 999
C                                       Locate file in FTAB.
 10   CALL LSERCH ('SRCH', LUN, FIND, F, JERR)
      IF (JERR.EQ.0) GO TO 20
         IERR = 2
         WRITE (ERRTER,1010) LUN
         GO TO 999
C                                       Okay, so far.
 20   IERR = 0
      IMAP = 0
C                                       Close disk message file.
      IF (LUN.EQ.ERRLUN) THEN
         CALL ZDACLS (FTAB(FIND+1), IMAP, IERR)
         IF (IERR.NE.0) WRITE (ERRTER,1020) IERR
C                                       Close terminal message file.
      ELSE
         CALL ZTTCLS (LUN, FIND, IERR)
         IF (IERR.NE.0) THEN
            IERR = 1
            IF (FTAB(FIND+1+NFCBER).GT.0) THEN
               CALL ZMSGWR ('ZTTCLS', FTAB(FIND+1+NFCBER), ' ', -999,
     *            .FALSE.)
            ELSE
               WRITE (ERRTER,1030) IERR, LUN
               END IF
            END IF
         END IF
C                                       Clear FTAB entry.
 900  CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
      IF (JERR.NE.0) THEN
         IERR = IERR + 2
         WRITE (ERRTER,1900) LUN
         END IF
      CALL FSERCH (FIND, I, J, JERR)
      IF (JERR.EQ.0) THEN
         IF (I.EQ.1) DEVNAM(J) = ' '
         IF (I.EQ.2) NONNAM(J) = ' '
         IF (I.EQ.3) MAPNAM(J) = ' '
         END IF
C
 999   RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (' ZMSGCL: INVALID LUN = ',I2)
 1010 FORMAT (' ZMSGCL: LUN = ',I2,' ALREADY CLOSED IN FTAB ON SEARCH')
 1020 FORMAT (' ZMSGCL: ZDACLS RETURNS ERROR = ',I1)
 1030 FORMAT (' ZMSGCL: FORTRAN ERROR ',I4,' ON CLOSE OF LUN ',I2)
 1900 FORMAT (' ZMSGCL: LUN = ',I2,' ALREADY CLOSED IN FTAB ON CLOSE')
      END
