      SUBROUTINE ZTTOPN (LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! open a terminal device
C# Z2 Terminal
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
C   Open a terminal - does not use MSGWRT because it is called by
C   ZMSGOP.  Does do the FTAB open here (not to be done in ZOPEN).
C   Inputs:
C      LUN    I   Logical unit number (5 or 6)
C   Output:
C      FIND   I   Position in FTAB
C      IERR   I   Error return code: 0 => no error
C                    1 => Fortran open error
C                    2 => input error
C   VMS version - requires that FOR005 and FOR006 be assigned elsewhere
C-----------------------------------------------------------------------
      INTEGER   LUN, FIND, IERR
C
      LOGICAL   F
      INTEGER   JERR
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
C                                       Check inputs.
      FIND = 0
      IF ((LUN.EQ.5) .OR. (LUN.EQ.6)) GO TO 10
         IERR = 2
         GO TO 999
C                                       Don't reopen if already open
 10   CALL LSERCH ('SRCH', LUN, FIND, F, IERR)
      IF (IERR.EQ.0) GO TO 999
      IERR = 0
C                                       Open in FTAB
      CALL LSERCH ('OPEN', LUN, FIND, F, JERR)
      IF (JERR.NE.0) THEN
         FIND = 0
         IERR = 5
         GO TO 999
         END IF
C                                       Do simple OPEN
C                                       assume FOR005,FOR006 assigned
      OPEN (UNIT=LUN, STATUS='UNKNOWN', IOSTAT=JERR)
      IF (JERR.NE.0) THEN
         FTAB(FIND+1+NFCBER) = JERR
         IERR = 1
         CALL LSERCH ('CLOS', LUN, FIND, F, JERR)
      ELSE
         FTAB(FIND+1+NFCBER) = 0
         END IF
C
 999  RETURN
      END
