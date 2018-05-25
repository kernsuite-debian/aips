      SUBROUTINE YDOERR (ASUBR, JERR, IERR)
C-----------------------------------------------------------------------
C! responds to errors encountered in IVAS routines
C# TV-util
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
C   YDOERR responds to error conditions on the TV I/O.
C   Inputs:
C      ASUBR   C*6   Subroutine encountering error
C      JERR    I     Error from IVAS routines
C   Output:
C      IERR    I     Error code 0 => okay
C   Version for IIS IVAS: does master clear, fetches status info, and
C   prints
C-----------------------------------------------------------------------
      CHARACTER ASUBR*(*)
      INTEGER   JERR, IERR
C
      INTEGER   DESC(20), IDESC, ADESC, ITEXT, ATEXT, JJ, IER
      INTEGER   FIVASSTSGET
      CHARACTER*80 ETEXT
      INCLUDE 'INCS:DMSG.INC'
      DATA ITEXT, IDESC /80, 20/
C-----------------------------------------------------------------------
C                                       No error
      IERR = 0
      IF (JERR.EQ.0) GO TO 999
C                                       Print: there was error
      JJ = JERR / 32768
      IERR = JERR - JJ * 32768
      WRITE (MSGTXT,1000) IERR, ASUBR
      CALL MSGWRT (6)
C                                       Clear hold status
      CALL YTVMC
C                                       get status info
C
C
      IER = FIVASSTSGET (DESC, IDESC, ADESC, ETEXT, ITEXT, ATEXT)
      IF (IER.EQ.0) GO TO 20
         WRITE (MSGTXT,1010) IER
         CALL MSGWRT (6)
         CALL YTVMC
C                                       Tell all
 20   WRITE (MSGTXT,1020) ETEXT(1:ATEXT)
      CALL MSGWRT (6)
      WRITE (MSGTXT,1021) ADESC, DESC(1)
      CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IVAS ERROR',I7,' ENCOUNTERED IN ',A)
 1010 FORMAT ('ERROR',I10,' ENCOUNTERED READING IVAS STATUS')
 1020 FORMAT (A)
 1021 FORMAT ('IVAS DESC. LENGTH=',I10,' (1)=',I10)
      END
