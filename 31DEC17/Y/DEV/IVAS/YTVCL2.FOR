      SUBROUTINE YTVCL2 (LUN, IND, IERR)
C-----------------------------------------------------------------------
C! close actual TV device (called by YTVCLS)
C# TV-IO
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
C   YTVCL2 closes TV DEVICE associated with LUN removing any EXCLusive
C   use state and clears up the FTAB.
C   Inputs:
C      LUN    I   logical unit number
C      IND    I   pointer into FTAB
C   Output:
C      IERR   I   error code: 0 -> no error
C                    1 -> file already closed in FTAB
C                    >10 -> FIVASCLOSE error plus LSERCH error
C   IIS IVAS version - calls IVAS library (NO Z needed).
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, IERR
C
      INTEGER   IER, JERR, FIVASCLOSE
      LOGICAL   F
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA F /.FALSE./
C-----------------------------------------------------------------------
C                                       Remove from FTAB
      CALL LSERCH ('CLOS', LUN, IND, F, IER)
      IERR = IER
C                                       Set to no restrict
      CALL YVRTR (F, IER)
      IERR = IERR + IER
C                                       Close IVAS
      JERR = FIVASCLOSE (YBUFF(11))
      IF (JERR.NE.0) IERR = IERR + 10
      IF (JERR.EQ.0) GO TO 999
         WRITE (MSGTXT,1000) JERR
         CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IVASCLOSE ERROR',I10)
      END
