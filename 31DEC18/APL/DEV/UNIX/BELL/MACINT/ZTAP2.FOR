      SUBROUTINE ZTAP2 (OP, COUNT, FCB, IERR)
C-----------------------------------------------------------------------
C! position (forward/back record/file), write EOF, etc. for tapes
C# Z2 Tape
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003, 2006
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
C   Performs standard tape manipulating functions.
C   Inputs:
C      OP      C*4    Operation to be performed:
C                     'REWI' = rewind the tape on unit LUN
C                     'ADVF' = advance file marks
C                     'BAKF' = backspace file marks.
C                     'ADVR' = advance records
C                     'BAKR' = backspace records.
C                     'WEOF' = write end of file on unit LUN
C                     'BEGW' = mark to begin write (bakf/weof..)
C                     'AEOI' = advance to end-of-information (eg 2 EOFs)
C   In/Out:
C      COUNT   I      In: number of files/records to skip
C                     Out (AEOI only): file # at EOI (> 0 absolute, < 0
C                        relative to start of skip)
C      FCB     I(*)   File control block for open tape device
C   Outputs:
C      IERR    I      Error return: 0 => ok
C                        3 = I/O error.
C                        4 = End Of File
C                        5 = Beginning Of Medium
C                        6 = End Of Medium
C   Mac version - stubbed until a tape device is available to test
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   COUNT, FCB(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'ZTAP2: STUBBED FOR LATER DEVELOPMENT'
      CALL MSGWRT (8)
C
 999  RETURN
      END
