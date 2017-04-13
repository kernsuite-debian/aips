      SUBROUTINE YTVCL2 (LUN, IND, IERR)
C-----------------------------------------------------------------------
C! close actual TV device (called by YTVCLS)
C# TV-IO
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   YTVCL2 closes TV DEVICE associated with LUN removing any EXCLusive
C   use state and clears up the FTAB.
C   Inputs:
C      LUN    I   logical unit number
C      IND    I   pointer into FTAB
C   Output:
C      IERR   I   error code: 0 -> no error
C                    1 -> Deaccess or Deassign error
C                    2 -> file already closed in FTAB
C                    3 -> both errors
C                    4 -> erroneous LUN
C   SSS Version - call ZSSSCL
C-----------------------------------------------------------------------
      INTEGER   LUN, IND, IERR
C
      INTEGER   OPCODE, DAT(4), BUF, NWSD
C-----------------------------------------------------------------------
C                                       tell XAS we are closing
      OPCODE = 12
      CALL FILL (4, 0, DAT)
      CALL ZSSSXF (OPCODE, DAT, 0, BUF, NWSD, IERR)
C                                       close device
      CALL ZSSSCL (LUN, IND, IERR)
C
 999  RETURN
      END
