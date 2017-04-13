      SUBROUTINE YSTCUR (OP, X, Y, NPOINT, PACK, VRTRTC, BUFFER, IERR)
C-----------------------------------------------------------------------
C! reads/writes the cursor pattern array
C# Y3 TV-basic
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
C   YSTCUR reads/writes the TV cursor array which has the pattern
C   exhibited when the cursor is visible.  NOTE: if more than one
C   row is read/written at a time, then the Y value decreases!!!
C   Inputs:
C      OP      C*4    'READ' from TV or 'WRIT' to TV
C      X       I      initial X position (1-64, 1 => LHS)
C      Y       I      initial Y position (1-64, 1 => bot)
C      NPOINT  I      # pixel values in BUFFER
C      PACK    L      T => 2 values/word, F => 1 value/word
C      VRTRTC  L      T => do it on vertical retrace only
C   In/Out:
C      BUFFER  I(*)   data array (lsb's used only)
C   Output:
C      IERR    I      error code of Z...XF : 0 => ok, 2 => input error
C   No TV version - stub: level 3 called by Y only.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      LOGICAL   PACK, VRTRTC
      INTEGER   X, Y, NPOINT, BUFFER(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'THERE IS NO TV CURSOR CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END
