      SUBROUTINE ZM70MC (FCB)
C-----------------------------------------------------------------------
C! issues a master clear to an IIS Model 70 TV
C# Z IO-TV
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
C   Perform a master clear on an IIS model 70 TV device and clear the
C   I/O buffer.
C   Inputs:
C      FCB      I(*)        File control block in FTAB for TV device
C   Generic version - calls ZM70M2.
C-----------------------------------------------------------------------
      INTEGER   FCB(*)
C
      INTEGER   NXB, MXB, XB(4200), IERR
      LOGICAL   MAP
      INCLUDE 'INCS:DZCH.INC'
      INCLUDE 'INCS:DDCH.INC'
      COMMON /XBM70/ NXB, MXB, XB
      DATA MAP /.FALSE./
C-----------------------------------------------------------------------
C                                       Clear and ignore the I/O buffer.
      MXB = 4200
      NXB = 0
      CALL FILL (MXB, 0, XB)
      MXB = MXB * (NBITWD / 16)
C                                       Issue the master clear.
      CALL ZM70M2 (FCB(1), IERR)
      IF (IERR.NE.0) CALL ZERROR ('ZM70M2', FCB(1+FCBERR), ' ',
     *   FCB(1), MAP)
C
 999  RETURN
      END
