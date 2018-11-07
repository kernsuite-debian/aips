      SUBROUTINE YMNMAX (RMIN, RMAX, GMIN, GMAX, BMIN, BMAX, VRTRTC,
     *   IERR)
C-----------------------------------------------------------------------
C! read 3 min/max values from TV data paths (IIS only, not used)
C# Y2 TV-basic
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
C   YMNMAX reads the min & max values put out by the 3 summers (before
C   application of constants, shifts and OFM) from the TV
C   Inputs:
C      VRTRTC  L    do it on vertical retrace only
C   Output:
C      RMIN    I    red minimum
C      RMAX    I    red maximum
C      GMIN    I    green minimum
C      GMAX    I    green maximum
C      BMIN    I    blue minimum
C      BMAX    I    blue maximum
C      IERR    I    error code of Z...XF : 0 => ok, 2 => input error
C   No TV (min/max register) version - stub.  Level 2 for IIS only.
C-----------------------------------------------------------------------
      INTEGER   RMIN, RMAX, GMIN, GMAX, BMIN, BMAX, IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'THERE IS NO TV MIN/MAX REGISTER CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END
