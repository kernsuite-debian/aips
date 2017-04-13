      SUBROUTINE YRHIST (MODE, COLOR, INITI, NINT, HISTOG, IERR)
C-----------------------------------------------------------------------
C! read the histogram of the selected TV output color
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
C   YRHIST reads the histogram of the output of a selected OFM of the
C   TV.   Intensity values 0 through OFMOUT may be read.
C   Inputs:
C      MODE    I         selects area to histogram: 0 blotch,
C                        1 not blotch, 2 all, 3 external blotch
C      COLOR   I         bit map of single color (RGB - 4,2,1)
C      INITI   I         first intensity to histo (1 - 1024)
C      NINT    I         # values to get
C   Output:
C      HISTOG  I(NINT)   histogram
C      IERR    I         error code of Z...XF : 0 => ok, 2 => input err
C   No TV (histogram generator) version - stub: level 2 for IIS so far.
C-----------------------------------------------------------------------
      INTEGER   MODE, COLOR, INITI, NINT, HISTOG(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'THERE IS NO TV HISTOGRAM GENERATOR CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END
