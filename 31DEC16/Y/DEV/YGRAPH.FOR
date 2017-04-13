      SUBROUTINE YGRAPH (OP, CHAN, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! turns TV graphics planes on and off
C# Y1 TV-basic
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
C   YGRAPH is used to turn graphics overlay planes on & off by
C   altering the graphics color look up table.  The color pattern is:
C      CHAN = 1    insert   yellow            drawing plots
C             2    insert   green+.05 red     axis labels
C             3    insert   blue + 0.6 green  blotch
C                           + red
C             4    insert   black             label backgrounds
C             5-7  add      nothing           null channels
C             8    insert   purple            cursor
C   Inputs:
C      OP      C*4       'ONNN' or 'OFFF'
C      CHAN    I         channel number (1 - 8)
C   Output:
C      SCRTCH  I(256)    scratch buffer
C      IERR    I         error code of Z...XF: 0 => ok
C                                              2 => input error
C   NO TV VERSION
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, SCRTCH(256), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'YGRAPH: THERE IS NO TV CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END
