      SUBROUTINE YGGRAM (OP, VRTRTC, RGBBUF, IERR)
C-----------------------------------------------------------------------
C! controls the TV graphics color assignments
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
C   YGGRAM reads/writes the TV graphics color assignment RAM.  The
C   data are packed in this look up table for colors as:
C         bit 15 (msb)   :   1 - graphics replace 0 - add to image
C         bits 10-14     :   red value for this graphics value
C         bits  5- 9     :   green
C         bits  0- 4     :   blue
C   Inputs:
C      OP      C*4       'READ' from TV or 'WRIT' to TV
C      VRTRTC  L         T => do it only on vertical retrace
C   In/Out:
C      RGBBUF  I(256)    data array
C   Out:
C      IERR    I         error code of Z...XF : 0 => ok
C   NO TV VERSION - this is a level 3 IIS M70 function.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   RGBBUF(256), IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'THERE IS NO TV CONFIGURED WITH A GRAPHICS COLOR RAM'
      CALL MSGWRT (8)
C
 999  RETURN
      END
