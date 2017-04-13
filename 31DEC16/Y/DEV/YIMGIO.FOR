      SUBROUTINE YIMGIO (OP, CHAN, X, Y, IANGL, NPIX, IMAGE, IERR)
C-----------------------------------------------------------------------
C! read/write data to the TV grey and graphics memories
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
C   YIMGIO reads/writes a line of image data to the TV screen.  For
C   graphics overlay planes, the data are solely 0's and 1's in the
C   least significant bit of IMAGE after a READ.  For 'WRIT', all bits
C   of each word should be equal (i.e. all 1's or all 0's for graphics).
C   NOTE***** on 'WRIT', the buffer may be altered by this routine for
C   some IANGLs.
C   Inputs:
C      OP      C*4       'READ' from TV or 'WRIT' to TV
C      CHAN    I         channel number (1 to NGRAY+NGRAPH)
C      X       I         start pixel position
C      Y       I         end pixel position
C      IANGL   I         = 0 => horizontal (to right)
C                        = 1 => vertical (up the screen)
C                        = 2 => horizontal (to left)
C                        = 3 => vertical (down the screen)
C      NPIX    I         number of pixels
C   In/Out:
C      IMAGE   I(NPIX)   data (only no header)
C   Output:
C      IERR    I         error code of Z...XF - 0 => ok
C                                                  2 => input err
C   NO TV VERSION - stub
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, X, Y, IANGL, NPIX, IMAGE(*), IERR
C
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 2
      MSGTXT = 'YIMGIO: THERE IS NO TV CONFIGURED'
      CALL MSGWRT (8)
C
 999  RETURN
      END
