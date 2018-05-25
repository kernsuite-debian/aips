      SUBROUTINE IMVECT (OP, CHAN, COUNT, XDATA, YDATA, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! draws connected line segments on TV
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   IMVECT writes a connected sequence of line segments on a TV channel
C   calling YCNECT
C   Inputs:
C      OP      C*4     'ONNN' line of ones (max intensity)
C                      'OFFF' line of zeros (min intensity)
C      CHAN    I       channel number (1 to NGRAY+NGRAPH)
C      COUNT   I       number of X,Y pairs ( > 1)
C      XDATA   I(COUNT)      X coordinates X1,X2,...
C      YDATA   I(COUNT)      Y coordinates Y1,Y2,...
C   Output:
C      SCRTCH  I(*)    scratch buffer (size MAXXTV)
C      IERR    I       error code of ZM70XF - 0 => ok;  2 => input error
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   CHAN, COUNT, XDATA(*), YDATA(*), SCRTCH(*), IERR
C
      INTEGER   INT
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       check inputs
      IERR = 2
      INT = -99
      IF ((OP.EQ.'ONNN') .AND. (CHAN.LE.NGRAY)) INT = MAXINT
      IF ((OP.EQ.'ONNN') .AND. (CHAN.GT.NGRAY)) INT = 2**NGRAPH - 1
      IF (OP.EQ.'OFFF') INT = 0
      IF (INT.NE.-99) CALL TVVECT (INT, CHAN, COUNT, XDATA, YDATA,
     *   SCRTCH, IERR)
C
 999  RETURN
      END
