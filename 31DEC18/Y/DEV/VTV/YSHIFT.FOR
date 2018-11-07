      SUBROUTINE YSHIFT (OP, SHIFTR, SHIFTG, SHIFTB, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! read/write the shift (bias) registers of the TV (IIS M70, not used)
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
C   YSHIFT reads/writes the TV shift registers - which shift the
C   13-bit output of the adders before entry into the 10-bit OFM
C   Inputs:
C      OP      C*4   'READ' from TV or 'WRIT' to TV
C      VRTRTC  L     T => do on vertical retrace only
C   In/Out:
C      SHIFTR  I     # bits to shift (right) red channel
C      SHIFTG  I     # bits to shift green channel
C      SHIFTB  I     # bits to shift blue channel
C   Output:
C      IERR    I     error code of Z...XF : 0 - ok, 2 - input error
C   TV by communication version.
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   SHIFTR, SHIFTG, SHIFTB, IERR
      LOGICAL   VRTRTC
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      BUFFER(1) = 0
      IF (VRTRTC) BUFFER(1) = 1
      IF (OP.EQ.'WRIT') THEN
         BUFFER(2) = SHIFTR
         BUFFER(3) = SHIFTG
         BUFFER(4) = SHIFTB
         BUFSW = 4
         BUFSR = 0
      ELSE
         BUFSW = 1
         BUFSR = 4
         END IF
      CALL MKYBUF (OP, 'YSHIFT', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         CALL UNYBUF (BUFSR, HEADER, BUFFER)
         SHIFTR = BUFFER(2)
         SHIFTG = BUFFER(3)
         SHIFTB = BUFFER(4)
         END IF
C
 999  RETURN
      END
