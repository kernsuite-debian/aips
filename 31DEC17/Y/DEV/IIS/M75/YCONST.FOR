      SUBROUTINE YCONST (OP, RCONST, GCONST, BCONST, VRTRTC, IERR)
C-----------------------------------------------------------------------
C! controls the constant registers added to the TV picture - not used
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
C   YCONST reads/writes the constants which are added to the 3 sum
C   channels on the TV.
C   Inputs:
C      OP      C*4   'READ' from TV, 'WRIT' to TV
C   In/out:
C      RCONST  I     constant for red channel
C      GCONST  I     constant for green channel
C      BCONST  I     constant for blue channel
C      VRTRTC  L     Vertical retrace - do only on ?
C   Output:
C      IERR    I     error code of Z...XF : 0 => ok
C   Version for IIS M75
C-----------------------------------------------------------------------
      CHARACTER OP*4
      INTEGER   RCONST, GCONST, BCONST, IERR
      LOGICAL   VRTRTC
C
      INTEGER   UNIT, HEADER(8), BUFFER(3), BUFSIZ, NB, ZRBG
      LOGICAL   COMAND
      DATA UNIT, COMAND, BUFSIZ /8,.TRUE.,3/
C                                       Color select (M75)
      DATA ZRBG /7/
C-----------------------------------------------------------------------
C                                       WRIT: fill buffer
      IF (OP.EQ.'WRIT') THEN
         BUFFER(1) = BCONST
         BUFFER(2) = GCONST
         BUFFER(3) = RCONST
         END IF
C                                       Header & IO
      CALL YMKHDR (OP, UNIT, COMAND, BUFSIZ, 8, 0, ZRBG, 0, VRTRTC,
     *   HEADER)
      NB = 2 * BUFSIZ
      CALL ZM70XF (OP, NB, HEADER, BUFFER, IERR)
C                                       Return answers
      IF ((IERR.EQ.0) .AND. (OP.NE.'WRIT')) THEN
         BCONST = BUFFER(1)
         GCONST = BUFFER(2)
         RCONST = BUFFER(3)
         END IF
C
 999  RETURN
      END
