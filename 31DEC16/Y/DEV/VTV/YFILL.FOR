      SUBROUTINE YFILL (CHAN, IX0, IY0, IXT, IYT, IVAL, IBLK, IERR)
C-----------------------------------------------------------------------
C! fill rectangle of TV memory with a constant value
C# Y0 TV-util
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
C   YFILL will write a constant in a given rectangle in a given graphics
C   or image plane.  It will use fast methods if full screen requested
C   with IVAL 0.
C   Inputs:
C      CHAN     I   Channel (1 to NGRAY+NGRAPH)
C      IX0      I   lower left X pixel (1 relative) of rectangle
C      IY0      I   lower left Y pixel of rectangle.
C      IXT      I   top right X pixel of rectangle.
C      IYT      I   top right Y pixel of rectangle.
C      IVAL     I   desired value: for graphics = 0 or 1
C                                  for grey scale = 0 - MAXINT
C   In/out:
C      IBLK     I(IXT-IX0+1)   work buffer.
C   Output:
C      IERR     I   error code of Z...XF: 0 ok, 2 input error
C   Virtual TV (by communication) version.
C-----------------------------------------------------------------------
      INTEGER   CHAN, IX0, IY0, IXT, IYT, IVAL, IBLK(*), IERR
C
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       partial planes
      IF ((IX0.GT.1) .OR. (IY0.GT.1) .OR. (IXT.LT.MAXXTV(1)) .OR.
     *   (IYT.LT.MAXXTV(2)) .OR. (IVAL.NE.0)) THEN
         IERR = 2
         IF ((IXT.LT.IX0) .OR. (IYT.LT.IY0)) GO TO 999
         IF ((IX0.LT.1) .OR. (IY0.LT.1)) GO TO 999
         IF ((IXT.GT.MAXXTV(1)) .OR. (IYT.GT.MAXXTV(2))) GO TO 999
         BUFFER(1) = CHAN
         BUFFER(2) = IX0
         BUFFER(3) = IY0
         BUFFER(4) = IXT
         BUFFER(5) = IYT
         BUFFER(6) = IVAL
         BUFSR = 0
         BUFSW = 6
         CALL MKYBUF ('WRIT', 'YFILL', BUFSW, BUFSR, BUFFER, HEADER)
         CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       full planes
      ELSE
         CALL YZERO (CHAN, IERR)
         END IF
C
 999  RETURN
      END
