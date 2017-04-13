      SUBROUTINE YCOVER (OVER, BUF, IERR)
C-----------------------------------------------------------------------
C! checks for overlapped images on the TV by quadrant
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
C   YCOVER checks to see if there are partially replaced images in any
C   of the TV planes currently visible by quadrant
C   Outputs:
C      OVER   L(4)     T => there are in quadr. I
C      BUF    I(512)   scratch
C      IERR   I        Error code: 0 => ok, other catlg IO error
C   Virtual TV version for TV by communication.
C-----------------------------------------------------------------------
      LOGICAL   OVER(4)
      INTEGER   BUF(512), IERR
C
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      BUFFER(1) = 0
      BUFFER(2) = 0
      BUFFER(3) = 0
      BUFFER(4) = 0
      BUFSW = 0
      BUFSR = 4
      CALL MKYBUF ('READ', 'YCOVER', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       get values
      IF (IERR.EQ.0) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         OVER(1) = BUFFER(1).GT.0
         OVER(2) = BUFFER(2).GT.0
         OVER(3) = BUFFER(3).GT.0
         OVER(4) = BUFFER(4).GT.0
         END IF
C
 999  RETURN
      END
