      SUBROUTINE YCINIT (IPLANE, BUFF)
C-----------------------------------------------------------------------
C! initialize image catalog for specified TV memory plane
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
C   Initialize image catalog for plane IPLANE - TK now done with TKCATL
C   Input:
C      IPLANE   I        Image plane to initialize
C   Output:
C      BUFF     I(256)   Working buffer
C   VIRTUAL TV version for TV by communication.
C-----------------------------------------------------------------------
      INTEGER   IPLANE, BUFF(256)
C
      INTEGER   IERR
      INCLUDE 'INCS:DVTV.INC'
C-----------------------------------------------------------------------
      BUFSW = 1
      BUFSR = 0
      BUFFER(1) = IPLANE
      CALL MKYBUF ('WRIT', 'YCINIT', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C
 999  RETURN
      END
