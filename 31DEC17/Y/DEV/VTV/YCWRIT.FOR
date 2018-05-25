      SUBROUTINE YCWRIT (IPLANE, IMAWIN, CATBLK, BUFF, IERR)
C-----------------------------------------------------------------------
C! write image header to image catalog, update image catalog directory
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
C   Write image catalog block in CATBLK into image catalog
C   Inputs:
C      IPLANE   I        image plane involved
C      IMAWIN   I(4)     Corners of image on screen
C      CATBLK   I(256)   Image catalog block
C  Outputs:
C      BUFF     I(256)   working buffer
C      IERR     I        error code: 0 => ok
C                           1 => no room in catalog
C                           2 => IO problems
C   Virtual TV version for TV by communication.
C-----------------------------------------------------------------------
      INTEGER   IPLANE, IMAWIN(4), CATBLK(256), BUFF(256), IERR
C
      REAL      CATR(256)
      INTEGER   CATI(256)
      REAL      CATD(128)
      HOLLERITH CATH(256)
      INCLUDE 'INCS:DVTV.INC'
      EQUIVALENCE (CATI, CATH, CATR, CATD, BUFFER(1025))
C-----------------------------------------------------------------------
      BUFFER(1) = IPLANE
      CALL COPY (4, IMAWIN, BUFFER(2))
      CALL COPY (256, CATBLK, CATI)
      CALL HDRBUF (1, CATI, CATH, CATR, CATD, BUFFER(6))
      BUFSW = 304
      BUFSR = 0
      CALL MKYBUF ('WRIT', 'YCWRIT', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C
 999  RETURN
      END
