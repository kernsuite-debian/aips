      SUBROUTINE YCREAD (IPLANE, IX, IY, CATBLK ,IERR)
C-----------------------------------------------------------------------
C! read the image catalog, return image header for TV only
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
C   Read image catalog block into CATBLK - TV only (TK in TKCATL)
C   Inputs:
C      IPLANE  I       plane containing image whose block is wanted
C      IX      I       X pixel coordinate of a point within image
C      IY      I       Y pixel coordinate of point within image
C   Outputs:
C      CATBLK  I(256)  Image catalog block
C      IERR    I       error codes: 0 => ok
C                         1 => IX, IY lies outside image
C                         2 => Catalog i/o errors
C                         3 => refers to TK device
C   VERSION for TV by COMMUNICATION
C-----------------------------------------------------------------------
      INTEGER   IPLANE, IX, IY, CATBLK(256), IERR
C
      INTEGER   CATI(256)
      HOLLERITH CATH(256)
      REAL      CATR(256)
      DOUBLE PRECISION CATD(128)
      INCLUDE 'INCS:DVTV.INC'
      EQUIVALENCE (CATI, CATH, CATR, CATD, BUFFER(1025))
C-----------------------------------------------------------------------
C                                       send request
      BUFSR = 302
      BUFSW = 3
      BUFFER(1) = IPLANE
      BUFFER(2) = IX
      BUFFER(3) = IY
C                                       do it
      CALL MKYBUF ('READ', 'YCREAD', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       unpack header
      IF (IERR.EQ.0) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         CALL HDRBUF (2, CATI, CATH, CATR, CATD, BUFFER(4))
         CALL COPY (256, CATI, CATBLK)
         END IF
C
 999  RETURN
      END
