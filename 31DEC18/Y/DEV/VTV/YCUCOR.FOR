      SUBROUTINE YCUCOR (RPOS, QUAD, CORN, IERR)
C-----------------------------------------------------------------------
C! correct cursor position for scroll; return image coordinates, header
C# Y0 TV-appl
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
C   YCUCOR takes a cursor position (corrected for zoom, but not scroll)
C   corrects it for scroll, determines the quadrant of the TV, and gets
C   the corresponding image header in common /MAPHDR/ and returns the
C   image coordinates.
C   NOTE WELL: RPOS ON INPUT MUST BE CORRECTED FOR ZOOM AND NOT SCROLL.
C   To get this from a raw TV position call YCURSE with OPCODE 'FXIT'
C   (or, of course, 'READ') and quadrant set to -1.    *************
C   Inputs:
C      RPOS    R(2)    X,Y screen pos before  zoom & scroll
C   Output:
C      QUAD    I       TV quadrant to use for scrolls
C                      Out: if in=-1, no scroll, else find quadrant
C                           (needs real TV pos)
C      CORN    R(7)    Image coordinates (pixels)
C      IERR    I       error code of Z...XF : 0 - ok
C                                             2 - input error
C   VIRTUAL TV version for TV by communication.
C-----------------------------------------------------------------------
      REAL      RPOS(2), CORN(7)
      INTEGER   IERR, QUAD
C
      INTEGER   I, J
      INCLUDE 'INCS:DVTV.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      BUFFER(1) = RPOS(1)
      BUFFER(2) = 1000.0 * (RPOS(1) - BUFFER(1))
      BUFFER(3) = RPOS(2)
      BUFFER(4) = 1000.0 * (RPOS(2) - BUFFER(3))
      BUFSW = 4
      BUFSR = 318
      CALL MKYBUF ('READ', 'YCUCOR', BUFSW, BUFSR, BUFFER, HEADER)
      CALL ZVTVXF (BUFSW, BUFSR, HEADER, IERR)
C                                       return outputs
      IF (IERR.EQ.0) THEN
         CALL UNYBUF (BUFSR, BUFFER, HEADER)
         QUAD = BUFFER(5)
         DO 10 I = 1,7
            J = 2 * I + 4
            CORN(I) = BUFFER(J) + BUFFER(J+1) / 1000.0
 10         CONTINUE
         CALL HDRBUF (2, CATBLK, CATH, CATR, CATD, BUFFER(20))
         END IF
C
 999  RETURN
      END
