      SUBROUTINE SET1DG (ISTART, IEND, GPOS, GWIDTH, GMAX, IERR)
C-----------------------------------------------------------------------
C! sets initial guess parameters with the TEK for fitting slices
C# Graphics Slice
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
C   SET1DG will allow the user to determine initial guess parameters
C   for gaussian fitting of slices by pointing to the center, height
C   and halfwidth edge with the Tektronix 4012 cursor.
C   Inputs:
C      ISTART  I   first gaussian component to ask for.
C      IEND    I   last gaussian component to ask for.
C   Outputs:
C      GPOS    R(4)   positions of centers of gaussian componts.
C      GWIDTH  R(4)   width of gaussian components at half the heights.
C      GMAX    R(4)   heights of gaussian components.
C      IERR    I   0 = ok, 1=image cat error. 2=tek error.
C   Common:
C      /MAPHDR/ CATBLK  image header (input)
C-----------------------------------------------------------------------
      INTEGER   ISTART, IEND, IERR
      REAL      GMAX(4), GPOS(2,4), GWIDTH(3,4)
C
      REAL      ARC, VAL
      INTEGER   ICOMP
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Loop for all components.
      DO 10 ICOMP = ISTART,IEND
C                                       Find arc sec of curs from cent.
         WRITE (MSGTXT,1000) ICOMP
         CALL MSGWRT (1)
         CALL TKSLAC (CATBLK, VAL, ARC, IERR)
         IF (IERR.NE.0) GO TO 999
         GPOS(1,ICOMP) = ARC
         GMAX(ICOMP) = VAL
C                                       Get halfwidth.
         WRITE (MSGTXT,1002) ICOMP
         CALL MSGWRT (1)
         CALL TKSLAC (CATBLK, VAL, ARC, IERR)
         IF (IERR.NE.0) GO TO 999
         GWIDTH(1,ICOMP) = 2.0 * ABS (GPOS(1,ICOMP) - ARC)
 10      CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Position cursor at center & height of Gaussian comp',I3)
 1002 FORMAT ('Position cursor at halfwidth of Gaussian component',I4)
      END
