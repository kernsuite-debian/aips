      SUBROUTINE SUBHDR (BLC, TRC, XINC, YINC)
C-----------------------------------------------------------------------
C! changes input to output header correcting for subimaging
C# Map-util Header
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
C   SUBHDR corrects the header for subimaging: changes number of points
C   on the axes, the reference pixels, and the alternate axis (freq vs
C   velocity) reference pixel.  It corrects the first two axes for use
C   of pixel increments - namely the reference pixel and the axis incr.
C   Inputs:
C      BLC    R(7)        Bottom left corner to use
C      TRC    R(7)        Top right corner to use
C      XINC   R           Pixel increment on first axis
C      YINC   R           Pixel increment on second axis
C   Common:
C      /MAPHDR/ CATBLK     map header (in/out)
C-----------------------------------------------------------------------
      REAL      BLC(7), TRC(7), XINC, YINC
C
      CHARACTER FCHARS(3)*4, CHTEMP*4
      HOLLERITH CATH(256)
      REAL      CATR(256), AINC(7)
      INTEGER   IPL, IPH, CATBLK(256), NAX, I, J
      DOUBLE PRECISION    CATD(128)
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATH, CATR, CATD)
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Regular axis parameters
      NAX = CATBLK(KIDIM)
      CALL RFILL (7, 1.0, AINC)
      IF (XINC.GT.0.001) AINC(1) = XINC
      IF (YINC.GT.0.001) AINC(2) = YINC
      DO 10 I = 1,NAX
         IPL = BLC(I) + 0.01
         IPH = TRC(I) + 0.01
         CATBLK(KINAX+I-1) = (IPH - IPL) / AINC(I) + 1
         CATR(KRCRP+I-1) = (CATR(KRCRP+I-1) - IPL) / AINC(I) + 1.
         CATR(KRCIC+I-1) = CATR(KRCIC+I-1) * AINC(I)
 10      CONTINUE
C                                       Alternate axis
      IF (CATBLK(KIALT).EQ.0) GO TO 999
         DO 25 I = 1,NAX
            IPL = KHCTP + (I-1) * 2
            CALL H2CHR (4, 1, CATH(IPL), CHTEMP)
            DO 20 J = 1,3
               IF (FCHARS(J).EQ.CHTEMP) GO TO 30
 20            CONTINUE
 25         CONTINUE
         GO TO 999
C                                       Found one
 30      CONTINUE
            IPL = BLC(I) + 0.01
            CATR(KRARP) = (CATR(KRARP) - IPL) / AINC(I) + 1.0
C
 999  RETURN
      END
