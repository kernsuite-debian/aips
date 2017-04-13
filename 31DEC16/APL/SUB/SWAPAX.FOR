      SUBROUTINE SWAPAX (CATBLK, CATH, CATR, CATD, IAXIS1, IAXIS2, IERR)
C-----------------------------------------------------------------------
C! swaps the values for two axes
C# Header
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
C  SWAPAX (swap axis) will swap the header values for two given axes
C  in an image header.
C  INOUT:  CATBLK  I(256)   Image header integer values.
C          CATH    H(256)   Image header HOLLERITH values (usually
C                           equivalenced to CATBLK).
C          CATR    R(256)   Image header real values (usually
C                           equivalenced to CATBLK).
C          CATD    D(128)   Image header Real*8 values.
C  IN:     IAXIS1  I   One of the axis to switch.
C          IAXIS2  I   The other axis.
C  OUT:    IERR    I   0=ok, 1=IAXIS1 or IAXIS2 > 7 or < 1.
C-----------------------------------------------------------------------
      INTEGER   CATBLK(256), IAXIS1, IAXIS2, IERR
      DOUBLE PRECISION CATD(128)
      REAL      CATR(256)
      HOLLERITH CATH(256)
C
      HOLLERITH HTEMP
      INTEGER   ITEMP, IS1, IS2
      DOUBLE PRECISION DTEMP
      REAL      RTEMP
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       Check axis ranges.
      IF ((IAXIS1.LT.1) .OR. (IAXIS1.GT.KICTPN)) GO TO 990
      IF ((IAXIS2.LT.1) .OR. (IAXIS2.GT.KICTPN)) GO TO 990
      IERR = 0
C                                       Coordinate type.
      IS1 = (IAXIS1 - 1) * 2 + KHCTP
      IS2 = (IAXIS2 - 1) * 2 + KHCTP
      HTEMP = CATH(IS1)
      CATH(IS1) = CATH(IS2)
      CATH(IS2) = HTEMP
      HTEMP = CATH(IS1+1)
      CATH(IS1+1) = CATH(IS2+1)
      CATH(IS2+1) = HTEMP
C                                       Coordinate value at ref pixel.
      DTEMP = CATD(KDCRV+IAXIS1-1)
      CATD(KDCRV+IAXIS1-1) = CATD(KDCRV+IAXIS2-1)
      CATD(KDCRV+IAXIS2-1) = DTEMP
C                                       Coordinate value increment.
      RTEMP = CATR(KRCIC+IAXIS1-1)
      CATR(KRCIC+IAXIS1-1) = CATR(KRCIC+IAXIS2-1)
      CATR(KRCIC+IAXIS2-1) = RTEMP
C                                       Coordinate reference pixel.
      RTEMP = CATR(KRCRP+IAXIS1-1)
      CATR(KRCRP+IAXIS1-1) = CATR(KRCRP+IAXIS2-1)
      CATR(KRCRP+IAXIS2-1) = RTEMP
C                                       Coordinate rotation angles.
      RTEMP = CATR(KRCRT+IAXIS1-1)
      CATR(KRCRT+IAXIS1-1) = CATR(KRCRT+IAXIS2-1)
      CATR(KRCRT+IAXIS2-1) = RTEMP
C                                       No. of pixels on axis.
      ITEMP = CATBLK(KINAX+IAXIS1-1)
      CATBLK(KINAX+IAXIS1-1) = CATBLK(KINAX+IAXIS2-1)
      CATBLK(KINAX+IAXIS2-1) = ITEMP
      GO TO 999
C                                       Axis range error.
 990  WRITE (MSGTXT,1990) IAXIS1, IAXIS2
      CALL MSGWRT (6)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('SWAPAX: AXIS RANGE ERROR. IAXIS1, IAXIS2 =',2I6)
      END
