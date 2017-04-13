      SUBROUTINE LMPIX (X, Y, XPIX, YPIX)
C-----------------------------------------------------------------------
C! returns pixel location corresponding to specified coordinates
C# Coordinates Header
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   LMPIX determines the pixel location corresponding to a specified
C   angle coordinate value.  The pixel location is not necessarily an
C   integer.  The position parms are provided by the common /LOCATI/
C   which requires a previous call to SETLOC.  No specification of
C   which planes are involved is made.  Tha MAP header common is also
C   used.
C   Inputs:
C      X     D  longitude-coordinate value (header units)
C      Y     D  latitude-coordinate value (header units)
C   Commons input:
C      /LOCATI/
C      /MAPHDR/
C   Output:
C      XPIX  R  longitude-coordinate pixel location
C      YPIX  R  latitude-coordinate pixel location
C-----------------------------------------------------------------------
      REAL      XPIX, YPIX
      DOUBLE PRECISION X, Y
C
      DOUBLE PRECISION DX, DY, DZ, A, B, R, AX, AY, COSR, SINR,
     *   CATD(128)
      REAL      CATR(256)
      INTEGER   I, CATBLK(256), IERR
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DHDR.INC'
      COMMON /MAPHDR/ CATBLK
      EQUIVALENCE (CATBLK, CATR, CATD)
C-----------------------------------------------------------------------
      R = ROT(LOCNUM) * COND2R
C                                       linear on both axes
      IF (CORTYP(LOCNUM).GT.0) GO TO 10
         DX = X - CATD(KDCRV+KLOCL(LOCNUM))
         DY = Y - CATD(KDCRV+KLOCM(LOCNUM))
         GO TO 50
C                                       Partly non-linear
 10   A = COND2R * CATD(KDCRV+KLOCL(LOCNUM))
      B = COND2R * CATD(KDCRV+KLOCM(LOCNUM))
      AX = X * COND2R
      AY = Y * COND2R
      I = AXFUNC(KLOCL(LOCNUM)+1,LOCNUM)
      CALL DIRCOS (I, A, B, AX, AY, DX, DY, IERR)
C                                       back to degrees
 45   DX = DX / COND2R
      DY = DY / COND2R
C                                       Correct for rotation
 50   IF ((ROT(LOCNUM).EQ.0.0) .OR. (AXTYP(LOCNUM).EQ.0)) GO TO 60
         COSR = COS (R)
         SINR = SGNROT(LOCNUM) * SIN (R)
         DZ = DX*COSR + DY*SINR
         DY = DY*COSR - DX*SINR
         DX = DZ
C                                       Go to pixels
 60   IF (CATR(KRCIC+KLOCL(LOCNUM)).NE.0.0) THEN
         XPIX = DX / CATR(KRCIC+KLOCL(LOCNUM)) +
     *      CATR(KRCRP+KLOCL(LOCNUM))
      ELSE
         XPIX = 100000000
         END IF
      IF (CATR(KRCIC+KLOCM(LOCNUM)).NE.0.0) THEN
         YPIX = DY / CATR(KRCIC+KLOCM(LOCNUM)) +
     *      CATR(KRCRP+KLOCM(LOCNUM))
      ELSE
         YPIX = 100000000
         END IF
C
 999  RETURN
      END
