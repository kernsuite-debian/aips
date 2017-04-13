      SUBROUTINE XYVAL (XPIX, YPIX, X, Y, Z, IERR)
C-----------------------------------------------------------------------
C! returns coordinate values corresponding to specified pixel position
C# Coordinates
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2013
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
C   XYVAL determines the coordinate value (X,Y,Z) corresponding to the
C   pixel location (XPIX,YPIX).  The pixel values need not be integers.
C   The necessary map header data is passed via common /LOCATI/
C   requiring a previous call to SETLOC. This program is the inverse of
C   XYPIX.
C   Inputs:
C      XPIX    R    Pixel location, x-coordinate
C      YPIX    R    Pixel location, y-coordinate
C   Outputs:
C      X       D    X-coordinate value at pixel location
C      Y       D    Y-coordinate value at pixel location
C      Z       D    Z-coordinate value (if part of a position
C                   pair with either X or Y)
C      IERR    I    0 ok, 1 out of range, 2 bad type, 3 undefined
C   Common inputs:
C      /LOCATI/ position parms deduced from the map header by
C               subroutine SETLOC
C   UNITS ARE AS IN THE MAPHEADER: degrees for position coords
C-----------------------------------------------------------------------
      DOUBLE PRECISION X, Y, Z
      REAL      XPIX, YPIX
C
      REAL      COSR, SINR
      DOUBLE PRECISION A, B, C, DX, DY, DZ, TEMP
      INTEGER   IERR, I
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Offset from ref pixel
      DX = (XPIX-RPLOC(1,LOCNUM)) * AXINC(1,LOCNUM)
      DY = (YPIX-RPLOC(2,LOCNUM)) * AXINC(2,LOCNUM)
      IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) DZ =
     *   (ZDEPTH(ZAXIS(LOCNUM)-2,LOCNUM) - RPLOC(3,LOCNUM)) *
     *   AXINC(3,LOCNUM)
C                                       Take out rotation
      IF ((ROT(LOCNUM).NE.0.0) .AND. (AXTYP(LOCNUM).GT.0) .AND.
     *   (AXTYP(LOCNUM).LT.4)) THEN
         COSR = COS(ROT(LOCNUM)*COND2R)
         SINR = SGNROT(LOCNUM) * SIN(ROT(LOCNUM)*COND2R)
         IF (AXTYP(LOCNUM).EQ.1) THEN
            TEMP = DX * COSR - DY * SINR
            DY = DY * COSR + DX * SINR
            DX = TEMP
         ELSE IF (AXTYP(LOCNUM).EQ.2) THEN
            TEMP = DX * COSR - DZ * SINR
            DZ = DZ * COSR + DX * SINR
            DX = TEMP
         ELSE IF (AXTYP(LOCNUM).EQ.3) THEN
            TEMP = DY * COSR - DZ * SINR
            DZ = DZ * COSR + DY * SINR
            DY = TEMP
            END IF
         END IF
C                                       Linear transformation
      Z = 0.0
      I = AXFUNC(KLOCL(LOCNUM)+1,LOCNUM)
      IF ((CORTYP(LOCNUM).LE.0) .OR. (AXTYP(LOCNUM).EQ.4) .OR.
     *   (I.LE.1)) THEN
         IF ((AXTYP(LOCNUM).GT.1) .AND. (AXTYP(LOCNUM).LT.4)) Z =
     *      RPVAL(3,LOCNUM) + DZ
         X = RPVAL(1,LOCNUM) + DX
         Y = RPVAL(2,LOCNUM) + DY
C                                       Partly non-linear
      ELSE
         A = RPVAL(1,LOCNUM) * COND2R
         B = RPVAL(2,LOCNUM) * COND2R
         C = RPVAL(3,LOCNUM) * COND2R
         DX = DX * COND2R
         DY = DY * COND2R
         DZ = DZ * COND2R
C                                       x, y => L,M
         IF (CORTYP(LOCNUM).EQ.1) THEN
            CALL NEWPOS (I, A, B, DX, DY, X, Y, IERR)
C                                       x, y => M, L
         ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
            CALL NEWPOS (I, B, A, DY, DX, Y, X, IERR)
C                                       x, z => L, M
         ELSE IF (CORTYP(LOCNUM).EQ.3) THEN
            CALL NEWPOS (I, A, C, DX, DZ, X, Z, IERR)
            Y = B + DY
C                                       x, z => M, L
         ELSE IF (CORTYP(LOCNUM).EQ.4) THEN
            CALL NEWPOS (I, C, A, DZ, DX, Z, X, IERR)
            Y = B + DY
C                                       y, z => L, M
         ELSE IF (CORTYP(LOCNUM).EQ.5) THEN
            CALL NEWPOS (I, B, C, DY, DZ, Y, Z, IERR)
            X = A + DX
C                                       y, z => M, L
         ELSE IF (CORTYP(LOCNUM).EQ.6) THEN
            CALL NEWPOS (I, C, B, DZ, DY, Z, Y, IERR)
            X = A + DX
            END IF
C                                       correct units back
         X  = X  / COND2R
         Y  = Y  / COND2R
         Z  = Z  / COND2R
         DX = DX / COND2R
         DY = DY / COND2R
         END IF
C                                       Felocity
      IF ((KLOCF(LOCNUM).EQ.0) .AND. (AXFUNC(1,LOCNUM).EQ.1)) X =
     *   RPVAL(1,LOCNUM) + DX / (1.0D0 + AXDENU(LOCNUM) *
     *   (XPIX-RPLOC(1,LOCNUM)))
      IF ((KLOCF(LOCNUM).EQ.1) .AND. (AXFUNC(2,LOCNUM).EQ.1)) Y =
     *   RPVAL(2,LOCNUM) + DY / (1.0D0 + AXDENU(LOCNUM) *
     *   (YPIX-RPLOC(2,LOCNUM)))
C
 999  RETURN
      END
