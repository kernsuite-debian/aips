      SUBROUTINE XYPIX (X, Y, XPIX, YPIX, IERR)
C-----------------------------------------------------------------------
C! returns pixel position corresponding to given coordinates
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
C   XYPIX determines the pixel location corresponding to a specified
C   coordinate value.  The pixel location is not necessarily an
C   integer.  The position parms are provided by the common /LOCATI/
C   which requires a previous call to SETLOC.
C   Inputs:
C      X      D     X-coordinate value (header units)
C      Y      D     Y-coordinate value (header units)
C   Output:
C      XPIX   R     x-coordinate pixel location
C      YPIX   R     y-coordinate pixel location
C      IERR   I     0 ok, 1 out of range, 2 bad type, 3 undefined
C-----------------------------------------------------------------------
      REAL      XPIX, YPIX
      DOUBLE PRECISION X, Y
C
      DOUBLE PRECISION DX, DY, DZ, A, B, C, R, AX, AY, AZ, COSR, SINR
      INTEGER   I, IERR
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      DZ = 0.0D0
      R = ROT(LOCNUM) * COND2R
      IERR = 0
C                                       linear on both axes
      I = AXFUNC(KLOCL(LOCNUM)+1,LOCNUM)
      IF ((CORTYP(LOCNUM).LE.0) .OR. (AXTYP(LOCNUM).EQ.4) .OR.
     *   (I.LE.1)) THEN
         DX = X - RPVAL(1,LOCNUM)
         DY = Y - RPVAL(2,LOCNUM)
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) DZ =
     *      AXINC(3,LOCNUM) * (ZDEPTH(ZAXIS(LOCNUM)-2,LOCNUM) -
     *      RPLOC(3,LOCNUM))
C                                       Partly non-linear
      ELSE
         A = COND2R * RPVAL(1,LOCNUM)
         B = COND2R * RPVAL(2,LOCNUM)
         C = COND2R * RPVAL(3,LOCNUM)
         AX = X * COND2R
         AY = Y * COND2R
         IF ((AXTYP(LOCNUM).EQ.2) .OR. (AXTYP(LOCNUM).EQ.3)) DZ =
     *      (ZDEPTH(ZAXIS(LOCNUM)-2,LOCNUM) - RPLOC(3,LOCNUM)) *
     *      AXINC(3,LOCNUM) * COND2R
C                                       x, y => L, M
         IF (CORTYP(LOCNUM).EQ.1) THEN
            CALL DIRCOS (I, A, B, AX, AY, DX, DY, IERR)
C                                       x, y => M, L
         ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
            CALL DIRCOS (I, B, A, AY, AX, DY, DX, IERR)
C                                       x, z => L, M
         ELSE IF (CORTYP(LOCNUM).EQ.3) THEN
            CALL DIRDEC (I, DZ, AX, A, C, R, DX, AZ, IERR)
            DY = AY - B
C                                       x, z => M, L
         ELSE IF (CORTYP(LOCNUM).EQ.4) THEN
            CALL DIRRA (I, DZ, AX, C, A, R, DX, AZ, IERR)
            DY = AY - B
C                                       y, z => L, M
         ELSE IF (CORTYP(LOCNUM).EQ.5) THEN
            CALL DIRDEC (I, DZ, AY, B, C, R, DY, AZ, IERR)
            DX = AX - A
C                                       y, z => M, L
         ELSE IF (CORTYP(LOCNUM).EQ.6) THEN
            CALL DIRRA (I, DZ, AY, C, B, R, DY, AZ, IERR)
            DX = AX - A
            END IF
C                                       back to degrees
         IF (IERR.NE.0) GO TO 999
         DX = DX / COND2R
         DY = DY / COND2R
         DZ = DZ / COND2R
C                                       Correct for rotation
C                                       (DIRRA, DIRDEC already did)
         IF ((CORTYP(LOCNUM).LE.2) .AND. (ROT(LOCNUM).NE.0.0) .AND.
     *      (AXTYP(LOCNUM).NE.0)) THEN
            COSR = COS (R)
            SINR = SGNROT(LOCNUM) * SIN (R)
            IF (AXTYP(LOCNUM).EQ.2) DX = DX*COSR + DZ*SINR
            IF (AXTYP(LOCNUM).EQ.3) DY = DY*COSR + DZ*SINR
            IF (AXTYP(LOCNUM).EQ.1) THEN
               DZ = DX*COSR + DY*SINR
               DY = DY*COSR - DX*SINR
               DX = DZ
               END IF
            END IF
         END IF
C                                       Go to pixels
      XPIX = DX / AXINC(1,LOCNUM) + RPLOC(1,LOCNUM)
      YPIX = DY / AXINC(2,LOCNUM) + RPLOC(2,LOCNUM)
C                                       Felocity
      IF ((KLOCF(LOCNUM).EQ.0) .AND. (AXFUNC(1,LOCNUM).EQ.1)) XPIX = DX
     *   / (AXINC(1,LOCNUM) - AXDENU(LOCNUM)*DX)  +  RPLOC(1,LOCNUM)
      IF ((KLOCF(LOCNUM).EQ.1) .AND. (AXFUNC(2,LOCNUM).EQ.1)) YPIX = DY
     *   / (AXINC(2,LOCNUM) - AXDENU(LOCNUM)*DY)  +  RPLOC(2,LOCNUM)
C
 999  RETURN
      END
