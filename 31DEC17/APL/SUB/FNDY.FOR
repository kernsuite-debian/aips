      SUBROUTINE FNDY (YPIX, XVAL, YVAL, IERR)
C-----------------------------------------------------------------------
C! returns Y-axis coordinate value given Y pixel and X coordinate value
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
C   FNDY returns the Y axis coordinate value of a point given the X
C   axis coordinate value and the Y axis pixel position of the point.
C   Needed for rotations and non-linear axes (L-M).
C   Inputs:
C      YPIX   R     Y pixel position
C      XVAL   D     X coordinate value
C   Output:
C      YVAL   D     Y coordinate value
C      IERR   I     0 ok, 1 out of range, 2 bad type, 3 undefined
C   Common:
C      /LOCATI/ pos parms must have been set up by SETLOC
C-----------------------------------------------------------------------
      REAL      YPIX
      DOUBLE PRECISION    YVAL, XVAL
      INTEGER   IERR
C
      DOUBLE PRECISION    R, D, P, RA0, DEC0, DS, DY, DZ, COSR, SINR
      INTEGER   I
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       y not related to x, z
      DY = (YPIX - RPLOC(2,LOCNUM)) * AXINC(2,LOCNUM)
      IF ((AXTYP(LOCNUM).NE.1) .AND. (AXTYP(LOCNUM).NE.3)) THEN
         YVAL = DY + RPVAL(2,LOCNUM)
         IF ((KLOCF(LOCNUM).EQ.1) .AND. (AXFUNC(2,LOCNUM).EQ.1)) YVAL =
     *      RPVAL(2,LOCNUM) + DY / (1.0D0 + AXDENU(LOCNUM) *
     *      (YPIX-RPLOC(2,LOCNUM)))
C                                       related somehow
      ELSE
         I = AXFUNC(KLOCL(LOCNUM)+1,LOCNUM)
         R = ROT(LOCNUM) * COND2R
         COSR = COS (R)
         SINR = SGNROT(LOCNUM) * SIN (R)
C                                       x with y
         IF (AXTYP(LOCNUM).NE.3) THEN
C                                       linear
            IF ((CORTYP(LOCNUM).LE.0) .OR. (I.LE.1)) THEN
               IF (COSR.EQ.0) DY = 0.0
               IF (COSR.NE.0) DY = (DY + (XVAL-RPVAL(1,LOCNUM))*SINR) /
     *            COSR
               YVAL = DY + RPVAL(2,LOCNUM)
C                                       non-linear
            ELSE
               D = COND2R * XVAL
               P = COND2R * DY
               RA0 = COND2R * RPVAL(CORTYP(LOCNUM),LOCNUM)
               DEC0 = COND2R * RPVAL(3-CORTYP(LOCNUM),LOCNUM)
               IF (CORTYP(LOCNUM).EQ.1) THEN
                  CALL DIRDEC (I, P, D, RA0, DEC0, R, DS, YVAL, IERR)
               ELSE IF (CORTYP(LOCNUM).EQ.2) THEN
                  CALL DIRRA (I, P, D, RA0, DEC0, R, DS, YVAL, IERR)
                  END IF
               YVAL = YVAL / COND2R
               END IF
C                                       y with z : rotation first
         ELSE
            DZ = (ZDEPTH(ZAXIS(LOCNUM)-2,LOCNUM) - RPLOC(3,LOCNUM)) *
     *         AXINC(3,LOCNUM)
            P = DY*COSR - DZ*SINR
            DZ = DZ*COSR + DY*SINR
            DY = P
C                                       linear
            IF ((CORTYP(LOCNUM).LE.0) .OR. (I.LE.1)) THEN
               YVAL = DY + RPVAL(2,LOCNUM)
C                                       non-linear
            ELSE
               DY = COND2R * DY
               DZ = COND2R * DZ
               RA0 = COND2R * RPVAL(2,LOCNUM)
               DEC0 = COND2R * RPVAL(3,LOCNUM)
               IF (CORTYP(LOCNUM).EQ.5) THEN
                  CALL NEWPOS (I, RA0, DEC0, DY, DZ, YVAL, DS, IERR)
               ELSE IF (CORTYP(LOCNUM).EQ.6) THEN
                  CALL NEWPOS (I, DEC0, RA0, DZ, DY, DS, YVAL, IERR)
                  END IF
               YVAL = YVAL / COND2R
               END IF
            END IF
         END IF
C
 999  RETURN
      END
