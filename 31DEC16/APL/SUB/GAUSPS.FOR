      SUBROUTINE GAUSPS (DIR, PX0, PY0, BMAJP, BMINP, BPAP, BMAJS,
     *   BMINS, BPAS, IERR)
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   converts between Gaussians in pixels and Gaussians on the sky.  It
C   is assumed that the sky is approximately linear about the center
C   pixel for the position angle portion of the computation.
C   Inputs:
C      DIR     C*4   'P2S', 'S2P' direction of conversion
C      PX0     R     Center X pixel
C      PY0     R     Center Y pixel
C   In or out:
C      BMAJP   R     Major axis length pixels
C      BMINP   R     Minor axis length pixels
C      BPAP    R     Position angle of the major axis anticlockwise from
C                    the Y axis in degrees.
C      BMAJS   R     Major axis length in degrees
C      BMINS   R     Minor axis length in degrees
C      BPAS    R     Position angle of the major axis East from North
C   Output
C      IERR    I     Error code from coordinate routines
C   Common:
C      DCAT.INC      image header
C      DLOC.INC      location common with SETLOC already called
C-----------------------------------------------------------------------
      CHARACTER DIR*(*)
      REAL      PX0, PY0, BMAJS, BMINS, BPAS, BMAJP, BMINP, BPAP
      INTEGER   IERR
C
      REAL      XXT, YYT, COSC, SINC
      DOUBLE PRECISION X(3), Y(3), Z(3), XX, YY
      DOUBLE PRECISION C1, C2, DX, DY, DMIN, DMAJ, XSMALL, YSMALL
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:PSTD.INC'
C-----------------------------------------------------------------------
C                                       Pixels to sky
      IF (DIR.EQ.'P2S') THEN
         SINC = SIN (DG2RAD*BPAP)
         COSC = COS (DG2RAD*BPAP)
         CALL XYVAL (PX0, PY0, X(1), Y(1), Z(1), IERR)
         IF (IERR.NE.0) GO TO 990
         XXT = PX0 - BMAJP * SINC / 2
         YYT = PY0 + BMAJP * COSC / 2.
         CALL XYVAL (XXT, YYT, X(2), Y(2), Z(2), IERR)
         IF (IERR.NE.0) GO TO 990
         XXT = PX0 - BMINP * COSC / 2.
         YYT = PY0 - BMINP * SINC / 2.
         CALL XYVAL (XXT, YYT, X(3), Y(3), Z(3), IERR)
         IF (IERR.NE.0) GO TO 990
         IF (CORTYP(LOCNUM).EQ.1) THEN
C                                       X => RA,   Y => DECLINATION
C                                       X,Y projection of major semiaxis
C                                       in degrees
            DX = X(1)-X(2)
            DY = Y(1)-Y(2)
            DMAJ = SQRT(DX*DX + DY*DY)
            C1 = COS(DG2RAD*Y(1))
            C2 = COS(DG2RAD*Y(2))
            XSMALL = SQRT (DY*DY + DX*DX*C1*C2)
            XX = SIN(DG2RAD*Y(1)) * SIN(DG2RAD*Y(2)) +
     *         COS(DG2RAD*Y(1)) * COS(DG2RAD*Y(2)) *
     *         COS(DG2RAD*(X(1)-X(2)))
            IF (DMAJ .LT. 0.1D0/3600.D0) THEN
               BMAJS = XSMALL*2.0D0
            ELSE
               BMAJS = 2. * RAD2DG * ACOS (XX)
               END IF
C                                       X,Y projection of minor semiaxis
C                                       in degrees
            DX = X(1)-X(3)
            DY = Y(1)-Y(3)
            DMIN = SQRT(DX*DX + DY*DY)
            C1 = COS(DG2RAD*Y(1))
            C2 = COS(DG2RAD*Y(3))
            YSMALL = SQRT (DY*DY + DX*DX*C1*C2)
            YY = SIN(DG2RAD*Y(1)) * SIN(DG2RAD*Y(3)) +
     *         COS(DG2RAD*Y(1)) * COS(DG2RAD*Y(3)) *
     *         COS(DG2RAD*(X(1)-X(3)))
            IF (DMIN .LT. 0.1D0/3600.D0) THEN
               BMINS = YSMALL*2.0D0
            ELSE
               BMINS = 2. * RAD2DG * ACOS (YY)
               END IF
            BPAS = ATAN2 ((X(2)-X(1)) * COS(Y(1)*DG2RAD),
     *         Y(2) - Y(1)) / DG2RAD
         ELSE
C                                       X => DECLINATION, Y => RA
C                                       X,Y projection of major semiaxis
C                                       in degrees
            DX = X(1)-X(2)
            DY = Y(1)-Y(2)
            DMAJ = SQRT(DX*DX + DY*DY)
            C1 = COS(DG2RAD*X(1))
            C2 = COS(DG2RAD*X(2))
            XSMALL = SQRT (DX*DX + DY*DY*C1*C2)
            XX = SIN(DG2RAD*X(1)) * SIN(DG2RAD*X(2)) +
     *         COS(DG2RAD*X(1)) * COS(DG2RAD*X(2)) *
     *         COS(DG2RAD*(Y(1)-Y(2)))
            IF (DMAJ .LT. 0.1D0/3600.D0) THEN
               BMAJS = XSMALL*2.0D0
            ELSE
               BMAJS = 2. * RAD2DG * ACOS (XX)
               END IF
C                                       X,Y projection of minor semiaxis
C                                       in degrees
            DX = X(1)-X(3)
            DY = Y(1)-Y(3)
            DMIN = SQRT(DX*DX + DY*DY)
            C1 = COS(DG2RAD*X(1))
            C2 = COS(DG2RAD*X(3))
            YSMALL = SQRT (DX*DX + DY*DY*C1*C2)
            YY = SIN(DG2RAD*X(1)) * SIN(DG2RAD*X(3)) +
     *         COS(DG2RAD*X(1)) * COS(DG2RAD*X(3)) *
     *         COS(DG2RAD*(Y(1)-Y(3)))
            IF (DMIN .LT. 0.1D0/3600.D0) THEN
               BMINS = YSMALL*2.0D0
            ELSE
               BMINS = 2. * RAD2DG * ACOS (YY)
               END IF
            BPAS = ATAN2 ((Y(2)-Y(1)) * COS(X(1)*DG2RAD),
     *         X(2) - X(1)) / DG2RAD
            END IF
C                                       sky to pixel:
      ELSE
         END IF
      GO TO 999
C
 990  WRITE (MSGTXT,1990) IERR, DIR
      CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1990 FORMAT ('GAUSPS: ERROR',I4,' CONVERTING ',A)
      END
