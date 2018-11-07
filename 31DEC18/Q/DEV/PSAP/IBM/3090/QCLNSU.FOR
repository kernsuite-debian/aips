@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QCLNSU (COMP, AMAP, LMAP, ABEAM, BBEAM)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Low level Clark CLEAN routine.
C# AP-appl
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
C   Pseudo-AP version
C   QCLNSU does a CLEAN on the map points in the pseudo AP
C   using the portion of the beam in the pseudo AP.
C   Inputs:
C      COMP  I  Base address of the component vector:
C               0 => intensity
C               1 => x in cells
C               2 => Y in cells
C               3 => CLEAN loop gain (fractional)
C      AMAP  I  Base address of the map stored as (X,Y,intensity)
C               -1<X<NX  -1<Y<NY
C      LMAP  I  Number of map points
C      ABEAM I  Base address of the piece of the beam.  The Y
C               dimension varies the fastest: -BY<Y<BY,
C               X varies slowest -1<X<BX
C      BBEAM I  Base address of beam descriptor vector (BX,BY)
C-----------------------------------------------------------------------
      INTEGER   COMP, AMAP, LMAP, ABEAM, BBEAM, IBX, IBY, IDX, IDY,
     *   JCOMP, JAMAP, JBBEAM, LOOP, BEAMO, IFIX, IABS, SAVLOC, I,
     *   JAMAX, IMAX, IDXA(20000), IDYA(20000), BEAMOA(20000),
     *   IBYA, IBYT, JAMAP2, JCOMP1, JCOMP2
      REAL      XLOC, X, Y, DX, DY, SUBT, XCOMP, YCOMP,
     *   XMAX, AXMAX,
     *   TEMPX(20000), TEMPY(20000), TEMPZ(20000)
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                        Get 1-rel addresses.
      JCOMP = COMP + 1
      JCOMP1 = COMP + 2
      JCOMP2 = COMP + 3
      JAMAP = AMAP + 1
      JAMAP2 = AMAP + 3
      JBBEAM = BBEAM + 1
      IF (LMAP.LE.0) GO TO 999
      IF (LMAP.GT.20000) THEN
         WRITE(*,*) 'LMAP .GT. 20000 (QCLNSU).  LMAP = ', LMAP
         GO TO 999
      END IF
C                                        Get component to be CLEANed
      XCOMP = APCORE(JCOMP1)
      YCOMP = APCORE(JCOMP2)
      SUBT = (- APCORE(JCOMP)) * APCORE(JCOMP+3)
C                                        Get beam patch
      IBX = APCORE(JBBEAM) + 0.1
      IBY = APCORE(JBBEAM+1) + 0.1
      IBYA = IBY + ABEAM
C                                        Added stuff.
      IBYT = 2 * IBY - 1
      JAMAX = JAMAP + 3 * (LMAP - 1)
C                                      
      I = 1
      DO 10 LOOP = JAMAP, JAMAX, 3
         TEMPX(I) = APCORE(LOOP) - XCOMP
         TEMPY(I) = APCORE(LOOP+1) - YCOMP
         I = I + 1
 10      CONTINUE
C
      DO 20 I = 1,LMAP
         IF(TEMPX(I).LT.0.0) THEN
           IDXA(I) = TEMPX(I) - 0.1
         ELSE
           IDXA(I) = TEMPX(I) + 0.1
         ENDIF
         IF(TEMPY(I).LT.0.0) THEN
           IDYA(I) = TEMPY(I) - 0.1
         ELSE
           IDYA(I) = TEMPY(I) + 0.1
         ENDIF
 20      CONTINUE
C
      DO 40 I = 1,LMAP
         IF(IDXA(I).LT.0) THEN 
           BEAMOA(I) = (-IDXA(I)*IBYT)-IDYA(I)
         ELSE
           BEAMOA(I) = IDXA(I)*IBYT+IDYA(I)
         ENDIF
 40      CONTINUE
C
      DO 60 I = 1,LMAP
         TEMPZ(I) = APCORE(BEAMOA(I)+IBYA)
 60      CONTINUE
C
      DO 70 I = 1,LMAP
         IF( (ABS(IDXA(I)) .GE. IBX)
     *            .OR.( ABS(IDYA(I)) .GE. IBY))
     *      TEMPZ(I) = 0.0
 70      CONTINUE
C
      CALL SAXPY(LMAP,SUBT,TEMPZ,1,APCORE(JAMAP2),3)
C                                        Find largest mag. residual
C                                        using ESSL call.
      IMAX = ISAMAX(LMAP,APCORE(JAMAP2),3)
      SAVLOC = JAMAP2 + (IMAX - 1) * 3
C                                        Crunch location
      APCORE(JCOMP) = APCORE(SAVLOC)
      APCORE(JCOMP1) = APCORE(SAVLOC-2)
      APCORE(JCOMP2) = APCORE(SAVLOC-1)
C
 999  RETURN
      END
