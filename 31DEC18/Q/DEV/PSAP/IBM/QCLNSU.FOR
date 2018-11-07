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
     *   JCOMP, JAMAP, JBBEAM, LOOP, BEAMO, SAVLOC, IBY21, AABEAM
      REAL      DX, DY, SUBT, XCOMP, YCOMP, BX, BY, AXMAX, BY21
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                        Get 1-rel addresses.
      JCOMP = COMP + 1
      JAMAP = AMAP + 1
      JBBEAM = BBEAM + 1
      IF (LMAP.LE.0) GO TO 999
C                                        Get component to be CLEANed
      XCOMP = APCORE(JCOMP+1)
      YCOMP = APCORE(JCOMP+2)
      SUBT  = APCORE(JCOMP) * APCORE(JCOMP+3)
C                                        Get beam patch
      IBX = APCORE(JBBEAM) + 0.1
      IBY = APCORE(JBBEAM+1) + 0.1
C                                        Add Beam Y address to start
      AABEAM = ABEAM + IBY
C                                        Value useful for inner loop
      IBY21 = (IBY * 2) - 1
      BY21  = IBY21
C                                        Get beam patch in reals
      BX = APCORE(JBBEAM)
      BY = APCORE(JBBEAM+1)
C                                        Subtraction loop
      INCLUDE 'INCS:ZVND.INC'
      DO 100 LOOP = 1,LMAP
C                                        Get next residual loc
         DX = APCORE(JAMAP) - XCOMP
C                                        Check if X in beam patch.
         IF (ABS (DX) .LT. BX) THEN
C                                        Calc Y of residual
            DY = APCORE(JAMAP+1) - YCOMP
C                                        Check if Y in beam patch.
            IF (ABS (DY) .LT. BY) THEN
C                                        Yes, Calc Ap loc of beam
C               IDX = DX + SIGN (0.1, DX)
C               IDY = DY + SIGN (0.1, DY)
C                                        Get location of beam value
               IF (DX.GE.0) THEN
                  BEAMO = +DY + (DX * BY21)
               ELSE
                  BEAMO = -DY - (DX * BY21)
                  END IF
C                                        Subtract
               APCORE(JAMAP+2) = APCORE(JAMAP+2) -
     *            (SUBT * APCORE(BEAMO+AABEAM))
C                                        End if Y in beam patch.
               END IF
C                                        End if X in beam patch.
            END IF
C                                        Update pointer
         JAMAP = JAMAP + 3
 100     CONTINUE
C                                        Find largest mag. residual
      JAMAP = AMAP + 3
      AXMAX = ABS (APCORE(JAMAP))
      SAVLOC = JAMAP
C                                        Loop
      INCLUDE 'INCS:ZVND.INC'
      DO 200 LOOP = 1,LMAP
         IF (AXMAX.LT.ABS (APCORE(JAMAP))) THEN
C                                        New maximum.
            AXMAX = ABS (APCORE(JAMAP))
            SAVLOC = JAMAP
            END IF
         JAMAP = JAMAP+3
 200     CONTINUE
C                                        Crunch location
      APCORE(JCOMP) = APCORE(SAVLOC)
      APCORE(JCOMP+1) = APCORE(SAVLOC-2)
      APCORE(JCOMP+2) = APCORE(SAVLOC-1)
C
 999  RETURN
      END

