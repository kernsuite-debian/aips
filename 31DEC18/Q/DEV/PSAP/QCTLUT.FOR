      SUBROUTINE QCTLUT
C-----------------------------------------------------------------------
C! Pseudo AP routine: Initialize cosine lookup table etc.
C# AP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006
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
C   Initilizes cosine look table and other constants in common /COSLUT/
C   Table lookup scheme does a third order series expansion correction:
C     COS (tab+fract) = COS(tab)*COS(fract) - SIN(tab)*SIN(fract)
C     SIN (tab+fract) = SIN(tab)*COS(fract) + COS(tab)*SIN(fract)
C     Series expansion  for cos:
C          cos(fract) = 1 - (1/2) * fract**2 (fract is the
C                    difference from the phase of the tabulated value.)
C     Series expansion  for sin:
C                    sin(fract) = fract - (1/6) * fract**3
C   Outputs in common:
C      CLUTSC    R    Scaling factor = (LENLUT / 2*pi)
C      CLUTSI    R    1.0 / CLUTSC
C      LENLUT    I    Length of lookup table in words (must be a power
C                     of 2)
C      CTMASK    I    Mask to .AND. with to limit range (0,LENLUT-1)
C                     CTMASK = LENLUT - 1
C      CTBLBL    R    If .EQ. FBLANK then the table has been initialized
C      COSTAB    R(*) Cosine lookup table (0-2*pi)
C-----------------------------------------------------------------------
      DOUBLE PRECISION ARG, TWOPI
      INTEGER   LOOP, LENLUT, CTMASK
      REAL      CLUTSC, CLUTSI, CTBLBL, COSTAB(512)
      INCLUDE 'INCS:DDCH.INC'
      COMMON /COSLUT/ CLUTSC, CLUTSI, LENLUT, CTMASK, CTBLBL, COSTAB
C-----------------------------------------------------------------------
C                                       Set constants
      TWOPI = 8.0D0 * ATAN (1.0D0)
C                                       Mark as initialized
      CTBLBL = FBLANK
C                                       Length
      LENLUT = 512
C                                       Mask
      CTMASK = LENLUT -1
C                                       Scale
      CLUTSC = LENLUT / TWOPI
      CLUTSI = TWOPI / LENLUT
C                                       fill table
      DO 100 LOOP = 1,LENLUT
         ARG = (LOOP-1) * CLUTSI
         COSTAB(LOOP) = COS (ARG)
 100     CONTINUE
C
 999  RETURN
      END
