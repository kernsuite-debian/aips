      SUBROUTINE CUBHLX (START, ROTS, HUE, GAMMA, NLEVS, RED, GRN, BLU,
     *   NLO, nHI)
C-----------------------------------------------------------------------
C! OFM helix in color intensity space called by TVHELX
C# POPS=appl TV-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 2010
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
C   Does a helix in OFM colors, which is monotonically increasing in
C   perceived intensity (since R, G, B are appropriately weighted).
C   Calculates a "helix" colour table. The colours run along the
C   diagonal of the [R,G,B] colour cube, from black [0,0,0] to white
C   [1,1,1].  Deviations away from the diagonal vary quadratically,
C   increasing from zero at black, to a maximum, then decreasing to
C   zero at white, all the time rotating in colour (i.e. a tapered
C   helix along the [R,G,B] diagonal).
C
C   The parameters controlling the colour helix are:
C
C      START    colour (1=red, 2=green, 3=red; e.g. 0.5=purple);
C      ROTS     rotations in colour (typically -1.5 to 1.5, e.g. -1.0
C               is one blue->green->red cycle);
C      HUE      for hue intensity scaling (in the range 0.0 (B+W) to
C               1.0 to be strictly correct, larger values may be OK
C               with particular start/end colours);
C      GAMMA    set the gamma correction for intensity.
C
C   The routine returns a colour table NLEVS elements long in RED,
C   GRN and BLU (each element in the range 0.0 to 1.0), and also
C   returns the number of values, NLO and NHI, that were clipped to
C   0.0 or 1.0 respectively.
C   Dave Green --- MRAO --- 2010 September 20th
C-----------------------------------------------------------------------
      REAL      START, ROTS, HUE, GAMMA, RED(*), GRN(*), BLU(*)
      INTEGER   NLEVS, NLO, NHI
C
      REAL      PI, FRACT, ANGLE, AMP
      INTEGER   I
C-----------------------------------------------------------------------
      PI = 4.0 * ATAN (1.0)
      NLO = 0
      NHI = 0
C
      DO 20 I = 1,NLEVS
         FRACT = FLOAT (I-1) / FLOAT (NLEVS-1)
         ANGLE = 2 * PI * (START/3.0 + 1.0 + ROTS * FRACT)
         FRACT = FRACT ** GAMMA
         AMP = HUE * FRACT * (1-FRACT)/2.0
         RED(I) = FRACT + AMP * (-0.14861 * COS(ANGLE) +
     *      1.78277 * SIN(ANGLE))
         GRN(I) = FRACT + AMP * (-0.29227 * COS(ANGLE) -
     *      0.90649 * SIN(ANGLE))
         BLU(I) = FRACT + AMP * (+1.97294 * COS(ANGLE))
         IF (RED(I).LT.0.0) THEN
            RED(I) = 0.0
            NLO = NLO + 1
            END IF
         IF (GRN(I).LT.0.0) THEN
            GRN(I) = 0.0
            NLO = NLO + 1
            END IF
         IF (BLU(I).LT.0.0) THEN
            BLU(I) = 0.0
            NLO = NLO + 1
            END IF
         IF (RED(I).GT.1.0) THEN
            RED(I) = 1.0
            NHI = NHI + 1
            END IF
         IF (GRN(I).GT.1.0) THEN
            GRN(I) = 1.0
            NHI = NHI + 1
            END IF
         IF (BLU(I).GT.1.0) THEN
            BLU(I) = 1.0
            NHI = NHI + 1
            END IF
 20      CONTINUE
C
 999  RETURN
      END
