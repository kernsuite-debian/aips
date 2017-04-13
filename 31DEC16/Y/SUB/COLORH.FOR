      SUBROUTINE COLORH (NLEVS, STEPS, LOOPS, LITE, SATUR, HUE, GAMMA,
     *   IERR)
C-----------------------------------------------------------------------
C! writes OFM with color contour helix in lightness-hue-saturation space
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2008
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
C   COLORH uses an STC algorithm to produce color contouring along a
C   helix in the lightness-hue-saturation space.
C   Inputs:
C      NLEVS   I          number of intensities (256 or 1024)
C      STEPS   I          number of output colors (1 - 1024)
C      LOOPS   I          number of loops of helix
C      LITE    R(2)       min,max lightness (0.-100.)
C      SATUR   R(2)       min,max saturation (0.-100.)
C      HUE     R          start hue in degrees (0-360.)
C      GAMMA   R          gamma correction power (2.7 or 1.8 ok?)
C   Output:
C      IERR    I          error code of ZM70XF
C-----------------------------------------------------------------------
      INTEGER    NLEVS, STEPS, LOOPS, IERR
      REAL       LITE(2), SATUR(2), HUE, GAMMA
C
      INCLUDE 'INCS:PTVC.INC'
      INTEGER    NSTEPS, NLOOPS, IWID, KOLR, ISTRT, IT, NSTEP, LEVS, NW
      REAL       HSTEP, ZSTEP1, ALPHA, BETA, GAMINV, H, DTOR, Z, X, S,
     *   A, B, L, Y3, Y, COLR, COLX(3), COLY(3), COLZ(3), LBUF(TVMOFM,3)
      LOGICAL     VRTRTC
      INCLUDE 'INCS:DTVC.INC'
      DATA  COLX /  0.125, -1.090,  2.580/,
     *      COLY / -0.295,  2.040, -1.150/,
     *      COLZ /  1.170,  0.058, -0.422/
      DATA VRTRTC, DTOR /.TRUE., 0.017452393/
C-----------------------------------------------------------------------
C                                        check input some
      IERR = 2
      IF (GAMMA.LE.0.0) GO TO 999
      NW = TVMOFM
      LEVS = MIN (OFMINP+1, NLEVS)
      IF (LEVS.LT.32) LEVS = LUTOUT + 1
      IF (LEVS.GT.OFMINP+1) LEVS = OFMINP + 1
      NSTEPS = MAX (2, MIN (LEVS, STEPS))
      NLOOPS = MAX (1, MIN (256, LOOPS))
C
      HSTEP =  (DTOR * 360.0 * NLOOPS) / NSTEPS
      ZSTEP1 = NSTEPS - 1
      ALPHA = (LITE(2) - LITE(1)) / ZSTEP1
      BETA = SATUR(2) - SATUR(1)
      IWID = LEVS / NSTEPS
      GAMINV = 1.0 / GAMMA
      IT = OFMINP + 1
      CALL RFILL (IT, 0.0, LBUF(1,1))
      CALL RFILL (IT, 0.0, LBUF(1,2))
      CALL RFILL (IT, 0.0, LBUF(1,3))
C                                           loop over steps
      ISTRT = 1
      H = HUE * DTOR - HSTEP
      DO 20 NSTEP = 1,NSTEPS
         Z = NSTEP - 1
         X = (Z+Z) / ZSTEP1 - 1.0
         S = BETA * (1.0-X*X) + SATUR(1)
         H = H + HSTEP
         A = S * COS(H)
         B = S * SIN(H)
         L = ALPHA * Z  +  LITE(1)
C                                           LAB colors to RGB colors
         Y3 = (L + 16.0) / 116.0
         Y = Y3 ** 3
         X = (A/500.0 + Y3) ** 3
         Z = (Y3 - B/200.0) ** 3
         IF (ABS(GAMINV-1.).LT.0.02) THEN
            DO 10 KOLR = 1,3
               COLR = X * COLX(KOLR) + Y * COLY(KOLR) + Z * COLZ(KOLR)
               CALL RFILL (IWID, COLR, LBUF(ISTRT,KOLR))
 10            CONTINUE
         ELSE
            DO 15 KOLR = 1,3
               COLR = X * COLX(KOLR) + Y * COLY(KOLR) + Z * COLZ(KOLR)
               COLR = MAX (0.0, MIN (1.0, COLR))
               COLR = COLR ** GAMINV
               CALL RFILL (IWID, COLR, LBUF(ISTRT,KOLR))
 15            CONTINUE
            END IF
         ISTRT = ISTRT + IWID
 20      CONTINUE
C                                        loop over colors r g b
      LBUF(1,1) = 0.
      LBUF(1,2) = 0.
      LBUF(1,3) = 0.
      CALL OFMIO ('WRIT', NW, LEVS, VRTRTC, LBUF, IERR)
      IF (IERR.NE.0) GO TO 999
C
 999  RETURN
      END
