      SUBROUTINE QGRD6 (UV, VIS, WT, GRID, CONX, CONY, CONI, NO2, M,
     *   LROW, INC, NVIS)
C-----------------------------------------------------------------------
C! Convolves real (only) visibility data onto a grid & beam.
C# AP-appl UV Singledish
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2006, 2012
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
C   Pseudo-AP version
C   Convolves real-only visibility data onto a grid, producing a beam in
C   the imaginary part.  A single channel is gridded at a time.  It
C   assumes that NO points lie within one half the convolving function
C   support size of the outside edge.
C   Inputs:
C      UV    I  Location of (u,v) values in cells, unif weight FACTOR
C      VIS   I  Location of (complex) visibilities.
C      WT    I  Weight for data. Assumes any tapering has been done
C      GRID  I  base address of gridded data.
C               Order assumed to be the following
C               for each of the M rows:
C                1) 2 * LROW visibilities
C      CONX  I  base address of X convolving fn.
C      CONY  I  base address of Y convolving fn.
C               IF CONY = CONX then they both point at a circular array
C      CONI  I  # values / image cell in CONX (was assumed 100)
C      NO2   I  X support radius of convolving function
C      M     I  Y support size (2*radius + 1)
C      LROW  I  length of a (in reals) row ( max. X).
C      INC   I  increment for UV, VIS and WT
C      NVIS  I  number of visibilities to grid.
C   In the above, X refers to rows and y to columns in the gridded data,
C   NOT on the sky.  The total numbers of rows and cells used on a row
C   should be odd. All AP memory I/O values are assumed floating.
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, GRID, CONX, CONY, CONI, NO2, M, LROW, INC,
     *   NVIS
C
      LONGINT   JUV, JVIS, JWT, JGRID, JCONX, JCONY, JCX, JCY, JG, JJCX,
     *   JJLOOP
      INTEGER   IRND, N, INCR, HAFX, HAFY, IX, IY, OFFS, LINC
      DOUBLE PRECISION RE, RRE, AAIM, X, XX, XWT, Y, AIM, UWT, XCONI
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
      IRND(XX) = INT (XX + SIGN (0.5D0, XX))
C-----------------------------------------------------------------------
C                                        Convert addresses to 1 rel.
      JUV = UV + PSAPOF
      JVIS = VIS + PSAPOF
      JWT = WT + PSAPOF
      JGRID = GRID + PSAPOF
      JCONX = CONX + PSAPOF
      JCONY = CONY + PSAPOF
      N = NO2 * 2 + 1
      HAFX = LROW / 2 - NO2
      HAFY = -M/2 - 1
      OFFS = HAFX + LROW * HAFY
      INCR = 2 * LROW - 2 * N
      XCONI = CONI
C                                        Loop over visibilities.
      INCLUDE 'INCS:ZVND.INC'
      IF (JCONY.NE.JCONX) THEN
      INCLUDE 'INCS:ZVND.INC'
         DO 90 JJLOOP = 1,NVIS
C                                        Check weight.
            XWT = APCORE(JWT)
            UWT = APCORE(JUV+2)
            IF ((XWT.GT.0.0D0) .AND. (UWT.GT.0.0D0)) THEN
C                                        Determine location.
               X = APCORE(JUV+1)
               Y = APCORE(JUV)
C                                        Deter. conv. fn loc.
               JCX = IRND (XCONI * (IRND (X) - X - 0.5D0))
               JCY = IRND (XCONI * (IRND (Y) - Y - 0.5D0))
               JCX = JCX + JCONX + CONI
               JCY = JCY + JCONY + CONI
C                                        Determine grid loc.
               JG = 2 * (IRND (X) + OFFS + IRND (Y) * LROW)
               JG = JG + JGRID
C                                        Save JCX
               JJCX = JCX
C                                        Get visibility
               RE = APCORE(JVIS) * XWT * UWT
               AIM = XWT * UWT
C                                        Gridding loop
      INCLUDE 'INCS:ZVND.INC'
               DO 40 IY = 1,M
                  JCX = JJCX
                  RRE = RE * APCORE(JCY)
                  AAIM = AIM * APCORE(JCY)
      INCLUDE 'INCS:ZVND.INC'
                  DO 20 IX = 1,N
C                                        Sum to grid.
                     APCORE(JG) = APCORE(JG) + APCORE(JCX) * RRE
                     APCORE(JG+1) = APCORE(JG+1) + APCORE(JCX) * AAIM
C                                        Update pointers.
                     JCX = JCX + CONI
                     JG = JG + 2
 20                  CONTINUE
C                                        Update pointers.
                  JCY = JCY + CONI
                  JG = JG + INCR
 40               CONTINUE
               END IF
C                                       Update for next vis.
 80         JUV = JUV + INC
            JVIS = JVIS + INC
            JWT = JWT + INC
 90         CONTINUE
C                                        Precomputed 2D function
      ELSE
         LINC = N * CONI + 1
      INCLUDE 'INCS:ZVND.INC'
         DO 190 JJLOOP = 1,NVIS
C                                        Check weight.
            XWT = APCORE(JWT)
            UWT = APCORE(JUV+2)
            IF ((XWT.GT.0.0D0) .AND. (UWT.GT.0.0D0)) THEN
C                                        Determine location.
               X = APCORE(JUV+1)
               Y = APCORE(JUV)
C                                        Deter. conv. fn loc.
               JCX = IRND (XCONI * (IRND (X) - X - 0.5D0))
               JCY = IRND (XCONI * (IRND (Y) - Y - 0.5D0))
               JCX = JCX + JCONX + CONI
               JCY = (JCY + CONI) * LINC
C                                        Determine grid loc.
               JG = 2 * (IRND (X) + OFFS + IRND (Y) * LROW)
               JG = JG + JGRID
C                                        Save JCX
               JJCX = JCX
C                                        Get visibility
               RE = APCORE(JVIS) * XWT * UWT
               AIM = XWT * UWT
C                                        Gridding loop
      INCLUDE 'INCS:ZVND.INC'
               DO 140 IY = 1,M
                  JCX = JJCX + JCY
      INCLUDE 'INCS:ZVND.INC'
                  DO 120 IX = 1,N
C                                        Sum to grid.
                     APCORE(JG) = APCORE(JG) + APCORE(JCX) * RE
                     APCORE(JG+1) = APCORE(JG+1) + APCORE(JCX) * AIM
C                                        Update pointers.
                     JCX = JCX + CONI
                     JG = JG + 2
 120                 CONTINUE
C                                        Update pointers.
                  JCY = JCY + CONI * LINC
                  JG = JG + INCR
 140              CONTINUE
               END IF
C                                       Update for next vis.
            JUV = JUV + INC
            JVIS = JVIS + INC
            JWT = JWT + INC
 190        CONTINUE
         END IF
C
 999  RETURN
      END
