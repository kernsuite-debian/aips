      SUBROUTINE QGRID (UV, VIS, WT, L, G, CX, CY, NO2, M, LROW, CNT,
     *   TY)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Grid uv data into row.
C# AP-appl UV
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
C   FORTRAN version of FPS AP VFC routine.
C   QGRID grids visibility data that has been loaded into the AP
C   previously.  Data is floated and scaled; a taper is applied
C   to the weights if requested before gridding.
C   When taper is requested and TY=2 or 3 locations VIS-2 and
C   VIS-1 are used for work space.  If TY=1 locations VIS+2 and
C   VIS+3 are used.
C   Inputs:
C      UV    I  Location of (u,v) values in cells.
C      VIS   I  Location of (complex) visibilities.
C      WT    I  Weight for data.
C      L     I  Length of visibility record.
C      G     I  base address of gridded data.
C      CX    I  base address of X convolving fn. (Y on sky)
C      CY    I  base address of Y convolving fn. (X on sky)
C      NO2   I  INT( (# cells used on a row) / 2 )
C      M     I  number of rows kept in the AP.
C      LROW  I  length of a row ( max. X).
C      CNT   I  Number of visibility points.
C      TY    I  Type of visibility data.
C                1 = I MAPS
C                2 = Q,U MAPS
C                3 = V MAPS
C               If TY is negative then tapering is requested.
C               If CNT is neg., do not float or scale.
C   Expects necessary constants in following AP locations:
C           8 = -SIG(U)**2 (CELLS**2) FOR TAPER
C           9 = -SIG(V)**2 (CELLS**2) FOR TAPER
C          13 = UVSC (scaling for u,v)
C          14 = SCVIS  (scaling for visibilities)
C          15 = SCWT   (scaling for weights)
C     It is assumed that all values of v correspond to row M/2.
C-----------------------------------------------------------------------
      INTEGER   UV, VIS, WT, L, G, CX, CY, NO2, M, LROW, CNT, TY
C
      INTEGER   UV1, VIS1, IWORK1, VIS2, VIS3, JVIS, JWT, ITY, ICNT,
     *    IWORK2, I, LL, JUV
      LOGICAL   NOFLT, TAPER
C-----------------------------------------------------------------------
C                                        Check for data
      IF (CNT.EQ.0) GO TO 999
C                                       Get input values to be changed
      JUV = UV
      JVIS = VIS
      JWT = WT
      LL = L
      UV1 = JUV + 1
      VIS1 = JVIS + 1
C                                         Check for nofloat
      NOFLT = CNT.LT.0
      ICNT = ABS(CNT)
C                                        Check if taper requested.
      TAPER = TY.LT.0
      ITY = ABS(TY)
C                                         Float
      IF (.NOT.NOFLT) THEN
         CALL QVFLT (JUV, L, JUV, L, ICNT)
         CALL QVFLT (UV1, L, UV1, L, ICNT)
         CALL QVFLT (JVIS, L, JVIS, L, ICNT)
         CALL QVFLT (VIS1, L, VIS1, L, ICNT)
         CALL QVFLT (JWT, L, JWT, L, ICNT)
C                                        Scale
         CALL QVSMUL (JUV, L, 13, JUV, L, ICNT)
         CALL QVSMUL (UV1, L, 13, UV1, L, ICNT)
         CALL QVSMUL (JVIS, L, 14, JVIS, L, ICNT)
         CALL QVSMUL (VIS1, L, 14, VIS1, L, ICNT)
         CALL QVSMUL (JWT, L, 15, JWT, L, ICNT)
         END IF
C                                         Taper if requested.
      IF (TAPER) THEN
C                                         Check type.
C                                         Pick unused work area.
         IF (ITY.NE.1) THEN
            IWORK1 = JVIS - 2
            IWORK2 = JVIS - 1
C                                         Q,U or V map
         ELSE
            IWORK1 = JVIS + 2
            IWORK2 = JVIS + 3
            END IF
C                                        Apply taper.
         CALL QVSQ (JUV, L, IWORK1, L, ICNT)
         CALL QVSQ (UV1, L, IWORK2, L, ICNT)
         CALL QVSMUL (IWORK1, L, 8, IWORK1, L, ICNT)
         CALL QVSMUL (IWORK2, L, 9, IWORK2, L, ICNT)
         CALL QVADD (IWORK1, L, IWORK2, L, IWORK1, L, ICNT)
         CALL QVEXP (IWORK1, L, IWORK1, L, ICNT)
         CALL QVMUL (JWT, L, IWORK1, L, JWT, L, ICNT)
         END IF
C                                         I or V maps.
C                                         Gridding loop.
      IF (ITY.NE.2) THEN
         DO 60 I = 1,ICNT
C                                         Grid vis point.
            CALL QGRD1 (JUV, JVIS, JWT, G, CX, CY, NO2, M, LROW)
C                                         Update pointers.
            JUV = JUV + LL
            JVIS = JVIS + LL
            JWT = JWT + LL
 60         CONTINUE
C                                         QPOL,UPOL maps.
C                                         Finish float.
      ELSE
         IF (.NOT.NOFLT) THEN
            VIS2 = JVIS + 2
            VIS3 = JVIS + 3
            CALL QVFLT (VIS2, L, VIS2, L, ICNT)
            CALL QVFLT (VIS3, L, VIS3, L, ICNT)
            CALL QVSMUL (VIS2, L, 14, VIS2, L, ICNT)
            CALL QVSMUL (VIS3, L, 14, VIS3, L, ICNT)
            END IF
C                                         Gridding loop.
         DO 150 I = 1,ICNT
C                                         Grid vis. point.
            CALL QGRD2 (JUV, JVIS, JWT, G, CX, CY, NO2, M, LROW)
C                                         Update pointers.
            JUV = JUV + LL
            JVIS = JVIS + LL
            JWT = JWT + LL
 150        CONTINUE
         END IF
C
 999  RETURN
      END
