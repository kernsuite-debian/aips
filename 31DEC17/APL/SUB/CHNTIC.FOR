      SUBROUTINE CHNTIC (BLC, TRC, CHMAX)
C-----------------------------------------------------------------------
C! counts characters to the left of a plot (for labeling vertical axis)
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2003
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
C   CHNTIC counts the number of characters to the left of a plot used
C   for labeling the vertical axis.
C   Inputs:
C      BLC     R(2)   X AND Y pixels to form bottom left hand
C                     corner of the graph.
C      TRC     R(2)   X and Y pixels to form the top right hand
C                     corner of the graph.
C   Output:
C      CHMAX   I      Max number of characters.
C-----------------------------------------------------------------------
      REAL      BLC(2), TRC(2)
      INTEGER   CHMAX
C
      CHARACTER SPRTXT*24, CHDL*4
      DOUBLE PRECISION DEG, DEGC, DX, DY, PT5SEC, TICX, TICY
      REAL      X, Y, XYRATO, YX, TICL
      INTEGER   HML(2), AXISTP, I, ILEN, ITRY, INOI, COOTYP, IERR
      LOGICAL   NONUM
      INCLUDE 'INCS:DLOC.INC'
C-----------------------------------------------------------------------
C                                       Assign initial values.
      CHMAX = 0
      CALL CHECKL ('CHNTIC')
C                                       Find value at BLC.
C                                       vertical
      XYRATO = 1.0
      YX = 0.0
      AXISTP = LABTYP(LOCNUM) / 10
      COOTYP = 2
      IF ((AXISTP.EQ.5) .OR. (AXISTP.GE.7)) COOTYP = 1
      CALL TICINC (2, BLC, TRC, XYRATO, YX, DEGC, DEG, INOI, TICX, TICY,
     *   TICL, PT5SEC, ITRY, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Determine possible tic intervls
 30   NONUM = (CPREF(2,LOCNUM).EQ.' ') .AND. (CTYP(2,LOCNUM).EQ.' ')
      IF (NONUM) GO TO 999
C                                       Draw tic marks and values.
 200  HML(1) = -32000
      HML(2) = -32000
      CHDL = '$$'
      DO 290 I = 1,INOI
         DY = DEGC
         CALL FNDX (BLC(1), DY, DX, IERR)
C                                       Convert degrees to pixels.
         IF (IERR.EQ.0) CALL XYPIX (DX, DY, X, Y, IERR)
         IF (IERR.EQ.0) THEN
            IF ((X.GT.BLC(1)-0.01) .AND. (X.LT.TRC(1)+0.01) .AND.
     *         (Y.GT.BLC(2)-0.01) .AND. (Y.LT.TRC(2)+0.01)) THEN
               CALL TICSTR (ITRY, DEGC, PT5SEC, AXISTP, COOTYP, CHDL,
     *            HML, SPRTXT, ILEN)
               CHMAX = MAX (CHMAX, ILEN)
               END IF
            END IF
         DEGC = DEGC - DEG
 290     CONTINUE
C
 999  RETURN
      END
