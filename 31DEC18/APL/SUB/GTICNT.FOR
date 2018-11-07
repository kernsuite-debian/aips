      SUBROUTINE GTICNT (ILABEL, PIXR, NCH)
C-----------------------------------------------------------------------
C! Counts numbert of characters needed in wedge tick labels
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2002, 2014
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
C   Counts characters for vertical step wedge labeling
C   Inputs:
C      ILABEL   I      Label type -2 -> 2 => NCH = 0
C      PIXR     R(2)   Intensity range in wedge
C   Output:
C      NCH      I      Number characters needed
C-----------------------------------------------------------------------
      INTEGER   ILABEL, NCH
      REAL      PIXR(2)
C
      INTEGER   I, IX, IXL, ITRY, ILMAX, FRMT, IFRMT, LTYPE
      DOUBLE PRECISION PMIN, PMAX, DEG, DEGC, XINTER(24), DEGU, DEGL
      LOGICAL   NONUM
      CHARACTER PREFIX*5, SPRTXT*20
      REAL      PR
      INCLUDE 'INCS:DMSG.INC'
      DATA XINTER /0.001D0,0.002D0,0.005D0,0.01D0,0.02D0,0.05D0,0.1D0,
     *   0.2D0,0.5D0,1.D0,2.D0,5.D0,10.D0,20.D0,50.D0,100.D0,200.D0,
     *   500.D0,1000.D0,2000.D0,5000.D0,10000.D0,20000.D0,50000.D0/
C-----------------------------------------------------------------------
C                                       Corners give which axis has it
      NCH = 0
      LTYPE = MOD (ABS(ILABEL), 100)
      IF (LTYPE.LE.2) GO TO 999
C                                       get intensity range
      PMAX = PIXR(2)
      PMIN = PIXR(1)
      PR = PIXR(2) - PIXR(1)
      CALL METSCL (ILABEL, PR, PREFIX, NONUM)
      IF (NONUM) GO TO 999
      PMIN = PMIN * PR / (PIXR(2) - PIXR(1))
      PMAX = PMAX * PR / (PIXR(2) - PIXR(1))
      IX = 8
      IXL = 2
 10   DO 20 ITRY = 1,24
         DEG = XINTER(ITRY)
         DEGC = INT (PMAX / DEG) * DEG
         IF (DEGC.GT.PMAX) DEGC = DEGC - DEG
         I = (DEGC - PMIN) / DEG + 1
         IF ((I.GT.IXL) .AND. (I.LE.IX)) GO TO 30
 20      CONTINUE
      IF (IXL.EQ.0) THEN
         GO TO 999
      ELSE
         IXL = 0
         IX = IX + 3
         GO TO 10
         END IF
C                                       plot range
 30   DEGC = INT (PMAX / DEG) * DEG
      IF (DEGC.GT.PMAX) DEGC = DEGC - DEG
      DEGU = DEGC
      DEGC = INT (PMIN / DEG) * DEG
      IF (DEGC.LT.PMIN) DEGC = DEGC + DEG
      DEGL = DEGC
      DEGC = DEGL - DEG
      IXL = (DEGU - DEGL) / DEG + 1.001
C                                       max length of label
      PR = DEGU
      IF (ITRY.GT.9) IFRMT = FRMT (13, 3, PR, SPRTXT)
      IF (ITRY.LT.4) WRITE (SPRTXT,1030) DEGU
      IF ((ITRY.GE.4) .AND. (ITRY.LE.6)) WRITE (SPRTXT,1031) DEGU
      IF ((ITRY.GE.7) .AND. (ITRY.LE.9)) WRITE (SPRTXT,1032) DEGU
      CALL CHTRIM (SPRTXT, 13, SPRTXT, NCH)
      PR = DEGL
      IF (ITRY.GT.9) IFRMT = FRMT (13, 3, PR, SPRTXT)
      IF (ITRY.LT.4) WRITE (SPRTXT,1030) DEGL
      IF ((ITRY.GE.4) .AND. (ITRY.LE.6)) WRITE (SPRTXT,1031) DEGL
      IF ((ITRY.GE.7) .AND. (ITRY.LE.9)) WRITE (SPRTXT,1032) DEGL
      CALL CHTRIM (SPRTXT, 13, SPRTXT, ILMAX)
      NCH = MAX (NCH, ILMAX)
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT (F12.3)
 1031 FORMAT (F12.2)
 1032 FORMAT (F12.1)
      END
