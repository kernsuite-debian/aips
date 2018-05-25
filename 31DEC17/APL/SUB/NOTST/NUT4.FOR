      SUBROUTINE NUT4 (DATJUL, DELPSI, DELEPS, SPSI, SEPS)
C-----------------------------------------------------------------------
C! Computes nutation using a non ridgid earth model
C# Math Coordinates
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
C   Calculation of nutation, assuming a non-rigid Earth.  This is a
C   special version of NUT2.  For details, see Fred Schwab or Ken
C   Johnston.
C-----------------------------------------------------------------------
      INTEGER   I
      DOUBLE PRECISION    DATJUL, DELPSI, DELEPS, SPSI, SEPS
      DOUBLE PRECISION    A(6), SINBBL(23), COSBBL(23), SINBBS(46),
     *   COSBBS(46),
     *   BBL(23), BBS(46), CBIL(9), CBIS(3), TBIS(3), TBIL(9), TCIL(5),
     *   CCIL(5), BIL(23), BIS(46), CIL(17), CIS(25), TCIS1, TCIS3,
     *   CCIS1, CCIS3, DIV1, T, A21, A23, A24, A25, A26, A44
      DATA TBIL /-17.1886D0,-1.314D0,.2088D0,.1454D0,-.0514D0,.0214D0,
     *   .0124D0,.0016D0,-.0015D0/
      DATA CBIL /-.01737D0,-1.3D-4,2D-5,
     *   -3.1D-4,1.2D-4,-5D-5,1D-5,-1D-5,1D-5/
      DATA CBIS /-2D-5,1D-5,-4D-5/
      DATA TBIS /-.208D0,.0711D0,-.0342D0/
      DATA TCIL /9.2014D0,.5724D0,-.0904D0,.0224D0,-.0093D0/
      DATA CCIL /9.1D-4,-2.9D-4,4D-5,-6D-5,3D-5/
      DATA TCIS1, TCIS3, CCIS1, CCIS3 /.0906D0,.0115D0,-5D-5,-1D-5/
      DATA BIL /7*0D0,2*4.5D-3,-2.1D-3,2*0D0,-1.5D-3,-1D-3,
     *   1D-3,2*-5D-4,-4D-4,4D-4,3D-4,2*-3D-4,
     *   -2D-4/
      DATA BIS /3*0D0,-2.61D-2,-1.49D-2,1.14D-2,6D-3,
     *   5.8D-3,-5.7D-3,-5.2D-3,-4.4D-3,-3.2D-3,
     *   2.8D-3,2.6D-3,-2.6D-3,2.5D-3,1.9D-3,1.4D-3,-1.3D-3,-9D-4,7D-4,
     *   -7D-4,6D-4,2*-6D-4,6D-4,-6D-4,5D-4,2*-5D-4,
     *   -4D-4,4D-4,2*-4D-4,4D-4,3D-4,2*-3D-4,
     *   2*-2D-4,2D-4,3*-2D-4,2D-4,-2D-4/
      DATA CIL /5*0D0,-6.6D-3,-2.4D-3,7D-4,8D-4,5D-4,2*3D-4,
     *   2D-4,2*-2D-4,2D-4,5.4D-3/
      DATA CIS /0D0,1.83D-2,0D0,-5D-3,-3.1D-3,3D-3,2.2D-3,2.3D-3,
     *   1.4D-3,-1.1D-3,1.1D-3,-1D-3,-7D-4,7D-4,5D-4,-3D-4,
     *   2*3D-4,-2D-4,3D-4,-3D-4,2*3D-4,2D-4,4D-4/
C-----------------------------------------------------------------------
      DIV1 = ATAN(1.D0) / 162000.D0
      T = (DATJUL - 2415020.D0) / 36525.D0
      CALL DA13 (DATJUL, A(1), A(2), A(3))
      CALL DA46 (DATJUL, A(4), A(5), A(6))
      A21 = 2.0D0 * A(1)
      A23 = 2.0D0 * A(3)
      A24 = 2.0D0 * A(4)
      A25 = 2.0D0 * A(5)
      A26 = 2.0D0 * A(6)
      A44 = 4.0D0 * A(4)
      BBL(1) = A(5)
      BBL(2) = A21
      BBL(3) = A25
      BBL(4) = A(3)
      BBL(5) = A21 + A(3)
      BBL(6) = A21 - A(3)
      BBL(7) = A21 - A(5)
      BBL(8) = A24 - A26 - A(5)
      BBL(9) = A26 + A21 - A24
      BBL(10) = A21 - A25
      BBL(11) = A23
      BBL(12) = A21 + A23
      BBL(13) = A(3) + A(5)
      BBL(14) = A(5) - A(3)
      BBL(15) = A(5) - BBL(8)
      BBL(16) = A(5) - BBL(9)
      BBL(17) = A21 - A(3) - A(5)
      BBL(18) = BBL(17) - A(3)
      BBL(19) = A26 - A24 + A21 + A(5)
      BBL(20) = A21 - BBL(14)
      BBL(21) = A24 - A26
      BBL(22) = A(6) - A(4) + A(1)
      BBL(23) = BBL(22) - A(3)
      BBS(1) = A24
      BBS(2) = A(6)
      BBS(3) = A24 - A(5)
      BBS(4) = A(6) + A24
      BBS(5) = A(6) - A24 + A21
      BBS(6) = -A(6) + A24
      BBS(7) = A24 - A21
      BBS(8) = A(6) + A(5)
      BBS(9) = A(5) - A(6)
      BBS(10) = A44 - A(6) - A21
      BBS(11) = A(6) + A24 - A(5)
      BBS(12) = A44 - A21
      BBS(13) = A26
      BBS(14) = A(6) + A21
      BBS(15) = A26 + A24
      BBS(16) = A24 - A25
      BBS(17) = A24 - A(6) - A(5)
      BBS(18) = A24 - A(6) - A21 + A(5)
      BBS(19) = A(6) - A24 + A21 + A(5)
      BBS(20) = A44 - A(6) - A21 - A(5)
      BBS(21) = A(3) + A24
      BBS(22) = A(6) + A(3) - A24 + A21
      BBS(23) = A(6) + A24 - A21
      BBS(24) = A24 - A(3)
      BBS(25) = A44 - A21 + A(6)
      BBS(26) = A21 + A26
      BBS(27) = A24 - A21 + A(5)
      BBS(28) = A(6) - A(5) + A21
      BBS(29) = A21 - A24 + A(5)
      BBS(30) = A44 - A(5) - A21
      BBS(31) = A24 - A(5) + A26
      BBS(32) = A(6) - A(3)
      BBS(33) = A(3) - A24 + A21
      BBS(34) = A(4) - A(1)
      BBS(35) = A(6) - A24 + A25
      BBS(36) = A(6) + A24 - A25
      BBS(37) = A(6) + A(3)
      BBS(38) = A(6) - A(3) + A24
      BBS(39) = A(5) - A26
      BBS(40) = A21 - A(6) - A(5)
      BBS(41) = A26 + A(5)
      BBS(42) = A44 - A(6) - A(3) - A21
      BBS(43) = A44 - A(3) - A21
      BBS(44) = A(6) + A25
      BBS(45) = A(6) + A(3) + A24
      BBS(46) = 3D0 * A(6) + A24
      DO 10 I = 1,7
         BIL(I) = TBIL(I) + CBIL(I) * T
 10      CONTINUE
      DO 20 I = 11,12
         BIL(I) = TBIL(I-3) + CBIL(I-3) * T
 20      CONTINUE
      DO 30 I = 1,3
         BIS(I) = TBIS(I) + CBIS(I) * T
 30      CONTINUE
      DO 40 I = 1,5
         CIL(I) = TCIL(I) + CCIL(I) * T
 40      CONTINUE
      CIS(1) = TCIS1 + CCIS1 * T
      CIS(3) = TCIS3 + CCIS3 * T
      DO 50 I = 1,23
         SINBBL(I) = SIN (BBL(I))
 50      CONTINUE
      DO 60 I = 1,46
         SINBBS(I) = SIN (BBS(I))
 60      CONTINUE
      DO 70 I = 1,3
         COSBBL(I) = COS (BBL(I))
 70      CONTINUE
      DO 80 I = 4,7
         COSBBL(I) = COS (BBL(I+1))
 80      CONTINUE
      DO 90 I = 8,10
         COSBBL(I) = COS (BBL(I+4))
 90      CONTINUE
      DO 100 I = 11,16
         COSBBL(I) = COS (BBL(I+5))
 100     CONTINUE
      COSBBS(1) = COS (BBS(1))
      DO 110 I = 2,3
         COSBBS(I) = COS (BBS(I+1))
 110     CONTINUE
      COSBBS(4) = COS (BBS(6))
      DO 120 I = 5,9
         COSBBS(I) = COS (BBS(I+3))
 120     CONTINUE
      COSBBS(10) = COS (BBS(14))
      COSBBS(11) = COS (BBS(15))
      DO 130 I = 12,16
         COSBBS(I) = COS (BBS(I+5))
 130     CONTINUE
      DO 140 I = 17,24
         COSBBS(I) = COS (BBS(I+7))
 140     CONTINUE
      COSBBL(17) = COS (BBL(4))
      COSBBS(25) = COS (BBS(2))
      SPSI = 0.D0
      DO 150 I = 1,46
         SPSI = SPSI + BIS(I) * SINBBS(I)
 150     CONTINUE
      DELPSI = 0.D0
      DO 160 I = 1,23
         DELPSI = DELPSI + BIL(I) * SINBBL(I)
 160     CONTINUE
      DELPSI = DELPSI + SPSI
      SEPS = 0.D0
      DO 170 I = 1,25
         SEPS = SEPS + CIS(I) * COSBBS(I)
 170     CONTINUE
      DELEPS = 0.D0
      DO 180 I = 1,17
         DELEPS = DELEPS + CIL(I) * COSBBL(I)
 180     CONTINUE
      DELEPS = DELEPS + SEPS
      DELPSI = DIV1 * DELPSI
      DELEPS = DELEPS * DIV1
      SPSI = SPSI * DIV1
      SEPS = SEPS * DIV1
C
 999  RETURN
      END
