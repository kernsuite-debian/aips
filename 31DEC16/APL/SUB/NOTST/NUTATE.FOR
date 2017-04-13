      SUBROUTINE NUTATE (JD, DELPSI, DELEPS)
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
C     SUBROUTINE TO CALCULATE THE TWO NUTATION ANGLES DELTA PSI AND
C     DELTA EPSILON FROM THE JULIAN DAY NUMBER. THE NUMBER OF JULIAN
C     CENTURIES (=36525 DAYS) SINCE THE GREGORIAN DATE 1900 JAN 0 12H
C     (=JULIAN DAY NUMBER 2415020) IS FIRST COMPUTED. THE TWO ANGLES
C     ARE THEN COMPUTED FROM EXPRESSIONS GIVEN BY E.W.WOOLARD IN TABLE
C     26 OF A.P.A.E. VOL. 15 PART 1 1953.
C                                      H.NES, NTH, 1977.10.28
C-----------------------------------------------------------------------
      INTEGER   FACTOR(5,69), I, J, K
      DOUBLE PRECISION JD, JC, SINT, COST, DELPSI, DELEPS, THETA
      DOUBLE PRECISION ANGLE(5)
      DOUBLE PRECISION ARG(4,5)
      DOUBLE PRECISION ST(2,13)
      DOUBLE PRECISION CT(2,13)
      DOUBLE PRECISION S(30)
      DOUBLE PRECISION C(30)
      DOUBLE PRECISION SL(26)
      DOUBLE PRECISION PI
      DATA   ARG/
C     ARGUMENT OF L
     *   8.22512800925925923D- 1,  1.32555235863425925D+ 3,
     *   2.55324074074074074D- 5,  3.99691358024691357D- 8,
C     ARGUMENT OF L-PRIME
     *   9.95766203703703701D- 1,  9.99973604166666660D+ 1,
     *   -4.16666666666666667D- 7, -9.25925925925925926D- 9,
C     ARGUMENT OF F
     *   3.12524691358024691D- 2,  1.34222784763888888D+ 3,
     *   -8.91975308641975310D- 6, -9.25925925925925926D-10,
C     ARGUMENT OF D
     *   9.74270794753086417D- 1,  1.23685309504629629D+ 3,
     *   -3.98919753086419752D- 6,  5.24691358024691355D- 9,
C     ARGUMENT OF NODE
     *   7.19953541666666667D- 1, -5.37261668981481480D  0,
     *   5.77160493827160492D- 6,  6.17283950617283948D- 9/
      DATA FACTOR/
     *   0,0,0,0,1,  0,0,2,-2,2,  0,0,0,0,2,  0,0,2,0,2,
     *   0,1,0,0,0,  1,0,0,0,0,  0,1,2,-2,2,  0,0,2,0,1,
     *   1,0,2,0,2,  0,-1,2,-2,2,  0,0,2,-2,1,  0,2,0,0,0,
     *   0,2,2,-2,2,
     *   -1,0,2,0,2,  1,0,0,0,1, -1,0,0,0,1, -1,0,2,2,2,
     *   -2,0,2,0,1,  1,0,2,0,1,  0,0,2,2,2,  1,0,2,-2,2,
     *   2,0,2,0,2, -1,0,2,0,1,  0,1,0,0,1, -1,0,0,2,1,
     *   1,0,0,-2,1,  0,-1,0,0,1, -1,0,2,2,1,  0,1,2,0,2,
     *   0,-1,2,0,2,  1,0,2,2,2,  2,0,2,-2,2,  0,0,0,2,1,
     *   -2,0,0,2,1,  0,-1,2,-2,1,  1,0,2,-2,1,  0,0,0,-2,1,
     *   0,0,2,2,1,  0,-2,2,-2,1,  2,0,0,-2,1,  2,0,2,0,1,
     *   0,1,2,-2,1, -2,0,2,0,2,
     *   1,0,0,-2,0,  0,0,0,2,0,  2,0,0,-2,0,  2,0,0,0,0,
     *   0,0,2,0,0,  0,0,2,-2,0,  2,0,-2,0,0,  1,1,0,-2,0,
     *   1,0,0,2,0,  1,-1,0,0,0,  0,1,0,-2,0,  0,0,0,1,0,
     *   1,0,-2,0,0,  1,0,2,0,0,  1,1,0,0,0,  1,0,0,-1,0,
     *   1,-1,2,0,2,  1,-1,0,-1,0, -2,0,0,0,1, -1,0,2,-2,1,
     *   2,0,0,0,1, -1,-1,2,2,2,  0,-1,2,2,2,  1,0,0,0,2,
     *   1,1,2,0,2,  3,0,2,0,2 /
      DATA ST/
     *   -17.2327D0,-.01737D0, -1.2729D0,-.00013D0, +.2088D0,+.00002D0,
     *   -  .2037D0,-.00002D0, + .1261D0,-.00031D0, +.0675D0,+.00001D0,
     *   -  .0497D0,+.00012D0, - .0342D0,-.00004D0, -.0261D0,+.00000D0,
     *   +  .0214D0,-.00005D0, + .0124D0,+.00001D0, +.0016D0,-.00001D0,
     *   -  .0015D0,+.00001D0/
      DATA CT/
     *   + 9.2100D0,+.00091D0, + .5522D0,-.00029D0, -.0904D0,+.00004D0,
     *   +  .0884D0,-.00005D0, + .0000D0,+.00000D0, +.0000D0,+.00000D0,
     *   +  .0216D0,-.00006D0, + .0183D0,+.00000D0, +.0113D0,-.00001D0,
     *   -  .0093D0,+.00003D0, - .0066D0,+.00000D0, +.0000D0,+.00000D0,
     *   +  .0007D0,+.00000D0/
      DATA S/
     *   +.0114D0,+.0058D0,-.0057D0,-.0052D0,+.0045D0,-.0044D0,-.0032D0,
     *   +.0026D0,-.0026D0,+.0019D0,-.0015D0,+.0014D0,-.0013D0,-.0010D0,
     *   -.0009D0,+.0007D0,-.0006D0,-.0006D0,+.0006D0,-.0006D0,-.0005D0,
     *   -.0005D0,+.0005D0,-.0005D0,-.0005D0,-.0004D0,+.0004D0,-.0004D0,
     *   +.0003D0,-.0003D0/
      DATA C/
     *   -.0050D0,-.0031D0,+.0030D0,+.0022D0,-.0024D0,+.0023D0,+.0014D0,
     *   -.0011D0,+.0011D0,-.0010D0,+.0008D0,-.0007D0,+.0007D0,+.0005D0,
     *   +.0005D0,-.0003D0,+.0003D0,+.0003D0,-.0002D0,+.0003D0,+.0003D0,
     *   +.0003D0,-.0003D0,+.0003D0,+.0003D0,+.0002D0,-.0002D0,+.0002D0,
     *   -.0002D0,+.0002D0/
      DATA SL/
     *   -.0149D0,+.0060D0,+.0045D0,+.0028D0,+.0025D0,-.0021D0,+.0010D0,
     *   -.0007D0,+.0006D0,+.0004D0,-.0004D0,-.0004D0,+.0004D0,+.0003D0,
     *   -.0003D0,-.0003D0,-.0003D0,-.0002D0,-.0002D0,-.0002D0,+.0002D0,
     *   -.0002D0,-.0002D0,-.0002D0,+.0002D0,-.0002D0/
      DATA   PI/3.14159265358979D0/
C-----------------------------------------------------------------------
      JC = (JD-2415020.D0) / 36525.D0
      DO 10 I = 1,5
         ANGLE(I) = ARG(1,I) + ARG(2,I)*  JC + ARG(3,I)*JC*JC +
     *   ARG(4,I)*JC*JC*JC
         ANGLE(I) = MOD (ANGLE(I),1.0D0) * 2.D0*PI
  10  CONTINUE
      DELPSI = 0.D0
      DELEPS = 0.D0
      DO 50 J = 1,69
         THETA = 0.D0
         DO 20 K = 1,5
            THETA = THETA+FACTOR(K,J)*ANGLE(K)
  20     CONTINUE
         SINT = SIN (THETA)
         COST = COS (THETA)
         IF (J.GT.13) GO TO 30
            DELPSI = DELPSI + (ST(1,J) + ST(2,J)*JC) * SINT
            DELEPS = DELEPS + (CT(1,J) + CT(2,J)*JC) * COST
            GO TO 50
  30     CONTINUE
         IF (J.GT.43) GO TO 40
            DELPSI = DELPSI + S(J-13)*SINT
            DELEPS = DELEPS + C(J-13)*COST
            GO TO 50
  40     CONTINUE
         DELPSI = DELPSI + SL(J-43)*SINT
  50  CONTINUE
      DELPSI = DELPSI / 3600.D0 / 180.D0*PI
      DELEPS = DELEPS / 3600.D0 / 180.D0*PI
C
 999  RETURN
      END
