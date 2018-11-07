      SUBROUTINE RANDIN (ISET)
C-----------------------------------------------------------------------
C! initializes tables for random number routine RANDUM
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2004, 2007
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
C   RANDIN prepares data commons for RANDUM: NEW VERSION (NOV. 1984)
C   This version has the table initialized via a DATA statement from
C   computing with the true initializer.  It then diddles it using
C   the time for some number of iterations.  The true initializer
C   appears as comments in the code.
C   Output:
C      ISET   I      pointer to trouble if not= 0.
C-----------------------------------------------------------------------
      INTEGER   ISET
C
      INTEGER   JRAND, PRAND, QRAND, MRAND(100), NRAND, IDELAY, J,
     *   ITIME(3), INRAND(100)
C      INTEGER   INTSIZ, I, KOUNT, ITEMP
      REAL      X
      DOUBLE PRECISION RNRAND
      COMMON /RANDMM/ RNRAND, MRAND, NRAND, PRAND, QRAND, JRAND
      DATA INRAND /  232085556,  207778830, 1196718056, 948485703,
     *   575109199, 1673800147,  276050447,   97621426,  298849581,
     *  1614294203,  287231490, 1527258661,  315092415, 1254059662,
     *   383262807,  680251668, 1063205858,  913567034, 1916457777,
     *     1065785,  471260114, 1357337694, 2049183941,  388107337,
     *   776541639, 1656610632,  743748189, 1297950283,   70808122,
     *  1334648688, 126256024,   638278895, 1535413525, 1089828321,
     *   743301139,  696553491, 1127988381, 1468518420, 1728256288,
     *   797718733, 1831105798,  197149292,  900782615, 1756727822,
     *  1671772275, 1308545614,  235310070,  780032432,  497617739,
     *  1873523955, 1476486124,  559296624, 2128812478, 1777068439,
     *   395251973, 1165445378, 1048221075, 1027840040, 1577292529,
     *  1807006046, 1973376358, 1475264007, 1854897060, 2048873844,
     *    53028687,  130945066, 1637165294, 1088151758, 1728995902,
     *   846158640,  968599208, 1395300349,  950235533, 1946509113,
     *  1460642563,  334509914,  924109414,  983700604, 1537172894,
     *  1024764440, 1594166065,  319927957,  998361955, 1212254402,
     *   251417189, 1104359330, 1382452580,  663929484,  601548900,
     *  2109616455,  218116063,  505720591, 1304660207, 1997322280,
     *  1153302129, 1171767058, 1728954775, 1959312883,          0, 0/
C-----------------------------------------------------------------------
      ISET = 0
      PRAND = 98
      JRAND = 98
      QRAND = 27
      NRAND =  2147483647
      RNRAND = 2147483647.
      CALL COPY (100, INRAND, MRAND)
C ************ code below is true initializer **************************
C                                        init values
C      PRAND = 98
C      QRAND = 27
C      INTSIZ = 31
C      TWO = 2
C      ONE = TWO ** (INTSIZ-1)
C      NRAND = (ONE-1)*TWO + 1
C      RNRAND = FLOAT(NRAND)
C      JRAND = 0
C      ISET = 0
C      DO 10 I = 1,PRAND
C         MRAND(I) = ONE
C 10      CONTINUE
C      IDELAY = 100*PRAND
C                                        first round of buggering
C      DO 40 J = 1,INTSIZ
C         KOUNT = 0
C         DO 20 I = 1,IDELAY
C            CALL RANDUM (X)
C 20         CONTINUE
C         DO 30 I = 1,PRAND
C            ITEMP = ONE / (TWO ** (J-1))
C            ITEMP = (MRAND(I) - (MRAND(I)/ONE)*ONE) / ITEMP
C            IF (ITEMP.EQ.1) KOUNT = KOUNT + 1
C            IF (J.LT.INTSIZ) MRAND(I) = MRAND(I)/TWO + ONE
C 30         CONTINUE
C         IF (KOUNT.EQ.PRAND) ISET = J
C 40      CONTINUE
C                                        run it a while to clear the
C                                        initial non-randomness
C      IDELAY = 4000
C      DO 60 J = 1,IDELAY
C         DO 50 I = 1,PRAND
C            CALL RANDUM (X)
C 50         CONTINUE
C 60      CONTINUE
C ************ code above is true initializer **************************
C                                       Run it a bit to dither things
      CALL ZTIME (ITIME)
      IDELAY = 10104*ITIME(3) + 339*ITIME(2) + 8*ITIME(1)
      IF (IDELAY.EQ.0) IDELAY = 1111
      DO 20 J = 1,IDELAY
         CALL RANDUM (X)
 20      CONTINUE
C
 999  RETURN
      END
