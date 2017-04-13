      SUBROUTINE COINC (IV, NMI, CLI, IS, JV, NMJ, CLJ, JS, UID, INF,
     *   SCR, IER )
C-----------------------------------------------------------------------
C! Checks if two maps are exactly coincident.
C# Map Header
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
C   Checks if two maps are exactly coincident.
C   Inputs:
C      IV ,JV    I          volume nrs first and second map
C      NMI,NMJ   C*12       names first and second map
C      CLI,CLJ   C*6        class first and second map
C      IS ,JS    I          sequence nrs first and second map
C      UID       I          user number
C   Output:
C      INF       I          output information parameter.
C                           INF = 0 : maps coincident; INF > 0 indicates
C                           difference : 1 in # dims, 2 in # points/axis
C                           3 in pixel increments, 4 : maps are shifted
C                           in position.
C      SCR       I(256)     scratch array
C      IER       I          error in CATDIR or in CATIO.
C-----------------------------------------------------------------------
      CHARACTER NMI*12, CLI*6, NMJ*12, CLJ*6, STAT*4, TYPTMP*2
      INTEGER   IV, CN, IS, UID, SCR(256), IER, CAT1(256), JV, JS,
     *   CAT2(256), INF, NDIM, I, J, JRV, JIC, JRP
      REAL      CAT1R(256), CAT2R(256), EPS
      DOUBLE PRECISION    CAT1D(128), CAT2D(128), RFJ
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE (CAT1, CAT1R, CAT1D)
      EQUIVALENCE (CAT2, CAT2R, CAT2D)
      DATA EPS /0.001/
C-----------------------------------------------------------------------
C                                       CATBLK for first file
      TYPTMP = 'MA'
      CALL CATDIR ('SRNH', IV, CN, NMI, CLI, IS, TYPTMP, UID, STAT, SCR,
     *    IER)
      IF (IER.NE.0) GO TO 999
      CALL CATIO ('READ', IV, CN, CAT1, 'REST', SCR, IER)
      IF (IER.NE.0) GO TO 999
C                                       CATBLK for second file
      TYPTMP = 'MA'
      CALL CATDIR ('SRNH', JV, CN, NMJ, CLJ, JS, TYPTMP, UID, STAT, SCR,
     *    IER)
      IF (IER.NE.0) GO TO 999
      CALL CATIO ('READ', JV, CN, CAT2, 'REST', SCR, IER)
      IF (IER.NE.0) GO TO 999
C                                       same dimensions ?
      IF (CAT1(KIDIM).EQ.CAT2(KIDIM)) GO TO 10
         INF = 1
         GO TO 999
C                                       same # points per axis ?
 10   NDIM = CAT1(KIDIM)
      DO 20 I = 1,NDIM
         IF (CAT1(KINAX+I-1).EQ.CAT2(KINAX+I-1)) GO TO 20
            INF = 2
            GO TO 999
 20      CONTINUE
C                                       same pixel increments  ?
      DO 30 I = 1,NDIM
         J = KRCIC + I - 1
         IF (ABS (CAT1R(J)-CAT2R(J)).LE.EPS*ABS (CAT1R(J))) GO TO 30
            INF = 3
            GO TO 999
 30      CONTINUE
C                                       coincident ?
      DO 40 I = 1,NDIM
         JRV = KDCRV + I - 1
         JIC = KRCIC + I - 1
         JRP = KRCRP + I - 1
         RFJ = CAT1D(JRV) + CAT1R(JIC) * (CAT2R(JRP) - CAT1R(JRP))
         IF (ABS (CAT2D(JRV)-RFJ).LE.EPS*ABS (CAT2R(JIC))) GO TO 40
            INF = 4
            GO TO 999
 40      CONTINUE
C                                       yes, coincident
      INF = 0
C
 999  RETURN
      END
