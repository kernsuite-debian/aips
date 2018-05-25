@PROCESS VECTOR DIR('@DIR')
      SUBROUTINE QVTRAN (M, N, IAD, LV)
C-----------------------------------------------------------------------
C! Pseudo AP routine: Inplace transpose of a matrix of vectors.
C# AP-appl
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
C   Vectorizing compiler version
C   VTRANS transposes a (row-stored) M X N array of row vectors
C   of length LV.  The starting address is given by IAD.  The
C   algorithm works in place.  It is adapted from Boothroyd's
C   CACM ALG.#302.  Other, probably better, algorithms, are
C   CACM #'S 380 and 467, but they're not as simple to program.
C   Inputs:
C      M   I  First dimension of the vector array
C      N   I  Second dimension of the vector array
C      IAD I  Base address of the array
C      LV  I  Length of the vectors.
C-----------------------------------------------------------------------
      INTEGER   I, IAD, IADI, IADJ, IDN, IL1, J, K, LV, M, MDMN, MNM1,
     *   N, JN, J4, DLV, LOOP, KK
      REAL      TEMP
      INCLUDE 'INCS:DAPC.INC'
C-----------------------------------------------------------------------
C                                       Setup
      DLV = LV
      MNM1 = M * N - 1
      MDMN = MNM1 - N
      IDN = MNM1 - 1
      IF ((LV.LE.0) .OR. (IDN.LT.2)) GO TO 999
      K = 0
      IL1 = 1
C                                       Loop
C@DIR IGNORE RECRDEPS(APCORE)
      DO 80 I = 2,IDN
         KK = K + N
         IF (K.GT.MDMN) KK = K - MDMN
         K = KK
 40      J = K
 50      IF (J.GE.IL1) GO TO 60
C                                       J = MOD (J*N, MNM1)
            J = J * N - (J * N / MNM1) * MNM1
            GO TO 50
 60      IF (J.EQ.IL1) GO TO 75
            J = J + 1
            IADI = IAD + (I-1) * DLV
            IADJ = IAD + (J-1) * DLV
C                                       Swap rows
C@DIR IGNORE RECRDEPS(APCORE)
            DO 70 LOOP = 1,LV
               TEMP = APCORE(IADI+LOOP)
               APCORE(IADI+LOOP) = APCORE(IADJ+LOOP)
               APCORE(IADJ+LOOP) = TEMP
 70            CONTINUE
 75      IL1 = I
 80      CONTINUE
C
 999  RETURN
      END
