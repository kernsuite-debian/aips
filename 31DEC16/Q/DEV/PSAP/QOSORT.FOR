      SUBROUTINE QOSORT (A, N, NP2, KEY1, KEY2, LEN, WK, IP, IER)
C-----------------------------------------------------------------------
C! does quick sort on array of vectors, then reorders by calling QPERMA
C# Sort UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2015
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
C   OSORT does a quick sort on the keys and a permutation vector and
C   then reorders the data with QPERMA.  Data sorted into descending
C   order of the keys.
C   Input:
C      A     R(LEN,N)   Array of data to be sorted.
C      N     I         Number of records to be sorted
C      NP2   I         Number of words in work arrays = N+2
C      KEY1  I         Word in record of slower varying key
C      KEY2  I         Word in record of faster varying key
C      LEN   I         Length of record in R words (must be .LE. 2048)
C   Output:
C      WK    R(2,NP2)  Work buffer for keys.
C      IP    I(NP2)    Work buffer for permutation vector.
C      A     R(LEN,N)  Array of data sorted.
C      IER   I         Error code,   0 => OK
C                                    1 => input error (LEN > 2048)
C   Adapted from ACM alg. #347 - June 1981
C-----------------------------------------------------------------------
      INTEGER   N, NP2, KEY1, KEY2, LEN, IER
      LONGINT   IP(NP2)
      DOUBLE PRECISION A(LEN,N), WK(2,NP2)
C
      INTEGER   TP, TTP, IU(100), IL(100), I, IJ, J, K, KK, L, M
      REAL      T(2), TT(2)
C-----------------------------------------------------------------------
C                                       Check length
      IER = 1
      IF (LEN.GT.2048) GO TO 999
      IER = 0
C                                       Build array of keys to sort
C                                       Minus sign makes order descend.
C                                       Note "blockers" put at ends.
      DO 5 KK = 1,N
         I = KK + 1
         IP(I) = KK
         WK(1,I) = -A(KEY1,KK)
         WK(2,I) = -A(KEY2,KK)
 5       CONTINUE
      WK(1,1) = -1.0E20
      WK(1,N+2) = 1.E20
      M = 1
      I = 2
      J = N + 1
C                                       Sort subgroup
 10   CONTINUE
         IF (I.GE.J) GO TO 240
C
 20      CONTINUE
            K = I
            IJ = (J+I)/2
            T(1) = WK(1,IJ)
            T(2) = WK(2,IJ)
            TP = IP(IJ)
            IF (WK(1,I)-T(1)) 70,40,50
 40            IF (WK(2,I).LE.T(2)) GO TO 70
 50               DO 60 KK = 1,2
                     WK(KK,IJ) = WK(KK,I)
                     WK(KK,I) = T(KK)
                     T(KK) = WK(KK,IJ)
 60                  CONTINUE
                  IP(IJ) = IP(I)
                  IP(I) = TP
                  TP = IP(IJ)
C
 70         L = J
            IF (WK(1,J)-T(1)) 90,80,160
 80            IF (WK(2,J).GE.T(2)) GO TO 160
 90               DO 100 KK = 1,2
                     WK(KK,IJ) = WK(KK,J)
                     WK(KK,J) = T(KK)
                     T(KK) = WK(KK,IJ)
 100                 CONTINUE
                  IP(IJ) = IP(J)
                  IP(J) = TP
                  TP = IP(IJ)
                  IF (WK(1,I)-T(1)) 160,110,120
 110                 IF (WK(2,I).LE.T(2)) GO TO 160
 120                    DO 130 KK = 1,2
                           WK(KK,IJ) = WK(KK,I)
                           WK(KK,I) = T(KK)
                           T(KK) = WK(KK,IJ)
 130                       CONTINUE
                        IP(IJ) = IP(I)
                        IP(I) = TP
                        TP = IP(IJ)
                        GO TO 160
C
 140                 CONTINUE
                        DO 150 KK = 1,2
                           WK(KK,L) = WK(KK,K)
                           WK(KK,K) = TT(KK)
 150                       CONTINUE
                        IP(L) = IP(K)
                        IP(K) = TTP
C                                       Put substring on stack
 160        L = L - 1
               IF (L.GT.0) GO TO 165
                  IER = 3
                  GO TO 999
 165           IF (WK(1,L)-T(1)) 180,170,160
 170           IF (WK(2,L).GT.T(2)) GO TO 160
 180              TT(1) = WK(1,L)
                  TT(2) = WK(2,L)
                  TTP = IP(L)
 200              K = K + 1
                     IF (WK(1,K)-T(1)) 200,210,220
 210                 IF (WK(2,K).LT.T(2)) GO TO 200
 220                      IF (K.LE.L) GO TO 140
                              IF (L-I.LE.J-K) GO TO 230
                                 IL(M) = I
                                 IU(M) = L
                                 I = K
                                 M = M + 1
                                 GO TO 250
 230                          CONTINUE
                                 IL(M) = K
                                 IU(M) = J
                                 J = L
                                 M = M + 1
                                 GO TO 250
C                                       Next substring from stack
 240        CONTINUE
               M = M - 1
               IF (M.EQ.0) GO TO 900
               I = IL(M)
               J = IU(M)
C                                       Straight insertion sort done
C                                       if <= 10 elements in group
 250        IF (J-I.GE.11) GO TO 20
         IF (I.EQ.2) GO TO 10
      I = I-1
 260  I = I+1
         IF (I.EQ.J) GO TO 240
            T(1) = WK(1,I+1)
            T(2) = WK(2,I+1)
            TP = IP(I+1)
            IF (WK(1,I)-T(1)) 260,280,290
 280           IF (WK(2,I).LE.T(2)) GO TO 260
 290              K = I
C
 300              CONTINUE
                     WK(1,K+1) = WK(1,K)
                     WK(2,K+1) = WK(2,K)
                     IP(K+1) = IP(K)
                     K = K-1
                     IF (K.GT.0) GO TO 310
                        IER = 4
                        GO TO 999
 310                 IF (T(1)-WK(1,K)) 300,320,330
 320                    IF (T(2).LE.WK(2,K)) GO TO 300
 330                       WK(1,K+1) = T(1)
                           WK(2,K+1) = T(2)
                           IP(K+1) = TP
                           GO TO 260
C                                       Permute full matrix finally
 900  CONTINUE
         CALL QPERMA (A, LEN, N, LEN, 0, IP(2), WK)
C
 999  RETURN
      END
