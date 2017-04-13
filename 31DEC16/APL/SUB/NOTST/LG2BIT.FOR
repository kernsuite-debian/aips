      SUBROUTINE LG2BIT (N, LGA, BITA, IDIR)
C-----------------------------------------------------------------------
C! Converts between bit arrays and logical arrays
C# Math
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
C      Given an N-element logical array, LGA, LG2BIT encodes the
C  logical flags as bit flags packed within an M-element integer
C  array, where M = ceil(N/NBITWD); except that when the direction
C  flag, IDIR, is nonpositive, the inverse operation is performed.
C  Input:
C      N     I    Number of flags
C      IDIR  I     Direction flag:  when IDIR>0 an array of
C                  packed bit flags is produced from an array
C                  of logical flags; otherwise an array of logical
C                  flags is produced from an array of packed bit flags.
C  Input/Output (depending on IDIR):
C      LGA   L(N)     Logical array
C      BITA  I(ceil(N/NBITWD))     Integer array of packed bit flags.
C-----------------------------------------------------------------------
      INTEGER   N, BITA(1), IDIR, BITS(64), K, KK, I, M
      LOGICAL   LGA(N), T, F
      INCLUDE 'INCS:DDCH.INC'
      DATA T, F /.TRUE., .FALSE./
C-----------------------------------------------------------------------
      M = (N-1)/NBITWD+1
      KK = 0
      IF (IDIR.LE.0) GO TO 40
C                                       Pack logical to bit array
         DO 30 K = 1,M
            DO 10 I = 1,NBITWD
               KK = KK+1
               BITS(I) = 0
               IF (LGA(KK)) BITS(I) = 1
               IF (KK.GE.N) GO TO 20
 10            CONTINUE
 20         CALL ZPTBIT (NBITWD, BITA(K), BITS)
 30         CONTINUE
         GO TO 999
C                                       Unpack bits to logical array.
 40   CONTINUE
      DO 60 K = 1,M
         CALL ZGTBIT (NBITWD, BITA(K), BITS)
         DO 50 I = 1,NBITWD
            KK = KK+1
            LGA(KK) = F
            IF (BITS(I).EQ.1) LGA(KK) = T
            IF (KK.EQ.N) GO TO 999
 50         CONTINUE
 60      CONTINUE
C
 999  RETURN
      END
