      SUBROUTINE PASENC (IPAS, OPAS)
C-----------------------------------------------------------------------
C! encrypts a 12-character password into 3 Holleriths
C# System
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
C   PASENC encrypts a password string
C   Input:
C      IPAS   C*12   user-typed password (clear characters)
C   Output:
C      OPAS   H(3)   encode password
C   NOTE: the current encryption scheme is very elementary and should be
C   replaced.
C-----------------------------------------------------------------------
      CHARACTER IPAS*12
      HOLLERITH OPAS(3)
      INTEGER   J1, J2, IT, LT, I, NP, IOFF, J, I1, I2, II, BITS(16)
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      NP = 2 ** (NBITCH-2)
      DO 50 I = 1,12
C                                       get character
         J1 = (I-1) / 4 + 1
         J2 = I - 4 * (J1-1)
         I1 = J1 + J2 - 2
         IT = ICHAR (IPAS(I:I))
C                                       swap some bits
         LT = IT / 4
         LT = LT + (IT - 4 * LT) * NP
C                                       get output char position
         IF (I.EQ.1) IOFF = (2 * LT) / NP
         J = MOD (I+IOFF-1, 12) + 1
         J1 = (J-1) / 4 + 1
         J2 = J - 4 * (J1-1)
         J = (I1 + J1 + J2 - 2) / 2
C                                       play with bits
         CALL ZGTBIT (NBITCH, LT, BITS)
         DO 10 II = 1,NBITCH
            I2 = MOD (II+J-1, NBITCH) + 1
            IF (I2.GE.II) GO TO 10
               I1 = BITS(II)
               BITS(II) = BITS(I2)
               BITS(I2) = I1
 10         CONTINUE
         DO 20 II = 1,NBITCH
            I2 = MOD (2*II-J-1+NBITCH, NBITCH) + 1
            IF (I2.GE.II) GO TO 20
               I1 = BITS(II)
               BITS(II) = BITS(I2)
               BITS(I2) = I1
 20         CONTINUE
         DO 30 II = 1,NBITCH
            I2 = MOD (3*II-J-1+NBITCH, NBITCH) + 1
            IF (I2.GE.II) GO TO 30
               I1 = BITS(II)
               BITS(II) = BITS(I2)
               BITS(I2) = I1
 30         CONTINUE
C                                       pack the bits, store
         CALL ZPTBIT (NBITCH, LT, BITS)
         CALL ZPUTCH (LT, OPAS(J1), J2)
 50      CONTINUE
C
 999  RETURN
      END
