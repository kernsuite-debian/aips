      SUBROUTINE QXFOR2 (IPASS, ISEP, AINDEX, BINDEX, COSII, SINII, W)
C-----------------------------------------------------------------------
C! Pseudo AP routine: FFT utility routine.
C# AP-FFT
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
C   This program does as section of a FFT but avoids multiplication when
C   addition will do in the cos and sin series
C   Inputs:
C      IPASS   I     Pass number in fft loop
C      ISEP    I     Number of element processed in this pass
C      AINDEX  I     Index for Butterfly  fft loop
C      BINDEX  I     Index for Butterfly  fft loop
C      COSII   R     Cosine term for fft loop
C      SINII   R     Sine   term for fft loop
C   Input/Output:
C      W     R(2,M)  Complex work array.
C-----------------------------------------------------------------------
      INTEGER   IPASS, ISEP, AINDEX, BINDEX
      REAL      COSII, SINII, W(2,*)
      REAL      NEAR1
      PARAMETER (NEAR1=0.999999)
C
      INTEGER   J, JI, JI1
      REAL      AR, AI
C-----------------------------------------------------------------------
C                                       init indexes
      JI  = AINDEX
      JI1 = BINDEX
C                                       deterime odd or even pass
      IF (IPASS.NE.(2*(IPASS/2))) THEN
C                                       Odd pass
C                                       if special case cos=1
         IF (COSII.GT.NEAR1) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 100 J = 1,ISEP
               AR = W(1,JI)
               AI = W(2,JI)
               W(1,JI) =  W(1,JI1) - AR
               W(2,JI) =  W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) + AR
               W(2,JI1) = W(2,JI1) + AI
               JI1 = JI1 + 1
               JI = JI + 1
 100           CONTINUE
C                                       if special case sin=1
         ELSE IF (SINII.GT.NEAR1) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 150 J = 1,ISEP
               AR = -W(2,JI)
               AI =  W(1,JI)
               W(1,JI) =  W(1,JI1) - AR
               W(2,JI) =  W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) + AR
               W(2,JI1) = W(2,JI1) + AI
               JI1 = JI1 + 1
               JI = JI + 1
 150           CONTINUE
         ELSE
C                                       else normal case multiply
      INCLUDE 'INCS:ZVND.INC'
            DO 200 J = 1,ISEP
               AR = W(1,JI) * COSII - W(2,JI) * SINII
               AI = W(1,JI) * SINII + W(2,JI) * COSII
               W(1,JI) =  W(1,JI1) - AR
               W(2,JI) =  W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) + AR
               W(2,JI1) = W(2,JI1) + AI
               JI1 = JI1 + 1
               JI = JI + 1
 200           CONTINUE
C                                       end if normal case
            END IF
C                                       else Even pass
      ELSE
C                                       If special cos=1 case
         IF (COSII.GT.NEAR1) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 300 J = 1,ISEP
               AR = W(2,JI)
               AI = W(1,JI)
               W(1,JI) =  W(1,JI1) + AR
               W(2,JI) =  W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) - AR
               W(2,JI1) = W(2,JI1) + AI
               JI1 = JI1 + 1
               JI = JI + 1
 300           CONTINUE
C                                       if special case sin=1
         ELSE IF (SINII.GT.NEAR1) THEN
      INCLUDE 'INCS:ZVND.INC'
            DO 350 J = 1,ISEP
               AR =  W(1,JI)
               AI = -W(2,JI)
               W(1,JI) =  W(1,JI1) + AR
               W(2,JI) =  W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) - AR
               W(2,JI1) = W(2,JI1) + AI
               JI1 = JI1 + 1
               JI = JI + 1
 350           CONTINUE
C                                       else normal case, multiply
         ELSE
      INCLUDE 'INCS:ZVND.INC'
            DO 400 J = 1,ISEP
               AR = W(1,JI) * SINII + W(2,JI) * COSII
               AI = W(1,JI) * COSII - W(2,JI) * SINII
               W(1,JI) =  W(1,JI1) + AR
               W(2,JI) =  W(2,JI1) - AI
               W(1,JI1) = W(1,JI1) - AR
               W(2,JI1) = W(2,JI1) + AI
               JI1 = JI1 + 1
               JI = JI + 1
 400           CONTINUE
C                                       end if normal case
            END IF
C                                       end if even pass
         END IF
C
      RETURN
      END






