      SUBROUTINE MOM (ARA, X, Y, NX, NY, MOMAR, IERR)
C-----------------------------------------------------------------------
C! calculates moments in a 16x16 data array
C# Modeling
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
C   Calculate all 0th, 1st, and 2nd moments of a NX*NY subarray of ARA,
C   centered at X,Y.  NX and NY should be odd.
C   Inputs:
C      ARA     R(16,16)  Input data array
C      X       R         x-center for moment calculation
C      Y       R         y-center
C      NX      I         # of points to include in x-direction. NX
C                        should be odd.  The points will be centered
C                        about X (rounded)
C      NY      I         # of points in y-direction
C   Outputs:
C      MOMAR   R(6)      00,10,20,01,11,02 yx-moments of ARA
C      IERR    I         0 => o.k.
C                        1 => subarray doesn't fit in main array
C-----------------------------------------------------------------------
      REAL       ARA(16,16), MOMAR(6), X, Y
      INTEGER    NX, NY, IERR
C
      INTEGER    IND, I, J, NJ, INDX, INDY, IAX1, IAX2, IAY1, IAY2
      REAL       S, T
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
C                                       compute loop limits
      I = X + 0.5
      IAX1 = I - NX/2
      IAX2 = IAX1 + NX - 1
      I = Y + 0.5
      IAY1 = I - NY/2
      IAY2 = IAY1+ NY - 1
C                                       check loop limits
      IERR = 1
      IF((IAX1.LT.1) .OR. (IAX2.GT.16) .OR. (IAY1.LT.1) .OR.
     *   (IAY2.GT.16)) GO TO 999
C                                       compute moments
      IERR = 0
      IND = 0
      DO 40 I = 1,3
         NJ = 4 - I
         DO 30 J = 1,NJ
            IND = IND + 1
            S = 0.0
            DO 20 INDX = IAX1,IAX2
               DO 10 INDY = IAY1,IAY2
                  T = ARA(INDX, INDY)
                  IF (T.EQ.FBLANK) GO TO 10
                     IF (I.GT.1) T = T * ((INDX - X)**(I-1))
                     IF (J.GT.1) T = T * ((INDY - Y)**(J-1))
                     S = S + T
 10               CONTINUE
 20            CONTINUE
            MOMAR(IND) = S
 30         CONTINUE
 40      CONTINUE
C
 999  RETURN
      END
