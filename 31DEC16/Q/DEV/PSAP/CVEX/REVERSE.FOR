        SUBROUTINE REVERSE (K, I, ORDER)
C-----------------------------------------------------------------------
C! Convex Pseudo AP routine: Bit reversal routine.
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
        INTEGER TEMP(15), I, J, K, ORDER
C-----------------------------------------------------------------------
C                                       Take the input mask out the
C                                       least significant bit and test
C                                       and store the result in temp of
C                                       j for later bit reversal.
        K = I
        DO 10 J = 1,ORDER
           IF (K .AND. 1) THEN
              TEMP(J) = 1
           ELSE
              TEMP(J) = 0
              END IF
C                                       Shift to the next bit, right.
              K = K/2
 10        CONTINUE
C                                       read temp in the same order as
C                                       written but shift left now.
        K = 0
        DO 20 J = 1,ORDER
           K = K+K
           K = TEMP(J) .OR. K
 20        CONTINUE
 999    RETURN
        END
