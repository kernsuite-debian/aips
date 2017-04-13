      SUBROUTINE BOUNDS (A1, A2, B, IERR)
C-----------------------------------------------------------------------
C! prints message if 1 or 2 values are outside a specified range
C# Utility
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
C   This routine prints a warning message if an input value is below the
C   minimum or above a maximum value.
C   Input:  A1     R     Value to check for proper range.
C           A2     R     Value to check for proper range.
C           B(1)   R     Minimum allowed value.
C           B(2)   R     Maximum allowed value.
C   Output: IERR   I     Error: 0 ok, 1 A1 bad, 2 A2 bad, 3 both bad
C-----------------------------------------------------------------------
      REAL      A1, A2, B(2), TL, TH
      INTEGER   IERR
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      IERR = 0
      TL = 1.0E-5
      IF ((B(1).NE.0.0) .AND. (ABS(B(1)).LT.0.01)) TL = TL * ABS(B(1))
      TH = 1.0E-5
      IF ((B(2).NE.0.0) .AND. (ABS(B(2)).LT.0.01)) TH = TH * ABS(B(2))
C                                        A1 too small
      IF (A1-B(1).GE.-TL) GO TO 10
         IERR = 1
         WRITE (MSGTXT,1000) A1, B(1)
         IF (ABS(B(1)).LT.0.005) WRITE (MSGTXT,1001) A1, B(1)
         CALL MSGWRT (6)
         GO TO 20
C                                        A1 too big
 10   IF (A1-B(2).LT.TH) GO TO 20
         IERR = 1
         WRITE (MSGTXT,1010) A1, B(2)
         IF (ABS(B(2)).LT.0.005) WRITE (MSGTXT,1011) A1, B(2)
         CALL MSGWRT (6)
C                                        A2 too small
 20   IF (A1.EQ.A2) GO TO 999
      IF (A2-B(1).GE.-TL) GO TO 30
         IERR = 2 + IERR
         WRITE (MSGTXT,1000) A2, B(1)
         IF (ABS(B(1)).LT.0.005) WRITE (MSGTXT,1001) A2, B(1)
         CALL MSGWRT (6)
         GO TO 999
C                                        A2 too big
 30   IF (A1-B(2).LT.TH) GO TO 999
         IERR = 2 + IERR
         WRITE (MSGTXT,1010) A2, B(2)
         IF (ABS(B(2)).LT.0.005) WRITE (MSGTXT,1011) A2, B(2)
         CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('******  VALUE =',F12.4,' BELOW LIMIT',F12.4)
 1001 FORMAT ('******  VALUE =',1PE12.4,' BELOW LIMIT',1PE12.4)
 1010 FORMAT ('******  VALUE =',F12.4,' EXCEEDS LIMIT',F12.4)
 1011 FORMAT ('******  VALUE =',1PE12.4,' EXCEEDS LIMIT',1PE12.4)
      END
