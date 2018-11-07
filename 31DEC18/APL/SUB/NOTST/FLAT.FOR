      SUBROUTINE FLAT (LEVEL, OUT, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine: initialize an image to a value.
C# IO-util
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
C   FLAT initializes a map to LEVEL
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   OUT
      REAL      LEVEL
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C
      IERR = 0
      IF (AKOPEN (OUT, 1, 'WRIT', BUFFR1).NE.0) GO TO 990
C                                        Loop thru map
      DO 20 IY = 1,HNY
         IF (AKCESS (OUT, BUFFR1).NE.0) GO TO 990
C                                        Calculate the function
         CALL RFILL (HNX, 0.0, BUFFR1(BIND(1)))
         IF ((IY.LT.YBEG) .OR. (IY.GT.YEND)) GO TO 20
            I1 = BIND(1) + XBEG - 1
            DO 10 IX = XBEG,XEND
               BUFFR1(I1) = LEVEL
               I1 = I1 + 1
 10            CONTINUE
 20      CONTINUE
C
      IF (AKCLOS (OUT, BUFFR1).NE.0) GO TO 990
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLAT')
      END
