      SUBROUTINE SMTOBG (IN, INDEF, OUT, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine: Copies small image to a large one.
C# IO-util Map
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
C SMTOBG copies a small size scratch file (IN) to a large size one (OUT)
C     Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER   IN, OUT
      REAL      INDEF
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
      IF (AKOPEN (IN, 1, 'READ', BUFFR1) .NE.0) GO TO 990
      IF (AKOPEN (OUT, 2, 'WRIT', BUFFR2) .NE.0) GO TO 990
C                                        Loop thru map
      DO 110 IY = 1,NY
         IF (AKCESS (OUT, BUFFR2) .NE.0) GO TO 990
         CALL RFILL (NX, INDEF, BUFFR2(BIND(2)))
         IF ((IY.LT.BLC(2)) .OR. (IY.GT.TRC(2))) GO TO 110
            IF (AKCESS (IN, BUFFR1) .NE.0) GO TO 990
            I1 = BIND(1)
            I2 = BIND(2) + BLC(1) - 1
            DO 105 IX = XBEG, XEND
               BUFFR2(I2) = BUFFR1(I1)
               I1 = I1 + 1
               I2 = I2 + 1
 105           CONTINUE
 110     CONTINUE
C
      IF (AKCLOS (IN, BUFFR1) .NE.0) GO TO 990
      IF (AKCLOS (OUT, BUFFR2) .NE.0) GO TO 990
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('SMTOBG')
      END
