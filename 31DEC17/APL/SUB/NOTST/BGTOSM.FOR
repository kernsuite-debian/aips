      SUBROUTINE BGTOSM (IN, OUT, IERR)
C-----------------------------------------------------------------------
C! *TESS routine to copy a subset of a large image to a small one
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
C BGTOSM copies a large size scratch file (IN) to a small size one (OUT)
C   Programmer =  T.J. Cornwell      December 1987
C-----------------------------------------------------------------------
      INTEGER    IN, OUT
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
      IERR = 0
C
      IF (AKOPEN (IN,1,'READ',BUFFR1).NE.0) GO TO 990
      IF (AKOPEN (OUT,2,'WRIT',BUFFR2).NE.0) GO TO 990
C                                        Loop thru map
      DO 145 IY = 1, HNY
         IF (AKCESS(OUT,BUFFR2).NE.0) GO TO 990
         CALL RFILL (HNX, 0.0, BUFFR2(BIND(2)))
         IF ((IY.LT.YBEG).OR.(IY.GT.YEND)) GO TO 145
            IF (AKCESS(IN,BUFFR1).NE.0) GO TO 990
            I1 = BIND(1)
            I2 = BIND(2) + XBEG - 1
            DO 140 IX = BLC(1), TRC(1)
               BUFFR2(I2) = BUFFR1(I1)
               I1 = I1 + 1
               I2 = I2 + 1
 140           CONTINUE
 145     CONTINUE
C
      IF (AKCLOS (IN,BUFFR1).NE.0) GO TO 990
      IF (AKCLOS (OUT,BUFFR2).NE.0) GO TO 990
C
      GO TO 999
C
 990  WRITE (MSGTXT,1000)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('BGTOSM')
      END
