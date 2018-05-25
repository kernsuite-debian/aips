      SUBROUTINE FNDVAR (IFIELD, WTIN, IERR)
C-----------------------------------------------------------------------
C! Tim Cornwell routine: Convert errors in Jy/beam to Jy per cell
C# Math
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2003
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
C   Convert errors in Jy/beam into Jy/cell for interferometer images
C
C   Programmer =  T.J. Cornwell      October 1985
C-----------------------------------------------------------------------
      INTEGER   WTIN, IFIELD, JLIM
      REAL      RSWT, RSWTSQ, RNXY, RTEMP
      INTEGER   AKOPEN, AKCESS, AKCLOS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DTESS.INC'
      INCLUDE 'INCS:DTCIO.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C
      IERR = 0
C
      IF (NY.GE.24096) THEN
         RVAR(IFIELD) = 10.0
         MSGTXT = 'FNDVAR: NY >= 24096!: USING NPOINTS = 10.0'
         CALL MSGWRT (4)
         GO TO 999
         END IF
C
      RSWT   = 0.0
      RSWTSQ = 0.0
      RNXY = REAL(NX)*REAL(NY)
C
      IF (AKOPEN (WTIN, 1, 'READ', BUFFR1).NE.0) GO TO 990
C                                       Begin loop thru map.
      JLIM = NX / 2 + 1
      DO 125 IY = 1,JLIM
         IF (AKCESS(WTIN, BUFFR1).NE.0) GO TO 990
         I1 = BIND(1)
      INCLUDE 'INCS:ZVND.INC'
         DO 120 IX = 1,NY
            RTEMP = BUFFR1(I1)**2 + BUFFR1(I1+1)**2
            RSWT   = RSWT   + SQRT(RTEMP)
            RSWTSQ = RSWTSQ + RTEMP
            I1 = I1 + 2
 120        CONTINUE
 125     CONTINUE
      IF (AKCLOS (WTIN, BUFFR1).NE.0) GO TO 990
C
C
      RSWTSQ = RNXY * RSWTSQ
      RVAR(IFIELD) = SQRT (RSWTSQ / RSWT**2)
C
      GO TO 999
C
 990  WRITE (MSGTXT,1100)
      CALL MSGWRT (8)
      IERR = 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('FNDVAR')
      END
