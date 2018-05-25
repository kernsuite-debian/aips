      SUBROUTINE WINDOW (NAX, NPTS, BLC, TRC, IERR)
C-----------------------------------------------------------------------
C! translates user BLC, TRC parameters into usable window arrays
C# Map-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2006, 2012
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
C   WINDOW interprets user window BLC, TRC into usable values
C   Catches BLC = const & corrects
C   Inputs:
C      NAX   I        Number of axes
C      NPTS  I(7)     Number of points / axis
C   In/out:
C      BLC   R(7)     Bottom left corner
C      TRC   R(7)     Top right corner
C   Output:
C      IERR  I        Number of bad axes (0 => ok)
C-----------------------------------------------------------------------
      INTEGER    NAX, NPTS(7), IERR
      REAL       BLC(7), TRC(7)
C
      LOGICAL    BEQU
      INTEGER    I, N, J
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        BLC = const (all same?)
      BEQU = .TRUE.
      DO 10 I = 1,7
         IF (BLC(I).NE.BLC(1)) BEQU = .FALSE.
 10      CONTINUE
C                                        Set null axes
      IF (NAX.GE.7) GO TO 20
         N = NAX + 1
         DO 15 I = N,7
            BLC(I) = 1.0
            TRC(I) = 1.0
 15         CONTINUE
C                                        In range: set,check
 20   IERR = 0
      DO 30 I = 1,NAX
         IF (NPTS(I).LE.1) BLC(I) = 1.0
         IF (NPTS(I).LE.1) TRC(I) = 1.0
         IF (TRC(I).LE.0.89) TRC(I) = NPTS(I)
         IF (TRC(I).GT.NPTS(I)) TRC(I) = NPTS(I)
         IF (BLC(I).LT.1.0) BLC(I) = 1.0
         IF ((BEQU) .AND. (I.GT.2) .AND. (BLC(I).GT.TRC(I)))
     *      BLC(I) = 1.0
         IF (BLC(I).GT.TRC(I)) IERR = IERR + 1
 30      CONTINUE
      IF (IERR.EQ.0) GO TO 40
         MSGTXT = 'REQUESTED SUBIMAGE AT LEAST PARTLY OUTSIDE' //
     *      ' THE IMAGE:'
         CALL MSGWRT (7)
         WRITE (MSGTXT,1030) 'BLC', BLC
         CALL MSGWRT (7)
         WRITE (MSGTXT,1030) 'TRC', TRC
         CALL MSGWRT (7)
C                                       round off to integer
 40   DO 45 I = 1,2
         J = BLC(I) + 0.01
         BLC(I) = J
         J = TRC(I) + 0.99
         TRC(I) = J
 45      CONTINUE
      IF (NAX.LT.3) GO TO 999
         DO 50 I = 3,NAX
            J = BLC(I) + 0.5
            BLC(I) = J
            J = TRC(I) + 0.5
            TRC(I) = J
 50         CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1030 FORMAT ('INCORRECT ',A,1X,7F7.0)
      END
