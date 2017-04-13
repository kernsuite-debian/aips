      SUBROUTINE TVVECT (INT, CHAN, COUNT, XDATA, YDATA, SCRTCH, IERR)
C-----------------------------------------------------------------------
C! draws connected line segments on TV
C# TV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   TVVECT writes a connected sequence of line segments on a TV channel
C   calling YCNECT
C   Inputs:
C      INT     I          intensity to draw - should be 0-MAXINT in
C                         channels 1-NGRAY, 2**NGRAPH-1 in rest
C      CHAN    I          channel number (1 to NGRAY+NGRAPH)
C      COUNT   I          number of X,Y pairs ( > 1)
C      XDATA   I(COUNT)   X coordinates X1,X2,...
C      YDATA   I(COUNT)   Y coordinates Y1,Y2,...
C   Output:
C      SCRTCH  I(*)    scratch buffer (size MAXXTV)
C      IERR    I       error code of ZM70XF - 0 => ok;  2 => input error
C-----------------------------------------------------------------------
      INTEGER   INT, CHAN, COUNT, XDATA(*), YDATA(*), SCRTCH(*), IERR
C
      INTEGER   IDX, NWORDS, X, Y, X1, Y1, IER, I
      REAL      RX, RY, R, RM
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
C                                       check inputs
      IERR = 2
      IF ((CHAN.LT.1) .OR. (CHAN.GT.NGRAY+NGRAPH)) GO TO 999
      IF (COUNT.LT.2) GO TO 999
      IERR = 0
      RM = 0.0
      DO 10 I = 1,COUNT-1
         RX = XDATA(I+1) - XDATA(I)
         RY = YDATA(I+1) - YDATA(I)
         R = SQRT (RX*RX + RY*RY)
         RM = MAX (RM, R)
 10      CONTINUE
      IDX = RM + 1.0
C                                       init buffer values
      IF ((SCRTCH(1).NE.INT) .OR. (SCRTCH(IDX).NE.INT)) THEN
         DO 15 I = 1,IDX
            SCRTCH(I) = INT
 15         CONTINUE
         END IF
      IDX = 1
      NWORDS = COUNT
C                                       first point in a line
 20   IF (NWORDS.LT.IDX+1) GO TO 999
         X = XDATA(IDX)
         Y = YDATA(IDX)
         IDX = IDX + 1
         IF ((X.GE.1) .AND. (X.LE.MAXXTV(1)) .AND. (Y.GE.1)
     *      .AND. (Y.LE.MAXXTV(2))) GO TO 30
            IERR = 2
            GO TO 20
C                                       second point in a line
 30      IF (NWORDS.LT.IDX) GO TO 999
            X1 = X
            Y1 = Y
            X = XDATA(IDX)
            Y = YDATA(IDX)
            IDX = IDX + 1
            IF ((X.GE.1) .AND. (X.LE.MAXXTV(1)) .AND. (Y.GE.1)
     *         .AND. (Y.LE.MAXXTV(1))) GO TO 40
               IERR = 2
               GO TO 20
C                                       draw a line segment
 40         CALL YCNECT (X1, Y1, X, Y, CHAN, SCRTCH, IER)
            IF (IER.EQ.0) GO TO 30
C
      IERR = MAX (IERR, IER)
C
 999  RETURN
      END
