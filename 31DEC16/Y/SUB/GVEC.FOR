      SUBROUTINE GVEC (X, Y, BUFF, IERR)
C-----------------------------------------------------------------------
C! writes a move-pen-down (or write vector) command in a plot file
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002, 2006
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
C   GVEC will write a 'write vector' command record to a graph file or
C   the TV (in immediate TV mode).
C   The output record description is:
C            I     opcode, 5 in this program.
C            I     scaled X position.
C            I     scaled Y position.
C   Inputs:
C      X      R       x position
C      Y      R       y position.
C      BUFF   I(256)  buffer to use for I/O.
C   Output:
C      IERR   I       error code. 0 = ok.
C                        1 = disk problems.
C                        2 = TV problems
C   Common:
C      GPHPOS  incremented by 3.
C      GPHRRN  incremented by 1 if a write to disk is needed.
C-----------------------------------------------------------------------
      REAL      X, Y
      INTEGER   BUFF(256), IERR
C
      REAL      RX, RY, RLIM, ALPHA, BETA
      INTEGER   NEED, OPCODE, IER, IROUND, IXN, IYN, X1, X2, Y1, Y2
      LOGICAL   ISERR
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      DATA NEED, OPCODE /254, 5/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Scale X and Y.
      IF (GPHIX2.EQ.GPHIX1) THEN
         RX = 0.0
      ELSE
         RX = (X - GPHX1) / (GPHX2 - GPHX1)
         END IF
      IF (GPHIY2.EQ.GPHIY1) THEN
         RY = 0.0
      ELSE
         RY = (Y - GPHY1) / (GPHY2 - GPHY1)
         END IF
      RLIM = 4.0
      RX = MAX (-RLIM, MIN (RLIM, RX))
      RY = MAX (-RLIM, MIN (RLIM, RY))
C                                       Plot files
      IF (.NOT.GPHDOT) THEN
C                                       See if enough room in buffer.
         IF (GPHPOS.GE.NEED) THEN
            CALL GPHWRT (BUFF, IER)
            IF (IER.NE.0) THEN
               IERR = 1
               GO TO 999
               END IF
            END IF
C                                       Put data in buffer.
         RX = RX * SCALEF
         RY = RY * SCALEF
         BUFF(GPHPOS+1) = OPCODE
         BUFF(GPHPOS+2) = IROUND (RX)
         BUFF(GPHPOS+3) = IROUND (RY)
         GPHPOS = GPHPOS + 3
C                                       TV instead
      ELSE
         ISERR = .FALSE.
         IXN = RX * GPHSCX  +  GPHIX0 + 0.5
         IYN = RY * GPHSCY  +  GPHIY0 + 0.5
         X1 = MAX (1, MIN (MAXXTV(1), GPHIXL))
         Y1 = MAX (1, MIN (MAXXTV(2), GPHIYL))
         X2 = MAX (1, MIN (MAXXTV(1), IXN))
         Y2 = MAX (1, MIN (MAXXTV(2), IYN))
         IF ((X1.NE.GPHIXL) .OR. (Y1.NE.GPHIYL)) THEN
            ALPHA = 1.0
            IF (IXN.NE.GPHIXL) ALPHA = REAL(X1-IXN)/REAL(GPHIXL-IXN)
            BETA = 1.0
            IF (IYN.NE.GPHIYL) BETA = REAL(Y1-IYN)/REAL(GPHIYL-IYN)
            ALPHA = MIN (ALPHA, BETA)
            X1 = IXN + ALPHA * (GPHIXL-IXN) + 0.5
            Y1 = IYN + ALPHA * (GPHIYL-IYN) + 0.5
            ISERR = .TRUE.
            END IF
         IF ((X2.NE.IXN) .OR. (Y2.NE.IYN)) THEN
            ALPHA = 1.0
            IF (IXN.NE.GPHIXL) ALPHA = REAL(X2-GPHIXL)/REAL(IXN-GPHIXL)
            BETA = 1.0
            IF (IYN.NE.GPHIYL) BETA = REAL(Y2-GPHIYL)/REAL(IYN-GPHIYL)
            ALPHA = MIN (ALPHA, BETA)
            X2 = GPHIXL + ALPHA * (IXN-GPHIXL) + 0.5
            Y2 = GPHIYL + ALPHA * (IYN-GPHIYL) + 0.5
            ISERR = .TRUE.
            END IF
C                                       Channel okay?
         GPHLTY = MAX (1, GPHLTY)
         CALL GCINIT (GPHTVG(GPHLTY), 0, IERR)
         IF (IERR.NE.0) GO TO 999
C                                       write bright vectors
         BUFF(1) = MAX (1, MIN (MAXXTV(1), X1))
         BUFF(3) = MAX (1, MIN (MAXXTV(2), Y1))
         BUFF(2) = MAX (1, MIN (MAXXTV(1), X2))
         BUFF(4) = MAX (1, MIN (MAXXTV(2), Y2))
         CALL IMVECT ('ONNN', GPHTVG(GPHLTY), 2, BUFF, BUFF(3), GPHSCR,
     *      IERR)
C                                       set last position
         GPHIXL = IXN
         GPHIYL = IYN
         IF (ISERR) GPHERV = GPHERV + 1
         GPHCNT = GPHCNT + 1
         IF (GPHCNT.GT.5000) THEN
            CALL YHOLD ('OFFF', IER)
            CALL YHOLD ('ONNN', IER)
            GPHCNT = 0
            END IF
         END IF
C
 999  RETURN
      END
