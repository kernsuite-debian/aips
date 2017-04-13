      SUBROUTINE GPOS (X, Y, BUFF, IERR)
C-----------------------------------------------------------------------
C! write a position-"pen" command into a plot file (or TV)
C# Plot-util
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
C   GPOS will write a 'position vector' command record to a graph file.
C   The output record description is:
C            I     opcode, 4 in this program.
C            I     scaled X position.
C            I     scaled Y position.
C   Inputs:
C      X       R       x position
C      Y       R       y position.
C      BUFF    I(256)  buffer to use for I/O.
C   Output:
C      IERR    I       error code. 0 = ok.
C                         1 = disk problems.
C                         2 = TV problems
C   Common:
C      GPHPOS  incremented by 3.
C      GPHRRN  incremented by 1 if a write to disk is needed.
C-----------------------------------------------------------------------
      REAL      X, Y
      INTEGER   BUFF(256), IERR
C
      REAL      RX, RY, RLIM
      INTEGER   NEED, OPCODE, IER, IROUND
      INCLUDE 'INCS:DGPH.INC'
      DATA NEED, OPCODE /254, 4/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Scale X and Y.
      RLIM = 4.0
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
      RX = MAX (-RLIM, MIN (RLIM, RX))
      RY = MAX (-RLIM, MIN (RLIM, RY))
C                                       Plot file
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
         BUFF(GPHPOS+1) = OPCODE
         RX = RX * SCALEF
         RY = RY * SCALEF
         BUFF(GPHPOS+2) = IROUND (RX)
         BUFF(GPHPOS+3) = IROUND (RY)
         GPHPOS = GPHPOS + 3
C                                       TV
      ELSE
         GPHIXL = RX * GPHSCX + GPHIX0 + 0.5
         GPHIYL = RY * GPHSCY + GPHIY0 + 0.5
         END IF
C
 999  RETURN
      END
