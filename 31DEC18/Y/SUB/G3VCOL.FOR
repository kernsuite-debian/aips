      SUBROUTINE G3VCOL (RED, GREEN, BLUE, BUFF, IERR)
C-----------------------------------------------------------------------
C! writes a set 3-color line color record
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2003, 2006, 2008
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
C   G3VCOL writes a 'set 3-color vector color' command record to a graph
C   file or the TV (in immediate TV mode).
C   The output record description is:
C            I     opcode, 16 in this program.
C            R     Red color (0 - 1)
C            R     Green color (0 - 1)
C            R     Blue color (0 - 1)
C   Inputs:
C      RED     R        Red color (0.0 - 1.0)
C      GREEN   R        Green color (0.0 - 1.0)
C      BLUE    R        Blue color (0.0 - 1.0)
C      BUFF    I(256)   buffer to use for I/O.
C   Output:
C      IERR    I        error code. 0 = ok.
C                          1 = disk problems.
C                          2 = TV problems
C   Common:
C      GPHPOS  incremented by 4.
C      GPHRRN  incremented by 1 if a write to disk is needed.
C-----------------------------------------------------------------------
      REAL      RED, GREEN, BLUE
      INTEGER   BUFF(256), IERR
C
      REAL      RCOL(3)
      INTEGER   NEED, OPCODE, IER, ICOL(3), CCOL(3)
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      EQUIVALENCE (ICOL, RCOL)
      DATA NEED, OPCODE /253, 16/
C-----------------------------------------------------------------------
      IERR = 0
      RCOL(1) = MAX (0.0, MIN (1.0, RED))
      RCOL(2) = MAX (0.0, MIN (1.0, GREEN))
      RCOL(3) = MAX (0.0, MIN (1.0, BLUE))
      IF (MAXINT.LE.0) MAXINT = 8191
      CCOL(1) = RCOL(1) * MAXINT + 0.5
      CCOL(2) = RCOL(2) * MAXINT + 0.5
      CCOL(3) = RCOL(3) * MAXINT + 0.5
C                                       only if changed
      IF ((CCOL(1).NE.GPHCOL(1)) .OR. (CCOL(2).NE.GPHCOL(2)) .OR.
     *   (CCOL(3).NE.GPHCOL(3))) THEN
         GPHCOL(1) = CCOL(1)
         GPHCOL(2) = CCOL(2)
         GPHCOL(3) = CCOL(3)
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
            BUFF(GPHPOS+1) = OPCODE
            BUFF(GPHPOS+2) = ICOL(1)
            BUFF(GPHPOS+3) = ICOL(2)
            BUFF(GPHPOS+4) = ICOL(3)
            GPHPOS = GPHPOS + 4
            END IF
         END IF
C
 999  RETURN
      END
