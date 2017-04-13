      SUBROUTINE GLTYPE (LTYPE, BUFF, IERR)
C-----------------------------------------------------------------------
C! writes a line type command to the plot file
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 2002
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
C   GLTYPE will write a 'line type' command record to a graph file or
C   the TV (in immediate TV mode).
C   The output record description is:
C            I     opcode, 9 in this program.
C            I     line type: 1 exterior labeling, 2 contours, 3
C                     polarization vectors, 4 stars, ?
C   Inputs:
C      LTYPE   I       line type
C      BUFF    I(256)  buffer to use for I/O.
C   Output:
C      IERR   I       error code. 0 = ok.
C                        1 = disk problems.
C                        2 = TV problems
C   Common:
C      GPHPOS  incremented by 2
C      GPHRRN  incremented by 1 if a write to disk is needed.
C-----------------------------------------------------------------------
      INTEGER   LTYPE, BUFF(256), IERR
C
      INTEGER   NEED, OPCODE
      INCLUDE 'INCS:DGPH.INC'
      DATA NEED, OPCODE /255, 9/
C-----------------------------------------------------------------------
      IERR = 0
      IF (LTYPE.NE.GPHLTY) THEN
         GPHLTY = LTYPE
C                                       Plot files
         IF (.NOT.GPHDOT) THEN
C                                       See if enough room in buffer.
            IF (GPHPOS.GE.NEED) THEN
               CALL GPHWRT (BUFF, IERR)
               IF (IERR.NE.0) THEN
                  IERR = 1
                  GO TO 999
                  END IF
               END IF
C                                       Put data in buffer.
            BUFF(GPHPOS+1) = OPCODE
            BUFF(GPHPOS+2) = LTYPE
            GPHPOS = GPHPOS + 2
            END IF
         END IF
C
 999  RETURN
      END
