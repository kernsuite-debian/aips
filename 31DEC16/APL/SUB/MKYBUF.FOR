      SUBROUTINE MKYBUF (OP, PGM, BUFSW, BUFSR, BUFFER, HEADER)
C-----------------------------------------------------------------------
C! packages a command line into machine-independent form
C# TV-util
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
C   MKYBUF is a routine for packaging TV commands in a machine
C   independent form for later transmission through a communication
C   line.  The READ/WRITE operation code and program name are converted
C   to packed ASCII strings at the beginning of HEADER and the integer
C   parameter array is converted to 16-bit packed integers beginning at
C   the 9th 32-bit integer in HEADER.  The 6th integer is used for the
C   integer buffer size to write across the wire and the 7th to read
C   back from the wire.
C   Inputs:
C      OP      C*4      Operation code: 'READ', 'WRIT'
C      PGM     C*6      Subroutine name
C      BUFSW   I        Number I*4 values in BUFFER to send
C      BUFSR   I        Number of I*4 values in BUFFER to receive
C      BUFFER  I(*)     Buffer of parameter values - local integers,
C                       values range -32768 to 32767 only.
C   Output:
C      HEADER  I(*)     Machine independent (FITS standard) parameters
C                       for communication.  Must be >= 8 I*2s longer
C                       than BUFFER - in 16-bit form.
C-----------------------------------------------------------------------
      CHARACTER OP*4, PGM*(*)
      INTEGER   BUFSW, BUFSR, BUFFER(*), HEADER(*)
C
      CHARACTER CT*10
C-----------------------------------------------------------------------
C                                       Pack the OP codes:
      CT(1:4) = OP
      CT(5:) = PGM
      CALL ZCLC8 (10, CT, 1, HEADER)
      CALL ZILI16 (1, BUFSW, 6, HEADER)
      CALL ZILI16 (1, BUFSR, 7, HEADER)
      CALL ZILI16 (1, 0, 8, HEADER)
      IF (BUFSW.GT.0) CALL ZILI16 (BUFSW, BUFFER, 9, HEADER)
C
 999  RETURN
      END
