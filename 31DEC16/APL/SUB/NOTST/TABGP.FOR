      SUBROUTINE TABGP (OPCODE, BUFFER, GPROW, GPCOLS, TIME, PRN, AZ,
     *   ELEV, TECTAU, TECPHS, IRET)
C-----------------------------------------------------------------------
C! Does I/O to GP table opened by GPINI
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1996, 2007, 2013
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
C   Does I/O to GPS delay (GP) extention tables. Usually used after
C   setup by GPINI.
C   Inputs:
C      OPCODE   C*4      Operation code:
C                        'READ' = read entry from table.
C                        'WRIT' = write entry in table.
C                        'CLOS' = close file, flush on write
C      BUFFER   I(512)   I/O buffer and related storage, also defines
C                        file if open. Should have been returned by
C                        GPINI or TABINI.
C      GPROW    I        Next entry number to read or write.
C      GPCOLS   I(6)     The column pointer array in order,
C                        TIME, PRN, AZ, ELEV, TECTAU, TECPHS
C   Input/output: (written to or read from baseline file)
C      TIME    D        Time of measurement (days)
C      PRN     I        Satellite PRN (an id number)
C      AZ      R        Azimuth of satellite (degrees)
C      ELEV    R        Elevation of satellite
C      TECTAU  R        TEC along the line of sight derived from delay
C                       (electrons/m^2)
C      TECPHS  R        TEC along the line of sight derived from phase
C                       (electrons/m^2)
C   Output:
C      GPROW    I      Next solution number.
C      IRET     I      Error code, 0=>OK
C                                  < 0 => row flagged
C                                  > 0 => TABIO error
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4
      INTEGER   BUFFER(*), GPROW, GPCOLS(6), PRN, IRET
      DOUBLE PRECISION TIME
      REAL      AZ, ELEV, TECTAU, TECPHS
C
      INTEGER   RECI(7)
      REAL      RECORD(7)
      DOUBLE PRECISION RECD(1)
C
      INCLUDE 'INCS:DMSG.INC'
C
      EQUIVALENCE (RECD, RECORD, RECI)
C-----------------------------------------------------------------------
C                                       Check opcode:
      IF ((OPCODE.NE.'CLOS') .AND. (OPCODE.NE.'READ') .AND.
     *   (OPCODE.NE.'WRIT')) THEN
         MSGTXT = 'TABGP: UNRECOGNIZED OPCODE ' // OPCODE
         CALL MSGWRT (9)
         IRET = 99
         GO TO 999
         END IF
C                                       Close
      IF (OPCODE.EQ.'CLOS') THEN
         CALL TABIO ('CLOS', 0, GPROW, RECORD, BUFFER, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET
            CALL MSGWRT (9)
            END IF
         GO TO 999
         END IF
C                                       If write fill RECORD
      IF (OPCODE.NE.'READ') THEN
         RECD(GPCOLS(1))   = TIME
         RECI(GPCOLS(2))   = PRN
         RECORD(GPCOLS(3)) = AZ
         RECORD(GPCOLS(4)) = ELEV
         RECORD(GPCOLS(5)) = TECTAU
         RECORD(GPCOLS(6)) = TECPHS
         END IF
C                                       Process record.
      CALL TABIO (OPCODE, 0, GPROW, RECORD, BUFFER, IRET)
      GPROW = GPROW + 1
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         CALL MSGWRT (9)
C                                       If READ pick data from RECORD.
      ELSE IF (OPCODE.EQ.'READ') THEN
         TIME   = RECD(GPCOLS(1))
         PRN    = RECI(GPCOLS(2))
         AZ     = RECORD(GPCOLS(3))
         ELEV   = RECORD(GPCOLS(4))
         TECTAU = RECORD(GPCOLS(5))
         TECPHS = RECORD(GPCOLS(6))
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TABGP: TABIO ERROR ',I3)
      END
