      SUBROUTINE ZTFILL (FIND, MAP)
C-----------------------------------------------------------------------
C! zero-fill, initialize a file IO table (FTAB) entry
C# Z IO-basic
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
C   ZTFILL fills in initial values in the FTAB file control block
C   Inputs:
C      FIND   I   Index in FTAB to file control block
C      MAP    L   Map file indicator
C   Common: via DDCH.INC
C      FTAB   I(*)   input:  FTAB(FIND) = LUN
C                    output: FTAB(FIND+1...) = ? whatever is needed
C   VMS version - doesn't zero the event flag numbers.
C-----------------------------------------------------------------------
      INTEGER    FIND
      LOGICAL    MAP
C
      INTEGER    J, EFO, EFO1, EFO2, N
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DZCH.INC'
C-----------------------------------------------------------------------
C                                       Offsets for event flags.
      EFO = NMOFF + FCBSP1
      EFO1 = MOFF + FCBSP1
      EFO2 = EFO1 + MFCB
C                                        Map FCB. Don't 0 event flag
C                                        numbers put there by ZDCHI2
      IF (MAP) THEN
         N = NBTB3 / 2 - 1
         DO 10 J = 1,N
            IF ((J.NE.EFO1) .AND. (J.NE.EFO2)) FTAB(FIND+J) = 0
 10         CONTINUE
         GO TO 999
C                                        Non map.
      ELSE
         N = NBTB2 / 2 - 1
         DO 15 J = 1,N
            IF (J.NE.EFO) FTAB(FIND+J) = 0
 15         CONTINUE
         END IF
C
 999  RETURN
      END
