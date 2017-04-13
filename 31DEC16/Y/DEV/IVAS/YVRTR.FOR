      SUBROUTINE YVRTR (VRTRTC, IERR)
C-----------------------------------------------------------------------
C! controls IVAS TV update mode - during flyback or all the time
C# TV-IO
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
C   YVRTR switches the transfer restrictions on and off if the current
C   restriction disagrees with that previously specified.
C   Input:
C      VRTRTC  L    T => restrict to vertical flyback intervals
C                       to reduce flicker due to I/O
C   Output:
C      IERR    I    Error
C   Common:
C      YBUFF(1)     1 => restrict on, 0 => off
C   Version for IIS IVAS
C-----------------------------------------------------------------------
      LOGICAL   VRTRTC
      INTEGER   IERR
C
      INTEGER   FIVASGPHRESTRICT, FIVASSYNCRESTRICT, JERR
      INCLUDE 'INCS:DTVC.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Off restrictions
      IF (.NOT.VRTRTC) THEN
         IF (YBUFF(1).GT.0) THEN
            JERR = FIVASGPHRESTRICT (0)
            IF (JERR.NE.0) GO TO 900
            JERR = FIVASSYNCRESTRICT (0, 0)
            IF (JERR.NE.0) GO TO 900
            YBUFF(1) = 0
            END IF
C                                       ON restrictions
      ELSE
         IF (YBUFF(1).LE.0) THEN
            JERR = FIVASGPHRESTRICT (1)
            IF (JERR.NE.0) GO TO 900
            JERR = FIVASSYNCRESTRICT (1, 2)
            IF (JERR.NE.0) GO TO 900
            YBUFF(1) = 1
            END IF
         END IF
      GO TO 999
C                                       Error
 900  CALL YDOERR ('YVRTR', JERR, IERR)
C
 999  RETURN
      END
