      SUBROUTINE UVLIMT (BLMIN, BLMAX, UUMAX, VVMAX, IVIS, UIN, VIN,
     *   ERRCNT)
C-----------------------------------------------------------------------
C! Prints U-V point limits and increments error count
C# AP-util UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001
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
C   UVLIMT prints the U-V limits for gridded uv-data and then
C   Prints the offending data point.   Only a few errors are printed.
C   The limits are only printed if ERRCNT is zero.
C   Inputs:
C      BLMIN       R     Minimum Baseline Radius (lambda)
C      BLMAX       R     Maximum Baseline Radius (lambda)
C      UUMAX       R     Maximum U baseline length (lambda)
C      VVMAX       R     Maximum V baseline length (lambda)
C      IVIS        I     U-V Point number
C      UIN         R     U baseline length of input point
C      VIN         R     V baseline length of input point
C   Input/Output
C      ERRCNT      I     Count of Error Messages printed (incremented)
C-----------------------------------------------------------------------
      REAL      BLMIN, BLMAX, UUMAX, VVMAX, UIN, VIN
      INTEGER   IVIS, ERRCNT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       if not too many messages
      IF (ERRCNT.LT.10) THEN
C                                       Encode limits
         WRITE (MSGTXT,1100,ERR=900) BLMIN, BLMAX, UUMAX, VVMAX
         IF (ERRCNT.EQ.0) CALL MSGWRT (7)
C                                       tell user point lost
 900     WRITE (MSGTXT,1200,ERR=999) IVIS, UIN, VIN
         CALL MSGWRT (7)
C                                       tell that there are more
      ELSE IF (ERRCNT.EQ.10) THEN
         MSGTXT = 'UVLIMT: REMAINING MESSAGES SUPPRESSED'
         CALL MSGWRT (7)
         END IF
C                                       increment count
      ERRCNT = ERRCNT + 1
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('UVLIMT: LIMITS=',1PE9.2,1PE9.2,'; MAX U=',
     *         1PE9.2,', V=',1PE9.2)
 1200 FORMAT ('UVLIMT: Point',I9,'  (U=',F12.0,',V=',F12.0,') Dropped')
      END
