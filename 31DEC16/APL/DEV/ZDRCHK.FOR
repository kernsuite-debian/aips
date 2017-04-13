      SUBROUTINE ZDRCHK
C-----------------------------------------------------------------------
C! Checks to see that all CA files are present and writable or read only
C# Z IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 2004
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
C   DRCHK checks those CA files for the current user to see if they
C   are writable or not.
C   Output: COMMON
C-----------------------------------------------------------------------
      CHARACTER PHNAME*48
      INTEGER   IER, IV, IV1, IV2, LSIZE
      HOLLERITH ANAME(12)
      LOGICAL   IAMOK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DDRC.INC'
C-----------------------------------------------------------------------
      IV1 = 1
      IV2 = NVOL
C                                       Loop on disks
      DO 20 IV = IV1,IV2
         IF (.NOT.IAMOK(IV,'CA')) THEN
            DSKLOK(IV) = 2
         ELSE
            CALL ZPHFIL ('CA', IV, 0, 0, PHNAME, IER)
            CALL ZEXIST (IV, PHNAME, LSIZE, IER)
            IF (IER.NE.0) THEN
               DSKLOK(IV) = -1
            ELSE
               CALL CHR2H (48, PHNAME, 1, ANAME)
               CALL ZDRCH2 (ANAME, DSKLOK(IV))
               END IF
            END IF
 20      CONTINUE
C
 999  RETURN
      END
