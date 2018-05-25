      SUBROUTINE HIINIT (NFILES)
C-----------------------------------------------------------------------
C! initializes the history common area - must be called before history
C# History
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
C   HIINIT initializes the history common area /HICOM /
C   Inputs:
C      NFILES   I     number of HI files open at once (max)
C                     at least 20 are available via DHIS.INC
C-----------------------------------------------------------------------
      INTEGER    NFILES
C
      INTEGER NC
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
      NHIWRD = 7
      NHIFIL = 20
      NHIWPL = 18
      NHILPR = 252 / NHIWPL
      NC = NHIWRD * NFILES
      CALL FILL (NC, 0, HITAB)
      IF (NFILES.LE.NHIFIL) GO TO 999
         WRITE (MSGTXT,1000) NFILES, NHIFIL
         CALL MSGWRT (7)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('HIINIT:',I5,' HI FILES REQUESTED, BUT',I3,' IS LIMIT')
      END
