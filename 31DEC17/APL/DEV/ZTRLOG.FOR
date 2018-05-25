      SUBROUTINE ZTRLOG (LLEN, LOGNAM, XLEN, XLATED, XLNB, IERR)
C-----------------------------------------------------------------------
C! translate a logical name
C# System
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C  Translate a logical name (i.e., environment variable).
C  Inputs:
C     LLEN     I          Length of LOGNAM (1-relative)
C     LOGNAM   C*(LLEN)   Logical name
C     XLEN     I          Length of XLATED (1-relative)
C  Output:
C     XLATED   C*(XLEN)   Translation (blank filled)
C     XLNB     I          Position of last non-blank in XLATED
C                         (1-relative)
C     IERR     I          Error return code: 0 => no error
C                             1 => error
C  Generic version - calls ZTRLO2 with HOLLERITHs
C-----------------------------------------------------------------------
      INTEGER   LLEN, XLEN, XLNB, IERR
      CHARACTER LOGNAM*(*), XLATED*(*)
C
      HOLLERITH HOGNAM(64), HLATED(64), HBLANK
C-----------------------------------------------------------------------
       CALL CHR2H (4, '    ', 1, HBLANK)
       CALL RFILL (64, HBLANK, HOGNAM)
       CALL RFILL (64, HBLANK, HLATED)
       XLATED = ' '
       CALL CHR2H (LLEN, LOGNAM, 1, HOGNAM)
       CALL ZTRLO2 (LLEN, HOGNAM, XLEN, HLATED, XLNB, IERR)
       IF (XLNB.GT.0) CALL H2CHR (XLNB, 1, HLATED, XLATED)
C
 999  RETURN
      END
