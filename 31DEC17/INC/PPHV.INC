C   Include for FITS-IDI phase-cal tables
C-----------------------------------------------------------------------
C;  Copyright (C) 1998
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
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:PPCV.INC'
C
C     MAXPHC      The number of recognized columns in a PH table
C     PHREV       The current revision number for PH tables
C     PHRSIZ      The number of words in an PH table record
C
      INTEGER   MAXPHC, PHREV, PHRSIZ
      PARAMETER (MAXPHC = 17)
      PARAMETER (PHREV = 2)
      PARAMETER (PHRSIZ = 4 + 5 + 2 * (4 * MAXIF + 2 * MAXTON * MAXIF
     *                    + 3 * MAXTON * MAXIF))

