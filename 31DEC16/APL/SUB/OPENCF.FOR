      SUBROUTINE OPENCF (LUN, NAMS, ERROR)
C-----------------------------------------------------------------------
C! opens a cataloged file (main file only), simplifies call to FILOPN
C# IO-WaWa
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
C   WaWa IO system: Open a MAIN (i.e. Catalogd) file and associate
C   it with Logical Unit Number LUN.
C   Inputs:
C      LUN        I      Logical Unit No.
C   In/out:
C      NAMS       C*36   Catalog identification NAMESTRING:
C                        NAME:CLASS:PTYPE:SEQIN:VOL:USID
C                        NAME, CLASS, & USID ignored if PTYPE = 'SC'.
C   OUTPUTS:
C      ERROR      I      Error codes: 0 => o.k.
C                        Otherwise as returned by FILOPN
C   Written by WaWa (4/21/81)
C-----------------------------------------------------------------------
      CHARACTER NAMS*36
      INTEGER   LUN, ERROR
C
      INTEGER   IDUM
      CHARACTER CDUM*2
      LOGICAL   F
      DATA F, IDUM, CDUM /.FALSE., 0, ' '/
C-----------------------------------------------------------------------
      CALL FILOPN (LUN, NAMS, F, CDUM, IDUM, ERROR)
C
 999  RETURN
      END
