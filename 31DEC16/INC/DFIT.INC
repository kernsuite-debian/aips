C                                                          Include DFIT.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2003, 2005
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
      INTEGER   NCT, NKT, NCKT
      PARAMETER (NCT = 10)
      PARAMETER (NKT = 77)
      PARAMETER (NCKT = NCT+NKT)
C
      INTEGER   ATYPE(NCKT), APOINT(NCKT), CTYPE(NCT), KTYPE(NKT),
     *   CPOINT(NCT), POINT(NKT)
      CHARACTER AWORD(NCKT)*8, CWORD(NCT)*8, KWORD(NKT)*8
      EQUIVALENCE (AWORD(1), CWORD(1)),      (AWORD(NCT+1), KWORD(1))
      EQUIVALENCE (APOINT(1), CPOINT(1)),    (APOINT(NCT+1), POINT(1))
      EQUIVALENCE (ATYPE(1), CTYPE(1)),      (ATYPE(NCT+1), KTYPE(1))
C                                                          End DFIT.
