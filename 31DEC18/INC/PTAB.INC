C                                                  PTAB.INC
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
C                                        Parameters for binary tables.
C                                        Data type codes
C                                         TABDBL = double-precision fp
C                                         TABFLT = single-precision fp
C                                         TABHOL = Hollerith
C                                         TABINT = integer
C                                         TABLOG = logical
C                                         TABBIT = bit string
C                                         TABSEL = select flag
      INTEGER   TABDBL, TABFLT, TABHOL, TABINT, TABLOG, TABBIT, TABSEL
      PARAMETER (TABDBL = 1)
      PARAMETER (TABFLT = 2)
      PARAMETER (TABHOL = 3)
      PARAMETER (TABINT = 4)
      PARAMETER (TABLOG = 5)
      PARAMETER (TABBIT = 7)
      PARAMETER (TABSEL = 9)
C                                        TABIO information codes
C                                         TIOROW = table row
C                                         TIOPTR = DATPTR record
C                                         TIOSEL = data selection
C                                                  string
C                                         TIOTTL = column title
C                                         TIOUNT = column units
C                                         TIOKEY = keyword/value
      INTEGER TIOROW, TIOPTR, TIOSEL, TIOTTL, TIOUNT, TIOKEY
      PARAMETER (TIOROW = 0)
      PARAMETER (TIOPTR = 1)
      PARAMETER (TIOSEL = 2)
      PARAMETER (TIOTTL = 3)
      PARAMETER (TIOUNT = 4)
      PARAMETER (TIOKEY = 5)
C                                                  End PDAT.INC
