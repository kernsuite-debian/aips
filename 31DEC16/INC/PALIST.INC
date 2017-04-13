C                                       Include PALIST.INC
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
C                                       ALIST private include
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C                                       pointers and offsets for ALIST
      INTEGER   ALSIZE, ALSNAM, ALSBEG, ALSEND, ALSCHK, ALSCVL,
     *   ALSNDM, ALSHED, ALSDAT, ALSNXT, ALSKWD, ALSTYP, ALSDIM
C                                       ALSIZE = ALIST pointer for size
      PARAMETER (ALSIZE = 1)
C                                       ALSNAM = ALIST pointer for name
      PARAMETER (ALSNAM = 2)
C                                       ALSBEG = ALIST pointer for first
      PARAMETER (ALSBEG = ALSNAM+8)
C                                       ALSEND = ALIST pointer for next
C                                       free word 
      PARAMETER (ALSEND = ALSBEG+1)
C                                       ALSCHK = pointer to validity
C                                       check
      PARAMETER (ALSCHK = ALSEND+1)
C                                       ALSCVL = validity check (random
C                                       value)
      PARAMETER (ALSCVL = 1234567)
C                                       ALSNDM = pointer to max.
C                                       dimensionality. 
      PARAMETER (ALSNDM = ALSCHK+1)
C                                       ALSHED = pointer to length of
C                                       header.
      PARAMETER (ALSHED = ALSNDM+1)
C                                       ALSDAT = pointer to offset to
C                                       beginning of data.
      PARAMETER (ALSDAT = ALSHED+1)
C                                       ALSNXT = address offset of
C                                       start of next entry.
      PARAMETER (ALSNXT = 0)
C                                       ALSKWD = offset of keyword
      PARAMETER (ALSKWD = ALSNXT+1)
C                                       ALSTYP = offset of data type
      PARAMETER (ALSTYP = ALSKWD+4)
C                                       ALSDIM = offset of first
C                                       dimension.
      PARAMETER (ALSDIM = ALSTYP+1)
