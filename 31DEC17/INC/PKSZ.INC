C                                                          Include PKSZ
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
C                                       define K array and proc listing
C                                       sizes and break point
      INTEGER   KBLOCK, LBLOCK, KPOINT
      PARAMETER (KBLOCK = 388)
      PARAMETER (LBLOCK = 160)
      PARAMETER (KPOINT = 87712)
C                                       Any change above means changes
C                                       of sizes in $AIPSUB/SGLOCA
C                                       Save/Get version number
C                                       Restricted to versions =
C                                       5/5/96: -> 15 for MAXFLD -> 64
C                                       19980813: -> 16 for bigger K
      INTEGER   SGVERS
      PARAMETER (SGVERS = 16)
C                                       Also define size of temp array
C                                       called KKT
      INTEGER   KKTSIZ
      PARAMETER (KKTSIZ = 4500)
C                                                          End PKSZ
