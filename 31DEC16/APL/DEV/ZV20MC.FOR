      SUBROUTINE ZV20MC (FTAB)
C-----------------------------------------------------------------------
C! issue a master clear to the TV - for Comtal this is a No-Op
C# Z TV-IO
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997
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
C   issues a "master clear" to the TV.  This resets the TV IO system (if
C   necessary) to expect a command record next.
C   Generic version for Comtal - apparently this operation doesn't exist
C
C   Inputs:
C      FTAB     I(*)        FTAB entry for TV i/o channel
C-----------------------------------------------------------------------
      INTEGER   FTAB(*)
C-----------------------------------------------------------------------
C                                       NO-OP for Comtal
C
 999  RETURN
      END
