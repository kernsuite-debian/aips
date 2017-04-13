C                                                          Include DPTP.
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
C                                       Local include for PRTTP
      REAL      DOCRT
      CHARACTER PRTEXT*132, TITL1*132, TITL2*132
      INTEGER   IBUF(4096), NFILES, ITYPE, FDVEC(50), TBIND, PLUN, PIND,
     *   NLINES, INTAPE, IPAGE, IPTLEV, NCR, TAPBUF(29184)
      LOGICAL   RQUICK, SHORT
C                                       Local include for PRTTP
      COMMON /PRTTAP/ DOCRT, TAPBUF, IBUF,
     *   RQUICK, SHORT, NFILES, ITYPE, PLUN, PIND, NLINES, INTAPE,
     *   IPAGE, IPTLEV, NCR, FDVEC, TBIND
      COMMON /PRTPCH/ PRTEXT, TITL1, TITL2
C                                                          End DPTP.
