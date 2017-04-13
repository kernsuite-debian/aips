C                                                          Include DITB.
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
C                                       Wawa I/O common
      REAL    INDEF
      CHARACTER WRIT*4, REED*4, CLWR*4, CLRD*4, REST*4, OPEN*4, CLOS*4,
     *   SRCH*4, INFO*4, UPDT*4, FINI*4, CSTA*4, SUBNAM(8)*6
      INTEGER    LINT, LREAL, NFIL, EFIL, QUACK,
     *   POLUN, POFIN, POVOL, POCAT, POIOP, POASS, POBPX,
     *   PODIM, PONAX, POBLC, POTRC, PODEP, POBL, FILTAB(38,10)
      COMMON /WAWCHR/  WRIT, REED, CLWR, CLRD, REST, OPEN, CLOS,
     *   SRCH, INFO, UPDT, FINI, CSTA, SUBNAM
      COMMON /WAWAIO/ INDEF,  LINT, LREAL, NFIL, EFIL, QUACK,
     *   POLUN, POFIN, POVOL, POCAT, POIOP, POASS, POBPX,
     *   PODIM, PONAX, POBLC, POTRC, PODEP, POBL, FILTAB
C                                                          End DITB.
