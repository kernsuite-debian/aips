C                                                          Include DFIL.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2002
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
C                                       AIPS system catalog and scratch
      INTEGER   SCRLST, FILIST
      PARAMETER (SCRLST = 4096)
      PARAMETER (FILIST = 4096)
C
      INTEGER   NSCR, SCRVOL(SCRLST), SCRCNO(SCRLST), IBAD(10),
     *   LUNS(10), NCFILE, FVOL(FILIST), FCNO(FILIST), FRW(FILIST),
     *   CCNO
      LOGICAL   RQUICK
      COMMON /CFILES/ RQUICK, NSCR, SCRVOL, SCRCNO, NCFILE, FVOL, FCNO,
     *   FRW, CCNO, IBAD, LUNS
C                                                          End DFIL.
