C                                                          Include DUVP.
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
C                                       Local include for UVPLT
      REAL   USERID, NAMEIN(3), CLAIN(2), XSIN, XDISIN, XCHAN, XBIF,
     *   XINC,UVRANG(2), APARM(10), BPARM(10)
      REAL      BUFF1(1600)
      INTEGER   JBUFSZ
      REAL      TBEG, TFIN, XYSCL(2), XYOFF(2)
      INTEGER   IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, TYPCOR,
     *   TYPCCR, TYPEAX(2), NCH, BIF, VER, TESTEM(2)
      LOGICAL   UVREV, SCALEM(2), NOUVR
C                                       Local include for UVPLT
      COMMON /INPARM/ USERID, NAMEIN, CLAIN, XSIN, XDISIN, XCHAN, XBIF,
     *   XINC, UVRANG, APARM, BPARM
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /UVPCOM/ TBEG, TFIN, XYSCL, XYOFF,
     *   UVREV, SCALEM, NOUVR,
     *   IAW1, IAW2, INC, SEQIN, DISKIN, LUNI, INDI, TYPCOR, TYPCCR,
     *   TYPEAX, NCH, BIF, VER, TESTEM
C                                                          End DUVP.
