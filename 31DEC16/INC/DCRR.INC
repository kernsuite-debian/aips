C                                                          Include DCRR.
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
      INTEGER   CATBLK(256), JBUFSZ, ICH(4), INDEX(4), ISTOKE(4),
     *   IUSER, SEQIN, DISKIN, SEQOUT, DISKO, LIMIT2, MAXANT
      REAL      USERID, NAMEIN(3), CLAIN(2), XSIN, XDISIN, NAMOUT(3),
     *   CLAOUT(2), XSOUT, XDISO, DOOUTP, DOCRT, XCHN, UVRANG(2),
     *   TIMER(8), CPARM(10), BUFF1(2048), PTFLUX(4), TIME1, TIME2,
     *   FLUX(4), RMSL(4), NP(4,50,50), SVIS(4,50,50), S2VIS(4,50,50),
     *   VMAX(4,50,50), VMIN(4,50,50)
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /UVSUM/ NP, SVIS, S2VIS, VMAX, VMIN
      COMMON /INPARM/ USERID, NAMEIN, CLAIN, XSIN, XDISIN, NAMOUT,
     *   CLAOUT, XSOUT, XDISO, DOOUTP, DOCRT, XCHN, UVRANG, TIMER, CPARM
      COMMON /TSPARM/ PTFLUX, TIME1, TIME2, FLUX, RMSL, ISTOKE, ICH,
     *   INDEX, IUSER, SEQIN, DISKIN, SEQOUT, DISKO, LIMIT2, MAXANT
      COMMON /MAPHDR/ CATBLK
C                                                          End DCRR.
