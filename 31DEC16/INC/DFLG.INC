C-----------------------------------------------------------------------
C;  Copyright (C) 1998, 2007, 2011
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
C                                       Flag table info
      REAL      TMFLST, FLGTST(MAXFLG), FLGTND(MAXFLG)
      LOGICAL   FLGPOL(4,MAXFLG), TIMORD
      INTEGER   IFGRNO, NUMFLG, FGKOLS(MAXFGC), FGNUMV(MAXFGC),
     *   KNCOR, KNCF, KNCIF, KNCS, KCOR0, FGBUFF(512),
     *   FLGSOU(MAXFLG), FLGANT(MAXFLG), FLGBAS(MAXFLG), FLGSUB(MAXFLG),
     *   FLGBIF(MAXFLG), FLGEIF(MAXFLG), FLGBCH(MAXFLG), FLGECH(MAXFLG),
     *   FLGFQD(MAXFLG)
C                                       FLAG table descriptors
      COMMON /CFMINF/ FGBUFF, FGKOLS, FGNUMV, TMFLST, IFGRNO, NUMFLG,
     *   KNCOR, KNCF, KNCIF, KNCS, KCOR0, TIMORD
C                                       flag table data
      COMMON /CFMDAT/ FLGTST, FLGTND, FLGPOL, FLGSOU, FLGANT, FLGBAS,
     *   FLGSUB, FLGBIF, FLGEIF, FLGBCH, FLGECH, FLGFQD
C                                                          End DFLG.
