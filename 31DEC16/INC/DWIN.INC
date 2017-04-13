C                                                          Include DWIN.
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
      DOUBLE PRECISION
     *      BSC, BZE, RA1, DEC1, RA0, DEC0, BASLM(14)
      REAL      NAMEIN(3), NAMOUT(3), CLAOUT(2), TAPE, XFILES, UVWTFN,
     *   DISO, XVSMAX, BUFF2(4610), PSCAL(91,4), BASNO(91), BASLIN(91),
     *   SHA(91), DHA(91), BFREQ, BANDW, WVSMAX, DAYFR, PHIROT,
     *   NAMPAK(3), CLSPAK(2), STAT, DONOTS, XBAND
      INTEGER   NVIS
      LOGICAL   NDOEOF, ISBLNK, LCMPLX, LSTOKE, RQUICK
      INTEGER   BUFF1(9110), CATBLK(256), TLUN, TIND, LREC, NFILES,
     *   DISOUT, TAPEIN, GROUP, ICEND, IBLANK(2), TABLES, IBPP, NPARMS,
     *   ITAB(20), JADR(4), NSTACT, STRTRC(91,4), INT(91), LUNDS,
     *   DINDS, NDATP(91), POFF(91,4), NRINT, WDATE(6), CCNO, POLC,
     *   ICNO, SNO, WS, NRPOL, NSTOKES, IVOL, IVER, NBAND
      COMMON /BUFRS / BUFF2, BUFF1
      COMMON /INPARM/ NAMEIN, XBAND, TAPE, XFILES, NAMOUT, CLAOUT,
     *   DISO, UVWTFN, DONOTS, XVSMAX
      COMMON /SCRINF/ NVIS, NDOEOF, RQUICK, NFILES, DISOUT, TAPEIN,
     *   LREC, TLUN, TIND, LUNDS, DINDS, NBAND
      COMMON /MAPHDR/ CATBLK
      COMMON /FITINF/ BSC, BZE, ISBLNK, LCMPLX, LSTOKE, ICEND,
     *   IBLANK, TABLES, IBPP, NPARMS, ITAB, JADR, NSTACT
      COMMON /SRTDAT/ RA0, DEC0, RA1, DEC1, BASLM,  PSCAL, BASNO,
     *   BASLIN, SHA, DHA, BFREQ, BANDW, WVSMAX, DAYFR, NDATP, POFF,
     *   PHIROT, STRTRC, IINT, NRINT, WDATE, CCNO, POLC, NRPOL, NSTOKES
      COMMON /FILES/  NAMPAK, CLSPAK, STAT, ICNO, SNO, WS, IVOL, IVER
C                                                          End DWIN.
