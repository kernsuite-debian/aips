C                                                          Include DUIN.
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
C                                       Local include for UVLOD
      DOUBLE PRECISION
     *      PSCAL(20), POFF(20), BSC, BZE, WTSCAL, BXSTA(50),
     *   BYSTA(50), BZSTA(50)
      REAL      DOALL, NAMEIN(2), NAMOUT(3), CLAOUT(2), BAND, BITER,
     *   NITER, QUAL, TAPE, DOEOF, XFILES, INFILE(12), OUTSEQ, DISO,
     *   XVSMAX, BUFF2(4610), PTYPES(40), NAMSTA(2,50)
      INTEGER   IBLANK, NUMVIS
      INTEGER   BUFF1(4010), CATBLK(256), FDVEC(50), TBIND,
     *   LENREC, IQUAL, JBAND, NFILES, DISOUT, TAPEIN, GROUP, ICEND,
     *   TABLES, IBPP, NPARMS, ITAB(20), JADR(4), NSTACT,
     *   SCRBUF(256), TAPBUF(29184)
      LOGICAL   NDOEOF, ISBLNK, LCMPLX, LSTOKE, STDEXT, DODISK
C                                       Local include for UVLOD
      COMMON /BUFRS/ TAPBUF, BUFF2, BUFF1, SCRBUF
      COMMON /INPARM/ DOALL, NAMEIN, QUAL, BAND, BITER, NITER, TAPE,
     *   XFILES, INFILE, DOEOF, NAMOUT, CLAOUT, OUTSEQ, DISO, XVSMAX
      COMMON /SCRINF/ NUMVIS, NDOEOF, IQUAL, JBAND, NFILES, DISOUT,
     *   TAPEIN, LENREC, FDVEC, TBIND
      COMMON /MAPHDR/ CATBLK
      COMMON /FITINF/ PSCAL, POFF, BSC, BZE, WTSCAL, PTYPES, GROUP,
     *   ICEND, IBLANK, TABLES, IBPP, NPARMS, ITAB, JADR, NSTACT,
     *   DODISK, ISBLNK, LCMPLX, LSTOKE, STDEXT
      COMMON /STACOM/ BXSTA, BYSTA, BZSTA, NAMSTA
C                                                          End DUIN.
