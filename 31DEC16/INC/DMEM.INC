C                                                          Include DMEM.
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
     *   PTRJ, PTRK
      REAL   GAMMA,CHISQ,SCRAT(6),GAINU,GAIN,FMIN,FLUX,RESMAX,MAPLIM,
     *  GRDHSQ,GRDHER,ERSQ,GRDMAG,SCLF,RWNXY,RSNRSQ,SUMWT,EPSSTR,
     *  APSIZ,SCLDRT,SCLBEM,OFFDRT,FLMBDA,DELMAX,DELMIN,
     *  OFFBEM,XSPACE,YSPACE,MAPROT,TVFMAX,GRDJSQ,FITRMS,RJ,
     *  BEMFIL(6),RESFIL(6),HATFIL(6),WTFIL(6),DRTFIL(6),PHTFIL(6),
     *  PRSFIL(6),DATFIL(6),WK1FIL(6),WK2FIL(6),CNJFIL(6)
      INTEGER   NX,NY,MEMLST,MEMSTR,MEMLIM,BPS,HTYPE,
     *  LUNBEM,LUNHAT,LUNDAT,LUNRES,LUNPRS,LUNPHT,LUNWT,
     *  LUNWK1,LUNWK2,LUNCNJ,LUNDRT,LUNCL1,
     *  BEMVOL,RESVOL,PRSVOL,WTVOL,DRTVOL,HATVOL,PHTVOL,DATVOL,
     *  WK1VOL,WK2VOL,CNJVOL,
     *  BPBEM,BPRES,BPPRS,BPWT,BPDRT,BPWK1,BPWK2,BPCNJ,BPPHT,BPHAT,
     *  BPDAT,
     *  BOBEM(2),BORES(2),BOWT(2),BOPRS(2),BODRT(2),BOWK1(2),BOHAT(2),
     *  BOPHT(2),BODAT(2),BOWK2(2),BOCNJ(2),
     *  BMHIS(129),RESHIS(1050),ICENX,ICENY,
     *  BUFF1(8192),BUFSZ1,BUFF2(8192),BUFSZ2,BUFF3(8192),BUFSZ3,
     *  BUFF4(8192),BUFSZ4,ITER
C                                                          Include CMEM
      COMMON /MEMCOM/ PTRJ, PTRK, EPSSTR, RSNRSQ, SUMWT,
     *  GAMMA,CHISQ,SCRAT,GAINU,GAIN,FMIN,FLUX,RESMAX,MAPLIM,
     *  GRDHSQ,GRDHER,ERSQ,GRDMAG,SCLF,RWNXY,
     *  APSIZ,SCLDRT,SCLBEM,OFFDRT,FLMBDA,DELMAX,DELMIN,
     *  OFFBEM,XSPACE,YSPACE,MAPROT,TVFMAX,GRDJSQ,FITRMS,RJ,
     *  BEMFIL,RESFIL,HATFIL,WTFIL,DRTFIL,PHTFIL,
     *  PRSFIL,DATFIL,WK1FIL,WK2FIL,CNJFIL,
     *  NX,NY,MEMLST,MEMSTR,MEMLIM,BPS,HTYPE,
     *  LUNBEM,LUNHAT,LUNDAT,LUNRES,LUNPRS,LUNPHT,LUNWT,
     *  LUNWK1,LUNWK2,LUNCNJ,LUNDRT,LUNCL1,
     *  BEMVOL,RESVOL,PRSVOL,WTVOL,DRTVOL,HATVOL,PHTVOL,DATVOL,
     *  WK1VOL,WK2VOL,CNJVOL,
     *  BPBEM,BPRES,BPPRS,BPWT,BPDRT,BPWK1,BPWK2,BPCNJ,BPPHT,BPHAT,
     *  BPDAT,
     *  BOBEM,BORES,BOWT,BOPRS,BODRT,BOWK1,BOHAT,
     *  BOPHT,BODAT,BOWK2,BOCNJ,
     *  BMHIS,RESHIS,ICENX,ICENY,
     *  BUFF1,BUFSZ1,BUFF2,BUFSZ2,BUFF3,BUFSZ3,
     *  BUFF4,BUFSZ4,ITER
C                                                          End DMEM.
