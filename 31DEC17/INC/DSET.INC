C                                                          Include DSET.
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
C                                                           VLBIN
      INTEGER    ITFLAG,JISOC,INSOC, NFRQS2
      INTEGER    NRPRM,NAXIS,NRATE,NDLY,NDEC,NRA,NFREQ,
     *           NSTOKE,NCMPLX,IPOLAR,CHNLO,CHNHI,NFOUT,
     *           LOCHN,HICHN,NCHAN,DAYN,REFDAY,IA,IB,ILAG,
     *           NLAG
      LOGICAL    AEQPA, XFORM,MACHDL,LFBS,FBSAMP,VLECK,LINED,
     *           NOAC, NOUVC, LAFBS, SHIFT
      REAL       PHSSET,CP(32,32),SP(32,32),AVGX,AVGAC
      DOUBLE PRECISION
     *       OMEGA,AR(6),BR(6),ABR(6), SFACT,BXM,BYM,BZM,BM,
     *           SBXM,SBYM,SBZM,CBXM,CBYM,CBZM,UVCON,IATUT1,
     *           PI,TWOPI,SIDER,CLIGHT, GAST0,PACLK,ACLK,BCLK,
     *           ADOP,BDOP,AFREQ,TSTART,TSTOP,TSRC(50)
C                                                           VLBIN
      COMMON /CONST/  PI,TWOPI,SIDER,CLIGHT
      COMMON /SETCON/ SFACT,BXM,BYM,BZM,BM,SBXM,SBYM,SBZM,
     *                CBXM,CBYM,CBZM,UVCON,CP,SP,PHSSET,GAST0,PACLK,
     *                ACLK,BCLK,ADOP,BDOP,AFREQ,AVGX,AVGAC,IATUT1,
     *                DAYN,REFDAY,NFRQS2,IA,IB
      COMMON /SETFBS/ AEQPA,ITFLAG,JISOC,OMEGA,AR,BR,ABR
      COMMON /CONTRL/ TSRC,TSTART,TSTOP,LOCHN,HICHN,CHNLO,CHNHI,NFOUT,
     *                ILAG,NLAG,XFORM,MACHDL,LFBS,FBSAMP,VLECK,LINED,
     *                NOAC,NOUVC,LAFBS,SHIFT
      COMMON /NRAND/  NRPRM,NAXIS,NRATE,NDLY,NDEC,NRA,NFREQ,
     *                NSTOKE,NCMPLX,IPOLAR
C                                                          End DSET.
