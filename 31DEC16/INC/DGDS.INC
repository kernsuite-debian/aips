C                                                          Include DGDS.
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997, 1999, 2008-2009, 2014
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
C                                       include for uv modeling
      INTEGER   SCRBLK(256), KLNBLK(256), MFIELD, FLDSZ(2,MAXFLD),
     *   CCDISK(MAXFLD), CCCNO(MAXFLD), CCVER(MAXFLD), CNOBEM(MAXFLD),
     *   BEMVOL(MAXFLD), KSTOK, SCTYPE, VOFF, NSTOK, NCHANG
      LOGICAL   DOFFT, NONEG, DOPTMD, NGRDAT, DO3DIM, ONEBEM, DOALL4
      INTEGER   NSUBG(MAXFLD), NCLNG(MAXFLD)
      REAL      CELLSG(2), FLUXG(MAXFLD), TFLUXG, SSROT, CCROT,
     *   XPOFF(MAXFLD), YPOFF(MAXFLD), SCLUG(MAXFLD), SCLVG(MAXFLD),
     *   SCLWG(MAXFLD), SCLUM, SCLVM, FACGRD(2), DXCG(MAXFLD),
     *   DYCG(MAXFLD), DZCG(MAXFLD), OSFX, OSFY, PTFLX, PTRAOF, PTDCOF,
     *   PARMOD(6), UVROT, MAPROT, LIMFLX, UMATS(3,3,MAXFLD)
      DOUBLE PRECISION FREQG(MAXCIF)
      COMMON /MAPDES/  FREQG, SCRBLK, KLNBLK,
     *   CELLSG, SCLUG, SCLVG, SCLWG, SCLUM, SCLVM, UVROT, MAPROT,
     *   DXCG, DYCG, DZCG, FLUXG, TFLUXG, XPOFF, YPOFF, SSROT, CCROT,
     *   FACGRD, OSFX, OSFY, PTFLX, PTRAOF, PTDCOF, PARMOD, NCLNG,
     *   NSUBG, DOFFT, NONEG, DOPTMD, NGRDAT, DO3DIM, MFIELD, FLDSZ,
     *   CCDISK, CCCNO, CCVER, CNOBEM, BEMVOL, KSTOK, SCTYPE, VOFF,
     *   NSTOK, NCHANG, LIMFLX, UMATS, ONEBEM, DOALL4
C                                                          End DGDS.
