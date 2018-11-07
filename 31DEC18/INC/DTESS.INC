C                                                          Include DTESS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2002-2003
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
      INCLUDE 'INCS:PTESS.INC'
C
      REAL   DEC, CHISQ, TCHISQ, FLUX, TFLUX, GAIN, FITRMS,
     *   FFIT(MATFLD), FRES(MATFLD), RVAR(MATFLD), IMGMAX, IMGMIN,
     *   DEFMAX, DEFMIN, RESMAX, RESMIN, RCENX(MATFLD), RCENY(MATFLD),
     *   BMSIZE(MATFLD), CELLX, CELLY, RL0, RJ0, RJ1, RNPPB, SCLF,
     *   EPSSTR, BMAJ, BMIN, BPA, ALPHA, BETA, ALPINI, BETINI, DELALP,
     *   DELBET, DELAMX, DELAMN, DELBMX, DELBMN, GRDJSQ, GRDHSQ, GRDESQ,
     *   GRDFSQ, GRDJE, GRDJF, GRDHE, GRDHF, GRDEF, RWNXY, PBPARM(7)
      LOGICAL   DORES, DEFEXT, DOSTOP, MAP, EXCL, WAIT, T, F, LNGOUT,
     *   BLANKD
      INTEGER   NNX, NNY, VMSTR, VMLIM, BPS, ICENX, ICENY,
     *   SCRNM(MATFLD), BEAMTP(MATFLD), I1, I2, I3, I4, I5, I6, IX, IY,
     *   IERR,  NUMFIL, NUMSCR, DEF, IMG, DAT(MATFLD), RES, WT(MATFLD),
     *   WK1, WK2, DEL, SCR, PRS, WK3, VMOUT, CVMOUT, NFIELD, CHAN,
     *   DRTCNO
      CHARACTER ANAME*8
      COMMON /VMCOM/ DEC, CHISQ, TCHISQ, FLUX, TFLUX, GAIN, FITRMS,
     *   FFIT, FRES, RVAR, IMGMAX, IMGMIN, DEFMAX, DEFMIN, RESMAX,
     *   RESMIN, RCENX, RCENY, BMSIZE, CELLX, CELLY, RL0, RJ0, RJ1,
     *   RNPPB, SCLF, EPSSTR, BMAJ, BMIN, BPA, ALPHA, BETA, ALPINI,
     *   BETINI, DELALP, DELBET, DELAMX, DELAMN, DELBMX, DELBMN, GRDJSQ,
     *   GRDHSQ, GRDESQ, GRDFSQ, GRDJE, GRDJF, GRDHE, GRDHF, GRDEF,
     *   RWNXY, PBPARM, DORES, DEFEXT, DOSTOP, MAP, EXCL, WAIT, T, F,
     *   LNGOUT, BLANKD, NNX, NNY, VMSTR, VMLIM, BPS, ICENX, ICENY,
     *   SCRNM, BEAMTP, I1, I2, I3, I4, I5, I6, IX, IY, NUMFIL, NUMSCR,
     *   DEF, IMG, DAT, RES, WT, WK1, WK2, DEL, SCR, PRS, WK3, VMOUT,
     *   CVMOUT, NFIELD, CHAN, DRTCNO
      COMMON /VMCCOM/ ANAME
C                                                          End DTESS
