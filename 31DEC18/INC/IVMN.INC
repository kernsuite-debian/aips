C                                                          Include IVMN
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
C                                       Local include for VM
      REAL   CHISQ, GAIN, FRES, FLUX, DEFMAX, DEFMIN,
     *   RL0, IMGMAX, IMGMIN, RNPPB, TFLUX,
     *   ALPHA, BETA, ALPINI, BETINI, DELALP, DELBET,
     *   DELAMX, DELAMN, DELBMX, DELBMN, GRDJE, GRDJF, GRDJES, GRDJS,
     *   SCLF, EPSSTR, GRDHSQ, GRDESQ, GRDFSQ, GRDHE, GRDHF, GRDEF,
     *   LAMBDA, APSIZ, DELMAX, DELMIN, GRDJSQ, FITRMS, RJ0, RJ1,
     *   VMFILE(6,15), RWNXY, ACCTYP(15),
     *   REED, WRIT, FINI, UPDT, REST, SRCH, DPOP, CLOS, ON, OFF,
     *   BUFFR1(8192), BUFFR2(8192), BUFFR3(8192), BUFFR4(8192)
      LOGICAL   DORES, OPTIC, DEFEXT, DOSTOP, MAP, EXCL, WAIT, T, F,
     *   LNGOUT
      INTEGER   NX, NY, NNX, NNY, VMSTR, VMLIM, BPS, HNX, HNY,
     *   BMHIS(129), ICENX, ICENY, VMSZ(2,15), SCRNM(15),
     *   VMVOL(15), VMBP(15), VMBO(2,15), BUFNUM(15),
     *   BUFF1(1), BUFF2(1), BUFF3(1), BUFF4(1),
     *   BUFSZ(4), BIND(4), FIND(4),
     *   I1, I2, I3, I4,
     *   ITER, IBEG, IEND, JBEG, JEND, III, JJJ, ITV, IERR,
     *   NUMFIL, NUMSCR,
     *   BLC(2), TRC(2), WINR(4), WINW(4),
     *   BEM, DEF, IMG, DAT, RES, WT, WK1, DRT, WK2, DEL, SCR, PRS,
     *   GRS, PGR, WK3, VMOUT,
     *   N0, N1, N2, N3, N4, N5, N6, N8, N10, N12, N16, N24
C                                                          End IVMN
