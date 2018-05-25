C-----------------------------------------------------------------------
C                                                          Include DBPC.
C;  Copyright (C) 1996, 2005
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
C                                       Parameter includes.
      INCLUDE 'INCS:PUVD.INC'
C                                       Bandpass table
      DOUBLE PRECISION BPFREQ(MAXIF)
      REAL      CHNBND, LOWSHF, DELSHF, CURSHF(MAXANT), VLBUF(2),
     *   BPBUF(2), BPIBUF(2)
      INTEGER   IBPRNO, NBPINR, BPKOLS(MAXBPC), MXANUM, BPNUMV(MAXBPC),
     *   NANTBP, NPOLBP, NIFBP, NCHNBP, BCHNBP, NUMSHF, BPGOT(2,3),
     *   KSNCF, KSNCIF, KSNCS, LUNSBP, BPFIND, ANTREC(2,MAXANT),
     *   BPBUFF(512)
      LOGICAL   WPOLY
      LONGINT   PBPBUF, PBPIBF, PVLBUF, ANTNX(3,MAXANT), ANTPNT(2,3)
C                                       BP table
      COMMON /CBPINF/ BPBUFF, BPFREQ, ANTNX, PBPBUF, PBPIBF, PVLBUF,
     *   ANTPNT, BPBUF, BPIBUF, VLBUF, CHNBND, LOWSHF, DELSHF, CURSHF,
     *   IBPRNO, NBPINR, BPKOLS, BPNUMV, NANTBP, NPOLBP, NIFBP, NCHNBP,
     *   BCHNBP, NUMSHF, BPGOT, KSNCF, KSNCIF, KSNCS, MXANUM, LUNSBP,
     *   BPFIND, ANTREC, WPOLY
C                                                          End DBPC.
